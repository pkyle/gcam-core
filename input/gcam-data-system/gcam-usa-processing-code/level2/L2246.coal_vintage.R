if( !exists( "GCAMUSAPROC_DIR" ) ){
  if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
    GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
  } else {
    stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
  }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "L2246.coal_vintage.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "L2246.coal_vintage.R: Model input to include detailed coal vintages" )

# Authors: Gokul Iyer
# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

prime_mover_map <- readdata( "GCAMUSA_MAPPINGS","prime_mover_map")

# Reading in generator level data for 2010 and 2015 from EIA form 860 and 923. These will be used to create 
# a database of generation by unit and expected retirement date

eia_860_data_2010 <- readdata("GCAMUSA_LEVEL0_DATA","EIA_860_generators_existing_2010", skip=2)

eia_860_data_2015 <- readdata("GCAMUSA_LEVEL0_DATA","EIA_860_generators_existing_2015", skip=2)

eia_923_data_2010 <- readdata( "GCAMUSA_LEVEL0_DATA","EIA_923_generator_gen_fuel_2010" , skip = 4 )

eia_923_data_2015 <- readdata( "GCAMUSA_LEVEL0_DATA","EIA_923_generator_gen_fuel_2015" , skip = 4 )

eia_860_retired_2016 <- readdata("GCAMUSA_LEVEL0_DATA","EIA_860_generators_retired_2016", skip=2)

# Reading in files from the data-system

StubTechProd_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2240.StubTechProd_elec_USA_coalret" , skip = 4 )
StubTechEff_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2240.StubTechEff_elec_USA_coalret" , skip = 4 )
GlobalTechEff_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2240.GlobalTechEff_elec_coalret" , skip = 4 )
GlobalTechCapFac_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2240.GlobalTechCapFac_elec_coalret" , skip = 4 )
GlobalTechCapital_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2240.GlobalTechCapital_elec_coalret" , skip = 4 )
GlobalTechOMfixed_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2240.GlobalTechOMfixed_elec_coalret" , skip = 4 )
GlobalTechOMvar_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2240.GlobalTechOMvar_elec_coalret" , skip = 4 )
GlobalTechCapital_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2240.GlobalTechCapital_elec_coalret" , skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations


# Define assumptions about lifetimes for generators/units without data. The categories chosen for lifetime assumptions 
# are such that capacity in each category is roughly same. This is done to get a somewhat smooth behavior
# for coal retirements

avg_coal_plant_lifetime <- 70 # Assume  lifetime for units built after 1970

# Define vintage bins and categories
 
 vintage.breaks <- c(0, 1950, 1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)
 vintage.lables <- c("before 1950", "1951-1955", "1956-1960", "1961-1965", "1966-1970", "1971-1975", "1976-1980", 
                     "1981-1985", "1986-1990", "1991-1995", "1996-2000", "2001-2005", "2006-2010", "2011-2015")
 

# Creating a database of coal units operating in 2010 and 2015 that includes their their vintage, capacity and planned retirement year. 
# Using nameplate capacity for now. Not sure if summer capacity makes more sense
coal_units_2010 <-
  eia_860_data_2010 %>%
  select(STATE, PLANT_NAME, PLANT_CODE, GENERATOR_ID, OPERATING_YEAR, PLANNED_RETIREMENT_YEAR, PRIME_MOVER, ENERGY_SOURCE_1, SUMMER_CAPABILITY, NAMEPLATE) %>%
  mutate(unit.capacity.MW = if_else(is.na(NAMEPLATE), SUMMER_CAPABILITY, NAMEPLATE)) %>%
  select(-SUMMER_CAPABILITY, -NAMEPLATE) %>%
  left_join(prime_mover_map, by = c("PRIME_MOVER" = "Reported.Prime.Mover", "ENERGY_SOURCE_1" = "Reported.Fuel.Type.Code")) %>%
  filter(gcam_fuel == "coal") %>%
  select( -gcam_fuel, -ENERGY_SOURCE_1, -PRIME_MOVER, -elec_tech)


coal_units_2015 <-
  eia_860_data_2015 %>%
  select(State, Plant.Name, Plant.Code, Generator.ID, Operating.Year, Planned.Retirement.Year, Prime.Mover, Energy.Source.1, Summer.Capacity..MW., Nameplate.Capacity..MW.) %>%
  mutate(unit.capacity.MW = if_else(is.na(Nameplate.Capacity..MW.), Summer.Capacity..MW., Nameplate.Capacity..MW.)) %>%
  select(-Summer.Capacity..MW., -Nameplate.Capacity..MW.) %>%
  left_join(prime_mover_map, by = c("Prime.Mover" = "Reported.Prime.Mover", "Energy.Source.1" = "Reported.Fuel.Type.Code")) %>%
  filter(gcam_fuel == "coal") %>%
  select( -gcam_fuel, -Energy.Source.1, -Prime.Mover, -elec_tech)

# Creating database of units retired until 2016. Using nameplate capacity for now. Not sure if summer capacity
# makes more sense

coal_units_retired <-
  eia_860_retired_2016 %>%
  filter(Status == "RE") %>%
  select(State, Plant.Code, Plant.Name, Generator.ID, Technology, Prime.Mover, Energy.Source.1, Operating.Year, Nameplate.Capacity..MW., Summer.Capacity..MW., Retirement.Year) %>%
  mutate(unit.capacity.MW = if_else(is.na(Nameplate.Capacity..MW.),Summer.Capacity..MW., Nameplate.Capacity..MW.)) %>%
  select(-Summer.Capacity..MW., -Nameplate.Capacity..MW.) %>%
  left_join(prime_mover_map, by = c("Prime.Mover" = "Reported.Prime.Mover", "Energy.Source.1" = "Reported.Fuel.Type.Code")) %>%
  filter(gcam_fuel == "coal") %>%
  select(-Technology, -gcam_fuel, -Prime.Mover, -Energy.Source.1, -elec_tech) %>%
  # Adding vintage bins for now for the sake of ease of independent processing
  mutate(vintage.bin = cut(Operating.Year, breaks = vintage.breaks, labels = vintage.lables))

# Obtain unit/generator -level generation in 2010 and 2015 from generator-level generation data in Form 923.

# First, calculate generator-level generation in 2010

coal_units_gen_2010 <-
  coal_units_2010 %>%
  left_join(eia_923_data_2010 %>%
              select(State, Plant.ID, Plant.Name, Generator.ID, Net.Generation.Year.to.Date ) %>%
              rename(unit.gen.2010 = Net.Generation.Year.to.Date ) , by = c("STATE" = "State", "GENERATOR_ID" = "Generator.ID", "PLANT_CODE" = "Plant.ID", "PLANT_NAME" = "Plant.Name")) %>%
  mutate(unit.gen.2010= as.double(unit.gen.2010)) %>%
  mutate(unit.gen.2010= if_else(is.na(unit.gen.2010), 0, unit.gen.2010))
  ##### The above processing results in U.S. coal generation in 2010 to equal 1843080582 MWh which is 0.2% less than 
  ##### coal generation reported in EIA's Electricity data browser (1847 TWh).

# Then calculate generator-level generation in 2015

coal_units_gen_2015 <- 
  coal_units_2015 %>%
  left_join(eia_923_data_2015 %>%
              select(State = Plant.State, Plant.Id, Plant.Name, Generator.Id, Net.Generation.Year.To.Date ) %>%
              rename(unit.gen.2015 = Net.Generation.Year.To.Date ), by = c("State", "Generator.ID" = "Generator.Id", "Plant.Code" = "Plant.Id", "Plant.Name")) %>%
  mutate(unit.gen.2015= as.double(unit.gen.2015)) %>%
  mutate(unit.gen.2015 = if_else(is.na(unit.gen.2015), 0, unit.gen.2015))

  ##### The above processing results in U.S. coal generation in 2015 to equal 1333572580 MWh which is 1.4% less than 
  ##### coal generation reported in EIA's Electricity data browser (1352 TWh).

              

# Combine unit-level generation and capacity data with retirement data. This will generate a database of coal units operating in 2015.
# The Retirement.year variable reflects actual retirements through 2016 or planned retirements.

coal_units_gen_2010 <- 
  coal_units_gen_2010 %>%
  left_join(coal_units_retired %>% select(-unit.capacity.MW, -Operating.Year, - vintage.bin), 
            by = c("STATE" = "State", "PLANT_CODE"="Plant.Code", "PLANT_NAME" = "Plant.Name", "GENERATOR_ID" = "Generator.ID")) %>%
  # Note that NAs in Retirement year column indicate that the units were not retired through 2016.Repalce NAs with zeroes for now
  mutate(Retirement.Year = as.double(Retirement.Year), PLANNED_RETIREMENT_YEAR = as.double(PLANNED_RETIREMENT_YEAR)) %>%
  mutate(Retirement.Year = if_else(is.na(Retirement.Year), PLANNED_RETIREMENT_YEAR, Retirement.Year)) %>%
  mutate(Retirement.Year = if_else(is.na(Retirement.Year), 9999, Retirement.Year))


coal_units_gen_2015 <- 
  coal_units_gen_2015 %>%
  left_join(coal_units_retired %>% select(-unit.capacity.MW, -Operating.Year, -vintage.bin), 
            by = c("State", "Plant.Code", "Plant.Name",  "Generator.ID")) %>%
  # Note that NAs in Retirement year column indicate that the units were not retired through 2016.Repalce NAs with zeroes for now
  mutate(Retirement.Year = as.double(Retirement.Year), Planned.Retirement.Year = as.double(Planned.Retirement.Year)) %>%
  mutate(Retirement.Year = if_else(is.na(Retirement.Year), Planned.Retirement.Year, Retirement.Year))%>%
  mutate(Retirement.Year = if_else(is.na(Retirement.Year), 9999, Retirement.Year))

# Categorizing coal units by vintage bins. The bins are defined early on in the script.
# We stick with 5-year bins for now.

coal_units_gen_2010 <- 
  coal_units_gen_2010 %>%
  mutate(vintage.bin = cut(OPERATING_YEAR, breaks = vintage.breaks, labels = vintage.lables)) 

coal_units_gen_2015 <- 
  coal_units_gen_2015 %>%
  mutate(vintage.bin = cut(Operating.Year, breaks =vintage.breaks, labels =vintage.lables)) 


# Now estimate retirement years for those without retirement data.For units built before 1970, we assume that 
# the lifetime for units without retirement information equals the national average lifetime of units retired in that vintage bin
# through 2016. For units built after 1970, we assume an average lifetime defined above.

coal_units_retired_avg_lifetime <-
  coal_units_retired %>%
  filter(Operating.Year <= 1970) %>%
  mutate(lifetime = Retirement.Year - Operating.Year) %>%
  mutate(cap.lifetime = unit.capacity.MW * lifetime) %>%
  group_by(vintage.bin) %>%
  summarise(avg.lifetime = sum(cap.lifetime)/sum(unit.capacity.MW))

coal_units_gen_2010 <-
  coal_units_gen_2010 %>%
  left_join(coal_units_retired_avg_lifetime, by = c("vintage.bin")) %>%
  mutate(Retirement.Year = if_else(OPERATING_YEAR <=1970 & Retirement.Year == 9999, round(OPERATING_YEAR + avg.lifetime,0), Retirement.Year)) %>%
  mutate(Retirement.Year = if_else(OPERATING_YEAR >1970 & Retirement.Year == 9999, round(OPERATING_YEAR + avg_coal_plant_lifetime,0), Retirement.Year)) %>%
  select(-avg.lifetime)
  
  

coal_units_gen_2015 <-
  coal_units_gen_2015 %>%
  left_join(coal_units_retired_avg_lifetime, by = c("vintage.bin")) %>%
  mutate(Retirement.Year = if_else(Operating.Year <=1970 & Retirement.Year == 9999, round(Operating.Year + avg.lifetime,0), Retirement.Year)) %>%
  mutate(Retirement.Year = if_else(Operating.Year >1970 & Retirement.Year == 9999, round(Operating.Year + avg_coal_plant_lifetime,0), Retirement.Year)) %>%
  select(-avg.lifetime)


# Creating tables of generation in 2010 and 2015 by state and vintage bin

coal_vintage_gen_2010 <- 
  coal_units_gen_2010 %>%
  group_by(STATE, vintage.bin) %>%
  summarise(vintage.gen = sum(unit.gen.2010)) %>%
  mutate(vintage.gen = vintage.gen*conv_MWh_EJ) %>%
  ungroup() 

coal_vintage_gen_2015 <- 
  coal_units_gen_2015 %>%
  group_by(State, vintage.bin) %>%
  summarise(vintage.gen = sum(unit.gen.2015)) %>%
  mutate(vintage.gen = vintage.gen*conv_MWh_EJ) %>%
  ungroup() 

# Creating table of capacity-weighted lifetime from 2010 by state and vintage based on 2010 units

coal_vintage_lifetime_2010 <- 
  coal_units_gen_2010 %>%
  # Calculate lifetime from 2010 which is model bas year
  mutate(lifetime = Retirement.Year - 2010) %>%
  mutate(capacity.lifetime = unit.capacity.MW*lifetime) %>%
  group_by(STATE, vintage.bin) %>%
  summarise(lifetime = sum(capacity.lifetime)/sum(unit.capacity.MW))%>%
  mutate(lifetime = round(lifetime,0)) %>%
  # If lifetimes are less than 5, just fix them at 5 so that they are retired in the very next period
  mutate(lifetime = if_else(lifetime <=5, 5, lifetime)) %>%
  ungroup() 

coal_vintage_gen_2010 <-
  coal_vintage_gen_2010 %>%
  left_join(coal_vintage_lifetime_2010, by = c("STATE", "vintage.bin") )%>%
  # Keeping share of each vintage bin of the total generation in 2010 in each state ready to be applied to
  # calibrated value in 2010
  group_by(STATE) %>%
  mutate(share.vintage = vintage.gen/ sum(vintage.gen)) %>%
  ungroup() 


# Creating table of capacity-weighted lifetime from 2010 by state and vintage based on 2015 units

coal_vintage_lifetime_2015 <- 
  coal_units_gen_2015 %>%
  # Calculate lifetime from 2010 which is model base year
  mutate(lifetime = Retirement.Year - 2010) %>%
  mutate(capacity.lifetime = unit.capacity.MW*lifetime) %>%
  group_by(State, vintage.bin) %>%
  summarise(lifetime = sum(capacity.lifetime)/sum(unit.capacity.MW))%>%
  mutate(lifetime = round(lifetime,0)) %>%
  # If lifetimes are less than 5, we filter those out because we're already trying to "soft calibrate" 2015 using 
  # the fast-retire split. For 2020 retirements, we'll chose only those vintages with lifetimes >=5 and <10
  filter(lifetime >=5) %>%
  mutate(lifetime = if_else(lifetime <=10, 10, lifetime)) %>%
  ungroup() 

coal_vintage_gen_2015 <-
  coal_vintage_gen_2015 %>%
  left_join(coal_vintage_lifetime_2015, by = c("State", "vintage.bin") )%>%
  # Keeping share of each vintage bin of the total generation in 2015 in each state ready to be applied to
  # calibrated value in 2010
  filter(!is.na(lifetime)) %>%
  group_by(State) %>%
  mutate(share.vintage = vintage.gen/ sum(vintage.gen)) %>%
  ungroup() 


# Apply vintage share by state to calibrated values for slow retire component and create table to be read in

L2246.StubTechProd_coal_vintage_USA <-
  coal_vintage_gen_2015 %>%
  left_join(StubTechProd_elec_USA %>% 
              filter(subsector =="coal", year == "2010", calOutputValue != 0) %>%
              filter(grepl("slow_retire",stub.technology)) %>%
              select(region, supplysector, subsector, stub.technology, calOutputValue), by = c("State" = "region")) %>%
  filter(!is.na(calOutputValue)) %>%
  mutate(calOutputValue = calOutputValue * share.vintage) %>%
  rename(region = State) %>%
  # Creating new technologies. Naming the variable as stub.technology.new so that we can use stub.technology as reference later 
  mutate(stub.technology.new = paste(stub.technology, vintage.bin, sep = " ")) %>%
  mutate(year = 2010, share.weight.year = 2010, subs.share.weight = 1, share.weight = 1) %>%
  # Select variables. For now, include lifetime and vintage.bin as well. We'll remove it later
  select(region, supplysector, subsector, stub.technology.new, stub.technology, vintage.bin, year, calOutputValue, share.weight.year, subs.share.weight, share.weight, lifetime)

# Creating a new varibale that has stubtechprod by vintage and lifetime to simulate model output later
L2246.StubTechProd_coal_vintage_USA_lifetime <- L2246.StubTechProd_coal_vintage_USA


# Create table to read lifetimes for vintage bin techs by state
L2246.StubTechLifetime_coal_vintage_USA <-
  L2246.StubTechProd_coal_vintage_USA %>%
  filter(year == 2010)  %>%
  select(region, supplysector, subsector, stub.technology = stub.technology.new, year, lifetime)

# Create a table to read in energy inputs and efficiencies for the new technologies in calibration years. Assuming
# state average efficiency for all vintages since historically, there is not much correlation between 
# efficiency of a generator and its vintage.

L2246.StubTechEff_coal_vintage_USA <-
  L2246.StubTechProd_coal_vintage_USA %>%
  repeat_and_add_vector("year", model_base_years) %>%
  left_join(StubTechEff_elec_USA, by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
  select(region, supplysector, subsector, stub.technology = stub.technology.new, year, minicam.energy.input, efficiency, market.name)


# Create table to read in energy inputs for future years in global technology database

L2246.GlobalTechEff_coal_vintage_USA <-
  L2246.StubTechProd_coal_vintage_USA %>%
  group_by(supplysector, subsector, stub.technology, stub.technology.new, year) %>%
  summarise() %>%
  ungroup() %>%
  repeat_and_add_vector("year", model_years) %>%
  left_join(GlobalTechEff_elec_USA, by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
  select(supplysector, subsector, technology = stub.technology.new, year, minicam.energy.input, efficiency)

# Create table to read in non-energy inputs: capital costs, fixed and variable OM costs

L2246.GlobalTechCapFac_coal_vintage_USA <-
  L2246.StubTechProd_coal_vintage_USA %>%
  group_by(supplysector, subsector, stub.technology, stub.technology.new, year) %>%
  summarise() %>%
  ungroup() %>%
  repeat_and_add_vector("year", model_years) %>%
  left_join(GlobalTechCapFac_elec_USA, by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
  select(supplysector, subsector, technology = stub.technology.new, year, capacity.factor)

L2246.GlobalTechCapital_coal_vintage_USA <-
  L2246.StubTechProd_coal_vintage_USA %>%
  group_by(supplysector, subsector, stub.technology, stub.technology.new, year) %>%
  summarise() %>%
  ungroup() %>%
  repeat_and_add_vector("year", model_years) %>%
  left_join(GlobalTechCapital_elec_USA, by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
  select(supplysector, subsector, technology = stub.technology.new, year, input.capital, capital.overnight, fixed.charge.rate)

L2246.GlobalTechOMfixed_coal_vintage_USA <-
  L2246.StubTechProd_coal_vintage_USA %>%
  group_by(supplysector, subsector, stub.technology, stub.technology.new, year) %>%
  summarise() %>%
  ungroup() %>%
  repeat_and_add_vector("year", model_years) %>%
  left_join(GlobalTechOMfixed_elec_USA, by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
  select(supplysector, subsector, technology = stub.technology.new, year, input.OM.fixed, OM.fixed)

L2246.GlobalTechOMvar_coal_vintage_USA <-
  L2246.StubTechProd_coal_vintage_USA %>%
  group_by(supplysector, subsector, stub.technology, stub.technology.new, year) %>%
  summarise() %>%
  ungroup() %>%
  repeat_and_add_vector("year", model_years) %>%
  left_join(GlobalTechOMvar_elec_USA, by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
  select(supplysector, subsector, technology = stub.technology.new, year, input.OM.var, OM.var)


# Create table to read in shareweights in future years in global technology database
L2246.GlobalTechShrwt_coal_vintage_USA <-
  L2246.GlobalTechEff_coal_vintage_USA %>%
  select(-minicam.energy.input, -efficiency) %>%
  mutate(share.weight = 0)

# Clean up StubTechProd table

L2246.StubTechProd_coal_vintage_USA <- 
  L2246.StubTechProd_coal_vintage_USA %>%
  select(region, supplysector, subsector, stub.technology = stub.technology.new, year, calOutputValue, share.weight.year, subs.share.weight, share.weight)


# Read in zero caloutputvalue for other base years

L2246.StubTechProd_coal_vintage_USA <- 
  L2246.StubTechProd_coal_vintage_USA %>%
  bind_rows( L2246.StubTechProd_coal_vintage_USA%>%
               repeat_and_add_vector("year", model_base_years) %>%
               filter(year != 2010) %>%
               mutate(calOutputValue =0,share.weight.year =year, subs.share.weight = 1,  share.weight = 0))


# Read in zero calOutputValue for 2010 for existing coal conv pul technology
coal_vintage_gen_2015_states <- unique(coal_vintage_gen_2015$State)

L2246.StubTechProd_coal_vintage_USA <- 
  L2246.StubTechProd_coal_vintage_USA %>%
  bind_rows(StubTechProd_elec_USA %>% 
              filter(subsector =="coal", year == "2010", calOutputValue != 0)%>%
              filter(grepl("slow_retire", stub.technology)) %>%
              filter(region %in% coal_vintage_gen_2015_states) %>%
              mutate(calOutputValue = 0, share.weight = 0)) %>%
  arrange(region, year)


# Read in profit shutdown decider for  vintage technologies

L2246.StubTechProfitShutdown_coal_vintage_USA <-
  L2246.StubTechLifetime_coal_vintage_USA %>%
  mutate(median.shutdown.point = -0.1, profit.shutdown.steepness = 6) %>%
  select(-lifetime)

# Read in energy inputs for future periods

L2246.StubTechMarket_coal_vintage_USA <-
  L2246.StubTechEff_coal_vintage_USA %>%
  select(-efficiency) %>%
  filter(year == 2010) %>%
  repeat_and_add_vector("year",model_future_years) 


# -----------------------------------------------------------------------------
 # 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
 
write_mi_data( L2246.StubTechProd_coal_vintage_USA, "StubTechProd", "GCAMUSA_LEVEL2_DATA", "L2246.StubTechProd_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" )
write_mi_data( L2246.StubTechEff_coal_vintage_USA, "StubTechEff", "GCAMUSA_LEVEL2_DATA", "L2246.StubTechEff_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" )
write_mi_data( L2246.StubTechLifetime_coal_vintage_USA, "StubTechLifetime", "GCAMUSA_LEVEL2_DATA", "L2246.StubTechLifetime_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" ) 
write_mi_data( L2246.StubTechProfitShutdown_coal_vintage_USA, "StubTechProfitShutdown", "GCAMUSA_LEVEL2_DATA", "L2246.StubTechProfitShutdown_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" ) 
write_mi_data( L2246.GlobalTechShrwt_coal_vintage_USA, "GlobalTechShrwt", "GCAMUSA_LEVEL2_DATA", "L2246.GlobalTechShrwt_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" ) 
write_mi_data( L2246.GlobalTechEff_coal_vintage_USA, "GlobalTechEff", "GCAMUSA_LEVEL2_DATA", "L2246.GlobalTechEff_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" ) 
write_mi_data( L2246.StubTechMarket_coal_vintage_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L2246.StubTechMarket_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" )
write_mi_data( L2246.GlobalTechCapFac_coal_vintage_USA, "GlobalTechCapFac", "GCAMUSA_LEVEL2_DATA", "L2246.GlobalTechCapFac_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" )
write_mi_data( L2246.GlobalTechCapital_coal_vintage_USA, "GlobalTechCapital", "GCAMUSA_LEVEL2_DATA", "L2246.GlobalTechCapital_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" )
write_mi_data( L2246.GlobalTechOMfixed_coal_vintage_USA, "GlobalTechOMfixed", "GCAMUSA_LEVEL2_DATA", "L2246.GlobalTechOMfixed_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" )
write_mi_data( L2246.GlobalTechOMvar_coal_vintage_USA, "GlobalTechOMvar", "GCAMUSA_LEVEL2_DATA", "L2246.GlobalTechOMvar_coal_vintage_USA", "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml" )


insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_coal_vintage_USA.xml", "GCAMUSA_XML_FINAL", "coal_vintage_USA.xml", "", xml_tag="outFile" )

logstop()
 

