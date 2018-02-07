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
logstart( "L2238.PV_reeds_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for harmonizing GCAM-USA PV resource with ReEDS" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )

reedsregions_states <- readdata( "GCAMUSA_MAPPINGS", "reedsregions_states" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )


reeds_PV_curve_capacity <- readdata( "GCAMUSA_LEVEL0_DATA", "reeds_PV_curve_capacity", skip = 1  )
reeds_PV_curve_CF_avg <- readdata( "GCAMUSA_LEVEL0_DATA", "reeds_PV_curve_CF_avg" )
reeds_PV_curve_grid_cost <- readdata( "GCAMUSA_LEVEL0_DATA", "reeds_PV_curve_grid_cost", skip = 2  )

L223.StubTechCapFactor_elec_solar_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L223.StubTechCapFactor_elec_solar_USA" , skip = 4 )
L223.StubTechMarket_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L223.StubTechMarket_elec_USA" , skip = 4 )
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechCapFactor_elecS_solar_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.StubTechCapFactor_elecS_solar_USA" , skip = 4 )
  L2234.StubTechMarket_elecS_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.StubTechMarket_elecS_USA" , skip = 4 )
}
L2233.GlobalIntTechCapital_elec_itc <- readdata( "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechCapital_elec_itc", skip = 4 )

L223.GlobalIntTechOMfixed_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMfixed_elec", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations

printlog( "L2238.PV_CF_reeds: Capacity factor by ReEDS region and class" )
# Assigning average capacity factor by ReEDS region and class to a variable which will then be used to 
# come up with an average capacity factor by state and class.

L2238.PV_CF_reeds <- reeds_PV_curve_CF_avg

printlog( "L2238.PV_CF: Capacity factor by state and PV class" )
L2238.PV_CF_reeds %>%
  left_join(reedsregions_states, by = "BA") %>%
  select(State, PV.class, CF ) %>%
  group_by(State, PV.class) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() -> L2238.PV_CF

printlog( "L2238.PV_potential_EJ: Resource potential in EJ by state and class" )

# We first calculate the resource potential in EJ in each ReEDS region and class using the 
# potential in MW with the average capacity factor for each region and class. We then aggregate this to the state-level 

reeds_PV_curve_capacity %>%
  mutate (resource.potential.MW = upvsc1+upvsc2+upvsc3+upvsc4+upvsc5) %>%
  select(BA, PV.class,resource.potential.MW) %>%
  left_join(L2238.PV_CF_reeds, by = c("BA" , "PV.class")) %>%
  mutate(resource.potential.EJ = resource.potential.MW*8760*CF*conv_MWh_EJ) %>%
  left_join(reedsregions_states, by = "BA") %>%
  select(State, PV.class, resource.potential.EJ) %>%
  group_by(State, PV.class) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() -> L2238.PV_potential_EJ
  
printlog( "L2238.PV_matrix: Creating a matrix of costs (1975$/GJ) and resource potential (EJ) by state and class" )

L2233.GlobalIntTechCapital_elec_itc %>%
  filter(intermittent.technology == "PV", year == set_years("final-calibration-year")) %>%
  select(capital.overnight) -> L2238.PV_capital

L2238.PV_capital <- as.numeric(L2238.PV_capital)

L2233.GlobalIntTechCapital_elec_itc %>%
  filter(intermittent.technology == "PV", year == set_years("final-calibration-year")) %>%
  select(fixed.charge.rate) -> L2238.fcr

L2238.fcr <- as.numeric(L2238.fcr)

L223.GlobalIntTechOMfixed_elec %>%
  filter(intermittent.technology == "PV", year == set_years("final-calibration-year")) %>%
  select(OM.fixed) -> L2238.PV_OMfixed

L2238.PV_OMfixed <- as.numeric(L2238.PV_OMfixed)

L2238.PV_potential_EJ %>%
  left_join(L2238.PV_CF, by = c("State","PV.class")) %>% 
  mutate(capital.overnight = L2238.PV_capital, fcr = L2238.fcr, OM.fixed = L2238.PV_OMfixed) %>%
  mutate(price = fcr*capital.overnight/CF/8760/conv_kwh_GJ + OM.fixed/CF/8760/conv_kwh_GJ) -> L2238.PV_matrix

# We noticed that there are some classes with same capacity factor data. For example, in VT, class 3 and 4 have same
# capacity factors. This is becuase the hourly capacity factor data from ReEDS are also the same for these classes. This
# could be an error in the data. We get around this simply by making sure that the duplicate points in terms of capacity
# factors are removed and the resource potentials in those classes are accounted for. 
L2238.PV_matrix %>%
  group_by(State, CF, price) %>%
  summarise(resource.potential.EJ = sum(resource.potential.EJ)) %>%
  ungroup() %>%
  group_by(State) %>%
  arrange(State, desc(CF)) %>%
  mutate(CFmax = max(CF)) %>%
  ungroup() -> L2238.PV_matrix

# From the matrix of costs and supplies obtained above, we create a graded resource curve for Pvar versus supply. 
# Unlike the graded resource curves for depletable sources,each grade for the renewresource represents 
# the fraction of maximum resource (cumulative) and price. Hence, we first create a matrix of cumulative resource
# and price (Pvar). 

L2238.PV_matrix %>%
  group_by(State) %>%
  arrange(State, price) %>%
  mutate(Pmin = min(price), Pvar = price - Pmin) %>%
  mutate(available = round(resource.potential.EJ,3),extraction.cost = round(Pvar,3),CFmax = round(CFmax,3)) %>%
  mutate(grade = "grade") %>%
  mutate(grade_number = row_number()) %>%
  mutate (grade = paste (grade, grade_number, sep = ' ')) %>%  
  mutate(available = cumsum(available)) %>%
  ungroup() %>%
  select(State, grade, available, extraction.cost, CFmax) -> L2238.PV_curve

# Calculating maxSubResource for the graded renewable resource supply curve

L2238.PV_curve %>%
  group_by(State) %>%
  arrange(State, extraction.cost) %>%
  mutate(maxSubResource = max(available)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(State,maxSubResource) -> L2238.maxSubResource_PV

# The points on the graded curves need to be read in as fractions of the maxSubResource
L2238.PV_curve %>%
  left_join(L2238.maxSubResource_PV , by ="State") %>%
  mutate(available = available/maxSubResource) %>%
# Adjusting the curves so that we have a supply of 0 at a Pvar of 0. The available resource potential is accounted
# for in the subsequent grade because of the cumulative calculation above. 
  mutate(available= if_else(grade == "grade 1", 0, available)) %>%
# Removing duplicate grades within states.  This only impacts WA grade 5, 
# which has just 0.00012 EJ of resource and thus the same available fraction (1) as grade 4.
  distinct(State, available, .keep_all = TRUE) -> L2238.PV_curve
  
# Technological change in the supply curve is related to assumed improvements in capital cost.
# If capital cost changes from CC to a.CC, then every price point of the curve will scale by a factor a' given as follows:
# a' = (k1.a.CC + k2. OM-fixed) / (k1.CC + k2. OM-fixed) where k1 = FCR/(8760*kWh_GJ) and k2 = 1/(8760*kWh_GJ)
# Thus, we calculate model input parameter techChange (which is the reduction per year) as 1-a'^(1/5)

L2233.GlobalIntTechCapital_elec_itc %>%
  filter(intermittent.technology == "PV") %>%
  select(year, capital.overnight) %>%
  mutate(capital.overnight.lag = lag(capital.overnight, 1)) %>%
  mutate(capital.tech.change.5yr = capital.overnight.lag/capital.overnight) %>%
  mutate(fixed.charge.rate = L2238.fcr, OM.fixed = L2238.PV_OMfixed) %>%
  mutate(k1 = fixed.charge.rate/ (8760*conv_kwh_GJ), k2 = 1/(8760*conv_kwh_GJ)) %>%
  mutate(tech.change.5yr = (k1* capital.tech.change.5yr * capital.overnight + k2*OM.fixed)/(k1*capital.overnight + k2*OM.fixed)) %>%
  mutate(tech.change = round(abs(1-(tech.change.5yr)^(1/5)),5)) %>%
  select(year, tech.change) %>%
  filter(is.na(tech.change) == "FALSE") %>%
  filter(year > set_years("final-calibration-year"))-> L2238.PV_curve_tech_change


# Grid connection costs are read in as fixed non-energy cost adders (in $/GJ) that vary by state. Our starting data comprises of grid connection
# costs in $/MW by ReEDS region and PV class. This data also categorizes the connection cost into five bins in each region and class. 
# We first calculate the average cost for a region and class.Using this data, we then obtain grid connection cost
# in $/GJ for each region and class as FCR* (grid connection cost in $/MW) /(8760*CF*MWh_GJ). Costs are then obtained for a 
# state by averaging. In the future, we might think about a separate state-level curve for grid connection costs. 

reeds_PV_curve_grid_cost %>%
  gather(bin, cost, -BA, -PV.class) %>%
  group_by(BA, PV.class) %>%
  summarise (cost = mean(cost)) %>%
  ungroup() %>%
  left_join(L2238.PV_CF_reeds, by =c("BA", "PV.class")) %>%
  mutate(fcr = L2238.fcr) %>%
  mutate(grid.cost = fcr*cost/(8760*CF*conv_MWh_GJ)) %>%
  mutate(grid.cost = grid.cost* conv_2005_1975_USD,3) %>%
  left_join(reedsregions_states, by = "BA") %>%
  select(State, BA, PV.class, grid.cost) %>%
  group_by(State) %>%
  summarise (grid.cost = mean(grid.cost)) %>%
  ungroup() %>%
  mutate(grid.cost = round(grid.cost,5))-> L2238.grid.cost

# Preparing final tables to convert to level-2 csvs
# First populate the list of states we will be creating supply cuvres for. These are the states
# with at least two points. For all other states, we will assume constant marginal costs regardless of 
# deployment.

L2238.PV_curve %>%
  filter(grade == "grade 2") -> states_list_curve_temp

states_list_curve <- unique(states_list_curve_temp$State)

# Capacity factors at the technology level need to be updated for all states that have the resource available.
# Hence, creating a list of all states.

states_list <- unique(L2238.PV_curve$State)

# Table to read in renewresource, output.unit, price.unit and market

states_subregions %>%
  select(region = state) %>%
  mutate(renewresource = "PV_resource", output.unit = "EJ", price.unit = "1975$/GJ", market = region) %>%
  filter(region %in% states_list_curve)-> L2238.RenewRsrc_PV_USA_reeds

# Table to create the graded resource curves
L2238.PV_curve %>%
  mutate (renewresource = "PV_resource", sub.renewable.resource = "PV_resource") %>%
  select(region = State, renewresource, sub.renewable.resource, grade, available, extraction.cost) %>%
  filter(region %in% states_list_curve)-> L2238.GrdRenewRsrcCurves_PV_USA_reeds

# Table to read in maximum resource
L2238.maxSubResource_PV %>%
  mutate(renewresource = "PV_resource", sub.renewable.resource = "PV_resource") %>%
  select(region = State,renewresource,sub.renewable.resource, maxSubResource ) %>%
  filter(region %in% states_list_curve)-> L2238.GrdRenewRsrcMax_PV_USA_reeds

# Table to delete global solar resource minicam-energy-input
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechMarket_elecS_USA %>%
    filter(region %in% states_list_curve) %>%
    filter(grepl("PV",stub.technology )) %>%
    mutate(minicam.energy.input = "global solar resource") %>%
    select(region, supplysector, subsector, stub.technology, year, minicam.energy.input)-> L2238.DeleteStubTechMinicamEnergyInput_PV_USA_reeds
} else {
  L223.StubTechMarket_elec_USA %>%
    filter(region %in% states_list_curve) %>%
    filter(grepl("PV",stub.technology )) %>%
    mutate(minicam.energy.input = "global solar resource") %>%
    select(region, supplysector, subsector, stub.technology, year, minicam.energy.input)-> L2238.DeleteStubTechMinicamEnergyInput_PV_USA_reeds
}


# Table to read in energy inputs at the technology level
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechMarket_elecS_USA %>%
    filter(region %in% states_list_curve) %>%
    filter(grepl("PV",stub.technology )) %>%
    mutate(minicam.energy.input = "PV_resource", market.name = region, efficiency = 1) %>%
    select(region, supplysector, subsector, stub.technology, year, minicam.energy.input, efficiency, market.name)-> L2238.StubTechEff_PV_USA_reeds
} else {
  L223.StubTechMarket_elec_USA %>%
    filter(region %in% states_list_curve) %>%
    filter(grepl("PV",stub.technology )) %>%
    mutate(minicam.energy.input = "PV_resource", market.name = region, efficiency = 1) %>%
    select(region, supplysector, subsector, stub.technology, year, minicam.energy.input, efficiency, market.name)-> L2238.StubTechEff_PV_USA_reeds 
  
  }

if(use_mult_load_segments == "TRUE") {
  L2234.StubTechCapFactor_elecS_solar_USA %>%
    filter(region %in% states_list) %>%
    filter(grepl("PV",stub.technology )) %>%
    left_join(L2238.PV_curve, by= c("region" = "State")) %>%
    filter(is.na(CFmax) == "FALSE") %>%
    mutate(capacity.factor.capital = round(CFmax,5), capacity.factor.OM = round(CFmax,5)) %>%
    select(region, supplysector, subsector, stub.technology, year, 
           input.capital, capacity.factor.capital, input.OM.fixed, capacity.factor.OM) -> L2238.StubTechCapFactor_PV_USA_reeds
} else{
  L223.StubTechCapFactor_elec_solar_USA %>%
    filter(region %in% states_list) %>%
    filter(grepl("PV",stub.technology )) %>%
    left_join(L2238.PV_curve, by= c("region" = "State")) %>%
    filter(is.na(CFmax) == "FALSE") %>%
    mutate(capacity.factor.capital = round(CFmax,5), capacity.factor.OM = round(CFmax,5)) %>%
    select(region, supplysector, subsector, stub.technology, year, 
           input.capital, capacity.factor.capital, input.OM.fixed, capacity.factor.OM) -> L2238.StubTechCapFactor_PV_USA_reeds
}
  

# Copying tech change to all states and filtering out only the contiguous states

L2238.RenewRsrcTechChange_PV_USA_reeds <- write_to_all_states(L2238.PV_curve_tech_change,c("region", "year","tech.change"))
L2238.RenewRsrcTechChange_PV_USA_reeds %>%
  filter(region %in% states_list_curve) %>%
  mutate (renewresource = "PV_resource", sub.renewable.resource = "PV_resource") %>%
  select(region,renewresource, sub.renewable.resource,year.fillout = year, techChange = tech.change) -> L2238.RenewRsrcTechChange_PV_USA_reeds

# Reading the grid connection cost as a state-level non-energy cost adder
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechCapFactor_elecS_solar_USA %>%
    filter(region %in% states_list) %>%
    filter(grepl("PV",stub.technology )) %>%
    select(region, supplysector, subsector, stub.technology, year) %>%
    mutate(minicam.energy.input = "regional price adjustment") %>%
    left_join(L2238.grid.cost, by = c("region" = "State")) %>%
    rename (input.cost = grid.cost) %>%
    filter(is.na(input.cost) == "FALSE")-> L2238.StubTechCost_PV_USA_reeds
} else {
  L223.StubTechCapFactor_elec_solar_USA %>%
    filter(region %in% states_list) %>%
    filter(grepl("PV",stub.technology )) %>%
    select(region, supplysector, subsector, stub.technology, year) %>%
    mutate(minicam.energy.input = "regional price adjustment") %>%
    left_join(L2238.grid.cost, by = c("region" = "State")) %>%
    rename (input.cost = grid.cost) %>%
    filter(is.na(input.cost) == "FALSE")-> L2238.StubTechCost_PV_USA_reeds
    }
 
# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2238.DeleteStubTechMinicamEnergyInput_PV_USA_reeds, "DeleteStubTechMinicamEnergyInput", "GCAMUSA_LEVEL2_DATA", "L2238.DeleteStubTechMinicamEnergyInput_PV_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2238.RenewRsrc_PV_USA_reeds, "RenewRsrc", "GCAMUSA_LEVEL2_DATA", "L2238.RenewRsrc_PV_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2238.GrdRenewRsrcCurves_PV_USA_reeds, "GrdRenewRsrcCurves", "GCAMUSA_LEVEL2_DATA", "L2238.GrdRenewRsrcCurves_PV_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2238.GrdRenewRsrcMax_PV_USA_reeds, "GrdRenewRsrcMax", "GCAMUSA_LEVEL2_DATA", "L2238.GrdRenewRsrcMax_PV_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2238.StubTechEff_PV_USA_reeds, "StubTechEff", "GCAMUSA_LEVEL2_DATA", "L2238.StubTechEff_PV_USA_reedss", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2238.StubTechCapFactor_PV_USA_reeds, "StubTechCapFactor", "GCAMUSA_LEVEL2_DATA", "L2238.StubTechCapFactor_PV_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2238.RenewRsrcTechChange_PV_USA_reeds, "RenewRsrcTechChange", "GCAMUSA_LEVEL2_DATA", "L2238.RenewRsrcTechChange_PV_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2238.StubTechCost_PV_USA_reeds, "StubTechCost", "GCAMUSA_LEVEL2_DATA", "L2238.StubTechCost_PV_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml", "GCAMUSA_XML_FINAL", "solar_USA_reeds.xml", "", xml_tag="outFile" )

logstop()
