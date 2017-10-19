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
logstart( "L2239.CSP_reeds_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for harmonizing GCAM-USA CSP resource with ReEDS" )

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


reeds_CSP_curve_capacity <- readdata( "GCAMUSA_LEVEL0_DATA", "reeds_CSP_curve_capacity", skip = 1  )
reeds_CSP_curve_CF <- readdata( "GCAMUSA_LEVEL0_DATA", "reeds_CSP_curve_CF", skip = 1  )
reeds_CSP_curve_grid_cost <- readdata( "GCAMUSA_LEVEL0_DATA", "reeds_CSP_curve_grid_cost", skip = 2  )

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

printlog( "L2239.CSP_CF: Capacity factors for CSP systems by class" )
# Calculating average capacity factor by CSP class. Note that capacity factor data by region is not available. 
# Hence we assume same representative capacity factors by class across states. Also note that since capacity factors are given
# by timeslice, we need to ignore 0 capacity factors. 

reeds_CSP_curve_CF %>% 
  group_by(class) %>%
  summarise_if(is.numeric, funs(mean(.[.!=0]))) %>%
  ungroup () ->  L2239.CSP_CF

printlog( "L2239.CSP_potential_EJ: Resource potential in EJ by state and class" )

# We then calculate the resource potential in EJ in each ReEDS region and class my multiplying the 
# potential in MW with the average representative capacity factor for each class obtained above.  
# This is then aggregated up to the state-level.

reeds_CSP_curve_capacity %>%
  replace_na(list(cspsc1=0, cspsc2=0, cspsc3=0, cspsc4=0, cspsc5=0)) %>%
  mutate (resource.potential.MW = cspsc1+cspsc2+cspsc3+cspsc4+cspsc5) %>%
  select(Region, CSP.class,resource.potential.MW) %>%
  left_join(L2239.CSP_CF, by = c("CSP.class"= "class")) %>%
  mutate(resource.potential.EJ = resource.potential.MW*8760*conv_MWh_EJ) %>%
  left_join(reedsregions_states, by = "Region") %>%
  select(State, CSP.class, resource.potential.EJ) %>%
  group_by(State, CSP.class) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() -> L2239.CSP_potential_EJ
  
printlog( "L2239.CSP_matrix: Creating a matrix of costs (1975$/GJ) and resource potential (EJ) by state and class" )

L2233.GlobalIntTechCapital_elec_itc %>%
  filter(intermittent.technology == "CSP", year == set_years("final-calibration-year")) %>%
  select(capital.overnight) -> L2239.CSP_capital

L2239.CSP_capital <- as.numeric(L2239.CSP_capital)

L2233.GlobalIntTechCapital_elec_itc %>%
  filter(intermittent.technology == "CSP", year == set_years("final-calibration-year")) %>%
  select(fixed.charge.rate) -> L2239.fcr

L2239.fcr <- as.numeric(L2239.fcr)

L223.GlobalIntTechOMfixed_elec %>%
  filter(intermittent.technology == "CSP", year == set_years("final-calibration-year")) %>%
  select(OM.fixed) -> L2239.CSP_OMfixed

L2239.CSP_OMfixed <- as.numeric(L2239.CSP_OMfixed)

L2239.CSP_potential_EJ %>%
  left_join(L2239.CSP_CF, by = c("CSP.class" ="class")) %>% 
  mutate(capital.overnight = L2239.CSP_capital, fcr = L2239.fcr, OM.fixed = L2239.CSP_OMfixed) %>%
  mutate(price = fcr*capital.overnight/CF/8760/conv_kwh_GJ + OM.fixed/CF/8760/conv_kwh_GJ) -> L2239.CSP_matrix

# From the matrix of costs and supplies obtained above, we create a graded resource curve for Pvar versus supply. 
# Unlike the graded resource curves for depletable sources,each grade for the renewresource represents 
# the fraction of maximum resource (cumulative) and price. Hence, we first create a matrix of cumulative resource
# and price (Pvar). 

L2239.CSP_matrix %>%
  group_by(State) %>%
  arrange(State, price) %>%
  mutate(Pmin = min(price), Pvar = price - Pmin, CFmax = max(CF)) %>%
  mutate(available = round(resource.potential.EJ,3),extraction.cost = round(Pvar,3),CFmax = round(CFmax,3)) %>%
  mutate(grade = "grade") %>%
  mutate(grade_number = row_number()) %>%
  mutate (grade = paste (grade, grade_number, sep = ' ')) %>%  
  mutate(available = cumsum(available)) %>%
  ungroup() %>%
  select(State, grade, available, extraction.cost, CFmax) -> L2239.CSP_curve

# Calculating maxSubResource for the graded renewable resource supply curves

L2239.CSP_curve %>%
  group_by(State) %>%
  arrange(State, extraction.cost) %>%
  mutate(maxSubResource = max(available)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(State,maxSubResource) -> L2239.maxSubResource_CSP

# The points on the graded curves need to be read in as fractions of the maxSubResource
L2239.CSP_curve %>%
  left_join(L2239.maxSubResource_CSP, by ="State") %>%
  mutate(available = available/maxSubResource) %>%
  # Adjusting the curves so that we have a supply of 0 at a Pvar of 0. The available resource potential is accounted
  # for in the subsequent grade because of the cumulative calculation above.
  mutate(available= if_else(grade == "grade 1", 0, available)) -> L2239.CSP_curve
  
# Technological change in the supply curve is related to assumed improvements in capital cost.
# If capital cost changes from CC to a.CC, then every price point of the curve will scale by a factor a' given as follows:
# a' = (k1.a.CC + k2. OM-fixed) / (k1.CC + k2. OM-fixed) where k1 = FCR/(8760*kWh_GJ) and k2 = 1/(8760*kWh_GJ)
# Thus, we calculate model input parameter techChange (which is the reduction per year) as 1-a'^(1/5). This approach ignores
# changes in fixed OM costs over time. 

L2233.GlobalIntTechCapital_elec_itc %>%
  filter(intermittent.technology == "CSP") %>%
  select(year, capital.overnight) %>%
  mutate(capital.overnight.lag = lag(capital.overnight, 1)) %>%
  mutate(capital.tech.change.5yr = capital.overnight.lag/capital.overnight) %>%
  mutate(fixed.charge.rate = L2239.fcr, OM.fixed = L2239.CSP_OMfixed) %>%
  mutate(k1 = fixed.charge.rate/ (8760*conv_kwh_GJ), k2 = 1/(8760*conv_kwh_GJ)) %>%
  mutate(tech.change.5yr = (k1* capital.tech.change.5yr * capital.overnight + k2*OM.fixed)/(k1*capital.overnight + k2*OM.fixed)) %>%
  mutate(tech.change = round(abs(1-(tech.change.5yr)^(1/5)),5)) %>%
  select(year, tech.change) %>%
  filter(is.na(tech.change) == "FALSE") %>%
  filter(year > set_years("final-calibration-year"))-> L2239.CSP_curve_tech_change

# Grid connection costs are read in as fixed non-energy cost adders (in $/GJ) that vary by state. Our starting data comprises of grid connection
# costs in $/MW by ReEDS region and CSP class. This data also categorizes the connection cost into five bins in each region and class. 
# We first calculate the average cost for a region and class.Using this data, we then obtain grid connection cost
# in $/GJ for each region and class as FCR* (grid connection cost in $/MW) /(8760*CF*MWh_GJ). Costs are then obtained for a 
# state by averaging. In the future, we might think about a separate state-level curve for grid connection costs. 

reeds_CSP_curve_grid_cost %>%
  gather(bin, cost, -Region, -CSP.class) %>%
  replace_na(list(cost =0)) %>%
  group_by(Region, CSP.class) %>%
  summarise (cost = mean(cost)) %>%
  ungroup() %>%
  left_join(L2239.CSP_CF, by =c("CSP.class" ="class")) %>%
  mutate(fcr = L2239.fcr) %>%
  mutate(grid.cost = fcr*cost/(8760*CF*conv_MWh_GJ)) %>%
  mutate(grid.cost = grid.cost* conv_2004_1975_USD,3) %>%
  left_join(reedsregions_states, by = "Region") %>%
  select(State, Region, CSP.class, grid.cost) %>%
  group_by(State) %>%
  summarise (grid.cost = mean(grid.cost)) %>%
  ungroup() %>%
  mutate(grid.cost = round(grid.cost,5))-> L2239.grid.cost

# Preparing final tables to convert to level-2 csvs
# First populate the list of states we will be creating supply cuvres for. These are the states
# with at least two points. For all other states, we will assume constant marginal costs regardless of 
# deployment.

L2239.CSP_curve %>%
  filter(grade == "grade 2") -> states_list_curve_temp

states_list_curve <- unique(states_list_curve_temp$State)


# Capacity factors at the technology level need to be updated for all states that have the resource available.
# Hence, creating a list of all states.

states_list <- unique(L2239.CSP_curve$State)

# Table to delete global solar resource in states that have CSP resource. Since all states have PV_resource,
# only the ones that have CSP resource need to be included in this table. This needs to be checked though.

L2239.CSP_curve %>%
  filter(grade == "grade 2") %>%
  filter(State %in% states_list_curve) %>%
  select(State) %>%
  mutate(unlimited.resource = "global solar resource") -> L2239.DeleteUnlimitRsrc_USA_reeds
  
  
# Table to read in renewresource, output.unit, price.unit and market

states_subregions %>%
  select(region = state) %>%
  mutate(renewresource = "CSP_resource", output.unit = "EJ", price.unit = "1975$/GJ", market = region) %>%
  filter(region %in% states_list_curve)-> L2239.RenewRsrc_CSP_USA_reeds

# Table to create the graded resource curves
L2239.CSP_curve %>%
  mutate (renewresource = "CSP_resource", sub.renewable.resource = "CSP_resource") %>%
  select(region = State, renewresource, sub.renewable.resource, grade, available, extraction.cost) %>%
  filter(region %in% states_list_curve)-> L2239.GrdRenewRsrcCurves_CSP_USA_reeds

# Table to read in maximum resource
L2239.maxSubResource_CSP %>%
  mutate(renewresource = "CSP_resource", sub.renewable.resource = "CSP_resource") %>%
  select(region = State,renewresource,sub.renewable.resource, maxSubResource ) %>%
  filter(region %in% states_list_curve)-> L2239.GrdRenewRsrcMax_CSP_USA_reeds

# Table to delete global solar resource minicam-energy-input
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechMarket_elecS_USA %>%
    filter(region %in% states_list_curve) %>%
    filter(grepl("CSP",stub.technology )) %>%
    mutate(minicam.energy.input = "global solar resource") %>%
    select(region, supplysector, subsector, stub.technology, year, minicam.energy.input)-> L2239.DeleteStubTechMinicamEnergyInput_CSP_USA_reeds
  
  } else {
  
    L223.StubTechMarket_elec_USA %>%
    filter(region %in% states_list_curve) %>%
    filter(grepl("CSP",stub.technology )) %>%
    mutate(minicam.energy.input = "global solar resource") %>%
    select(region, supplysector, subsector, stub.technology, year, minicam.energy.input)-> L2239.DeleteStubTechMinicamEnergyInput_CSP_USA_reeds
}


# Table to read in energy inputs at the technology level
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechMarket_elecS_USA %>%
    filter(region %in% states_list_curve) %>%
    filter(grepl("CSP",stub.technology )) %>%
    mutate(minicam.energy.input = "CSP_resource", market.name = region, efficiency = 1) %>%
    select(region, supplysector, subsector, stub.technology, year, minicam.energy.input, efficiency, market.name)-> L2239.StubTechEff_CSP_USA_reeds
  
} else {

    L223.StubTechMarket_elec_USA %>%
    filter(region %in% states_list_curve) %>%
    filter(grepl("CSP",stub.technology )) %>%
    mutate(minicam.energy.input = "CSP_resource", market.name = region, efficiency = 1) %>%
    select(region, supplysector, subsector, stub.technology, year, minicam.energy.input, efficiency, market.name)-> L2239.StubTechEff_CSP_USA_reeds
  
 }

# Table to read in region-specific CFmax (that will be used to calculate Pmin within the model)
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechCapFactor_elecS_solar_USA %>%
    filter(region %in% states_list) %>%
    filter(grepl("CSP",stub.technology )) %>%
    filter(grepl("storage",stub.technology) == FALSE) %>%
    left_join(L2239.CSP_curve, by= c("region" = "State")) %>%
    filter(is.na(CFmax) == "FALSE") %>%
    mutate(capacity.factor.capital = round(CFmax,5), capacity.factor.OM = round(CFmax,5)) %>%
    select(region, supplysector, subsector, stub.technology, year, 
           input.capital, capacity.factor.capital, input.OM.fixed, capacity.factor.OM) -> L2239.StubTechCapFactor_CSP_nostorage_USA_reeds
  
  # We read in higher CFmax for CSP technologies with dedicated thermal storage. This is in contrast to our approach 
  # for wind and PV because CSP w/ thermal storage is argued to achieve better capacity factors compared to CSP systems without
  # storage in the literature.
  # Howeevr, wind w/ battery storage and PV w/ battery storage technologies do not really have solid arguments for 
  # higher capacity factors compared to their intermittent counterparts in the literature. 
  # We give a capacity factor credit of 0.2 to CSP with thermal storage technologies. This is rather arbitrary and needs 
  # looking into.
  # See also Muratori et al. 2017: Cost of power or power of cost: A US modeling perspective, Renewable and Sustainable Energy Reviews, 
  # 77, pp.861-874. 
  
  L2234.StubTechCapFactor_elecS_solar_USA %>%
    filter(region %in% states_list) %>%
    filter(grepl("CSP",stub.technology )) %>%
    filter(grepl("storage",stub.technology) == TRUE) %>%
    left_join(L2239.CSP_curve, by= c("region" = "State")) %>%
    filter(is.na(CFmax) == "FALSE") %>%
    mutate(capacity.factor.capital = round(CFmax+0.2,5), capacity.factor.OM = round(CFmax+0.2,5)) %>%
    select(region, supplysector, subsector, stub.technology, year, 
           input.capital, capacity.factor.capital, input.OM.fixed, capacity.factor.OM) -> L2239.StubTechCapFactor_CSP_storage_USA_reeds
  
  L2239.StubTechCapFactor_CSP_nostorage_USA_reeds %>%
    bind_rows(L2239.StubTechCapFactor_CSP_storage_USA_reeds) -> L2239.StubTechCapFactor_CSP_USA_reeds
  
} else{
  L223.StubTechCapFactor_elec_solar_USA %>%
    filter(region %in% states_list) %>%
    filter(stub.technology == "CSP") %>%
    left_join(L2239.CSP_curve, by= c("region" = "State")) %>%
    filter(is.na(CFmax) == "FALSE") %>%
    mutate(capacity.factor.capital = round(CFmax,5), capacity.factor.OM = round(CFmax,5)) %>%
    select(region, supplysector, subsector, stub.technology, year, 
           input.capital, capacity.factor.capital, input.OM.fixed, capacity.factor.OM) -> L2239.StubTechCapFactor_CSP_nostorage_USA_reeds
 
  # We read in higher CFmax for CSP technologies with dedicated thermal storage. This is in contrast to our approach 
  # for wind and PV because CSP w/ thermal storage is argued to achieve better capacity factors compared to CSP systems without
  # storage in the literature.
  # Howeevr, wind w/ battery storage and PV w/ battery storage technologies do not really have solid arguments for 
  # higher capacity factors compared to their intermittent counterparts in the literature. 
  # We give a capacity factor credit of 0.2 to CSP with thermal storage technologies. This is rather arbitrary and needs 
  # looking into.
  # See also Muratori et al. 2017: Cost of power or power of cost: A US modeling perspective, Renewable and Sustainable Energy Reviews, 
  # 77, pp.861-874.
  
  L223.StubTechCapFactor_elec_solar_USA %>%
    filter(region %in% states_list) %>%
    filter(stub.technology == "CSP_storage") %>%
    left_join(L2239.CSP_curve, by= c("region" = "State")) %>%
    filter(is.na(CFmax) == "FALSE") %>%
    mutate(capacity.factor.capital = round(CFmax + 0.2,5), capacity.factor.OM = round(CFmax + 0.2,5)) %>%
    select(region, supplysector, subsector, stub.technology, year, 
           input.capital, capacity.factor.capital, input.OM.fixed, capacity.factor.OM) -> L2239.StubTechCapFactor_CSP_storage_USA_reeds
  
  L2239.StubTechCapFactor_CSP_nostorage_USA_reeds %>%
    bind_rows(L2239.StubTechCapFactor_CSP_storage_USA_reeds) -> L2239.StubTechCapFactor_CSP_USA_reeds
}
  

# Copying tech change to all states and filtering out only the contiguous states

L2239.RenewRsrcTechChange_CSP_USA_reeds <- write_to_all_states(L2239.CSP_curve_tech_change,c("region", "year","tech.change"))
L2239.RenewRsrcTechChange_CSP_USA_reeds %>%
  filter(region %in% states_list_curve) %>%
  mutate (renewresource = "CSP_resource", sub.renewable.resource = "CSP_resource") %>%
  select(region,renewresource, sub.renewable.resource,year.fillout = year, techChange = tech.change) -> L2239.RenewRsrcTechChange_CSP_USA_reeds

# Reading the grid connection cost as a state-level non-energy cost adder
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechCapFactor_elecS_solar_USA %>%
    filter(region %in% states_list) %>%
    filter(grepl("CSP",stub.technology )) %>%
    select(region, supplysector, subsector, stub.technology, year) %>%
    mutate(minicam.energy.input = "regional price adjustment") %>%
    left_join(L2239.grid.cost, by = c("region" = "State")) %>%
    rename (input.cost = grid.cost) %>%
    filter(is.na(input.cost) == "FALSE")-> L2239.StubTechCost_CSP_USA_reeds

  } else {
  
    L223.StubTechCapFactor_elec_solar_USA %>%
    filter(region %in% states_list) %>%
    filter(grepl("CSP",stub.technology )) %>%
    select(region, supplysector, subsector, stub.technology, year) %>%
    mutate(minicam.energy.input = "regional price adjustment") %>%
    left_join(L2239.grid.cost, by = c("region" = "State")) %>%
    rename (input.cost = grid.cost) %>%
    filter(is.na(input.cost) == "FALSE")-> L2239.StubTechCost_CSP_USA_reeds
    }
 
# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2239.DeleteUnlimitRsrc_USA_reeds, "DeleteUnlimitRsrc", "GCAMUSA_LEVEL2_DATA", "L2239.DeleteUnlimitRsrc_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2239.DeleteStubTechMinicamEnergyInput_CSP_USA_reeds, "DeleteStubTechMinicamEnergyInput", "GCAMUSA_LEVEL2_DATA", "L2239.DeleteStubTechMinicamEnergyInput_CSP_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2239.RenewRsrc_CSP_USA_reeds, "RenewRsrc", "GCAMUSA_LEVEL2_DATA", "L2239.RenewRsrc_CSP_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2239.GrdRenewRsrcCurves_CSP_USA_reeds, "GrdRenewRsrcCurves", "GCAMUSA_LEVEL2_DATA", "L2239.GrdRenewRsrcCurves_CSP_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2239.GrdRenewRsrcMax_CSP_USA_reeds, "GrdRenewRsrcMax", "GCAMUSA_LEVEL2_DATA", "L2239.GrdRenewRsrcMax_CSP_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2239.StubTechEff_CSP_USA_reeds, "StubTechEff", "GCAMUSA_LEVEL2_DATA", "L2239.StubTechEff_CSP_USA_reedss", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2239.StubTechCapFactor_CSP_USA_reeds, "StubTechCapFactor", "GCAMUSA_LEVEL2_DATA", "L2239.StubTechCapFactor_CSP_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2239.RenewRsrcTechChange_CSP_USA_reeds, "RenewRsrcTechChange", "GCAMUSA_LEVEL2_DATA", "L2239.RenewRsrcTechChange_CSP_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )
write_mi_data( L2239.StubTechCost_CSP_USA_reeds, "StubTechCost", "GCAMUSA_LEVEL2_DATA", "L2239.StubTechCost_CSP_USA_reeds", "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_solar_USA_reeds.xml", "GCAMUSA_XML_FINAL", "solar_USA_reeds.xml", "", xml_tag="outFile" )

logstop()
