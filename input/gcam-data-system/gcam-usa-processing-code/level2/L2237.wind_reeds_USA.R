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
logstart( "L2237.wind_reeds_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for harmonizing GCAM-USA wind resource with ReEDS" )

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


reeds_wind_curve_capacity <- readdata( "GCAMUSA_LEVEL0_DATA", "reeds_wind_curve_capacity", skip = 1  )
reeds_wind_curve_CF_avg <- readdata( "GCAMUSA_LEVEL0_DATA", "reeds_wind_curve_CF_avg" )
reeds_wind_curve_grid_cost <- readdata( "GCAMUSA_LEVEL0_DATA", "reeds_wind_curve_grid_cost", skip = 2  )

L223.StubTechCapFactor_elec_wind_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L223.StubTechCapFactor_elec_wind_USA" , skip = 4 )
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechCapFactor_elecS_wind_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.StubTechCapFactor_elecS_wind_USA" , skip = 4 )
}
L2233.GlobalIntTechCapital_elec_itc <- readdata( "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechCapital_elec_itc", skip = 4 )

L223.GlobalIntTechOMfixed_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMfixed_elec", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations

printlog( "L2237.wind_CF_reeds: Capacity factor by ReEDS region and class" )
# Assigning average capacity factor by ReEDS region and class to a variable which will then be used to 
# come up with an average capacity factor by state and class.

L2237.wind_CF_reeds <- reeds_wind_curve_CF_avg

printlog( "L2237.wind_CF: Capacity factor by state and wind class" )
L2237.wind_CF_reeds %>%
  left_join(reedsregions_states, by = c("Wind.Resource.Region" = "Region")) %>%
  select(State, Wind.Class = TRG, CF ) %>%
  group_by(State, Wind.Class) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() -> L2237.wind_CF

printlog( "L2237.wind_potential_EJ: Resource potential in EJ by state and class" )


# We first calculate the resource potential in EJ in each ReEDS region and class using the 
# potential in MW with the average capacity factor for each region and class. We then aggregate this to the state-level

reeds_wind_curve_capacity %>%
  mutate (resource.potential.MW = wsc1+wsc2+wsc3+wsc4+wsc5) %>%
  select(Wind.Resource.Region, Wind.Class,resource.potential.MW) %>%
  left_join(L2237.wind_CF_reeds, by = c("Wind.Resource.Region" , "Wind.Class" = "TRG")) %>%
  mutate(resource.potential.EJ = resource.potential.MW*8760*CF*conv_MWh_EJ) %>%
  left_join(reedsregions_states, by = c("Wind.Resource.Region" = "Region")) %>%
  select(State, Wind.Class, resource.potential.EJ) %>%
  group_by(State, Wind.Class) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() -> L2237.wind_potential_EJ
  
printlog( "L2237.wind_matrix: Creating a matrix of costs (1975$/GJ) and cumulative resource potential (EJ) by state and class" )

L2233.GlobalIntTechCapital_elec_itc %>%
  filter(intermittent.technology == "wind", year == set_years("final-calibration-year")) %>%
  select(capital.overnight) -> L2237.wind_capital

L2237.wind_capital <- as.numeric(L2237.wind_capital)

L2233.GlobalIntTechCapital_elec_itc %>%
  filter(intermittent.technology == "wind", year == set_years("final-calibration-year")) %>%
  select(fixed.charge.rate) -> L2237.fcr

L2237.fcr <- as.numeric(L2237.fcr)

L223.GlobalIntTechOMfixed_elec %>%
  filter(intermittent.technology == "wind", year == set_years("final-calibration-year")) %>%
  select(OM.fixed) -> L2237.wind_OMfixed

L2237.wind_OMfixed <- as.numeric(L2237.wind_OMfixed)

L2237.wind_potential_EJ %>%
  left_join(L2237.wind_CF, by = c("State","Wind.Class")) %>% 
  group_by(State) %>%
  arrange(State, desc(CF)) %>%
  mutate(CFmax = max(CF)) %>%
  mutate(supply = cumsum(resource.potential.EJ)) %>%
  ungroup() %>%
  mutate(capital.overnight = L2237.wind_capital, fcr = L2237.fcr, OM.fixed = L2237.wind_OMfixed) %>%
  mutate(price = fcr*capital.overnight/CF/8760/conv_kwh_GJ + OM.fixed/CF/8760/conv_kwh_GJ) %>%
  select(State, price, supply, CFmax) -> L2237.wind_matrix

printlog( "L2237.wind_curve: estimating parameters of the smooth curve" )

# We need to define two useful functions that will be used later: evaluate_smooth_res_curve, and
# smooth_res_curve_approx_error. 
# The function, evaluate_smooth_res_curve, computes the smooth renewable resource function
# supply = (p - Pmin) ^ curve.exponent / (mid.price ^ curve.exponent +
#    (p - Pmin) ^ curve.exponent * maxSubResource

# TODO: Define these as global functions

evaluate_smooth_res_curve <- function(curve.exponent, mid.price, Pmin, maxSubResource, p) {
  p_pow_exp <- ( p - Pmin ) ^ curve.exponent
  supply <- p_pow_exp / ( mid.price ^ curve.exponent + p_pow_exp ) * maxSubResource
  # zero out the supply where the price was less than Pmin
  supply[ p < Pmin ] <- 0
  return( supply )
}

# The function, smooth_res_curve_approx_error, checks how well the given smooth renewable curve matches the given supply-points.
# Note that the first argument is the one that is changed by optimize when trying to minimize the error
smooth_res_curve_approx_error <- function(curve.exponent, mid.price, Pmin, maxSubResource, supply_points) {
  f_p <- evaluate_smooth_res_curve( curve.exponent, mid.price, Pmin, maxSubResource, supply_points$price )
  error <- f_p - supply_points$supply
  return( crossprod( error, error ) )
}

# Calculate maxSubResource, Pmin, and Pvar.
# Pmin represents the minimum cost of generating electricity from the resource. 
# Pmin comprises of the cost of generating power at the most optimal location.
# Pvar represents costs that are expected to increase from Pmin as deployment increases. 
# This models the increase in costs as more optimal locations are used first. 

L2237.wind_matrix %>%
  group_by(State) %>%
  arrange(State, price) %>%
  mutate(Pmin = min(price), Pvar = price - Pmin, maxSubResource = round(max(supply),5)) %>%
  ungroup() -> L2237.wind_curve

# Approximate mid-price using first supply points that are less than (p1, Q1) and greater than (p2,Q2) 50% of maxSubResource.
# Using these points, the mid-price can be estimated as:
# mid.price = ((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1))
# This assumes that the curve is largely linear between the two points above.

# Calculating P1 and Q1
L2237.wind_curve %>%
  mutate(percent.supply = supply/maxSubResource) %>%
  group_by(State) %>%
  arrange(State, desc(price)) %>%
  filter( percent.supply <= 0.5) %>%
  filter(row_number() == 1) %>%
  select(State, P1 = Pvar, Q1 = supply, maxSubResource) -> L2237.mid.price_1

# Calculating P2 and Q2
L2237.wind_curve %>%
  mutate(percent.supply = supply/maxSubResource) %>%
  group_by(State) %>%
  filter( percent.supply >= 0.5) %>%
  filter(row_number() == 1) %>%
  select(State, P2 = Pvar, Q2 = supply) -> L2237.mid.price_2
 
# Calculating mid.price
L2237.mid.price_1 %>%
  left_join(L2237.mid.price_2, by = "State") %>%
  mutate(mid.price = round(((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1)),5)) %>%
  select(State, mid.price) -> L2237.mid.price

L2237.wind_curve %>%
  left_join(L2237.mid.price, by = c("State")) -> L2237.wind_curve

# Defining variables to be used later. Note that the ReEDS data includes information for the 48 contiguous states only.
# For now, since this script creates add-on files, we'll assume that the existing curves in the remaining states are good enough.
states_list <- unique(L2237.wind_curve$State)
L2237.curve.exponent <- tibble()

for (L2237.state in states_list) {
  
  L2237.wind_curve %>%
    filter(State == L2237.state) -> L2237.wind_curve_state 
  
  L2237.wind_curve_state %>%
    select(price, supply) -> L2237.supply_points_state
  
  L2237.error_min_curve.exp <- optimize(f = smooth_res_curve_approx_error, interval=c(1.0,15.0),
                                        L2237.wind_curve_state$mid.price,
                                        L2237.wind_curve_state$Pmin,
                                        L2237.wind_curve_state$maxSubResource,
                                        L2237.supply_points_state )
  
  L2237.wind_curve_state$curve.exponent <-  round(L2237.error_min_curve.exp$minimum,5)
  L2237.wind_curve_state %>%
    select(State, curve.exponent) %>%
    filter(row_number() == 1) -> L2237.curve.exponent_state
  
  L2237.curve.exponent %>% 
    bind_rows(L2237.curve.exponent_state) -> L2237.curve.exponent
  
}
  

L2237.wind_curve %>%
  left_join(L2237.curve.exponent, by = "State") -> L2237.wind_curve


# Technological change in the supply curve is related to assumed improvements in capital cost.
# If capital cost changes from CC to a.CC, then every price point of the curve will scale by a factor a' given as follows:
# a' = (k1.a.CC + k2. OM-fixed) / (k1.CC + k2. OM-fixed) where k1 = FCR/(8760*kWh_GJ) and k2 = 1/(8760*kWh_GJ)
# Thus, we calculate model input parameter techChange (which is the reduction per year) as 1-a'^(1/5)

L2233.GlobalIntTechCapital_elec_itc %>%
  filter(intermittent.technology == "wind") %>%
  select(year, capital.overnight) %>%
  mutate(capital.overnight.lag = lag(capital.overnight, 1)) %>%
  mutate(capital.tech.change.5yr = capital.overnight.lag/capital.overnight) %>%
  mutate(fixed.charge.rate = L2237.fcr, OM.fixed = L2237.wind_OMfixed) %>%
  mutate(k1 = fixed.charge.rate/ (8760*conv_kwh_GJ), k2 = 1/(8760*conv_kwh_GJ)) %>%
  mutate(tech.change.5yr = (k1* capital.tech.change.5yr * capital.overnight + k2*OM.fixed)/(k1*capital.overnight + k2*OM.fixed)) %>%
  mutate(tech.change = round(abs(1-(tech.change.5yr)^(1/5)),5)) %>%
  select(year, tech.change) %>%
  filter(is.na(tech.change) == "FALSE") %>%
  filter(year > set_years("final-calibration-year"))-> L2237.wind_curve_tech_change


# Grid connection costs are read in as fixed non-energy cost adders (in $/GJ) that vary by state. Our starting data comprises of grid connection
# costs in $/MW by ReEDS region and wind class. This data also categorizes the connection cost into five bins in each region and class. 
# We first calculate the average cost for a region and class.Using this data, we then obtain grid connection cost
# in $/GJ for each region and class as FCR* (grid connection cost in $/MW) /(8760*CF*MWh_GJ). Costs are then obtained for a 
# state by averaging. In the future, we might think about a separate state-level curve for grid connection costs. 

reeds_wind_curve_grid_cost %>%
  select(-Wind.Type) %>%
  gather(bin, cost, -Wind.Resource.Region, -Wind.Class) %>%
  group_by(Wind.Resource.Region, Wind.Class) %>%
  summarise (cost = mean(cost)) %>%
  ungroup() %>%
  left_join(L2237.wind_CF_reeds, by =c("Wind.Resource.Region", "Wind.Class" = "TRG")) %>%
  mutate(fcr = L2237.fcr) %>%
  mutate(grid.cost = fcr*cost/(8760*CF*conv_MWh_GJ)) %>%
  mutate(grid.cost = grid.cost* conv_2013_1975_USD,3) %>%
  left_join(reedsregions_states, by = c("Wind.Resource.Region" = "Region")) %>%
  select(State, Wind.Resource.Region, Wind.Class, grid.cost) %>%
  group_by(State, Wind.Class) %>%
  summarise (grid.cost = mean(grid.cost)) %>%
  ungroup() %>%
  group_by(State) %>%
  summarise (grid.cost = mean(grid.cost)) %>%
  ungroup() %>%
  mutate(grid.cost = round(grid.cost,5))-> L2237.grid.cost

# Preparing final tables to convert to level-2 csvs

L2237.wind_curve %>%
  mutate (renewresource = "onshore wind resource", smooth.renewable.subresource = "onshore wind resource") %>%
  select(region = State, renewresource, smooth.renewable.subresource, maxSubResource, mid.price, curve.exponent) %>%
  group_by(region) %>%
  filter(row_number() == 1) %>%
  ungroup()-> L2237.SmthRenewRsrcCurves_wind_USA_reeds

if(use_mult_load_segments == "TRUE") {
  L2234.StubTechCapFactor_elecS_wind_USA %>%
    left_join(L2237.wind_curve, by= c("region" = "State")) %>%
    filter(is.na(CFmax) == "FALSE") %>%
    mutate(capacity.factor.capital = round(CFmax,5), capacity.factor.OM = round(CFmax,5)) %>%
    select(region, supplysector, subsector, stub.technology, year, 
           input.capital, capacity.factor.capital, input.OM.fixed, capacity.factor.OM) -> L2237.StubTechCapFactor_wind_USA_reeds
} else{
  L223.StubTechCapFactor_elec_wind_USA %>%
    left_join(L2237.wind_curve, by= c("region" = "State")) %>%
    filter(is.na(CFmax) == "FALSE") %>%
    mutate(capacity.factor.capital = round(CFmax,5), capacity.factor.OM = round(CFmax,5)) %>%
    select(region, supplysector, subsector, stub.technology, year, 
           input.capital, capacity.factor.capital, input.OM.fixed, capacity.factor.OM) -> L2237.StubTechCapFactor_wind_USA_reeds
}
  

# Copying tech change to all states and filtering out only the contiguous states

L2237.SmthRenewRsrcTechChange_wind_USA_reeds <- write_to_all_states(L2237.wind_curve_tech_change,c("region", "year","tech.change"))
L2237.SmthRenewRsrcTechChange_wind_USA_reeds %>%
  filter(region %in% states_list) %>%
  mutate (renewresource = "onshore wind resource", smooth.renewable.subresource = "onshore wind resource") %>%
  select(region,renewresource, smooth.renewable.subresource,year.fillout = year, techChange = tech.change) -> L2237.SmthRenewRsrcTechChange_wind_USA_reeds

# Reading the grid connection cost as a state-level non-energy cost adder
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechCapFactor_elecS_wind_USA %>%
    select(region, supplysector, subsector, stub.technology, year) %>%
    mutate(minicam.energy.input = "regional price adjustment") %>%
    left_join(L2237.grid.cost, by = c("region" = "State")) %>%
    rename (input.cost = grid.cost) %>%
    filter(is.na(input.cost) == "FALSE")-> L2237.StubTechCost_wind_USA_reeds
} else {
  L223.StubTechCapFactor_elec_wind_USA %>%
    select(region, supplysector, subsector, stub.technology, year) %>%
    mutate(minicam.energy.input = "regional price adjustment") %>%
    left_join(L2237.grid.cost, by = c("region" = "State")) %>%
    rename (input.cost = grid.cost) %>%
    filter(is.na(input.cost) == "FALSE")-> L2237.StubTechCost_wind_USA_reeds
    }
 
# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2237.SmthRenewRsrcCurves_wind_USA_reeds, "SmthRenewRsrcCurves", "GCAMUSA_LEVEL2_DATA", "L2237.SmthRenewRsrcCurves_wind_USA_reeds", "GCAMUSA_XML_BATCH", "batch_wind_USA_reeds.xml" )
write_mi_data( L2237.StubTechCapFactor_wind_USA_reeds, "StubTechCapFactor", "GCAMUSA_LEVEL2_DATA", "L2237.StubTechCapFactor_wind_USA_reeds", "GCAMUSA_XML_BATCH", "batch_wind_USA_reeds.xml" )
write_mi_data( L2237.SmthRenewRsrcTechChange_wind_USA_reeds, "SmthRenewRsrcTechChange", "GCAMUSA_LEVEL2_DATA", "L2237.SmthRenewRsrcTechChange_wind_USA_reeds", "GCAMUSA_XML_BATCH", "batch_wind_USA_reeds.xml" )
write_mi_data( L2237.StubTechCost_wind_USA_reeds, "StubTechCost", "GCAMUSA_LEVEL2_DATA", "L2237.StubTechCost_wind_USA_reeds", "GCAMUSA_XML_BATCH", "batch_wind_USA_reeds.xml" )

#write_mi_data( L2237.wind_curve, "StubTechCost", "GCAMUSA_LEVEL2_DATA", "L2237.wind_curve", "GCAMUSA_XML_BATCH", "batch_wind_USA_reeds.xml" )


insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_wind_USA_reeds.xml", "GCAMUSA_XML_FINAL", "wind_USA_reeds.xml", "", xml_tag="outFile" )

logstop()
