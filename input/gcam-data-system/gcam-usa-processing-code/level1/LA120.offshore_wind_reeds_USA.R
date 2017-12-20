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
logstart( "LA120.offshore_wind_reeds_USA.R" )
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

A23.globaltech_capital <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_capital", skip = 1 )
A23.globaltech_OMfixed <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_OMfixed", skip = 1 )
A20.offshore_wind_depth_cap_cost <- readdata( "ENERGY_ASSUMPTIONS", "A20.offshore_wind_depth_cap_cost", skip = 1 )

reedsregions_states <- readdata( "GCAMUSA_MAPPINGS", "reedsregions_states" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

A20.offshore_wind_class_depth <- readdata( "GCAMUSA_ASSUMPTIONS", "A20.offshore_wind_class_depth", skip = 1 )

reeds_offshore_wind_curve_capacity <- readdata( "GCAMUSA_LEVEL0_DATA", "reeds_offshore_wind_curve_capacity", skip = 1  )
reeds_offshore_wind_curve_CF_avg <- readdata( "ENERGY_LEVEL0_DATA", "reeds_offshore_wind_curve_CF_avg")
reeds_offshore_wind_curve_grid_cost <- readdata( "ENERGY_LEVEL0_DATA", "reeds_offshore_wind_curve_grid_cost", skip = 2 )

# -----------------------------------------------------------------------------
# 2. Perform computations

printlog( "L120.offshore_wind_CF_reeds: Capacity factor by ReEDS region and class" )
# Assigning average capacity factor by ReEDS region and class to a variable which will then be used to 
# come up with an average capacity factor by state and class.

L120.offshore_wind_CF_reeds <- reeds_offshore_wind_curve_CF_avg


printlog( "L120.offshore_wind_CF: Capacity factor by state and wind class" )
L120.offshore_wind_CF_reeds %>%
  left_join(reedsregions_states, by = c("Wind.Resource.Region" = "Region")) %>%
  select(State, Wind.Class = TRG, CF) %>%
  group_by(State, Wind.Class) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() -> L120.offshore_wind_CF


printlog( "L120.offshore_wind_potential_EJ: Resource potential in EJ by state and class" )
# We first calculate the resource potential in EJ in each ReEDS region and class using the 
# potential in MW with the average capacity factor for each region and class. 
# We then aggregate this to the state-level.

reeds_offshore_wind_curve_capacity %>%
  mutate (resource.potential.MW = wsc1+wsc2+wsc3+wsc4+wsc5) %>%
  select(Wind.Resource.Region, Wind.Class, resource.potential.MW) %>%
  left_join(L120.offshore_wind_CF_reeds, by = c("Wind.Resource.Region" , "Wind.Class" = "TRG")) %>%
  mutate(resource.potential.EJ = resource.potential.MW*8760*CF*conv_MWh_EJ) %>%
  left_join(reedsregions_states, by = c("Wind.Resource.Region" = "Region")) %>%
  select(State, Wind.Class, resource.potential.EJ) %>%
  group_by(State, Wind.Class) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() -> L120.offshore_wind_potential_EJ

# TODO:  not sure why, but 11 state/class combinations have CFs but no capacity... see:
# L120.offshore_wind_CF %>% anti_join(L120.offshore_wind_potential_EJ, by = c("State" , "Wind.Class")) -> DIFF


printlog( "L120.offshore_wind_matrix: Creating a matrix of costs (1975$/GJ) and cumulative resource potential (EJ) by state and class" )

A20.offshore_wind_class_depth %>%
  left_join(A20.offshore_wind_depth_cap_cost, by = c("depth_class")) %>%
  select(Wind.Class, capital.overnight) -> L2231.offshore_wind_capital

A23.globaltech_capital %>%
  filter(technology == "wind_offshore") %>%
  select(fixed.charge.rate) -> L120.offshore_wind_fcr
L120.offshore_wind_fcr <- as.numeric(L120.offshore_wind_fcr)

A23.globaltech_OMfixed %>%
  filter(technology == "wind_offshore") %>%
  select(X_final_historical_year) -> L120.offshore_wind_OMfixed
L120.offshore_wind_OMfixed <- as.numeric(L120.offshore_wind_OMfixed)

# NOTE that the process for calculating supply/ price is different for offshore wind (vs. onshore wind).  For offshore wind, we 
# (1) calculate the price associated with each wind class, then (2) arrange the dataset by region/ price and calculate 
# cumulative resource supply. (Conversely, for onshore wind, we (1) arrange the data by capacity factor and calculate supply, 
# then (2) calculate the corresponding prices.)  The reason for this difference is that, for offshore wind, price varies with 
# both capacity factor AND ocean depth.  (For onshore wind, CF is the only major determinant of price.)  Thus, arranging by CF 
# and calculating cumulative supply would result in an illogical offshore wind supply curve where price fluctuates up and down 
# as supply increases.
L120.offshore_wind_potential_EJ %>%
  left_join(L120.offshore_wind_CF, by = c("State", "Wind.Class")) %>%
  left_join(L2231.offshore_wind_capital, by = c("Wind.Class")) %>%
  mutate(fcr = L120.offshore_wind_fcr, OM.fixed = L120.offshore_wind_OMfixed) %>%
  mutate(price = fcr*capital.overnight/CF/8760/conv_kwh_GJ + OM.fixed/CF/8760/conv_kwh_GJ) %>%
  group_by(State) %>%
  mutate(CFmax = max(CF)) %>%
  arrange(State, price) %>%
  mutate(supply = cumsum(resource.potential.EJ)) %>%
  ungroup() %>%
  select(State, price, supply, CFmax) -> L120.offshore_wind_matrix

# Assigning resource to Alaska because the dataset does not include a resource estimate for Alaska.
# Alaska is assigned 5% of each supply point; thus, Alaska's resource is assumed to be a representative 
# sample of the total USA resource.
L120.CFmax.average <- L120.offshore_wind_matrix %>%
  summarise(CFmax = mean(CFmax))
L120.CFmax.average <- as.numeric(L120.CFmax.average) 

L120.offshore_wind_potential_EJ %>%
  left_join(L120.offshore_wind_CF, by = c("State", "Wind.Class")) %>%
  left_join(L2231.offshore_wind_capital, by = c("Wind.Class")) %>%
  mutate(fcr = L120.offshore_wind_fcr, OM.fixed = L120.offshore_wind_OMfixed) %>%
  mutate(price = fcr*capital.overnight/CF/8760/conv_kwh_GJ + OM.fixed/CF/8760/conv_kwh_GJ) %>%
  arrange(price) %>%
  mutate(resource.potential.EJ = resource.potential.EJ * .05) %>%
  mutate(State = "AK") %>%
  mutate(supply = cumsum(resource.potential.EJ)) %>%
  mutate(CFmax = L120.CFmax.average) %>%
  select(State, price, supply, CFmax) -> L120.offshore_wind_matrix.AK

L120.offshore_wind_matrix %>% 
  bind_rows(L120.offshore_wind_matrix.AK) -> L120.offshore_wind_matrix
  

printlog( "L120.offshore_wind_curve: estimating parameters of the smooth curve" )
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

L120.offshore_wind_matrix %>%
  group_by(State) %>%
  arrange(State, price) %>%
  mutate(Pmin = min(price), Pvar = price - Pmin, maxSubResource = round(max(supply),5)) %>%
  ungroup() -> L120.offshore_wind_curve

# Approximate mid-price using first supply points that are less than (p1, Q1) and greater than (p2,Q2) 50% of maxSubResource.
# Using these points, the mid-price can be estimated as:
# mid.price = ((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1))
# This assumes that the curve is largely linear between the two points above.

# Calculating P1 and Q1
L120.offshore_wind_curve %>%
  mutate(percent.supply = supply/maxSubResource) %>%
  group_by(State) %>%
  arrange(State, desc(price)) %>%
  filter(percent.supply <= 0.5) %>%
  filter(row_number() == 1) %>%
  select(State, P1 = Pvar, Q1 = supply, maxSubResource) -> L120.mid.price_1

# Calculating P2 and Q2
L120.offshore_wind_curve %>%
  mutate(percent.supply = supply/maxSubResource) %>%
  group_by(State) %>%
  filter( percent.supply >= 0.5) %>%
  filter(row_number() == 1) %>%
  select(State, P2 = Pvar, Q2 = supply) -> L120.mid.price_2

# Calculating mid.price
L120.mid.price_1 %>%
  left_join(L120.mid.price_2, by = "State") %>%
  mutate(mid.price = round(((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1)),5)) %>%
  select(State, mid.price) -> L120.mid.price

# NOTE:  Five states (AL, DE, IN, MS, NH) are dropped from L120.mid.price_1 because their lowest supply point contains 
# greater than 50% of the state's supply.  (AL, MS, & NH have only one supply point.)
L120.offshore_wind_matrix %>%
  anti_join(L120.mid.price, by = c("State")) -> L120.dropped.states

# To prevent states with offshore wind resource from being dropped:
# 1) add P,Q point of 99% of Pmin, 1% of supply at Pmin and recalculate L120.offshore_wind_curve parameters
L120.dropped.states %>%
  bind_rows(L120.dropped.states %>%
              group_by(State) %>%
              arrange(State, price) %>%
              filter(row_number() == 1) %>%
              ungroup() %>%
              mutate(price = price * .99, supply = supply * .01)) %>%
  group_by(State) %>%
  arrange(State, price) %>%
  mutate(Pmin = min(price), Pvar = price - Pmin, maxSubResource = round(max(supply),5)) %>%
  ungroup() -> L120.offshore_wind_curve_adj

# 2) calculate mid.price as above
L120.offshore_wind_curve_adj %>%
  mutate(percent.supply = supply/maxSubResource) %>%
  group_by(State) %>%
  arrange(State, desc(price)) %>%
  filter(percent.supply <= 0.5) %>%
  filter(row_number() == 1) %>%
  select(State, P1 = Pvar, Q1 = supply, maxSubResource) -> L120.mid.price_1_adj

L120.offshore_wind_curve_adj %>%
  mutate(percent.supply = supply/maxSubResource) %>%
  group_by(State) %>%
  filter( percent.supply >= 0.5) %>%
  filter(row_number() == 1) %>%
  select(State, P2 = Pvar, Q2 = supply) -> L120.mid.price_2_adj

L120.mid.price_1_adj %>%
  left_join(L120.mid.price_2_adj, by = "State") %>%
  mutate(mid.price = round(((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1)),5)) %>%
  select(State, mid.price) -> L120.mid.price_adj

# Remove dropped states from L120.offshore_wind_curve, and bind in revised data points
L120.offshore_wind_curve %>%
  anti_join(L120.offshore_wind_curve_adj, by = c("State")) %>%
  bind_rows(L120.offshore_wind_curve_adj) -> L120.offshore_wind_curve
  
# Bind in mid.price data for dropped states
L120.mid.price %>%
  bind_rows(L120.mid.price_adj) -> L120.mid.price

# Add mid.price to offshore_wind_curve
L120.offshore_wind_curve %>%
  left_join(L120.mid.price, by = c("State")) %>%
  arrange(State) -> L120.offshore_wind_curve

# Dropping regions with maxSubResource < .001 (i.e. NH, 0.00048 EJ), to avoid potential solution errors
L120.offshore_wind_curve %>% 
  filter(maxSubResource > .001) -> L120.offshore_wind_curve

# Defining variables to be used later. Note that the ReEDS data includes information for the 48 contiguous states only.
# For now, since this script creates add-on files, we'll assume that the existing curves in the remaining states are good enough.
states_list <- unique(L120.offshore_wind_curve$State)
L120.curve.exponent <- tibble()

for (L120.state in states_list) {
  
  L120.offshore_wind_curve %>%
    filter(State == L120.state) -> L120.offshore_wind_curve_state 
  
  L120.offshore_wind_curve_state %>%
    select(price, supply) -> L120.supply_points_state
  
  L120.error_min_curve.exp <- optimize(f = smooth_res_curve_approx_error, interval=c(1.0,15.0),
                                        L120.offshore_wind_curve_state$mid.price,
                                        L120.offshore_wind_curve_state$Pmin,
                                        L120.offshore_wind_curve_state$maxSubResource,
                                        L120.offshore_wind_curve_state )
  
  L120.offshore_wind_curve_state$curve.exponent <-  round(L120.error_min_curve.exp$minimum,5)
  L120.offshore_wind_curve_state %>%
    select(State, curve.exponent) %>%
    filter(row_number() == 1) -> L120.curve.exponent_state
  
  L120.curve.exponent %>% 
    bind_rows(L120.curve.exponent_state) -> L120.curve.exponent
  
}

L120.offshore_wind_curve %>%
  left_join(L120.curve.exponent, by = "State") -> L120.offshore_wind_curve

# Prepare cost curve for output
L120.offshore_wind_curve %>%
  mutate (renewresource = "offshore wind resource", smooth.renewable.subresource = "offshore wind resource") %>%
  select(region = State, renewresource, smooth.renewable.subresource, maxSubResource, mid.price, curve.exponent) %>%
  group_by(region) %>%
  filter(row_number() == 1) %>%
  ungroup()-> L120.RsrcCurves_EJ_R_offshore_wind_USA


printlog( "L120.GridCost_offshore_wind_USA: region-specific grid connection cost adders" )
# Grid connection costs are read in as fixed non-energy cost adders (in $/GJ) that vary by state. Our starting data consists 
# of grid connection costs in $/MW by ReEDS region and wind class. This data also categorizes the connection cost into five 
# bins in each region and class. Using this data, we obtain a grid connection cost in $/GJ for each region/ class/ bin data 
# point as FCR * (grid connection cost in $/MW) / (8760*CF*MWh_GJ). Costs are then obtained for a state by averaging. 
# In the future, we might think about a separate state-level curve for grid connection costs. 

reeds_offshore_wind_curve_grid_cost %>%
  select(-Wind.Type) %>%
  gather(bin, cost, -Wind.Resource.Region, -Wind.Class) %>%
  filter(cost != 0) %>%
  left_join(L120.offshore_wind_CF_reeds, by =c("Wind.Resource.Region", "Wind.Class" = "TRG")) %>%
  mutate(fcr = L120.offshore_wind_fcr) %>%
  mutate(grid.cost = fcr*cost/(8760*CF*conv_MWh_GJ)) %>%
  mutate(grid.cost = grid.cost*conv_2013_1975_USD) %>%
  left_join(reedsregions_states %>%
              select(Region, State), 
            by = c("Wind.Resource.Region" = "Region")) %>%
  group_by(State) %>%
  summarise(grid.cost = round(mean(grid.cost),5)) %>%
  ungroup() %>%
  filter(State %in% states_list) -> L120.GridCost_offshore_wind_USA

# Assinging Alaska USA-maximum grid connection cost
L120.GridCost_offshore_wind_USA %>%
  bind_rows(L120.GridCost_offshore_wind_USA %>%
              summarise(grid.cost = max(grid.cost)) %>%
              mutate(State = "AK")) -> L120.GridCost_offshore_wind_USA


printlog( "L120.RegCapFactor_offshore_wind: region-specific offshore wind capacity factors" )

L120.offshore_wind_curve %>%
  distinct(State, CFmax) %>%
  filter(is.na(CFmax) == "FALSE") -> L120.RegCapFactor_offshore_wind_USA

# -----------------------------------------------------------------------------
# 3. Output

# Add comments for each table
comments.L120.RsrcCurves_EJ_R_offshore_wind_USA <- c( "Offshore wind resource curves by state","Unit = EJ" )
# comments.L120.TechChange_offshore_wind_USA <- c( "Technological change for offshore wind","Unit = %" )
comments.L120.GridCost_offshore_wind_USA <- c( "Regional offshore wind grid connection cost adder","Unit = 1975$/GJ" )
comments.L120.RegCapFactor_offshore_wind_USA <- c( "State-specific offshore wind capacity factors","Unit = capacity factor" )

# Write tables as CSV files
writedata( L120.RsrcCurves_EJ_R_offshore_wind_USA, domain="GCAMUSA_LEVEL1_DATA", fn="L120.RsrcCurves_EJ_R_offshore_wind_USA", comments=comments.L120.RsrcCurves_EJ_R_offshore_wind_USA )
# writedata( L120.TechChange_offshore_wind_USA, domain="GCAMUSA_LEVEL1_DATA", fn="L120.TechChange_offshore_wind_USA", comments=comments.L120.TechChange_offshore_wind_USA )
writedata( L120.GridCost_offshore_wind_USA, domain="GCAMUSA_LEVEL1_DATA", fn="L120.GridCost_offshore_wind_USA", comments=comments.L120.GridCost_offshore_wind_USA )
writedata( L120.RegCapFactor_offshore_wind_USA, domain="GCAMUSA_LEVEL1_DATA", fn="L120.RegCapFactor_offshore_wind_USA", comments=comments.L120.RegCapFactor_offshore_wind_USA )

logstop()
