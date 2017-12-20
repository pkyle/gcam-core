
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "LA120.offshore_wind.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Offshore wind resource supply curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")

A23.globaltech_capital <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_capital", skip = 1 )
A23.globaltech_OMfixed <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_OMfixed", skip = 1 )

A20.wind_class_CFs <- readdata( "ENERGY_ASSUMPTIONS", "A20.wind_class_CFs", skip = 1 )
A20.offshore_wind_depth_cap_cost <- readdata( "ENERGY_ASSUMPTIONS", "A20.offshore_wind_depth_cap_cost", skip = 1 )

NREL_offshore_energy <- readdata( "ENERGY_LEVEL0_DATA", "NREL_offshore_energy", skip = 3 )
reeds_offshore_wind_curve_grid_cost <- readdata( "ENERGY_LEVEL0_DATA", "reeds_offshore_wind_curve_grid_cost", skip = 2 )
reeds_offshore_wind_curve_CF_avg <- readdata( "ENERGY_LEVEL0_DATA", "reeds_offshore_wind_curve_CF_avg" )

# -----------------------------------------------------------------------------
# 2. Perform computations

printlog( "L120.offshore_wind_potential_EJ: Resource potential in EJ by GCAM region and class" )
# Map NREL data on resource potential by country to GCAM 32 regions, convert PWh to EJ, 
# aggregate by GCAM region/ wind class/ depth class

NREL_offshore_energy %>%
  select(-total) %>%
  left_join(iso_GCAM_regID %>%
              select(country_name, GCAM_region_ID),
            by = c("IAM_country" = "country_name")) %>%
  gather(wind_class, resource.potential.PWh, -IAM_country, -GCAM_region_ID, -depth_class, -distance_to_shore) %>%
  mutate(resource.potential.EJ = resource.potential.PWh * 1000 * conv_TWh_EJ ) %>%
  group_by(GCAM_region_ID, wind_class, depth_class) %>%
  summarise(resource.potential.EJ = sum(resource.potential.EJ)) %>%
  ungroup() %>%
  filter(resource.potential.EJ != 0) -> L120.offshore_wind_potential_EJ
  

printlog( "L120.offshore_wind_matrix: Creating a matrix of costs (1975$/GJ) and 
          cumulative resource potential (EJ) by region and wind_class" )

L120.offshore_wind_capital <- A20.offshore_wind_depth_cap_cost

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
  left_join(A20.wind_class_CFs, by = c("wind_class")) %>%
  left_join(L120.offshore_wind_capital, by = c("depth_class")) %>%
  mutate(fcr = L120.offshore_wind_fcr, OM.fixed = L120.offshore_wind_OMfixed) %>%
  mutate(price = fcr*capital.overnight/CF/8760/conv_kwh_GJ + OM.fixed/CF/8760/conv_kwh_GJ) %>% 
  group_by(GCAM_region_ID) %>% 
  mutate(CFmax = max(CF)) %>%
  arrange(GCAM_region_ID, price) %>%
  mutate(supply = cumsum(resource.potential.EJ)) %>%
  ungroup() %>%
  select(GCAM_region_ID, price, supply, CFmax) -> L120.offshore_wind_matrix

# Assigning additional resource to the USA because the global dataset does not include any resource for Alaska, 
# even though initial estimates suggest that its resource potential could be very large.
# Each supply point is simply increased by 5%; thus, Alaska's resource is assumed to be a representative 
# sample of the total USA resource.
L120.offshore_wind_matrix %>%
  mutate(supply = if_else(GCAM_region_ID == 1, supply * 1.05, supply)) -> L120.offshore_wind_matrix


printlog( "L120.RsrcCurves_EJ_R_offshore_wind: estimating parameters of the smooth renewable resource curve" )
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
# NOTE:  Dropping states with maxSubResource < .01 EJ.  This removes two states (MS & NH) which have 
# only one wind resource data point.

L120.offshore_wind_matrix %>%
  group_by(GCAM_region_ID) %>%
  arrange(GCAM_region_ID, price) %>%
  mutate(Pmin = min(price), Pvar = price - Pmin, maxSubResource = round(max(supply),5)) %>%
  ungroup() -> L120.offshore_wind_curve

# Approximate mid-price using first supply points that are less than (p1, Q1) and greater than (p2,Q2) 50% of maxSubResource.
# Using these points, the mid-price can be estimated as:
# mid.price = ((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1))
# This assumes that the curve is largely linear between the two points above.

# Calculating P1 and Q1
L120.offshore_wind_curve %>%
  mutate(percent.supply = supply/maxSubResource) %>%
  group_by(GCAM_region_ID) %>%
  arrange(GCAM_region_ID, desc(price)) %>%
  filter( percent.supply <= 0.5) %>%
  filter(row_number() == 1) %>%
  select(GCAM_region_ID, P1 = Pvar, Q1 = supply, maxSubResource) -> L120.mid.price_1

# Calculating P2 and Q2
L120.offshore_wind_curve %>%
  mutate(percent.supply = supply/maxSubResource) %>%
  group_by(GCAM_region_ID) %>%
  filter( percent.supply >= 0.5) %>%
  filter(row_number() == 1) %>%
  select(GCAM_region_ID, P2 = Pvar, Q2 = supply) -> L120.mid.price_2

# Calculating mid.price
L120.mid.price_1 %>%
  left_join(L120.mid.price_2, by = "GCAM_region_ID") %>%
  mutate(mid.price = round(((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1)),5)) %>%
  select(GCAM_region_ID, mid.price) -> L120.mid.price

L120.offshore_wind_curve %>%
  left_join(L120.mid.price, by = c("GCAM_region_ID")) -> L120.offshore_wind_curve

# Defining variables to be used later.
region_list <- unique(L120.offshore_wind_curve$GCAM_region_ID)
L120.curve.exponent <- tibble()

for (L120.region in region_list) {
  
  L120.offshore_wind_curve %>%
    filter(GCAM_region_ID == L120.region) -> L120.offshore_wind_curve_region 
  
  L120.offshore_wind_curve_region %>%
    select(price, supply) -> L120.supply_points_region
  
  L120.error_min_curve.exp <- optimize(f = smooth_res_curve_approx_error, interval=c(1.0,15.0),
                                        L120.offshore_wind_curve_region$mid.price,
                                        L120.offshore_wind_curve_region$Pmin,
                                        L120.offshore_wind_curve_region$maxSubResource,
                                        L120.offshore_wind_curve_region )
  
  L120.offshore_wind_curve_region$curve.exponent <-  round(L120.error_min_curve.exp$minimum,5)
  L120.offshore_wind_curve_region %>%
    select(GCAM_region_ID, curve.exponent) %>%
    filter(row_number() == 1) -> L120.curve.exponent_region
  
  L120.curve.exponent %>% 
    bind_rows(L120.curve.exponent_region) -> L120.curve.exponent
  
}

L120.offshore_wind_curve %>%
  left_join(L120.curve.exponent, by = "GCAM_region_ID") -> L120.offshore_wind_curve

# Prepare supply curve for output
L120.offshore_wind_curve %>%
  mutate (resource = "offshore wind resource", subresource = "offshore wind resource") %>%
  select(GCAM_region_ID, resource, subresource, maxSubResource, mid.price, curve.exponent) %>%
  group_by(GCAM_region_ID) %>%
  filter(row_number() == 1) %>%
  ungroup()-> L120.RsrcCurves_EJ_R_offshore_wind


printlog( "L120.TechChange_offshore_wind: technological change for offshore wind" )
# Technological change in the supply curve is related to assumed improvements in capital cost.
# If capital cost changes from CC to a.CC, then every price point of the curve will scale by a factor a' given as follows:
# a' = (k1.a.CC + k2. OM-fixed) / (k1.CC + k2. OM-fixed) where k1 = FCR/(8760*kWh_GJ) and k2 = 1/(8760*kWh_GJ)
# Thus, we calculate model input parameter techChange (which is the reduction per year) as 1-a'^(1/5)

# First, calculate capital cost over time for "wind_offshore" technology
A23.globaltech_capital %>%
  filter(technology == "wind_offshore") %>%
  interpolate_and_melt(c( model_base_years, model_future_years ), value.name="capital.overnight", 
                       digits = digits_capital, rule=3 ) %>%
  select(sector.name = supplysector, subsector.name = subsector, intermittent.technology = technology, year, 
         input.capital, capital.overnight, fixed.charge.rate, capacity.factor) -> L120.offshore_wind_cap_cost

# Second, calculate technological change
L120.offshore_wind_cap_cost %>%
  filter(intermittent.technology == "wind_offshore") %>%
  select(year, capital.overnight) %>%
  mutate(capital.overnight.lag = lag(capital.overnight, 1)) %>%
  mutate(capital.tech.change.5yr = capital.overnight.lag/capital.overnight) %>%
  mutate(fixed.charge.rate = L120.offshore_wind_fcr, OM.fixed = L120.offshore_wind_OMfixed) %>%
  mutate(k1 = fixed.charge.rate/ (8760*conv_kwh_GJ), k2 = 1/(8760*conv_kwh_GJ)) %>%
  mutate(tech.change.5yr = (k1* capital.tech.change.5yr * capital.overnight + k2*OM.fixed)/(k1*capital.overnight + k2*OM.fixed)) %>%
  mutate(tech.change = round(abs(1-(tech.change.5yr)^(1/5)),5)) %>%
  select(year, tech.change) %>%
  filter(is.na(tech.change) == "FALSE") %>%
  filter(year > set_years("final-calibration-year")) -> L120.TechChange_offshore_wind


printlog( "L120.GridCost_offshore_wind: grid connection cost-adder for offshore wind" )
# Grid connection costs are read in as fixed non-energy cost adders (in $/GJ). Since we do not have grid connection cost
# data available at the country or GCAM region level (we have USA grid connection costs by ReEDS region and wind class), 
# we apply the USA average grid connection cost to all GCAM regions.

# Calculate USA-average grid connection cost
reeds_offshore_wind_curve_grid_cost %>%
  select(-Wind.Type) %>%
  gather(bin, cost, -Wind.Resource.Region, -Wind.Class) %>%
  filter(cost != 0) %>%
  left_join(reeds_offshore_wind_curve_CF_avg, by =c("Wind.Resource.Region", "Wind.Class" = "TRG")) %>%
  mutate(fcr = L120.offshore_wind_fcr) %>%
  mutate(grid.cost = fcr*cost/(8760*CF*conv_MWh_GJ)) %>%
  mutate(grid.cost = grid.cost*conv_2013_1975_USD) %>%
  summarise(grid.cost = round(mean(grid.cost),5)) -> L120.USA.grid.cost.avg
L120.USA.grid.cost.avg <- as.numeric(L120.USA.grid.cost.avg)

# Set grid connection cost for all regions as USA-average
GCAM_region_names %>% 
  select(GCAM_region_ID) %>%
  mutate(grid.cost = L120.USA.grid.cost.avg) -> L120.GridCost_offshore_wind


printlog( "L120.RegCapFactor_offshore_wind: region-specific offshore wind capacity factors" )

L120.offshore_wind_curve %>%
  distinct(GCAM_region_ID, CFmax) %>%
  filter(is.na(CFmax) == "FALSE") -> L120.RegCapFactor_offshore_wind

# -----------------------------------------------------------------------------
# 3. Output
# Add comments for each table
comments.L120.RsrcCurves_EJ_R_offshore_wind <- c( "Offshore wind resource curves by GCAM region","Unit = EJ" )
comments.L120.TechChange_offshore_wind <- c( "Technological change for offshore wind","Unit = %" )
comments.L120.GridCost_offshore_wind <- c( "Regional offshore wind grid connection cost adder","Unit = 1975$/GJ" )
comments.L120.RegCapFactor_offshore_wind <- c( "Region-specific offshore wind capacity factors","Unit = capacity factor" )

# Write tables as CSV files
writedata( L120.RsrcCurves_EJ_R_offshore_wind, domain="ENERGY_LEVEL1_DATA", fn="L120.RsrcCurves_EJ_R_offshore_wind", comments=comments.L120.RsrcCurves_EJ_R_offshore_wind )
writedata( L120.TechChange_offshore_wind, domain="ENERGY_LEVEL1_DATA", fn="L120.TechChange_offshore_wind", comments=comments.L120.TechChange_offshore_wind )
writedata( L120.GridCost_offshore_wind, domain="ENERGY_LEVEL1_DATA", fn="L120.GridCost_offshore_wind", comments=comments.L120.GridCost_offshore_wind )
writedata( L120.RegCapFactor_offshore_wind, domain="ENERGY_LEVEL1_DATA", fn="L120.RegCapFactor_offshore_wind", comments=comments.L120.RegCapFactor_offshore_wind )

logstop()
