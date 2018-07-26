
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
logstart( "L2231.wind_update.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Electricity sector" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ccs_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
fuel_energy_input <- readdata( "ENERGY_MAPPINGS", "fuel_energy_input" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )

A20.wind_class_CFs <- readdata( "ENERGY_ASSUMPTIONS", "A20.wind_class_CFs", skip = 1 )
A23.globaltech_capital <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_capital", skip = 1 )
A23.globaltech_OMfixed <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_OMfixed", skip = 1 )

NREL_onshore_energy <- readdata( "ENERGY_LEVEL0_DATA", "NREL_onshore_energy", skip = 3 )
reeds_wind_curve_CF_avg <- readdata( "ENERGY_LEVEL0_DATA", "reeds_wind_curve_CF_avg" )
reeds_wind_curve_grid_cost <- readdata( "ENERGY_LEVEL0_DATA", "reeds_wind_curve_grid_cost", skip = 2  )

L223.StubTechCapFactor_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.StubTechCapFactor_elec", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations

printlog( "L2231.onshore_wind_potential_EJ: Resource potential in EJ by GCAM region and class" )
# First, map NREL data on resource potential by country to GCAM 32 regions, convert PWh to EJ
# Second, aggregate by GCAM region/ wind class

NREL_onshore_energy %>%
  select(-total) %>%
  left_join(iso_GCAM_regID %>%
              select(country_name, GCAM_region_ID),
            by = c("IAM.Country.Name" = "country_name")) %>%
  left_join(GCAM_region_names, by = c("GCAM_region_ID")) %>%
  select(-IAM.Country.Name, -GCAM_region_ID) %>%
  gather(wind_class, resource.potential.EJ, -region, -distance) %>%
  mutate(resource.potential.EJ = resource.potential.EJ * 1000 * conv_TWh_EJ ) %>%
  group_by(region, wind_class) %>%
  summarise(resource.potential.EJ = sum(resource.potential.EJ)) %>%
  ungroup() %>%
  filter(resource.potential.EJ != 0) -> L2231.onshore_wind_potential_EJ


printlog( "L2231.onshore_wind_matrix: Creating a matrix of costs (1975$/GJ) and cumulative resource potential (EJ) by state and class" )

A23.globaltech_capital %>%
  filter(technology == "wind") %>%
  select(X_final_model_base_year) -> L2231.onshore_wind_capital
L2231.onshore_wind_capital <- as.numeric(L2231.onshore_wind_capital)

A23.globaltech_capital %>%
  filter(technology == "wind") %>%
  select(fixed.charge.rate) -> L2231.onshore_wind_fcr
L2231.onshore_wind_fcr <- as.numeric(L2231.onshore_wind_fcr)

A23.globaltech_OMfixed %>%
  filter(technology == "wind") %>%
  select(X_final_model_base_year) -> L2231.onshore_wind_OMfixed
L2231.onshore_wind_OMfixed <- as.numeric(L2231.onshore_wind_OMfixed)

L2231.onshore_wind_potential_EJ %>%
  left_join(A20.wind_class_CFs, by = c("wind_class")) %>%
  mutate(capital.overnight = L2231.onshore_wind_capital, fcr = L2231.onshore_wind_fcr, OM.fixed = L2231.onshore_wind_OMfixed) %>%
  mutate(price = fcr*capital.overnight/CF/8760/conv_kwh_GJ + OM.fixed/CF/8760/conv_kwh_GJ) %>% 
  group_by(region) %>% 
  mutate(CFmax = max(CF)) %>%
  arrange(region, price) %>%
  mutate(supply = cumsum(resource.potential.EJ)) %>%
  ungroup() %>%
  select(region, price, supply, CFmax) -> L2231.onshore_wind_matrix


printlog( "L2231.onshore_wind_curve: estimating parameters of the smooth curve" )
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

L2231.onshore_wind_matrix %>%
  group_by(region) %>%
  arrange(region, price) %>%
  mutate(Pmin = min(price), Pvar = price - Pmin, maxSubResource = round(max(supply),5)) %>%
  ungroup() -> L2231.onshore_wind_curve

# Approximate mid-price using first supply points that are less than (p1, Q1) and greater than (p2,Q2) 50% of maxSubResource.
# Using these points, the mid-price can be estimated as:
# mid.price = ((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1))
# This assumes that the curve is largely linear between the two points above.

# Calculating P1 and Q1
L2231.onshore_wind_curve %>%
  mutate(percent.supply = supply / maxSubResource) %>%
  group_by(region) %>%
  arrange(region, desc(price)) %>%
  filter(percent.supply <= 0.5) %>%
  filter(row_number() == 1) %>%
  select(region, P1 = Pvar, Q1 = supply, maxSubResource) -> L2231.mid.price_1

# Calculating P2 and Q2
L2231.onshore_wind_curve %>%
  mutate(percent.supply = supply / maxSubResource) %>%
  group_by(region) %>%
  filter(percent.supply >= 0.5) %>%
  filter(row_number() == 1) %>%
  select(region, P2 = Pvar, Q2 = supply) -> L2231.mid.price_2

# Calculating mid.price
L2231.mid.price_1 %>%
  left_join(L2231.mid.price_2, by = "region") %>%
  mutate(mid.price = round(((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1)),5)) %>%
  select(region, mid.price) -> L2231.mid.price

L2231.onshore_wind_curve %>%
  left_join(L2231.mid.price, by = c("region")) -> L2231.onshore_wind_curve

# Defining variables to be used later.
region_list <- unique(L2231.onshore_wind_curve$region)
L2231.curve.exponent <- tibble()

for (L2231.region in region_list) {
  
  L2231.onshore_wind_curve %>%
    filter(region == L2231.region) -> L2231.onshore_wind_curve_region 
  
  L2231.onshore_wind_curve_region %>%
    select(price, supply) -> L2231.supply_points_region
  
  L2231.error_min_curve.exp <- optimize(f = smooth_res_curve_approx_error, interval=c(1.0,15.0),
                                        L2231.onshore_wind_curve_region$mid.price,
                                        L2231.onshore_wind_curve_region$Pmin,
                                        L2231.onshore_wind_curve_region$maxSubResource,
                                        L2231.onshore_wind_curve_region )
  
  L2231.onshore_wind_curve_region$curve.exponent <-  round(L2231.error_min_curve.exp$minimum,5)
  L2231.onshore_wind_curve_region %>%
    select(region, curve.exponent) %>%
    filter(row_number() == 1) -> L2231.curve.exponent_region
  
  L2231.curve.exponent %>% 
    bind_rows(L2231.curve.exponent_region) -> L2231.curve.exponent
  
}

L2231.onshore_wind_curve %>%
  left_join(L2231.curve.exponent, by = "region") -> L2231.onshore_wind_curve


printlog( "L2231.TechChange_onshore_wind: technological change for onshore wind" )
# Technological change in the supply curve is related to assumed improvements in capital cost.
# If capital cost changes from CC to a.CC, then every price point of the curve will scale by a factor a' given as follows:
# a' = (k1.a.CC + k2. OM-fixed) / (k1.CC + k2. OM-fixed) where k1 = FCR/(8760*kWh_GJ) and k2 = 1/(8760*kWh_GJ)
# Thus, we calculate model input parameter techChange (which is the reduction per year) as 1-a'^(1/5)

# First, calculate capital cost over time for "wind_offshore" technology
A23.globaltech_capital %>%
  filter(technology == "wind") %>%
  interpolate_and_melt(c( model_base_years, model_future_years ), value.name="capital.overnight", 
                       digits = digits_capital, rule=3 ) %>%
  select(sector.name = supplysector, subsector.name = subsector, intermittent.technology = technology, year, 
         input.capital, capital.overnight, fixed.charge.rate) -> L2231.onshore_wind_cap_cost

# Second, calculate technological change
L2231.onshore_wind_cap_cost %>%
  filter(intermittent.technology == "wind") %>%
  select(year, capital.overnight) %>%
  mutate(capital.overnight.lag = lag(capital.overnight, 1)) %>%
  mutate(capital.tech.change.5yr = capital.overnight.lag/capital.overnight) %>%
  mutate(fixed.charge.rate = L2231.onshore_wind_fcr, OM.fixed = L2231.onshore_wind_OMfixed) %>%
  mutate(k1 = fixed.charge.rate/ (8760*conv_kwh_GJ), k2 = 1/(8760*conv_kwh_GJ)) %>%
  mutate(tech.change.5yr = (k1* capital.tech.change.5yr * capital.overnight + k2*OM.fixed)/(k1*capital.overnight + k2*OM.fixed)) %>%
  mutate(tech.change = round(abs(1-(tech.change.5yr)^(1/5)),5)) %>%
  select(year, tech.change) %>%
  filter(is.na(tech.change) == "FALSE") %>%
  filter(year > set_years("final-calibration-year")) -> L2231.TechChange_onshore_wind


printlog( "L2231.GridCost_onshore_wind: grid connection cost-adder for onshore wind" )
# Grid connection costs are read in as fixed non-energy cost adders (in $/GJ). Since we do not have grid connection cost
# data available at the country or GCAM region level (we have USA grid connection costs by ReEDS region and wind class), 
# we apply the USA average grid connection cost to all GCAM regions.

# Calculate USA-average grid connection cost
reeds_wind_curve_grid_cost %>%
  select(-Wind.Type) %>%
  gather(bin, cost, -Wind.Resource.Region, -Wind.Class) %>%
  filter(cost != 0) %>%
  left_join(reeds_wind_curve_CF_avg, by =c("Wind.Resource.Region", "Wind.Class" = "TRG")) %>%
  mutate(fcr = L2231.onshore_wind_fcr) %>%
  mutate(grid.cost = fcr*cost/(8760*CF*conv_MWh_GJ)) %>%
  mutate(grid.cost = grid.cost*conv_2013_1975_USD) %>%
  summarise(grid.cost = round(mean(grid.cost),5)) -> L2231.grid.cost
L2231.grid.cost <- as.numeric(L2231.grid.cost)

# Set grid connection cost for all regions as USA-average
GCAM_region_names %>% 
  select(region) %>%
  mutate(grid.cost = L2231.grid.cost) -> L2231.GridCost_onshore_wind


# Preparing final tables to convert to level-2 csvs
printlog( "L2231.SmthRenewRsrcCurves_onshore_wind: smooth renewable resource curve for onshore wind" )

region_order <- unique(GCAM_region_names$region)

L2231.onshore_wind_curve %>%
  mutate(renewresource = "onshore wind resource", smooth.renewable.subresource = "onshore wind resource") %>%
  mutate(year.fillout = min( model_base_years )) %>%
  select(region, renewresource, smooth.renewable.subresource, year.fillout, maxSubResource, mid.price, curve.exponent) %>%
  group_by(region) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(region =  factor(region, levels  = region_order)) %>%
  arrange(region) -> L2231.SmthRenewRsrcCurves_onshore_wind


printlog( "L2231.StubTechCapFactor_onshore_wind: region-specific capacity factors for onshore wind" )

L223.StubTechCapFactor_elec %>%
  filter(subsector == "wind", !grepl("_offshore", stub.technology)) %>%
  left_join(L2231.onshore_wind_curve %>%
              distinct(region, CFmax), 
            by= c("region")) %>%
  mutate(capacity.factor = round(CFmax,5)) %>%
  select(region, supplysector, subsector, stub.technology, year, capacity.factor) -> L2231.StubTechCapFactor_onshore_wind


printlog( "L2231.SmthRenewRsrcTechChange_onshore_wind: technological change for onshore wind" )

# Copying tech change to all regions
L2231.SmthRenewRsrcTechChange_onshore_wind <- write_to_all_regions(L2231.TechChange_onshore_wind, c("region", "year", "tech.change"))
L2231.SmthRenewRsrcTechChange_onshore_wind %>%
  mutate(renewresource = "onshore wind resource", smooth.renewable.subresource = "onshore wind resource") %>%
  select(region,renewresource, smooth.renewable.subresource, 
         year.fillout = year, techChange = tech.change) -> L2231.SmthRenewRsrcTechChange_onshore_wind


printlog( "L2231.StubTechCost_onshore_wind: onshore wind grid connection non-energy cost adder" )

# Reading the grid connection cost as a state-level non-energy cost adder
L223.StubTechCapFactor_elec %>%
  filter(subsector == "wind", !grepl("_offshore", stub.technology)) %>%
  select(region, supplysector, subsector, stub.technology, year) %>%
  mutate(minicam.energy.input = "regional price adjustment") %>%
  left_join(L2231.GridCost_onshore_wind, by = c("region")) %>%
  rename(input.cost = grid.cost) -> L2231.StubTechCost_onshore_wind

# -----------------------------------------------------------------------------
# 3. OPTION 1:  Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2231.SmthRenewRsrcCurves_onshore_wind, "SmthRenewRsrcCurves", "ENERGY_LEVEL2_DATA", "L2231.SmthRenewRsrcCurves_onshore_wind", "ENERGY_XML_BATCH", "batch_onshore_wind.xml" )
write_mi_data( L2231.StubTechCapFactor_onshore_wind, "StubTechCapFactor", "ENERGY_LEVEL2_DATA", "L2231.StubTechCapFactor_onshore_wind", "ENERGY_XML_BATCH", "batch_onshore_wind.xml" )
write_mi_data( L2231.SmthRenewRsrcTechChange_onshore_wind, "SmthRenewRsrcTechChange", "ENERGY_LEVEL2_DATA", "L2231.SmthRenewRsrcTechChange_onshore_wind", "ENERGY_XML_BATCH", "batch_onshore_wind.xml" )
write_mi_data( L2231.StubTechCost_onshore_wind, "StubTechCost", "ENERGY_LEVEL2_DATA", "L2231.StubTechCost_onshore_wind", "ENERGY_XML_BATCH", "batch_onshore_wind.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_onshore_wind.xml", "ENERGY_XML_FINAL", "onshore_wind.xml", "", xml_tag="outFile" )

logstop()
