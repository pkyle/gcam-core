# This script creates add-ons to harmonize reference case generation technology costs for multiple load segments with AEO-2016. 
# These assumptions are also consistent with the Base case scenario in the DOE Report: "Energy CO2 Emissions Impacts of Clean Energy Technology Innovation and Policy": 
# For technologies in GCAM that are not listed in the AEO, we simply use the cost assumptions from the global version of
# GCAM. 
# The script also creates add-ons for Advanced Tech scenario for the multiple load segments 
# consistent with the Advanced technology scenario in the DOE report.Link to the DOE report:
# https://energy.gov/sites/prod/files/2017/01/f34/Energy%20CO2%20Emissions%20Impacts%20of%20Clean%20Energy%20Technology%20Innovation%20and%20Policy.pdf
# The script also creates add-ons for ITC and PTC policies for multiple load segments technologies


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
 logstart( "L2247.elecS_tech_costs.R" )
 adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
 adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
 printlog( "L2247.elecS_tech_costs.R: MLS add-ons to harmonize generation tech costs with AEO-2016. Also create Adv Tech add-ons" )

# -----------------------------------------------------------------------------
# 1. Read files
 sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
 sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
 sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
 sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
 sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
 sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
 sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
 

#NOTE: this code file only builds the electric sector model input if use_mult_load_segments <- TRUE
if( use_mult_load_segments ){	
  # L223.GlobalIntTechCapital_elec <- readdata("ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_elec", skip=4)
  # L223.GlobalTechCapital_elec <- readdata("ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_elec", skip=4)
  # L223.GlobalTechOMfixed_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMfixed_elec", skip = 4 )
  # L223.GlobalIntTechOMfixed_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMfixed_elec", skip = 4 )
  # L223.GlobalTechOMvar_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMvar_elec", skip = 4 )
  # L223.GlobalIntTechOMvar_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMvar_elec", skip = 4 )

A23.elec_overnight_costs_aeo <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elec_overnight_costs_QER", skip=4)
# Note that we named files with a "QER" suffix originally that needs to either be removed or changed to "aeo". We'll do this at a later stage.
A23.itc <- readdata("GCAMUSA_ASSUMPTIONS", "A23.itc_QER", skip=4)
A23.ptc <- readdata("GCAMUSA_ASSUMPTIONS", "A23.ptc_QER",skip=4)
A23.ptc_inttech <- readdata("GCAMUSA_ASSUMPTIONS", "A23.ptc_inttech_QER", skip=4)

A23.elec_overnight_costs_aeo_adv <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elec_overnight_costs_QER_adv", skip=4)
A23.elec_OM_aeo_adv <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elec_OM_QER_adv", skip=4)

A23.elecS_tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_associations" )
A23.elecS_inttech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_inttech_associations" )
# A23.elecS_tech_availability <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_availability" )
A23.elec_tech_associations_coal_retire <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elec_tech_associations_coal_retire" ) %>%
  filter(grepl("generation", Electric.sector))

L2234.GlobalTechCapital_elecS <- readdata( "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechCapital_elecS", skip =4)
L2234.GlobalIntTechCapital_elecS <- readdata( "GCAMUSA_LEVEL2_DATA", "L2234.GlobalIntTechCapital_elecS", skip =4)
L2234.GlobalTechOMfixed_elecS <- readdata( "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechOMfixed_elecS", skip =4)
L2234.GlobalIntTechOMfixed_elecS <- readdata( "GCAMUSA_LEVEL2_DATA", "L2234.GlobalIntTechOMfixed_elecS", skip =4)
L2234.GlobalTechOMvar_elecS <- readdata( "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechOMvar_elecS", skip =4)
L2234.GlobalIntTechOMvar_elecS <- readdata( "GCAMUSA_LEVEL2_DATA", "L2234.GlobalIntTechOMvar_elecS", skip =4)

L2240.GlobalTechCapital_elec_coalret <- readdata( "GCAMUSA_LEVEL2_DATA", "L2240.GlobalTechCapital_elec_coalret", skip =4)
L2240.GlobalTechOMfixed_elec_coalret <- readdata( "GCAMUSA_LEVEL2_DATA", "L2240.GlobalTechOMfixed_elec_coalret", skip =4)
L2240.GlobalTechOMvar_elec_coalret <- readdata( "GCAMUSA_LEVEL2_DATA", "L2240.GlobalTechOMvar_elec_coalret", skip =4)

L2246.GlobalTechCapital_coal_vintage_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L2246.GlobalTechCapital_coal_vintage_USA", skip =4 )
L2246.GlobalTechOMfixed_coal_vintage_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L2246.GlobalTechOMfixed_coal_vintage_USA", skip =4 )
L2246.GlobalTechOMvar_coal_vintage_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L2246.GlobalTechOMvar_coal_vintage_USA", skip =4 )

conv_2013_1975 <- 0.293288
conv_2014_2013 <- 0.983834

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Build GCAM capital cost table for all model years from AEO/DOE data

# First, initialize variables
max_year_aeo <- max(A23.elec_overnight_costs_aeo$year)
L2234.load_segments <- unique(A23.elecS_inttech_associations$Electric.sector)

# Next, create a full list of technologies and costs, and association files. 
# Tech associations must have MLS technologies and corresponding core technologies

L2234.GlobalTechCapital_elecS <-
  L2234.GlobalTechCapital_elecS %>%
  bind_rows(L2240.GlobalTechCapital_elec_coalret) %>%
  bind_rows(L2246.GlobalTechCapital_coal_vintage_USA)

L2234.GlobalTechOMfixed_elecS <-
  L2234.GlobalTechOMfixed_elecS %>%
  bind_rows(L2240.GlobalTechOMfixed_elec_coalret) %>%
  bind_rows(L2246.GlobalTechOMfixed_coal_vintage_USA)

L2234.GlobalTechOMvar_elecS <-
  L2234.GlobalTechOMvar_elecS %>%
  bind_rows(L2240.GlobalTechOMvar_elec_coalret) %>%
  bind_rows(L2246.GlobalTechOMvar_coal_vintage_USA) 

elec_tech_associations_coal_retire <-
  A23.elec_tech_associations_coal_retire %>%
  mutate(subsector.1 = "coal", supplysector = "electricity", technology = "coal (conv pul") %>%
  select(Electric.sector, subsector, Electric.sector.technology, supplysector, subsector.1, technology)

elec_tech_associations_coal_vintage <-
  L2246.GlobalTechCapital_coal_vintage_USA %>%
  distinct(supplysector, subsector, technology) %>%
  rename(Electric.sector = supplysector, Electric.sector.technology = technology) %>%
  mutate(supplysector = "electricity", subsector.1 = "coal", technology = "coal (conv pul") %>%
  select(Electric.sector, subsector, Electric.sector.technology, supplysector, subsector.1, technology)

elecS_tech_associations <-
  A23.elecS_tech_associations %>%
  bind_rows(elec_tech_associations_coal_retire) %>%
  bind_rows(elec_tech_associations_coal_vintage)

elecS_inttech_associations <-
  A23.elecS_inttech_associations 

# First, copy the 2015 data to all base years and convert to right units

A23.elec_overnight_costs_aeo_base_years <-
  A23.elec_overnight_costs_aeo %>%
  filter(year == 2015) %>%
  repeat_and_add_vector('year', model_base_years) 

A23.elec_overnight_costs_aeo <- 
  A23.elec_overnight_costs_aeo %>%
  bind_rows(A23.elec_overnight_costs_aeo_base_years) %>%
  arrange(GCAM.technology, year) %>% 
  mutate(overnight.cost = overnight.cost * conv_2013_1975) 

# Next, copy the aeo costs over to the global technology database. For years beyond max_aeo_year (typically 2040), we assume that the 
# rate of technological change is the same as what is assumed in the core.

L2247.GlobalTechCapital_elecS <-
  L2234.GlobalTechCapital_elecS %>%
  left_join(L2234.GlobalTechCapital_elecS %>% 
              filter(year == max_year_aeo) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.aeo = capital.overnight.y) %>%
  mutate(capital.overnight.ratio = capital.overnight/capital.overnight.max.year.aeo) %>%
  left_join(elecS_tech_associations %>%
              select(-supplysector, -subsector.1), by = c("technology" = "Electric.sector.technology", "supplysector" = "Electric.sector", "subsector")) %>%
  left_join(A23.elec_overnight_costs_aeo, by = c("technology.y" = "GCAM.technology", "year")) %>%
  rename(capital.overnight.aeo = overnight.cost) %>%
  mutate(capital.overnight = if_else(year<= max_year_aeo & !is.na(capital.overnight.aeo), capital.overnight.aeo, capital.overnight)) %>%
  select(-capital.overnight.max.year.aeo,  -capital.overnight.aeo)
# Not removing technology.y variable to be used for ITC calculations

L2247.GlobalTechCapital_elecS <-
  L2247.GlobalTechCapital_elecS %>%
  left_join(L2247.GlobalTechCapital_elecS %>% 
              filter(year == max_year_aeo) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.aeo = capital.overnight.y) %>%
  mutate(capital.overnight = as.double(capital.overnight)) %>%
  mutate(capital.overnight = if_else(year > max_year_aeo, capital.overnight.max.year.aeo * capital.overnight.ratio, capital.overnight)) %>%
  select(-capital.overnight.ratio,  -capital.overnight.max.year.aeo) 
  
L2247.GlobalIntTechCapital_elecS <-
  L2234.GlobalIntTechCapital_elecS %>%
  left_join(L2234.GlobalIntTechCapital_elecS %>% 
              filter(year == max_year_aeo) %>% 
              select(intermittent.technology, capital.overnight), by = "intermittent.technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.aeo = capital.overnight.y) %>%
  mutate(capital.overnight.ratio = capital.overnight/capital.overnight.max.year.aeo) %>%
  left_join(elecS_inttech_associations %>%
              select(-supplysector, -subsector.1), by = c("intermittent.technology" = "Electric.sector.intermittent.technology", "supplysector" = "Electric.sector", "subsector")) %>%
  left_join(A23.elec_overnight_costs_aeo, by = c("intermittent.technology.y" = "GCAM.technology", "year")) %>%
  rename(capital.overnight.aeo = overnight.cost) %>%
  mutate(capital.overnight = as.double(capital.overnight)) %>%
  mutate(capital.overnight = if_else(year<= max_year_aeo & !is.na(capital.overnight.aeo), capital.overnight.aeo, capital.overnight)) %>%
  select(-capital.overnight.max.year.aeo, -capital.overnight.aeo)
# Not removing technology.y variable to be used for ITC calculations

L2247.GlobalIntTechCapital_elecS <-
  L2247.GlobalIntTechCapital_elecS %>%
  left_join(L2247.GlobalIntTechCapital_elecS %>% 
              filter(year == max_year_aeo) %>% 
              select(intermittent.technology, capital.overnight), by = "intermittent.technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.aeo = capital.overnight.y) %>%
  mutate(capital.overnight = as.double(capital.overnight)) %>%
  mutate(capital.overnight = if_else(year > max_year_aeo, capital.overnight.max.year.aeo * capital.overnight.ratio, capital.overnight)) %>%
  select(-capital.overnight.ratio,  -capital.overnight.max.year.aeo) 

# 2b. ITC and PTC

# Adjust FCRs by 1-ITC for technologies that have ITC

L2247.GlobalTechFCROnly_elecS_itc <-
  L2247.GlobalTechCapital_elecS %>%
  left_join(A23.itc, by = c("year", "technology.y" = "GCAM.technology")) %>%
  mutate(fixed.charge.rate = fixed.charge.rate * (1-itc)) %>%
  filter(!is.na(fixed.charge.rate)) %>%
  select(-itc, -technology.y, -capital.overnight) 

L2247.GlobalIntTechFCROnly_elecS_itc <-
  L2247.GlobalIntTechCapital_elecS %>%  
  left_join(A23.itc, by = c("year", "intermittent.technology.y" = "GCAM.technology")) %>%
  mutate(fixed.charge.rate = fixed.charge.rate * (1-itc)) %>%
  filter(!is.na(fixed.charge.rate)) %>%
  select(-itc, -intermittent.technology.y, -capital.overnight)  

# Clean up tables to be printed. We don't need FCR and capacity factor assumptions in the tables to be printed since we did not update
# those assumptions
L2247.GlobalTechCapitalOnly_elecS <-
  L2247.GlobalTechCapital_elecS %>%
  select(-technology.y, -fixed.charge.rate)

L2247.GlobalIntTechCapitalOnly_elecS <-
  L2247.GlobalIntTechCapital_elecS %>%
  select(-intermittent.technology.y, -fixed.charge.rate)

# Build table to read in PTC as cost adder (subtracter)

L2247.GlobalTechRegPriceAdj_ptc <- 
  A23.ptc %>%
  left_join(elecS_tech_associations, by = c("supplysector", "subsector" = "subsector.1", "technology")) %>%
  select(supplysector = Electric.sector, subsector, technology = Electric.sector.technology, year, minicam.non.energy.input, input.cost)

L2247.GlobalIntTechRegPriceAdj_ptc <- 
  A23.ptc_inttech %>%
  left_join(elecS_inttech_associations, by = c("supplysector", "subsector" = "subsector.1", "intermittent.technology")) %>%
  select(supplysector = Electric.sector, subsector, intermittent.technology = Electric.sector.intermittent.technology, year, minicam.non.energy.input, input.cost)

# 2c. Generate csvs for advanced costs. These tables contain advanced tech assumptions for technologies that are provided by DOE.For technologies
# without advanced assumptions, we assume reference assumptions

# First, convert costs to 1975 units
A23.elec_overnight_costs_aeo_adv <-
  A23.elec_overnight_costs_aeo_adv %>%
  mutate(overnight.cost = overnight.cost * conv_2013_1975)

# Keep list of technologies whose costs need to be updated

adv_tech_list <- unique(A23.elec_overnight_costs_aeo_adv$GCAM.technology)

# Next, create advanced tech cost tables for technologies and intermittent technologies
L2247.GlobalTechCapital_elecS_adv <-
  L2247.GlobalTechCapital_elecS %>%
  left_join(L2247.GlobalTechCapital_elecS %>% 
              filter(year == max_year_aeo) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.aeo = capital.overnight.y) %>%
  mutate(capital.overnight.ratio = capital.overnight/capital.overnight.max.year.aeo) %>%
  left_join(elecS_tech_associations %>%
              select(-supplysector, -subsector.1), by = c("technology" = "Electric.sector.technology", "supplysector" = "Electric.sector", "subsector")) %>%
  left_join(A23.elec_overnight_costs_aeo_adv, by = c("technology.y" = "GCAM.technology", "year")) %>%
  rename(capital.overnight.aeo = overnight.cost) %>%
  mutate(capital.overnight = if_else(year<= max_year_aeo & !is.na(capital.overnight.aeo), capital.overnight.aeo, capital.overnight)) %>%
  select(-capital.overnight.max.year.aeo,  -capital.overnight.aeo)

L2247.GlobalTechCapital_elecS_adv <-
  L2247.GlobalTechCapital_elecS_adv %>%
  left_join(L2247.GlobalTechCapital_elecS_adv %>% 
              filter(year == max_year_aeo) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.aeo = capital.overnight.y) %>%
  mutate(capital.overnight = as.double(capital.overnight)) %>%
  mutate(capital.overnight = if_else(year > max_year_aeo, capital.overnight.max.year.aeo * capital.overnight.ratio, capital.overnight)) %>%
  select(-capital.overnight.ratio,  -capital.overnight.max.year.aeo) %>%
  filter(technology.y %in% adv_tech_list)
  

L2247.GlobalIntTechCapital_elecS_adv <-
  L2247.GlobalIntTechCapital_elecS %>%
  left_join(L2247.GlobalIntTechCapital_elecS %>% 
              filter(year == max_year_aeo) %>% 
              select(intermittent.technology, capital.overnight), by = "intermittent.technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.aeo = capital.overnight.y) %>%
  mutate(capital.overnight.ratio = capital.overnight/capital.overnight.max.year.aeo) %>%
  left_join(elecS_inttech_associations %>%
              select(-supplysector, -subsector.1), by = c("intermittent.technology" = "Electric.sector.intermittent.technology", "supplysector" = "Electric.sector", "subsector")) %>%
  left_join(A23.elec_overnight_costs_aeo_adv, by = c("intermittent.technology.y" = "GCAM.technology", "year")) %>%
  rename(capital.overnight.aeo = overnight.cost) %>%
  mutate(capital.overnight = if_else(year<= max_year_aeo & !is.na(capital.overnight.aeo), capital.overnight.aeo, capital.overnight)) %>%
  select(-capital.overnight.max.year.aeo, -capital.overnight.aeo)


L2247.GlobalIntTechCapital_elecS_adv<-
  L2247.GlobalIntTechCapital_elecS_adv %>%
  left_join(L2247.GlobalIntTechCapital_elecS_adv %>% 
              filter(year == max_year_aeo) %>% 
              select(intermittent.technology, capital.overnight), by = "intermittent.technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.aeo = capital.overnight.y) %>%
  mutate(capital.overnight = as.double(capital.overnight)) %>%
  mutate(capital.overnight = if_else(year > max_year_aeo, capital.overnight.max.year.aeo * capital.overnight.ratio, capital.overnight)) %>%
  select(-capital.overnight.ratio,  -capital.overnight.max.year.aeo) %>%
  filter(intermittent.technology.y %in% adv_tech_list)
  
# Then, create separate advanced tech cost trajectories for PV_storage, CSP_storage and wind_storage. 
# We assume that the the costs of these technologies reduce at the same rate as the corresponding intermittent variant until the 
# maximum year of aeo data and then at the same rate as reference asumptions

int_tech_list <- c("CSP", "PV", "wind")
storage_tech_list <- c("CSP_storage", "PV_storage", "wind_storage")

A23.elec_overnight_costs_aeo_adv_int <-
  A23.elec_overnight_costs_aeo_adv %>%
  filter(GCAM.technology %in% int_tech_list) 

A23.elec_overnight_costs_aeo_adv_storage <-
  A23.elec_overnight_costs_aeo_adv_int %>%  
  left_join(A23.elec_overnight_costs_aeo_adv_int %>% 
              group_by(GCAM.technology) %>% 
              filter(year == model_future_years[1]) %>%
              ungroup() %>% 
              select(-year), by = "GCAM.technology") %>%
  mutate(overnight.cost.ratio.aeo.adv = overnight.cost.x/ overnight.cost.y ) %>%
  select(-overnight.cost.x, -overnight.cost.y) %>%
  mutate(GCAM.technology = paste(GCAM.technology, "_storage", sep =""))

L2247.GlobalTechCapital_elecS_storage <- 
  L2247.GlobalTechCapital_elecS %>%
  left_join(elecS_tech_associations %>%
              select(-supplysector, -subsector.1), by = c("technology" = "Electric.sector.technology", "supplysector" = "Electric.sector", "subsector")) %>%
  filter(technology.y %in% storage_tech_list) 

L2247.GlobalTechCapital_elecS_storage <-
  L2247.GlobalTechCapital_elecS_storage %>%
  left_join(L2247.GlobalTechCapital_elecS_storage %>% 
              filter(year == max_year_aeo) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  mutate(capital.overnight.ratio = capital.overnight.x / capital.overnight.y) %>%
  rename (capital.overnight = capital.overnight.x) %>%
  select(-capital.overnight.y) 

L2247.GlobalTechCapital_elecS_adv_storage <-
  L2247.GlobalTechCapital_elecS_storage %>%
  left_join(L2247.GlobalTechCapital_elecS_storage %>% 
              filter(year == 2015) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  rename(capital.overnight.2015 = capital.overnight.y) %>%
  left_join(A23.elec_overnight_costs_aeo_adv_storage, by = c("technology.y" = "GCAM.technology", "year")) %>%
  rename(capital.overnight = capital.overnight.x) %>%
  mutate(capital.overnight = if_else(year <= 2015, capital.overnight, 
                                     if_else(year >2015 & year <= max_year_aeo, capital.overnight.2015 * overnight.cost.ratio.aeo.adv, capital.overnight))) %>%
  arrange(technology, year) 

L2247.GlobalTechCapital_elecS_adv_storage <-
  L2247.GlobalTechCapital_elecS_adv_storage %>%
  left_join(L2247.GlobalTechCapital_elecS_adv_storage %>%
              filter(year == max_year_aeo) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.aeo = capital.overnight.y) %>%
  mutate(capital.overnight = if_else(year > max_year_aeo, capital.overnight.max.year.aeo * capital.overnight.ratio, capital.overnight)) %>%
  select(-capital.overnight.ratio, -capital.overnight.2015, -capital.overnight.max.year.aeo, -overnight.cost.ratio.aeo.adv)

# Now, update the advanced tech cost table with above assumptions
L2247.GlobalTechCapital_elecS_adv <- 
  L2247.GlobalTechCapital_elecS_adv %>%
  filter(!grepl("storage", technology.y)) %>%
  bind_rows(L2247.GlobalTechCapital_elecS_adv_storage) %>%
  arrange(technology, year)
  
# Clean up the advanced tech capital cost tables to get ready for printing. We don't need FCR and capacity factor assumptions in the tables to be printed since we did not update
# those assumptions

L2247.GlobalTechCapitalOnly_elecS_adv <- 
  L2247.GlobalTechCapital_elecS_adv %>%
  # Filtering out years through 2015 to make sure that the model outputs are not different in 2015 between a reference and advanced case. 
  filter(year > 2015) %>%
  select(-technology.y, -fixed.charge.rate, -technology.y.y)

L2247.GlobalIntTechCapitalOnly_elecS_adv <- 
  L2247.GlobalIntTechCapital_elecS_adv %>%
  # Filtering out years through 2015 to make sure that the model outputs are not different in 2015 between a reference and advanced case. 
  filter(year > 2015) %>%
  select(-intermittent.technology.y, -fixed.charge.rate, -intermittent.technology.y.y)

# Advanced Fixed OM cost assumptions

L2247.GlobalTechOMfixed_elecS_adv <-
  L2234.GlobalTechOMfixed_elecS %>%
  left_join(L2234.GlobalTechOMfixed_elecS %>% 
              filter(year == max_year_aeo) %>% 
              select(technology, OM.fixed), by = "technology") %>%
  rename(OM.fixed = OM.fixed.x, OM.fixed.max.year.aeo = OM.fixed.y) %>%
  mutate(OM.fixed.ratio = OM.fixed/OM.fixed.max.year.aeo) %>%
  select(-OM.fixed.max.year.aeo) %>%
  left_join(elecS_tech_associations %>%
              select(-supplysector, -subsector.1), by = c("technology" = "Electric.sector.technology", "supplysector" = "Electric.sector", "subsector")) %>%
  left_join(A23.elec_OM_aeo_adv %>% select(-OM.var), by = c("year", "technology.y" = "GCAM.technology")) %>%
  mutate(OM.fixed.x = if_else(is.na(OM.fixed.y) == TRUE, OM.fixed.x,OM.fixed.y)) %>%
  rename(OM.fixed = OM.fixed.x) %>%
  select(-OM.fixed.y) 
  
L2247.GlobalTechOMfixed_elecS_adv <- 
  L2247.GlobalTechOMfixed_elecS_adv %>%
  left_join(L2247.GlobalTechOMfixed_elecS_adv %>% 
              filter(year == max_year_aeo) %>% 
              select(technology, OM.fixed), by = "technology") %>%
  rename(OM.fixed = OM.fixed.x, OM.fixed.max.year.aeo = OM.fixed.y) %>%
  mutate(OM.fixed = if_else(year > max_year_aeo, OM.fixed.max.year.aeo * OM.fixed.ratio, OM.fixed)) %>%
  select(-OM.fixed.ratio, -OM.fixed.max.year.aeo) %>%
  filter(technology.y %in% adv_tech_list) %>%
  select(-technology.y) %>%
  filter(year > 2015) %>%
  mutate(OM.fixed = if_else(is.nan(OM.fixed) == TRUE,0, OM.fixed))

# Advanced Intermittent Technology fixed OM-costs

L2247.GlobalIntTechOMfixed_elecS_adv <-
  L2234.GlobalIntTechOMfixed_elecS %>%
  left_join(L2234.GlobalIntTechOMfixed_elecS %>% 
              filter(year == max_year_aeo) %>% 
              select(intermittent.technology, OM.fixed), by = "intermittent.technology") %>%
  rename(OM.fixed = OM.fixed.x, OM.fixed.max.year.aeo = OM.fixed.y) %>%
  mutate(OM.fixed.ratio = OM.fixed/OM.fixed.max.year.aeo) %>%
  select(-OM.fixed.max.year.aeo) %>%
  left_join(elecS_inttech_associations %>%
              select(-supplysector, -subsector.1), by = c("intermittent.technology" = "Electric.sector.intermittent.technology", "supplysector" = "Electric.sector", "subsector")) %>%
  left_join(A23.elec_OM_aeo_adv %>% select(-OM.var), by = c("year", "intermittent.technology.y" = "GCAM.technology")) %>%
  mutate(OM.fixed.x = if_else(is.na(OM.fixed.y) == TRUE, OM.fixed.x,OM.fixed.y)) %>%
  rename(OM.fixed = OM.fixed.x) %>%
  select(-OM.fixed.y) 

L2247.GlobalIntTechOMfixed_elecS_adv <- 
  L2247.GlobalIntTechOMfixed_elecS_adv %>%
  left_join(L2247.GlobalIntTechOMfixed_elecS_adv %>% 
              filter(year == max_year_aeo) %>% 
              select(intermittent.technology, OM.fixed), by = "intermittent.technology") %>%
  rename(OM.fixed = OM.fixed.x, OM.fixed.max.year.aeo = OM.fixed.y) %>%
  mutate(OM.fixed = if_else(year > max_year_aeo, OM.fixed.max.year.aeo * OM.fixed.ratio, OM.fixed)) %>%
  select(-OM.fixed.ratio, -OM.fixed.max.year.aeo) %>%
  filter(intermittent.technology.y %in% adv_tech_list) %>%
  select(-intermittent.technology.y) %>%
  filter(year > 2015) %>%
  mutate(OM.fixed = if_else(is.nan(OM.fixed) == TRUE,0, OM.fixed))

# Cleaning up fixed-OM tables to get them ready for printing. We don't need capacity factor asumptions in this table since we did not
# update them.

L2247.GlobalTechOMfixedOnly_elecS_adv <-
  L2247.GlobalTechOMfixed_elecS_adv

L2247.GlobalIntTechOMfixedOnly_elecS_adv <-
  L2247.GlobalIntTechOMfixed_elecS_adv


# Advanced Technology Variable OM

L2247.GlobalTechOMvar_elecS_adv <-
  L2234.GlobalTechOMvar_elecS %>%
  left_join(L2234.GlobalTechOMvar_elecS %>% 
              filter(year == max_year_aeo) %>% 
              select(technology, OM.var), by = "technology") %>%
  rename(OM.var = OM.var.x, OM.var.max.year.aeo = OM.var.y) %>%
  mutate(OM.var.ratio = OM.var/OM.var.max.year.aeo) %>%
  select(-OM.var.max.year.aeo) %>%
  left_join(elecS_tech_associations %>%
              select(-supplysector, -subsector.1), by = c("technology" = "Electric.sector.technology", "supplysector" = "Electric.sector", "subsector")) %>%
  left_join(A23.elec_OM_aeo_adv %>% select(-OM.fixed), by = c("year", "technology.y" = "GCAM.technology")) %>%
  mutate(OM.var.x = if_else(is.na(OM.var.y) == TRUE, OM.var.x,OM.var.y)) %>%
  rename(OM.var = OM.var.x) %>%
  select(-OM.var.y) 

L2247.GlobalTechOMvar_elecS_adv <- 
  L2247.GlobalTechOMvar_elecS_adv %>%
  left_join(L2247.GlobalTechOMvar_elecS_adv %>% 
              filter(year == max_year_aeo) %>% 
              select(technology, OM.var), by = "technology") %>%
  rename(OM.var = OM.var.x, OM.var.max.year.aeo = OM.var.y) %>%
  mutate(OM.var = if_else(year > max_year_aeo, OM.var.max.year.aeo * OM.var.ratio, OM.var)) %>%
  select(-OM.var.ratio, -OM.var.max.year.aeo) %>%
  filter(technology.y %in% adv_tech_list) %>%
  select(-technology.y) %>%
  filter(year > 2015) %>%
  mutate(OM.var = if_else(is.nan(OM.var) == TRUE,0, OM.var))

# Advanced Intermittent Technology Variable OM

L2247.GlobalIntTechOMvar_elecS_adv <-
  L2234.GlobalIntTechOMvar_elecS %>%
  left_join(L2234.GlobalIntTechOMvar_elecS %>% 
              filter(year == max_year_aeo) %>% 
              select(intermittent.technology, OM.var), by = "intermittent.technology") %>%
  rename(OM.var = OM.var.x, OM.var.max.year.aeo = OM.var.y) %>%
  mutate(OM.var.ratio = OM.var/OM.var.max.year.aeo) %>%
  select(-OM.var.max.year.aeo) %>%
  left_join(elecS_inttech_associations %>%
              select(-supplysector, -subsector.1), by = c("intermittent.technology" = "Electric.sector.intermittent.technology", "supplysector" = "Electric.sector", "subsector")) %>%
  left_join(A23.elec_OM_aeo_adv %>% select(-OM.fixed), by = c("year", "intermittent.technology.y" = "GCAM.technology")) %>%
  mutate(OM.var.x = if_else(is.na(OM.var.y) == TRUE, OM.var.x,OM.var.y)) %>%
  rename(OM.var = OM.var.x) %>%
  select(-OM.var.y) 

L2247.GlobalIntTechOMvar_elecS_adv <- 
  L2247.GlobalIntTechOMvar_elecS_adv %>%
  left_join(L2247.GlobalIntTechOMvar_elecS_adv %>% 
              filter(year == max_year_aeo) %>% 
              select(intermittent.technology, OM.var), by = "intermittent.technology") %>%
  rename(OM.var = OM.var.x, OM.var.max.year.aeo = OM.var.y) %>%
  mutate(OM.var = if_else(year > max_year_aeo, OM.var.max.year.aeo * OM.var.ratio, OM.var)) %>%
  select(-OM.var.ratio, -OM.var.max.year.aeo) %>%
  filter(intermittent.technology.y %in% adv_tech_list) %>%
  select(-intermittent.technology.y) %>%
  filter(year > 2015) %>%
  mutate(OM.var = if_else(is.nan(OM.var) == TRUE,0, OM.var))

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

# Capital Costs
write_mi_data(L2247.GlobalTechCapitalOnly_elecS, "GlobalTechCapitalOnly", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalIntTechCapitalOnly_elecS", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA.xml")
write_mi_data(L2247.GlobalIntTechCapitalOnly_elecS, "GlobalIntTechCapitalOnly", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalIntTechCapitalOnly_elecS", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA.xml", "GCAMUSA_XML_FINAL", "elecS_costs_USA.xml", "", xml_tag="outFile" )

# Adv Tech costs 
write_mi_data(L2247.GlobalTechCapitalOnly_elecS_adv, "GlobalTechCapitalOnly", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalTechCapitalOnly_elecS_adv", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_adv.xml")
write_mi_data(L2247.GlobalIntTechCapitalOnly_elecS_adv, "GlobalIntTechCapitalOnly", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalIntTechCapitalOnly_elecS_adv", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_adv.xml")
write_mi_data(L2247.GlobalTechOMfixedOnly_elecS_adv, "GlobalTechOMfixed", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalIntTechOMfixedOnly_elecS_adv", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_adv.xml")
write_mi_data(L2247.GlobalIntTechOMfixedOnly_elecS_adv, "GlobalIntTechOMfixed", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalIntTechOMfixedOnly_elecS_adv", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_adv.xml")
write_mi_data(L2247.GlobalTechOMvar_elecS_adv, "GlobalTechOMvar", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalTechOMvar_elecS_adv", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_adv.xml")
write_mi_data(L2247.GlobalIntTechOMvar_elecS_adv, "GlobalIntTechOMvar", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalIntTechOMvar_elecS_adv", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_adv.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_adv.xml", "GCAMUSA_XML_FINAL", "elecS_costs_USA_adv.xml", "", xml_tag="outFile" )

# ITC 
write_mi_data(L2247.GlobalTechFCROnly_elecS_itc, "GlobalTechFCROnly", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalTechFCROnly_elecS_itc", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_itc.xml")
write_mi_data(L2247.GlobalIntTechFCROnly_elecS_itc, "GlobalIntTechFCROnly", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalIntTechFCROnly_elecS_itc", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_itc.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_itc.xml", "GCAMUSA_XML_FINAL", "elecS_costs_USA_itc.xml", "", xml_tag="outFile" )

# PTC
write_mi_data(L2247.GlobalTechRegPriceAdj_ptc, "GlobalTechRegPriceAdj", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalTechRegPriceAdj_ptc", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_ptc.xml")
write_mi_data(L2247.GlobalIntTechRegPriceAdj_ptc, "GlobalIntTechRegPriceAdj", "GCAMUSA_LEVEL2_DATA", "L2247.GlobalIntTechRegPriceAdj_ptc", "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_ptc.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elecS_costs_USA_ptc.xml", "GCAMUSA_XML_FINAL", "elecS_costs_USA_ptc.xml", "", xml_tag="outFile" )

}

logstop()
