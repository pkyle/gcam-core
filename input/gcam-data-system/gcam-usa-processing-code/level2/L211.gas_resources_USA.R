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
logstart( "L211.gas_resources_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for GCAM USA resources" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
A23.gas_sector_vertical <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.gas_sector_vertical" )
L111.gas_supply_state_T_EJ <- readdata( "GCAMUSA_LEVEL1_DATA", "L111.gas_supply_state_T_EJ" )
L111.gas_prod_state_T_Yh_EJ <- readdata( "GCAMUSA_LEVEL1_DATA", "L111.gas_prod_state_T_Yh_EJ" )
ETSAP_gas_cost_range <- readdata( "GCAMUSA_LEVEL0_DATA", "ETSAP_gas_cost_range" )
BOEM_gas_cost <- readdata( "GCAMUSA_LEVEL0_DATA", "BOEM_gas_cost", skip = 3 )
A10.TechChange <- readdata( "GCAMUSA_ASSUMPTIONS", "A10.TechChange" )
A10.subsector_interp <- readdata( "GCAMUSA_ASSUMPTIONS", "A10.subsector_interp" )
A10.subsector_shrwt <- readdata( "GCAMUSA_ASSUMPTIONS", "A10.subsector_shrwt" )
L111.unconv_gas_supply_state_EJ<- readdata( "GCAMUSA_LEVEL1_DATA", "L111.unconv_gas_supply_state_EJ" )

L201.ghg_res <- readdata( "EMISSIONS_LEVEL2_DATA", "L201.ghg_res", skip = 4 )
L201.nonghg_res <- readdata( "EMISSIONS_LEVEL2_DATA", "L201.nonghg_res", skip = 4 )
L252.ResMAC_fos <- readdata( "EMISSIONS_LEVEL2_DATA", "L252.ResMAC_fos", skip = 4 )

# NOTE: FUNCTION STILL NEEDS TO BE RE-WRITTEN IN DPLYR
compute_offshore_costs <- function( supply, costs ) {
    ret.costs <- data.frame()
    grades <- c( "grade.hist", "grade.1", "grade.2", "grade.3", "grade.max" )
    supply$cumul.1 <- supply$grade.1
    supply$cumul.2 <- supply$cumul.1 + supply$grade.2
    supply$cumul.3 <- supply$cumul.2 + supply$grade.3
    for( r in unique( costs$region ) ) {
        costs.region <- subset( costs, region == r )
        supply.region <- subset( supply, state == r )
        lmfit <- lm( formula = price ~ sqrt( quantity ) + quantity, data=costs.region )
        ret.costs.region <- data.frame( state=r, grade=grades, cost=0 )
        min.cost <- min( costs.region$price ) + 1.5
        ret.costs.region$cost[1] <- min.cost * 0.5
        ret.costs.region$cost[2] <- min.cost * 0.9
        ret.costs.region$cost[3] <- min.cost
        ret.costs.region$cost[4:5] <- predict( lmfit, data.frame( quantity=c( supply.region$cumul.2, supply.region$cumul.3 ) ) )
        # TODO: get better grasp of how "reserve adjustment factors" was taking into
        # account.  They claim a value of 0.4.
        ret.costs.region$cost[4] <- ret.costs.region$cost[4] * 0.6
        ret.costs.region$cost[4] <- pmax( ret.costs.region$cost[4], ret.costs.region$cost[3] * 1.1 )
        ret.costs.region$cost[5] <- ret.costs.region$cost[5] * 0.6
        ret.costs <- rbind( ret.costs, ret.costs.region )
    }
    return(ret.costs)
}

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files
# Structure for "natural gas production" supply sector
printlog( "L211.Supplysector_NGprod: USA natural gas production supply sector" )
L211.gas_prod_sector_name <- "natural gas production"

A23.gas_sector_vertical %>% 
  filter(supplysector == L211.gas_prod_sector_name) %>%
  mutate(region = "USA", 
         output.unit="EJ", 
         input.unit="EJ", 
         price.unit="1975$/GJ", 
         logit.year.fillout=model_base_years[1], 
         logit.type=NA) %>% 
  select(region, supplysector, output.unit, input.unit, price.unit, 
         logit.year.fillout, logit.exponent, logit.type) -> L211.Supplysector_NGprod

# Need to create dummy socio-economics for offshore resource regions
printlog( "Socioeconomic information in the offshore regions (required for GCAM to run with these regions)" )
printlog( "L211.InterestRate_Offshore: Interest rates in the offshore resource regions" )
L211.offshore_regions <- unique( BOEM_gas_cost$region )
L211.InterestRate_Offshore <- data.frame( region = L211.offshore_regions, interest.rate = default_interest.rate )

printlog( "L211.Pop_Offshore: Population" )
L211.InterestRate_Offshore %>% 
  repeat_and_add_vector('year', model_years) %>% 
  mutate(totalPop = 1) %>%
  select(-interest.rate) -> L211.Pop_Offshore

printlog( "L211.BaseGDP_Offshore: Base GDP in Offshore resource regions" )
L211.BaseGDP_Offshore <- data.frame( region = L211.offshore_regions, baseGDP = 1 )

printlog( "L211.LaborForceFillout_Offshore: labor force in the Offshore resource regions" )
L211.LaborForceFillout_Offshore <- data.frame(
  region = L211.offshore_regions,
  year.fillout = min( model_base_years ),
  laborforce = default_laborforce )

# Create resource supply curves
printlog( "L211.PrimaryCO2Coef: Add CO2 coefficients." )
L111.gas_supply_state_T_EJ %>% 
  bind_rows(L111.unconv_gas_supply_state_EJ) %>%
  distinct(state, resource, .keep_all = FALSE) %>%
  mutate(co2.coef = 14.2) -> L211.PrimaryCO2Coef

printlog( "L211.Depresource: resource parameters" )
L211.PrimaryCO2Coef %>% 
  select(state, resource) %>% 
  mutate(output.unit = "EJ", 
         price.unit = "1975$/GJ", 
         market = state) -> L211.Depresource

L211.PrimaryCO2Coef %>% 
  select(state, resource) %>% 
  mutate(year = model_base_years[1], 
         price = 0.81) -> L211.DepresourcePrice

printlog( "L211.DepresourceTechChange: subresource technological change" )
L211.PrimaryCO2Coef %>% 
  select(state, resource) %>% 
  left_join(A10.TechChange %>% gather(year, value, -resource, -subresource), by = c("resource")) %>%
  mutate(year = as.numeric(gsub("X", "", year))) -> L211.DepresourceTechChange

printlog( "L211.Grades: resource grades" )
# First need to specifiy resource costs
# Start with onshore cost esitmates
# Arbitrary but unless we care about historical prices it doesn't matter
ETSAP_gas_cost_range %>% 
  mutate(grade.hist = low_cost * 0.5, 
         grade.2 = ((high_cost - low_cost ) / 3) + low_cost) %>%
  # Assume that all gas will be used up at a cost 5 times higher than high_cost
  # TODO: better place for this assumptions
  mutate(grade.max = high_cost * 5) %>%
  gather(grade, cost, -type) %>%
  mutate(grade = if_else(grade == "low_cost", "grade.1", grade), 
         grade = if_else(grade == "high_cost", "grade.3", grade)) %>%
  rename(depresource = type) %>%
  mutate(cost = cost * conv_2008_1975_USD) -> L211.GradeCost.onshore
  
# Offshore cost esitmates are more detailed however we will just simply these
BOEM_gas_cost %>% 
  mutate(quantity = quantity * conv_MMcf_EJ * 1e6, 
         price = (price * conv_2005_1975_USD) / (conv_MMcf_EJ * 1e6) ) -> L211.GradeCost.offshore

# NOTE:  compute_offshore_costs FUNCTION MUST BE RE-WRITTEN BEFORE THIS CAN BE RE-WRITTEN IN DPLYR
L211.GradeCost.offshore <- compute_offshore_costs( L111.gas_supply_state_T_EJ, L211.GradeCost.offshore )
L211.GradeCost.offshore %>% 
  mutate(depresource = "offshore gas") -> L211.GradeCost.offshore

# Duplicate costs by state and put on and off shore together
L111.gas_supply_state_T_EJ %>% 
  distinct(state) %>%
  filter(!grepl( 'OCS', state )) -> onshore_states
L211.onshore_regions <- unique(onshore_states$state)

L211.GradeCost.onshore %>% 
  repeat_and_add_vector('state', L211.onshore_regions) %>%
  select(state, depresource, grade, cost) -> L211.GradeCost

L111.unconv_gas_supply_state_EJ %>% 
  rename(depresource = resource, cost = extractioncost) %>%
  select(state, depresource, grade, cost) -> L211.GradeCost.unconv

L211.GradeCost %>% 
  bind_rows(L211.GradeCost.offshore, L211.GradeCost.unconv) -> L211.GradeCost

# We need to add resource to cover historical production since it is not included in the supply curves
# Do not include the first year for cumulative production since the model does not consider it
L111.gas_prod_state_T_Yh_EJ %>% 
  select(state, depresource, subresource, X_model_base_years) %>%
  mutate(grade = "grade.hist") %>%
  gather(year, value, X_model_base_years) %>%
  mutate(year = as.numeric(gsub("X", "", year))) %>%
  group_by(state, depresource, subresource, grade) %>%
  mutate(timestep = year - lag(year, n = 1L)) %>%
  filter(timestep != is.na(timestep)) %>%
  mutate(value = value * timestep) %>%
  summarise(available = sum(value)) -> L211.CumulHistProduction

# Merge costs and available
# Sort by costs while grouping by state and resource to get grades in an appropriate order
L111.gas_supply_state_T_EJ %>%
  mutate(grade.max = 0) %>%
  rename(depresource = resource) %>%
  mutate(subresource = depresource) %>%
  gather(grade, available, -state, -depresource, -subresource) -> L211.GradeAvail

L111.unconv_gas_supply_state_EJ %>%
  select(state, resource, grade, available) %>%
  rename(depresource = resource) %>%
  mutate(subresource = depresource) -> L211.GradeAvail.unconv

L211.GradeAvail %>% bind_rows(L211.CumulHistProduction, L211.GradeAvail.unconv) %>%
  left_join(L211.GradeCost, by = c("state", "depresource", "grade")) %>%
  mutate(available = round(available, 7), 
         cost = round(cost, 3)) %>%
  arrange(state, depresource, cost) -> L211.Grades

# Create state regional natural gas sectors which aggregate resource types.
printlog( "L211.Sector: Regional natural gas sector to aggregate gas types." )
L111.gas_supply_state_T_EJ %>% 
  distinct(state) -> L211.gas_regions

# TODO: these logit assumptions matter
L211.gas_regions %>% 
  rename(region = state) %>%
  mutate(supplysector = L211.gas_prod_sector_name, 
         output.unit = "EJ", 
         input.unit = "EJ", 
         price.unit = "1975$/GJ", 
         logit.year.fillout = model_base_years[1], 
         logit.exponent = -3, 
         logit.type = NA) -> L211.Sector

L211.Supplysector_NGprod %>% bind_rows(L211.Sector) -> L211.Sector

L211.SectorLogitTables <- get_logit_fn_tables( L211.Sector, names_SupplysectorLogitType,
                                               base.header="Supplysector_", include.equiv.table=T, write.all.regions=F )

L211.Sector %>% select(names_Supplysector) -> L211.Sector

# Add USA & state natural gas production sectors to CO2 Coefs, format for output
L211.PrimaryCO2Coef %>% 
  bind_rows(L211.Sector %>% 
              select(region) %>% 
              rename(state = region) %>%
              mutate(resource = L211.gas_prod_sector_name, 
                     co2.coef = 14.2)) %>%
  rename(region = state, PrimaryFuelCO2Coef.name = resource, PrimaryFuelCO2Coef = co2.coef )-> L211.PrimaryCO2Coef

printlog( "L211.Subsector: Regional natural gas subsector to aggregate gas types." )
# NOTE: these logit assumptions do not matter as there is no competition at this nest
L111.gas_supply_state_T_EJ %>%
  select(state, resource) %>%
  bind_rows(L111.unconv_gas_supply_state_EJ %>% 
              select(state, resource)) %>%
  distinct(state, resource) %>%
  rename(region = state) %>%
  rename(subsector = resource) %>%
  mutate(supplysector = L211.gas_prod_sector_name, 
         logit.year.fillout = model_base_years[1], 
         logit.exponent=-6, 
         logit.type=NA) -> L211.Subsector

L211.SubsectorLogitTables <- get_logit_fn_tables( L211.Subsector, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )

L211.Subsector %>% select(names_SubsectorLogit) -> L211.Subsector

printlog( "L211.SubsInterpRule: Regional natural gas subsector interpolation rules." )
# Shareweight defaults to zero, which will get reset by tech cal if appropriate
L211.Subsector %>% 
  select(names_Subsector) %>%
  mutate(year = model_base_years[1], 
         share.weight = 0) -> L211.SubsShrwtFlt

# Warning: we should partition this table in two if to.value is not NA
L211.Subsector %>% 
  select(names_Subsector) %>%
  left_join(A10.subsector_interp, by = c("supplysector", "subsector")) %>%
  set_years() %>%
  select(names_SubsectorInterp) -> L211.SubsInterpRule
  
#########################################################################################
#Create L211.Subs_shrwt for those states WITH unconventional resources
L211.SubsInterpRule %>%
  filter(subsector == "unconventional gas other") %>%
  select(region) %>%
  distinct(region)-> Unconventional_States

A10.subsector_shrwt %>%
  filter(region == "USA") %>%
  write_to_all_states(names(A10.subsector_shrwt)) %>%
  inner_join(Unconventional_States, by = c("region")) -> L211.Subs_shrwt

########################################################################################

printlog( "L211.GlobalDBTechInput: Pass through tech" )
L211.Subsector %>%
  distinct(supplysector,subsector) %>%
  mutate(technology = subsector) %>%
  repeat_and_add_vector('year', model_years) -> L211.GlobalDBTechInput

L211.GlobalDBTechInput %>% 
  mutate(share.weight = if_else(year <= final_model_base_year, 0, 1)) -> L211.GlobalDBTechShrwt

L211.GlobalDBTechInput %>% 
  mutate(minicam.energy.input = technology, 
         coefficient = 1) -> L211.GlobalDBTechCoef

printlog( "L211.TechCal: natural gas resource calibration" )
L111.gas_prod_state_T_Yh_EJ %>%
  select(state, depresource, X_model_base_years) %>%
  gather(year, value, X_model_base_years) %>%
  mutate(year = as.integer(gsub("X", "", year))) -> L211.gas_prod_state_T_Th_EJ

L211.gas_prod_state_T_Th_EJ %>% 
  filter(value > 0) %>%
  rename(region = state) %>%
  mutate(supplysector = L211.gas_prod_sector_name, 
         subsector = depresource, 
         stub.technology = depresource, 
         minicam.energy.input = depresource, 
         calibrated.value = round(value, 7), 
         share.weight.year = year,
         subs.share.weight = 1, 
         tech.share.weight = 1) %>%
  select(names_StubTechCalInput) -> L211.TechCal

#printlog( "L211.TechStubs: empty stubs for state / gas type that did not produce historically" )
L211.Subsector %>% 
  anti_join(L211.TechCal, by = c("region", "subsector")) %>%
  select(names_Subsector) %>%
  mutate(technology = subsector) -> L211.TechStubs

# Add these resources to the traded natural gas sector
printlog( "L211.TNGSubsInterp: The interpolation rule for the regions in the traded natural gas sector." )
L211.regional_ng_sector <- "natural gas production"
L211.gas_prod_state_T_Th_EJ %>% group_by(state, year) %>%
  summarise(value = sum(value)) %>% 
  ungroup(state) -> L211.gas_prod_state_Yh_EJ

L211.gas_prod_state_Yh_EJ %>% 
  distinct(state) %>%
  mutate(region = "USA", 
         supplysector = L211.regional_ng_sector, 
         subsector = paste(state, L211.gas_prod_sector_name),  
         apply.to = "share-weight", 
         from.year = as.character(final_model_base_year), 
         to.year = as.character(model_years[ length( model_years ) ]),
         interpolation.function="fixed" ) %>%
  select(-state) -> L211.TNGSubsInterp

L211.SubsInterpRule %>% 
  bind_rows(L211.TNGSubsInterp) -> L211.SubsInterpRule

printlog( "L211.TNGSubsectorLogit: The subsector logits for the regions in the traded natural gas sector." )
# NOTE: these logit assumptions do not matter as there is no competition at this nest
L211.TNGSubsInterp %>%
  select(names_Subsector) %>%
  mutate(logit.year.fillout = model_base_years[1], 
         logit.exponent = -6, 
         logit.type = NA) -> L211.TNGSubsectorLogit

L211.TNGSubsectorLogitTables <- get_logit_fn_tables( L211.TNGSubsectorLogit, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )

L211.TNGSubsectorLogit %>% select(names_SubsectorLogit) -> L211.TNGSubsectorLogit

# Combine these tables with the state tables to reduce number of files to write
L211.Subsector %>% bind_rows(L211.TNGSubsectorLogit) -> L211.Subsector

for( logit.table in names( L211.TNGSubsectorLogitTables ) ) {
    if( nrow( L211.TNGSubsectorLogitTables[[ logit.table ]]$data ) > 0 ) {
        L211.SubsectorLogitTables[[ logit.table ]]$data <- rbind( L211.SubsectorLogitTables[[ logit.table ]]$data,
            L211.TNGSubsectorLogitTables[[ logit.table ]]$data )
    }
}

printlog( "L211.TNGTech: create the tech to simply pass through state production" )
L211.gas_prod_state_Yh_EJ %>% 
  rename(market.name = state) %>%
  mutate(minicam.energy.input = L211.gas_prod_sector_name,
         region = "USA", 
         supplysector = L211.regional_ng_sector, 
         subsector = paste(market.name, minicam.energy.input), 
         technology = subsector) -> L211.TNGTech

L211.TNGTech %>% 
  mutate(calOutputValue = round(value, 7), 
         share.weight.year = year, 
         subs.share.weight = if_else(value > 0, 1, 0 ), 
         tech.share.weight = subs.share.weight) %>%
  select(names_Production) -> L211.TNGTechProduction

L211.TNGTech %>% 
  mutate(coefficient = 1) %>%
  select(names_TechCoef) -> L211.TNGTechCoef

printlog( "L211.DepresourceCal: subresource calproduction" )
L211.TechCal %>% 
  rename(depresource = subsector) %>% 
  rename(subresource = stub.technology) %>%
  rename(cal.production = calibrated.value) %>%
  select(region, depresource, subresource, year, cal.production) -> L211.DepresourceCal

printlog( "L211.ghg_NG: GHG emissions from natural gas production for all states / resource types" )
L211.NG_resources <- unique(L211.Depresource$resource)
L211.gas_regions <- unique(L211.gas_regions$state)

L201.ghg_res %>%
  filter(region == "USA", depresource =="natural gas") %>%
  repeat_and_add_vector('state', L211.gas_regions) %>%
  repeat_and_add_vector('resource', L211.NG_resources) %>%
  semi_join(L211.Depresource, by = c("state", "resource")) %>%
  select(state, resource, Non.CO2, emiss.coef) %>%
  rename(region = state, depresource = resource)-> L211.ghg_NG

printlog( "L211.nonghg_NG: Pollutant emissions from natural gas production for all states / resource types" )
L201.nonghg_res %>%
  filter(region == "USA", depresource =="natural gas") %>%
  repeat_and_add_vector('state', L211.gas_regions) %>%
  repeat_and_add_vector('resource', L211.NG_resources) %>%
  semi_join(L211.Depresource, by = c("state", "resource")) %>%
  select(state, resource, Non.CO2, emiss.coef) %>%
  rename(region = state, depresource = resource)-> L211.nonghg_NG

printlog( "L211.ResMAC_NG: Fossil resource MAC curves" )
L252.ResMAC_fos %>%
  filter(region == "USA", depresource =="natural gas") %>%
  repeat_and_add_vector('state', L211.gas_regions) %>%
  repeat_and_add_vector('resource', L211.NG_resources) %>%
  semi_join(L211.Depresource, by = c("state", "resource")) %>%
  select(state, resource, Non.CO2, mac.control, tax, mac.reduction, market.name) %>%
  rename(region = state, depresource = resource)-> L211.ResMAC_NG

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L211.PrimaryCO2Coef, "CarbonCoef", "GCAMUSA_LEVEL2_DATA", "L211.PrimaryCO2Coef", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.Depresource, "DepRsrc", "GCAMUSA_LEVEL2_DATA", "L211.Depresource", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.DepresourcePrice, "DepRsrcPrice", "GCAMUSA_LEVEL2_DATA", "L211.DepresourcePrice", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
# write_mi_data( L211.DepresourceCal, "DepResourceCal", "GCAMUSA_LEVEL2_DATA", "L211.DepresourceCal", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.DepresourceCal, "DepRsrcCalProd", "GCAMUSA_LEVEL2_DATA", "L211.DepresourceCal", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.DepresourceTechChange, "DepRsrcTechChange", "GCAMUSA_LEVEL2_DATA", "L211.DepresourceTechChange", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.Grades, "DepRsrcCurves", "GCAMUSA_LEVEL2_DATA", "L211.Grades", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )

# for( logit.table in names( L211.NGprodLogitTable ) ) {
# write_mi_data( L211.NGprodLogitTable[[ logit.table ]]$data, L211.NGprodLogitTable[[ logit.table ]]$header,
#     "GCAMUSA_LEVEL2_DATA", paste0("L211.", L211.NGprodLogitTable[[ logit.table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
#     "batch_gas_resources_USA.xml" )
# }
# write_mi_data( L211.Supplysector_NGprod, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L211.Supplysector_NGprod", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )

for( logit.table in names( L211.SectorLogitTables ) ) {
write_mi_data( L211.SectorLogitTables[[ logit.table ]]$data, L211.SectorLogitTables[[ logit.table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L211.", L211.SectorLogitTables[[ logit.table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
    "batch_gas_resources_USA.xml" )
}
write_mi_data( L211.Sector, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L211.Sector", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )

for( logit.table in names( L211.SubsectorLogitTables ) ) {
write_mi_data( L211.SubsectorLogitTables[[ logit.table ]]$data, L211.SubsectorLogitTables[[ logit.table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L211.", L211.SubsectorLogitTables[[ logit.table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
    "batch_gas_resources_USA.xml" )
}
write_mi_data( L211.Subsector, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L211.Subsector", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )

write_mi_data( L211.SubsShrwtFlt, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L211.SubsShrwtFlt", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.SubsInterpRule, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L211.SubsInterpRule", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.Subs_shrwt, "SubsectorShrwt", "GCAMUSA_LEVEL2_DATA", "L211.Subs_shrwt", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.GlobalDBTechShrwt, "GlobalTechShrwt", "GCAMUSA_LEVEL2_DATA", "L211.GlobalDBTechShrwt", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.GlobalDBTechCoef, "GlobalTechCoef", "GCAMUSA_LEVEL2_DATA", "L211.GlobalDBTechCoef", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.TechCal, "StubTechCalInput", "GCAMUSA_LEVEL2_DATA", "L211.TechCal", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.TechStubs, "StubTech", "GCAMUSA_LEVEL2_DATA", "L211.TechStubs", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.TNGTechProduction, "Production", "GCAMUSA_LEVEL2_DATA", "L211.TNGTechProduction", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.TNGTechCoef, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L211.TNGTechCoef", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.InterestRate_Offshore, "InterestRate", "GCAMUSA_LEVEL2_DATA", "L211.InterestRate_Offshore", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.Pop_Offshore, "Pop", "GCAMUSA_LEVEL2_DATA", "L211.Pop_Offshore", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.BaseGDP_Offshore, "BaseGDP", "GCAMUSA_LEVEL2_DATA", "L211.BaseGDP_Offshore", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )
write_mi_data( L211.LaborForceFillout_Offshore, "LaborForceFillout", "GCAMUSA_LEVEL2_DATA", "L211.LaborForceFillout_Offshore", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" )

write_mi_data( L211.ghg_NG, "ResEmissCoef", "GCAMUSA_LEVEL2_DATA", "L211.ghg_NG", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" ) 
write_mi_data( L211.nonghg_NG, "ResEmissCoef", "GCAMUSA_LEVEL2_DATA", "L211.nonghg_NG", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" ) 
write_mi_data( L211.ResMAC_NG, "ResMAC", "GCAMUSA_LEVEL2_DATA", "L211.ResMAC_NG", "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_gas_resources_USA.xml", "GCAMUSA_XML_FINAL", "gas_resources_USA.xml", "", xml_tag="outFile" )

logstop()
