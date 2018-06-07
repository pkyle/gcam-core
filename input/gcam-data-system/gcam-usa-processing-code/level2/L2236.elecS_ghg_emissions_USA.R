# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "GCAMUSAPROC_DIR" ) ){
  if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
    GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
  } else {
    stop("Could not determine location of usa data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
  }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))

logstart( "L2236.en_ghg_emissions_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))


printlog( "GHG emissions parameters for energy technologies in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )

if( use_mult_load_segments ){	

L1231.in_EJ_state_elec_F_tech <- readdata( "GCAMUSA_LEVEL1_DATA", "L1231.in_EJ_state_elec_F_tech" )
L201.en_ghg_emissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L201.en_ghg_emissions", skip = 4)
L241.nonco2_tech_coeff <- readdata( "EMISSIONS_LEVEL2_DATA", "L241.nonco2_tech_coeff", skip = 4)

A23.elecS_tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_associations" )
A23.elecS_tech_availability <-  readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_availability" )
L2234.StubTechMarket_elecS_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L2234.StubTechMarket_elecS_USA", skip = 4 )
L2234.StubTechProd_elecS_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L2234.StubTechProd_elecS_USA", skip = 4 )

# -----------------------------------------------------------------------------

# 2.  Build tables for CSVs
# 2a. Emission coefficients

# Write electricity emission coefficients to multiple load segments

# Remove technologies in the electricity load segments that don't make sense
L2234.load_segments <- unique(A23.elecS_tech_associations$Electric.sector)

A23.elecS_tech_associations %>% 
  anti_join(A23.elecS_tech_availability, 
            by = c("Electric.sector.technology" = "stub.technology")) %>%
  mutate(Electric.sector = factor(Electric.sector, levels = L2234.load_segments)) %>%
  arrange(subsector, Electric.sector) %>%
  mutate(Electric.sector = as.character(Electric.sector)) -> A23.elecS_tech_associations

L241.nonco2_tech_coeff %>%
  filter(region == "USA", supplysector == "electricity", Non.CO2 %in% c("N2O","CH4") ) %>%
  left_join(A23.elecS_tech_associations %>%
              select(-subsector),
            by = c("supplysector", "subsector" =  "subsector.1", "stub.technology" =  "technology")) %>%
  select(Electric.sector, subsector, Electric.sector.technology, year, Non.CO2, emiss.coeff) %>%
  rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
  repeat_and_add_vector('region', states) %>%
  semi_join(L2234.StubTechMarket_elecS_USA %>% 
              select(names_StubTechYr), 
            by = c("supplysector", "subsector", "stub.technology", "year", "region")) %>%
  select(names_StubTechNonCO2, emiss.coeff) -> L2236.elecS_ghg_tech_coeff_USA


# 2b. Input Emissions 

printlog( "L236.en_ghg_emissions_USA: Calibrated input emissions of N2O and CH4 by U.S. state" )
# Filter the emissions data for USA & electricity sector
L201.en_ghg_emissions %>%
  filter(region == "USA" & grepl("electricity", supplysector)) %>%
  spread(Non.CO2, input.emissions) -> L2236.elec_ghg_emissions_USA

# Organize the state fuel input data
# Electricity segments
L1231.in_EJ_state_elec_F_tech %>%
  gather(year,fuel_input,-state,-sector,-fuel,-technology) %>%
  mutate(year = as.numeric(substr(year,2,5)), sector = "electricity") %>%
  filter(year %in% L2236.elec_ghg_emissions_USA$year & technology %in% L2236.elec_ghg_emissions_USA$stub.technology) %>%
  left_join(A23.elecS_tech_associations %>%
              select(-subsector.1),
            by = c("sector" = "supplysector", "technology")) %>%
  select(state, supplysector = Electric.sector, subsector, stub.technology = Electric.sector.technology, 
         year, technology, fuel, tech_fuel_input = fuel_input) %>%
  left_join(L2234.StubTechProd_elecS_USA %>%
              select(names_StubTechYr, calOutputValue),
            by = c("state" = "region", "supplysector", "subsector", "stub.technology", "year")) %>%
  group_by(state, technology, year) %>%
  mutate(tech_calOuput = sum(calOutputValue),
         segment_share = calOutputValue / tech_calOuput,
         fuel_input = round(tech_fuel_input * segment_share, 6),
         fuel_input = if_else(is.na(fuel_input), 0, fuel_input)) %>%
  ungroup() %>%
  select(state, supplysector, stub.technology, year, technology, fuel, fuel_input) %>%
  semi_join(L2234.StubTechMarket_elecS_USA %>% 
              select(names_StubTechYr), 
            by = c("supplysector", "stub.technology", "year", "state"  = "region")) -> L2236.elecS_fuel_input_state

# Compute state shares for each category in the fuel input table
# Share out CH4 and N2O emissions by state based on the fuel input shares
L2236.elecS_fuel_input_state %>% 
  mutate(sector = "electricity") %>%
  left_join(L2236.elec_ghg_emissions_USA %>%
              select(-region) %>%
              rename(sector = supplysector, technology = stub.technology), 
            by = c("technology", "year")) %>%
  group_by(technology, year) %>%
  mutate(fuel_input_USA = sum(fuel_input),
         fuel_input_share = fuel_input / fuel_input_USA,
         CH4 = fuel_input_share * CH4,
         N2O = fuel_input_share * N2O) %>%
  ungroup() %>%
  select(region = state, supplysector, subsector, stub.technology, year, CH4, N2O) -> L2236.elecS_ghg_emissions_state
  
# Format for csv file
L2236.elecS_ghg_emissions_state %>%
  gather(Non.CO2, input.emissions, -region, -supplysector, -subsector, -stub.technology, -year, convert=TRUE) %>%
  arrange(region, supplysector, subsector, stub.technology, Non.CO2, year) %>%
  select(names_StubTechYr, Non.CO2, input.emissions) %>%
  mutate(input.emissions = round(input.emissions, digits_emissions)) -> L2236.elecS_ghg_emissions_USA

  
# Check for missing values
stopifnot(!any(is.na(L2236.elecS_ghg_tech_coeff_USA)))
stopifnot(!any(is.na(L2236.elecS_ghg_emissions_USA)))


# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2236.elecS_ghg_tech_coeff_USA, "InputEmissCoeff", "GCAMUSA_LEVEL2_DATA", "L2236.elecS_ghg_tech_coeff_USA", "GCAMUSA_XML_BATCH", "batch_elecS_ghg_emissions_USA.xml" ) 
write_mi_data( L2236.elecS_ghg_emissions_USA, "InputEmissions", "GCAMUSA_LEVEL2_DATA", "L2236.elecS_ghg_emissions_USA", "GCAMUSA_XML_BATCH", "batch_elecS_ghg_emissions_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elecS_ghg_emissions_USA.xml", "GCAMUSA_XML_FINAL", "elecS_ghg_emissions_USA.xml", "", xml_tag="outFile" )

} #close out from use_mult_load_segments

logstop()
