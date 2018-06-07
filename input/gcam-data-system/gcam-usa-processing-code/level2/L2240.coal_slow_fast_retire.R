# This script creates add-on files to take the fractional reduction in coal electricity generation for each state and forces that 
# generation to retire in 2015. It also tempers retirement assumptions for the remaining coal fleet to allow 
# most 2015 generation to continue through mid-century.

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
logstart( "L2240.coal_slow_fast_retire_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "L2240.coal_slow_fast_retire_USA.R: Model input for removing coal capacity retired between 2010 and 2015" )

# Authors: Matthew Binsted and Gokul Iyer
# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

if(use_mult_load_segments == "TRUE") {
  A23.coal_conv_pul_delete <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.coal_conv_pul_delete" )  %>%
    filter(grepl("generation", supplysector))
  A23.elec_tech_associations_coal_retire <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elec_tech_associations_coal_retire" ) %>%
    filter(grepl("generation", Electric.sector))
  A23.elec_tech_coal_retire_SCurve <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elec_tech_coal_retire_SCurve" ) %>%
    filter(grepl("generation", Electric.sector)) %>%
    select(-Electric.sector)
  fraction_fast_retire_generation <- readdata( "GCAMUSA_LEVEL0_DATA","fraction_fast_retire_generation" , skip = 4 )
  StubTechProd_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.StubTechProd_elecS_USA" , skip = 4 )
  StubTechEff_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.StubTechEff_elecS_USA" , skip = 4 )
  StubTechMarket_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.StubTechMarket_elecS_USA" , skip = 4 )
  GlobalTechShrwt_elec <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.GlobalTechShrwt_elecS" , skip = 4 )
  GlobalTechCapital_elec <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.GlobalTechCapital_elecS" , skip = 4 )
  GlobalTechOMfixed_elec <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.GlobalTechOMfixed_elecS" , skip = 4 )
  GlobalTechOMvar_elec <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.GlobalTechOMvar_elecS" , skip = 4 )
  GlobalTechEff_elec <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.GlobalTechEff_elecS" , skip = 4 )
  GlobalTechProfitShutdown_elec <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.GlobalTechProfitShutdown_elecS" , skip = 4 )
  elec_ghg_emissions_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2236.elecS_ghg_emissions_USA" , skip = 4 )
    
} else{
  A23.coal_conv_pul_delete <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.coal_conv_pul_delete" ) %>%
    filter(supplysector == "electricity")
  A23.elec_tech_associations_coal_retire <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elec_tech_associations_coal_retire" ) %>%
    filter(Electric.sector == "electricity")
  A23.elec_tech_coal_retire_SCurve <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elec_tech_coal_retire_SCurve" ) %>%
    filter(Electric.sector == "electricity") %>%
    select(-Electric.sector)
  fraction_fast_retire_generation <- readdata( "GCAMUSA_LEVEL0_DATA","fraction_fast_retire_generation" , skip = 4 )
  StubTechProd_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L223.StubTechProd_elec_USA" , skip = 4 )
  StubTechEff_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L223.StubTechEff_elec_USA" , skip = 4 )
  StubTechMarket_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L223.StubTechMarket_elec_USA" , skip = 4 )
  GlobalTechShrwt_elec <- readdata( "ENERGY_LEVEL2_DATA","L223.GlobalTechShrwt_elec" , skip = 4 )
  GlobalTechCapital_elec <- readdata( "ENERGY_LEVEL2_DATA","L223.GlobalTechCapital_elec" , skip = 4 ) %>%
    rename(supplysector = sector.name, subsector = subsector.name)
  GlobalTechOMfixed_elec <- readdata( "ENERGY_LEVEL2_DATA","L223.GlobalTechOMfixed_elec" , skip = 4 ) %>%
    rename(supplysector = sector.name, subsector = subsector.name)
  GlobalTechOMvar_elec <- readdata( "ENERGY_LEVEL2_DATA","L223.GlobalTechOMvar_elec" , skip = 4 ) %>%
    rename(supplysector = sector.name, subsector = subsector.name)
  GlobalTechEff_elec <- readdata( "ENERGY_LEVEL2_DATA","L223.GlobalTechEff_elec" , skip = 4 ) %>%
    rename(supplysector = sector.name, subsector = subsector.name)
  GlobalTechProfitShutdown_elec <- readdata( "ENERGY_LEVEL2_DATA","L223.GlobalTechProfitShutdown_elec" , skip = 4 ) %>%
    rename(supplysector = sector.name, subsector = subsector.name)
  elec_ghg_emissions_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L273.en_ghg_emissions_USA" , skip = 4 ) %>%
    filter(supplysector == "electricity")
}

# -----------------------------------------------------------------------------
# 2. Perform computations

gcamusa_regions <- unique(states_subregions$state)

# 2.0. Delete old coal_base_conv pul stub-technology in the states 
L2240.coal_conv_pul_delete <- write_to_all_states(A23.coal_conv_pul_delete, c("region", "supplysector", "subsector", "stub.technology"))

# 2.1.a. Create two technologies: coal_base_conv pul_slow_retire and coal_base_conv pul_fast_retire 

printlog( "L2240.StubTechProd_elec_USA_coalret:  Calibration outputs for base load coal electricity plants by U.S. state" )

L2240.coal_retire_temp <- A23.elec_tech_associations_coal_retire

L2240.coal_retire_temp %>% 
  repeat_and_add_vector('year', model_base_years) %>% 
  repeat_and_add_vector('region', gcamusa_regions) %>% 
  left_join(StubTechProd_elec_USA, by = c("region", "Electric.sector" = "supplysector", "subsector", "technology" = "stub.technology", "year")) %>% 
  left_join(fraction_fast_retire_generation, by = c("region")) -> L2240.coal_retire_temp

L2240.coal_retire_temp %>%  
  filter(grepl("fast_retire", Electric.sector.technology)) %>%
  mutate(calOutputValue = calOutputValue * fast.retire) %>%
  select(-technology, -fast.retire) -> L2240.coal_fast_retire_temp

L2240.coal_retire_temp %>%  
  filter(grepl("slow_retire", Electric.sector.technology)) %>% 
  mutate(slow.retire = 1 - fast.retire) %>%
  mutate(calOutputValue = calOutputValue * slow.retire) %>%
  select(-technology, -fast.retire, -slow.retire) -> L2240.coal_slow_retire_temp

L2240.coal_fast_retire_temp %>% 
  bind_rows(L2240.coal_slow_retire_temp) %>% 
  rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>% 
  select(region, supplysector, subsector, stub.technology, year, calOutputValue, share.weight.year, subs.share.weight, share.weight) -> L2240.StubTechProd_elec_USA_coalret

# 2.1.b. Create a table to read in efficiencies for the new technologies in calibration years 

printlog( "L2240.StubTechEff_elec_USA_coalret: Efficiencies of U.S. base load coal electricity plants in calibration years" )

L2240.StubTechEff_elec_USA_coalret <- A23.elec_tech_associations_coal_retire

L2240.StubTechEff_elec_USA_coalret %>% 
  repeat_and_add_vector('year', model_base_years) %>% 
  repeat_and_add_vector('region', gcamusa_regions) %>% 
  left_join(StubTechEff_elec_USA, by= c("region", "Electric.sector" = "supplysector", "subsector", "technology" = "stub.technology", "year")) %>%
  filter(efficiency != 0) %>%
  select(region, Electric.sector, subsector, Electric.sector.technology, year, minicam.energy.input, efficiency, market.name) %>%
  rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) -> L2240.StubTechEff_elec_USA_coalret

# 2.2.  Create a table to read in s-curve retirement parameters for the new technologies

printlog( "L2240.StubTechSCurve_elec_coalret:  S-curve shutdown decider for historic U.S. base load coal electricity plants" )

L2240.StubTechProd_elec_USA_coalret %>% 
  select(region, supplysector, subsector, stub.technology, year) %>% 
  left_join(A23.elec_tech_coal_retire_SCurve, by = c("stub.technology" = "Electric.sector.technology")) %>%
  filter(year == set_years("final-calibration-year")) -> L2240.StubTechSCurve_elec_coalret

# 2.3.  Energy and non-energy inputs for the new technologies 

# Energy Inputs
printlog( "L2240.StubTechMarket_elec_coalret:  Energy inputs" )

L2240.StubTechMarket_elec_coalret <- A23.elec_tech_associations_coal_retire
L2240.StubTechMarket_elec_coalret %>% 
  repeat_and_add_vector('year', model_years) %>% 
  repeat_and_add_vector('region', gcamusa_regions) %>%
  left_join(StubTechMarket_elec_USA, by= c("region", "Electric.sector" = "supplysector", "subsector", "technology" = "stub.technology", "year")) %>%
  select(region, Electric.sector, subsector, Electric.sector.technology, year, minicam.energy.input, market.name) %>%
  rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) -> L2240.StubTechMarket_elec_coalret
  
# Share-weights

printlog( "L2240.GlobalTechShrwt_elec_coalret: Shareweights for historic U.S. base load coal electricity plants" )

L2240.GlobalTechShrwt_elec_coalret <- A23.elec_tech_associations_coal_retire
L2240.GlobalTechShrwt_elec_coalret %>% 
  repeat_and_add_vector('year', model_years) %>% 
  left_join(GlobalTechShrwt_elec, by= c("technology","year")) %>% 
  select(sector.name, subsector.name, Electric.sector.technology, year, share.weight) %>%
  rename(technology = Electric.sector.technology) -> L2240.GlobalTechShrwt_elec_coalret

# Capital costs

printlog( "L2240.GlobalTechCapital_elec_coalret: Capital costs of historic U.S. base load coal electricity plants" )

L2240.GlobalTechCapital_elec_coalret <- A23.elec_tech_associations_coal_retire
L2240.GlobalTechCapital_elec_coalret %>% 
  repeat_and_add_vector('year', model_years) %>% 
  left_join(GlobalTechCapital_elec, by= c("Electric.sector" = "supplysector", "subsector", "technology", "year")) %>%
  select(-technology) %>%
  rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2240.GlobalTechCapital_elec_coalret

# Fixed OM costs

printlog( "L2240.GlobalTechOMfixed_elec_coalret: Fixed OM costs of historic U.S. base load coal electricity plants" )

L2240.GlobalTechOMfixed_elec_coalret <- A23.elec_tech_associations_coal_retire
L2240.GlobalTechOMfixed_elec_coalret %>% 
  repeat_and_add_vector('year', model_years) %>% 
  left_join(GlobalTechOMfixed_elec, by= c("Electric.sector" = "supplysector", "subsector", "technology", "year")) %>%
  select(-technology) %>%
  rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2240.GlobalTechOMfixed_elec_coalret

# Variable OM costs

printlog( "L2240.GlobalTechOMvar_elec_coalret: Variable OM costs of historic U.S. base load coal electricity plants" )

L2240.GlobalTechOMvar_elec_coalret <- A23.elec_tech_associations_coal_retire
L2240.GlobalTechOMvar_elec_coalret %>% 
  repeat_and_add_vector('year', model_years) %>% 
  left_join(GlobalTechOMvar_elec, by= c("Electric.sector" = "supplysector", "subsector", "technology", "year")) %>%
  select(-technology) %>%
  rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2240.GlobalTechOMvar_elec_coalret

# Efficiencies for future years - read in in the global technology database

printlog( "L2240.GlobalTechEff_elec_coalret: Efficiencies of historic U.S. base load coal electricity plants in future years" )

L2240.GlobalTechEff_elec_coalret <- A23.elec_tech_associations_coal_retire
L2240.GlobalTechEff_elec_coalret %>% 
  repeat_and_add_vector('year', model_years) %>% 
  left_join(GlobalTechEff_elec, by= c("Electric.sector" = "supplysector", "subsector", "technology", "year")) %>%
  select(-technology) %>%
  rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2240.GlobalTechEff_elec_coalret

# Profit Shutdown decider

printlog( "L2240.GlobalTechProfitShutdown_elec_coalret: Profit shut-down decider for historic U.S. base load coal electricity plants" )

L2240.GlobalTechProfitShutdown_elec_coalret <- A23.elec_tech_associations_coal_retire
L2240.GlobalTechProfitShutdown_elec_coalret %>% 
  repeat_and_add_vector('year', model_years) %>% 
  filter(year > 2005) %>% 
  left_join(GlobalTechProfitShutdown_elec, by= c("Electric.sector" = "supplysector", "subsector", "technology", "year")) %>%
  select(-technology) %>%
  rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2240.GlobalTechProfitShutdown_elec_coalret

# Emission coefficients

printlog( "L2240.ghg_emissions_coalret_USA: Non-CO2 emissions coefficients for coal fast & slow retire technologies" )

L2240.ghg_emissions_coalret_USA <- A23.elec_tech_associations_coal_retire
L2240.ghg_emissions_coalret_USA %>% 
  repeat_and_add_vector('region', gcamusa_regions) %>%
  left_join(elec_ghg_emissions_USA, 
            by= c("region", "Electric.sector" = "supplysector", "subsector", "technology" = "stub.technology")) %>%
  rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>% 
  left_join(fraction_fast_retire_generation, by = c("region")) %>%
  mutate(share = ifelse(grepl("slow_retire", stub.technology), 1 - fast.retire, fast.retire))  %>%
  mutate(input.emissions = input.emissions * share) %>%
  select(names_StubTechYr, Non.CO2, input.emissions) -> L2240.ghg_emissions_coalret_USA

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2240.coal_conv_pul_delete, "DeleteStubTech", "GCAMUSA_LEVEL2_DATA", "L2240.coal_conv_pul_delete", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml" )
write_mi_data( L2240.StubTechProd_elec_USA_coalret, "StubTechProd", "GCAMUSA_LEVEL2_DATA", "L2240.StubTechProd_elec_USA_coalret", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml")
write_mi_data( L2240.StubTechEff_elec_USA_coalret, "StubTechEff", "GCAMUSA_LEVEL2_DATA", "L2240.StubTechEff_elec_USA_coalret", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml")
write_mi_data( L2240.StubTechSCurve_elec_coalret, "StubTechSCurve", "GCAMUSA_LEVEL2_DATA", "L2240.StubTechSCurve_elec_coalret", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml")
write_mi_data( L2240.StubTechMarket_elec_coalret, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L2240.StubTechMarket_elec_coalret", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml")
write_mi_data( L2240.GlobalTechShrwt_elec_coalret, "GlobalTechShrwt", "GCAMUSA_LEVEL2_DATA", "L2240.GlobalTechShrwt_elec_coalret", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml")
write_mi_data( L2240.GlobalTechCapital_elec_coalret, "GlobalTechCapital", "GCAMUSA_LEVEL2_DATA", "L2240.GlobalTechCapital_elec_coalret", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml")
write_mi_data( L2240.GlobalTechOMfixed_elec_coalret, "GlobalTechOMfixed", "GCAMUSA_LEVEL2_DATA", "L2240.GlobalTechOMfixed_elec_coalret", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml")
write_mi_data( L2240.GlobalTechOMvar_elec_coalret, "GlobalTechOMvar", "GCAMUSA_LEVEL2_DATA", "L2240.GlobalTechOMvar_elec_coalret", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml")
write_mi_data( L2240.GlobalTechEff_elec_coalret, "GlobalTechEff", "GCAMUSA_LEVEL2_DATA", "L2240.GlobalTechEff_elec_coalret", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml")
write_mi_data( L2240.GlobalTechProfitShutdown_elec_coalret, "GlobalTechProfitShutdown", "GCAMUSA_LEVEL2_DATA", "L2240.GlobalTechProfitShutdown_elec_coalret", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml")
write_mi_data( L2240.ghg_emissions_coalret_USA, "InputEmissions", "GCAMUSA_LEVEL2_DATA", "L2240.ghg_emissions_coalret_USA", "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_coal_retire_USA.xml", "GCAMUSA_XML_FINAL", "coal_retire_USA.xml", "", xml_tag="outFile" )

logstop()
