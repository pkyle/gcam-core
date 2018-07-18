# This script creates the USA industry sector in order to track industrial processes emissions.

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
logstart( "L2324.industry_process_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Add on file to track industrial processes emissions" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ind_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

A32.process_eff_USA <- readdata( "GCAMUSA_ASSUMPTIONS", "A32.process_eff_USA" )

L232.StubTechCoef_industry_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L232.StubTechCoef_industry_USA", skip = 4  )

# -----------------------------------------------------------------------------
# 2. Perform computations

L232.StubTechCoef_industry_USA %>%
  select(-minicam.energy.input, -coefficient, -market.name) %>%
  left_join(A32.process_eff_USA %>%
              select(-region),
            by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
  mutate(market.name = "USA") -> L2324.StubTechCoef_indproc_USA

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L2324.StubTechCoef_indproc_USA, "StubTechCoef", "GCAMUSA_LEVEL2_DATA", "L2324.StubTechCoef_indproc_USA", "GCAMUSA_XML_BATCH", "batch_industry_process_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_industry_process_USA.xml", "GCAMUSA_XML_FINAL", "industry_process_USA.xml", "", xml_tag="outFile" )

logstop()
