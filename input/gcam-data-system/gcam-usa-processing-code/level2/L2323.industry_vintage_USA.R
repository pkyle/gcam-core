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
logstart( "L2322.industry_vintage_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Add on file to read in s-curve parameters for industrial energy use sector" )

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

L232.StubTechMarket_ind_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L232.StubTechMarket_ind_USA", skip = 4 )
# -----------------------------------------------------------------------------
# 2. Perform computations

L232.StubTechMarket_ind_USA %>%
  select(-minicam.energy.input, -market.name) %>%
  filter(year >= max(model_base_years)) %>%
  filter(supplysector != "industrial feedstocks") %>%
  mutate ( lifetime = 60, steepness = 0.1, half.life = 30) -> L2322.StubTechSCurve_industry_USA


# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2322.StubTechSCurve_industry_USA, "StubTechSCurve", "GCAMUSA_LEVEL2_DATA", "L2322.StubTechSCurve_industry_USA", "GCAMUSA_XML_BATCH", "batch_industry_vintage_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_industry_vintage_USA.xml", "GCAMUSA_XML_FINAL", "industry_vintage_USA.xml", "", xml_tag="outFile" )

logstop()
