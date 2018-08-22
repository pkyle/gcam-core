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
logstart( "L2012.socioeconomics_SSPs_USA.R" )
printlog( "GCAM-USA population and GDP in the SSPs" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
L100.Pop_thous_state_SSP <- readdata( "GCAMUSA_LEVEL1_DATA", "L100.Pop_thous_state_SSP" )
L201.LaborProductivity_SSP2 <- readdata( "SOCIO_LEVEL2_DATA", "L201.LaborProductivity_SSP2" , skip = 4)

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "L2012.Pop_GCAMUSA_SSP: Population by state in the SSPs" )
L201.Pop_GCAMUSA_SSP <- interpolate_and_melt(L100.Pop_thous_state_SSP, model_years, value.name = "totalPop", digits = digits_Pop) %>%
  rename(region = state)
for(i in unique(L201.Pop_GCAMUSA_SSP$Scenario)){
  object <- subset(L201.Pop_GCAMUSA_SSP, Scenario == i)[names_Pop]
  objectname <- paste0("L201.Pop_USA_", i)
  batchXMLstring <- paste0( "batch_population_USA_", i, ".xml" )
  write_mi_data( object, "Pop", "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", batchXMLstring )
  XMLstring <- sub( "batch_", "", batchXMLstring )
  insert_file_into_batchxml( "GCAMUSA_XML_BATCH", batchXMLstring, "GCAMUSA_XML_FINAL", XMLstring, "", xml_tag="outFile" )
}

#GDP - not done for now

logstop()
