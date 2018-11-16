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
logstart( "LB2012.socioeconomics_SSPs_USA.R" )
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
L201.LaborProductivity_SSP3 <- readdata( "SOCIO_LEVEL2_DATA", "L201.LaborProductivity_SSP3" , skip = 4)
L201.LaborProductivity_SSP5 <- readdata( "SOCIO_LEVEL2_DATA", "L201.LaborProductivity_SSP5" , skip = 4)
L232.IncomeElasticity_ind_ssp3 <- readdata( "ENERGY_LEVEL2_DATA", "L232.IncomeElasticity_ind_ssp3", skip = 4)
L232.IncomeElasticity_ind_ssp5 <- readdata( "ENERGY_LEVEL2_DATA", "L232.IncomeElasticity_ind_ssp5", skip = 4)
L2321.IncomeElasticity_cement_ssp3 <- readdata( "ENERGY_LEVEL2_DATA", "L2321.IncomeElasticity_cement_ssp3", skip = 4)
L2321.IncomeElasticity_cement_ssp5 <- readdata( "ENERGY_LEVEL2_DATA", "L2321.IncomeElasticity_cement_ssp5", skip = 4)
L2321.Supplysector_cement_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L2321.Supplysector_cement_USA", skip = 4)

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "L2012.Pop_GCAMUSA_SSP: Population by state in the SSPs" )
L201.Pop_GCAMUSA_SSP <- interpolate_and_melt(L100.Pop_thous_state_SSP, model_years, value.name = "totalPop", digits = digits_Pop) %>%
  rename(region = state)
for(i in unique(L201.Pop_GCAMUSA_SSP$Scenario)){
  object <- subset(L201.Pop_GCAMUSA_SSP, Scenario == i)[names_Pop]
  objectname <- paste0("L2012.Pop_USA_", i)
  batchXMLstring <- paste0( "batch_population_USA_", i, ".xml" )
  write_mi_data( object, "Pop", "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", batchXMLstring )
  XMLstring <- sub( "batch_", "", batchXMLstring )
  insert_file_into_batchxml( "GCAMUSA_XML_BATCH", batchXMLstring, "GCAMUSA_XML_FINAL", XMLstring, "", xml_tag="outFile" )
}

printlog("GDP in selected SSPs - just modify the labor productivity assumptions from the defaults")
L2012.LaborProductivity_USA_SSP3 <- subset( L201.LaborProductivity_SSP3, region == "USA") %>%
  write_to_all_states(names_LaborProductivity)
L2012.LaborProductivity_USA_SSP5 <- subset( L201.LaborProductivity_SSP5, region == "USA") %>%
  write_to_all_states(names_LaborProductivity)

printlog("Income elasticities in selected SSPs")
# Industry
L2012.IncomeElasticity_ind_USA_SSP3 <- subset( L232.IncomeElasticity_ind_ssp3, region == "USA") %>%
  write_to_all_states(names_IncomeElasticity)
L2012.IncomeElasticity_ind_USA_SSP5 <- subset( L232.IncomeElasticity_ind_ssp5, region == "USA") %>%
  write_to_all_states(names_IncomeElasticity)

# Cement
L2012.IncomeElasticity_cement_USA_SSP3 <- subset( L2321.IncomeElasticity_cement_ssp3, region == "USA") %>%
  write_to_all_states(names_IncomeElasticity) %>%
  filter(region %in% L2321.Supplysector_cement_USA$region)
L2012.IncomeElasticity_cement_USA_SSP5 <- subset( L2321.IncomeElasticity_cement_ssp5, region == "USA") %>%
  write_to_all_states(names_IncomeElasticity) %>%
  filter(region %in% L2321.Supplysector_cement_USA$region)

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2012.LaborProductivity_USA_SSP3, "LaborProductivity", "GCAMUSA_LEVEL2_DATA", "L2012.LaborProductivity_USA_SSP3", "GCAMUSA_XML_BATCH", "batch_GDP_elas_USA_SSP3.xml" )
write_mi_data( L2012.IncomeElasticity_ind_USA_SSP3, "IncomeElasticity", "GCAMUSA_LEVEL2_DATA", "L2012.IncomeElasticity_ind_USA_SSP3", "GCAMUSA_XML_BATCH", "batch_GDP_elas_USA_SSP3.xml" )
write_mi_data( L2012.IncomeElasticity_cement_USA_SSP3, "IncomeElasticity", "GCAMUSA_LEVEL2_DATA", "L2012.IncomeElasticity_cement_USA_SSP3", "GCAMUSA_XML_BATCH", "batch_GDP_elas_USA_SSP3.xml" )

write_mi_data( L2012.LaborProductivity_USA_SSP5, "LaborProductivity", "GCAMUSA_LEVEL2_DATA", "L2012.LaborProductivity_USA_SSP5", "GCAMUSA_XML_BATCH", "batch_GDP_elas_USA_SSP5.xml" )
write_mi_data( L2012.IncomeElasticity_ind_USA_SSP5, "IncomeElasticity", "GCAMUSA_LEVEL2_DATA", "L2012.IncomeElasticity_ind_USA_SSP5", "GCAMUSA_XML_BATCH", "batch_GDP_elas_USA_SSP5.xml" )
write_mi_data( L2012.IncomeElasticity_cement_USA_SSP5, "IncomeElasticity", "GCAMUSA_LEVEL2_DATA", "L2012.IncomeElasticity_cement_USA_SSP5", "GCAMUSA_XML_BATCH", "batch_GDP_elas_USA_SSP5.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_GDP_elas_USA_SSP3.xml", "GCAMUSA_XML_FINAL", "GDP_elas_USA_SSP3.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_GDP_elas_USA_SSP5.xml", "GCAMUSA_XML_FINAL", "GDP_elas_USA_SSP5.xml", "", xml_tag="outFile" )

logstop()
