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
logstart( "L2231.nonewcoal_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Optional moratorium on new pulverized coal plants in USA states" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
L223.StubTechEff_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechEff_elec_USA", skip = 4, must.exist = F )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs

L223.StubTechEff_elec_USA %>% 
  filter(subsector == "coal") %>%
  filter(!duplicated(region)) %>%
  select(region, supplysector, subsector, stub.technology) %>%
  mutate(initial.available.year = min(model_base_years), final.available.year = max( model_base_years )) ->
  L2231.StubTechAvail_elec_USA

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2231.StubTechAvail_elec_USA, "StubTechAvail", "GCAMUSA_LEVEL2_DATA", "L2231.StubTechAvail_elec_USA", "GCAMUSA_XML_BATCH", "batch_nonewcoal_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_nonewcoal_USA.xml", "GCAMUSA_XML_FINAL", "nonewcoal_USA.xml", "", xml_tag="outFile" )

logstop()