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
logstart( "L2236.nonewcoal_elecS_USA.R" )
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
A23.elecS_tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_associations" )

L223.StubTechEff_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechEff_elec_USA", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs

gcamusa_regions <- unique(L223.StubTechEff_elec_USA$region)

A23.elecS_tech_associations %>% 
  filter(subsector == "coal") %>%
  select(Electric.sector, subsector, Electric.sector.technology) %>%
  rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
  filter(!grepl("CCS",stub.technology)) %>%
  rbind(c("industrial energy use", "coal", "coal cogen")) %>%
  repeat_and_add_vector('region', gcamusa_regions) %>%
  select(region, supplysector, subsector, stub.technology)%>%
  mutate(initial.available.year = min(model_base_years), final.available.year = max( model_base_years )) ->
  L2236.StubTechAvail_NoNewCoal_elecS_USA

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2236.StubTechAvail_NoNewCoal_elecS_USA, "StubTechAvail", "GCAMUSA_LEVEL2_DATA", "L2236.StubTechAvail_NoNewCoal_elecS_USA", "GCAMUSA_XML_BATCH", "batch_nonewcoal_elecS_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_nonewcoal_elecS_USA.xml", "GCAMUSA_XML_FINAL", "nonewcoal_elecS_USA.xml", "", xml_tag="outFile" )

logstop()