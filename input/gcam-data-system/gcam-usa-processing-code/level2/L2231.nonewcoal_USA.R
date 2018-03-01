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

A23.elec_tech_associations_coal_retire <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elec_tech_associations_coal_retire" )

if(use_mult_load_segments == "TRUE") {
  A23.elecS_tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_associations" )
  A23.elecS_tech_availability <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_availability" )
} else{
  L223.StubTechMarket_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechMarket_elec_USA", skip = 4 )
}

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs

if(use_mult_load_segments == "TRUE") {
  
  L2234.load_segments <- unique(A23.elecS_tech_associations$Electric.sector)
  
  A23.elecS_tech_associations %>% 
    anti_join(A23.elecS_tech_availability, 
              by = c("Electric.sector.technology" = "stub.technology")) %>%
    mutate(Electric.sector = factor(Electric.sector, levels = L2234.load_segments)) %>%
    arrange(subsector, Electric.sector) %>%
    mutate(Electric.sector = as.character(Electric.sector)) -> A23.elecS_tech_associations

  A23.elecS_tech_associations %>% 
    select(Electric.sector, subsector, Electric.sector.technology) %>%
    rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
    filter(subsector == "coal", !grepl("CCS", stub.technology)) %>%
    rbind(c("industrial energy use", "coal", "coal cogen")) %>%
    repeat_and_add_vector('region', states) %>%
    repeat_and_add_vector('year', future_years)  %>%
    mutate(share.weight = 0) %>% 
    select(names_StubTechYr, share.weight) -> L2231.StubTechShrwt_elec_USA
  
  A23.elec_tech_associations_coal_retire %>%
    select(Electric.sector, subsector, Electric.sector.technology) %>%
    rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
    filter(grepl("generation", supplysector)) %>%
    repeat_and_add_vector('region', states) %>%
    repeat_and_add_vector('year', future_years)  %>%
    mutate(share.weight = 0) %>% 
    select(names_StubTechYr, share.weight) -> L2231.StubTechShrwt_coal_retire_elec_USA
  
} else{
  
  L223.StubTechMarket_elec_USA %>% 
    filter(subsector == "coal", !grepl("CCS",stub.technology)) %>%
    distinct(supplysector, subsector, stub.technology) %>%
    rbind(c("industrial energy use", "coal", "coal cogen")) %>%
    repeat_and_add_vector('region', states) %>%
    repeat_and_add_vector('year', future_years)  %>%
    mutate(share.weight = 0) %>% 
    select(names_StubTechYr, share.weight) -> L2231.StubTechShrwt_elec_USA
  
  A23.elec_tech_associations_coal_retire %>%
    select(Electric.sector, subsector, Electric.sector.technology) %>%
    rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
    filter(supplysector == "electricity") %>%
    repeat_and_add_vector('region', states) %>%
    repeat_and_add_vector('year', future_years)  %>%
    mutate(share.weight = 0) %>% 
    select(names_StubTechYr, share.weight) -> L2231.StubTechShrwt_coal_retire_elec_USA
  
}

L2231.StubTechShrwt_elec_USA %>%
  bind_rows(L2231.StubTechShrwt_coal_retire_elec_USA) -> L2231.StubTechShrwt_elec_USA

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2231.StubTechShrwt_elec_USA, "StubTechShrwt", "GCAMUSA_LEVEL2_DATA", "L2231.StubTechShrwt_elec_USA", "GCAMUSA_XML_BATCH", "batch_nonewcoal_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_nonewcoal_USA.xml", "GCAMUSA_XML_FINAL", "nonewcoal_USA.xml", "", xml_tag="outFile" )

logstop()