# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
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
logstart( "L228.regional_biomass_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Primary energy prices in the states, and electricity T&D sectors" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions")

A21.sector <- readdata( "ENERGY_ASSUMPTIONS", "A21.sector" )
A26.sector <- readdata( "ENERGY_ASSUMPTIONS", "A26.sector" )

L202.CarbonCoef <- readdata( "ENERGY_LEVEL2_DATA", "L202.CarbonCoef", skip = 4 )

L221.GlobalTechCoef_en <- readdata( "ENERGY_LEVEL2_DATA", "L221.GlobalTechCoef_en", skip = 4 )
L221.SubsectorInterp_en <- readdata( "ENERGY_LEVEL2_DATA", "L221.SubsectorInterp_en", skip = 4 )
L221.StubTech_en <- readdata( "ENERGY_LEVEL2_DATA", "L221.StubTech_en", skip = 4 )
L221.StubTechShrwt_bio <- readdata( "ENERGY_LEVEL2_DATA", "L221.StubTechShrwt_bio", skip = 4 )
L221.StubTechFractProd_en <- readdata( "ENERGY_LEVEL2_DATA", "L221.StubTechFractProd_en", skip = 4 )
L221.StubTechFractSecOut_en <- readdata( "ENERGY_LEVEL2_DATA", "L221.StubTechFractSecOut_en", skip = 4 )

L221.DepRsrc_en <- readdata( "ENERGY_LEVEL2_DATA", "L221.DepRsrc_en", skip = 4 )
L221.DepRsrcPrice_en <- readdata( "ENERGY_LEVEL2_DATA", "L221.DepRsrcPrice_en", skip = 4 )

L226.SubsectorInterp_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.SubsectorInterp_en", skip = 4 )
L226.GlobalTechEff_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.GlobalTechEff_en", skip = 4 )
L226.GlobalTechCost_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.GlobalTechCost_en", skip = 4 )

A28.sector <- readdata( "GCAMUSA_ASSUMPTIONS", "A28.sector", skip = 1 )

# -----------------------------------------------------------------------------
# 2.    Build tables for CSVs
# 2.a.  Supply sectors and subsectors

printlog( "L228.DeleteSupplysector_dbm_USA: Deleting USA-level biomass sectors" )
# Not deleting USA regional biomass because gas processing & H2 central production
# still both occur at the USA level and consume USA regional biomass

A28.sector %>%
  filter(supplysector != "regional biomass") %>%
  mutate(region = "USA") %>%
  select(region, supplysector) -> L228.DeleteSupplysector_bio_USA


printlog( "L228.Supplysector_bio_USA: Supply sector information for state-level biomass sectors" )

L228.USA_biomass_sectors <- unique(A28.sector$supplysector)

A21.sector %>%
  bind_rows(A26.sector) %>%
  filter(supplysector %in% L228.USA_biomass_sectors) %>%
  repeat_and_add_vector('region', states) %>%
  mutate(logit.year.fillout = min( model_base_years )) %>%
  select(-traded) -> L228.Supplysector_bio_USA

L228.SectorLogitTables_bio_USA <- get_logit_fn_tables( L228.Supplysector_bio_USA, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=T, write.all.regions=F )
L228.Supplysector_bio_USA <- L228.Supplysector_bio_USA[ names_Supplysector ]


printlog( "L228.SubsectorShrwtFllt_bio_USA: subsector shareweights of state-level biomass sectors" )

L228.Supplysector_bio_USA %>%
  mutate(subsector = supplysector, 
         year.fillout = min( model_base_years ),
         share.weight = 1) %>%
  select( names_SubsectorShrwtFllt ) -> L228.SubsectorShrwtFllt_bio_USA


printlog( "L228.SubsectorInterp_bio_USA: subsector shareweight interpolations for state-level biomass sectors" )

L221.SubsectorInterp_en %>%
  bind_rows(L226.SubsectorInterp_en) %>%
  filter(region == "USA") %>%
  semi_join(A28.sector, by = c("supplysector")) %>%
  select(-region) %>%
  repeat_and_add_vector('region', states) %>%
  select(names_SubsectorInterp) -> L228.SubsectorInterp_bio_USA
  

# NOTE: There is only one tech per subsector so the logit choice does not matter

L228.SubsectorShrwtFllt_bio_USA %>%
  select( names_Subsector ) %>%
  mutate(logit.year.fillout = min( model_base_years ), 
         logit.exponent = -3,
         logit.type = NA) -> L228.SubsectorLogit_bio_USA

L228.SubsectorLogitTables_bio_USA <- get_logit_fn_tables( L228.SubsectorLogit_bio_USA, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
L228.SubsectorLogit_bio_USA <- L228.SubsectorLogit_bio_USA[, names_SubsectorLogit ]


# -----------------------------------------------------------------------------
# 2.b.  Stub-technologies for biomass sectors that consume from global markets

printlog( "L228.StubTech_bio_USA: stub-technologies for state-level biomass sector" )

L221.StubTech_en %>%
  filter(region == "USA") %>%
  semi_join(A28.sector, by = c("supplysector")) %>%
  select(-region) %>%
  repeat_and_add_vector('region', states) %>%
  select(names_StubTech) -> L228.StubTech_bio_USA


printlog( "L228.StubTechMarket_bio_USA: technology inputs, markets, & coefficients of state-level biomass sectors" )

L221.GlobalTechCoef_en %>%  
  rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
  semi_join(L228.StubTech_bio_USA, by = c("supplysector", "subsector", "stub.technology")) %>%
  repeat_and_add_vector('region', states) %>%
  mutate(market.name = "USA") %>%
  select(names_StubTechMarket) -> L228.StubTechMarket_bio_USA


printlog( "L228.StubTechShrwt_rbO_USA: technology share-weights of state-level regional biomassOil sectors" )

L221.StubTechShrwt_bio %>%
  filter(region == "USA") %>%
  select(-region) %>%
  repeat_and_add_vector('region', states) %>%
  select(names_StubTechYr, share.weight) -> L228.StubTechShrwt_rbO_USA
  

printlog( "L228.StubTechFractSecOut_bio_USA: Secondary (feed) outputs of global technologies for upstream (bio)energy" )
printlog( "NOTE: secondary outputs are only considered in future time periods" )
printlog( "NOTE: secondary outputs are only written for the regions/technologies where applicable, so the global tech database can not be used" )

L221.StubTechFractSecOut_en %>%
  filter(region == "USA") %>%
  semi_join(A28.sector, by = c("supplysector")) %>%
  select(-region) %>%
  repeat_and_add_vector('region', states) %>%
  select(names_StubTechFractSecOut) -> L228.StubTechFractSecOut_bio_USA


printlog( "L228.StubTechFractProd_bio_USA: cost curve points for producing secondary output feedcrops" )

L221.StubTechFractProd_en %>%
  filter(region == "USA") %>%
  semi_join(A28.sector, by = c("supplysector")) %>%
  select(-region) %>%
  repeat_and_add_vector('region', states) %>%
  select(names_StubTechYr, "fractional.secondary.output", price, "fraction.produced") -> L228.StubTechFractProd_bio_USA


# -----------------------------------------------------------------------------
# 2.c.  Connecting state-level DDGS & feedcakes secondary outputs to USA sector

printlog( "L228.DepRsrc_DDGS_USA: depletable resource info for state-level DDGS & feedcake secondary outputs" )
L221.DepRsrc_en %>%
  filter(region == "USA") %>%
  repeat_and_add_vector('region', states) %>%
  select(names_DepRsrc) -> L228.DepRsrc_DDGS_USA


printlog( "L228.DepRsrcPrice_DDGS_USA: depletable resource prices for state-level DDGS & feedcake secondary outputs" )  
L221.DepRsrcPrice_en %>%
  filter(region == "USA") %>%
  repeat_and_add_vector('region', states) %>%
  select(names_DepRsrcPrice) -> L228.DepRsrcPrice_DDGS_USA


# -----------------------------------------------------------------------------
# 2.d.  Technologies for delivered biomass sectors, which consume from state-level markets

printlog( "NOTE: can't use stub technology for delivered biomass sectors because they would inherit the wrong energy-inputs" )
printlog( "L228.Tech_dbm_USA: technologies for state-level delivered biomass sector" )

L226.GlobalTechEff_en %>%
  semi_join(A28.sector, by = c("sector.name" = "supplysector")) %>%
  rename(supplysector = sector.name, subsector = subsector.name) %>%
  repeat_and_add_vector('region', states) %>%
  select(names_Tech) %>%
  distinct() -> L228.Tech_dbm_USA


printlog( "L228.TechShrwt_dbm_USA: technology shareweights of state-level delivered biomass sectors" )

L228.Tech_dbm_USA %>%
  repeat_and_add_vector('year', model_years) %>%
  mutate(share.weight = 1) %>%
  select(names_TechYr, "share.weight") -> L228.TechShrwt_dbm_USA


printlog( "L228.TechEff_dbm_USA: technology efficiencies of state-level delivered biomass sectors" )

L226.GlobalTechEff_en %>%
  semi_join(A28.sector, by = c("sector.name" = "supplysector")) %>%
  rename(supplysector = sector.name, subsector = subsector.name) %>%
  repeat_and_add_vector('region', states) %>%
  mutate(market.name = region) %>%
  select(names_TechEff) -> L228.TechEff_dbm_USA


printlog( "L228.TechCoef_dbm_USA: technology coefficients and market names of state-level delivered biomass sectors" )

L228.TechShrwt_dbm_USA %>%
  select( names_TechYr ) %>%
  mutate(minicam.energy.input = "regional biomass",
         coefficient = 1,
         market.name = region) -> L228.TechCoef_dbm_USA


printlog( "L228.TechCost_dbm_USA: technology coefficients and market names of state-level delivered biomass sectors" )

L226.GlobalTechCost_en %>%
  semi_join(A28.sector, by = c("sector.name" = "supplysector")) %>%
  rename(supplysector = sector.name, subsector = subsector.name)  %>%
  repeat_and_add_vector('region', states) %>%
  select( names_TechCost ) -> L228.TechCost_dbm_USA


# -----------------------------------------------------------------------------
# 2.e.  Carbon coefficients for state-level biomass sectors

printlog( "L228.CarbonCoef_bio_USA: carbon coef for state-level biomass sectors" )

L202.CarbonCoef %>%
  filter(region == "USA", PrimaryFuelCO2Coef.name %in% c(L228.USA_biomass_sectors, "biomass")) %>%
  select(-region) %>%
  repeat_and_add_vector('region', states) %>%
  select(region, PrimaryFuelCO2Coef.name, PrimaryFuelCO2Coef) -> L228.CarbonCoef_bio_USA


# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
# PART 1: REGIONAL BIOMASS, CORN / SUGAR FOR ETHANOL, BIOMASSOIL

write_mi_data( L228.DeleteSupplysector_bio_USA, "DeleteSupplysector", "GCAMUSA_LEVEL2_DATA", "L228.DeleteSupplysector_bio_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" ) 
for( curr_table in names ( L228.SectorLogitTables_bio_USA ) ) {
  write_mi_data( L228.SectorLogitTables_bio_USA[[ curr_table ]]$data, L228.SectorLogitTables_bio_USA[[ curr_table ]]$header,
                 "GCAMUSA_LEVEL2_DATA", paste0("L228.", L228.SectorLogitTables_bio_USA[[ curr_table ]]$header, "_bio_USA" ), "GCAMUSA_XML_BATCH",
                 "batch_regional_biomass_USA.xml" )
  }
write_mi_data( L228.Supplysector_bio_USA, IDstring="Supplysector", domain="GCAMUSA_LEVEL2_DATA", fn="L228.Supplysector_bio_USA", 
               batch_XML_domain="GCAMUSA_XML_BATCH", batch_XML_file="batch_regional_biomass_USA.xml" ) 
write_mi_data( L228.SubsectorShrwtFllt_bio_USA, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L228.SubsectorShrwtFllt_bio_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" ) 
write_mi_data( L228.SubsectorInterp_bio_USA, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L228.SubsectorInterp_bio_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )
for( curr_table in names ( L228.SubsectorLogitTables_bio_USA ) ) {
  write_mi_data( L228.SubsectorLogitTables_bio_USA[[ curr_table ]]$data, L228.SubsectorLogitTables_bio_USA[[ curr_table ]]$header,
                 "GCAMUSA_LEVEL2_DATA", paste0("L228.", L228.SubsectorLogitTables_bio_USA[[ curr_table ]]$header, "_bio_USA" ), "GCAMUSA_XML_BATCH",
                 "batch_regional_biomass_USA.xml" )
  }
write_mi_data( L228.SubsectorLogit_bio_USA, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L228.SubsectorLogit_bio_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )

write_mi_data( L228.StubTech_bio_USA, "StubTech", "GCAMUSA_LEVEL2_DATA", "L228.StubTech_bio_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )
write_mi_data( L228.StubTechMarket_bio_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L228.StubTechMarket_bio_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )
write_mi_data( L228.StubTechShrwt_rbO_USA, "StubTechShrwt", "GCAMUSA_LEVEL2_DATA", "L228.StubTechShrwt_rbO_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )
write_mi_data( L228.StubTechFractSecOut_bio_USA, "StubTechFractSecOut", "GCAMUSA_LEVEL2_DATA", "L228.StubTechFractSecOut_bio_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )
write_mi_data( L228.StubTechFractProd_bio_USA, "StubTechFractProd", "GCAMUSA_LEVEL2_DATA", "L228.StubTechFractProd_bio_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )

write_mi_data( L228.DepRsrc_DDGS_USA, "DepRsrc", "GCAMUSA_LEVEL2_DATA", "L228.DepRsrc_DDGS_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )
write_mi_data( L228.DepRsrcPrice_DDGS_USA, "DepRsrcPrice", "GCAMUSA_LEVEL2_DATA", "L228.DepRsrcPrice_DDGS_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )

write_mi_data( L228.Tech_dbm_USA, "Tech", "GCAMUSA_LEVEL2_DATA", "L228.Tech_dbm_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )
write_mi_data( L228.TechShrwt_dbm_USA, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L228.TechShrwt_dbm_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )
write_mi_data( L228.TechEff_dbm_USA, "TechEff", "GCAMUSA_LEVEL2_DATA", "L228.TechEff_dbm_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" )
write_mi_data( L228.TechCoef_dbm_USA, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L228.TechCoef_dbm_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" ) 
write_mi_data( L228.TechCost_dbm_USA, "TechCost", "GCAMUSA_LEVEL2_DATA", "L228.TechCost_dbm_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" ) 

write_mi_data( L228.CarbonCoef_bio_USA, "CarbonCoef", "GCAMUSA_LEVEL2_DATA", "L228.CarbonCoef_bio_USA", "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_regional_biomass_USA.xml", "GCAMUSA_XML_FINAL", "regional_biomass_USA.xml", "", xml_tag="outFile" )

logstop()
