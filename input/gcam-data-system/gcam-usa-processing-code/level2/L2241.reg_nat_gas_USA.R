# This script creates "natural gas production" sector for the USA region.
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
logstart( "L2241.reg_gas_input.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for USA regional natural gas to demand USA natural gas prouction" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

A23.gas_sector_vertical <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.gas_sector_vertical" )
A23.gas_subsector_shrwt <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.gas_subsector_shrwt" )
A23.gas_subsector_shrwt_interp <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.gas_subsector_shrwt_interp" )
A23.gas_subsector_shrwt_interpto <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.gas_subsector_shrwt_interpto" )
A23.gas_tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.gas_tech_associations" )
L111.gas_prod_state_T_Yh_EJ <- readdata( "GCAMUSA_LEVEL1_DATA", "L111.gas_prod_state_T_Yh_EJ" )
L222.StubTechProd_gasproc <- readdata( "ENERGY_LEVEL2_DATA", "L222.StubTechProd_gasproc", skip = 4 )
L221.GlobalTechCoef_en <- readdata( "ENERGY_LEVEL2_DATA", "L221.GlobalTechCoef_en", skip = 4 )
L221.PrimaryConsKeyword_en <- readdata( "ENERGY_LEVEL2_DATA", "L221.PrimaryConsKeyword_en", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations

# Delete USA natural gas subresource
printlog( "L2241.Delete_NG_USA: Delete USA natural gas subresource" )
L2241.DeleteSubRsrc_NG_USA <- data.frame(region = "USA", depresource = "natural gas", subresource = "natural gas")

# Delete "regional natural gas" subsector
printlog( "L2241.Delete_regNG_subsector: delete USA regional natural gas subsector" )
A23.gas_sector_vertical %>% 
  filter(supplysector == "regional natural gas") %>% 
  mutate(region = "USA") %>% 
  mutate(subsector = "regional natural gas") %>%
  select(region, supplysector, subsector) -> L2241.Delete_regNG_subsector

# Modify regional natural gas sector to aggregate domestic and imported resources
# Logit for USA regional natural gas
printlog( "L2241.Supplysector_regNG_USA: regional natural gas logit competition" )
A23.gas_sector_vertical %>% 
  filter(supplysector == "regional natural gas") %>%
  mutate(region = "USA") %>% 
  mutate(logit.year.fillout = model_base_years[1]) %>%
  mutate(logit.type=NA) %>%
  select(region, supplysector, output.unit, input.unit, price.unit, 
         logit.year.fillout, logit.exponent, logit.type) -> L2241.Supplysector_regNG_USA

L2241.SectorLogitTables <- get_logit_fn_tables( L2241.Supplysector_regNG_USA, names_SupplysectorLogitType,
                                               base.header="Supplysector_", include.equiv.table=T, write.all.regions=F )
L2241.Supplysector_regNG_USA <- L2241.Supplysector_regNG_USA[, names_Supplysector ]

# Logits for USA domestic natural gas and imported natural gas
# Note these logit assumptions do not matter as there is no competition at this nest
printlog( "L2241.Supplysector_regNG_USA: regional natural gas subsector to aggregate gas types" )
A23.gas_tech_associations %>% 
  mutate(region = "USA") %>% 
  left_join(A23.gas_sector_vertical, by = c("supplysector")) %>%
  mutate(logit.year.fillout = model_base_years[1]) %>%
  mutate(logit.type=NA) %>%
  select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type) -> L2241.Subsector_regNG_USA

L2241.SubsectorLogitTables <- get_logit_fn_tables( L2241.Subsector_regNG_USA, names_SubsectorLogitType,
                                                  base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
L2241.Subsector_regNG_USA <- L2241.Subsector_regNG_USA[, names_SubsectorLogit ]

# Shareweights for USA domestic natural gas and imported natural gas
L2241.SubsectorShrwt_regNG_USA <- set_years(A23.gas_subsector_shrwt)
L2241.SubsectorShrwtInterp_regNG_USA <- set_years(A23.gas_subsector_shrwt_interp)
L2241.SubsectorShrwtInterpTo_regNG_USA <- set_years(A23.gas_subsector_shrwt_interpto)

# Provide calOutputValue for "regional natural gas" market
printlog( "L2241.StubTechProd_regNG_USA: regional natural gas calibration values" )
L111.gas_prod_state_T_Yh_EJ %>% 
  select(state, X_model_base_years) %>%
  gather(year, value, -state) %>%
  mutate(region = "USA") %>%
  group_by(region, year) %>% 
  summarise(calOutputValue = sum(value)) %>%
  mutate(year = as.numeric(gsub("X", "", year))) %>%
  left_join(A23.gas_tech_associations %>% filter(subsector == "domestic natural gas") %>% 
              mutate(region = "USA"), by = c("region")) %>%
    select(region, supplysector, subsector, stub.technology, year, calOutputValue) -> L2241.CalOutput_domNG

L222.StubTechProd_gasproc %>% 
  filter(region == "USA", subsector == "natural gas") %>%
  rename(share.weight.year = year.share.weight) %>% 
  left_join(L2241.CalOutput_domNG %>% rename(domNG = calOutputValue) %>% 
                                     select(region, year, domNG), 
                                   by = c("region", "year")) %>%
  mutate(calOutputValue = calOutputValue - domNG) %>%
  select(-domNG) -> L2241.Demand_impNG

A23.gas_tech_associations %>% 
  select(supplysector, subsector, stub.technology) %>%
  filter(subsector == "imported natural gas") %>%
  repeat_and_add_vector('year', model_base_years) %>% 
  left_join(L2241.Demand_impNG %>% select(region, year, calOutputValue), by = c("year")) -> L2241.CalOutput_impNG

L2241.CalOutput_domNG %>% 
  bind_rows(L2241.CalOutput_impNG) %>%
  mutate(share.weight.year = year) %>%
  mutate(subs.share.weight = 1) %>%
  mutate(share.weight = 1) %>%
  mutate(calOutputValue = round(calOutputValue, 7)) -> L2241.StubTechProd_regNG_USA

# Global technology databases for USA domestic natural gas and imported natural gas
printlog( "L2241.GlobalTechCoef_regNG_USA: Inputs & efficiencies for USA regional natural gas technologies" )
A23.gas_tech_associations %>% 
  repeat_and_add_vector('year', model_years) %>% 
  rename(sector = supplysector) %>%
  rename(technology = stub.technology) %>%
  select(sector, subsector, technology, year, minicam.energy.input, coefficient) -> L2241.GlobalTechCoef_regNG_USA

A23.gas_tech_associations %>% 
  repeat_and_add_vector('year', model_years) %>% 
  select(sector.name = supplysector, subsector.name = subsector, technology = stub.technology, year) %>%
  left_join(L221.PrimaryConsKeyword_en %>%
              select(sector.name, year, primary.consumption),
            by = c("sector.name", "year")) -> L2241.PrimaryConsKeyword_regNG_USA

printlog( "L2241.GlobalTechShrwt_regNG_USA: Share-weights for USA regional natural gas technologies" )
A23.gas_tech_associations %>% 
  repeat_and_add_vector('year', model_years) %>% 
  rename(sector = supplysector) %>%
  rename(technology = stub.technology) %>%
  mutate(share.weight = 1) %>%
  select(sector, subsector, technology, year, share.weight) -> L2241.GlobalTechShrwt_regNG_USA

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2241.DeleteSubRsrc_NG_USA, "DeleteSubRsrc", "GCAMUSA_LEVEL2_DATA", "L2241.DeleteSubRsrc_NG_USA", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml" )
write_mi_data( L2241.Delete_regNG_subsector, "DeleteSubsector", "GCAMUSA_LEVEL2_DATA", "L2241.Delete_regNG_subsector", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml")

for( logit.table in names( L2241.SectorLogitTables ) ) {
  write_mi_data( L2241.SectorLogitTables[[ logit.table ]]$data, L2241.SectorLogitTables[[ logit.table ]]$header,
                 "GCAMUSA_LEVEL2_DATA", paste0("L2241.", L2241.SectorLogitTables[[ logit.table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
                 "batch_reg_nat_gas_USA.xml" )
}
write_mi_data( L2241.Supplysector_regNG_USA, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L2241.Supplysector_regNG_USA", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml" )

for( logit.table in names( L2241.SubsectorLogitTables ) ) {
  write_mi_data( L2241.SubsectorLogitTables[[ logit.table ]]$data, L2241.SubsectorLogitTables[[ logit.table ]]$header,
                 "GCAMUSA_LEVEL2_DATA", paste0("L2241.", L2241.SubsectorLogitTables[[ logit.table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
                 "batch_reg_nat_gas_USA.xml" )
}
write_mi_data( L2241.Subsector_regNG_USA, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L2241.Subsector_regNG_USA", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml" )
write_mi_data( L2241.PrimaryConsKeyword_regNG_USA, "PrimaryConsKeyword", "GCAMUSA_LEVEL2_DATA", "L2241.PrimaryConsKeyword_regNG_USA", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml" )

write_mi_data( L2241.StubTechProd_regNG_USA, "StubTechProd, ", "GCAMUSA_LEVEL2_DATA", "L2241.StubTechProd_regNG_USA", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml")
write_mi_data( L2241.GlobalTechCoef_regNG_USA, "GlobalTechCoef", "GCAMUSA_LEVEL2_DATA", "L2241.GlobalTechCoef_regNG_USA", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml" )
write_mi_data( L2241.GlobalTechShrwt_regNG_USA, "GlobalTechShrwt", "GCAMUSA_LEVEL2_DATA", "L2241.GlobalTechShrwt_regNG_USA", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml" )
write_mi_data( L2241.SubsectorShrwt_regNG_USA, "SubsectorShrwt", "GCAMUSA_LEVEL2_DATA", "L2241.SubsectorShrwt_regNG_USA", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml" )
write_mi_data( L2241.SubsectorShrwtInterp_regNG_USA, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L2241.SubsectorShrwtInterp_regNG_USA", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml" )
write_mi_data( L2241.SubsectorShrwtInterpTo_regNG_USA, "SubsectorInterpTo", "GCAMUSA_LEVEL2_DATA", "L2241.SubsectorShrwtInterpTo_regNG_USA", "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_reg_nat_gas_USA.xml", "GCAMUSA_XML_FINAL", "reg_nat_gas_USA.xml", "", xml_tag="outFile" )

logstop()
