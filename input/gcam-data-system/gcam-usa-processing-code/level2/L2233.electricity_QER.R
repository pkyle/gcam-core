# This script creates add-ons to harmonize reference case generation technology costs with AEO-2016. These assumptions are also 
# consistent with the Base case scenario in the DOE Report: "Energy CO2 Emissions Impacts of Clean Energy Technology Innovation and Policy": 
# For technologies in GCAM that are not listed in the AEO, we simply use the cost assumptions from the global version of
# GCAM. 
# The script also creates add-ons for Advanced Tech scenario consistent with the Advanced technology scenario in the DOE report.
# Link to the DOE report:
# https://energy.gov/sites/prod/files/2017/01/f34/Energy%20CO2%20Emissions%20Impacts%20of%20Clean%20Energy%20Technology%20Innovation%20and%20Policy.pdf

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
 logstart( "L2233.electricity_QER.R" )
 adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
 adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
 printlog( "L2233.electricity_QER.R: Electricity sector add-ons to harmonize generation tech costs with AEO-2016. Also create Adv Tech add-ons" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")

L223.GlobalTechCapFac_elec <- readdata("ENERGY_LEVEL2_DATA", "L223.GlobalTechCapFac_elec", skip=4)
L223.GlobalIntTechCapital_elec <- readdata("ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_elec", skip=4)
L223.GlobalTechCapital_elec <- readdata("ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_elec", skip=4)
L223.GlobalIntTechCapital_elec <- readdata("ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_elec", skip=4)
L223.GlobalTechCapital_elec <- readdata("ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_elec", skip=4)
L223.GlobalTechOMfixed_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMfixed_elec", skip = 4 )
L223.GlobalIntTechOMfixed_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMfixed_elec", skip = 4 )
L223.GlobalTechOMvar_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMvar_elec", skip = 4 )
L223.GlobalIntTechOMvar_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMvar_elec", skip = 4 )

A23.elec_overnight_costs_QER <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elec_overnight_costs_QER", skip=4)
A23.itc_QER <- readdata("GCAMUSA_ASSUMPTIONS", "A23.itc_QER", skip=4)
A23.ptc_QER <- readdata("GCAMUSA_ASSUMPTIONS", "A23.ptc_QER",skip=4)
A23.ptc_inttech_QER <- readdata("GCAMUSA_ASSUMPTIONS", "A23.ptc_inttech_QER", skip=4)

A23.elec_overnight_costs_QER_adv <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elec_overnight_costs_QER_adv", skip=4)
A23.elec_OM_QER_adv <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elec_OM_QER_adv", skip=4)


conv_2013_1975 <- 0.293288
conv_2014_2013 <- 0.983834

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Build GCAM capital cost table from QER data

max_year_QER <- max(A23.elec_overnight_costs_QER$year)

A23.elec_overnight_costs_QER %>%
  filter(year == 2015) %>%
  repeat_and_add_vector('year', model_base_years) -> A23.elec_overnight_costs_QER_base_years

A23.elec_overnight_costs_QER %>%
  bind_rows(A23.elec_overnight_costs_QER_base_years) %>%
  arrange(GCAM.technology, year) %>% 
  mutate(overnight.cost = overnight.cost * conv_2013_1975) -> A23.elec_overnight_costs_QER


L223.GlobalTechCapital_elec %>%
  left_join(L223.GlobalTechCapital_elec %>% 
              filter(year == max_year_QER) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.QER = capital.overnight.y) %>%
  mutate(capital.overnight.ratio = capital.overnight/capital.overnight.max.year.QER) %>%
  select(-capital.overnight.max.year.QER) %>%
  left_join(A23.elec_overnight_costs_QER, by = c("year","technology" = "GCAM.technology")) %>%
  mutate(overnight.cost = as.integer(overnight.cost)) %>%
  mutate(capital.overnight = if_else(is.na(overnight.cost) == TRUE, capital.overnight, overnight.cost)) -> L2233.GlobalTechCapital_elec

L2233.GlobalTechCapital_elec %>%
  left_join(L2233.GlobalTechCapital_elec %>% 
              filter(year == max_year_QER) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.QER = capital.overnight.y) %>%
  mutate(capital.overnight = as.double(capital.overnight)) %>%
  mutate(capital.overnight = if_else(year > max_year_QER, capital.overnight.max.year.QER * capital.overnight.ratio, capital.overnight)) %>%
  select(-capital.overnight.ratio, -overnight.cost, -capital.overnight.max.year.QER) -> L2233.GlobalTechCapital_elec
  
L223.GlobalIntTechCapital_elec %>%
  left_join(L223.GlobalIntTechCapital_elec %>% 
              filter(year == max_year_QER) %>% 
              select(intermittent.technology, capital.overnight), by = "intermittent.technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.QER = capital.overnight.y) %>%
  mutate(capital.overnight.ratio = capital.overnight/capital.overnight.max.year.QER) %>%
  select(-capital.overnight.max.year.QER) %>%
  left_join(A23.elec_overnight_costs_QER, by = c("year","intermittent.technology" = "GCAM.technology")) %>%
  mutate(overnight.cost = as.integer(overnight.cost)) %>%
  mutate(capital.overnight = if_else(is.na(overnight.cost) == TRUE, capital.overnight, overnight.cost)) -> L2233.GlobalIntTechCapital_elec

L2233.GlobalIntTechCapital_elec %>%
  left_join(L2233.GlobalIntTechCapital_elec %>% 
              filter(year == max_year_QER) %>% 
              select(intermittent.technology, capital.overnight), by = "intermittent.technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.QER = capital.overnight.y) %>%
  mutate(capital.overnight = as.double(capital.overnight)) %>%
  mutate(capital.overnight = if_else(year > max_year_QER, capital.overnight.max.year.QER * capital.overnight.ratio, capital.overnight)) %>%
  select(-capital.overnight.ratio, -overnight.cost, -capital.overnight.max.year.QER) -> L2233.GlobalIntTechCapital_elec

# 2b. ITC and PTC

# Adjust FCRs by 1-ITC

L2233.GlobalTechCapital_elec %>%
  left_join(A23.itc_QER, by = c("year", "technology" = "GCAM.technology")) %>%
  mutate(fixed.charge.rate = if_else(is.na(itc) == TRUE, fixed.charge.rate, fixed.charge.rate * (1-itc))) %>%
  select(-itc) ->  L2233.GlobalTechCapital_elec_itc
  
L2233.GlobalIntTechCapital_elec %>%
  left_join(A23.itc_QER, by = c("year", "intermittent.technology" = "GCAM.technology")) %>%
  mutate(fixed.charge.rate = if_else(is.na(itc) == TRUE, fixed.charge.rate, fixed.charge.rate * (1-itc))) %>%
  select(-itc) ->  L2233.GlobalIntTechCapital_elec_itc  

# Build table to read in PTC as cost adder (subtracter)

L2233.GlobalTechRegPriceAdj_ptc <- A23.ptc_QER

L2233.GlobalIntTechRegPriceAdj_ptc <- A23.ptc_inttech_QER


# 2c. Generate csvs for advanced costs. These costs include the ITC. 

# Technology capital costs with ITC

L2233.GlobalTechCapital_elec_itc %>%
  left_join(L2233.GlobalTechCapital_elec_itc %>% 
              filter(year == max_year_QER) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.QER = capital.overnight.y) %>%
  mutate(capital.overnight.ratio = capital.overnight/capital.overnight.max.year.QER) %>%
  select(-capital.overnight.max.year.QER) %>%
  left_join(A23.elec_overnight_costs_QER_adv, by = c("year","technology" = "GCAM.technology")) %>%
  mutate(capital.overnight = if_else(is.na(overnight.cost) == TRUE, capital.overnight, overnight.cost)) -> L2233.GlobalTechCapital_elec_itc_adv

L2233.GlobalTechCapital_elec_itc_adv %>%
  left_join(L2233.GlobalTechCapital_elec_itc_adv %>% 
              filter(year == max_year_QER) %>% 
              select(technology, capital.overnight), by = "technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.QER = capital.overnight.y) %>%
  mutate(capital.overnight = if_else(year > max_year_QER, capital.overnight.max.year.QER * capital.overnight.ratio, capital.overnight)) %>%
  select(-capital.overnight.ratio, -overnight.cost, -capital.overnight.max.year.QER) -> L2233.GlobalTechCapital_elec_itc_adv

# Intermittent-Technology capital costs with ITC
L2233.GlobalIntTechCapital_elec_itc %>%
  left_join(L2233.GlobalIntTechCapital_elec_itc %>% 
              filter(year == max_year_QER) %>% 
              select(intermittent.technology, capital.overnight), by = "intermittent.technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.QER = capital.overnight.y) %>%
  mutate(capital.overnight.ratio = capital.overnight/capital.overnight.max.year.QER) %>%
  select(-capital.overnight.max.year.QER) %>%
  left_join(A23.elec_overnight_costs_QER_adv, by = c("year","intermittent.technology" = "GCAM.technology")) %>%
  mutate(capital.overnight = if_else(is.na(overnight.cost) == TRUE, capital.overnight, overnight.cost)) -> L2233.GlobalIntTechCapital_elec_itc_adv

L2233.GlobalIntTechCapital_elec_itc_adv %>%
  left_join(L2233.GlobalIntTechCapital_elec_itc_adv %>% 
              filter(year == max_year_QER) %>% 
              select(intermittent.technology, capital.overnight), by = "intermittent.technology") %>%
  rename(capital.overnight = capital.overnight.x, capital.overnight.max.year.QER = capital.overnight.y) %>%
  mutate(capital.overnight = if_else(year > max_year_QER, capital.overnight.max.year.QER * capital.overnight.ratio, capital.overnight)) %>%
  select(-capital.overnight.ratio, -overnight.cost, -capital.overnight.max.year.QER) -> L2233.GlobalIntTechCapital_elec_itc_adv

# Technology fixed OM-costs
L223.GlobalTechOMfixed_elec %>%
  left_join(L223.GlobalTechOMfixed_elec %>% 
              filter(year == max_year_QER) %>% 
              select(technology, OM.fixed), by = "technology") %>%
  rename(OM.fixed = OM.fixed.x, OM.fixed.max.year.QER = OM.fixed.y) %>%
  mutate(OM.fixed.ratio = OM.fixed/OM.fixed.max.year.QER) %>%
  select(-OM.fixed.max.year.QER) %>%
  left_join(A23.elec_OM_QER_adv %>% select(-OM.var), by = c("year", "technology" = "GCAM.technology")) %>%
  mutate(OM.fixed.x = if_else(is.na(OM.fixed.y) == TRUE, OM.fixed.x,OM.fixed.y)) %>%
  rename(OM.fixed = OM.fixed.x) %>%
  select(-OM.fixed.y) -> L2233.GlobalTechOMfixed_elec_adv
  
L2233.GlobalTechOMfixed_elec_adv %>%
  left_join(L2233.GlobalTechOMfixed_elec_adv %>% 
              filter(year == max_year_QER) %>% 
              select(technology, OM.fixed), by = "technology") %>%
  rename(OM.fixed = OM.fixed.x, OM.fixed.max.year.QER = OM.fixed.y) %>%
  mutate(OM.fixed = if_else(year > max_year_QER, OM.fixed.max.year.QER * OM.fixed.ratio, OM.fixed)) %>%
  select(-OM.fixed.ratio, -OM.fixed.max.year.QER) -> L2233.GlobalTechOMfixed_elec_adv

# Intermittent Technology fixed OM-costs
L223.GlobalIntTechOMfixed_elec %>%
  left_join(L223.GlobalIntTechOMfixed_elec %>% 
              filter(year == max_year_QER) %>% 
              select(intermittent.technology, OM.fixed), by = "intermittent.technology") %>%
  rename(OM.fixed = OM.fixed.x, OM.fixed.max.year.QER = OM.fixed.y) %>%
  mutate(OM.fixed.ratio = OM.fixed/OM.fixed.max.year.QER) %>%
  select(-OM.fixed.max.year.QER) %>%
  left_join(A23.elec_OM_QER_adv %>% select(-OM.var), by = c("year", "intermittent.technology" = "GCAM.technology")) %>%
  mutate(OM.fixed.x = if_else(is.na(OM.fixed.y) == TRUE, OM.fixed.x,OM.fixed.y)) %>%
  rename(OM.fixed = OM.fixed.x) %>%
  select(-OM.fixed.y) -> L2233.GlobalIntTechOMfixed_elec_adv

L2233.GlobalIntTechOMfixed_elec_adv %>%
  left_join(L2233.GlobalIntTechOMfixed_elec_adv %>% 
              filter(year == max_year_QER) %>% 
              select(intermittent.technology, OM.fixed), by = "intermittent.technology") %>%
  rename(OM.fixed = OM.fixed.x, OM.fixed.max.year.QER = OM.fixed.y) %>%
  mutate(OM.fixed = if_else(year > max_year_QER, OM.fixed.max.year.QER * OM.fixed.ratio, OM.fixed)) %>%
  select(-OM.fixed.ratio, -OM.fixed.max.year.QER) -> L2233.GlobalIntTechOMfixed_elec_adv

# Technology Variable OM
L223.GlobalTechOMvar_elec %>%
  left_join(L223.GlobalTechOMvar_elec %>% 
              filter(year == max_year_QER) %>% 
              select(technology, OM.var), by = "technology") %>%
  rename(OM.var = OM.var.x, OM.var.max.year.QER = OM.var.y) %>%
  mutate(OM.var.ratio = OM.var/OM.var.max.year.QER) %>%
  select(-OM.var.max.year.QER) %>%
  left_join(A23.elec_OM_QER_adv %>% select(-OM.fixed), by = c("year", "technology" = "GCAM.technology")) %>%
  mutate(OM.var.x = if_else(is.na(OM.var.y) == TRUE, OM.var.x,OM.var.y)) %>%
  rename(OM.var = OM.var.x) %>%
  select(-OM.var.y) -> L2233.GlobalTechOMvar_elec_adv

L2233.GlobalTechOMvar_elec_adv %>%
  left_join(L2233.GlobalTechOMvar_elec_adv %>% 
              filter(year == max_year_QER) %>% 
              select(technology, OM.var), by = "technology") %>%
  rename(OM.var = OM.var.x, OM.var.max.year.QER = OM.var.y) %>%
  mutate(OM.var = if_else(year > max_year_QER, OM.var.max.year.QER * OM.var.ratio, OM.var)) %>%
  select(-OM.var.ratio, -OM.var.max.year.QER) %>%
  mutate(OM.var = if_else(is.nan(OM.var) == TRUE,0, OM.var)) -> L2233.GlobalTechOMvar_elec_adv

# Intermittent Technology Variable OM
L223.GlobalIntTechOMvar_elec %>%
  left_join(L223.GlobalIntTechOMvar_elec %>% 
              filter(year == max_year_QER) %>% 
              select(intermittent.technology, OM.var), by = "intermittent.technology") %>%
  rename(OM.var = OM.var.x, OM.var.max.year.QER = OM.var.y) %>%
  mutate(OM.var.ratio = OM.var/OM.var.max.year.QER) %>%
  select(-OM.var.max.year.QER) %>%
  left_join(A23.elec_OM_QER_adv %>% select(-OM.fixed), by = c("year", "intermittent.technology" = "GCAM.technology")) %>%
  mutate(OM.var.x = if_else(is.na(OM.var.y) == TRUE, OM.var.x,OM.var.y)) %>%
  rename(OM.var = OM.var.x) %>%
  select(-OM.var.y) -> L2233.GlobalIntTechOMvar_elec_adv

L2233.GlobalIntTechOMvar_elec_adv %>%
  left_join(L2233.GlobalIntTechOMvar_elec_adv %>% 
              filter(year == max_year_QER) %>% 
              select(intermittent.technology, OM.var), by = "intermittent.technology") %>%
  rename(OM.var = OM.var.x, OM.var.max.year.QER = OM.var.y) %>%
  mutate(OM.var = if_else(year > max_year_QER, OM.var.max.year.QER * OM.var.ratio, OM.var)) %>%
  select(-OM.var.ratio, -OM.var.max.year.QER) %>%
  mutate(OM.var = if_else(is.nan(OM.var) == TRUE,0, OM.var)) -> L2233.GlobalIntTechOMvar_elec_adv


# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

# Capital Costs
write_mi_data(L2233.GlobalTechCapital_elec, "GlobalTechCapital", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechCapital_elec", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA.xml")
write_mi_data(L2233.GlobalIntTechCapital_elec, "GlobalIntTechCapital", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechCapital_elec", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_costs_USA.xml", "GCAMUSA_XML_FINAL", "elec_costs_USA.xml", "", xml_tag="outFile" )

# Capital costs with ITC 
write_mi_data(L2233.GlobalTechCapital_elec_itc, "GlobalTechCapital", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechCapital_elec_itc", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_itc.xml")
write_mi_data(L2233.GlobalIntTechCapital_elec_itc, "GlobalIntTechCapital", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechCapital_elec_itc", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_itc.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_itc.xml", "GCAMUSA_XML_FINAL", "elec_costs_USA_itc.xml", "", xml_tag="outFile" )

# Adv Tech costs
write_mi_data(L2233.GlobalTechCapital_elec_itc_adv, "GlobalTechCapital", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechCapital_elec_itc_adv", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_itc_adv.xml")
write_mi_data(L2233.GlobalIntTechCapital_elec_itc_adv, "GlobalIntTechCapital", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechCapital_elec_itc_adv", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_itc_adv.xml")
write_mi_data(L2233.GlobalTechOMfixed_elec_adv, "GlobalTechOMfixed", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechOMfixed_elec_adv", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_itc_adv.xml")
write_mi_data(L2233.GlobalIntTechOMfixed_elec_adv, "GlobalIntTechOMfixed", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechOMfixed_elec_adv", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_itc_adv.xml")
write_mi_data(L2233.GlobalTechOMvar_elec_adv, "GlobalTechOMvar", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechOMvar_elec_adv", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_itc_adv.xml")
write_mi_data(L2233.GlobalIntTechOMvar_elec_adv, "GlobalIntTechOMvar", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechOMvar_elec_adv", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_itc_adv.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_itc_adv.xml", "GCAMUSA_XML_FINAL", "elec_costs_USA_itc_adv.xml", "", xml_tag="outFile" )

# PTC
write_mi_data(L2233.GlobalTechRegPriceAdj_ptc, "GlobalTechRegPriceAdj", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechRegPriceAdj_ptc", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_ptc.xml")
write_mi_data(L2233.GlobalIntTechRegPriceAdj_ptc, "GlobalIntTechRegPriceAdj", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechRegPriceAdj_ptc", "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_ptc.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_costs_USA_ptc.xml", "GCAMUSA_XML_FINAL", "elec_costs_USA_ptc.xml", "", xml_tag="outFile" )

logstop()
