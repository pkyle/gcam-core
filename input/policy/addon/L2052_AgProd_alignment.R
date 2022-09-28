library(gcamdata)
source("./R/constants.R")
source("./R/module-helpers.R")
source("./R/pipeline-helpers.R")
source("./R/utils-data.R")
library(dplyr)
library(tidyr)
library(tibble)
library(assertthat)
library(magrittr)
library(readr)

# Add-on R to generate alternative agriculture input files to model yield intensification
# Yang Ou 2022 Sep
# requirement: do a full driver or place the required level 2 data in the gcamdata/output folder


# yield
# This part contains updats AgProdChange for _lo technologies, so that their yield will grow to the same level as the
# corresponding _hi options by 2050, then keep the same as _high options after 2050
# ------------------------------------------------------------------------------------------------------------------------------

# read-in base yield
# This is GCAM output from query "ag tech yield"
Ag_yield_output <- read_csv("./Ag_yield_output.csv", comment = "#")

# gcamdata level 2 files
L2052.AgProdChange_ag_irr_ref <- read_csv("../../gcamdata/outputs/L2052.AgProdChange_ag_irr_ref.csv", comment = "#")
L2052.AgProdChange_bio_irr_ref <- read_csv("../../gcamdata/outputs/L2052.AgProdChange_bio_irr_ref.csv", comment = "#")

# obtain yield 2050 for hi_tech
Ag_yield_output_2050_hi <- Ag_yield_output %>%
  select(-scenario) %>%
  select(region, technology, X2050 = `2050`) %>%
  mutate(tech_base = gsub("_hi|_lo", "", technology)) %>%
  filter(grepl("_hi", technology)) %>%
  select(region, tech_base, X2050_hi = X2050)

# obtain yield 2015 for lo_tech
Ag_yield_output_2015_lo <- Ag_yield_output %>%
  select(-scenario) %>%
  select(region, technology, X2015 = `2015`) %>%
  mutate(tech_base = gsub("_hi|_lo", "", technology)) %>%
  filter(grepl("_lo", technology)) %>%
  select(region, tech_base, X2015_lo = X2015)


# backward calculate AgProd rate that makes lo_tech achieves the same yield by 2050 as the hi_techs
Ag_yield_output_2015_lo %>%
  left_join_error_no_match(Ag_yield_output_2050_hi, by = c("region", "tech_base")) %>%
  mutate(AgProdChange_adj = (X2050_hi / X2015_lo) ^ (1/35) - 1) %>%
  mutate(AgProdChange_adj = ifelse(is.na(AgProdChange_adj) | is.infinite(AgProdChange_adj), 0, AgProdChange_adj)) %>%
  mutate(AgProductionTechnology = paste0(tech_base, "_lo")) %>%
  select(region, AgProductionTechnology, AgProdChange_adj) %>%
  repeat_add_columns(tibble::tibble(year = seq(2020, 2050, 5))) -> L2052.AgProdChange_ag_irr_ref_lowAdj

# update low-tech production change

L2052.AgProdChange_ag_irr_ref %>% inner_join(L2052.AgProdChange_ag_irr_ref_lowAdj,
                                             by = c("region", "AgProductionTechnology", "year")) %>%
  mutate(AgProdChange = round(AgProdChange_adj, 4)) %>%
  select(-AgProdChange_adj) %>%
  bind_rows(L2052.AgProdChange_ag_irr_ref %>%
              filter(grepl("_hi", AgProductionTechnology) & year > 2050) %>%
              mutate(AgProductionTechnology = gsub("_hi", "_lo", AgProductionTechnology))) %>%
  arrange(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year)-> L2052.AgProdChange_ag_irr_ref_LOW

L2052.AgProdChange_bio_irr_ref %>% inner_join(L2052.AgProdChange_ag_irr_ref_lowAdj,
                                              by = c("region", "AgProductionTechnology", "year")) %>%
  mutate(AgProdChange = round(AgProdChange_adj, 4)) %>%
  select(-AgProdChange_adj) %>%
  bind_rows(L2052.AgProdChange_bio_irr_ref %>%
              filter(grepl("_hi", AgProductionTechnology) & year > 2050) %>%
              mutate(AgProductionTechnology = gsub("_hi", "_lo", AgProductionTechnology))) %>%
  arrange(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year)-> L2052.AgProdChange_bio_irr_ref_LOW



# generate xml
create_xml("ag_prodchange_ref_IRR_MGMT_YLD_intensification.xml",
           mi_header = "./inst/extdata/mi_headers/ModelInterface_headers.txt") %>%
  add_xml_data(data = L2052.AgProdChange_ag_irr_ref_LOW, header = "AgProdChange") %>%
  add_xml_data(data = L2052.AgProdChange_bio_irr_ref_LOW, header = "AgProdChange") %>%
  run_xml_conversion()


# N Fertilizer
# This part updates N fertilizer coefficients for _lo technologies, which will linearly grow to the same level as the
# corresponding _hi options by 2050, then keep the same as _high options after 2050
# ------------------------------------------------------------------------------------------------------------------------------
L2062.AgCoef_Fert_ag_irr_mgmt <- read_csv("../../gcamdata/outputs/L2062.AgCoef_Fert_ag_irr_mgmt.csv", comment = "#")


L2062.AgCoef_Fert_ag_irr_mgmt_update <- L2062.AgCoef_Fert_ag_irr_mgmt %>%
  filter(grepl("_lo", AgProductionTechnology) & year == 2015) %>%
  bind_rows(L2062.AgCoef_Fert_ag_irr_mgmt %>%
              filter(grepl("_hi", AgProductionTechnology) & year ==2050) %>%
              mutate(AgProductionTechnology = gsub("_hi", "_lo", AgProductionTechnology))) %>%
  group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.energy.input) %>%
  complete(nesting(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.energy.input),
           year = seq(2015, 2050, 5)) %>%
  mutate(coefficient = approx_fun(year, coefficient, rule = 1),year = as.integer(year)) %>%
  ungroup() %>%
  bind_rows(L2062.AgCoef_Fert_ag_irr_mgmt %>%
              filter(grepl("_hi", AgProductionTechnology) & year > 2050) %>%
              mutate(AgProductionTechnology = gsub("_hi", "_lo", AgProductionTechnology))) %>%
  bind_rows(L2062.AgCoef_Fert_ag_irr_mgmt %>%
              filter(grepl("_lo", AgProductionTechnology) & year < 2015)) %>%
  arrange(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.energy.input, year)


create_xml("ag_Fert_IRR_MGMT_YLD_intensification.xml",
           mi_header = "./inst/extdata/mi_headers/ModelInterface_headers.txt") %>%
  add_xml_data(data = L2062.AgCoef_Fert_ag_irr_mgmt_update, header = "AgCoef") %>%
  run_xml_conversion()



