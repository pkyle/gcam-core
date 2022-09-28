# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_prodchange_Fert_YLD_xml
#'
#' Construct XML data structure for \code{ag_prodchange_Fert_YLD.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_prodchange_Fert_YLD.xml}. The corresponding file in the
#' original data system was \code{batch_ag_prodchange_Fert_YLD.xml.R} (aglu XML).
module_aglu_batch_ag_prodchange_Fert_YLD_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2052.AgProdChange_ag_irr_YLD",
              "L2052.AgProdChange_bio_irr_YLD",
             "L2062.AgCoef_Fert_ag_irr_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_prodchange_Fert_YLD.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2052.AgProdChange_ag_irr_YLD <- get_data(all_data, "L2052.AgProdChange_ag_irr_YLD")
    L2052.AgProdChange_bio_irr_YLD <- get_data(all_data, "L2052.AgProdChange_bio_irr_YLD")
    L2062.AgCoef_Fert_ag_irr_mgmt <- get_data(all_data, "L2062.AgCoef_Fert_ag_irr_mgmt")

    # Process N fertilizer coefficient data
    L2062.AgCoef_Fert_ag_irr_mgmt_update <- L2062.AgCoef_Fert_ag_irr_mgmt %>%
      filter(grepl("_lo", AgProductionTechnology) & year == 2015) %>%
      bind_rows(L2062.AgCoef_Fert_ag_irr_mgmt %>%
                  filter(grepl("_hi", AgProductionTechnology) & year ==2050) %>%
                  mutate(AgProductionTechnology = gsub("_hi", "_lo", AgProductionTechnology))) %>%
      complete(nesting(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.energy.input),
               year = seq(2015, 2050, 5)) %>%
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient, rule = 1), digits = aglu.DIGITS_CALOUTPUT),
             year = as.integer(year)) %>%
      ungroup() %>%
      bind_rows(L2062.AgCoef_Fert_ag_irr_mgmt %>%
                  filter(grepl("_hi", AgProductionTechnology) & year > 2050) %>%
                  mutate(AgProductionTechnology = gsub("_hi", "_lo", AgProductionTechnology))) %>%
      bind_rows(L2062.AgCoef_Fert_ag_irr_mgmt %>%
                  filter(grepl("_lo", AgProductionTechnology) & year < 2015)) %>%
      arrange(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.energy.input, year)

    # ===================================================

    # Produce outputs
    create_xml("ag_prodchange_Fert_YLD.xml") %>%
      add_xml_data(L2052.AgProdChange_ag_irr_YLD, "AgProdChange") %>%
      add_xml_data(L2052.AgProdChange_bio_irr_YLD, "AgProdChange") %>%
      add_xml_data(L2062.AgCoef_Fert_ag_irr_mgmt_update, "AgCoef") %>%
      add_precursors("L2052.AgProdChange_ag_irr_YLD", "L2052.AgProdChange_bio_irr_YLD", "L2062.AgCoef_Fert_ag_irr_mgmt") ->
      ag_prodchange_Fert_YLD.xml

    return_data(ag_prodchange_Fert_YLD.xml)
  } else {
    stop("Unknown command")
  }
}
