# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_DECARB_industry_scenarios_xml
#'
#' Construct XML data structure for \code{industry_DECARB_ref.xml} and \code{industry_DECARB_hielec.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_DECARB_ref.xml} and \code{industry_DECARB_hielec.xml}.
module_gcamusa_batch_DECARB_industry_scenarios_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/DECARB_ind_scenarios",
             "L232.Supplysector_ind_USA",
             "L2321.StubTech_cement_USA",
             "L2322.SubsectorShrwtFllt_Fert_USA",
             "L225.SubsectorShrwtFllt_h2_USA",
             "L225.StubTechMarket_h2_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_DECARB_ref.xml",
             XML = "industry_DECARB_hielec.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    DECARB_ind_scenarios <- get_data(all_data, "gcam-usa/DECARB_ind_scenarios")
    L232.Supplysector_ind_USA <- get_data(all_data, "L232.Supplysector_ind_USA", strip_attributes = TRUE)
    L2321.StubTech_cement_USA <- get_data(all_data, "L2321.StubTech_cement_USA", strip_attributes = TRUE)
    L2322.SubsectorShrwtFllt_Fert_USA <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert_USA", strip_attributes = TRUE)
    L225.SubsectorShrwtFllt_h2_USA <- get_data(all_data, "L225.SubsectorShrwtFllt_h2_USA", strip_attributes = TRUE)
    L225.StubTechMarket_h2_USA <- get_data(all_data, "L225.StubTechMarket_h2_USA", strip_attributes = TRUE)

    # ===================================================

    # Define the scenario differentiation year (model time period)
    DECARB_divergence_year <- 2025

    # Go through the scenarios table variable-by-variable and scenario-by-scenario
    # 1: Supplysector logit exponents
    SupplysectorLogit_DECARB <- filter(DECARB_ind_scenarios,
                                       is.na(subsector),
                                       is.na(technology),
                                       variable == "logit.exponent") %>%
      select(scenario, supplysector, logit.exponent = value)

      L232.Supplysector_ind_DECARB <- SupplysectorLogit_DECARB %>%
        left_join(select(L232.Supplysector_ind_USA, -logit.exponent),
                  by = "supplysector") %>%
        select(c(LEVEL2_DATA_NAMES[["Supplysector"]], logit.type, scenario))

    L232.Supplysector_ind_DECARB_ref <- filter(L232.Supplysector_ind_DECARB, scenario == "ref")
    L232.Supplysector_ind_DECARB_hielec <- filter(L232.Supplysector_ind_DECARB, scenario == "hielec")

    # 2. Technology share-weights
    TechShrwt_DECARB_ind <- filter(DECARB_ind_scenarios,
                           !is.na(technology),
                           variable == "share.weight") %>%
      select(scenario, supplysector, subsector, stub.technology = technology, share.weight = value)

    L232.StubTechShrwt_ind_DECARB <- TechShrwt_DECARB_ind %>%
      left_join(L2321.StubTech_cement_USA,
                by = c("supplysector", "subsector", "stub.technology")) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS[MODEL_YEARS >= DECARB_divergence_year])) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechShrwt"]], scenario))

    L232.StubTechShrwt_ind_DECARB_ref <- filter(L232.StubTechShrwt_ind_DECARB, scenario == "ref")
    #L232.StubTechShrwt_ind_DECARB <- filter(L232.StubTechShrwt_ind_DECARB, scenario == "hielec")

    # 3. Subsector share-weights
    SubsShrwt_DECARB_ind <- filter(DECARB_ind_scenarios,
                                   is.na(technology),
                                   variable == "share.weight") %>%
      select(scenario, supplysector, subsector, share.weight = value)

    L232.shareweight_table <- bind_rows(L2322.SubsectorShrwtFllt_Fert_USA,
                                        L225.SubsectorShrwtFllt_h2_USA) %>%
      select(-share.weight)

    L232.SubsectorShrwtFllt_ind_DECARB <- SubsShrwt_DECARB_ind %>%
      left_join(L232.shareweight_table, by = c("supplysector", "subsector")) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], scenario))


    L232.SubsectorShrwtFllt_ind_DECARB_ref <- filter(L232.SubsectorShrwtFllt_ind_DECARB, scenario == "ref")
    L232.SubsectorShrwtFllt_ind_DECARB_hielec <- filter(L232.SubsectorShrwtFllt_ind_DECARB, scenario == "hielec")

    # 4. price-unit conversion
    PriceUnitConversion_DECARB_ind <- filter(DECARB_ind_scenarios,
                                   variable == "price.unit.conversion") %>%
      select(scenario, supplysector, subsector, stub.technology = technology, price.unit.conversion = value)

    L232.StubTechInputPMult_ind_DECARB <- L225.StubTechMarket_h2_USA %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechYr"]], "minicam.energy.input")) %>%
      inner_join(PriceUnitConversion_DECARB_ind, by = c("supplysector", "subsector", "stub.technology"))

    # L232.StubTechInputPMult_ind_DECARB_ref <- filter(L232.StubTechInputPMult_ind_DECARB, scenario == "ref") %>%
    #  select(LEVEL2_DATA_NAMES[["StubTechInputPMult"]])
    L232.StubTechInputPMult_ind_DECARB_hielec <- filter(L232.StubTechInputPMult_ind_DECARB, scenario == "hielec") %>%
      select(LEVEL2_DATA_NAMES[["StubTechInputPMult"]])

    # Produce outputs
    create_xml("industry_DECARB_ref.xml") %>%
      add_logit_tables_xml(L232.Supplysector_ind_DECARB_ref, "Supplysector") %>%
      add_xml_data(L232.StubTechShrwt_ind_DECARB_ref, "StubTechShrwt") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind_DECARB_ref, "SubsectorShrwtFllt") %>%
      add_precursors("gcam-usa/DECARB_ind_scenarios",
                     "L232.Supplysector_ind_USA",
                     "L2321.StubTech_cement_USA",
                     "L2322.SubsectorShrwtFllt_Fert_USA") ->
      industry_DECARB_ref.xml

    create_xml("industry_DECARB_hielec.xml") %>%
      add_logit_tables_xml(L232.Supplysector_ind_DECARB_hielec, "Supplysector") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind_DECARB_hielec, "SubsectorShrwtFllt") %>%
      add_xml_data(L232.StubTechInputPMult_ind_DECARB_hielec, "StubTechInputPMult") %>%
      add_precursors("gcam-usa/DECARB_ind_scenarios",
                     "L232.Supplysector_ind_USA",
                     "L225.SubsectorShrwtFllt_h2_USA",
                     "L225.StubTechMarket_h2_USA") ->
      industry_DECARB_hielec.xml

    return_data(industry_DECARB_ref.xml,
                industry_DECARB_hielec.xml)
  } else {
    stop("Unknown command")
  }
}
