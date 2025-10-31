# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_IEF_industry_scenarios_xml
#'
#' Construct XML data structure for \code{industry_IEF_StatedPolicies_USA.xml}, \code{industry_IEF_MidTech_USA.xml}, and \code{industry_IEF_HiTech_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_IEF_StatedPolicies_USA.xml}, \code{industry_IEF_MidTech_USA.xml}, and \code{industry_IEF_HiTech_USA.xml}.
module_gcamusa_IEF_industry_scenarios_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/ind_tech_scenarios",
             "L232.Supplysector_ind_USA",
             "L2321.StubTech_cement_USA",
             "L2322.SubsectorShrwtFllt_Fert_USA",
             "L225.SubsectorShrwtFllt_h2_USA",
             "L225.StubTechMarket_h2_USA",
             "L232.SubsectorLogit_ind_USA"))
             #"L2323.SubsectorLogit_iron_steel_US"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_IEF_StatedPolicies_USA.xml",
             XML = "industry_IEF_MidTech_USA.xml",
             XML = "industry_IEF_HiTech_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    ind_tech_scenarios <- get_data(all_data, "gcam-usa/ind_tech_scenarios")
    L232.Supplysector_ind_USA <- get_data(all_data, "L232.Supplysector_ind_USA", strip_attributes = TRUE)
    L2321.StubTech_cement_USA <- get_data(all_data, "L2321.StubTech_cement_USA", strip_attributes = TRUE)
    L2322.SubsectorShrwtFllt_Fert_USA <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert_USA", strip_attributes = TRUE)
    L225.SubsectorShrwtFllt_h2_USA <- get_data(all_data, "L225.SubsectorShrwtFllt_h2_USA", strip_attributes = TRUE)
    L225.StubTechMarket_h2_USA <- get_data(all_data, "L225.StubTechMarket_h2_USA", strip_attributes = TRUE)
    L232.SubsectorLogit_ind_USA <- get_data(all_data, "L232.SubsectorLogit_ind_USA", strip_attributes = TRUE)



    # ===================================================

    # Drop the iron and steel assumptions from ind_tech_scenarios as this sector is not in this version of GCAM-USA
    ind_tech_scenarios <- filter(ind_tech_scenarios, supplysector != "iron and steel")

    # Define the scenario differentiation year (model time period)
    IEF_divergence_year <- 2025

    # Go through the scenarios table variable-by-variable and scenario-by-scenario
    # 1: Supplysector logit exponents
    SupplysectorLogit_IEF <- filter(ind_tech_scenarios,
                                       is.na(subsector),
                                       is.na(technology),
                                       variable == "logit.exponent") %>%
      select(scenario, supplysector, logit.exponent = value)

      L232.Supplysector_ind_IEF <- SupplysectorLogit_IEF %>%
        left_join(select(L232.Supplysector_ind_USA, -logit.exponent),
                  by = "supplysector") %>%
        select(c(LEVEL2_DATA_NAMES[["Supplysector"]], logit.type, scenario))

    L232.Supplysector_ind_IEF_StatedPolicies <- filter(L232.Supplysector_ind_IEF, scenario == "Stated Policies")
    L232.Supplysector_ind_IEF_MidTech <- filter(L232.Supplysector_ind_IEF, scenario == "MidTech")
    L232.Supplysector_ind_IEF_HiTech <- filter(L232.Supplysector_ind_IEF, scenario == "HiTech")

    # 2. Technology share-weights
    TechShrwt_IEF_ind <- filter(ind_tech_scenarios,
                           !is.na(technology),
                           variable == "share.weight") %>%
      select(scenario, supplysector, subsector, stub.technology = technology, share.weight = value)

    L232.StubTechShrwt_ind_IEF <- TechShrwt_IEF_ind %>%
      left_join(L2321.StubTech_cement_USA,
                by = c("supplysector", "subsector", "stub.technology")) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS[MODEL_YEARS >= IEF_divergence_year])) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechShrwt"]], scenario))

    L232.StubTechShrwt_ind_IEF_StatedPolicies <- filter(L232.StubTechShrwt_ind_IEF, scenario == "Stated Policies")
    L232.StubTechShrwt_ind_IEF_MidTech <- filter(L232.StubTechShrwt_ind_IEF, scenario == "MidTech")
    #L232.StubTechShrwt_ind_IEF <- filter(L232.StubTechShrwt_ind_IEF, scenario == "HiTech")

    # 3.A Subsector share-weights: Fillout
    SubsShrwt_IEF_ind <- filter(ind_tech_scenarios,
                                   is.na(technology),
                                   variable == "share.weight") %>%
      select(scenario, supplysector, subsector, share.weight = value)

    L232.shareweight_table <- bind_rows(L2322.SubsectorShrwtFllt_Fert_USA,
                                        L225.SubsectorShrwtFllt_h2_USA) %>%
      select(-share.weight)

    L232.SubsectorShrwtFllt_ind_IEF <- SubsShrwt_IEF_ind %>%
      left_join(L232.shareweight_table, by = c("supplysector", "subsector")) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], scenario))


    L232.SubsectorShrwtFllt_ind_IEF_StatedPolicies <- filter(L232.SubsectorShrwtFllt_ind_IEF, scenario == "Stated Policies")
    L232.SubsectorShrwtFllt_ind_IEF_MidTech <- filter(L232.SubsectorShrwtFllt_ind_IEF, scenario == "MidTech")
    L232.SubsectorShrwtFllt_ind_IEF_HiTech <- filter(L232.SubsectorShrwtFllt_ind_IEF, scenario == "HiTech")


    # 3.B Subsector share-weights: Interpolation Rules
    State_ind_logit <- L232.SubsectorLogit_ind_USA

    SubsectorInterp_IEF_ind <- filter(ind_tech_scenarios, variable %in% c("from.year", "to.value", "to.year", "interpolation.function")) %>%
      select(scenario, supplysector, subsector, variable, value, rule_number) %>%
      spread(key = variable, value = value) %>%
      mutate(apply.to = "share-weight",
             from.year = as.numeric(from.year),
             to.value = as.numeric(to.value),
             to.year = as.integer(to.year)) %>%
      left_join(select(State_ind_logit, region, supplysector, subsector),   #this is what spreads it to all states, will get a warning
                by = c("supplysector", "subsector")) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], scenario))  # what ensures that the rule number order is perpetuated?

    #L244.SubsectorInterpTo_IEF_StatedPolicies <- filter(SubsectorInterp_IEF_ind, scenario == "Stated Policies")
    L244.SubsectorInterpTo_IEF_MidTech <- filter(SubsectorInterp_IEF_ind, scenario == "MidTech")
    L244.SubsectorInterpTo_IEF_HiTech <- filter(SubsectorInterp_IEF_ind, scenario == "HiTech")



    # 4. price-unit conversion, technology in control file doesn't exist, clarification needed
    PriceUnitConversion_IEF_ind <- filter(ind_tech_scenarios,
                                   variable == "price.unit.conversion") %>%
      select(scenario, supplysector, subsector, stub.technology = technology, price.unit.conversion = value)

    L232.StubTechInputPMult_ind_IEF <- L225.StubTechMarket_h2_USA %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechYr"]], "minicam.energy.input")) %>%
      inner_join(PriceUnitConversion_IEF_ind, by = c("supplysector", "subsector", "stub.technology"))

    # L232.StubTechInputPMult_ind_IEF_StatedPolicies <- filter(L232.StubTechInputPMult_ind_IEF, scenario == "Stated Policies") %>%
    #  select(LEVEL2_DATA_NAMES[["StubTechInputPMult"]])
    L232.StubTechInputPMult_ind_IEF_HiTech <- filter(L232.StubTechInputPMult_ind_IEF, scenario == "HiTech") %>%
      select(LEVEL2_DATA_NAMES[["StubTechInputPMult"]])

    # Produce outputs
    create_xml("industry_IEF_StatedPolicies_USA.xml") %>%
      add_logit_tables_xml(L232.Supplysector_ind_IEF_StatedPolicies, "Supplysector") %>%
      add_xml_data(L232.StubTechShrwt_ind_IEF_StatedPolicies, "StubTechShrwt") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind_IEF_StatedPolicies, "SubsectorShrwtFllt") %>%
      add_precursors("gcam-usa/ind_tech_scenarios",
                     "L232.Supplysector_ind_USA",
                     "L2321.StubTech_cement_USA",
                     "L2322.SubsectorShrwtFllt_Fert_USA") ->
      industry_IEF_StatedPolicies_USA.xml

    create_xml("industry_IEF_MidTech_USA.xml") %>%
      add_logit_tables_xml(L232.Supplysector_ind_IEF_MidTech, "Supplysector") %>%
     add_xml_data(L232.StubTechShrwt_ind_IEF_MidTech, "StubTechShrwt") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind_IEF_MidTech, "SubsectorShrwtFllt") %>%
      add_xml_data(L244.SubsectorInterpTo_IEF_MidTech, "SubsectorInterpTo") %>%
      add_precursors("gcam-usa/ind_tech_scenarios",
                     "L232.Supplysector_ind_USA",
                     "L2321.StubTech_cement_USA",
                     "L2322.SubsectorShrwtFllt_Fert_USA",
                     "L232.SubsectorLogit_ind_USA") ->
      industry_IEF_MidTech_USA.xml

    create_xml("industry_IEF_HiTech_USA.xml") %>%
      add_logit_tables_xml(L232.Supplysector_ind_IEF_HiTech, "Supplysector") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind_IEF_HiTech, "SubsectorShrwtFllt") %>%
      add_xml_data(L244.SubsectorInterpTo_IEF_HiTech, "SubsectorInterpTo") %>%
      add_xml_data(L232.StubTechInputPMult_ind_IEF_HiTech, "StubTechInputPMult") %>%
      add_precursors("gcam-usa/ind_tech_scenarios",
                     "L232.Supplysector_ind_USA",
                     "L225.SubsectorShrwtFllt_h2_USA",
                     "L225.StubTechMarket_h2_USA",
                     "L232.SubsectorLogit_ind_USA") ->
      industry_IEF_HiTech_USA.xml

    return_data(industry_IEF_StatedPolicies_USA.xml,
                industry_IEF_MidTech_USA.xml,
                industry_IEF_HiTech_USA.xml)
  } else {
    stop("Unknown command")
  }
}



