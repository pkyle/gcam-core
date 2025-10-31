# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_IEF_trn_scenarios_xml
#'
#' Construct XML data structure for \code{transport_IEF_StatedPolicies_USAreg.xml},
#' \code{transport_IEF_MidTech_USAreg.xml}, \code{transport_IEF_HiTech_USAreg.xml},
#' \code{transport_IEF_StatedPolicies_USA.xml}, \code{transport_IEF_HiTech_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:\code{transport_IEF_StatedPolicies_USAreg.xml}, \code{transport_IEF_MidTech_USAreg.xml},
#'  \code{transport_IEF_HiTech_USAreg.xml}, \code{transport_IEF_StatedPolicies_USA.xml},
#'  \code{transport_IEF_HiTech_USA.xml}.
module_gcamusa_IEF_trn_scenarios_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/trn_tech_scenarios",
             FILE = "gcam-usa/states_subregions",
             "L254.StubTranTechCoef_USA",
             "L254.StubTranTechCoef",
             "L254.StubTranTechCost",
             "L254.StubTranTechCost_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transport_IEF_StatedPolicies_USAreg.xml",
             XML = "transport_IEF_MidTech_USAreg.xml",
             XML = "transport_IEF_HiTech_USAreg.xml",
             XML = "transport_IEF_StatedPolicies_USA.xml",
             XML = "transport_IEF_HiTech_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    trn_tech_scenarios <- get_data(all_data, "gcam-usa/trn_tech_scenarios")
    L254.StubTranTechCoef_USA <- get_data(all_data, "L254.StubTranTechCoef_USA", strip_attributes = TRUE)
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef", strip_attributes = TRUE)
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost", strip_attributes = TRUE)
    L254.StubTranTechCost_USA <- get_data(all_data, "L254.StubTranTechCost_USA", strip_attributes = TRUE)
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions",strip_attributes = TRUE)

    # ===================================================

    # Process tables at the USA region level to the states level.
    # All tables for which processing is identical are done by a function.
    # This applies to the supplysectors, subsectors, and stub tech characteristics of the states.
    process_USA_to_states <- function(data) {
      state <- region <- grid_region <- subsector <- market.name <-
        minicam.energy.input <- NULL  # silence package check notes

      data_new <- data %>%
        filter(region == gcam.USA_REGION) %>%
        write_to_all_states(names = c(names(data), "region"))

      # Re-set markets from USA to grid region, if the minicam.energy.input is considered a regional fuel market
      if("market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                       grid_region[minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS])) %>%
          select(-grid_region)
      }

      # For fuels consumed from state markets, the market.name is the region
      if("market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS,
                                       region[minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS]))
      }

      data_new
    }

    # Define the scenario differentiation year (model time period)
    IEF_divergence_year <- 2025
    IEF_terminal_year <- 2050
    IEF_years <- MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS >= IEF_divergence_year &
                                         MODEL_FUTURE_YEARS <= IEF_terminal_year]

    # Go through the scenarios table variable-by-variable and scenario-by-scenario
    # 1. Technology share-weights
    # Note that these aren't the actual share-weight paths which use interpolation rules
    # The values here will be used as the "to-value" of the corresponding technology's interpolation rule
    # and held constant thereafter
    Trn_Shrwt_IEF <- filter(trn_tech_scenarios,
                           !is.na(technology),
                           variable == "share.weight") %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, share.weight = value) %>%
      mutate(share.weight = as.numeric(share.weight)) %>%
      repeat_add_columns(tibble(year = IEF_years)) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]], scenario))

    L254.GlobalTechShrwt_trn_IEF_StatedPolicies <- filter(Trn_Shrwt_IEF, scenario == "Stated Policies")
    L254.GlobalTechShrwt_trn_IEF_MidTech <- filter(Trn_Shrwt_IEF, scenario == "MidTech")
    L254.GlobalTechShrwt_trn_IEF_HiTech <- filter(Trn_Shrwt_IEF, scenario == "HiTech")

    # 2. Global technology share-weight interpolation rules
    GlobalTechInterpTo_IEF <- filter(trn_tech_scenarios, variable %in% c("from.year", "to.value", "to.year", "interpolation.function")) %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, variable, value, rule_number) %>%
      spread(key = variable, value = value) %>%
      mutate(apply.to = "share.weight",
             from.year = as.numeric(from.year),
             to.value = as.numeric(to.value),
             to.year = as.integer(to.year)) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]], scenario))

    L254.GlobalTechInterpTo_IEF_StatedPolicies <- filter(GlobalTechInterpTo_IEF, scenario == "Stated Policies")
    L254.GlobalTechInterpTo_IEF_MidTech <- filter(GlobalTechInterpTo_IEF, scenario == "MidTech")
    L254.GlobalTechInterpTo_IEF_HiTech <- filter(GlobalTechInterpTo_IEF, scenario == "HiTech")

    # 3. Energy intensity adjustment
    Coef_IEF <- filter(trn_tech_scenarios,
                          !is.na(adj_factor),
                          variable == "coefficient") %>%
      select(scenario, supplysector, tranSubsector = subsector, stub.technology = technology, year = value, adj_factor) %>%
      mutate(year = as.numeric(year))

    L254.StubTranTechCoef_IEF <- Coef_IEF %>%
      left_join(L254.StubTranTechCoef %>%
                  filter(region == 'USA'),
                by = c("supplysector", "tranSubsector", "stub.technology", "year")) %>%
      mutate(coefficient = coefficient * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTranTechCoef"]], scenario))

    L254.StubTranTechCoef_IEF %>%
      mutate(coefficient = round(coefficient, digits = gcamusa.DIGITS_TRNUSA_DEFAULT)) %>%
      process_USA_to_states ->
      L254.StubTranTechCoef_USA_IEF

    L254.StubTranTechCoef_IEF_StatedPolicies <- filter(L254.StubTranTechCoef_IEF, scenario == "Stated Policies")
    # L254.StubTranTechCoef_IEF_MidTech <- filter(L254.StubTranTechCoef_IEF, scenario == "MidTech")
    L254.StubTranTechCoef_IEF_HiTech <- filter(L254.StubTranTechCoef_IEF, scenario == "HiTech")

    L254.StubTranTechCoef_USA_IEF_StatedPolicies <- filter(L254.StubTranTechCoef_USA_IEF, scenario == "Stated Policies")
    # L254.StubTranTechCoef_IEF_USA_MidTech <- filter(L254.StubTranTechCoef_USA_IEF, scenario == "MidTech")
    L254.StubTranTechCoef_USA_IEF_HiTech <- filter(L254.StubTranTechCoef_USA_IEF, scenario == "HiTech")

    # 4. Capital costs adjustment
    Cost_IEF <- filter(trn_tech_scenarios,
                          !is.na(adj_factor),
                          variable == "input.cost") %>%
      select(scenario, supplysector, tranSubsector = subsector, stub.technology = technology, year = value, adj_factor) %>%
      mutate(year = as.numeric(year))

    L254.StubTranTechCost_IEF <- Cost_IEF %>%
      left_join(L254.StubTranTechCost %>%
                  filter(region == 'USA'),
                by = c("supplysector", "tranSubsector", "stub.technology", "year")) %>%
      mutate(input.cost = input.cost * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTranTechCost"]], scenario))

    process_USA_to_states(L254.StubTranTechCost_IEF) -> L254.StubTranTechCost_USA_IEF

    L254.StubTranTechCost_IEF_StatedPolicies <- filter(L254.StubTranTechCost_IEF, scenario == "Stated Policies")
    # L254.StubTranTechCost_IEF_MidTech <- filter(L254.StubTranTechCost_IEF, scenario == "MidTech")
    L254.StubTranTechCost_IEF_HiTech <- filter(L254.StubTranTechCost_IEF, scenario == "HiTech")

    L254.StubTranTechCost_USA_IEF_StatedPolicies <- filter(L254.StubTranTechCost_USA_IEF, scenario == "Stated Policies")
    # L254.StubTranTechCost_IEF_USA_MidTech <- filter(L254.StubTranTechCost_USA_IEF, scenario == "MidTech")
    L254.StubTranTechCost_USA_IEF_HiTech <- filter(L254.StubTranTechCost_USA_IEF, scenario == "HiTech")

    # Produce outputs
    create_xml("transport_IEF_StatedPolicies_USAreg.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_IEF_StatedPolicies, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_IEF_StatedPolicies, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_IEF_StatedPolicies, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_IEF_StatedPolicies, "StubTranTechCoef") %>%
      add_precursors("gcam-usa/trn_tech_scenarios",
                     "L254.StubTranTechCoef",
                     "L254.StubTranTechCost") ->
      transport_IEF_StatedPolicies_USAreg.xml

    create_xml("transport_IEF_MidTech_USAreg.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_IEF_MidTech, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_IEF_MidTech, "GlobalTechShrwt") %>%
      # add_xml_data(L254.StubTranTechCost_IEF_MidTech, "StubTranTechCost") %>% MidTech is the benchmark scenario, so no adj_factors for MidTech
      # add_xml_data(L254.StubTranTechCoef_IEF_MidTech, "StubTranTechCoef") %>%
      add_precursors("gcam-usa/trn_tech_scenarios") ->
      transport_IEF_MidTech_USAreg.xml

    create_xml("transport_IEF_HiTech_USAreg.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_IEF_HiTech, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_IEF_HiTech, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_IEF_HiTech, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_IEF_HiTech, "StubTranTechCoef") %>%
      add_precursors("gcam-usa/trn_tech_scenarios",
                     "L254.StubTranTechCoef",
                     "L254.StubTranTechCost") ->
      transport_IEF_HiTech_USAreg.xml

    create_xml("transport_IEF_StatedPolicies_USA.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_IEF_StatedPolicies, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_IEF_StatedPolicies, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_USA_IEF_StatedPolicies, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA_IEF_StatedPolicies, "StubTranTechCoef") %>%
      add_precursors("gcam-usa/trn_tech_scenarios",
                     "gcam-usa/states_subregions",
                     "L254.StubTranTechCoef_USA",
                     "L254.StubTranTechCost_USA") ->
      transport_IEF_StatedPolicies_USA.xml

    # MidTech is the benchmark scenario for StubTech coef and cost, so no adjustment factors are needed for MidTech
    # Because the other tables are GlobalTech which should apply to both GCAM and GCAM-USA, there's no need
    # to create a GCAM-USA specific XML for MidTech
    # create_xml("transport_gcamusa_IEF_MidTech.xml") %>%
    #   add_xml_data(L254.GlobalTechInterpTo_IEF_MidTech, "GlobalTechInterpTo") %>%
    #   add_xml_data(L254.GlobalTechShrwt_trn_IEF_MidTech, "GlobalTechShrwt") %>%
    #   # add_xml_data(L254.StubTranTechCost_USA_IEF_MidTech, "StubTranTechCost") %>%
    #   # add_xml_data(L254.StubTranTechCoef_USA_IEF_MidTech, "StubTranTechCoef") %>%
    #   add_precursors("gcam-usa/trn_tech_scenarios",
    #                  "gcam-usa/states_subregions") ->
    #   transport_gcamusa_IEF_MidTech.xml

    create_xml("transport_IEF_HiTech_USA.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_IEF_HiTech, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_IEF_HiTech, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_USA_IEF_HiTech, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA_IEF_HiTech, "StubTranTechCoef") %>%
      add_precursors("gcam-usa/trn_tech_scenarios",
                     "gcam-usa/states_subregions",
                     "L254.StubTranTechCoef_USA",
                     "L254.StubTranTechCost_USA") ->
      transport_IEF_HiTech_USA.xml


    return_data(transport_IEF_StatedPolicies_USAreg.xml,
                transport_IEF_MidTech_USAreg.xml,
                transport_IEF_HiTech_USAreg.xml,
                transport_IEF_StatedPolicies_USA.xml,
                transport_IEF_HiTech_USA.xml)
  } else {
    stop("Unknown command")
  }
}
