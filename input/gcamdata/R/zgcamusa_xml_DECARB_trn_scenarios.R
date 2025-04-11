# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_DECARB_trn_scenarios_xml
#'
#' Construct XML data structure for \code{transport_DECARB_IRA.xml},
#' \code{transport_DECARB_MidTech.xml}, \code{transport_DECARB_HiTech.xml},
#' \code{transport_gcamusa_DECARB_IRA.xml}, \code{transport_gcamusa_DECARB_HiTech.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:\code{transport_DECARB_IRA.xml}, \code{transport_DECARB_MidTech.xml},
#'  \code{transport_DECARB_HiTech.xml}, \code{transport_gcamusa_DECARB_IRA.xml},
#'  \code{transport_gcamusa_DECARB_HiTech.xml}.
module_gcamusa_DECARB_trn_scenarios_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/DECARB_trn_scenarios",
             FILE = "gcam-usa/states_subregions",
             "L254.StubTranTechCoef_USA",
             "L254.StubTranTechCoef",
             "L254.StubTranTechCost",
             "L254.StubTranTechCost_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transport_DECARB_IRA.xml",
             XML = "transport_DECARB_MidTech.xml",
             XML = "transport_DECARB_HiTech.xml",
             XML = "transport_gcamusa_DECARB_IRA.xml",
             XML = "transport_gcamusa_DECARB_HiTech.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    DECARB_trn_scenarios <- get_data(all_data, "gcam-usa/DECARB_trn_scenarios")
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
    DECARB_divergence_year <- 2025
    DECARB_terminal_year <- 2050
    DECARB_years <- MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS >= DECARB_divergence_year &
                                         MODEL_FUTURE_YEARS <= DECARB_terminal_year]

    # Go through the scenarios table variable-by-variable and scenario-by-scenario
    # 1. Technology share-weights
    # Note that these aren't the actual share-weight paths which use interpolation rules
    # The values here will be used as the "to-value" of the corresponding technology's interpolation rule
    # and held constant thereafter
    Trn_Shrwt_DECARB <- filter(DECARB_trn_scenarios,
                           !is.na(technology),
                           variable == "share.weight") %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, share.weight = value) %>%
      mutate(share.weight = as.numeric(share.weight)) %>%
      repeat_add_columns(tibble(year = DECARB_years)) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]], scenario))

    L254.GlobalTechShrwt_trn_DECARB_IRA <- filter(Trn_Shrwt_DECARB, scenario == "IRA")
    L254.GlobalTechShrwt_trn_DECARB_MidTech <- filter(Trn_Shrwt_DECARB, scenario == "MidTech")
    L254.GlobalTechShrwt_trn_DECARB_HiTech <- filter(Trn_Shrwt_DECARB, scenario == "HiTech")

    # 2. Global technology share-weight interpolation rules
    GlobalTechInterpTo_DECARB <- filter(DECARB_trn_scenarios, variable %in% c("from.year", "to.value", "to.year", "interpolation.function")) %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, variable, value, rule_number) %>%
      spread(key = variable, value = value) %>%
      mutate(apply.to = "share.weight",
             from.year = as.numeric(from.year),
             to.value = as.numeric(to.value),
             to.year = as.integer(to.year)) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]], scenario))

    L254.GlobalTechInterpTo_DECARB_IRA <- filter(GlobalTechInterpTo_DECARB, scenario == "IRA")
    L254.GlobalTechInterpTo_DECARB_MidTech <- filter(GlobalTechInterpTo_DECARB, scenario == "MidTech")
    L254.GlobalTechInterpTo_DECARB_HiTech <- filter(GlobalTechInterpTo_DECARB, scenario == "HiTech")

    # 3. Energy intensity adjustment
    Coef_DECARB <- filter(DECARB_trn_scenarios,
                          !is.na(adj_factor),
                          variable == "coefficient") %>%
      select(scenario, supplysector, tranSubsector = subsector, stub.technology = technology, year = value, adj_factor) %>%
      mutate(year = as.numeric(year))

    L254.StubTranTechCoef_DECARB <- Coef_DECARB %>%
      left_join(L254.StubTranTechCoef %>%
                  filter(region == 'USA'),
                by = c("supplysector", "tranSubsector", "stub.technology", "year")) %>%
      mutate(coefficient = coefficient * adj_factor) %>%
      filter(year >= DECARB_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTranTechCoef"]], scenario))

    L254.StubTranTechCoef_DECARB %>%
      mutate(coefficient = round(coefficient, digits = gcamusa.DIGITS_TRNUSA_DEFAULT)) %>%
      process_USA_to_states ->
      L254.StubTranTechCoef_USA_DECARB

    L254.StubTranTechCoef_DECARB_IRA <- filter(L254.StubTranTechCoef_DECARB, scenario == "IRA")
    # L254.StubTranTechCoef_DECARB_MidTech <- filter(L254.StubTranTechCoef_DECARB, scenario == "MidTech")
    L254.StubTranTechCoef_DECARB_HiTech <- filter(L254.StubTranTechCoef_DECARB, scenario == "HiTech")

    L254.StubTranTechCoef_USA_DECARB_IRA <- filter(L254.StubTranTechCoef_USA_DECARB, scenario == "IRA")
    # L254.StubTranTechCoef_DECARB_USA_MidTech <- filter(L254.StubTranTechCoef_USA_DECARB, scenario == "MidTech")
    L254.StubTranTechCoef_USA_DECARB_HiTech <- filter(L254.StubTranTechCoef_USA_DECARB, scenario == "HiTech")

    # 4. Capital costs adjustment
    Cost_DECARB <- filter(DECARB_trn_scenarios,
                          !is.na(adj_factor),
                          variable == "input.cost") %>%
      select(scenario, supplysector, tranSubsector = subsector, stub.technology = technology, year = value, adj_factor) %>%
      mutate(year = as.numeric(year))

    L254.StubTranTechCost_DECARB <- Cost_DECARB %>%
      left_join(L254.StubTranTechCost %>%
                  filter(region == 'USA'),
                by = c("supplysector", "tranSubsector", "stub.technology", "year")) %>%
      mutate(input.cost = input.cost * adj_factor) %>%
      filter(year >= DECARB_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTranTechCost"]], scenario))

    process_USA_to_states(L254.StubTranTechCost_DECARB) -> L254.StubTranTechCost_USA_DECARB

    L254.StubTranTechCost_DECARB_IRA <- filter(L254.StubTranTechCost_DECARB, scenario == "IRA")
    # L254.StubTranTechCost_DECARB_MidTech <- filter(L254.StubTranTechCost_DECARB, scenario == "MidTech")
    L254.StubTranTechCost_DECARB_HiTech <- filter(L254.StubTranTechCost_DECARB, scenario == "HiTech")

    L254.StubTranTechCost_USA_DECARB_IRA <- filter(L254.StubTranTechCost_USA_DECARB, scenario == "IRA")
    # L254.StubTranTechCost_DECARB_USA_MidTech <- filter(L254.StubTranTechCost_USA_DECARB, scenario == "MidTech")
    L254.StubTranTechCost_USA_DECARB_HiTech <- filter(L254.StubTranTechCost_USA_DECARB, scenario == "HiTech")

    # Produce outputs
    create_xml("transport_DECARB_IRA.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_DECARB_IRA, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_DECARB_IRA, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_DECARB_IRA, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_DECARB_IRA, "StubTranTechCoef") %>%
      add_precursors("gcam-usa/DECARB_trn_scenarios",
                     "L254.StubTranTechCoef",
                     "L254.StubTranTechCost") ->
      transport_DECARB_IRA.xml

    create_xml("transport_DECARB_MidTech.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_DECARB_MidTech, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_DECARB_MidTech, "GlobalTechShrwt") %>%
      # add_xml_data(L254.StubTranTechCost_DECARB_MidTech, "StubTranTechCost") %>% MidTech is the benchmark scenario, so no adj_factors for MidTech
      # add_xml_data(L254.StubTranTechCoef_DECARB_MidTech, "StubTranTechCoef") %>%
      add_precursors("gcam-usa/DECARB_trn_scenarios") ->
      transport_DECARB_MidTech.xml

    create_xml("transport_DECARB_HiTech.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_DECARB_HiTech, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_DECARB_HiTech, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_DECARB_HiTech, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_DECARB_HiTech, "StubTranTechCoef") %>%
      add_precursors("gcam-usa/DECARB_trn_scenarios",
                     "L254.StubTranTechCoef",
                     "L254.StubTranTechCost") ->
      transport_DECARB_HiTech.xml

    create_xml("transport_gcamusa_DECARB_IRA.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_DECARB_IRA, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_DECARB_IRA, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_USA_DECARB_IRA, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA_DECARB_IRA, "StubTranTechCoef") %>%
      add_precursors("gcam-usa/DECARB_trn_scenarios",
                     "gcam-usa/states_subregions",
                     "L254.StubTranTechCoef_USA",
                     "L254.StubTranTechCost_USA") ->
      transport_gcamusa_DECARB_IRA.xml

    # MidTech is the benchmark scenario for StubTech coef and cost, so no adjustment factors are needed for MidTech
    # Because the other tables are GlobalTech which should apply to both GCAM and GCAM-USA, there's no need
    # to create a GCAM-USA specific XML for MidTech
    # create_xml("transport_gcamusa_DECARB_MidTech.xml") %>%
    #   add_xml_data(L254.GlobalTechInterpTo_DECARB_MidTech, "GlobalTechInterpTo") %>%
    #   add_xml_data(L254.GlobalTechShrwt_trn_DECARB_MidTech, "GlobalTechShrwt") %>%
    #   # add_xml_data(L254.StubTranTechCost_USA_DECARB_MidTech, "StubTranTechCost") %>%
    #   # add_xml_data(L254.StubTranTechCoef_USA_DECARB_MidTech, "StubTranTechCoef") %>%
    #   add_precursors("gcam-usa/DECARB_trn_scenarios",
    #                  "gcam-usa/states_subregions") ->
    #   transport_gcamusa_DECARB_MidTech.xml

    create_xml("transport_gcamusa_DECARB_HiTech.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_DECARB_HiTech, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_DECARB_HiTech, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_USA_DECARB_HiTech, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA_DECARB_HiTech, "StubTranTechCoef") %>%
      add_precursors("gcam-usa/DECARB_trn_scenarios",
                     "gcam-usa/states_subregions",
                     "L254.StubTranTechCoef_USA",
                     "L254.StubTranTechCost_USA") ->
      transport_gcamusa_DECARB_HiTech.xml


    return_data(transport_DECARB_IRA.xml,
                transport_DECARB_MidTech.xml,
                transport_DECARB_HiTech.xml,
                transport_gcamusa_DECARB_IRA.xml,
                # transport_gcamusa_DECARB_MidTech.xml,
                transport_gcamusa_DECARB_HiTech.xml)
  } else {
    stop("Unknown command")
  }
}
