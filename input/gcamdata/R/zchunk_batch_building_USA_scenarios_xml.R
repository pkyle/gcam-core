# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_building_USA_scenarios_xml
#'
#' Construct XML data structure for \code{building_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_USA.xml}. The corresponding file in the
#' original data system was \code{batch_building_USA.xml} (gcamusa XML).
module_gcamusa_batch_building_USA_scenarios_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/DECARB_bld_scenarios",
             "L244.Supplysector_bld_gcamusa",
             "L244.SubsectorLogit_bld_gcamusa",
             "L244.GlobalTechCost_bld_gcamusa",
             "L244.GlobalTechEff_bld",
             "L244.GlobalTechShrwt_bld_gcamusa"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_DECARB_ref.xml",
             XML = "building_DECARB_hielec.xml",
             XML = "building_DECARB_adv.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    DECARB_bld_scenarios <- get_data(all_data, "gcam-usa/DECARB_bld_scenarios")
    L244.Supplysector_bld_gcamusa <- get_data(all_data, "L244.Supplysector_bld_gcamusa")
    L244.SubsectorLogit_bld_gcamusa <- get_data(all_data, "L244.SubsectorLogit_bld_gcamusa")
    L244.GlobalTechCost_bld_gcamusa <- get_data(all_data, "L244.GlobalTechCost_bld_gcamusa")
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld")
    L244.GlobalTechShrwt_bld_gcamusa <- get_data(all_data, "L244.GlobalTechShrwt_bld_gcamusa")

    # ===================================================

    # Define the scenario differentiation year (model time period)
    DECARB_divergence_year <- 2025

    # Go through the scenarios table variable-by-variable and scenario-by-scenario
    # 1: Supplysector logit exponents
    SupplysectorLogit_DECARB <- filter(DECARB_bld_scenarios,
                                       is.na(subsector),
                                       is.na(technology),
                                       variable == "logit.exponent") %>%
      select(scenario, supplysector, logit.exponent = value)

    L244.Supplysector_bld_DECARB <- SupplysectorLogit_DECARB %>%
      left_join(select(L244.Supplysector_bld_gcamusa, -logit.exponent),
                by = "supplysector") %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], logit.type, scenario))

    L244.Supplysector_bld_DECARB_ref <- filter(L244.Supplysector_bld_DECARB, scenario == "ref")
    L244.Supplysector_bld_DECARB_hielec <- filter(L244.Supplysector_bld_DECARB, scenario == "alt1")
    L244.Supplysector_bld_DECARB_adv <- filter(L244.Supplysector_bld_DECARB, scenario == "alt2")

    # 2: Subsector logit exponents
    SubsectorLogit_DECARB <- filter(DECARB_bld_scenarios,
                                    !is.na(subsector),
                                    is.na(technology),
                                    variable == "logit.exponent") %>%
      select(scenario, supplysector, subsector, logit.exponent = value)

    L244.SubsectorLogit_bld_DECARB <- SubsectorLogit_DECARB %>%
      left_join(select(L244.SubsectorLogit_bld_gcamusa, -logit.exponent),
                by = c("supplysector", "subsector")) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], logit.type, scenario))

    L244.SubsectorLogit_bld_DECARB_ref <- filter(L244.SubsectorLogit_bld_DECARB, scenario == "ref")
    L244.SubsectorLogit_bld_DECARB_hielec <- filter(L244.SubsectorLogit_bld_DECARB, scenario == "alt1")
    L244.SubsectorLogit_bld_DECARB_adv <- filter(L244.SubsectorLogit_bld_DECARB, scenario == "alt2")

    # 3. Technology share-weights
    Shrwt_DECARB <- filter(DECARB_bld_scenarios,
                           !is.na(technology),
                           variable == "share.weight") %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, share.weight = value)

    L244.GlobalTechShrwt_bld_DECARB <- Shrwt_DECARB %>%
      left_join(select(L244.GlobalTechShrwt_bld_gcamusa, -share.weight),
                by = c("sector.name", "subsector.name", "technology")) %>%
      filter(year >= DECARB_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]], scenario))

    L244.GlobalTechShrwt_bld_DECARB_ref <- filter(L244.GlobalTechShrwt_bld_DECARB, scenario == "ref")
    L244.GlobalTechShrwt_bld_DECARB_hielec <- filter(L244.GlobalTechShrwt_bld_DECARB, scenario == "alt1")
    L244.GlobalTechShrwt_bld_DECARB_adv <- filter(L244.GlobalTechShrwt_bld_DECARB, scenario == "alt2")

    # 4. Technology costs
    Costs_DECARB <- filter(DECARB_bld_scenarios,
                           !is.na(adj_factor),
                           variable == "input.cost") %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, adj_factor)

    L244.GlobalTechCost_bld_gcamusa_DECARB <- Costs_DECARB %>%
      filter(!is.na(technology)) %>%
      left_join(L244.GlobalTechCost_bld_gcamusa,
                by = c("sector.name", "subsector.name", "technology")) %>%
      mutate(input.cost = input.cost * adj_factor) %>%
      filter(year >= DECARB_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechCost"]], scenario))

    Cost_tmp <- Costs_DECARB %>%
      filter(is.na(technology)) %>%
      select(-technology) %>%
      left_join(L244.GlobalTechCost_bld_gcamusa,
                by = c("sector.name", "subsector.name")) %>%
      mutate(input.cost = input.cost * adj_factor) %>%
      filter(year >= DECARB_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechCost"]], scenario))

    L244.GlobalTechCost_bld_gcamusa_DECARB <- bind_rows(L244.GlobalTechCost_bld_gcamusa_DECARB, Cost_tmp)

    L244.GlobalTechCost_bld_gcamusa_DECARB_ref <- filter(L244.GlobalTechCost_bld_gcamusa_DECARB, scenario == "ref")
    L244.GlobalTechCost_bld_gcamusa_DECARB_hielec <- filter(L244.GlobalTechCost_bld_gcamusa_DECARB, scenario == "alt1")
    L244.GlobalTechCost_bld_gcamusa_DECARB_adv <- filter(L244.GlobalTechCost_bld_gcamusa_DECARB, scenario == "alt2")

    # 4. Technology costs
    Efficiencies_DECARB <- filter(DECARB_bld_scenarios,
                                  !is.na(adj_factor),
                                  variable == "efficiency") %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, adj_factor)

    L244.GlobalTechEff_bld_gcamusa_DECARB <- Efficiencies_DECARB %>%
      filter(!is.na(technology)) %>%
      left_join(L244.GlobalTechEff_bld,
                by = c("sector.name", "subsector.name", "technology")) %>%
      mutate(efficiency = efficiency * adj_factor) %>%
      filter(year >= DECARB_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechEff"]], scenario))

    Efficiency_tmp <- Efficiencies_DECARB %>%
      filter(is.na(technology)) %>%
      select(-technology) %>%
      left_join(L244.GlobalTechEff_bld,
                by = c("sector.name", "subsector.name")) %>%
      mutate(efficiency = efficiency * adj_factor) %>%
      filter(year >= DECARB_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechEff"]], scenario))

    L244.GlobalTechEff_bld_gcamusa_DECARB <- bind_rows(L244.GlobalTechEff_bld_gcamusa_DECARB, Efficiency_tmp)

    L244.GlobalTechEff_bld_gcamusa_DECARB_ref <- filter(L244.GlobalTechEff_bld_gcamusa_DECARB, scenario == "ref")
    L244.GlobalTechEff_bld_gcamusa_DECARB_hielec <- filter(L244.GlobalTechEff_bld_gcamusa_DECARB, scenario == "alt1")
    L244.GlobalTechEff_bld_gcamusa_DECARB_adv <- filter(L244.GlobalTechEff_bld_gcamusa_DECARB, scenario == "alt2")


    # Produce outputs
    create_xml("building_DECARB_ref.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_DECARB_ref, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_DECARB_ref, "SubsectorLogit") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_DECARB_ref, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_gcamusa_DECARB_ref, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_gcamusa_DECARB_ref, "GlobalTechEff") %>%
      add_precursors("gcam-usa/DECARB_bld_scenarios",
                     "L244.Supplysector_bld_gcamusa",
                     "L244.SubsectorLogit_bld_gcamusa",
                     "L244.GlobalTechShrwt_bld_gcamusa",
                     "L244.GlobalTechEff_bld",
                     "L244.GlobalTechCost_bld_gcamusa") ->
      building_DECARB_ref.xml

    create_xml("building_DECARB_hielec.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_DECARB_hielec, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_DECARB_hielec, "SubsectorLogit") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_DECARB_hielec, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_gcamusa_DECARB_hielec, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_gcamusa_DECARB_hielec, "GlobalTechEff") %>%
      same_precursors_as(building_DECARB_ref.xml) ->
      building_DECARB_hielec.xml

    create_xml("building_DECARB_adv.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_DECARB_adv, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_DECARB_adv, "SubsectorLogit") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_DECARB_adv, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_gcamusa_DECARB_adv, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_gcamusa_DECARB_adv, "GlobalTechEff") %>%
      same_precursors_as(building_DECARB_ref.xml) ->
      building_DECARB_adv.xml

    return_data(building_DECARB_ref.xml,
                building_DECARB_hielec.xml,
                building_DECARB_adv.xml)
  } else {
    stop("Unknown command")
  }
}
