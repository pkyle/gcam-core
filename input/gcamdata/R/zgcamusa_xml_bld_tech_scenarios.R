# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_bld_tech_scenarios_xml
#'
#' Construct XML data structure for \code{building_USA_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_USA_USA.xml}. The corresponding file in the
#' original data system was \code{batch_building_USA_USA.xml} (gcamusa XML).
module_gcamusa_bld_tech_scenarios_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/bld_tech_scenarios",
             "L244.ShellConductance_bld_gcamusa",
             "L244.Supplysector_bld_gcamusa",
             "L244.SubsectorLogit_bld_gcamusa",
             "L244.GlobalTechCost_bld_gcamusa",
             "L244.GlobalTechEff_bld",
             "L244.GlobalTechShrwt_bld_gcamusa"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_IEF_StatedPolicies_USA.xml",
             XML = "building_IEF_MidTech_USA.xml",
             XML = "building_IEF_HiTech_USA.xml",
             XML = "building_IEF_Breakthru_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    bld_tech_scenarios <- get_data(all_data, "gcam-usa/bld_tech_scenarios")
    L244.ShellConductance_bld_gcamusa <- get_data(all_data, "L244.ShellConductance_bld_gcamusa", strip_attributes = TRUE)
    L244.Supplysector_bld_gcamusa <- get_data(all_data, "L244.Supplysector_bld_gcamusa", strip_attributes = TRUE)
    L244.SubsectorLogit_bld_gcamusa <- get_data(all_data, "L244.SubsectorLogit_bld_gcamusa", strip_attributes = TRUE)
    L244.GlobalTechCost_bld_gcamusa <- get_data(all_data, "L244.GlobalTechCost_bld_gcamusa", strip_attributes = TRUE)
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld", strip_attributes = TRUE)
    L244.GlobalTechShrwt_bld_gcamusa <- get_data(all_data, "L244.GlobalTechShrwt_bld_gcamusa", strip_attributes = TRUE)

    # ===================================================

    # Define the scenario differentiation year (model time period)
    IEF_divergence_year <- 2025

    # Go through the scenarios table variable-by-variable and scenario-by-scenario
    # 1: Supplysector logit exponents
    SupplysectorLogit_IEF <- filter(bld_tech_scenarios,
                                       is.na(subsector),
                                       is.na(technology),
                                       variable == "logit.exponent") %>%
      select(scenario, supplysector, logit.exponent = value) %>%
      mutate(logit.exponent = as.numeric(logit.exponent))

    L244.Supplysector_bld_IEF <- SupplysectorLogit_IEF %>%
      left_join(select(L244.Supplysector_bld_gcamusa, -logit.exponent),
                by = "supplysector") %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], logit.type, scenario))

    L244.Supplysector_bld_IEF_StatedPolicies <- filter(L244.Supplysector_bld_IEF, scenario == "Stated Policies")
    L244.Supplysector_bld_IEF_MidTech <- filter(L244.Supplysector_bld_IEF, scenario == "MidTech")
    L244.Supplysector_bld_IEF_HiTech <- filter(L244.Supplysector_bld_IEF, scenario == "HiTech")
    L244.Supplysector_bld_IEF_Breakthru <- filter(L244.Supplysector_bld_IEF, scenario == "Breakthru")

    # 2: Subsector logit exponents
    SubsectorLogit_IEF <- filter(bld_tech_scenarios,
                                    !is.na(subsector),
                                    is.na(technology),
                                    variable == "logit.exponent") %>%
      select(scenario, supplysector, subsector, logit.exponent = value) %>%
      mutate(logit.exponent = as.numeric(logit.exponent))

    L244.SubsectorLogit_bld_IEF <- SubsectorLogit_IEF %>%
      left_join(select(L244.SubsectorLogit_bld_gcamusa, -logit.exponent),
                by = c("supplysector", "subsector")) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], logit.type, scenario))

    L244.SubsectorLogit_bld_IEF_StatedPolicies <- filter(L244.SubsectorLogit_bld_IEF, scenario == "Stated Policies")
    L244.SubsectorLogit_bld_IEF_MidTech <- filter(L244.SubsectorLogit_bld_IEF, scenario == "MidTech")
    L244.SubsectorLogit_bld_IEF_HiTech <- filter(L244.SubsectorLogit_bld_IEF, scenario == "HiTech")
    L244.SubsectorLogit_bld_IEF_Breakthru <- filter(L244.SubsectorLogit_bld_IEF, scenario == "Breakthru")

    # 3. Technology share-weights
    Shrwt_IEF <- filter(bld_tech_scenarios,
                           !is.na(technology),
                           variable == "share.weight") %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, share.weight = value) %>%
      mutate(share.weight = as.numeric(share.weight))

    L244.GlobalTechShrwt_bld_IEF <- Shrwt_IEF %>%
      left_join(select(L244.GlobalTechShrwt_bld_gcamusa, -share.weight),
                by = c("sector.name", "subsector.name", "technology")) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]], scenario))

    L244.GlobalTechShrwt_bld_IEF_StatedPolicies <- filter(L244.GlobalTechShrwt_bld_IEF, scenario == "Stated Policies")
    L244.GlobalTechShrwt_bld_IEF_MidTech <- filter(L244.GlobalTechShrwt_bld_IEF, scenario == "MidTech")
    L244.GlobalTechShrwt_bld_IEF_HiTech <- filter(L244.GlobalTechShrwt_bld_IEF, scenario == "HiTech")
    L244.GlobalTechShrwt_bld_IEF_Breakthru <- filter(L244.GlobalTechShrwt_bld_IEF, scenario == "Breakthru")

    # 3.5. Global technology share-weight interpolation rules
    L244.GlobalTechInterpTo_IEF <- filter(bld_tech_scenarios,
                                           !is.na(technology),
                                           variable %in% c("from.year", "to.value", "to.year", "interpolation.function")) %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, variable, value, rule_number) %>%
      spread(key = variable, value = value) %>%
      mutate(apply.to = "share.weight",
             from.year = as.numeric(from.year),
             to.value = as.numeric(to.value),
             to.year = as.integer(to.year)) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]], scenario))

    L244.GlobalTechInterpTo_IEF_StatedPolicies <- filter(L244.GlobalTechInterpTo_IEF, scenario == "Stated Policies")
    L244.GlobalTechInterpTo_IEF_MidTech <- filter(L244.GlobalTechInterpTo_IEF, scenario == "MidTech")
    L244.GlobalTechInterpTo_IEF_HiTech <- filter(L244.GlobalTechInterpTo_IEF, scenario == "HiTech")
    L244.GlobalTechInterpTo_IEF_Breakthru <- filter(L244.GlobalTechInterpTo_IEF, scenario == "Breakthru")

    # 4. Technology costs
    Costs_IEF <- filter(bld_tech_scenarios,
                           !is.na(adj_factor),
                           variable == "input.cost") %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, adj_factor)

    L244.GlobalTechCost_bld_gcamusa_IEF <- Costs_IEF %>%
      filter(!is.na(technology)) %>%
      left_join(L244.GlobalTechCost_bld_gcamusa,
                by = c("sector.name", "subsector.name", "technology")) %>%
      mutate(input.cost = input.cost * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechCost"]], scenario))

    Cost_tmp_subsector <- Costs_IEF %>%
      filter(is.na(technology)) %>%
      select(-technology) %>%
      left_join(L244.GlobalTechCost_bld_gcamusa,
                by = c("sector.name", "subsector.name")) %>%
      mutate(input.cost = input.cost * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechCost"]], scenario))

    Cost_tmp_sector <- Costs_IEF %>%
      filter(is.na(technology) & is.na(subsector.name)) %>%
      select(-technology, -subsector.name) %>%
      left_join(L244.GlobalTechCost_bld_gcamusa,
                by = c("sector.name")) %>%
      mutate(input.cost = input.cost * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechCost"]], scenario))

    L244.GlobalTechCost_bld_gcamusa_IEF <- bind_rows(L244.GlobalTechCost_bld_gcamusa_IEF, Cost_tmp_subsector, Cost_tmp_sector)

    L244.GlobalTechCost_bld_gcamusa_IEF_StatedPolicies <- filter(L244.GlobalTechCost_bld_gcamusa_IEF, scenario == "Stated Policies")
    L244.GlobalTechCost_bld_gcamusa_IEF_MidTech <- filter(L244.GlobalTechCost_bld_gcamusa_IEF, scenario == "MidTech")
    L244.GlobalTechCost_bld_gcamusa_IEF_HiTech <- filter(L244.GlobalTechCost_bld_gcamusa_IEF, scenario == "HiTech")
    L244.GlobalTechCost_bld_gcamusa_IEF_Breakthru <- filter(L244.GlobalTechCost_bld_gcamusa_IEF, scenario == "Breakthru")

    # 5. Technology efficiencies
    Efficiencies_IEF <- filter(bld_tech_scenarios,
                                  !is.na(adj_factor),
                                  variable == "efficiency") %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, adj_factor)

    L244.GlobalTechEff_bld_gcamusa_IEF <- Efficiencies_IEF %>%
      filter(!is.na(technology)) %>%
      left_join(L244.GlobalTechEff_bld,
                by = c("sector.name", "subsector.name", "technology")) %>%
      mutate(efficiency = efficiency * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechEff"]], scenario))

    # Apply adjustment factor to all technologies within a subsector, where individual technologies aren't specified
    Efficiency_tmp <- Efficiencies_IEF %>%
      filter(is.na(technology)) %>%
      select(-technology) %>%
      left_join(L244.GlobalTechEff_bld,
                by = c("sector.name", "subsector.name")) %>%
      mutate(efficiency = efficiency * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechEff"]], scenario))

    L244.GlobalTechEff_bld_gcamusa_IEF <- bind_rows(L244.GlobalTechEff_bld_gcamusa_IEF, Efficiency_tmp)

    L244.GlobalTechEff_bld_gcamusa_IEF_StatedPolicies <- filter(L244.GlobalTechEff_bld_gcamusa_IEF, scenario == "Stated Policies")
    L244.GlobalTechEff_bld_gcamusa_IEF_MidTech <- filter(L244.GlobalTechEff_bld_gcamusa_IEF, scenario == "MidTech")
    L244.GlobalTechEff_bld_gcamusa_IEF_HiTech <- filter(L244.GlobalTechEff_bld_gcamusa_IEF, scenario == "HiTech")
    L244.GlobalTechEff_bld_gcamusa_IEF_Breakthru <- filter(L244.GlobalTechEff_bld_gcamusa_IEF, scenario == "Breakthru")

    # 5b. Shell conductance (shell efficiency)
    # Expand the adjustment factor to all analysis years, but interpolate linearly from 1 in 2020
    # to the factor in 2050 in order to account for the stock effects
    IEF_shell_years <- seq(2020, 2050, 5)
    ShellConductanceMult_IEF <- filter(bld_tech_scenarios,
                                  !is.na(adj_factor),
                                  variable == "shell.conductance") %>%
      select(scenario, gcam.consumer, adj_factor) %>%
      mutate(year = max(IEF_shell_years)) %>%
      complete(nesting(scenario, gcam.consumer), year = IEF_shell_years) %>%
      mutate(adj_factor = if_else(year == min(IEF_shell_years), 1, adj_factor)) %>%
      group_by(scenario, gcam.consumer) %>%
      mutate(adj_factor = approx_fun(year, adj_factor, rule = 2)) %>%
      ungroup()

    L244.ShellConductance_bld_gcamusa_IEF <- L244.ShellConductance_bld_gcamusa %>%
      inner_join(ShellConductanceMult_IEF, by = c("gcam.consumer", "year")) %>%
      mutate(shell.conductance = shell.conductance * adj_factor)

    L244.ShellConductance_bld_gcamusa_IEF_StatedPolicies <- filter(L244.ShellConductance_bld_gcamusa_IEF, scenario == "Stated Policies") %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])
    L244.ShellConductance_bld_gcamusa_IEF_MidTech <- filter(L244.ShellConductance_bld_gcamusa_IEF, scenario == "MidTech") %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])
    L244.ShellConductance_bld_gcamusa_IEF_HiTech <- filter(L244.ShellConductance_bld_gcamusa_IEF, scenario == "HiTech") %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])
    L244.ShellConductance_bld_gcamusa_IEF_Breakthru <- filter(L244.ShellConductance_bld_gcamusa_IEF, scenario == "Breakthru") %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])

    # 6. Subsector share-weight interpolation rules
    SubsectorInterpTo_IEF <- filter(bld_tech_scenarios, variable %in% c("from.year", "to.value", "to.year", "interpolation.function")) %>%
      filter(is.na(technology)) %>%
      select(scenario, supplysector, subsector, variable, value, rule_number) %>%
      spread(key = variable, value = value) %>%
      mutate(apply.to = "share-weight",
             from.year = as.numeric(from.year),
             to.value = as.numeric(to.value),
             to.year = as.integer(to.year)) %>%
      left_join(select(L244.SubsectorLogit_bld_gcamusa, region, supplysector, subsector),
                by = c("supplysector", "subsector")) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], scenario))

    L244.SubsectorInterpTo_IEF_StatedPolicies <- filter(SubsectorInterpTo_IEF, scenario == "Stated Policies")
    L244.SubsectorInterpTo_IEF_MidTech <- filter(SubsectorInterpTo_IEF, scenario == "MidTech")
    L244.SubsectorInterpTo_IEF_HiTech <- filter(SubsectorInterpTo_IEF, scenario == "HiTech")
    L244.SubsectorInterpTo_IEF_Breakthru <- filter(SubsectorInterpTo_IEF, scenario == "Breakthru")



    # Produce outputs
    create_xml("building_IEF_StatedPolicies_USA.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_IEF_StatedPolicies, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_IEF_StatedPolicies, "SubsectorLogit") %>%
      add_xml_data(L244.SubsectorInterpTo_IEF_StatedPolicies, "SubsectorInterpTo") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_IEF_StatedPolicies, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_gcamusa_IEF_StatedPolicies, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_gcamusa_IEF_StatedPolicies, "GlobalTechEff") %>%
      add_xml_data(L244.ShellConductance_bld_gcamusa_IEF_StatedPolicies, "ShellConductance") %>%
      add_xml_data(L244.GlobalTechInterpTo_IEF_StatedPolicies, "GlobalTechInterpTo") %>%
      add_precursors("gcam-usa/bld_tech_scenarios",
                     "L244.Supplysector_bld_gcamusa",
                     "L244.SubsectorLogit_bld_gcamusa",
                     "L244.GlobalTechShrwt_bld_gcamusa",
                     "L244.GlobalTechEff_bld",
                     "L244.GlobalTechCost_bld_gcamusa",
                     "L244.ShellConductance_bld_gcamusa") ->
      building_IEF_StatedPolicies_USA.xml

    create_xml("building_IEF_MidTech_USA.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_IEF_MidTech, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_IEF_MidTech, "SubsectorLogit") %>%
      add_xml_data(L244.SubsectorInterpTo_IEF_MidTech, "SubsectorInterpTo") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_IEF_MidTech, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_gcamusa_IEF_MidTech, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_gcamusa_IEF_MidTech, "GlobalTechEff") %>%
      add_xml_data(L244.ShellConductance_bld_gcamusa_IEF_MidTech, "ShellConductance") %>%
      add_xml_data(L244.GlobalTechInterpTo_IEF_MidTech, "GlobalTechInterpTo") %>%
      same_precursors_as(building_IEF_StatedPolicies_USA.xml) ->
      building_IEF_MidTech_USA.xml

    create_xml("building_IEF_HiTech_USA.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_IEF_HiTech, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_IEF_HiTech, "SubsectorLogit") %>%
      add_xml_data(L244.SubsectorInterpTo_IEF_HiTech, "SubsectorInterpTo") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_IEF_HiTech, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_gcamusa_IEF_HiTech, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_gcamusa_IEF_HiTech, "GlobalTechEff") %>%
      add_xml_data(L244.ShellConductance_bld_gcamusa_IEF_HiTech, "ShellConductance") %>%
      add_xml_data(L244.GlobalTechInterpTo_IEF_HiTech, "GlobalTechInterpTo") %>%
      same_precursors_as(building_IEF_StatedPolicies_USA.xml) ->
      building_IEF_HiTech_USA.xml

    create_xml("building_IEF_Breakthru_USA.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_IEF_Breakthru, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_IEF_Breakthru, "SubsectorLogit") %>%
      add_xml_data(L244.SubsectorInterpTo_IEF_Breakthru, "SubsectorInterpTo") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_IEF_Breakthru, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_gcamusa_IEF_Breakthru, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_gcamusa_IEF_Breakthru, "GlobalTechEff") %>%
      add_xml_data(L244.ShellConductance_bld_gcamusa_IEF_Breakthru, "ShellConductance") %>%
      add_xml_data(L244.GlobalTechInterpTo_IEF_Breakthru, "GlobalTechInterpTo") %>%
      same_precursors_as(building_IEF_StatedPolicies_USA.xml) ->
      building_IEF_Breakthru_USA.xml

    return_data(building_IEF_StatedPolicies_USA.xml,
                building_IEF_MidTech_USA.xml,
                building_IEF_HiTech_USA.xml,
                building_IEF_Breakthru_USA.xml)
  } else {
    stop("Unknown command")
  }
}

