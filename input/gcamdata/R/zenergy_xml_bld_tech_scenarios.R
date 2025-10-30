# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_bld_tech_scenarios_xml
#'
#' Construct XML data structure for \code{building_USA_USAreg.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_IEF_StatedPolicies_USAreg.xml}. \code{building_IEF_MidTech_USAreg.xml}. \code{building_IEF_HiTech_USAreg.xml}.
#'  \code{building_IEF_Breakthru_USAreg.xml}. The corresponding file in the
#' original data system was \code{batch_building_USA_USAreg.xml} (energy XML).
module_energy_bld_tech_scenarios_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/bld_tech_scenarios",
             "L244.ShellConductance_bld",
             "L244.Supplysector_bld",
             "L244.SubsectorLogit_bld",
             "L244.GlobalTechCost_bld",
             "L244.GlobalTechEff_bld",
             "L244.GlobalTechShrwt_bld"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_IEF_StatedPolicies_USAreg.xml",
             XML = "building_IEF_MidTech_USAreg.xml",
             XML = "building_IEF_HiTech_USAreg.xml",
             XML = "building_IEF_Breakthru_USAreg.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    bld_tech_scenarios <- get_data(all_data, "gcam-usa/bld_tech_scenarios")
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld", strip_attributes = TRUE)
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld", strip_attributes = TRUE)
    L244.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld", strip_attributes = TRUE)
    L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld", strip_attributes = TRUE)
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld", strip_attributes = TRUE)
    L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld", strip_attributes = TRUE)

    # ===================================================

    # Define the scenario differentiation year (model time period)
    IEF_divergence_year <- 2025

    # Go through the scenarios table variable-by-variable and scenario-by-scenario
    # 1: Supplysector logit exponents
    SupplysectorLogit_IEF <- filter(bld_tech_scenarios,
                                       is.na(subsector),
                                       is.na(technology),
                                       variable == "logit.exponent") %>%
      select(scenario, base_supplysector = supplysector, logit.exponent = value) %>%
      mutate(logit.exponent = as.numeric(logit.exponent))

    L244.Supplysector_bld_usa <- L244.Supplysector_bld %>%
      filter(region == "USA") %>%
      mutate(base_supplysector = sub(" TradBio_d[0-9]{1,2}$| modern_d[0-9]{1,2}$| coal_d[0-9]{1,2}$", "", supplysector))

    L244.Supplysector_bld_IEF <- SupplysectorLogit_IEF %>%
      left_join(select(L244.Supplysector_bld_usa, -logit.exponent),
                by = "base_supplysector") %>%
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
      select(scenario, base_supplysector = supplysector, subsector, logit.exponent = value) %>%
      mutate(logit.exponent = as.numeric(logit.exponent))

    L244.SubsectorLogit_bld_det <- L244.SubsectorLogit_bld %>%
      filter(region == "USA") %>%
      mutate(base_supplysector = sub(" TradBio_d[0-9]{1,2}$| modern_d[0-9]{1,2}$| coal_d[0-9]{1,2}$", "", supplysector))

    L244.SubsectorLogit_bld_IEF <- SubsectorLogit_IEF %>%
      left_join(select(L244.SubsectorLogit_bld_det, -logit.exponent),
                by = c("base_supplysector", "subsector")) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], logit.type, scenario))

    L244.SubsectorLogit_bld_IEF_StatedPolicies <- filter(L244.SubsectorLogit_bld_IEF, scenario == "Stated Policies")
    L244.SubsectorLogit_bld_IEF_MidTech <- filter(L244.SubsectorLogit_bld_IEF, scenario == "MidTech")
    L244.SubsectorLogit_bld_IEF_HiTech <- filter(L244.SubsectorLogit_bld_IEF, scenario == "HiTech")
    L244.SubsectorLogit_bld_IEF_Breakthru <- filter(L244.SubsectorLogit_bld_IEF, scenario == "Breakthru")

    # 3. Technology share-weights
    Shrwt_IEF <- filter(bld_tech_scenarios,
                           !is.na(technology),
                           variable == "share.weight") %>%
      select(scenario, base_supplysector = supplysector, subsector.name = subsector, technology, share.weight = value) %>%
      mutate(share.weight = as.numeric(share.weight))

    L244.GlobalTechShrwt_bld <- L244.GlobalTechShrwt_bld %>%
      mutate(base_supplysector = sub(" TradBio_d[0-9]{1,2}$| modern_d[0-9]{1,2}$| coal_d[0-9]{1,2}$", "", sector.name))

    L244.GlobalTechShrwt_bld_IEF <- Shrwt_IEF %>%
      left_join(select(L244.GlobalTechShrwt_bld, -share.weight),
                by = c("base_supplysector", "subsector.name", "technology")) %>%
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
      select(scenario, base_supplysector = supplysector, subsector.name = subsector, technology, variable, value, rule_number) %>%
      left_join(distinct(select(L244.GlobalTechShrwt_bld, base_supplysector, sector.name, subsector.name, technology)),
                by = c("base_supplysector", "subsector.name", "technology")) %>%
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
      select(scenario, base_supplysector = supplysector, subsector.name = subsector, technology, adj_factor)

    L244.GlobalTechCost_bld <- L244.GlobalTechCost_bld %>%
      mutate(base_supplysector = sub(" TradBio_d[0-9]{1,2}$| modern_d[0-9]{1,2}$| coal_d[0-9]{1,2}$", "", sector.name))

    L244.GlobalTechCost_bld_IEF <- Costs_IEF %>%
      filter(!is.na(technology)) %>%
      left_join(L244.GlobalTechCost_bld,
                by = c("base_supplysector", "subsector.name", "technology")) %>%
      mutate(input.cost = input.cost * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechCost"]], scenario))

    Cost_tmp_subsector <- Costs_IEF %>%
      filter(is.na(technology)) %>%
      select(-technology) %>%
      left_join(L244.GlobalTechCost_bld,
                by = c("base_supplysector", "subsector.name")) %>%
      mutate(input.cost = input.cost * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechCost"]], scenario))

    L244.GlobalTechCost_bld_IEF <- bind_rows(L244.GlobalTechCost_bld_IEF, Cost_tmp_subsector)

    L244.GlobalTechCost_bld_IEF_StatedPolicies <- filter(L244.GlobalTechCost_bld_IEF, scenario == "Stated Policies")
    L244.GlobalTechCost_bld_IEF_MidTech <- filter(L244.GlobalTechCost_bld_IEF, scenario == "MidTech")
    L244.GlobalTechCost_bld_IEF_HiTech <- filter(L244.GlobalTechCost_bld_IEF, scenario == "HiTech")
    L244.GlobalTechCost_bld_IEF_Breakthru <- filter(L244.GlobalTechCost_bld_IEF, scenario == "Breakthru")

    # 5. Technology efficiencies
    Efficiencies_IEF <- filter(bld_tech_scenarios,
                                  !is.na(adj_factor),
                                  variable == "efficiency") %>%
      select(scenario, base_supplysector = supplysector, subsector.name = subsector, technology, adj_factor)

    L244.GlobalTechEff_bld <- L244.GlobalTechEff_bld %>%
      mutate(base_supplysector = sub(" TradBio_d[0-9]{1,2}$| modern_d[0-9]{1,2}$| coal_d[0-9]{1,2}$", "", sector.name))

    L244.GlobalTechEff_bld_IEF <- Efficiencies_IEF %>%
      filter(!is.na(technology)) %>%
      left_join(L244.GlobalTechEff_bld,
                by = c("base_supplysector", "subsector.name", "technology")) %>%
      mutate(efficiency = efficiency * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechEff"]], scenario))

    # Apply adjustment factor to all technologies within a subsector, where individual technologies aren't specified
    Efficiency_tmp <- Efficiencies_IEF %>%
      filter(is.na(technology)) %>%
      select(-technology) %>%
      left_join(L244.GlobalTechEff_bld,
                by = c("base_supplysector", "subsector.name")) %>%
      mutate(efficiency = efficiency * adj_factor) %>%
      filter(year >= IEF_divergence_year) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechEff"]], scenario))

    L244.GlobalTechEff_bld_IEF <- bind_rows(L244.GlobalTechEff_bld_IEF, Efficiency_tmp)

    L244.GlobalTechEff_bld_IEF_StatedPolicies <- filter(L244.GlobalTechEff_bld_IEF, scenario == "Stated Policies")
    L244.GlobalTechEff_bld_IEF_MidTech <- filter(L244.GlobalTechEff_bld_IEF, scenario == "MidTech")
    L244.GlobalTechEff_bld_IEF_HiTech <- filter(L244.GlobalTechEff_bld_IEF, scenario == "HiTech")
    L244.GlobalTechEff_bld_IEF_Breakthru <- filter(L244.GlobalTechEff_bld_IEF, scenario == "Breakthru")

    # 5b. Shell conductance (shell efficiency)
    # Expand the adjustment factor to all analysis years, but interpolate linearly from 1 in 2020
    # to the factor in 2050 in order to account for the stock effects
    IEF_shell_years <- seq(2020, 2050, 5)
    ShellConductanceMult_IEF <- filter(bld_tech_scenarios,
                                  !is.na(adj_factor),
                                  variable == "shell.conductance") %>%
      select(scenario, base.gcam.consumer = gcam.consumer, adj_factor) %>%
      mutate(year = max(IEF_shell_years)) %>%
      complete(nesting(scenario, base.gcam.consumer), year = IEF_shell_years) %>%
      mutate(adj_factor = if_else(year == min(IEF_shell_years), 1, adj_factor)) %>%
      group_by(scenario, base.gcam.consumer) %>%
      mutate(adj_factor = approx_fun(year, adj_factor, rule = 2)) %>%
      ungroup()

    L244.ShellConductance_bld_IEF <- L244.ShellConductance_bld %>%
      filter(region == "USA") %>%
      mutate(base.gcam.consumer = sub("_d[0-9]{1,2}$", "", gcam.consumer)) %>%
      inner_join(ShellConductanceMult_IEF, by = c("base.gcam.consumer", "year")) %>%
      mutate(shell.conductance = shell.conductance * adj_factor)

    L244.ShellConductance_bld_IEF_StatedPolicies <- filter(L244.ShellConductance_bld_IEF, scenario == "Stated Policies") %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])
    L244.ShellConductance_bld_IEF_MidTech <- filter(L244.ShellConductance_bld_IEF, scenario == "MidTech") %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])
    L244.ShellConductance_bld_IEF_HiTech <- filter(L244.ShellConductance_bld_IEF, scenario == "HiTech") %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])
    L244.ShellConductance_bld_IEF_Breakthru <- filter(L244.ShellConductance_bld_IEF, scenario == "Breakthru") %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])

    # 6. Subsector share-weight interpolation rules
    SubsectorInterpTo_IEF <- filter(bld_tech_scenarios, variable %in% c("from.year", "to.value", "to.year", "interpolation.function")) %>%
      filter(is.na(technology)) %>%
      select(scenario, base_supplysector = supplysector, subsector, variable, value, rule_number) %>%
      left_join(distinct(select(L244.GlobalTechShrwt_bld, base_supplysector, sector.name, subsector.name)),
                by = c("base_supplysector", "subsector" = "subsector.name")) %>%
      rename(supplysector = sector.name) %>%
      spread(key = variable, value = value) %>%
      mutate(region = "USA",
             apply.to = "share-weight",
             from.year = as.numeric(from.year),
             to.value = as.numeric(to.value),
             to.year = as.integer(to.year)) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], scenario))

    L244.SubsectorInterpTo_IEF_StatedPolicies <- filter(SubsectorInterpTo_IEF, scenario == "Stated Policies")
    L244.SubsectorInterpTo_IEF_MidTech <- filter(SubsectorInterpTo_IEF, scenario == "MidTech")
    L244.SubsectorInterpTo_IEF_HiTech <- filter(SubsectorInterpTo_IEF, scenario == "HiTech")
    L244.SubsectorInterpTo_IEF_Breakthru <- filter(SubsectorInterpTo_IEF, scenario == "Breakthru")


    # Produce outputs
    create_xml("building_IEF_StatedPolicies_USAreg.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_IEF_StatedPolicies, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_IEF_StatedPolicies, "SubsectorLogit") %>%
      add_xml_data(L244.SubsectorInterpTo_IEF_StatedPolicies, "SubsectorInterpTo") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_IEF_StatedPolicies, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_IEF_StatedPolicies, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_IEF_StatedPolicies, "GlobalTechEff") %>%
      add_xml_data(L244.ShellConductance_bld_IEF_StatedPolicies, "ShellConductance") %>%
      add_xml_data(L244.GlobalTechInterpTo_IEF_StatedPolicies, "GlobalTechInterpTo") %>%
      add_precursors("gcam-usa/bld_tech_scenarios",
                     "L244.Supplysector_bld",
                     "L244.SubsectorLogit_bld",
                     "L244.GlobalTechCost_bld",
                     "L244.GlobalTechEff_bld",
                     "L244.ShellConductance_bld") ->
      building_IEF_StatedPolicies_USAreg.xml

    create_xml("building_IEF_MidTech_USAreg.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_IEF_MidTech, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_IEF_MidTech, "SubsectorLogit") %>%
      add_xml_data(L244.SubsectorInterpTo_IEF_MidTech, "SubsectorInterpTo") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_IEF_MidTech, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_IEF_MidTech, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_IEF_MidTech, "GlobalTechEff") %>%
      add_xml_data(L244.ShellConductance_bld_IEF_MidTech, "ShellConductance") %>%
      add_xml_data(L244.GlobalTechInterpTo_IEF_MidTech, "GlobalTechInterpTo") %>%
      same_precursors_as(building_IEF_StatedPolicies_USAreg.xml) ->
      building_IEF_MidTech_USAreg.xml

    create_xml("building_IEF_HiTech_USAreg.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_IEF_HiTech, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_IEF_HiTech, "SubsectorLogit") %>%
      add_xml_data(L244.SubsectorInterpTo_IEF_HiTech, "SubsectorInterpTo") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_IEF_HiTech, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_IEF_HiTech, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_IEF_HiTech, "GlobalTechEff") %>%
      add_xml_data(L244.ShellConductance_bld_IEF_HiTech, "ShellConductance") %>%
      add_xml_data(L244.GlobalTechInterpTo_IEF_HiTech, "GlobalTechInterpTo") %>%
      same_precursors_as(building_IEF_StatedPolicies_USAreg.xml) ->
      building_IEF_HiTech_USAreg.xml

    create_xml("building_IEF_Breakthru_USAreg.xml") %>%
      add_logit_tables_xml(L244.Supplysector_bld_IEF_Breakthru, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_IEF_Breakthru, "SubsectorLogit") %>%
      add_xml_data(L244.SubsectorInterpTo_IEF_Breakthru, "SubsectorInterpTo") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_IEF_Breakthru, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_IEF_Breakthru, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechEff_bld_IEF_Breakthru, "GlobalTechEff") %>%
      add_xml_data(L244.ShellConductance_bld_IEF_Breakthru, "ShellConductance") %>%
      add_xml_data(L244.GlobalTechInterpTo_IEF_Breakthru, "GlobalTechInterpTo") %>%
      same_precursors_as(building_IEF_StatedPolicies_USAreg.xml) ->
      building_IEF_Breakthru_USAreg.xml

    return_data(building_IEF_StatedPolicies_USAreg.xml,
                building_IEF_MidTech_USAreg.xml,
                building_IEF_HiTech_USAreg.xml,
                building_IEF_Breakthru_USAreg.xml)
  } else {
    stop("Unknown command")
  }
}

