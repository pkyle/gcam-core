# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_ammonia_scenarios
#'
#' Construct XML data structure for \code{hydrogen.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen.xml}. The corresponding file in the
#' original data system was \code{batch_hydrogen.xml.R} (energy XML).
module_energy_ammonia_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A25.subsector_shrwt_scen",
             FILE = "energy/A322.globaltech_shrwt_scen",
             FILE = "energy/A322.subsector_interp_scen",
             FILE = "energy/A54.globaltranTech_interp_scen",
             FILE = "energy/A54.globaltranTech_shrwt_scen"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "NH3_ship.xml",
             XML = "NGCCS_NH3.xml",
             XML = "elec_NH3.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A25.subsector_shrwt_scen <- get_data(all_data, "energy/A25.subsector_shrwt_scen")
    A322.globaltech_shrwt_scen <- get_data(all_data, "energy/A322.globaltech_shrwt_scen")
    A322.subsector_interp_scen <- get_data(all_data, "energy/A322.subsector_interp_scen")
    A54.globaltranTech_interp_scen <- get_data(all_data, "energy/A54.globaltranTech_interp_scen")
    A54.globaltranTech_shrwt_scen <- get_data(all_data, "energy/A54.globaltranTech_shrwt_scen")

    # ===================================================

    # hydrogen (25) production technology mix is only specified in the scenarios with elec_nh3
    A25.subsector_shrwt_scen %>%
      filter(!is.na(year.fillout),
             scenario == "elec_NH3") %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L225.SubsectorShrwtFllt_elec_NH3

    # ammonia at the technology level is only specified in scenarios with NGCCS NH3 (blue ammonia)
    A322.globaltech_shrwt_scen %>%
      filter(scenario == "NGCCS_NH3") %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(scenario, supplysector, subsector, technology), year = MODEL_YEARS[MODEL_YEARS >= min(year)]) %>%
      arrange(scenario, supplysector, subsector, technology, year) %>%
      group_by(scenario, supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) ->
      L2322.GlobalTechShrwt_NGCCS_NH3

    # Subsector interpolation applies to both the NGCCS_NH3 and elec_NH3 scenarios
    A322.subsector_interp_scen %>%
      filter(scenario == "NGCCS_NH3") %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpFrom"]],
                           GCAM_region_names) ->
      L2322.SubsectorInterpFrom_NGCCS_NH3

    A322.subsector_interp_scen %>%
      filter(scenario == "elec_NH3") %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpFrom"]],
                           GCAM_region_names) ->
      L2322.SubsectorInterpFrom_elec_NH3

    # Global trantech shareweights and interpolation rules apply only to the NH3_ship scenario
    A54.globaltranTech_interp_scen %>%
      filter(scenario == "NH3_ship") %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechInterp"]]) ->
      L254.GlobalTranTechInterp_NH3_ship

    A54.globaltranTech_shrwt_scen %>%
      filter(scenario == "NH3_ship") %>%
      gather_years %>%
      complete(nesting(scenario, supplysector, tranSubsector, tranTechnology), year = MODEL_YEARS[MODEL_YEARS >= min(year)]) %>%
      group_by(scenario, supplysector, tranSubsector, tranTechnology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2),
             share.weight = round(share.weight, energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]]) ->
      L254.GlobalTranTechShrwt_NH3_ship

    # Produce outputs
    create_xml("NH3_ship.xml") %>%
      add_xml_data(L254.GlobalTranTechInterp_NH3_ship, "GlobalTranTechInterp") %>%
      add_xml_data(L254.GlobalTranTechShrwt_NH3_ship, "GlobalTranTechShrwt") %>%
      add_precursors("energy/A54.globaltranTech_interp_scen",
                     "energy/A54.globaltranTech_shrwt_scen") ->
      NH3_ship.xml

    create_xml("NGCCS_NH3.xml") %>%
      add_xml_data(L2322.GlobalTechShrwt_NGCCS_NH3, "GlobalTechShrwt") %>%
      add_xml_data(L2322.SubsectorInterpFrom_NGCCS_NH3, "SubsectorInterpFrom") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A322.globaltech_shrwt_scen",
                     "energy/A322.subsector_interp_scen") ->
      NGCCS_NH3.xml

    create_xml("elec_NH3.xml") %>%
      add_xml_data(L225.SubsectorShrwtFllt_elec_NH3, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterpFrom_elec_NH3, "SubsectorInterpFrom") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A25.subsector_shrwt_scen",
                     "energy/A322.subsector_interp_scen") ->
      elec_NH3.xml

    return_data(NH3_ship.xml,
                NGCCS_NH3.xml,
                elec_NH3.xml)
  } else {
    stop("Unknown command")
  }
}
