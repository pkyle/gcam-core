# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_elec_tech_scenarios_xml
#'
#' Construct XML files to define electric sector technology levels (adv, low).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{wind_adv1.xml}. The corresponding file in the
#' original data system was \code{batch_wind_adv_xml.R} (energy XML).
module_energy_elec_tech_scenarios_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "energy/A23.globalinttech",
      "L1233.globaltech_capital_ATB_adv",
      "L1233.globaltech_capital_ATB_low",
      "L1233.globaltech_OMfixed_ATB_adv",
      "L1233.globaltech_OMfixed_ATB_low",
      "L1233.globaltech_OMvar_ATB_adv",
      "L1233.globaltech_OMvar_ATB_low",
      "L125.nuclear_hydrogen_costs_low",
      "L125.nuclear_hydrogen_costs_adv",
      # for adjusting rooftop_pv for capital tracking purposes
      "L223.StubTechCapFactor_elec")

  MODULE_OUTPUTS <-
    c(XML = "elec_bio_low1.xml",
      XML = "geo_tech_adv1.xml",
      XML = "geo_low1.xml",
      XML = "nuclear_adv1.xml",
      XML = "nuclear_low1.xml",
      XML = "solar_adv1.xml",
      XML = "solar_low1.xml",
      XML = "wind_adv1.xml",
      XML = "wind_low1.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence global package checks
    value <- year <- NULL

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Process data to final formats
    # Compile capital cost assumptions, round, separate intermittent and standard technologies
    L1233.globaltech_capital_ATB_adv %>%
      mutate(scenario = "adv") %>%
      bind_rows(mutate(L1233.globaltech_capital_ATB_low, scenario = "low")) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL)) %>%
      select(c("scenario", LEVEL2_DATA_NAMES[["GlobalTechCapital"]])) ->
      L223.GlobalTechCapital_elec_scen

    # Subset the intermittent technologies using semi_join(A23.globalinttech), and standard techs using anti_join(A23.globalinttech)
    L223.GlobalTechCapital_elec_scen %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector",
                                          "subsector.name" = "subsector",
                                          "technology" = "intermittent.technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechCapital_elec_scen

    L223.GlobalTechCapital_elec_scen %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector",
                                          "subsector.name" = "subsector",
                                          "technology" = "intermittent.technology")) ->
      L223.GlobalTechCapital_elec_scen

    # convert rooftop_pv costs to standard non energy input, for capital tracking purposes
    L223.StubTechCapFactor_elec %>%
      filter(stub.technology == "rooftop_pv") %>%
      repeat_add_columns(tibble(scenario = c("adv", "low"))) %>%
      left_join_error_no_match(L223.GlobalIntTechCapital_elec_scen, by=c("scenario",
                                                                         "supplysector" = "sector.name",
                                                                         "subsector" = "subsector.name",
                                                                         "stub.technology" = "intermittent.technology",
                                                                         "year")) %>%
      mutate(input.cost = capital.overnight * fixed.charge.rate / (capacity.factor * CONV_YEAR_HOURS * CONV_KWH_GJ)) %>%
      select(-capacity.factor, -capital.overnight, -fixed.charge.rate) %>%
      rename(minicam.non.energy.input = input.capital) ->
      L223.StubTechCost_roofpv_scen

    # Separate capital costs of global electricity technologies into individual data tables for each technology
    # adv scenario capital costs
    L223.GlobalTechCapital_geo_adv <- filter(L223.GlobalTechCapital_elec_scen, subsector.name == "geothermal", scenario == "adv")
    L223.GlobalTechCapital_nuc_adv <- filter(L223.GlobalTechCapital_elec_scen, subsector.name == "nuclear", scenario == "adv")
    L223.GlobalTechCapital_sol_adv <- filter(L223.GlobalTechCapital_elec_scen, subsector.name == "solar", scenario == "adv")
    L223.GlobalIntTechCapital_sol_adv <- filter(L223.GlobalIntTechCapital_elec_scen, subsector.name == "solar", scenario == "adv")
    L223.StubTechCost_roofpv_adv <- filter(L223.StubTechCost_roofpv_scen, scenario == "adv")
    L223.GlobalIntTechCapital_wind_adv <- filter(L223.GlobalIntTechCapital_elec_scen, subsector.name == "wind", scenario == "adv")
    L223.GlobalTechCapital_wind_adv <- filter(L223.GlobalTechCapital_elec_scen, subsector.name == "wind", scenario == "adv")

    # low scenario capital costs
    L223.GlobalTechCapital_bio_low <- filter(L223.GlobalTechCapital_elec_scen, subsector.name == "biomass", scenario == "low")
    L223.GlobalTechCapital_geo_low <- filter(L223.GlobalTechCapital_elec_scen, subsector.name == "geothermal", scenario == "low")
    L223.GlobalTechCapital_nuc_low <- filter(L223.GlobalTechCapital_elec_scen, subsector.name == "nuclear", scenario == "low")
    L223.GlobalTechCapital_sol_low <- filter(L223.GlobalTechCapital_elec_scen, subsector.name == "solar", scenario == "low")
    L223.GlobalIntTechCapital_sol_low <- filter(L223.GlobalIntTechCapital_elec_scen, subsector.name == "solar", scenario == "low")
    L223.StubTechCost_roofpv_low <- filter(L223.StubTechCost_roofpv_scen, scenario == "low")
    L223.GlobalIntTechCapital_wind_low <- filter(L223.GlobalIntTechCapital_elec_scen, subsector.name == "wind", scenario == "low")
    L223.GlobalTechCapital_wind_low <- filter(L223.GlobalTechCapital_elec_scen, subsector.name == "wind", scenario == "low")


    # ===================================================

    # Produce outputs
    # Adv scenario XML files
    create_xml("geo_tech_adv1.xml") %>%
      add_xml_data(L223.GlobalTechCapital_geo_adv, "GlobalTechCapital") %>%
      add_precursors("L1233.globaltech_capital_ATB_adv",
                     "L1233.globaltech_OMfixed_ATB_adv",
                     "L1233.globaltech_OMvar_ATB_adv") ->
      geo_tech_adv1.xml

    create_xml("nuclear_adv1.xml") %>%
      add_xml_data(L223.GlobalTechCapital_nuc_adv, "GlobalTechCapital") %>%
      add_xml_data(L125.nuclear_hydrogen_costs_adv, "GlobalTechCost") %>%
      same_precursors_as(geo_tech_adv1.xml) %>%
      add_precursors("L125.nuclear_hydrogen_costs_adv") ->
      nuclear_adv1.xml

    create_xml("solar_adv1.xml") %>%
      add_xml_data(L223.GlobalTechCapital_sol_adv, "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalIntTechCapital_sol_adv, "GlobalIntTechCapital") %>%
      add_xml_data(L223.StubTechCost_roofpv_adv, "StubTechCost") %>%
      same_precursors_as(geo_tech_adv1.xml) %>%
      add_precursors("energy/A23.globalinttech",
                     "L223.StubTechCapFactor_elec") ->
      solar_adv1.xml

    create_xml("wind_adv1.xml") %>%
      add_xml_data(L223.GlobalTechCapital_wind_adv, "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalIntTechCapital_wind_adv, "GlobalIntTechCapital") %>%
      same_precursors_as(geo_tech_adv1.xml) %>%
      add_precursors("energy/A23.globalinttech") ->
      wind_adv1.xml

    # Low scenario XML files
    create_xml("elec_bio_low1.xml") %>%
      add_xml_data(L223.GlobalTechCapital_bio_low, "GlobalTechCapital") %>%
      add_precursors("L1233.globaltech_capital_ATB_low",
                     "L1233.globaltech_OMfixed_ATB_low",
                     "L1233.globaltech_OMvar_ATB_low") ->
      elec_bio_low1.xml

    create_xml("geo_low1.xml") %>%
      add_xml_data(L223.GlobalTechCapital_geo_low, "GlobalTechCapital") %>%
      same_precursors_as(elec_bio_low1.xml) ->
      geo_low1.xml

    create_xml("nuclear_low1.xml") %>%
      add_xml_data(L223.GlobalTechCapital_nuc_low, "GlobalTechCapital") %>%
      add_xml_data(L125.nuclear_hydrogen_costs_low, "GlobalTechCost") %>%
      same_precursors_as(elec_bio_low1.xml) %>%
      add_precursors("L125.nuclear_hydrogen_costs_low") ->
      nuclear_low1.xml

    create_xml("solar_low1.xml") %>%
      add_xml_data(L223.GlobalTechCapital_sol_low, "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalIntTechCapital_sol_low, "GlobalIntTechCapital") %>%
      add_xml_data(L223.StubTechCost_roofpv_low, "StubTechCost") %>%
      same_precursors_as(elec_bio_low1.xml) %>%
      add_precursors("energy/A23.globalinttech",
                     "L223.StubTechCapFactor_elec") ->
      solar_low1.xml

    create_xml("wind_low1.xml") %>%
      add_xml_data(L223.GlobalTechCapital_wind_low, "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalIntTechCapital_wind_low, "GlobalIntTechCapital") %>%
      same_precursors_as(elec_bio_low1.xml) %>%
      add_precursors("energy/A23.globalinttech") ->
      wind_low1.xml


    return_data(geo_tech_adv1.xml,
                nuclear_adv1.xml,
                solar_adv1.xml,
                wind_adv1.xml,
                elec_bio_low1.xml,
                geo_low1.xml,
                nuclear_low1.xml,
                solar_low1.xml,
                wind_low1.xml)
  } else {
    stop("Unknown command")
  }
}
