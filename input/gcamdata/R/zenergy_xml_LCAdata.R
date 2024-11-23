# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LCAdata_xml
#'
#' Construct XML data structure for \code{en_transformation.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation.xml.R} (energy XML).
module_energy_LCAdata_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A81.emissions_remapping",
             FILE = "energy/A81.LCA_inputs",
             "L201.ghg_res",
             "L252.ResMAC_fos",
             "L252.ResMAC_fos_phaseInTime",
             "L252.ResMAC_fos_tc_average",
             "L2112.AGRBio",
             "L252.AgMAC"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "LCAdata.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A81.emissions_remapping <- get_data(all_data, "emissions/A81.emissions_remapping")
    A81.LCA_inputs <- get_data(all_data, "energy/A81.LCA_inputs")
    L201.ghg_res <- get_data(all_data, "L201.ghg_res")
    L252.ResMAC_fos <- get_data(all_data, "L252.ResMAC_fos")
    L252.ResMAC_fos_phaseInTime <- get_data(all_data, "L252.ResMAC_fos_phaseInTime")
    L252.ResMAC_fos_tc_average <- get_data(all_data, "L252.ResMAC_fos_tc_average")
    L2112.AGRBio <- get_data(all_data, "L2112.AGRBio")
    L252.AgMAC <- get_data(all_data, "L252.AgMAC")

    # ===================================================
    # resources: set the usa coefs to a small number in resources, and move the coefs to the assigned sector
    L281.ghg_upstream_usa <- filter(L201.ghg_res, region == "USA") %>%
      inner_join(A81.emissions_remapping,
                 by = c(resource = "from.id1", subresource = "from.id2", technology = "from.id3")) %>%
      select(region,
             supplysector = to.supplysector,
             subsector = to.subsector,
             stub.technology = to.technology,
             year, Non.CO2, emiss.coeff = emiss.coef)

    L281.ghg_res_usa <- filter(L201.ghg_res, region == "USA") %>%
      semi_join(A81.emissions_remapping,
                by = c(resource = "from.id1", subresource = "from.id2", technology = "from.id3")) %>%
      mutate(emiss.coef = pmin(emiss.coef, 1e-7))

    # resource MAC curves: move to assigned sectors/technologies
    L281.MAC_upstream_usa <- filter(L252.ResMAC_fos, region == "USA") %>%
      inner_join(A81.emissions_remapping,
                 by = c(resource = "from.id1", subresource = "from.id2", technology = "from.id3")) %>%
      select(region,
             supplysector = to.supplysector,
             subsector = to.subsector,
             stub.technology = to.technology,
             year, Non.CO2, mac.control, tax, mac.reduction, market.name)

    L281.MAC_upstream_usa <- filter(L252.ResMAC_fos, region == "USA") %>%
      inner_join(A81.emissions_remapping,
                 by = c(resource = "from.id1", subresource = "from.id2", technology = "from.id3")) %>%
      select(region,
             supplysector = to.supplysector,
             subsector = to.subsector,
             stub.technology = to.technology,
             year, Non.CO2, mac.control, tax, mac.reduction, market.name)

    L281.MACPhaseIn_upstream_usa <- filter(L252.ResMAC_fos_phaseInTime, region == "USA") %>%
      inner_join(A81.emissions_remapping,
                 by = c(resource = "from.id1", subresource = "from.id2", technology = "from.id3")) %>%
      select(region,
             supplysector = to.supplysector,
             subsector = to.subsector,
             stub.technology = to.technology,
             year, Non.CO2, mac.control, mac.phase.in.time)

    L281.MACTC_upstream_usa <- filter(L252.ResMAC_fos_tc_average, region == "USA") %>%
      inner_join(A81.emissions_remapping,
                 by = c(resource = "from.id1", subresource = "from.id2", technology = "from.id3")) %>%
      select(region,
             supplysector = to.supplysector,
             subsector = to.subsector,
             stub.technology = to.technology,
             year, Non.CO2, mac.control, tech.change.year, tech.change)

    # agricultural nonco2 emissions and mac
    L281.OutputEmissCoeffAg_usa <- filter(L2112.AGRBio, region == "USA") %>%
      inner_join(A81.emissions_remapping,
                 by = c(AgSupplySector = "from.id1", AgSupplySubsector = "from.id2", AgProductionTechnology = "from.id3")) %>%
      select(region,
             AgSupplySector = to.supplysector,
             AgSupplySubsector = to.subsector,
             AgProductionTechnology = to.technology,
             year, Non.CO2, emiss.coef = bio_N2O_coef)

    L281.AgMAC_usa <- filter(L252.AgMAC, region == "USA") %>%
      inner_join(A81.emissions_remapping,
                 by = c(AgSupplySector = "from.id1", AgSupplySubsector = "from.id2", AgProductionTechnology = "from.id3")) %>%
      select(region,
             AgSupplySector = to.supplysector,
             AgSupplySubsector = to.subsector,
             AgProductionTechnology = to.technology,
             year, Non.CO2, mac.control, tax, mac.reduction, market.name)

    # assign inputs and price-multipliers (where relevant) to technologies throughout the energy system
    lca_years <- tibble(year = MODEL_YEARS[MODEL_YEARS >= unique(A81.LCA_inputs$from.year)])
    L281.StubTechCoef_lca <- A81.LCA_inputs %>%
      repeat_add_columns(lca_years) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    L281.StubTechInputPMult_lca <- A81.LCA_inputs %>%
      filter(!is.na(price.unit.conversion)) %>%
      repeat_add_columns(lca_years) %>%
      select(LEVEL2_DATA_NAMES[["StubTechInputPMult"]])

    # Produce outputs
    create_xml("LCAdata.xml") %>%
      add_xml_data(L281.ghg_upstream_usa, "OutputEmissCoeff") %>%
      add_xml_data(L281.ghg_res_usa, "ResEmissCoef") %>%
      add_xml_data(L281.MAC_upstream_usa, "MAC") %>%
      add_xml_data(L281.MACPhaseIn_upstream_usa, "MACPhaseIn") %>%
      add_xml_data(L281.MACTC_upstream_usa, "MACTC") %>%
      add_xml_data(L281.OutputEmissCoeffAg_usa, "OutputEmissCoeffAg") %>%
      add_xml_data(L281.AgMAC_usa, "AgMAC") %>%
      add_xml_data(L281.StubTechCoef_lca, "StubTechCoef") %>%
      add_xml_data(L281.StubTechInputPMult_lca, "StubTechInputPMult") %>%
      add_precursors("emissions/A81.emissions_remapping",
                     "energy/A81.LCA_inputs",
                     "L201.ghg_res",
                     "L252.ResMAC_fos",
                     "L252.ResMAC_fos_phaseInTime",
                     "L252.ResMAC_fos_tc_average",
                     "L2112.AGRBio",
                     "L252.AgMAC") ->
      LCAdata.xml

    return_data(LCAdata.xml)
  } else {
    stop("Unknown command")
  }
}
