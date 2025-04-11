# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_hydrogen_xml
#'
#' Construct XML data structure for \code{hydrogen.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen.xml}. The corresponding file in the
#' original data system was \code{batch_hydrogen.xml.R} (energy XML).
module_energy_hydrogen_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.Supplysector_h2",
             "L225.SectorUseTrialMarket_h2",
              "L225.SubsectorLogit_h2",
              "L225.SubsectorShrwtFllt_h2",
              "L225.StubTech_h2",
              "L225.StubTechCost_h2",
              "L225.StubTechCost_h2_adv",
              "L225.StubTechCost_h2_lotech",
              "L225.GlobalTechCoef_h2",
              "L225.StubTechCoef_h2",
              "L225.StubTechCoef_h2_adv",
              "L225.StubTechCoef_h2_lotech",
              "L225.GlobalTechCost_h2",
              "L225.GlobalTechTrackCapital_h2",
              "L225.GlobalTechShrwt_h2",
              "L225.PrimaryRenewKeyword_h2",
              "L225.AvgFossilEffKeyword_h2",
              "L225.GlobalTechCapture_h2",
              "L225.GlobalTechInputPMult_h2",
              "L225.GlobalTechProfitShutdown_h2",
              "L225.GlobalTechSCurve_h2",
              "L225.OutputEmissCoeff_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hydrogen.xml",
             XML = "hydrogen_electrolysis_adv.xml",
             XML = "hydrogen_electrolysis_lotech.xml",
             XML = "hydrogen_noBiomass_SP.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L225.Supplysector_h2 <- get_data(all_data, "L225.Supplysector_h2")
    L225.SectorUseTrialMarket_h2 <- get_data(all_data, "L225.SectorUseTrialMarket_h2")
    L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2")
    L225.SubsectorShrwtFllt_h2 <- get_data(all_data, "L225.SubsectorShrwtFllt_h2")
    L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2")
    L225.GlobalTechCoef_h2 <- get_data(all_data, "L225.GlobalTechCoef_h2")
    L225.StubTechCoef_h2 <- get_data(all_data, "L225.StubTechCoef_h2")
    L225.StubTechCoef_h2_adv <- get_data(all_data, "L225.StubTechCoef_h2_adv")
    L225.StubTechCoef_h2_lotech <- get_data(all_data, "L225.StubTechCoef_h2_lotech")
    L225.GlobalTechCost_h2 <- get_data(all_data, "L225.GlobalTechCost_h2")
    L225.GlobalTechTrackCapital_h2 <- get_data(all_data, "L225.GlobalTechTrackCapital_h2")
    L225.GlobalTechShrwt_h2 <- get_data(all_data, "L225.GlobalTechShrwt_h2")
    L225.PrimaryRenewKeyword_h2 <- get_data(all_data, "L225.PrimaryRenewKeyword_h2")
    L225.AvgFossilEffKeyword_h2 <- get_data(all_data, "L225.AvgFossilEffKeyword_h2")
    L225.GlobalTechCapture_h2 <- get_data(all_data, "L225.GlobalTechCapture_h2")
    L225.GlobalTechInputPMult_h2 <- get_data(all_data, "L225.GlobalTechInputPMult_h2")
    L225.GlobalTechProfitShutdown_h2 <- get_data(all_data, "L225.GlobalTechProfitShutdown_h2")
    L225.GlobalTechSCurve_h2 <- get_data(all_data, "L225.GlobalTechSCurve_h2")
    L225.StubTechCost_h2 <- get_data(all_data, "L225.StubTechCost_h2")
    L225.StubTechCost_h2_adv <- get_data(all_data, "L225.StubTechCost_h2_adv")
    L225.StubTechCost_h2_lotech <- get_data(all_data, "L225.StubTechCost_h2_lotech")
    L225.OutputEmissCoeff_h2 <- get_data(all_data, "L225.OutputEmissCoeff_h2")
    # ===================================================
    # YZ 1/28/2025 deactivate biomass in hydrogen production for Stated Policies scenario
    L225.SubsectorShrwtFllt_h2 %>%
      filter(region == 'USA', subsector == "biomass") %>%
      mutate(share.weight = 0)->L225.SubsectorShrwtFllt_h2_SP

    # Produce outputs
    create_xml("hydrogen.xml") %>%
      add_logit_tables_xml(L225.Supplysector_h2, "Supplysector") %>%
      add_xml_data(L225.SectorUseTrialMarket_h2, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L225.SubsectorLogit_h2, "SubsectorLogit") -> hydrogen.xml

    hydrogen.xml <- hydrogen.xml %>%
      add_xml_data(L225.SubsectorShrwtFllt_h2, "SubsectorShrwtFllt") %>%
      add_xml_data(L225.StubTech_h2, "StubTech") %>%
      add_xml_data(L225.StubTechCost_h2, "StubTechCost") %>%
      add_xml_data(L225.StubTechCoef_h2, "StubTechCoef") %>%
      add_xml_data(L225.GlobalTechCoef_h2, "GlobalTechCoef") %>%
      # set non-energy inputs to be read as tracking
      add_node_equiv_xml("input") %>%
      add_xml_data(L225.GlobalTechTrackCapital_h2, "GlobalTechTrackCapital") %>%
      add_xml_data(L225.GlobalTechCost_h2, "GlobalTechCost") %>%
      add_xml_data(L225.GlobalTechShrwt_h2, "GlobalTechShrwt") %>%
      add_xml_data(L225.PrimaryRenewKeyword_h2, "PrimaryRenewKeyword") %>%
      add_xml_data(L225.AvgFossilEffKeyword_h2, "AvgFossilEffKeyword") %>%
      add_xml_data(L225.GlobalTechCapture_h2, "GlobalTechCapture") %>%
      add_xml_data(L225.GlobalTechInputPMult_h2, "GlobalTechInputPMult") %>%
      add_xml_data(L225.GlobalTechSCurve_h2, "GlobalTechSCurve") %>%
      add_xml_data(L225.GlobalTechProfitShutdown_h2, "GlobalTechProfitShutdown") %>%
      add_xml_data(L225.OutputEmissCoeff_h2, "OutputEmissCoeff") %>%
      add_precursors("L225.Supplysector_h2",
                     "L225.SectorUseTrialMarket_h2",
                     "L225.SubsectorLogit_h2",
                     "L225.SubsectorShrwtFllt_h2",
                     "L225.StubTech_h2",
                     "L225.StubTechCost_h2",
                     "L225.StubTechCoef_h2",
                     "L225.GlobalTechCoef_h2",
                     "L225.GlobalTechTrackCapital_h2",
                     "L225.GlobalTechCost_h2",
                     "L225.GlobalTechShrwt_h2",
                     "L225.PrimaryRenewKeyword_h2",
                     "L225.AvgFossilEffKeyword_h2",
                     "L225.GlobalTechCapture_h2",
                     "L225.GlobalTechInputPMult_h2",
                     "L225.GlobalTechSCurve_h2",
                     "L225.GlobalTechProfitShutdown_h2",
                     "L225.OutputEmissCoeff_h2") ->
      hydrogen.xml

    #add-on with high-technology (i.e. low-cost) renewable electrolysis
    create_xml("hydrogen_electrolysis_adv.xml") %>%
      add_xml_data(L225.StubTechCost_h2_adv, "StubTechCost") %>%
      add_xml_data(L225.StubTechCoef_h2_adv, "StubTechCoef") %>%
      add_precursors("L225.StubTechCost_h2_adv", "L225.StubTechCoef_h2_adv") ->
      hydrogen_electrolysis_adv.xml

    #HFTO program goals met for renewable electrolysis
    create_xml("hydrogen_electrolysis_lotech.xml") %>%
      add_xml_data(L225.StubTechCost_h2_lotech, "StubTechCost") %>%
      add_xml_data(L225.StubTechCoef_h2_lotech, "StubTechCoef") %>%
      add_precursors("L225.StubTechCost_h2_lotech", "L225.StubTechCoef_h2_lotech") ->
      hydrogen_electrolysis_lotech.xml

    # YZ 1/28/2025
    create_xml("hydrogen_noBiomass_SP.xml") %>%
      add_xml_data(L225.SubsectorShrwtFllt_h2_SP, "SubsectorShrwtFllt") %>%
      add_precursors("L225.SubsectorShrwtFllt_h2") ->
      hydrogen_noBiomass_SP.xml

    return_data(hydrogen.xml,
                hydrogen_electrolysis_adv.xml,
                hydrogen_electrolysis_lotech.xml,
                hydrogen_noBiomass_SP.xml)
  } else {
    stop("Unknown command")
  }
}
