# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_transportation_USA_DECARB_high_elec_xml
#'
#' Construct XML data structure for \code{transportation_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_USA.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_transportation_USA_DECARB_high_elec_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.DeleteSupplysector_USAtrn_high_elec",
             "L254.DeleteFinalDemand_USAtrn_high_elec",
             "L254.Supplysector_trn_USA_high_elec",
             "L254.FinalEnergyKeyword_trn_USA_high_elec",
             "L254.tranSubsectorLogit_USA_high_elec",
             "L254.tranSubsectorShrwtFllt_USA_high_elec",
             "L254.tranSubsectorInterp_USA_high_elec",
             "L254.tranSubsectorSpeed_USA_high_elec",
             "L254.tranSubsectorSpeed_passthru_USA_high_elec",
             "L254.tranSubsectorSpeed_noVOTT_USA_high_elec",
             "L254.tranSubsectorSpeed_nonmotor_USA_high_elec",
             "L254.tranSubsectorVOTT_USA_high_elec",
             "L254.tranSubsectorFuelPref_USA_high_elec",
             "L254.StubTranTech_USA_high_elec",
             "L254.StubTranTech_passthru_USA_high_elec",
             "L254.StubTranTech_nonmotor_USA_high_elec",
             "L254.StubTranTechLoadFactor_USA_high_elec",
             "L254.StubTranTechCost_USA_high_elec",
             "L254.StubTranTechCoef_USA_high_elec",
             "L254.PerCapitaBased_trn_USA_high_elec",
             "L254.PriceElasticity_trn_USA_high_elec",
             "L254.IncomeElasticity_trn_USA_high_elec",
             "L254.StubTranTechCalInput_USA_high_elec",
             "L254.StubTranTechProd_nonmotor_USA_high_elec",
             "L254.StubTranTechCalInput_passthru_USA_high_elec",
             "L254.BaseService_trn_USA_high_elec"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transportation_USA_DECARB_high_elec.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.DeleteSupplysector_USAtrn_high_elec <- get_data(all_data, "L254.DeleteSupplysector_USAtrn_high_elec")

    L254.DeleteFinalDemand_USAtrn_high_elec <- get_data(all_data, "L254.DeleteFinalDemand_USAtrn_high_elec")
    L254.Supplysector_trn_USA_high_elec <- get_data(all_data, "L254.Supplysector_trn_USA_high_elec")

    L254.FinalEnergyKeyword_trn_USA_high_elec <- get_data(all_data, "L254.FinalEnergyKeyword_trn_USA_high_elec")
    L254.tranSubsectorLogit_USA_high_elec <- get_data(all_data, "L254.tranSubsectorLogit_USA_high_elec")
    L254.tranSubsectorShrwtFllt_USA_high_elec <- get_data(all_data, "L254.tranSubsectorShrwtFllt_USA_high_elec")
    L254.tranSubsectorInterp_USA_high_elec <- get_data(all_data, "L254.tranSubsectorInterp_USA_high_elec")
    L254.tranSubsectorSpeed_USA_high_elec <- get_data(all_data, "L254.tranSubsectorSpeed_USA_high_elec")
    L254.tranSubsectorSpeed_passthru_USA_high_elec <- get_data(all_data, "L254.tranSubsectorSpeed_passthru_USA_high_elec")
    L254.tranSubsectorSpeed_noVOTT_USA_high_elec <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT_USA_high_elec")
    L254.tranSubsectorSpeed_nonmotor_USA_high_elec <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor_USA_high_elec")
    L254.tranSubsectorVOTT_USA_high_elec <- get_data(all_data, "L254.tranSubsectorVOTT_USA_high_elec")
    L254.StubTranTech_USA_high_elec <- get_data(all_data, "L254.StubTranTech_USA_high_elec")
    L254.StubTranTech_passthru_USA_high_elec <- get_data(all_data, "L254.StubTranTech_passthru_USA_high_elec")
    L254.StubTranTech_nonmotor_USA_high_elec <- get_data(all_data, "L254.StubTranTech_nonmotor_USA_high_elec")
    L254.StubTranTechLoadFactor_USA_high_elec <- get_data(all_data, "L254.StubTranTechLoadFactor_USA_high_elec")
    L254.StubTranTechCost_USA_high_elec <- get_data(all_data, "L254.StubTranTechCost_USA_high_elec")
    L254.StubTranTechCoef_USA_high_elec <- get_data(all_data, "L254.StubTranTechCoef_USA_high_elec")
    L254.PerCapitaBased_trn_USA_high_elec <- get_data(all_data, "L254.PerCapitaBased_trn_USA_high_elec")
    L254.PriceElasticity_trn_USA_high_elec <- get_data(all_data, "L254.PriceElasticity_trn_USA_high_elec")
    L254.IncomeElasticity_trn_USA_high_elec <- get_data(all_data, "L254.IncomeElasticity_trn_USA_high_elec")
    L254.StubTranTechCalInput_USA_high_elec <- get_data(all_data, "L254.StubTranTechCalInput_USA_high_elec")
    L254.StubTranTechProd_nonmotor_USA_high_elec <- get_data(all_data, "L254.StubTranTechProd_nonmotor_USA_high_elec")
    L254.StubTranTechCalInput_passthru_USA_high_elec <- get_data(all_data, "L254.StubTranTechCalInput_passthru_USA_high_elec")
    L254.BaseService_trn_USA_high_elec <- get_data(all_data, "L254.BaseService_trn_USA_high_elec")

    # ===================================================

    # Produce outputs
    create_xml("transportation_USA_DECARB_high_elec.xml") %>%
      add_xml_data(L254.DeleteSupplysector_USAtrn_high_elec, "DeleteSupplysector") %>%
      add_xml_data(L254.DeleteFinalDemand_USAtrn_high_elec, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L254.Supplysector_trn_USA_high_elec, "Supplysector") %>%
      add_xml_data(L254.FinalEnergyKeyword_trn_USA_high_elec, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L254.tranSubsectorLogit_USA_high_elec, "tranSubsectorLogit", "tranSubsector") %>%
      add_xml_data(L254.tranSubsectorShrwtFllt_USA_high_elec, "tranSubsectorShrwtFllt") %>%
      add_xml_data(L254.tranSubsectorInterp_USA_high_elec, "tranSubsectorInterp") %>%
      add_xml_data(L254.tranSubsectorSpeed_USA_high_elec, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_passthru_USA_high_elec, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_noVOTT_USA_high_elec, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_nonmotor_USA_high_elec, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorVOTT_USA_high_elec, "tranSubsectorVOTT") %>%
      #add_xml_data(L254.tranSubsectorFuelPref_USA_high_elec, "tranSubsectorFuelPref") %>%
      add_xml_data(L254.StubTranTech_USA_high_elec, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_passthru_USA_high_elec, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_nonmotor_USA_high_elec, "StubTranTech") %>%
      add_xml_data(L254.StubTranTechLoadFactor_USA_high_elec, "StubTranTechLoadFactor") %>%
      add_xml_data(L254.StubTranTechCost_USA_high_elec, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA_high_elec, "StubTranTechCoef") %>%
      add_xml_data(L254.PerCapitaBased_trn_USA_high_elec, "PerCapitaBased") %>%
      add_xml_data(L254.PriceElasticity_trn_USA_high_elec, "PriceElasticity") %>%
      add_xml_data(L254.IncomeElasticity_trn_USA_high_elec, "IncomeElasticity") %>%
      add_xml_data(L254.StubTranTechCalInput_USA_high_elec, "StubTranTechCalInput") %>%
      add_xml_data(L254.StubTranTechProd_nonmotor_USA_high_elec, "StubTranTechProd") %>%
      add_xml_data(L254.StubTranTechCalInput_passthru_USA_high_elec, "StubTranTechCalInput") %>%
      add_xml_data(L254.BaseService_trn_USA_high_elec, "BaseService") %>%
      add_precursors("L254.DeleteSupplysector_USAtrn_high_elec",
                     "L254.DeleteFinalDemand_USAtrn_high_elec",
                     "L254.Supplysector_trn_USA_high_elec",
                     "L254.FinalEnergyKeyword_trn_USA_high_elec",
                     "L254.tranSubsectorLogit_USA_high_elec",
                     "L254.tranSubsectorShrwtFllt_USA_high_elec",
                     "L254.tranSubsectorInterp_USA_high_elec",
                     "L254.tranSubsectorSpeed_USA_high_elec",
                     "L254.tranSubsectorSpeed_passthru_USA_high_elec",
                     "L254.tranSubsectorSpeed_noVOTT_USA_high_elec",
                     "L254.tranSubsectorSpeed_nonmotor_USA_high_elec",
                     "L254.tranSubsectorVOTT_USA_high_elec",
                     "L254.tranSubsectorFuelPref_USA_high_elec",
                     "L254.StubTranTech_USA_high_elec",
                     "L254.StubTranTech_passthru_USA_high_elec",
                     "L254.StubTranTech_nonmotor_USA_high_elec",
                     "L254.StubTranTechLoadFactor_USA_high_elec",
                     "L254.StubTranTechCost_USA_high_elec",
                     "L254.StubTranTechCoef_USA_high_elec",
                     "L254.PerCapitaBased_trn_USA_high_elec",
                     "L254.PriceElasticity_trn_USA_high_elec",
                     "L254.IncomeElasticity_trn_USA_high_elec",
                     "L254.StubTranTechCalInput_USA_high_elec",
                     "L254.StubTranTechProd_nonmotor_USA_high_elec",
                     "L254.StubTranTechCalInput_passthru_USA_high_elec",
                     "L254.BaseService_trn_USA_high_elec") ->
      transportation_USA_DECARB_high_elec.xml

    return_data(transportation_USA_DECARB_high_elec.xml)
  } else {
    stop("Unknown command")
  }
}
