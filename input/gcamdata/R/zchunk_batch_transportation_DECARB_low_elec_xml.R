# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_transportation_UCD_CORE_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).
module_energy_batch_transportation_DECARB_low_elec_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.


  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.Supplysector_trn_low_elec",
             "L254.FinalEnergyKeyword_trn_low_elec",
             "L254.tranSubsectorLogit_low_elec",
             "L254.tranSubsectorShrwtFllt_low_elec",
             "L254.tranSubsectorInterp_low_elec",
             "L254.tranSubsectorSpeed_low_elec",
             "L254.tranSubsectorSpeed_passthru_low_elec",
             "L254.tranSubsectorSpeed_noVOTT_low_elec",
             "L254.tranSubsectorSpeed_nonmotor_low_elec",
             "L254.tranSubsectorVOTT_low_elec",
             "L254.tranSubsectorFuelPref_low_elec",
             "L254.StubTranTech_low_elec",
             "L254.StubTech_passthru_low_elec",
             "L254.StubTech_nonmotor_low_elec",
             "L254.GlobalTechShrwt_passthru_low_elec",
             "L254.GlobalTechShrwt_nonmotor_low_elec",
             "L254.GlobalTechCoef_passthru_low_elec",
             "L254.GlobalRenewTech_nonmotor_low_elec",
             "L254.GlobalTranTechInterp_low_elec",
             "L254.GlobalTranTechShrwt_low_elec",
             "L254.GlobalTranTechSCurve_low_elec",
             "L254.GlobalTranTechProfitShutdown_low_elec",
             "L254.StubTranTechCalInput_low_elec",
             "L254.StubTranTechLoadFactor_low_elec",
             "L254.StubTranTechCost_low_elec",
             "L254.StubTranTechCoef_low_elec",
             "L254.StubTechCalInput_passthru_low_elec",
             "L254.StubTechProd_nonmotor_low_elec",
             "L254.PerCapitaBased_trn_low_elec",
             "L254.PriceElasticity_trn_low_elec",
             "L254.IncomeElasticity_trn_low_elec",
             "L254.BaseService_trn_low_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    #xml_files<- c("transportation_UCD_CORE.xml","transportation_UCD_SSP1.xml","transportation_UCD_SSP3.xml","transportation_UCD_SSP5.xml","transportation_UCD_highEV.xml")
    xml_files<- c("transportation_DECARB_low_elec.xml")
    names(xml_files) <- rep("XML", length(xml_files))
    return(xml_files)
  } else if(command == driver.MAKE) {

    ## silence package check.
    sce <- year <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.tranSubsectorSpeed_low_elec <- get_data(all_data, "L254.tranSubsectorSpeed_low_elec")
    L254.StubTranTech_low_elec <- get_data(all_data, "L254.StubTranTech_low_elec")
    L254.StubTranTechLoadFactor_low_elec <- get_data(all_data, "L254.StubTranTechLoadFactor_low_elec")
    L254.StubTranTechCost_low_elec <- get_data(all_data, "L254.StubTranTechCost_low_elec")

    L254.Supplysector_trn_low_elec <- get_data(all_data, "L254.Supplysector_trn_low_elec")
    L254.FinalEnergyKeyword_trn_low_elec <- get_data(all_data, "L254.FinalEnergyKeyword_trn_low_elec")
    L254.tranSubsectorLogit_low_elec <- get_data(all_data, "L254.tranSubsectorLogit_low_elec")
    L254.tranSubsectorShrwtFllt_low_elec <- get_data(all_data, "L254.tranSubsectorShrwtFllt_low_elec")
    L254.tranSubsectorInterp_low_elec <- get_data(all_data, "L254.tranSubsectorInterp_low_elec")

    L254.tranSubsectorSpeed_passthru_low_elec <- get_data(all_data, "L254.tranSubsectorSpeed_passthru_low_elec")
    L254.tranSubsectorSpeed_noVOTT_low_elec <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT_low_elec")
    L254.tranSubsectorSpeed_nonmotor_low_elec <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor_low_elec")
    L254.tranSubsectorVOTT_low_elec <- get_data(all_data, "L254.tranSubsectorVOTT_low_elec")
    L254.tranSubsectorFuelPref_low_elec <- get_data(all_data, "L254.tranSubsectorFuelPref_low_elec")

    L254.StubTech_passthru_low_elec <- get_data(all_data, "L254.StubTech_passthru_low_elec")
    L254.StubTech_nonmotor_low_elec <- get_data(all_data, "L254.StubTech_nonmotor_low_elec")
    L254.GlobalTechShrwt_passthru_low_elec<- get_data(all_data, "L254.GlobalTechShrwt_passthru_low_elec")
    L254.GlobalTechShrwt_nonmotor_low_elec <- get_data(all_data, "L254.GlobalTechShrwt_nonmotor_low_elec")
    L254.GlobalTechCoef_passthru_low_elec <- get_data(all_data, "L254.GlobalTechCoef_passthru_low_elec")
    L254.GlobalRenewTech_nonmotor_low_elec <- get_data(all_data, "L254.GlobalRenewTech_nonmotor_low_elec")
    L254.GlobalTranTechInterp_low_elec <- get_data(all_data, "L254.GlobalTranTechInterp_low_elec")
    L254.GlobalTranTechShrwt_low_elec <- get_data(all_data, "L254.GlobalTranTechShrwt_low_elec")
    L254.GlobalTranTechSCurve_low_elec <- get_data(all_data, "L254.GlobalTranTechSCurve_low_elec")
    L254.GlobalTranTechProfitShutdown_low_elec <- get_data(all_data, "L254.GlobalTranTechProfitShutdown_low_elec")
    L254.StubTranTechCalInput_low_elec <- get_data(all_data, "L254.StubTranTechCalInput_low_elec")


    L254.StubTranTechCoef_low_elec <- get_data(all_data, "L254.StubTranTechCoef_low_elec")
    L254.StubTechCalInput_passthru_low_elec <- get_data(all_data, "L254.StubTechCalInput_passthru_low_elec")
    L254.StubTechProd_nonmotor_low_elec <- get_data(all_data, "L254.StubTechProd_nonmotor_low_elec")
    L254.PerCapitaBased_trn_low_elec <- get_data(all_data, "L254.PerCapitaBased_trn_low_elec")
    L254.PriceElasticity_trn_low_elec <- get_data(all_data, "L254.PriceElasticity_trn_low_elec")
    L254.IncomeElasticity_trn_low_elec <- get_data(all_data, "L254.IncomeElasticity_trn_low_elec")
    L254.BaseService_trn_low_elec <- get_data(all_data, "L254.BaseService_trn_low_elec")


    # ===================================================

    # Produce outputs
    # Because `return_data` gets the name of the object from what's actually given in the call,
    # we need to assign xml_tmp to a correctly-named variable in the current environment
    # transportation_UCD_CORE.xml <- transportation_UCD_SSP1.xml <- transportation_UCD_SSP2.xml <-
    #   transportation_UCD_SSP3.xml <- transportation_UCD_SSP5.xml <- transportation_UCD_CORE_highEV.xml <- NULL  # silence package check notes
    transportation_UCD_CORE.xml <- transportation_UCD_SSP1.xml <- transportation_UCD_SSP2.xml <-
      transportation_UCD_SSP3.xml <- transportation_UCD_SSP5.xml <- NULL  # silence package check notes

    ret_data <- c()
    curr_env <- environment()

    #for (i in c("CORE","SSP1","SSP3","SSP5", "highEV")){
    for (i in c("CORE")){
      xml_name <- paste0("transportation_DECARB_low_elec.xml")
      #Read SSP specific data
      L254.tranSubsectorSpeed_SSP_low_elec <- L254.tranSubsectorSpeed_low_elec %>% filter(sce== i)
      L254.StubTranTech_SSP_low_elec <- L254.StubTranTech_low_elec %>% filter(sce== i)
      #kbn 2020-03-26 We have energy demand assumptions only for SSP1. So get that data for SSP1. For the other SSPs, keep
      #data from the CORE.
      if (i=="SSP1"){
      #VOTT and Demand data
      L254.tranSubsectorSpeed_passthru_SSP_low_elec <- L254.tranSubsectorSpeed_passthru_low_elec %>% filter(sce==i)
      L254.tranSubsectorVOTT_SSP_low_elec<- L254.tranSubsectorVOTT_low_elec %>% filter(sce==i)
      L254.tranSubsectorFuelPref_SSP_low_elec<-L254.tranSubsectorFuelPref_low_elec %>% filter(sce==i)
      L254.PerCapitaBased_trn_SSP_low_elec<- L254.PerCapitaBased_trn_low_elec %>% filter(sce==i)
      L254.PriceElasticity_trn_SSP_low_elec <- L254.PriceElasticity_trn_low_elec %>%  filter(sce==i)
      L254.IncomeElasticity_trn_SSP_low_elec <- L254.IncomeElasticity_trn_low_elec %>% filter(sce==i)}else{
        L254.tranSubsectorSpeed_passthru_SSP_low_elec <- L254.tranSubsectorSpeed_passthru_low_elec %>% filter(sce=="CORE")
        L254.tranSubsectorVOTT_SSP_low_elec<- L254.tranSubsectorVOTT_low_elec %>% filter(sce=="CORE")
        L254.tranSubsectorFuelPref_SSP_low_elec<-L254.tranSubsectorFuelPref_low_elec %>% filter(sce=="CORE")
        L254.PerCapitaBased_trn_SSP_low_elec <- L254.PerCapitaBased_trn_low_elec %>% filter(sce=="CORE")
        L254.PriceElasticity_trn_SSP_low_elec <- L254.PriceElasticity_trn_low_elec %>% filter(sce=="CORE")
        L254.IncomeElasticity_trn_SSP_low_elec <- L254.IncomeElasticity_trn_low_elec %>% filter(sce=="CORE")
      }



      #kbn 2020-02-11 For the SSPs, we want to bring in values such as co-efficients, load factors and costs after the base year. This is because we are
      # feeding the model outputs from the CORE in the base year, so having SSP values for these variables in the base year would lead to a calibration error
      # i.e. mismatch between calibrated output and actual.

      L254.StubTranTechLoadFactor_SSP_low_elec <- L254.StubTranTechLoadFactor_low_elec %>% filter(sce== i)
      if (i != "CORE"){L254.StubTranTechLoadFactor_SSP_low_elec<-L254.StubTranTechLoadFactor_low_elec %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}


      L254.StubTranTechCost_SSP_low_elec <- L254.StubTranTechCost_low_elec %>%  filter(sce== i)
      if (i != "CORE"){L254.StubTranTechCost_SSP_low_elec<-L254.StubTranTechCost_low_elec %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTranTechCoef_SSP_low_elec <- L254.StubTranTechCoef_low_elec %>%  filter(sce== i)

      if (i != "CORE"){L254.StubTranTechCoef_SSP_low_elec<-L254.StubTranTechCoef_low_elec %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTech_passthru_SSP_low_elec <- L254.StubTech_passthru_low_elec %>% filter(sce==i)
      L254.StubTech_nonmotor_SSP_low_elec <- L254.StubTech_nonmotor_low_elec %>% filter(sce==i)
      L254.Supplysector_trn_SSP_low_elec  <- L254.Supplysector_trn_low_elec %>% filter(sce==i)
      L254.FinalEnergyKeyword_trn_SSP_low_elec <- L254.FinalEnergyKeyword_trn_low_elec %>% filter(sce==i)
      L254.tranSubsectorLogit_SSP_low_elec <- L254.tranSubsectorLogit_low_elec %>% filter(sce==i)
      #L254.tranSubsectorShrwt_SSP <- L254.tranSubsectorShrwt %>%  filter(sce ==i)
      L254.tranSubsectorShrwtFllt_SSP_low_elec <- L254.tranSubsectorShrwtFllt_low_elec %>%  filter(sce ==i)
      L254.tranSubsectorInterp_SSP_low_elec <- L254.tranSubsectorInterp_low_elec %>%  filter(sce ==i)
      L254.tranSubsectorFuelPref_SSP_low_elec <- L254.tranSubsectorFuelPref_low_elec %>%  filter(sce ==i)
      L254.StubTranTechCalInput_SSP_low_elec <-  L254.StubTranTechCalInput_low_elec %>% filter(sce ==i)
      L254.GlobalTranTechInterp_SSP_low_elec <- L254.GlobalTranTechInterp_low_elec %>% filter(sce==i)
      L254.GlobalTranTechShrwt_SSP_low_elec <- L254.GlobalTranTechShrwt_low_elec %>%  filter(sce==i)
      if (i != "CORE"){L254.StubTranTechCalInput_SSP_low_elec<-L254.StubTranTechCalInput_low_elec %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.BaseService_trn_SSP_low_elec <- L254.BaseService_trn_low_elec %>% filter(sce =="CORE")


      #Create xmls
      create_xml(xml_name) %>%
        add_logit_tables_xml(L254.Supplysector_trn_SSP_low_elec, "Supplysector") %>%
        add_xml_data(L254.FinalEnergyKeyword_trn_SSP_low_elec, "FinalEnergyKeyword") %>%
        add_logit_tables_xml(L254.tranSubsectorLogit_SSP_low_elec, "tranSubsectorLogit", "tranSubsector") %>%
        add_xml_data(L254.tranSubsectorShrwtFllt_SSP_low_elec, "tranSubsectorShrwtFllt") %>%
        add_xml_data(L254.tranSubsectorInterp_SSP_low_elec, "tranSubsectorInterp") %>%
        add_xml_data(L254.tranSubsectorSpeed_SSP_low_elec, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_passthru_SSP_low_elec, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_noVOTT_low_elec, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_nonmotor_low_elec, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorVOTT_SSP_low_elec, "tranSubsectorVOTT") %>%
        add_xml_data(L254.tranSubsectorFuelPref_SSP_low_elec, "tranSubsectorFuelPref") %>%
        add_xml_data(L254.StubTranTech_SSP_low_elec, "StubTranTech") %>%
        add_xml_data(L254.StubTech_passthru_SSP_low_elec, "StubTranTech") %>%
        add_xml_data(L254.StubTech_nonmotor_SSP_low_elec, "StubTranTech") %>%
        add_xml_data(L254.GlobalTechShrwt_passthru_low_elec, "GlobalTechShrwt") %>%
        add_xml_data(L254.GlobalTechShrwt_nonmotor_low_elec, "GlobalTechShrwt") %>%
        add_xml_data(L254.GlobalTechCoef_passthru_low_elec, "GlobalTechCoef") %>%
        add_xml_data(L254.GlobalRenewTech_nonmotor_low_elec, "GlobalRenewTech") %>%
        add_xml_data(L254.GlobalTranTechInterp_SSP_low_elec, "GlobalTranTechInterp") %>%
        add_xml_data(L254.GlobalTranTechShrwt_SSP_low_elec, "GlobalTranTechShrwt") %>%
        add_xml_data(L254.GlobalTranTechSCurve_low_elec, "GlobalTranTechSCurve") %>%
        add_xml_data(L254.GlobalTranTechProfitShutdown_low_elec, "GlobalTranTechProfitShutdown") %>%
        add_xml_data(L254.StubTranTechCalInput_SSP_low_elec, "StubTranTechCalInput") %>%
        add_xml_data(L254.StubTranTechLoadFactor_SSP_low_elec, "StubTranTechLoadFactor") %>%
        add_xml_data(L254.StubTranTechCost_SSP_low_elec, "StubTranTechCost") %>%
        add_xml_data(L254.StubTranTechCoef_SSP_low_elec, "StubTranTechCoef") %>%
        add_xml_data(L254.StubTechCalInput_passthru_low_elec, "StubTranTechCalInput") %>%
        add_xml_data(L254.StubTechProd_nonmotor_low_elec, "StubTranTechProd") %>%
        add_xml_data(L254.PerCapitaBased_trn_SSP_low_elec, "PerCapitaBased") %>%
        add_xml_data(L254.PriceElasticity_trn_SSP_low_elec, "PriceElasticity") %>%
        add_xml_data(L254.IncomeElasticity_trn_SSP_low_elec, "IncomeElasticity") %>%
        add_xml_data(L254.BaseService_trn_SSP_low_elec, "BaseService") %>%
        add_precursors("L254.Supplysector_trn_low_elec",
                       "L254.FinalEnergyKeyword_trn_low_elec",
                       "L254.tranSubsectorLogit_low_elec",
                       "L254.tranSubsectorShrwtFllt_low_elec",
                       "L254.tranSubsectorInterp_low_elec",
                       "L254.tranSubsectorSpeed_low_elec",
                       "L254.tranSubsectorSpeed_passthru_low_elec",
                       "L254.tranSubsectorSpeed_noVOTT_low_elec",
                       "L254.tranSubsectorSpeed_nonmotor_low_elec",
                       "L254.tranSubsectorVOTT_low_elec",
                       "L254.tranSubsectorFuelPref_low_elec",
                       "L254.StubTranTech_low_elec",
                       "L254.StubTech_passthru_low_elec",
                       "L254.StubTech_nonmotor_low_elec",
                       "L254.GlobalTechShrwt_passthru_low_elec",
                       "L254.GlobalTechShrwt_nonmotor_low_elec",
                       "L254.GlobalTechCoef_passthru_low_elec",
                       "L254.GlobalRenewTech_nonmotor_low_elec",
                       "L254.GlobalTranTechInterp_low_elec",
                       "L254.GlobalTranTechShrwt_low_elec",
                       "L254.GlobalTranTechSCurve_low_elec",
                       "L254.GlobalTranTechProfitShutdown_low_elec",
                       "L254.StubTranTechCalInput_low_elec",
                       "L254.StubTranTechLoadFactor_low_elec",
                       "L254.StubTranTechCost_low_elec",
                       "L254.StubTranTechCoef_low_elec",
                       "L254.StubTechCalInput_passthru_low_elec",
                       "L254.StubTechProd_nonmotor_low_elec",
                       "L254.PerCapitaBased_trn_low_elec",
                       "L254.PriceElasticity_trn_low_elec",
                       "L254.IncomeElasticity_trn_low_elec",
                       "L254.BaseService_trn_low_elec")  %>%
                        assign(xml_name, ., envir = curr_env)


      ret_data <- c(ret_data, xml_name)

    }
    #Return all xmls
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()

  } else {
    stop("Unknown command")
  }
}
