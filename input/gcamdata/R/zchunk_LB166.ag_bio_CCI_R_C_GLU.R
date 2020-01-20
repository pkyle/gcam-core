#' module_aglu_LB166.ag_bio_CCI_R_C_GLU
#'
#' Calculate base year bioenergy yields by GCAM region and GLU
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L166.YieldRatio_R_C_Y_GLU_irr_CCIscen}.
#' @details Calculate global average yields for each FAO crop in the base year;
#' calculate each region / zone / crop's comparative yield; compute bioenergy yields as
#' this region/zone-specific index multiplied by a base yield.
#' @references Wullschleger, S.D., E.B. Davis, M.E. Borsuk, C.A. Gunderson, and L.R. Lynd. 2010.
#' Biomass production in switchgrass across the United States: database description and determinants
#' of yield. Agronomy Journal 102: 1158-1168. doi:10.2134/agronj2010.0087.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL June 2017
module_aglu_LB166.ag_bio_CCI_R_C_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             "L151.ag_irrProd_t_ctry_crop",
             "L151.ag_rfdProd_t_ctry_crop",
             "L112.ag_YieldRate_R_C_Y_GLU",
             "L112.bio_YieldRate_R_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L166.YieldRatio_R_C_Y_GLU_irr_CCIscen"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    value <- Prod_t <- HA <-  NULL # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    L151.ag_irrProd_t_ctry_crop <- get_data(all_data, "L151.ag_irrProd_t_ctry_crop")
    L151.ag_rfdProd_t_ctry_crop <- get_data(all_data, "L151.ag_rfdProd_t_ctry_crop")
    L112.ag_YieldRate_R_C_Y_GLU <- get_data(all_data, "L112.ag_YieldRate_R_C_Y_GLU")
    L112.bio_YieldRate_R_Y_GLU <- get_data(all_data, "L112.bio_YieldRate_R_Y_GLU")

    # Processing code goes here
    # Load AgMIP climate-yield impact files
    ISI_MIP_2030_pdssat_rcp2p6_ir <- get_data(all_data,
"aglu/ISI-MIP/ISI-MIP_growth_rates_30-year_average_2030s_vs_2000s_pDSSAT_MIROC-ESM-CHEM_rcp2p6_co2_ir_for_agclim50iii_national_all_crops")
    ISI_MIP_2030_pdssat_rcp2p6_rf <- get_data(all_data,
"aglu/ISI-MIP/ISI-MIP_growth_rates_30-year_average_2030s_vs_2000s_pDSSAT_MIROC-ESM-CHEM_rcp2p6_co2_rf_for_agclim50iii_national_all_crops")
    ISI_MIP_2030_pdssat_rcp8p5_ir <- get_data(all_data,
"aglu/ISI-MIP/ISI-MIP_growth_rates_30-year_average_2030s_vs_2000s_pDSSAT_MIROC-ESM-CHEM_rcp8p5_co2_ir_for_agclim50iii_national_all_crops")
    ISI_MIP_2030_pdssat_rcp8p5_rf <- get_data(all_data,
"aglu/ISI-MIP/ISI-MIP_growth_rates_30-year_average_2030s_vs_2000s_pDSSAT_MIROC-ESM-CHEM_rcp8p5_co2_rf_for_agclim50iii_national_all_crops")
    ISI_MIP_2085_pdssat_rcp2p6_ir <- get_data(all_data,
"aglu/ISI-MIP/ISI-MIP_growth_rates_30-year_average_2085s_vs_2000s_pDSSAT_MIROC-ESM-CHEM_rcp2p6_co2_ir_for_agclim50iii_national_all_crops")
    ISI_MIP_2085_pdssat_rcp2p6_rf <- get_data(all_data,
"aglu/ISI-MIP/ISI-MIP_growth_rates_30-year_average_2085s_vs_2000s_pDSSAT_MIROC-ESM-CHEM_rcp2p6_co2_rf_for_agclim50iii_national_all_crops")
    ISI_MIP_2085_pdssat_rcp8p5_ir <- get_data(all_data,
"aglu/ISI-MIP/ISI-MIP_growth_rates_30-year_average_2085s_vs_2000s_pDSSAT_MIROC-ESM-CHEM_rcp8p5_co2_ir_for_agclim50iii_national_all_crops")
    ISI_MIP_2085_pdssat_rcp8p5_rf <- get_data(all_data,
"aglu/ISI-MIP/ISI-MIP_growth_rates_30-year_average_2085s_vs_2000s_pDSSAT_MIROC-ESM-CHEM_rcp8p5_co2_rf_for_agclim50iii_national_all_crops")






    # Produce outputs
    L166.YieldRatio_R_C_Y_GLU_irr_CCIscen %>%
      add_title("Area-weighted climate change impacts on yields by region / crop / year/ basin / impact scenario") %>%
      add_units(" Unitless multiplier") %>%
      add_comments("Estimated from ISI-MIP data for AgMIP2020 project") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop",
                     "L112.ag_YieldRate_R_C_Y_GLU",
                     "L112.bio_YieldRate_R_Y_GLU") ->
      L166.YieldRatio_R_C_Y_GLU_irr_CCIscen

    return_data(L166.YieldRatio_R_C_Y_GLU_irr_CCIscen)
  } else {
    stop("Unknown command")
  }
}
