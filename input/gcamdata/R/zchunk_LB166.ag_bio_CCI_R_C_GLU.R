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
             "L112.bio_YieldRate_R_Y_GLU",
             FILE = "aglu/ISI-MIP/ISI_MIP_2030_pdssat_rcp2p6_ir",
             FILE = "aglu/ISI-MIP/ISI_MIP_2030_pdssat_rcp2p6_rf",
             FILE = "aglu/ISI-MIP/ISI_MIP_2030_pdssat_rcp8p5_ir",
             FILE = "aglu/ISI-MIP/ISI_MIP_2030_pdssat_rcp8p5_rf",
             FILE = "aglu/ISI-MIP/ISI_MIP_2085_pdssat_rcp2p6_ir",
             FILE = "aglu/ISI-MIP/ISI_MIP_2085_pdssat_rcp2p6_rf",
             FILE = "aglu/ISI-MIP/ISI_MIP_2085_pdssat_rcp8p5_ir",
             FILE = "aglu/ISI-MIP/ISI_MIP_2085_pdssat_rcp8p5_rf"
             ))
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
    ISI_MIP_2030_pdssat_rcp2p6_ir <- get_data(all_data,"aglu/ISI-MIP/ISI_MIP_2030_pdssat_rcp2p6_ir")
    ISI_MIP_2030_pdssat_rcp2p6_rf <- get_data(all_data,"aglu/ISI-MIP/ISI_MIP_2030_pdssat_rcp2p6_rf")
    ISI_MIP_2030_pdssat_rcp8p5_ir <- get_data(all_data,"aglu/ISI-MIP/ISI_MIP_2030_pdssat_rcp8p5_ir")
    ISI_MIP_2030_pdssat_rcp8p5_rf <- get_data(all_data,"aglu/ISI-MIP/ISI_MIP_2030_pdssat_rcp8p5_rf")
    ISI_MIP_2085_pdssat_rcp2p6_ir <- get_data(all_data,"aglu/ISI-MIP/ISI_MIP_2085_pdssat_rcp2p6_ir")
    ISI_MIP_2085_pdssat_rcp2p6_rf <- get_data(all_data,"aglu/ISI-MIP/ISI_MIP_2085_pdssat_rcp2p6_rf")
    ISI_MIP_2085_pdssat_rcp8p5_ir <- get_data(all_data,"aglu/ISI-MIP/ISI_MIP_2085_pdssat_rcp8p5_ir")
    ISI_MIP_2085_pdssat_rcp8p5_rf <- get_data(all_data,"aglu/ISI-MIP/ISI_MIP_2085_pdssat_rcp8p5_rf")

    ISI_MIP_2030_pdssat_rcp2p6_ir$other = "2030_irr"
    ISI_MIP_2030_pdssat_rcp2p6_rf$other = "2030_rfd"
    ISI_MIP_2030_pdssat_rcp8p5_ir$other = "2030_irr"
    ISI_MIP_2030_pdssat_rcp8p5_rf$other = "2030_rfd"
    ISI_MIP_2085_pdssat_rcp2p6_ir$other = "2085_irr"
    ISI_MIP_2085_pdssat_rcp2p6_rf$other = "2085_rfd"
    ISI_MIP_2085_pdssat_rcp8p5_ir$other = "2085_irr"
    ISI_MIP_2085_pdssat_rcp8p5_rf$other = "2085_rfd"


    ISI_MIP_2030_pdssat_rcp2p6_ir<-ISI_MIP_2030_pdssat_rcp2p6_ir %>% select(other,everything())
    ISI_MIP_2030_pdssat_rcp2p6_rf<-ISI_MIP_2030_pdssat_rcp2p6_rf %>% select(other,everything())
    ISI_MIP_2030_pdssat_rcp8p5_ir<-ISI_MIP_2030_pdssat_rcp8p5_ir %>% select(other,everything())
    ISI_MIP_2030_pdssat_rcp8p5_rf<-ISI_MIP_2030_pdssat_rcp8p5_rf %>% select(other,everything())
    ISI_MIP_2085_pdssat_rcp2p6_ir<-ISI_MIP_2085_pdssat_rcp2p6_ir %>% select(other,everything())
    ISI_MIP_2085_pdssat_rcp2p6_rf<-ISI_MIP_2085_pdssat_rcp2p6_rf %>% select(other,everything())
    ISI_MIP_2085_pdssat_rcp8p5_ir<-ISI_MIP_2085_pdssat_rcp8p5_ir %>% select(other,everything())
    ISI_MIP_2085_pdssat_rcp8p5_rf<-ISI_MIP_2085_pdssat_rcp8p5_rf %>% select(other,everything())

    ISI_MIP_df <- bind_rows(ISI_MIP_2030_pdssat_rcp2p6_ir,ISI_MIP_2030_pdssat_rcp2p6_rf,ISI_MIP_2030_pdssat_rcp8p5_ir,
                            ISI_MIP_2030_pdssat_rcp8p5_rf,ISI_MIP_2085_pdssat_rcp2p6_ir,ISI_MIP_2085_pdssat_rcp2p6_rf,
                            ISI_MIP_2085_pdssat_rcp8p5_ir,ISI_MIP_2085_pdssat_rcp8p5_rf)

    ISI_MIP_df<-separate(ISI_MIP_df,crop, c("crop_model", "climate_model","rcp","co2_fertilization","crop_name"),sep = "_")
    ISI_MIP_df<-separate(ISI_MIP_df,other, c("year", "irr_level"),sep = "_")

    GrowthRate2030<-ISI_MIP_df %>% filter(year==2030)
    selNum<-unlist(lapply(GrowthRate2030,is.numeric))
    GrowthRate2030<-GrowthRate2030[,selNum]
    YieldRatio2030<-(1+GrowthRate2030)^(2030-2010)

    GrowthRate2085<-ISI_MIP_df %>% filter(year==2085)
    selNum<-unlist(lapply(GrowthRate2085,is.numeric))
    GrowthRate2085<-GrowthRate2085[,selNum]
    YieldRatio2085<-(1+GrowthRate2085)^(2085-2010)

    YieldRatio <-ISI_MIP_df %>% select(-crop_model,-climate_model,-co2_fertilization)
    selNum<-unlist(lapply(YieldRatio,is.numeric))

    YieldRatio[which(YieldRatio$year=='2030'),selNum]<-YieldRatio2030
    YieldRatio[which(YieldRatio$year=='2085'),selNum]<-YieldRatio2085

    long_YieldRatio <- YieldRatio %>%
      gather(iso,yield_ratio,-year,-irr_level,-rcp,-crop_name) %>%
      mutate(iso = tolower(iso))
    Wide_YieldRatio <- long_YieldRatio %>% spread(year,yield_ratio)

    final_yieldRatio <- Wide_YieldRatio
    final_yieldRatio$`2010` <- 1
    addCol <- setdiff(seq(2010,2100,5),c(2010,2030,2085))
    addCol <- as.character(addCol)
    final_yieldRatio[addCol]<-NA
    final_yieldRatio<-final_yieldRatio[,c(1:4,7:10,5,11:20,6,21:23)]
    yearseq <-seq(2010,2100,5)

    for(i in 1:nrow(final_yieldRatio)){
      temp <- final_yieldRatio[i,5:23]
      tempIntp <- approx_fun(yearseq, as.numeric(temp), rule = 2)
      final_yieldRatio[i,5:23] <- tempIntp
    }

# At this point we apply the yield ratios in each year to the irrigated and rainfed production volumes from the base year
# This sequence first merges in the irrigated and rainfed production by country, crop, and basin, adding the necessary
    # identifier column. PDSSAT impacts crop is joined in from the main crop mapping file, and then the join can be performed
    L166.ag_Prod_t_ctry_crop_irr <- mutate(L151.ag_irrProd_t_ctry_crop, irr_level = "irr") %>%
      rename(Prod = irrProd) %>%
      bind_rows(mutate(L151.ag_rfdProd_t_ctry_crop, irr_level = "rfd") %>%
                  rename(Prod = rfdProd)) %>%
      inner_join(select(FAO_ag_items_PRODSTAT, GTAP_crop, pdssat_crop),
                               by = "GTAP_crop") %>%
      drop_na(pdssat_crop)

    # Note - using left_join so that the data table can expand by the number of RCP emissions scenarios
    L166.ag_Prod_t_ctry_crop_irr_Y <- L166.ag_Prod_t_ctry_crop_irr %>%
      left_join(final_yieldRatio,
                by = c("iso", pdssat_crop = "crop_name", "irr_level"))




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
