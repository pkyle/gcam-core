

# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB143.ag_P_IO_R_C_Y_GLU
#'
#' Calculate the P fertilizer input-output coefficients by GCAM region / commodity / year / GLU.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L143.ag_P_IO_cons_R_C_Y_GLU}. No corresponding file in the
#' original data system
#' @details This chunk calculates P fertilizer input-output coefficients
#' by GCAM region / commodity / year / GLU.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join group_by left_join mutate right_join select semi_join summarise
#' @importFrom tidyr complete replace_na
#' @importFrom RcppRoll roll_mean
#' @author RC June 2017
module_aglu_LB143.ag_P_IO_R_C_Y_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             FILE = "aglu/Pinput_crop_ctry",
             "L100.LDS_ag_prod_t",
             "L100.FAO_Fert_Cons_tN",
             "L100.FAO_Fert_Prod_tN",
             "L101.ag_Prod_Mt_R_C_Y_GLU",
             "L101.LDS_ctry_crop_SHARES",
             "L141.ag_Fert_Cons_MtN_ctry_crop",
             "L100.LDS_ag_prod_t",
             "L100.FAO_ag_Prod_t",
             "L101.ag_Prod_Mt_R_C_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L143.ag_P_IO_cons_R_C_Y_GLU")) #-> P i/o coefficients by region/com
  } else if(command == driver.MAKE) {

    country <- totP <- item <- iso <- GTAP_crop <- GCAM_commodity <- year <- total <- value <-
      Pinput <- commodWT <- commodP <- Pin_commod <- prod_share_GLU <- Pin_commod_GLU <- P_cons <-
      P_IO <- GCAM_region_ID <- GLU <- NULL   # silence package checks

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID",strip_attributes =  T)
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT",strip_attributes =  T)
    P_input <- get_data(all_data, "aglu/Pinput_crop_ctry",strip_attributes =  T)
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t",strip_attributes =  T)
    L100.FAO_Fert_Cons_tN <- get_data(all_data, "L100.FAO_Fert_Cons_tN",strip_attributes =  T)
    L100.FAO_Fert_Prod_tN <- get_data(all_data, "L100.FAO_Fert_Prod_tN",strip_attributes =  T)
    L101.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y_GLU",strip_attributes =  T)
    L141.ag_Fert_Cons_MtN_ctry_crop <- get_data(all_data, "L141.ag_Fert_Cons_MtN_ctry_crop",strip_attributes =  T)
    L101.LDS_ctry_crop_SHARES <- get_data(all_data, "L101.LDS_ctry_crop_SHARES",strip_attributes =  T)
    L100.LDS_ag_prod_t<-get_data(all_data, "L100.LDS_ag_prod_t",strip_attributes =  T)
    L100.FAO_ag_Prod_t<-get_data(all_data, "L100.FAO_ag_Prod_t",strip_attributes =  T)
    L101.ag_Prod_Mt_R_C_Y<-get_data(all_data, "L101.ag_Prod_Mt_R_C_Y",strip_attributes =  T)


    # Calculate fertilizer input-output coefficients, scaled so that consumption of fertilizer balance
    # First, downscale fertilizer demands by country and crop to GLU
    # NOTE: Allocate fertilizer consumption to GLUs on the basis of production, not harvested area
    # Calculate agriculture prodcution total by country and crop

    # Calculate fertilizer input-output coefficients, scaled so that consumption of fertilizer balance
    # First, downscale fertilizer demands by country and crop to GLU
    # NOTE: Allocate fertilizer consumption to GLUs on the basis of production, not harvested area
    # Calculate agriculture prodcution total by country and crop


    L100.FAO_ag_Prod_t %>% select(countries,item,year,value) %>%
      full_join(P_input,by=c('countries','item','year'))    %>%
      drop_na(item) %>% #all-NA rows eliminated
      mutate(totP_kg=replace_na(totP_kg,0)) %>%
      left_join(FAO_ag_items_PRODSTAT %>% select(GCAM_commodity, GCAM_subsector, item),by="item") %>%
      mutate(totP_t=totP_kg/1000)->Pin_crop_commod


    Pin_crop_commod %>%
      group_by(GCAM_commodity,GCAM_subsector,countries,year) %>%
      summarize(total=sum(value)) %>%
      ungroup()->Pin_commod_totals

    Pin_commod_totals%>%
      right_join(Pin_crop_commod, by = c("countries",  "year","GCAM_commodity", "GCAM_subsector")) %>%
      mutate(commodWT=value/total,commodP=commodWT*totP_t)->FAO_prodwts

    FAO_prodwts %>% select(GCAM_commodity,GCAM_subsector,countries,item,year,totP_t,commodWT,commodP,value) %>%
      group_by(GCAM_commodity, GCAM_subsector, countries, year) %>%
      summarize(Pin_commod=sum(commodP,na.rm=T),
                totProd=sum(value,na.rm=T)) %>%
      ungroup() -> Pin_commod_country

  # get GLU shares of country production
    Pin_commod_country %>% rename(country_name=countries) %>%
      left_join(iso_GCAM_regID,by='country_name')%>%
      inner_join(L101.LDS_ctry_crop_SHARES,by=c("GCAM_commodity","GCAM_subsector", "iso")) %>%
      mutate(Pin_commod_GLU=Pin_commod*prod_share_GLU)->Pin_commod_ctry_glu

    # weighted avg of country production to get regional production
    Pin_commod_ctry_glu %>%
      group_by(GCAM_commodity,GCAM_subsector,year,GCAM_region_ID,GLU) %>%
      summarize(reg_prod=sum(totProd)) %>%
      ungroup()->tot_prods

    tot_prods %>% left_join(Pin_commod_ctry_glu, by = c("GCAM_commodity", "GCAM_subsector", "year", "GCAM_region_ID", "GLU")) %>%
      mutate(P_prodwt=if_else(reg_prod==0,0,Pin_commod_GLU*totProd/reg_prod)) %>%
      group_by(GCAM_commodity,GCAM_subsector,GCAM_region_ID,GLU,year) %>%
      summarize(P_region=sum(P_prodwt,na.rm=T)) %>%
      ungroup()->Pin_commod_region_glu


    Pin_commod_region_glu %>%
      group_by(GCAM_commodity,GCAM_subsector,GCAM_region_ID)%>%
      mutate(Pin_avg=roll_mean(P_region,n=5,fill=NA))%>%
      ungroup() %>%
      filter(year %in% 1973:2012)-> Pin_rollavg #some series start in 1961, others in 1971, need same time window for all.


    # Check to make sure that the fertilizer inputs do not blink in and out (if present in any year, need to be present in all years)
    Pin_rollavg %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      summarise(value = sum(Pin_avg)) %>%                 # Get the total of all years
      ungroup() %>%
      filter(value != 0) %>%                            # Filter the region/commodity/GLU that are not completely missing for all years
      select(-value) %>%
      unique() ->   FertIOcheck

    Pin_rollavg %>% filter(year>1991) %>%
      # Filter the observations with the selected region/commodity/GLU combinations
      semi_join(FertIOcheck, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU"))  %>%
      select(GCAM_region_ID,GCAM_commodity,GCAM_subsector,GLU,year,Pin_avg)    %>%
      filter(Pin_avg==0) %>%
      select(-Pin_avg, -year) %>%
      unique() -> blinks

    # with full data set (corrected for rolling average ends) 1973-2012,
    # 821 commodity/region/GLU combinations are 'blinking'
    # Restricting to data after 1991 (post-USSR) only 349 combinations are "blinks" -

  #eliminate "blinks"
    Pin_rollavg %>% filter(year>1991) %>%
      anti_join(blinks, by = c("GCAM_commodity", "GCAM_subsector", "GCAM_region_ID", "GLU")) %>%
      mutate(P_Mt=Pin_avg*1e-6) %>%
      select(-Pin_avg)->
        L143.ag_P_cons_Mt_R_C_Y_GLU

    ## checks
    # L143.ag_P_cons_Mt_R_C_Y_GLU %>% group_by(GCAM_commodity,GCAM_region_ID,GLU) %>%
    #   semi_join(FertIOcheck) %>%
    #   summarize(rows=n()) %>% # make sure all have same number of rows ie all years are present
    #   filter(rows<max(rows)) # (empty)
    #
    # L143.ag_P_cons_Mt_R_C_Y_GLU %>% semi_join(FertIOcheck) %>% filter(P_Mt==0) #(empty)


    #from consumption total to I/O
    L143.ag_P_cons_Mt_R_C_Y_GLU  %>%
      left_join(L101.ag_Prod_Mt_R_C_Y_GLU, by = c("GCAM_commodity", "GCAM_subsector", "year", "GCAM_region_ID", "GLU")) %>%
      drop_na(value) %>%
      mutate(P_IO_raw = if_else(is.na(P_Mt/value), 0, P_Mt/value)) %>%
      select(GCAM_commodity, GCAM_subsector, GCAM_region_ID,GLU,year,P_IO_raw)->
      P_IO_raw

    P_IO_raw %>%
      group_by(GCAM_commodity, GCAM_subsector) %>%
      summarize(P90=quantile(P_IO_raw,probs=0.9))->commodLIM

    P_IO_raw %>% left_join(commodLIM, by=c("GCAM_commodity", "GCAM_subsector")) %>%
      mutate(P_IO=if_else(P_IO_raw>P90,P90,P_IO_raw)) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector,GLU, year, P_IO) ->
      L143.ag_P_IO_cons_R_C_Y_GLU

    # Produce outputs


    L143.ag_P_IO_cons_R_C_Y_GLU %>%
      add_title("Phosphorous input-output coefficients by GCAM region / crop / year / GLU") %>%
      add_units("Unitless IO") %>%
      add_comments("P demands are downscaled to GLU based on agriculture production share in each country") %>%
      add_comments("Input-output coefficients for each crop are first calculated as fertilizer demands divided by agriculture production in the base year") %>%
      add_legacy_name("none") %>%
      add_precursors( "common/iso_GCAM_regID",
                      "aglu/FAO/FAO_ag_items_PRODSTAT",
                      "aglu/Pinput_crop_ctry",
                      "L100.LDS_ag_prod_t",
                      "L100.FAO_Fert_Cons_tN",
                      "L100.FAO_Fert_Prod_tN",
                      "L101.ag_Prod_Mt_R_C_Y_GLU",
                      "L101.LDS_ctry_crop_SHARES",
                      "L141.ag_Fert_Cons_MtN_ctry_crop",
                      "L100.LDS_ag_prod_t",
                      "L100.FAO_ag_Prod_t",
                      "L101.ag_Prod_Mt_R_C_Y") ->
      L143.ag_P_IO_cons_R_C_Y_GLU

    return_data(L143.ag_P_IO_cons_R_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
