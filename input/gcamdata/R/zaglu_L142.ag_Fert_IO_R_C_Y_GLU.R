# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L142.ag_Fert_IO_R_C_Y_GLU
#'
#' Calculate the adjusted fertilizer production by country / year, fertilizer net exports by GCAM region / year,
#' and fertilizer input-output coefficients by GCAM region / commodity / year / GLU.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.ag_NFert_Prod_MtN_ctry_Y}, \code{L142.ag_NFert_NetExp_MtN_R_Y}, \code{L142.ag_SyntheticNFert_IO_R_C_Y_GLU}. The corresponding file in the
#' original data system was \code{LB142.ag_Fert_IO_R_C_Y_GLU.R} (aglu level1).
#' @details This chunk calculates fertilizer production by country / year (adjusted to global total consumption),
#' fertilizer net exports by GCAM region / year as production minus consumption, and fertilizer input-output coefficients
#' by GCAM region / commodity / year / GLU.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join group_by left_join mutate right_join select semi_join summarise
#' @importFrom tidyr complete replace_na
#' @author RC June 2017
module_aglu_L142.ag_Fert_IO_R_C_Y_GLU <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
      "L100.LDS_ag_prod_t",
      "L100.FAO_Fert_Prod_tN",
      "L100.FAO_Fert_Prod_tP2O5",
      "L101.ag_Prod_Mt_R_C_Y_GLU",
      "L141.ag_Fert_Cons_MtN_ctry_crop_Yh",
      "L141.ag_Fert_Cons_MtP2O5_ctry_crop_Yh")

  MODULE_OUTPUTS <-
    c("L142.ag_NFert_Prod_MtN_ctry_Y",
      "L142.ag_NFert_NetExp_MtN_R_Y",
      "L142.ag_SyntheticNFert_IO_R_C_Y_GLU",
      "L142.ag_PFert_Prod_MtP2O5_R_Y",
      "L142.ag_PFert_NetExp_MtP2O5_R_Y",
      "L142.ag_PFert_IO_R_C_Y_GLU")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    Fert_Cons_MtN <- Fert_Cons_MtN_unscaled <- Fert_IO <- Fert_IO_unscaled <- Prod_share <-
      prod <- cons <- total <- adj <- scaler <- GCAM_commodity <- GCAM_region_ID <- GTAP_crop <-
      GLU <- iso <- value <- year <- GCAM_subsector <- NULL   # silence package checks

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Compile N fertilizer production and consumption by country, and adjust country production so that production and consumption balance globally
    # For consumption, use the data assigned to country and crop, which omits some fertilizer consumption (e.g. Iceland) that
    # could not be matched to production in GCAM
    L141.ag_Fert_Cons_tN_ctry_Yh <- L141.ag_Fert_Cons_MtN_ctry_crop_Yh %>%
      group_by(iso, year) %>%
      summarise(cons = sum(Fert_Cons_MtN) / CONV_T_MT) %>%
      ungroup()

    L100.FAO_Fert_Prod_tN %>%
      select(iso, year, prod = value) %>%
      # Combine with fertilizer consumption, use full_join to keep all observations, such as ones only have consumption
      full_join(L141.ag_Fert_Cons_tN_ctry_Yh, by = c("iso", "year")) %>%
      replace_na(list(prod = 0, cons = 0)) %>%
      group_by(year) %>%
      # Calculate the global total production and consumption
      summarise(prod = sum(prod), cons = sum(cons)) %>%
      ungroup() %>%
      # Calculate the rate to adjust production so that global production equals consumption
      mutate(adj = cons / prod) %>%
      select(year, adj) ->
      L142.ag_Fert_Prod_adj

    L100.FAO_Fert_Prod_tN %>%
      select(iso, year, value) %>%
      left_join_error_no_match(L142.ag_Fert_Prod_adj, by = "year") %>%   # Match in the rates for adjustment
      mutate(value = value * adj,                                        # Adjust production
             value = value * CONV_T_MT) %>%                              # Convert unit of production from tons to million tons of Nitrogen
      select(-adj) ->
      L142.ag_NFert_Prod_MtN_ctry_Y

    # Aggregate N fertilizer adjusted production and consumption to GCAM region level to calculate net exports
    L142.ag_NFert_Prod_MtN_ctry_Y %>%
      rename(prod = value) %>%
      # Combine with fertilizer consumption, use full_join to keep all observations, such as ones only have consumption
      full_join(L141.ag_Fert_Cons_tN_ctry_Yh, by = c("iso", "year")) %>%
      replace_na(list(prod = 0, cons = 0)) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%           # Match in GCAM region ID
      group_by(GCAM_region_ID, year) %>%
      summarise(prod = sum(prod), cons = sum(cons)) %>%                  # Aggregate to region total
      ungroup() %>%                                                      # Ungroup before complete
      mutate(cons = cons * CONV_T_MT,                                    # Convert unit of consumption from tons to million tons of Nitrogen
             value = prod - cons,                                        # Calculate net exports as production minus consumption
             GCAM_commodity = aglu.N_FERT_NAME) %>%                      # Add GCAM commodity category for N fertilizer
      select(-prod) %>%                                                  # Only regional consumption and net exports are needed
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(cons = 0, value = 0)) ->  # Fill in missing regions with 0
      L142.ag_Fert_MtN_R_Y

    # Separate the table for consumption by region / year
    L142.ag_Fert_MtN_R_Y %>%
      select(-value) ->
      L142.ag_Fert_Cons_MtN_R_Y

    # Separate the table for net exports by region / year
    L142.ag_Fert_MtN_R_Y %>%
      select(-cons) ->
      L142.ag_NFert_NetExp_MtN_R_Y

    # Aggregate N fertilizer consumption by country and GTAP crop to GCAM region and GCAM commodity+subsector
    L142.ag_Fert_Cons_MtN_R_C_Yh <- L141.ag_Fert_Cons_MtN_ctry_crop_Yh %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      left_join_error_no_match(distinct(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector)),
                               by = "GTAP_crop") %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year) %>%
      summarise(Fert_Cons_MtN = sum(Fert_Cons_MtN)) %>%
      ungroup()

    # Calculate fertilizer input-output coefficients: kg N per kg crop produced, by GCAM region, GCAM commodity, year
    # This is not yet at the level of the GLU
    L142.ag_N_Fert_IO_R_C_Yh <- L101.ag_Prod_Mt_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year) %>%
      summarise(ag_Prod_Mt = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(L142.ag_Fert_Cons_MtN_R_C_Yh,
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "year"),
                               ignore_columns = "Fert_Cons_MtN") %>%
      replace_na(list(Fert_Cons_MtN = 0)) %>%
      mutate(value = if_else(ag_Prod_Mt == 0, 0, Fert_Cons_MtN / ag_Prod_Mt)) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, value)

     # Expand IO coefs to the GLU. Drop the input altogether if all years have coefs of 0
    L142.ag_SyntheticNFert_IO_R_C_Y_GLU <- L101.ag_Prod_Mt_R_C_Y_GLU %>%
      select(-value) %>%
      left_join_error_no_match(L142.ag_N_Fert_IO_R_C_Yh,
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "year")) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      mutate(max_hist_coef = max(value)) %>%
      ungroup() %>%
      filter(max_hist_coef > 0) %>%
      select(-max_hist_coef)

    # Phosphorus
    # Production: first compute global production scalers, by year, so that production = consumption
    L142.ag_PFert_Prod_MtP2O5_glbl_Y <- L141.ag_Fert_Cons_MtP2O5_ctry_crop_Yh %>%
      group_by(year) %>%
      summarise(Prod_MtP2O5 = sum(Fert_Cons_MtP2O5)) %>%
      ungroup()

    L142.ag_PFert_ProdScalers <- L100.FAO_Fert_Prod_tP2O5 %>%
      group_by(year) %>%
      summarise(Prod_MtP2O5_unscaled = sum(value) * CONV_T_MT) %>%
      ungroup() %>%
      left_join_error_no_match(L142.ag_PFert_Prod_MtP2O5_glbl_Y, by = "year") %>%
      mutate(ProdScaler = Prod_MtP2O5 / Prod_MtP2O5_unscaled) %>%
      select(year, ProdScaler)

    L142.ag_PFert_Prod_MtP2O5_ctry_Y <- L100.FAO_Fert_Prod_tP2O5 %>%
      left_join_error_no_match(L142.ag_PFert_ProdScalers, by = "year") %>%
      mutate(value = value * ProdScaler * CONV_T_MT) %>%
      select(iso, year, value)

    # Net exports by region: production minus consumption
    # Production by region and year
    L142.ag_PFert_Prod_MtP2O5_R_Y <- L142.ag_PFert_Prod_MtP2O5_ctry_Y %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(prod = sum(value)) %>%
      ungroup()

    # Consumption by region and year
    L142.ag_PFert_Cons_MtP2O5_R_Y <- L141.ag_Fert_Cons_MtP2O5_ctry_crop_Yh %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(cons = sum(Fert_Cons_MtP2O5)) %>%
      ungroup()

    L142.ag_PFert_NetExp_MtP2O5_R_Y <- L142.ag_PFert_Prod_MtP2O5_R_Y %>%
      left_join_error_no_match(L142.ag_PFert_Cons_MtP2O5_R_Y, by = c("GCAM_region_ID", "year")) %>%
      mutate(GCAM_commodity = aglu.P_FERT_NAME,
             value = prod - cons) %>%
      select(GCAM_region_ID, GCAM_commodity, year, value)

    # Aggregate P fertilizer consumption by country and GTAP crop to GCAM region and GCAM commodity+subsector
    L142.ag_Fert_Cons_MtP2O5_R_C_Yh <- L141.ag_Fert_Cons_MtP2O5_ctry_crop_Yh %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      left_join_error_no_match(distinct(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector)),
                               by = "GTAP_crop") %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year) %>%
      summarise(Fert_Cons_MtP2O5 = sum(Fert_Cons_MtP2O5)) %>%
      ungroup()

    # Phosphorus fertilizer input-output coefficients: kg P per kg crop produced, by GCAM region, GCAM commodity, year
    # This is not yet at the level of the GLU
    # P input to crop is phosphorus, not phosphate, so the CONV_P2O5_P needs to be applied here
    L142.ag_P_Fert_IO_R_C_Yh <- L101.ag_Prod_Mt_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year) %>%
      summarise(ag_Prod_Mt = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(L142.ag_Fert_Cons_MtP2O5_R_C_Yh,
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "year"),
                               ignore_columns = "Fert_Cons_MtP2O5") %>%
      replace_na(list(Fert_Cons_MtP2O5 = 0)) %>%
      mutate(value = if_else(ag_Prod_Mt == 0, 0, Fert_Cons_MtP2O5 * CONV_P2O5_P / ag_Prod_Mt)) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, value)

    # Expand IO coefs to the GLU. Drop the input altogether if all years have coefs of 0
    L142.ag_PFert_IO_R_C_Y_GLU <- L101.ag_Prod_Mt_R_C_Y_GLU %>%
      select(-value) %>%
      left_join_error_no_match(L142.ag_P_Fert_IO_R_C_Yh,
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "year")) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      mutate(max_hist_coef = max(value)) %>%
      ungroup() %>%
      filter(max_hist_coef > 0) %>%
      select(-max_hist_coef)


    # Produce outputs
    L142.ag_NFert_Prod_MtN_ctry_Y %>%
      add_title("Nitrogen fertilizer production by country / year") %>%
      add_units("Unit = MtN") %>%
      add_comments("Nitrogen fertilizer production by country is adjusted so that global total production equals consumption") %>%
      add_comments("Units are converted from tons to million tons of Nitrogen") %>%
      add_legacy_name("L142.ag_NFert_Prod_MtN_ctry_Y") %>%
      add_precursors("L141.ag_Fert_Cons_MtN_ctry_crop_Yh",
                     "L100.FAO_Fert_Prod_tN") ->
      L142.ag_NFert_Prod_MtN_ctry_Y

    L142.ag_NFert_NetExp_MtN_R_Y %>%
      add_title("Nitrogen fertilizer net exports by GCAM region / year") %>%
      add_units("Unit = MtN") %>%
      add_comments("Nitrogen fertilizer consumption and adjusted production are aggregated from country to GCAM region level") %>%
      add_comments("Net exports are calculated as production minus consumption, in million tons of Nitrogen") %>%
      add_legacy_name("L142.ag_NFert_NetExp_MtN_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_Fert_Prod_tN",
                     "L141.ag_Fert_Cons_MtN_ctry_crop_Yh") ->
      L142.ag_NFert_NetExp_MtN_R_Y

    L142.ag_SyntheticNFert_IO_R_C_Y_GLU %>%
      add_title("Nitrogen fertilizer input-output coefficients by GCAM region / crop / year / GLU") %>%
      add_units("kg N per kg crop produced") %>%
      add_comments("Input-output coefficients for each crop are calculated as fertilizer consumption divided by crop production") %>%
      add_legacy_name("L142.ag_SyntheticNFert_IO_R_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L100.LDS_ag_prod_t",
                     "L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L141.ag_Fert_Cons_MtN_ctry_crop_Yh") ->
      L142.ag_SyntheticNFert_IO_R_C_Y_GLU

    L142.ag_PFert_Prod_MtP2O5_R_Y %>%
      add_title("Phosphate fertilizer production by region / year") %>%
      add_units("Unit = MtP2O5") %>%
      add_comments("Fertilizer production by country is adjusted so that global total production equals consumption") %>%
      add_comments("Units are converted from tons to million tons of phosphate") %>%
      add_precursors("L141.ag_Fert_Cons_MtP2O5_ctry_crop_Yh",
                     "L100.FAO_Fert_Prod_tP2O5") ->
      L142.ag_PFert_Prod_MtP2O5_R_Y

    L142.ag_PFert_NetExp_MtP2O5_R_Y %>%
      add_title("Phosphorus fertilizer net exports by GCAM region / year") %>%
      add_units("Unit = MtP2O5") %>%
      add_comments("Phosphorus fertilizer consumption and adjusted production are aggregated from country to GCAM region level") %>%
      add_comments("Net exports are calculated as production minus consumption, in million tons of Phosphorus (i.e., not phosphate)") %>%
      same_precursors_as(L142.ag_PFert_Prod_MtP2O5_R_Y) ->
      L142.ag_PFert_NetExp_MtP2O5_R_Y

    L142.ag_PFert_IO_R_C_Y_GLU %>%
      add_title("Phosphorus fertilizer input-output coefficients by GCAM region / crop / year / GLU") %>%
      add_units("kg P per kg crop produced") %>%
      add_comments("Input-output coefficients for each crop are calculated as fertilizer consumption divided by crop production") %>%
      add_legacy_name("L142.ag_SyntheticNFert_IO_R_C_Y_GLU") %>%
      add_precursors("aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L100.LDS_ag_prod_t",
                     "L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L141.ag_Fert_Cons_MtP2O5_ctry_crop_Yh") ->
      L142.ag_PFert_IO_R_C_Y_GLU

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
