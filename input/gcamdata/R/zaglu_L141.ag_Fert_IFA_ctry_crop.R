# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L141.ag_Fert_IFA_ctry_crop
#'
#' Reconcile disparate IFA fertilizer consumption data to calculate fertilizer consumption (demand) for each GTAP country/crop.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L141.ag_Fert_Cons_MtN_ctry_crop_Yh}. The corresponding file in the
#' original data system was \code{LB141.ag_Fert_IFA_ctry_crop.R} (aglu level1).
#' @details Multiple harvested area data sources (LDS, FAO) are reconciled and used with bottom-up fertilizer consumption data from
#' IFA2002 to calculate fertilizer demand for each country and crop.
#' Top down estimates are calculated using IFA fertilizer data, and the top down estimates are used to fill in missing data from
#' the bottom up estimates and scale the bottom-up estimates, making the final output.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr gather drop_na replace_na
#' @author ACS May 2017
module_aglu_L141.ag_Fert_IFA_ctry_crop <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
      FILE = "aglu/FUBC_crops",
      FILE = "aglu/FUBC_9_raw_data",
      FILE = "aglu/IFA_regions",
      "L100.LDS_ag_HA_ha",
      "L100.FAO_ag_HA_ha",
      "L100.FAO_Fert_Cons_tN",
      "L100.FAO_Fert_Cons_tP2O5")

  MODULE_OUTPUTS <-
    c("L141.ag_Fert_Cons_MtN_ctry_crop_Yh",
      "L141.ag_Fert_Cons_MtP2O5_ctry_crop_Yh")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    Country <- FUBC_crop_name <- Fert_Cons_MtN <- Nrate_tNha <- Nfert_Scaler <- IFA_N_t <-
      FUBC_HA_ha <- GCAM_commodity <- GCAM_region_ID <- GTAP_crop <- HA_ha <- item <- ISO3_code <-
      IFA_region <- iso <- Nfert_Scaler <- value <- year <- NULL   # silence package checks

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Overall method description
    # The 2025 FUBC inventory has 64 countries with ~10-15 crop commodity types per country. Some countries have a "residual"
    # or "other crops" category, and others have a large number of unmapped crops.
    # Missing values are backfilled from averages for the given IFA_region

    # Process FUBC (fertilizer use by country) inventory data
    # First make sure the same countries have the same names in the IFA region mapping table
    IFA_regions %>%
      mutate(Country = if_else(grepl("Bolivia", Country), "Bolivia", Country),
             Country = if_else(grepl("Iran", Country), "Iran", Country),
             Country = if_else(grepl("Russia", Country), "Russia", Country),
             Country = if_else(grepl("Tanzania", Country), "Tanzania", Country),
             Country = if_else(grepl("United Kingdom", Country), "United Kingdom", Country),
             Country = if_else(grepl("United States", Country), "United States", Country),
             ISO3_code = if_else(ISO3_code == "ROU", "ROM", ISO3_code)) %>%
      distinct() ->
      L141.IFA_region_mapping

    FUBC_crops %>%
      gather(key = "Country", value = "FUBC_crop_name", -GTAP_crop) ->
      L141.FUBC_crop_mapping

    # Set the harvested area used in the FUBC, join in country ISO codes and IFA regions (used for gap filling),
    # and set aside for FAO land area join
    FUBC_9_raw_data %>%
      filter(IFA_N_t > 0 | IFA_P2O5_t > 0,
             Original_crop_name_in_FUBC_report %in% L141.FUBC_crop_mapping$FUBC_crop_name) %>%
      mutate(FUBC_HA_ha = if_else(FAO_area_used_Yes_No == "Yes", FAO_area_ha, IFA_area_ha)) %>%
      select(Country = Original_country_name_in_FUBC_report, FUBC_crop_name = Original_crop_name_in_FUBC_report,
             year = Year_for_FAO_area, IFA_N_t, IFA_P2O5_t, FUBC_HA_ha) %>%
      left_join_error_no_match(L141.IFA_region_mapping, by = "Country") ->
      L141.FUBC_init

    L141.GCAMFAO_area_check <- L100.FAO_ag_HA_ha %>%
      filter(year %in% unique(L141.FUBC_init$year)) %>%
      mutate(ISO3_code = toupper(iso)) %>%
      inner_join(L141.IFA_region_mapping, by = "ISO3_code") %>%
      left_join(FAO_ag_items_PRODSTAT[c("item_code", "GTAP_crop")], by = "item_code") %>%
      inner_join(L141.FUBC_crop_mapping, by = c("Country", "GTAP_crop")) %>%
      group_by(ISO3_code, FUBC_crop_name, year) %>%
      summarise(FAO_HA_ha = sum(value)) %>%
      ungroup()

    # The harvested areas in the FUBC (FUBC_HA_ha) are often quite different from what is estimated in GCAM/FAOSTAT (FAO_HA_ha)
    # The method below generally assigns N rates calculated from the totals (IFA_N_t) divided by the FAO area (FAO_HA_ha),
    # but when the derived rate (GCAMFAO_Nrate) would be higher than an assumed arbitrary theshold (0.2 tonnes of N per hectare), and
    # FUBC-derived rate (FUBC_Nrate) is less than GCAMFAO_Nrate, then FUBC_Nrate is used.
    # This still leaves 25 observations (of 761; 3%) with high N application rates
    HIGH_N_RATE_tNha <- 0.2
    L141.FUBC_N_P_rates_ctry <- L141.FUBC_init %>%
      left_join_error_no_match(L141.GCAMFAO_area_check, by = c("ISO3_code", "FUBC_crop_name", "year"),
                               ignore_columns = "FAO_HA_ha") %>%
      replace_na(list(FAO_HA_ha = 0)) %>%
      mutate(iso = tolower(ISO3_code),
             FUBC_Nrate = IFA_N_t / FUBC_HA_ha,
             GCAMFAO_Nrate = IFA_N_t / FAO_HA_ha,
             N_rate_tNha = if_else(GCAMFAO_Nrate < HIGH_N_RATE_tNha, GCAMFAO_Nrate,
                                  if_else(GCAMFAO_Nrate > HIGH_N_RATE_tNha & GCAMFAO_Nrate > FUBC_Nrate, FUBC_Nrate, GCAMFAO_Nrate)),
             P_rate_tPha = IFA_P2O5_t / FAO_HA_ha) %>%
      select(Country, iso, FUBC_crop_name, year, N_rate_tNha, P_rate_tPha)

# These rates are expanded to the full set of GTAP/FAO crops, within the 64 countries that are in the FUBC.
    L141.N_P_rates_ctry64_crop <- L141.FUBC_N_P_rates_ctry %>%
      left_join(L141.FUBC_crop_mapping, by = c("Country", "FUBC_crop_name")) %>%
      select(iso, GTAP_crop, year, N_rate_tNha, P_rate_tPha)

    # Next, calculate the weighted average N and P application rates by IFA region, to use in the countries not in the FUBC
    L141.N_P_rates_regIFA_crop <- L100.FAO_ag_HA_ha %>%
      semi_join(L141.N_P_rates_ctry64_crop, by = c("iso", "year")) %>%
      # Some of the fodder crops have multiple GTAP crops per each item code. Normally we wouldn't want an expanding join
      # (which will duplicate the quantities) but here these are just being used to calculate weighted averages, for each
      # GTAP crop and country
      left_join(FAO_ag_items_PRODSTAT[c("item_code", "GTAP_crop")], by = "item_code") %>%
      drop_na(GTAP_crop) %>%
      group_by(iso, GTAP_crop, year) %>%
      summarise(HA_ha = sum(value)) %>%
      ungroup() %>%
      mutate(ISO3_code = toupper(iso)) %>%
      inner_join(L141.IFA_region_mapping, by = "ISO3_code") %>%
      left_join_error_no_match(L141.N_P_rates_ctry64_crop, by = c("iso", "GTAP_crop", "year"), ignore_columns = c("N_rate_tNha", "P_rate_tPha")) %>%
      replace_na(list(N_rate_tNha = 0, P_rate_tPha = 0)) %>%
      mutate(Nquantity_t = N_rate_tNha * HA_ha,
             Pquantity_t = P_rate_tPha * HA_ha) %>%
      group_by(Region_IFA, GTAP_crop) %>%
      summarise(HA_ha = sum(HA_ha),
                Nquantity_t = sum(Nquantity_t),
                Pquantity_t = sum(Pquantity_t)) %>%
      ungroup() %>%
      mutate(IFAreg_N_rate_tNha = Nquantity_t / HA_ha,
             IFAreg_P_rate_tPha = Pquantity_t / HA_ha) %>%
      replace_na(list(IFAreg_N_rate_tNha = 0, IFAreg_P_rate_tPha = 0)) %>%
      select(Region_IFA, GTAP_crop, IFAreg_N_rate_tNha, IFAreg_P_rate_tPha)

    # Next, a bottom-up table of default Nrates by country and crop type is created. This table has no year, and will
    # be scaled in each country to match the fertilizer consumption quantities observed by nation and year
    L141.N_P_rates_ctry_crop <- L141.IFA_region_mapping %>%
      select(ISO3_code, Region_IFA) %>%
      distinct() %>%
      mutate(iso = tolower(ISO3_code)) %>%
      left_join(L141.N_P_rates_regIFA_crop, by = c("Region_IFA")) %>%
      left_join_error_no_match(select(L141.N_P_rates_ctry64_crop, iso, GTAP_crop, N_rate_tNha, P_rate_tPha),
                               by = c("iso", "GTAP_crop"),
                               ignore_columns = c("N_rate_tNha", "P_rate_tPha")) %>%
      mutate(N_rate_tNha = if_else(is.na(N_rate_tNha), IFAreg_N_rate_tNha, N_rate_tNha),
             P_rate_tPha = if_else(is.na(P_rate_tPha), IFAreg_P_rate_tPha, P_rate_tPha)) %>%
      select(iso, GTAP_crop, N_rate_tNha, P_rate_tPha)

    # These rates are applied to the historical harvested areas by crop type, and scaled to match each country and
    # year's actual fertilizer consumption totals
    # Drop fertilizer consumption from any country-crop combinations that aren't in L100.LDS_ag_HA_ha, as they can't
    # be assigned to basins
    L100.LDS_ag_HA_ha %>%
      distinct(iso, GTAP_crop) -> L141.ctry_crop_filter

    # The Phosphorus data has several countries missing (Somalia, Timor L'este), so the left_join returns missing values
    L141.ag_NPFert_Scaler_ctry_year <- L100.FAO_ag_HA_ha %>%
      left_join_keep_first_only(FAO_ag_items_PRODSTAT[c("item_code", "GTAP_crop")], by = "item_code") %>%
      drop_na(GTAP_crop) %>%
      semi_join(L141.ctry_crop_filter, by = c("iso", "GTAP_crop")) %>%
      inner_join(L141.N_P_rates_ctry_crop, by = c("iso", "GTAP_crop")) %>%
      mutate(Nfert_t = value * N_rate_tNha,
             Pfert_t = value * P_rate_tPha) %>%
      group_by(iso, year) %>%
      summarise(NFert_t = sum(Nfert_t),
                PFert_t = sum(Pfert_t)) %>%
      ungroup() %>%
      left_join_error_no_match(select(L100.FAO_Fert_Cons_tN, iso, year, Nvalue = value), by = c("iso", "year")) %>%
      left_join_error_no_match(select(L100.FAO_Fert_Cons_tP2O5, iso, year, Pvalue = value), by = c("iso", "year"),
                               ignore_columns = "Pvalue") %>%
      mutate(Nfert_Scaler = Nvalue / NFert_t,
             Pfert_Scaler = if_else(is.na(Pvalue), 0, Pvalue / PFert_t)) %>%
      select(iso, year, Nfert_Scaler, Pfert_Scaler)

    L141.ag_Fert_Cons_MtNP_ctry_crop_Yh <- L100.FAO_ag_HA_ha %>%
      left_join_keep_first_only(FAO_ag_items_PRODSTAT[c("item_code", "GTAP_crop")], by = "item_code") %>%
      drop_na(GTAP_crop) %>%
      semi_join(L141.ctry_crop_filter, by = c("iso", "GTAP_crop")) %>%
      inner_join(L141.N_P_rates_ctry_crop, by = c("iso", "GTAP_crop")) %>%
      left_join_error_no_match(L141.ag_NPFert_Scaler_ctry_year, by = c("iso", "year")) %>%
      mutate(Fert_Cons_MtN = value * N_rate_tNha * Nfert_Scaler * CONV_T_MT,
             Fert_Cons_MtP2O5 = value * P_rate_tPha * Pfert_Scaler * CONV_T_MT)

    L141.ag_Fert_Cons_MtN_ctry_crop_Yh <- select(L141.ag_Fert_Cons_MtNP_ctry_crop_Yh, iso, GTAP_crop, year, Fert_Cons_MtN)
    L141.ag_Fert_Cons_MtP2O5_ctry_crop_Yh <- select(L141.ag_Fert_Cons_MtNP_ctry_crop_Yh, iso, GTAP_crop, year, Fert_Cons_MtP2O5)

    # Produce outputs
    L141.ag_Fert_Cons_MtN_ctry_crop_Yh %>%
      add_title("Synthetic nitrogen fertilizer consumption by country / GTAP crop / historical year") %>%
      add_units("Megatons of Nitrogen (MtN)") %>%
      add_comments("Bottom-up estimates of fertilizer consumption are calculated using the 2025 revision of the") %>%
      add_comments("Fertilizer Use by Crop inventory for 64 countries. Rates are extrapolated to others using the") %>%
      add_comments("International Fertilizer Association regions. Bottom-up rates are scaled by country and year") %>%
      add_comments("to match FAOSTAT estimates of fertilizer consumption") %>%
      add_precursors("aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FUBC_crops",
                     "aglu/FUBC_9_raw_data",
                     "aglu/IFA_regions",
                     "L100.LDS_ag_HA_ha",
                     "L100.FAO_ag_HA_ha",
                     "L100.FAO_Fert_Cons_tN") ->
      L141.ag_Fert_Cons_MtN_ctry_crop_Yh

    L141.ag_Fert_Cons_MtP2O5_ctry_crop_Yh %>%
      add_title("Phosphorus (phosphate) fertilizer consumption by country / GTAP crop / historical year") %>%
      add_units("Mt of phosphate (P2O5)") %>%
      add_comments("Bottom-up estimates of fertilizer consumption are calculated using the 2025 revision of the") %>%
      add_comments("Fertilizer Use by Crop inventory for 64 countries. Rates are extrapolated to others using the") %>%
      add_comments("International Fertilizer Association regions. Bottom-up rates are scaled by country and year") %>%
      add_comments("to match FAOSTAT estimates of fertilizer consumption") %>%
      same_precursors_as(L141.ag_Fert_Cons_MtN_ctry_crop_Yh) %>%
      add_precursors("L100.FAO_Fert_Cons_tP2O5") ->
      L141.ag_Fert_Cons_MtP2O5_ctry_crop_Yh

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
