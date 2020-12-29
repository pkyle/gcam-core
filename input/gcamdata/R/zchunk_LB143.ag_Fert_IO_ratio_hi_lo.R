#' module_aglu_LB143.ag_Fert_IO_ratio_hi_lo
#'
#' Calculate the ratio of fertilizer applied to the "hi" technology divided by the "lo" technology, by region / basin / year
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{asdf}.
#' @details This chunk calculates the ratio in the fertilizer input-output coefficients of the "hi-yield" technology divided by the "lo-yield"
#' technology for all regions, basins, and crops.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Eva December 2020
module_aglu_LB143.ag_Fert_IO_ratio_hi_lo <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             FILE = "aglu/Vish_M_coefs",
             FILE = "aglu/Vish_Ninput_kgha",
             "L100.LDS_ag_HA_ha",
             "L100.LDS_ag_prod_t",
             "L142.ag_Fert_IO_R_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L143.AgFertIO_Ratio_R_C_GLU"))
  } else if(command == driver.MAKE) {

    Fert_Cons_MtN <- NULL   # silence package checks

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    Vish_M_coefs <- get_data(all_data, "aglu/Vish_M_coefs")
    Vish_Ninput_kgha <- get_data(all_data, "aglu/Vish_Ninput_kgha")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L142.ag_Fert_IO_R_C_Y_GLU <- get_data(all_data, "L142.ag_Fert_IO_R_C_Y_GLU")

# Perform computations

    # Step 1 - downscale the Vish datasets to the full list of countries (~200), crops (~180) and basins (235)
    L143.Mcoef_ctry_crop_GLU <- Vish_M_coefs %>%
      rename(Crop = `Crop/Country`) %>%
      gather(key = "Country", value = "M", -Crop) %>%
      left_join(select(AGLU_ctry, iso, Vishwakarma_country),
                by = c(Country = "Vishwakarma_country")) %>%
      left_join(select(FAO_ag_items_PRODSTAT, Vish_crop, GTAP_crop), by = c(Crop = "Vish_crop")) %>%
      select(iso, GTAP_crop, M) %>%
      inner_join(select(L100.LDS_ag_HA_ha, iso, GLU, GTAP_crop),
                by = c("iso", "GTAP_crop"))

    # Step 2 - Compute the estimated base-year yields of every crop/country/basin (these aren't the actual yields; it's the output of the functional form)
    L143.Yield_ctry_crop_GLU <- Vish_Ninput_kgha %>%
      rename(Crop = `Crop/Country`) %>%
      gather(key = "Country", value = "I", -Crop) %>%
      left_join(select(AGLU_ctry, iso, Vishwakarma_country), by = c(Country = "Vishwakarma_country")) %>%
      left_join(select(FAO_ag_items_PRODSTAT, Vish_crop, GTAP_crop), by = c(Crop = "Vish_crop")) %>%
      select(iso, GTAP_crop, I) %>%
      inner_join(select(L100.LDS_ag_HA_ha, iso, GLU, GTAP_crop), by = c("iso", "GTAP_crop")) %>%
      inner_join(L143.Mcoef_ctry_crop_GLU, by = c("iso", "GTAP_crop", "GLU")) %>%
      mutate(Y = M*I/(M+I))

    # Step 3 - Starting from the harvested area of each crop/country/basin, estimate the "yield" (as above) and "production"
    # (computed as estimated yield times actual harvested area)
    L143.Prod_ctry_crop_GLU <- L143.Yield_ctry_crop_GLU %>%
      inner_join(L100.LDS_ag_HA_ha, by = c("iso", "GTAP_crop", "GLU")) %>%
      rename(area_ha = value) %>%
      mutate(prod = Y*area_ha) %>%
      drop_na() %>%
      filter(prod > 0)

    # Apply land shares to disaggregate hi and lo yields
    # Estimate the input output coefficient for hi and lo yield
    # Estimate the ratio of coef_hi to coef_lo
    L143.AgProdFert_ctry_crop_GLU <- L143.Prod_ctry_crop_GLU %>%
      mutate(yieldmult_hi = 1 + aglu.MGMT_YIELD_ADJ,
             yieldmult_lo = 1 - aglu.MGMT_YIELD_ADJ,
             # Calculate the land shares to allocate to hi and lo is the rest (currently the shares are set at 0.5/0.5 to all)
             landshare_lo = (1 - yieldmult_hi) / (yieldmult_lo - yieldmult_hi),
             landshare_hi = 1 - landshare_lo,
             # indicate whether the data in the given row are usable. Using M to determine this
             usable = if_else(M < 0 | M > 1e6, FALSE, TRUE),
             # Calculate hi and lo yields (kg/ha)
             Y_hi = Y * yieldmult_hi,
             Y_lo = Y * yieldmult_lo,
             # Calculate hi and lo harvested area (ha)
             area_ha_hi = area_ha * landshare_hi,
             area_ha_lo = area_ha * landshare_lo,
             # Calculate hi and lo ag production (kg)
             prod_hi = Y_hi * area_ha_hi,
             prod_lo = Y_lo * area_ha_lo,
             # Estimate nitrogen inputs (kg/ha)
             I_hi = Y_hi * M / (M - Y_hi),
             I_lo = Y_lo * M / (M - Y_lo),
             I_hi = pmin(aglu.MAX_Ihi_I_RATIO * I, I_hi),
             I_hi = if_else(I_hi < 0, aglu.MAX_Ihi_I_RATIO * I, I_hi),
             # Estimate fertilizer applied (kg)
             fert_hi = I_hi * area_ha_hi,
             fert_lo = I_lo * area_ha_lo) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      inner_join(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector),
                               by = "GTAP_crop") %>%
      drop_na(GCAM_commodity)

    L143.AgFertIO_Ratio_R_C_GLU <- L143.AgProdFert_ctry_crop_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      filter(usable) %>%
      summarise(prod_hi = sum(prod_hi),
                prod_lo = sum(prod_lo),
                fert_hi = sum(fert_hi),
                fert_lo = sum(fert_lo)) %>%
      ungroup() %>%
      mutate(io_coef_hi = fert_hi / prod_hi,
             io_coef_lo = fert_lo /prod_lo,
             # Estimate the ratio of coef_hi to coef_lo
             FERT_IO_RATIO_HI_LO = pmin(aglu.MAX_FERT_IO_RATIO_HI_LO, io_coef_hi / io_coef_lo))

    # Produce outputs
    L143.AgFertIO_Ratio_R_C_GLU %>%
      add_title("Fertilizer IO coef ratio (hi/lo) by region / crop / GLU") %>%
      add_units("Unitless") %>%
      add_comments("Assumed to apply to all years") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/Vish_M_coefs",
                     "aglu/Vish_Ninput_kgha",
                     "L100.LDS_ag_prod_t",
                     "L100.LDS_ag_HA_ha",
                     "L142.ag_Fert_IO_R_C_Y_GLU") ->
      L143.AgFertIO_Ratio_R_C_GLU

    return_data(L143.AgFertIO_Ratio_R_C_GLU)
  } else {
    stop("Unknown command")
  }
}
