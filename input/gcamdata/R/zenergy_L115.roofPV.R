# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L115.roofPV
#'
#' Convert rooftop PV resources from the 14 GCAM regions, i.e., "region_GCAM3", to the 32 GCAM region IDs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L115.RsrcCurves_EJ_R_roofPV}. The corresponding file in the
#' original data system was \code{LA115.roofPV.R} (energy level1).
#' @details Rooftop PV resources are given in the input file according to the 14 GCAM regions, i.e., "region_GCAM3." Because some regions span multiple GCAM region IDs, population in 2010 was used to allocate proportionally.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by mutate select summarise
#' @author AJS April 2017
module_energy_L115.roofPV <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/A15.roofPV_curves",
             "L100.Pop_thous_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L115.RsrcCurves_EJ_R_roofPV"))
  } else if(command == driver.MAKE) {

    year <- iso <- country_name <- region_GCAM3 <- value <- maxSubResource <-
        popSum <- `curve-exponent` <- `mid-price` <- GCAM_region_ID <-
        resource <- subresource <- curve.exponent <- gdpSupplyElast <-
        subResourceCapacityFactor <- mid.price <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A15.roofPV_curves <- get_data(all_data, "energy/A15.roofPV_curves")
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")

    # ===================================================
    # Categorizing countries and regions to GCAM Region ID; selecting population in 2010
    L100.Pop_thous_ctry_Yh %>%
      filter(year == 2010) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      select(-iso, -country_name, -year) ->
      x

    # Sum of population by regions
    x %>%
      group_by(region_GCAM3) %>%
      summarise(popSum = sum(value)) ->
      pop_RG3

    # Building resource curves by GCAM Region ID. Because some regions span multiple GCAM Region IDs, population in 2010 is used to allocate.
    x %>%
      left_join_error_no_match(pop_RG3, by = "region_GCAM3") %>%
      left_join_error_no_match(A15.roofPV_curves, by = "region_GCAM3") %>%
      mutate(maxSubResource = maxSubResource * value / popSum) %>%
      rename(curve.exponent = `curve-exponent`, mid.price = `mid-price`) %>%
      group_by(GCAM_region_ID, resource, subresource, curve.exponent, gdpSupplyElast) %>%
      summarise(maxSubResource = sum(maxSubResource),
                mid.price = median(mid.price)) %>%
      ungroup() %>%
      select(GCAM_region_ID, resource, maxSubResource, mid.price, subresource, curve.exponent, gdpSupplyElast) ->
      L115.RsrcCurves_EJ_R_roofPV

    # ===================================================

    L115.RsrcCurves_EJ_R_roofPV %>%
      add_title("Rooftop PV resources by GCAM region ID") %>%
      add_units("EJ for maxSubResource; 1975 $/GJ for midprice; unitless for gdpSupplyElast") %>%
      add_comments("Resources converted from 14 GCAM regions to 32 GCAM region IDs") %>%
      add_legacy_name("L115.RsrcCurves_EJ_R_roofPV") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A15.roofPV_curves", "L100.Pop_thous_ctry_Yh") ->
      L115.RsrcCurves_EJ_R_roofPV

    return_data(L115.RsrcCurves_EJ_R_roofPV)
  } else {
    stop("Unknown command")
  }
}
