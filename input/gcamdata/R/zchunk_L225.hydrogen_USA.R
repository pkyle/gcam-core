# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L225.hydrogen_USA
#'
#' Selects the subsectors to be removed from the hydrogen sectors for GCAM USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.DeleteSubsector_h2_USA}. The corresponding file in the
#' original data system was \code{L225.hydrogen_USA.R} (gcam-usa level2).
#' @details This chunk selects the subsectors to be removed from the hydrogen sectors in GCAM USA on the national level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @author KD September 2017
module_gcamusa_L225.hydrogen_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A25.globaltech_shrwt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.DeleteStubTech_h2_USA",
             "L225.DeleteSubsector_h2_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- subsector <- supplysector <- NULL  # silence package check notes

    # Load required inputs
    A25.globaltech_shrwt <- get_data(all_data, "energy/A25.globaltech_shrwt")

    # ===================================================
    # This chunk selects the subsectors to be removed from the
    # hydrogen sectors in GCAM USA on the national level.

    # Since there is no basis for inter-state competition in the hydrogen sector
    # keep the logit exponents for hydrogen at the national level for GCAM USA.
    # Select the wind, solar, and electricity subsectors because these resources do
    # not exists in the national level in GCAM USA.
    A25.globaltech_shrwt %>%
      filter(technology == "electrolysis") %>%
      mutate(region = gcam.USA_REGION) %>%
      select(region, supplysector, subsector, stub.technology = technology) ->
      L225.DeleteStubTech_h2_USA

    # Remove also any subsectors that had all of their technologies removed
    A25.globaltech_shrwt %>%
      select(supplysector, subsector, stub.technology = technology) ->
      L225.full_tech_set

    L225.full_tech_set %>%
      anti_join(L225.DeleteStubTech_h2_USA, by = c("supplysector", "subsector", "stub.technology")) ->
      L225.remaining_tech_set

    A25.globaltech_shrwt %>%
      select(supplysector, subsector) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION) %>%
      anti_join(L225.remaining_tech_set, by = c("supplysector", "subsector")) ->
      L225.DeleteSubsector_h2_USA

    # ===================================================

    # Produce outputs
    L225.DeleteStubTech_h2_USA %>%
      add_title("Electrolysis-based technologies to be removed from USA region as there is no national market in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Temporary fix to allow GCAM-USA to be run with 2021 hydrogen module updates") %>%
      add_precursors("energy/A25.globaltech_shrwt") ->
      L225.DeleteStubTech_h2_USA

    L225.DeleteSubsector_h2_USA %>%
      add_title("Electrolysis-only subsectors to be removed from USA region as there is no national market in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Temporary fix to allow GCAM-USA to be run with 2021 hydrogen module updates") %>%
      add_precursors("energy/A25.globaltech_shrwt") ->
      L225.DeleteSubsector_h2_USA

    return_data(L225.DeleteStubTech_h2_USA, L225.DeleteSubsector_h2_USA)
  } else {
    stop("Unknown command")
  }
}
