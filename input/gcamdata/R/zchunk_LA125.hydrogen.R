# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA125.hydrogen
#'
#' Provides supply sector information, subsector information, technology information for hydrogen sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{asdf}.
#' @details Takes inputs from H2A and generates GCAM's assumptions by technology and year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate select
#' @importFrom tidyr complete nesting
#' @author GPK/JF/PW July 2021
module_energy_LA125.hydrogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A22.globaltech_coef",
             FILE = "energy/A23.globaltech_capital",
             FILE = "energy/A23.globaltech_eff"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L125.globaltech_coef",
             "L125.globaltech_cost"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- NULL  # silence package check notes

    # Load required inputs
    A22.globaltech_coef <- get_data(all_data, "energy/A22.globaltech_coef")
    A23.globaltech_capital <- get_data(all_data, "energy/A23.globaltech_capital", strip_attributes = TRUE)
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff", strip_attributes = TRUE)

    # ===================================================

    # Process data




    # Temporarily create the "return" data objects out of thin air
    L125.globaltech_coef <- tibble(asdf = 1:5)
    L125.globaltech_cost <- tibble(asdf = 1:5)

    # ===================================================
    # Produce outputs

    L125.globaltech_coef %>%
      add_title("Input-output coefficients of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated original data into all model years") %>%
      add_precursors("energy/A22.globaltech_coef", "energy/A23.globaltech_eff")  ->
      L125.globaltech_coef

    L125.globaltech_cost %>%
      add_title("Costs of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechCost_h2") %>%
      add_precursors("energy/A23.globaltech_capital")  ->
      L125.globaltech_cost

    return_data(L125.globaltech_coef, L125.globaltech_cost)
  } else {
    stop("Unknown command")
  }
}
