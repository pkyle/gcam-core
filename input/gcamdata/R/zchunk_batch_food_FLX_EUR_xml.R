#' module_aglu_batch_food_FLX_EUR_xml
#'
#' Construct XML data structure for \code{food_FLX_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{food_FLX_EUR.xml}.
module_aglu_batch_food_FLX_EUR_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.IncomeElasticity_FLX",
             "L203.FuelPrefElast_FLX"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "food_FLX_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    EUR_regions <- c("EU-12", "EU-15", "European Free Trade Association")
    L203.IncomeElasticity_FLX_EUR <- get_data(all_data, "L203.IncomeElasticity_FLX") %>%
      filter(region %in% EUR_regions)
    L203.FuelPrefElast_FLX_EUR <- get_data(all_data, "L203.FuelPrefElast_FLX") %>%
      filter(region %in% EUR_regions)

    # ===================================================

    # Produce outputs
    create_xml("food_FLX_EUR.xml") %>%
      add_xml_data(L203.IncomeElasticity_FLX_EUR, "IncomeElasticity") %>%
      add_xml_data(L203.FuelPrefElast_FLX_EUR, "FuelPrefElast") %>%
      add_precursors("L203.IncomeElasticity_FLX", "L203.FuelPrefElast_FLX") ->
      food_FLX_EUR.xml

    return_data(food_FLX_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
