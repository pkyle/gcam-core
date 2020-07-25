#' module_aglu_batch_food_FLX_DEV_xml
#'
#' Construct XML data structure for \code{food_FLX_DEV.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{food_FLX_DEV.xml}.
module_aglu_batch_food_FLX_DEV_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.IncomeElasticity_FLX",
             "L203.FuelPrefElast_FLX"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "food_FLX_DEV.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.IncomeElasticity_FLX_DEV <- get_data(all_data, "L203.IncomeElasticity_FLX") %>%
      filter(region %in% c("USA", "Canada", "Australia_NZ", "EU-12", "EU-15",
                           "European Free Trade Association", "Japan"))
    L203.FuelPrefElast_FLX_DEV <- get_data(all_data, "L203.FuelPrefElast_FLX") %>%
      filter(region %in% c("USA", "Canada", "Australia_NZ", "EU-12", "EU-15",
                           "European Free Trade Association", "Japan"))

    # ===================================================

    # Produce outputs
    create_xml("food_FLX_DEV.xml") %>%
      add_xml_data(L203.IncomeElasticity_FLX_DEV, "IncomeElasticity") %>%
      add_xml_data(L203.FuelPrefElast_FLX_DEV, "FuelPrefElast") %>%
      add_precursors("L203.IncomeElasticity_FLX", "L203.FuelPrefElast_FLX") ->
      food_FLX_DEV.xml

    return_data(food_FLX_DEV.xml)
  } else {
    stop("Unknown command")
  }
}
