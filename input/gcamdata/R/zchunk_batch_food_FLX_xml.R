#' module_aglu_batch_food_FLX_xml
#'
#' Construct XML data structure for \code{food_FLX.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{food_FLX.xml}.
module_aglu_batch_food_FLX_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.IncomeElasticity_FLX",
             "L203.FuelPrefElast_FLX"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "food_FLX.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.IncomeElasticity_FLX <- get_data(all_data, "L203.IncomeElasticity_FLX")
    L203.FuelPrefElast_FLX <- get_data(all_data, "L203.FuelPrefElast_FLX")

    # ===================================================

    # Produce outputs
    create_xml("food_FLX.xml") %>%
      add_xml_data(L203.IncomeElasticity_FLX, "IncomeElasticity") %>%
      add_xml_data(L203.FuelPrefElast_FLX, "FuelPrefElast") %>%
      add_precursors("L203.IncomeElasticity_FLX", "L203.FuelPrefElast_FLX") ->
      food_FLX.xml

    return_data(food_FLX.xml)
  } else {
    stop("Unknown command")
  }
}
