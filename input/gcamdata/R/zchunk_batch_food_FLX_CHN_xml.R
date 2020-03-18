#' module_aglu_batch_food_FLX_CHN_xml
#'
#' Construct XML data structure for \code{food_FLX_CHN.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{food_FLX_CHN.xml}.
module_aglu_batch_food_FLX_CHN_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.IncomeElasticity_FLX",
             "L203.FuelPrefElast_FLX"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "food_FLX_CHN.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.IncomeElasticity_FLX_CHN <- get_data(all_data, "L203.IncomeElasticity_FLX") %>%
      filter(region == "China")
    L203.FuelPrefElast_FLX_CHN <- get_data(all_data, "L203.FuelPrefElast_FLX") %>%
      filter(region == "China")

    # ===================================================

    # Produce outputs
    create_xml("food_FLX_CHN.xml") %>%
      add_xml_data(L203.IncomeElasticity_FLX_CHN, "IncomeElasticity") %>%
      add_xml_data(L203.FuelPrefElast_FLX_CHN, "FuelPrefElast") %>%
      add_precursors("L203.IncomeElasticity_FLX", "L203.FuelPrefElast_FLX") ->
      food_FLX_CHN.xml

    return_data(food_FLX_CHN.xml)
  } else {
    stop("Unknown command")
  }
}
