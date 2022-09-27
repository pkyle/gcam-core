# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_P_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{ag_Fert_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_FP_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_ag_P_IRR_MGMT.xml} (aglu XML).
module_aglu_batch_ag_P_IRR_MGMT_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2092.AgCoef_P_ag_irr_mgmt",
             "L2092.AgCoef_P_bio_irr_mgmt",
             "L2092.AgCost_ag_irr_mgmt_adj",
             "L2092.AgCost_bio_irr_mgmt_adj"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_P_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2092.AgCoef_P_ag_irr_mgmt <- get_data(all_data, "L2092.AgCoef_P_ag_irr_mgmt")
    L2092.AgCoef_P_bio_irr_mgmt <- get_data(all_data, "L2092.AgCoef_P_bio_irr_mgmt")
    L2092.AgCost_ag_irr_mgmt_adj <- get_data(all_data, "L2092.AgCost_ag_irr_mgmt_adj")
    L2092.AgCost_bio_irr_mgmt_adj <- get_data(all_data, "L2092.AgCost_bio_irr_mgmt_adj")
    # ===================================================

    # Produce outputs
    create_xml("ag_P_IRR_MGMT.xml") %>%
      add_xml_data(L2092.AgCoef_P_ag_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2092.AgCoef_P_bio_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2092.AgCost_ag_irr_mgmt_adj,"AgCost") %>%
      add_xml_data(L2092.AgCost_bio_irr_mgmt_adj, "AgCost") %>%
      add_precursors("L2092.AgCoef_P_ag_irr_mgmt",
                     "L2092.AgCoef_P_bio_irr_mgmt",
                     "L2092.AgCost_ag_irr_mgmt_adj",
                     "L2092.AgCost_bio_irr_mgmt_adj") ->
      ag_P_IRR_MGMT.xml

    return_data(ag_P_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
