#' module_aglu_batch_ag_prodchange_AgMIP_ssp1_rcp2p6_xml
#'
#' Construct XML data structure for \code{ag_prodchange_AgMIP_ssp1_rcp2p6.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_prodchange_AgMIP_ssp1_rcp2p6.xml}. The corresponding file in the
#' original data system was \code{batch_ag_prodchange_AgMIP_ssp1_rcp2p6.xml.R} (aglu XML).
module_aglu_batch_ag_prodchange_AgMIP_ssp1_rcp2p6_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2052.AgProdChange_SSP1_rcp2p6"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_prodchange_AgMIP_ssp1_rcp2p6.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2052.AgProdChange_SSP1_rcp2p6 <- get_data(all_data, "L2052.AgProdChange_SSP1_rcp2p6")

    # ===================================================

    # Produce outputs
    create_xml("ag_prodchange_AgMIP_ssp1_rcp2p6.xml") %>%
      add_xml_data(L2052.AgProdChange_SSP1_rcp2p6, "AgProdChange") %>%
      add_precursors("L2052.AgProdChange_SSP1_rcp2p6") ->
      ag_prodchange_AgMIP_ssp1_rcp2p6.xml

    return_data(ag_prodchange_AgMIP_ssp1_rcp2p6.xml)
  } else {
    stop("Unknown command")
  }
}
