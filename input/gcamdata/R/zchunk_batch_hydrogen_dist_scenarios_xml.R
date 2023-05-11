# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_hydrogen_dist_scenarios_xml
#'
#' Construct XML data structure for \code{hydrogen_dist_pipeline.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen_dist_pipeline.xml}. The corresponding file in the
#' original data system was \code{batch_hydrogen.xml.R} (energy XML).
module_energy_batch_hydrogen_dist_scenarios_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.SubsectorShrwtFllt_h2",
             "L225.GlobalTechShrwt_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hydrogen_dist_pipeline.xml",
             XML = "hydrogen_dist_liqtruck.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L225.SubsectorShrwtFllt_h2 <- get_data(all_data, "L225.SubsectorShrwtFllt_h2")
    L225.GlobalTechShrwt_h2 <- get_data(all_data, "L225.GlobalTechShrwt_h2")

    L225.SubsectorShrwtFllt_h2pipe <- subset(L225.SubsectorShrwtFllt_h2,
                                             supplysector == "H2 liquid truck"  | subsector == "H2 liquid truck") %>%
      mutate(share.weight = 0)

    L225.GlobalTechShrwt_h2pipe <- subset(L225.GlobalTechShrwt_h2,
                                          technology == "H2 liquid truck") %>%
      mutate(share.weight = 0)

    L225.SubsectorShrwtFllt_h2truck <- subset(L225.SubsectorShrwtFllt_h2,
                                              supplysector == "H2 pipeline" | subsector == "H2 pipeline") %>%
      mutate(share.weight = 0)

    L225.GlobalTechShrwt_h2truck <- subset(L225.GlobalTechShrwt_h2,
                                           technology == "H2 pipeline") %>%
      mutate(share.weight = 0)

    # ===================================================

    # Produce outputs
    hydrogen_dist_pipeline.xml <- create_xml("hydrogen_dist_pipeline.xml") %>%
      add_xml_data(L225.SubsectorShrwtFllt_h2pipe, "SubsectorShrwtFllt") %>%
      add_xml_data(L225.GlobalTechShrwt_h2pipe, "GlobalTechShrwt") %>%
      add_precursors("L225.SubsectorShrwtFllt_h2",
                     "L225.GlobalTechShrwt_h2") ->
      hydrogen_dist_pipeline.xml

    hydrogen_dist_liqtruck.xml <- create_xml("hydrogen_dist_liqtruck.xml") %>%
      add_xml_data(L225.SubsectorShrwtFllt_h2truck, "SubsectorShrwtFllt") %>%
      add_xml_data(L225.GlobalTechShrwt_h2truck, "GlobalTechShrwt") %>%
      add_precursors("L225.SubsectorShrwtFllt_h2",
                     "L225.GlobalTechShrwt_h2") ->
      hydrogen_dist_liqtruck.xml

    return_data(hydrogen_dist_pipeline.xml, hydrogen_dist_liqtruck.xml)
  } else {
    stop("Unknown command")
  }
}
