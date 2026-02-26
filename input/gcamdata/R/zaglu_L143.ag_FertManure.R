# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L143.ag_FertManure
#'
#' Say what we're doing here (high level)
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.ag_Fert_Prod_MtN_ctry_Y}, \code{L142.ag_Fert_NetExp_MtN_R_Y}, \code{L142.ag_Fert_IO_R_C_Y_GLU}. The corresponding file in the
#' original data system was \code{LB142.ag_Fert_IO_R_C_Y_GLU.R} (aglu level1).
#' @details This chunk calculates fertilizer production by country / year (adjusted to global total consumption),
#' fertilizer net exports by GCAM region / year as production minus consumption, and fertilizer input-output coefficients
#' by GCAM region / commodity / year / GLU.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join group_by left_join mutate right_join select semi_join summarise
#' @importFrom tidyr complete replace_na
#' @author RC June 2017
module_aglu_L143.ag_FertManure <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/FAO/GCAMFAOSTAT_NManure",
      FILE = "aglu/FAO/FAO_an_types_manure",
      FILE = "aglu/AGLU_ctry",
      FILE = "common/iso_GCAM_regID",
      "L101.ag_Prod_Mt_R_C_Y_GLU",
      "L109.an_ALL_Mt_R_C_Y",
      "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU")

  MODULE_OUTPUTS <-
    c("L143.ag_NManure_IO_R_C_Y_GLU",
      "L143.an_NManure_SecOut_kgNperkg_R_C_Y",
      "L143.an_NManure_Mt_R_C_Y")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    GLU <- iso <- value <- year <- GCAM_subsector <- NULL   # silence package checks

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

   # DATA PROCESSING GOES HERE
    L143.ag_NManure_IO_R_C_Y_GLU <- "asdf"
    L143.an_NManure_SecOut_kgNperkg_R_C_Y <- "asdf"
    L143.an_NManure_Mt_R_C_Y <- "asdf"

    # Produce outputs
    L143.ag_NManure_IO_R_C_Y_GLU %>%
      add_title("N manure application per unit crop production by region/crop/year/GLU") %>%
      add_units("Unit = kg of manure N per kg crop production") %>%
      add_comments("Manure nitrogen is aggregated from all animal types") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_NManure",
                     "aglu/FAO/FAO_an_types_manure",
                     "aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU") ->
      L143.ag_NManure_IO_R_C_Y_GLU

    L143.an_NManure_SecOut_kgNperkg_R_C_Y %>%
      add_title("N manure secondary output coefficients") %>%
      add_units("Unit = kg N manure per kg animal commodity produced") %>%
      add_comments("N manure produced divided by production of each animal commodity by region and year") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_NManure",
                     "aglu/FAO/FAO_an_types_manure",
                     "aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L143.an_NManure_SecOut_kgNperkg_R_C_Y

    L143.an_NManure_Mt_R_C_Y %>%
      add_title("N manure production") %>%
      add_units("Unit = Mt N") %>%
      add_comments("N manure produced by region and animal commodity type") %>%
      same_precursors_as(L143.an_NManure_SecOut_kgNperkg_R_C_Y) ->
      L143.an_NManure_Mt_R_C_Y

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
