# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L273.nonghg_refinery
#'
#' Non-GHG input emissions parameters for refining sector in the USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L273.nonghg_state_refinery_USA}
#' @details This chunk calculates Non-GHG emissions parameters for refining technologies in the USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MAW March 2022

module_gcamusa_L273.nonghg_refinery <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L270.nonghg_tg_state_refinery_F_Yb",
             "L222.StubTechProd_refining_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L273.nonghg_state_refinery_USA"))
  } else if(command == driver.MAKE) {

    # silence check package notes
    year <- region <- supplysector <- subsector <- calibrated.value <- Non.CO2 <- value <- input.emissions <-
      fuel_input <- emiss.coef <- fuel <- sector <- emiss.coeff <- stub.technology <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions", strip_attributes = TRUE)
    L270.nonghg_tg_state_refinery_F_Yb <- get_data(all_data, "L270.nonghg_tg_state_refinery_F_Yb", strip_attributes = TRUE)
    L222.StubTechProd_refining_USA <- get_data(all_data, "L222.StubTechProd_refining_USA", strip_attributes = TRUE)

    # ===================================================
    # Perform computations

    ### Petroleum Refining: oil refining
    # We have petroleum refining emissions by state, so just need to
    # assign them to the correct supplysector / subsector / technology
    # Emissions are downscaled to aviation fuels vs. the rest of the refining sector
    pet_ref_emissions <- L270.nonghg_tg_state_refinery_F_Yb %>%
      filter(sector == "petroleum_refining",
             year %in% MODEL_YEARS,
             state %in% gcamusa.STATES) %>%
      group_by(state, Non.CO2, year) %>%
      summarise(input.emissions = sum(value)) %>%
      ungroup()

    # create structure and assign shares to aviation fuels/oil refining vs refining/oil refining
    pet_ref_shares <- L222.StubTechProd_refining_USA %>%
      filter(stub.technology == "oil refining") %>%
      group_by(region, year) %>%
      mutate(share = calOutputValue / sum(calOutputValue)) %>%
      replace_na(list(share = 0)) %>%
      ungroup() %>%
      select(region, supplysector, subsector, stub.technology, year, share)

    pet_ref_final <- pet_ref_shares %>%
      inner_join( pet_ref_emissions, by = c(region = "state", "year")) %>%
      mutate(input.emissions = input.emissions * share) %>%
      select(-share)

    ### ethanol_production: corn ethanol
    # We have ethanol production emissions by state, so just need to
    # assign them to the correct supplysector / subsector / technology
    ethanol_prod_emissions <- L270.nonghg_tg_state_refinery_F_Yb %>%
      filter(sector == "ethanol_production",
            year %in% MODEL_YEARS) %>%
      group_by(state, Non.CO2, year) %>%
      summarise(input.emissions = sum(value)) %>%
      ungroup() %>%
      rename(region = state)

    # create structure (no downscaling needed here as base-year aviation fuels are all petroleum based)
    ethanol_prod_structure <- L222.StubTechProd_refining_USA %>%
      filter( stub.technology == "corn ethanol",
              year %in% ethanol_prod_emissions$year) %>%
      distinct(supplysector, subsector, stub.technology, year)

    ethanol_prod_final <- ethanol_prod_emissions %>%
      left_join_error_no_match( ethanol_prod_structure, by = "year" )

    ### biodiesel_production: biodiesel
    # We have biodiesel production emissions by state, so just need to
    # assign them to the correct supplysector / subsector / technology
    biodiesel_prod_emissions <- L270.nonghg_tg_state_refinery_F_Yb %>%
      filter(sector == "biodiesel_production",
             year %in% MODEL_YEARS) %>%
      group_by(state, Non.CO2, year) %>%
      summarise(input.emissions = sum(value)) %>%
      ungroup() %>%
      rename(region = state)

    # create structure (no downscaling needed here as base-year aviation fuels are all petroleum based)
    biodiesel_prod_structure <- L222.StubTechProd_refining_USA %>%
      filter( stub.technology == "biodiesel",
              year %in% biodiesel_prod_emissions$year) %>%
      distinct(supplysector, subsector, stub.technology, year)

    biodiesel_prod_final <- biodiesel_prod_emissions %>%
      left_join_error_no_match( biodiesel_prod_structure, by = "year" )

    ### combine tables
    # State Level
    L273.nonghg_state_refinery_USA <- pet_ref_final %>%
      bind_rows(ethanol_prod_final,
                biodiesel_prod_final) %>%
      #change SO2 to SO2_1
      mutate(Non.CO2 = sub("SO2", "SO2_1", Non.CO2))

    # TODO: other techs: default emissions factors from GREET
    # TODO: or use Ellie's method, dropping EFs into folder


    # ===================================================

    # Produce outputs

    L273.nonghg_state_refinery_USA %>%
      add_title("Non-GHG input emissions parameters for refining technologies in the USA") %>%
      add_units("Tg") %>%
      add_comments("Emissions at the USA level") %>%
      add_precursors(FILE="gcam-usa/states_subregions",
                     "L270.nonghg_tg_state_refinery_F_Yb",
                     "L222.StubTechProd_refining_USA") ->
      L273.nonghg_state_refinery_USA

    return_data(L273.nonghg_state_refinery_USA)

  } else {
    stop("Unknown command")
  }
}
