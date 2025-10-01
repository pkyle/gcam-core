# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_elec_tech_scenarios_xml
#'
#' Construct XML files to define electric sector technology levels (adv, low).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{nuclear_adv_USA.xml}, \code{nuclear_low_USA.xml}.
module_gcamusa_elec_tech_scenarios_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L1233.globaltech_capital_ATB_adv",
      "L1233.globaltech_capital_ATB_low",
      "L1233.globaltech_OMfixed_ATB_adv",
      "L1233.globaltech_OMfixed_ATB_low",
      "L1233.globaltech_OMvar_ATB_adv",
      "L1233.globaltech_OMvar_ATB_low",
      "L2233.GlobalTechCapital_elecS_USA",
      "L2233.GlobalTechOMfixed_elecS_cool_USA",
      "L2233.GlobalTechOMvar_elecS_cool_USA")

  MODULE_OUTPUTS <-
    c(XML = "nuclear_adv_USA.xml",
      XML = "nuclear_low_USA.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence global package checks
    value <- year <- NULL

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Process data to final formats
    # Start with ref assumptions for nuclear, filter to future years, join in advtech costs for replacement
    assign_gcamusa_nuc_tech_cost <- function(refdata, replacement_data, costvar){
      refdata <- refdata[names(refdata) != costvar]

      replacement_data <- replacement_data[c("technology", "year", costvar)]

      outdata <- filter(refdata, subsector.name0 == 'nuclear',
                        year %in% MODEL_FUTURE_YEARS) %>%
        mutate(base_techname = sub("nuc_base_", "", subsector.name)) %>%
        inner_join(replacement_data, by = c(base_techname = "technology", "year")) %>%
        select(-base_techname)

      return(outdata)
    }

    # Capital costs
    L2233.GlobalTechCapital_elecS_USA_nuc_adv <-
      assign_gcamusa_nuc_tech_cost(refdata = L2233.GlobalTechCapital_elecS_USA,
                                   replacement_data = L1233.globaltech_capital_ATB_adv,
                                   costvar = "capital.overnight") %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))
    L2233.GlobalTechCapital_elecS_USA_nuc_low <-
      assign_gcamusa_nuc_tech_cost(refdata = L2233.GlobalTechCapital_elecS_USA,
                                   replacement_data = L1233.globaltech_capital_ATB_low,
                                   costvar = "capital.overnight") %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))

    # Fixed O&M
    L2233.GlobalTechOMfixed_elecS_cool_USA_nuc_adv <-
      assign_gcamusa_nuc_tech_cost(refdata = L2233.GlobalTechOMfixed_elecS_cool_USA,
                                   replacement_data = L1233.globaltech_OMfixed_ATB_adv,
                                   costvar = "OM.fixed") %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))
    L2233.GlobalTechOMfixed_elecS_cool_USA_nuc_low <-
      assign_gcamusa_nuc_tech_cost(refdata = L2233.GlobalTechOMfixed_elecS_cool_USA,
                                   replacement_data = L1233.globaltech_OMfixed_ATB_low,
                                   costvar = "OM.fixed") %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))

    # Variable O&M
    L2233.GlobalTechOMvar_elecS_cool_USA_nuc_adv <-
      assign_gcamusa_nuc_tech_cost(refdata = L2233.GlobalTechOMvar_elecS_cool_USA,
                                   replacement_data = L1233.globaltech_OMvar_ATB_adv,
                                   costvar = "OM.var") %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM))
    L2233.GlobalTechOMvar_elecS_cool_USA_nuc_low <-
      assign_gcamusa_nuc_tech_cost(refdata = L2233.GlobalTechOMvar_elecS_cool_USA,
                                   replacement_data = L1233.globaltech_OMvar_ATB_low,
                                   costvar = "OM.var") %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM))

    # ===================================================

    # Produce outputs
    create_xml("nuclear_adv_USA.xml") %>%
      add_xml_data(L2233.GlobalTechCapital_elecS_USA_nuc_adv, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecS_cool_USA_nuc_adv, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecS_cool_USA_nuc_adv, "GlobalTechOMvar") %>%
      add_precursors("L1233.globaltech_capital_ATB_adv",
                     "L1233.globaltech_OMfixed_ATB_adv",
                     "L1233.globaltech_OMvar_ATB_adv",
                     "L2233.GlobalTechOMvar_elecS_cool_USA",
                     "L2233.GlobalTechOMfixed_elecS_cool_USA",
                     "L2233.GlobalTechCapital_elecS_USA") ->
      nuclear_adv_USA.xml

    create_xml("nuclear_low_USA.xml") %>%
      add_xml_data(L2233.GlobalTechCapital_elecS_USA_nuc_low, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecS_cool_USA_nuc_low, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecS_cool_USA_nuc_low, "GlobalTechOMvar") %>%
      add_precursors("L1233.globaltech_capital_ATB_low",
                     "L1233.globaltech_OMfixed_ATB_low",
                     "L1233.globaltech_OMvar_ATB_low",
                     "L2233.GlobalTechOMvar_elecS_cool_USA",
                     "L2233.GlobalTechOMfixed_elecS_cool_USA",
                     "L2233.GlobalTechCapital_elecS_USA") ->
      nuclear_low_USA.xml

    return_data(nuclear_adv_USA.xml,
                nuclear_low_USA.xml)
  } else {
    stop("Unknown command")
  }
}
