# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2092.ag_P_irr_mgmt
#'
#' Specifies fertilizer coefficients for all technologies; adjusts nonLandVariableCost to remove fertilizer cost.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2092.AgCoef_P_ag_irr_mgmt,}, \code{L2092.AgCoef_P_bio_irr_mgmt}. No corresponding file in the
#' original data system
#' @details This chunk maps the fertilizer coefficients calculated in LB142 to all agricultural technologies.
#' We assume coefficients (in kgP per kgCrop) are equal for all four technologies (irr v rfd; hi v lo).
#' Adjust nonLandVariableCost to remove the now explicitly computed fertilizer cost.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else left_join mutate select
#' @importFrom tidyr replace_na
#' @author MO July 2020
module_aglu_L2092.ag_P_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "common/GCAM_region_names",
              FILE = "water/basin_to_country_mapping",
              FILE = "aglu/A_Fodderbio_chars",
              "L2062.AgCost_ag_irr_mgmt_adj",
              "L2062.AgCost_bio_irr_mgmt_adj",
              "L143.ag_P_IO_cons_R_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2092.AgCoef_P_ag_irr_mgmt",
             "L2092.AgCoef_P_bio_irr_mgmt",
             "L2092.AgCost_ag_irr_mgmt_adj",
             "L2092.AgCost_bio_irr_mgmt_adj"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- GCAM_region_ID <- GCAM_commodity <- GLU <- GLU_name <- IRR_RFD <-
      MGMT <- region <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <-
      minicam.energy.input <- coefficient <- WaterContent <- nonLandVariableCost <-
      FertCost <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names",strip_attributes =  T)
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping",strip_attributes =  T)
    A_Fodderbio_chars <- get_data(all_data, "aglu/A_Fodderbio_chars",strip_attributes =  T)
    L2062.AgCost_ag_irr_mgmt_adj <- get_data(all_data, "L2062.AgCost_ag_irr_mgmt_adj",strip_attributes =  T)
    L2062.AgCost_bio_irr_mgmt_adj <- get_data(all_data, "L2062.AgCost_bio_irr_mgmt_adj",strip_attributes =  T)
    L143.ag_P_IO_cons_R_C_Y_GLU<-get_data(all_data,"L143.ag_P_IO_cons_R_C_Y_GLU",strip_attributes =  T)

    #set P price (2010$) price per kg based on 2009-2014 USDA avg
    P_price=3.553
    P_year=2010

    # Process Fertilizer Coefficients: Copy coefficients to all four technologies (irr/rfd + hi/lo)
    L143.ag_P_IO_cons_R_C_Y_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(basin_to_country_mapping[ c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%

      # Copy coefficients to all four technologies
      repeat_add_columns(tibble::tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%

      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_subsector, GLU_name, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = paste(paste(AgSupplySubsector, IRR_RFD, sep = aglu.IRR_DELIMITER),
                                            MGMT, sep = aglu.MGMT_DELIMITER)) %>%

      # Add name of minicam.energy.input
      mutate(minicam.energy.input = "P_fertilizer") %>%
      mutate(coefficient = round(P_IO, digits = energy.DIGITS_COEFFICIENT)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.energy.input, year, coefficient) ->
      L2092.AgCoef_P_ag_irr_mgmt


    # Copy final base year coefficients to all future years, bind with historic coefficients, then remove zeroes
    # P data goes only through 2012 - use 2010 value for 2015 and future years
    L2092.AgCoef_P_ag_irr_mgmt %>%
      filter(year == 1992) %>%
      select(-year) %>%
      repeat_add_columns(tibble::tibble(year = c(1975,1990))) %>%
      bind_rows(L2092.AgCoef_P_ag_irr_mgmt) %>%
      filter(year %in% c(1975,1990,2005,2010)) -> past #MODEL_BASE_YEARS includes 2015 which isn't in our P dataset

    L2092.AgCoef_P_ag_irr_mgmt %>%
      filter(year == 2010) %>%
      select(-year) %>%
      repeat_add_columns(tibble::tibble(year = c(2015, MODEL_FUTURE_YEARS)))->future

    past %>%
      bind_rows(future) %>%
      filter(coefficient > 0) ->
      L2092.AgCoef_P_ag_irr_mgmt

    # P coefficients are hard to find in literature: P response is very dependent on soil properties, etc.
    # In general, N coefficients are 1000x P coefficients and P-specific fertilizer is not currently used in most
    # biomass for bioenergy production situations (Tang et al 2020; Haines et al 2015; Woodson et al 2013) so we set coef=0 for now
    # #
    aglu_P_coef=0
    # Calculate fertilizer coefficients for grassy bioenergy crops
    A_Fodderbio_chars %>%
      filter(GCAM_commodity == "biomassGrass") %>%
      mutate(coefficient = (aglu_P_coef * CONV_G_KG / aglu.BIO_GRASS_YIELD_KGCM2    # Convert from application per unit area to per unit carbon
                            * aglu.CCONTENT_CELLULOSE * (1 - WaterContent))                         # Convert from carbon to wet biomass
             / (aglu.BIO_ENERGY_CONTENT_GJT * CONV_KG_T)) ->                         # Convert from biomass to energy
      bio_grass_coef

    A_Fodderbio_chars %>%
      filter(GCAM_commodity == "biomassTree") %>%
      mutate(coefficient = (aglu_P_coef * CONV_G_KG / aglu.BIO_TREE_YIELD_KGCM2    # Convert from application per unit area to per unit carbon
                            * aglu.CCONTENT_CELLULOSE * (1 - WaterContent))                         # Convert from carbon to wet biomass
             / (aglu.BIO_ENERGY_CONTENT_GJT * CONV_KG_T)) ->                         # Convert from biomass to energy
      bio_tree_coef

    # Map fertilizer coefficients to all bioenergy technologies

    L2062.AgCost_bio_irr_mgmt_adj %>% # We are just using this data.frame to get the region/sector/tech names
      select(-nonLandVariableCost) %>%
      mutate(minicam.energy.input = "P_fertilizer",
             coefficient = if_else(grepl("^biomassGrass", AgSupplySubsector),
                                   bio_grass_coef$coefficient, bio_tree_coef$coefficient)) ->
      L2092.AgCoef_P_bio_irr_mgmt

##COST##
    # From L2062:
    #  mutate(FertCost = coefficient * aglu.FERT_PRICE * gdp_deflator(1975, aglu.FERT_PRICE_YEAR) * CONV_KG_T / CONV_NH3_N
    # HERE: already in elemental P, already $/kg -> PCost=coefficient*P_price*gdp_deflator(1975, P_year)

    # Adjust nonLandVariableCost to separate P cost (which is accounted for specifically)
    L2062.AgCost_ag_irr_mgmt_adj %>%
      # Note: using left_join because there are instances with cost but no fertilizer use.
      left_join(L2092.AgCoef_P_ag_irr_mgmt,
                by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      # Set fertilizer coefficient to zero when missing. This will lead to zero fertilizer cost.
      replace_na(list(coefficient = 0)) %>%
      # P cost: price per kg based on 2009-2014 USDA avg
      mutate(PCost=coefficient*P_price*gdp_deflator(1975,P_year),
             nonLandVariableCost=round(nonLandVariableCost-PCost,aglu.DIGITS_CALPRICE)) %>%
      select(-minicam.energy.input, -coefficient, -PCost) ->
      L2092.AgCost_ag_irr_mgmt_adj

    # Adjust nonLandVariableCost to separate P cost (which is accounted for specifically)
    L2062.AgCost_bio_irr_mgmt_adj %>%
      # Note: using left_join because there are instances with cost but no fertilizer use.
      left_join(L2092.AgCoef_P_bio_irr_mgmt,
                by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      # Set fertilizer coefficient to zero when missing. This will lead to zero fertilizer cost.
      replace_na(list(coefficient = 0)) %>%
      # P cost: price per kg based on 2009-2014 USDA avg
      mutate(PCost=coefficient*P_price*gdp_deflator(1975,2010),
             nonLandVariableCost=round(nonLandVariableCost-PCost,aglu.DIGITS_CALPRICE)) %>%
      select(-minicam.energy.input, -coefficient, -PCost) ->
      L2092.AgCost_bio_irr_mgmt_adj


    # Produce outputs
    L2092.AgCoef_P_ag_irr_mgmt %>%
      add_title("P coefficients for agricultural technologies") %>%
      add_units("kgN per kg crop") %>%
      add_comments("Map fertilizer coefficients in L142.ag_Fert_IO_R_C_Y_GLU to all technologies") %>%
      add_comments("Note: we are using the same coefficient for all four management technologies (irrigated, rainfed, hi and lo") %>%
      add_legacy_name("L2062.AgCoef_Fert_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L143.ag_P_IO_cons_R_C_Y_GLU") ->
      L2092.AgCoef_P_ag_irr_mgmt

    L2092.AgCoef_P_bio_irr_mgmt %>%
      add_title("P coefficients for bioenergy technologies") %>%
      add_units("kgN per GJ") %>%
      add_comments("Compute bioenergy fertilizer coefficients from read-in constants") %>%
      add_comments("Note: L2062.AgCost_ag_irr_mgmt_adj is only used to identify all bioenergy technologies") %>%
      add_legacy_name("L2062.AgCoef_Fert_bio_irr_mgmt") %>%
      add_precursors("aglu/A_Fodderbio_chars",
                     "L2062.AgCost_ag_irr_mgmt_adj") ->
      L2092.AgCoef_P_bio_irr_mgmt

    L2092.AgCost_ag_irr_mgmt_adj %>%
      add_title("Adjusted non-land variable cost for agricultural technologies") %>%
      add_units("1975$ per kg") %>%
      add_comments("Subtract cost of P fertilizer from non-land variable cost.") %>%
      add_comments("Fertilizer costs is computed using a fixed P cost and the fertilizer coefficient") %>%
      same_precursors_as(L2092.AgCoef_P_ag_irr_mgmt) %>%
      add_precursors("L2062.AgCost_ag_irr_mgmt_adj") ->
      L2092.AgCost_ag_irr_mgmt_adj

    L2092.AgCost_bio_irr_mgmt_adj %>%
      add_title("Adjusted non-land variable cost for biomass technologies") %>%
      add_units("1975$ per kg") %>%
      add_comments("Subtract cost of P fertilizer from non-land variable cost.") %>%
      add_comments("Fertilizer costs is computed using a fixed P cost and the fertilizer coefficient") %>%
      same_precursors_as(L2092.AgCoef_P_bio_irr_mgmt) %>%
      add_precursors("L2062.AgCost_bio_irr_mgmt_adj") ->
      L2092.AgCost_bio_irr_mgmt_adj

    return_data(L2092.AgCoef_P_ag_irr_mgmt, L2092.AgCoef_P_bio_irr_mgmt, L2092.AgCost_ag_irr_mgmt_adj, L2092.AgCost_bio_irr_mgmt_adj)
  } else {
    stop("Unknown command")
  }
}
