# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2062.ag_Fert_irr_mgmt
#'
#' Specifies fertilizer coefficients for all technologies; adjusts nonLandVariableCost to remove fertilizer cost.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2062.AgCoef_Fert_ag_irr_mgmt}, \code{L2062.AgCoef_Fert_bio_irr_mgmt}, \code{L2062.AgCost_ag_irr_mgmt_adj}, \code{L2062.AgCost_bio_irr_mgmt_adj}. The corresponding file in the
#' original data system was \code{L2062.ag_Fert_irr_mgmt.R} (aglu level2).
#' @details This chunk maps the fertilizer coefficients calculated in LB142 to all agricultural technologies.
#' We assume coefficients (in kgN per kgCrop) are equal for all four technologies (irr v rfd; hi v lo).
#' Adjust nonLandVariableCost to remove the now explicitly computed fertilizer cost.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else left_join mutate select
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @author KVC June 2017
module_aglu_L2062.ag_Fert_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "common/GCAM_region_names",
              FILE = "water/basin_to_country_mapping",
              FILE = "aglu/A_Fodderbio_chars",
              "L142.ag_Fert_IO_R_C_Y_GLU",
              "L143.AgFertIO_Ratio_R_C_GLU",
              "L2012.AgProduction_ag_irr_mgmt",
              "L2052.AgCost_ag_irr_mgmt",
              "L2052.AgCost_bio_irr_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2062.AgCoef_Fert_ag_irr_mgmt",
             "L2062.AgCoef_Fert_bio_irr_mgmt",
             "L2062.AgCost_ag_irr_mgmt_adj",
             "L2062.AgCost_bio_irr_mgmt_adj"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- GCAM_region_ID <- GCAM_commodity <- GLU <- GLU_name <- IRR_RFD <-
      MGMT <- region <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <-
      minicam.energy.input <- coefficient <- WaterContent <- nonLandVariableCost <-
      FertCost <- GCAM_subsector <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_Fodderbio_chars <- get_data(all_data, "aglu/A_Fodderbio_chars")
    L142.ag_Fert_IO_R_C_Y_GLU <- get_data(all_data, "L142.ag_Fert_IO_R_C_Y_GLU", strip_attributes = TRUE)
    L143.AgFertIO_Ratio_R_C_GLU <- get_data(all_data, "L143.AgFertIO_Ratio_R_C_GLU")
    L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")
    L2052.AgCost_ag_irr_mgmt <- get_data(all_data, "L2052.AgCost_ag_irr_mgmt", strip_attributes = TRUE)
    L2052.AgCost_bio_irr_mgmt <- get_data(all_data, "L2052.AgCost_bio_irr_mgmt", strip_attributes = TRUE)

    # Prepare the production weights for being used to differentiate fertilizer IO coefs between hi and lo
    # technologies while conserving total fertilizer consumption quantities of regions/crops/basins/irrigation levels
    L2062.AgProduction_ag_irr_mgmt <- L2012.AgProduction_ag_irr_mgmt %>%
      mutate(level = paste0("output_",
                            substr(AgProductionTechnology, nchar(AgProductionTechnology) - 1, nchar(AgProductionTechnology))),
             AgProductionTechnology = substr(AgProductionTechnology, 1, nchar(AgProductionTechnology) - 3)) %>%
      select(LEVEL2_DATA_NAMES[["AgTechYr"]], level, calOutputValue) %>%
      spread(key = level, value = calOutputValue)

    # Process Fertilizer Coefficients: Copy coefficients to the two irrigation levels (irr/rfd),
    # and modify the fertilizer IO coefficients according to the exogenous ratio between hi and lo coefs
    L142.ag_Fert_IO_R_C_Y_GLU %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[ c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%

      # Copy coefficients to the two irrigation technologies
      repeat_add_columns(tibble::tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      # Add sector, subsector, technology names.
      # At this stage, the technology doesn't include the management level, for joining in the production volumes
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_subsector, GLU_name, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = paste(paste(AgSupplySubsector, IRR_RFD, sep = aglu.IRR_DELIMITER))) %>%

      left_join(select(L143.AgFertIO_Ratio_R_C_GLU, GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, FERT_IO_RATIO_HI_LO),
                by=c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      replace_na(list(FERT_IO_RATIO_HI_LO = aglu.FERT_IO_RATIO_HI_LO)) %>%
      mutate(minicam.energy.input = "N fertilizer") %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.energy.input, year, coefficient = value, FERT_IO_RATIO_HI_LO) %>%
      left_join_error_no_match(L2062.AgProduction_ag_irr_mgmt, by = LEVEL2_DATA_NAMES[["AgTechYr"]]) %>%
      mutate(output_tot = output_hi + output_lo,
             coef_lo = (coefficient * output_tot) / (FERT_IO_RATIO_HI_LO * output_hi + output_lo),
             coef_hi = coef_lo * FERT_IO_RATIO_HI_LO) %>%
      replace_na(list(coef_lo = 1, coef_hi = 1)) %>%
      repeat_add_columns(tibble(level = c("hi", "lo"))) %>%
      mutate(coefficient = round(if_else(level == "hi", coef_hi, coef_lo), aglu.DIGITS_CALOUTPUT + 1),
             AgProductionTechnology = paste(AgProductionTechnology, level, sep = aglu.MGMT_DELIMITER)) %>%
      select(LEVEL2_DATA_NAMES[["AgCoef"]]) ->
      L2062.AgCoef_Fert_ag_irr_mgmt

    # Copy final base year coefficients to all future years, bind with historic coefficients, then remove zeroes
    # Note: this assumes constant fertilizer coefficients in the future
    L2062.AgCoef_Fert_ag_irr_mgmt %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L2062.AgCoef_Fert_ag_irr_mgmt) ->
      L2062.AgCoef_Fert_ag_irr_mgmt

    # Calculate fertilizer coefficients for grassy bioenergy crops
    A_Fodderbio_chars %>%
      filter(GCAM_commodity == "biomassGrass") %>%
      mutate(coefficient = (aglu.BIO_GRASS_FERT_IO_GNM2 * CONV_G_KG / aglu.BIO_GRASS_YIELD_KGCM2    # Convert from application per unit area to per unit carbon
                            * aglu.CCONTENT_CELLULOSE * (1 - WaterContent))                         # Convert from carbon to wet biomass
             / (aglu.BIO_ENERGY_CONTENT_GJT * CONV_KG_T)) ->                         # Convert from biomass to energy
      bio_grass_coef

    # Calculate fertilizer coefficients for tree bioenergy crops
    A_Fodderbio_chars %>%
      filter(GCAM_commodity == "biomassTree") %>%
      mutate(coefficient = (aglu.BIO_TREE_FERT_IO_GNM2 * CONV_G_KG / aglu.BIO_TREE_YIELD_KGCM2    # Convert from application per unit area to per unit carbon
                            * aglu.CCONTENT_CELLULOSE * (1 - WaterContent))                         # Convert from carbon to wet biomass
             / (aglu.BIO_ENERGY_CONTENT_GJT * CONV_KG_T)) ->                         # Convert from biomass to energy
      bio_tree_coef

    # Replace GLU names and Add region names and estimating median IO coef for region and basin
    L143.AgFertIO_Ratio_R_GLU <- L143.AgFertIO_Ratio_R_C_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      group_by(region, GLU) %>%
      summarise(FERT_IO_RATIO_HI_LO = median(FERT_IO_RATIO_HI_LO)) %>%
      ungroup()

    # Map fertilizer coefficients to all bioenergy technologies
    # Differentiate the bioenergy crop fertilizer IO coefs according to mgmt level
    # For bioenergy crops, there isn't any need to worry about "output"
    # The equation for coef_lo simplifies to coef_avg * 2 / (Ratio + 1). coef_hi is twice this (4 / (Ratio + 1))

    L2052.AgCost_bio_irr_mgmt %>%
      select(-nonLandVariableCost) %>%                  # We are just using this data.frame to get the region/sector/tech names
      separate(AgSupplySubsector, c("Crop", "GLU"), sep="_", remove=FALSE) %>%
      select(-Crop) %>%
      left_join(L143.AgFertIO_Ratio_R_GLU, by=c("region", "GLU")) %>%
      replace_na(list(FERT_IO_RATIO_HI_LO = aglu.FERT_IO_RATIO_HI_LO)) %>%
      mutate(minicam.energy.input = "N fertilizer",
             coefficient = if_else(grepl("^biomass_grass", AgSupplySubsector),
                                   bio_grass_coef$coefficient, bio_tree_coef$coefficient),
             coefficient = round(if_else(grepl("_lo", AgProductionTechnology),
                                         coefficient * 2 / (FERT_IO_RATIO_HI_LO + 1),
                                         coefficient * 2 * FERT_IO_RATIO_HI_LO / (FERT_IO_RATIO_HI_LO + 1)),
                                 aglu.DIGITS_CALOUTPUT)) %>%
      select(LEVEL2_DATA_NAMES[["AgCoef"]]) ->
      L2062.AgCoef_Fert_bio_irr_mgmt

    # Adjust nonLandVariableCost to separate fertilizer cost (which is accounted for specifically)
    L2052.AgCost_ag_irr_mgmt %>%
      # Note: using left_join because there are instances with cost but no fertilizer use.
      left_join(L2062.AgCoef_Fert_ag_irr_mgmt,
                by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%

      # Set fertilizer coefficient to zero when missing. This will lead to zero fertilizer cost.
      replace_na(list(coefficient = 0)) %>%

      # Calculate fertilizer cost using a fixed value (specified in constants.R in current $ per ton of NH3)
      # and the fertilizer coefficient calculated above. Subtract from original nonLandVariableCost.
      mutate(FertCost = coefficient * aglu.FERT_PRICE * gdp_deflator(1975, aglu.FERT_PRICE_YEAR) * CONV_KG_T / CONV_NH3_N,
             nonLandVariableCost = round(nonLandVariableCost - FertCost, aglu.DIGITS_CALPRICE)) %>%
      select(-minicam.energy.input, -coefficient, -FertCost) ->
      L2062.AgCost_ag_irr_mgmt_adj

    # Adjust nonLandVariableCost to separate fertilizer cost (which is accounted for specifically)
    L2052.AgCost_bio_irr_mgmt %>%
      # Note: using left_join because there are instances with cost but no fertilizer use
      left_join(L2062.AgCoef_Fert_bio_irr_mgmt,
                by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%

      # Set fertilizer coefficient to zero when missing. This will lead to zero fertilizer cost.
      replace_na(list(coefficient = 0)) %>%

      # Calculate fertilizer cost using a fixed value (specified in constants.R in 2007$ per ton of NH3)
      # and the fertilizer coefficient calculated above. Subtract from original nonLandVariableCost.
      mutate(FertCost = coefficient * aglu.FERT_PRICE * gdp_deflator(1975, 2007) * CONV_KG_T / CONV_NH3_N,
             nonLandVariableCost = round(nonLandVariableCost - FertCost, aglu.DIGITS_CALPRICE)) %>%
      select(-minicam.energy.input, -coefficient, -FertCost) ->
      L2062.AgCost_bio_irr_mgmt_adj

    # Produce outputs
    L2062.AgCoef_Fert_ag_irr_mgmt %>%
      add_title("Fertilizer coefficients for agricultural technologies") %>%
      add_units("kgN per kg crop") %>%
      add_comments("Map fertilizer coefficients in L142.ag_Fert_IO_R_C_Y_GLU to all technologies") %>%
      add_comments("Note: we are using the same coefficient for all four management technologies (irrigated, rainfed, hi and lo") %>%
      add_legacy_name("L2062.AgCoef_Fert_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L142.ag_Fert_IO_R_C_Y_GLU",
                     "L143.AgFertIO_Ratio_R_C_GLU",
                     "L2012.AgProduction_ag_irr_mgmt") ->
      L2062.AgCoef_Fert_ag_irr_mgmt
    L2062.AgCoef_Fert_bio_irr_mgmt %>%
      add_title("Fertilizer coefficients for bioenergy technologies") %>%
      add_units("kgN per GJ") %>%
      add_comments("Compute bioenergy fertilizer coefficients from read-in constants") %>%
      add_comments("Note: L2052.AgCost_bio_irr_mgmt is only used to identify all bioenergy technologies") %>%
      add_legacy_name("L2062.AgCoef_Fert_bio_irr_mgmt") %>%
      add_precursors("aglu/A_Fodderbio_chars",
                     "L2052.AgCost_bio_irr_mgmt") ->
      L2062.AgCoef_Fert_bio_irr_mgmt
    L2062.AgCost_ag_irr_mgmt_adj %>%
      add_title("Adjusted non-land variable cost for agricultural technologies") %>%
      add_units("1975$ per kg") %>%
      add_comments("Subtract cost of fertilizer from non-land variable cost.") %>%
      add_comments("Fertilizer costs is computed using a fixed NH3 cost and the fertilizer coefficient") %>%
      add_legacy_name("L2062.AgCost_ag_irr_mgmt_adj") %>%
      same_precursors_as(L2062.AgCoef_Fert_ag_irr_mgmt) %>%
      add_precursors("L2052.AgCost_ag_irr_mgmt") ->
      L2062.AgCost_ag_irr_mgmt_adj
    L2062.AgCost_bio_irr_mgmt_adj %>%
      add_title("Adjusted non-land variable cost for agricultural technologies") %>%
      add_units("1975$ per GJ") %>%
      add_comments("Subtract cost of fertilizer from non-land variable cost.") %>%
      add_comments("Fertilizer costs is computed using a fixed NH3 cost and the fertilizer coefficient") %>%
      add_legacy_name("L2062.AgCost_bio_irr_mgmt_adj") %>%
      same_precursors_as(L2062.AgCoef_Fert_bio_irr_mgmt) %>%
      add_precursors("L2052.AgCost_bio_irr_mgmt")  ->
      L2062.AgCost_bio_irr_mgmt_adj

    return_data(L2062.AgCoef_Fert_ag_irr_mgmt, L2062.AgCoef_Fert_bio_irr_mgmt, L2062.AgCost_ag_irr_mgmt_adj, L2062.AgCost_bio_irr_mgmt_adj)
  } else {
    stop("Unknown command")
  }
}
