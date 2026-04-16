# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L143.ag_FertManure
#'
#'  Aggregate FAO Nitrogen content in manure to GCAM livestock and crop types by country / year
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

# L143.ag_NManure_IO_R_C_Y_GLU

    GCAMFAOSTAT_NManure_long <- GCAMFAOSTAT_NManure %>%
      pivot_longer(
        cols = `1961`:`2023`,
        names_to = "year",
        values_to = "FAO_Value_kg"
      ) %>%
      mutate(year = as.integer(year),
             FAO_Value_Mt = FAO_Value_kg / 1e9)

    drop_FAO <- c(
      "World", "Africa", "Eastern Africa", "Middle Africa",
      "Northern Africa", "Southern Africa", "Sub-Saharan Africa",
      "Western Africa", "Americas", "Northern America", "Central America",
      "Caribbean", "South America", "Latin America and the Caribbean",
      "Asia", "Central Asia", "Eastern Asia", "Southern Asia",
      "South-eastern Asia", "Western Asia", "Europe", "Eastern Europe",
      "Northern Europe", "Southern Europe", "Western Europe", "Oceania",
      "Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia",
      "European Union (27)", "Least Developed Countries (LDCs)",
      "Land Locked Developing Countries (LLDCs)",
      "Small Island Developing States (SIDS)",
      "Low Income Food Deficit Countries (LIFDCs)",
      "Net Food Importing Developing Countries (NFIDCs)",
      "Annex I countries", "Non-Annex I countries", "OECD",
      "Belgium-Luxembourg", "Czechoslovakia", "Ethiopia PDR",
      "Serbia and Montenegro", "Sudan (former)", "USSR", "Yugoslav SFR", "China"
    )

    GCAMFAOSTAT_NManure_clean <- GCAMFAOSTAT_NManure_long |>
      dplyr::filter(!FAO_country %in% drop_FAO)

    # Join to iso_GCAM_regID to get iso and GCAM_region_ID to FAO_country
    AGLU_ctry1 <- select(AGLU_ctry, FAO_country, iso) %>%
      filter(FAO_country %in% GCAMFAOSTAT_NManure_clean$FAO_country) %>%
      distinct()

    #Need to make sure AGLU_ctry and GCAM_region_ID have the necessary FAO_countries to match
    #for example, I think United Kingdom has 3 different names in each csv file, so it won't join naturally

    #diff_AGLU_ctry_not_iso_GCAM_regID <- anti_join(AGLU_ctry, iso_GCAM_regID)
    #diff_iso_GCAM_regID_not_AGLU_ctry <- anti_join(iso_GCAM_regID, AGLU_ctry)


    GCAMFAOSTAT_ID <- GCAMFAOSTAT_NManure_clean %>%
      left_join(AGLU_ctry1, by = "FAO_country")%>%
      left_join(
        iso_GCAM_regID,
        by = "iso"
      )

    # Only keep the total of animals that are in the livestock sector of GCAM
    required_elements <- c(
      "Cattle, dairy",
      "Cattle, non-dairy",
      "Buffalo",
      "Swine total",
      "Chickens total",
      "Ducks",
      "Turkey",
      "Sheep and Goats total"
    )

    # NManure by GCAM region and year
    L143.NManure_R_Y <- GCAMFAOSTAT_ID %>%
      filter(element %in% required_elements,
             year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(NManure_Mt = sum(FAO_Value_Mt)) %>%
      ungroup()

    # Land shares: share of land by crop/subsector/GLU within each GCAM region and year
    L143.LandShares_R_C_Y_GLU <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      filter(year %in% L143.NManure_ctry_yr$year) %>%
      group_by(GCAM_region_ID, year) %>%
      mutate(landshare = value / sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, landshare)

    # NManure quantity by crop/subsector/GLU: NManure by GCAM region and year times land shares
    L143.NManure_Mt_R_C_GLU_Y <- L143.LandShares_R_C_Y_GLU %>%
      left_join_error_no_match(L143.NManure_R_Y, by = c("GCAM_region_ID", "year")) %>%
      mutate(NManure_Mt = NManure_Mt * landshare) %>%
      select(-landshare)




    #calculate shares of land per commodity, per GLU, per region, per year
    shares <- merged_df %>%
      group_by(year, GCAM_region_ID) %>%
      mutate(
        total_year_region_glu_commodity = sum(value, na.rm = TRUE),
        share = value / total_year_region_glu_commodity
      ) %>%
      ungroup()


    shares_with_manure <- shares %>%
      mutate(
        manure_allocated = share * FAO_Value_Mt
      )

    # merge datasets
    merged_df_1 <-  shares_with_manure %>%
      left_join(
        L101.ag_Prod_Mt_R_C_Y_GLU,
        by = c("GCAM_region_ID", "year", "GLU", "GCAM_commodity"),
        relationship = "many-to-many")

    # L101.ag_Prod_Mt_R_C_Y_GLU only has data from 1973-2023, so I'm removing years 1961-1972
    L143.ag_NManure_IO_R_C_Y_GLU <- merged_df_1 %>%
      mutate(
        NManure_IO = manure_allocated / value.y,
        NManure_IO = replace_na(NManure_IO, 0)
      ) %>%
      filter(year >= 1973, year <= 2023) %>%
      select(GCAM_region_ID, GCAM_commodity, year, GLU, NManure_IO)


# L143.an_NManure_SecOut_MtNperMt_R_C_Y


    GCAMFAOSTAT_NManure_single_lvstk <- GCAMFAOSTAT_ID %>%
      filter(element %in% required_elements)

    # there are
    merged_df_2 <- GCAMFAOSTAT_NManure_single_lvstk %>%
      left_join(FAO_an_types_manure,
                by = c("element"),
                relationship = "many-to-one")

    # Join the data sets
    merged_df_3<- merged_df_2 %>%
      left_join(L109.an_ALL_Mt_R_C_Y_kg,
                by = c("GCAM_region_ID", "year", "GCAM_commodity"),
                relationship = "many-to-many")

    L143.an_NManure_SecOut_MtNperMt_R_C_Y <- merged_df_3 %>%
      mutate(
        NManure_SecOut = FAO_Value_Mt / Prod_Mt,
        NManure_SecOut = replace_na(NManure_SecOut, 0)
      ) %>%
      select(GCAM_region_ID, GCAM_commodity, year, NManure_SecOut)

# L143.an_NManure_Mt_R_C_Y

    L143.an_NManure_Mt_R_C_Y <- merged_df %>%
      select(GCAM_region_ID, GCAM_commodity, year, FAO_Value_Mt)


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
      add_units("Unit = Mt N manure per Mt animal commodity produced") %>%
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
