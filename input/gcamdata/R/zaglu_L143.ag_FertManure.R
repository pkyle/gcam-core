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
      "L143.an_NManure_SecOut_MtNperMt_R_C_Y",
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

    # Make the wide table to long, with 'year' as a single column, and 'FAO_Value_kg' as a
    # single column, converting Nmanure values from kg to Mt in column 'FAO_Value_Mt'
    GCAMFAOSTAT_NManure_long <- GCAMFAOSTAT_NManure %>%
      pivot_longer(
        cols = `1961`:`2023`,
        names_to = "year",
        values_to = "FAO_Value_kg"
      ) %>%
      mutate(year = as.integer(year),
             FAO_Value_Mt = FAO_Value_kg / 1e9)

    # Dropping totaled areas in FAO such as whole continents, whole regions, and
    # country titles that represent several countries that are already represented in
    # the dataset. This allows us to avoid including duplicates in our calculations.
    drop_from_FAO <- c(
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

    # Removing rows with the above total areas from the long NManure dataset. This results
    # in unique countries in the column 'FAO_country'
    GCAMFAOSTAT_NManure_unique_countries <- GCAMFAOSTAT_NManure_long |>
      dplyr::filter(!FAO_country %in% drop_from_FAO)

    # Identifying the distinct "FAO_country" and "iso" combinations in NManure that
    # can be satisfied by AGLU
    AGLU_ctry_NManure_unique <- select(AGLU_ctry, FAO_country, iso) %>%
      filter(FAO_country %in% GCAMFAOSTAT_NManure_unique_countries$FAO_country) %>%
      distinct()

    # Join NManure to AGLU_ctry by the column 'FAO_country' to attribute 'iso' codes to
    # each unique 'FAO_country'. Join NManure to iso_GCAM_regID by 'iso' to have GCAM regions associated.
    GCAMFAOSTAT_GCAM_region_ID <- GCAMFAOSTAT_NManure_unique_countries %>%
      left_join(AGLU_ctry_NManure_unique, by = "FAO_country")%>%
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

    # NManure aggregated by GCAM region and year, only keeping the required animal types associated to commodities in GCAM
    # listed above and years pertinent for the GCAM model
    L143.NManure_R_Y <- GCAMFAOSTAT_GCAM_region_ID %>%
      filter(element %in% required_elements,
             year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(NManure_Mt = sum(FAO_Value_Mt)) %>%
      ungroup()

    # Land shares: share of land by crop/subsector/GLU within each GCAM region and year
    L143.LandShares_R_C_Y_GLU <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      filter(year %in% L143.NManure_R_Y$year) %>%
      group_by(GCAM_region_ID, year) %>%
      mutate(landshare = value / sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, landshare)

    # NManure quantity by crop/subsector/GLU: NManure by GCAM region and year times land shares
    L143.NManure_Mt_R_C_GLU_Y <- L143.LandShares_R_C_Y_GLU %>%
      left_join_error_no_match(L143.NManure_R_Y, by = c("GCAM_region_ID", "year")) %>%
      mutate(NManure_Mt = NManure_Mt * landshare) %>%
      select(-landshare)

    # NManure multipled by land shares joined to crop production quantities
    L143.NManure_Mt_R_C_GLU_Y_Crop_Prod <-   L143.NManure_Mt_R_C_GLU_Y %>%
      left_join_error_no_match(
        L101.ag_Prod_Mt_R_C_Y_GLU,
        by = c("GCAM_region_ID","GCAM_commodity", "GCAM_subsector", "GLU", "year"))

    # Calculating Input/Output coefficients (NManure divided by production quantity)
    L143.ag_NManure_IO_R_C_Y_GLU <- L143.NManure_Mt_R_C_GLU_Y_Crop_Prod %>%
      mutate(
        NManure_IO = NManure_Mt / value,
        NManure_IO = replace_na(NManure_IO, 0)
      ) %>%
      select(GCAM_region_ID, GCAM_commodity,GCAM_subsector,GLU, year,NManure_IO)


# L143.an_NManure_SecOut_MtNperMt_R_C_Y

    # NManure aggregated by GCAM region and year, only keeping the required animal types associated to commodities in GCAM
    # listed above
    L143.NManure_R_Y_Lvstk <- GCAMFAOSTAT_GCAM_region_ID %>%
      filter(element %in% required_elements,
             year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, year, element) %>%
      summarise(NManure_Mt = sum(FAO_Value_Mt)) %>%
      ungroup()

    # NManure aggregated by GCAM region and year with required animal types, aggregated to animal commodities in GCAM
    L143.NManure_R_Y_Lvstk_Commodity <- L143.NManure_R_Y_Lvstk %>%
      left_join_error_no_match(FAO_an_types_manure,
                by = "element") %>%
    group_by(GCAM_region_ID, year, GCAM_commodity) %>%
      summarise(NManure_Mt_commodity = sum(NManure_Mt)) %>%
      ungroup()

    # NManure aggregated by GCAM region and year with animal commodities joined to food (animal commodity) production in Mt
    L143.NManure_R_Y_Lvstk_Commodity_Mt <- L143.NManure_R_Y_Lvstk_Commodity  %>%
      left_join_error_no_match(L109.an_ALL_Mt_R_C_Y,
                by = c("GCAM_region_ID", "year", "GCAM_commodity"))

    # Calculating the secondary output of NManure (NManure content Mt / animal commodity Mt)
    L143.an_NManure_SecOut_MtNperMt_R_C_Y <- L143.NManure_R_Y_Lvstk_Commodity_Mt %>%
      mutate(
        NManure_SecOut = NManure_Mt_commodity / Prod_Mt,
        NManure_SecOut = replace_na(NManure_SecOut, 0)
      ) %>%
      select(GCAM_region_ID, GCAM_commodity, year, NManure_SecOut)


# L143.an_NManure_Mt_R_C_Y

    # NManure aggregated by GCAM region and year with animal commodities to food (animal commodity) production in Mt
    L143.an_NManure_Mt_R_C_Y <- L143.NManure_R_Y_Lvstk_Commodity_Mt %>%
      select(GCAM_region_ID, GCAM_commodity, year,NManure_Mt_commodity)


    # Produce outputs
    L143.ag_NManure_IO_R_C_Y_GLU %>%
      add_title("N manure application per unit crop production by region/crop/year/GLU") %>%
      add_units("Unit = Mt of manure N per Mt crop production") %>%
      add_comments("Manure nitrogen is aggregated from all animal types") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_NManure",
                     "aglu/FAO/FAO_an_types_manure",
                     "aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU") ->
      L143.ag_NManure_IO_R_C_Y_GLU

    L143.an_NManure_SecOut_MtNperMt_R_C_Y %>%
      add_title("N manure secondary output coefficients") %>%
      add_units("Unit = Mt N manure per Mt animal commodity produced") %>%
      add_comments("N manure produced divided by production of each animal commodity by region and year") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_NManure",
                     "aglu/FAO/FAO_an_types_manure",
                     "aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L143.an_NManure_SecOut_MtNperMt_R_C_Y

    L143.an_NManure_Mt_R_C_Y %>%
      add_title("N manure production") %>%
      add_units("Unit = Mt N") %>%
      add_comments("N manure produced by region and animal commodity type") %>%
      same_precursors_as(L143.an_NManure_SecOut_MtNperMt_R_C_Y) ->
      L143.an_NManure_Mt_R_C_Y

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
