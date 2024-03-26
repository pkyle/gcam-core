# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L222.en_transformation
#'
#' Prepare the assumptions and calibrated outputs for energy transformation supplysectors, subsectors, and technologies specific to USA sectors and/or states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L222.DeleteStubTech_USAen}, \code{L222.PassThroughSector_USAen}, \code{L222.Tech_USAen},
#' \code{L222.TechShrwt_USAen}, \code{L222.TechInterp_USAen}, \code{L222.TechCoef_USAen},
#' \code{L222.Production_USArefining}, \code{L222.SectorLogitTables_USA[[ curr_table ]]$data},
#' \code{L222.Supplysector_en_USA}, \code{L222.SubsectorShrwtFllt_en_USA}, \code{L222.StubTechProd_refining_USA},
#' \code{L222.StubTechMarket_en_USA}, \code{L222.CarbonCoef_en_USA}, \code{L222.GlobalTechSCurve_en_USA},
#' \code{L222.GlobalTechProfitShutdown_en_USA}, \code{L222.GlobalTechCost_en_USA}, \code{L222.SubsectorLogit_en_USA},
#' \code{L222.StubTech_en_USA}, \code{L222.StubTechCoef_refining_USA}, \code{L222.GlobalTechInterp_en_USA},
#' \code{L222.GlobalTechCoef_en_USA}, \code{L222.GlobalTechShrwt_en_USA}, \code{L222.GlobalTechCapture_en_USA}.
#' The corresponding file in the original data system was \code{L222.en_transformation_USA.R} (gcam-usa level2).
#' @details This chunk sets up the USA energy transformation technology databases as well as writing out assumptions to all states/sectors/markets for shareweights and logits.
#' Calibrated outputs and I:O coefficients are updated from global values produced by \code{\link{module_energy_L222.en_transformation}}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate one_of pull select summarise
#' @importFrom tidyr separate unite
#' @author ACS Nov 2017
module_gcamusa_L222.en_transformation <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/SEDS_refining_feedstock_prod",
             "L222.Supplysector_en",
             "L222.SubsectorLogit_en",
             "L222.StubTech_en",
             "L222.StubTechCoef_refining",
             "L222.StubTechProd_refining",
             "L222.GlobalTechInterp_en",
             "L222.GlobalTechCoef_en",
             "L222.GlobalTechCost_en",
             "L222.GlobalTechShrwt_en",
             "L222.GlobalTechCapture_en",
             "L222.GlobalTechSCurve_en",
             "L222.GlobalTechProfitShutdown_en",
             "L122.out_EJ_state_refining_F",
             "L202.CarbonCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L222.DeleteStubTech_USAen",
             "L222.PassThroughSector_USAen",
             "L222.Tech_USAen",
             "L222.TechShrwt_USAen",
             "L222.TechInterp_USAen",
             "L222.TechCoef_USAen",
             "L222.Production_USArefining",
             "L222.Supplysector_en_USA",
             "L222.SubsectorShrwtFllt_en_USA",
             "L222.StubTechProd_refining_USA",
             "L222.StubTechMarket_en_USA",
             "L222.CarbonCoef_en_USA",
             "L222.GlobalTechSCurve_en_USA",
             "L222.GlobalTechProfitShutdown_en_USA",
             "L222.GlobalTechCost_en_USA",
             "L222.SubsectorLogit_en_USA",
             "L222.StubTech_en_USA",
             "L222.StubTechCoef_refining_USA",
             "L222.GlobalTechInterp_en_USA",
             "L222.GlobalTechCoef_en_USA",
             "L222.GlobalTechShrwt_en_USA",
             "L222.GlobalTechCapture_en_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    SEDS_refining_feedstock_prod <- get_data(all_data, "gcam-usa/SEDS_refining_feedstock_prod")
    L222.Supplysector_en <- get_data(all_data, "L222.Supplysector_en", strip_attributes = TRUE)
    L222.SubsectorLogit_en <- get_data(all_data, "L222.SubsectorLogit_en", strip_attributes = TRUE)
    L222.StubTech_en <- get_data(all_data, "L222.StubTech_en", strip_attributes = TRUE)
    L222.StubTechCoef_refining <- get_data(all_data, "L222.StubTechCoef_refining", strip_attributes = TRUE)
    L222.StubTechProd_refining <- get_data(all_data, "L222.StubTechProd_refining", strip_attributes = TRUE)
    L222.GlobalTechInterp_en <- get_data(all_data, "L222.GlobalTechInterp_en", strip_attributes = TRUE)
    L222.GlobalTechCoef_en <- get_data(all_data, "L222.GlobalTechCoef_en", strip_attributes = TRUE)
    L222.GlobalTechCost_en <- get_data(all_data, "L222.GlobalTechCost_en", strip_attributes = TRUE)
    L222.GlobalTechShrwt_en <- get_data(all_data, "L222.GlobalTechShrwt_en", strip_attributes = TRUE)
    L222.GlobalTechCapture_en <- get_data(all_data, "L222.GlobalTechCapture_en", strip_attributes = TRUE)
    L222.GlobalTechSCurve_en <- get_data(all_data, "L222.GlobalTechSCurve_en", strip_attributes = TRUE)
    L222.GlobalTechProfitShutdown_en <- get_data(all_data, "L222.GlobalTechProfitShutdown_en", strip_attributes = TRUE)
    L122.out_EJ_state_refining_F <- get_data(all_data, "L122.out_EJ_state_refining_F", strip_attributes = TRUE)
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef", strip_attributes = TRUE)

    # silence check package notes
    logit.year.fillout <- year <- from.year <- to.year <- region <- supplysector <- subsector <-
      technology <- sector.name <- subsector.name <- sector <- state <- fuel <- value <- market.name <-
      calOutputValue <- minicam.energy.input <- grid_region <- stub.technology <- coal <- natural_gas <- coal_fract <-
      share.weight <- gas_fract <- NULL

    # Some helpful functions:
    #
    # global_energy_to_USA_nonGlobalTech - takes global energy inputs for non global tech
    # from L222.en_transformation.R and processes for use in USA
    global_energy_to_USA_nonGlobalTech <- function(data) {
      data %>%
        filter(region == gcam.USA_REGION,
               supplysector %in% gcamusa.SECTOR_EN_NAMES) %>%
        write_to_all_states(names = c(names(data), "region")) %>%
        filter((subsector == "oil refining" & region %in% oil_refining_states) |
                 subsector != "oil refining") %>%
        mutate(supplysector = paste(supplysector, subsector))
    } # global_energy_to_USA_nonGlobalTech

    # global_energy_to_USA_GlobalTech - takes global energy inputs for global tech
    # from L222.en_transformation.R and processes for use in USA
    global_energy_to_USA_GlobalTech <- function(data) {
      data %>%
        filter(sector.name %in% gcamusa.SECTOR_EN_NAMES) %>%
        mutate(sector.name = paste(sector.name, subsector.name))
    } # global_energy_to_USA_GlobalTech

    # Oil refining and aviation fuels sectors are only created in states where the production is > 0 in the historical period.
    # Collect these states. Other techs are available everywhere
    L122.out_EJ_state_refining_F %>%
      filter(sector == "oil refining",
             year %in% MODEL_BASE_YEARS) %>%
      group_by(state, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      filter(value > 0) %>%
      pull(state) ->
      oil_refining_states

    # L222.DeleteStubTech_USAen: remove existing stub technologies in the USA region.
    # The supplysector and subsector structure in the sectors defined in gcamusa.SECTOR_EN_NAMES are retained
    L222.StubTech_en %>%
      filter(region == gcam.USA_REGION,
             supplysector %in% gcamusa.SECTOR_EN_NAMES) ->
      L222.DeleteStubTech_USAen

    # L222.Tech_USAen: Just the technology pass-throughs used to set the proper node name, USA region
    L222.SubsectorLogit_en %>%
      select(region, supplysector, subsector) %>%
      filter(region == gcam.USA_REGION,
             supplysector %in% gcamusa.SECTOR_EN_NAMES) %>%
      repeat_add_columns(tibble(state = gcamusa.STATES)) %>%
      filter((subsector == "oil refining" & state %in% oil_refining_states) |
               subsector != "oil refining") %>%
      mutate(technology = paste(state, supplysector, subsector, sep = gcamusa.STATE_SUBSECTOR_DELIMITER)) ->
      L222.Tech_USAen

    # save some of this information for the PassThroughSector information
    # L222.PassThroughSector_USAen: PassThroughSector information to send vintaging info from states to USA.
    L222.Tech_USAen %>%
      mutate(marginal.revenue.market = region,
             region = state,
             pass.through.sector = paste(supplysector, subsector),
             marginal.revenue.sector = supplysector) %>%
      select(region, pass.through.sector, marginal.revenue.sector, marginal.revenue.market) %>%
      distinct() ->
      L222.PassThroughSector_USAen

    # select only relevant columns for L222.Tech_USAen, particularly dropping state
    L222.Tech_USAen %>%
      select(one_of(LEVEL2_DATA_NAMES[["Tech"]])) ->
      L222.Tech_USAen

    # L222.TechInterp_USAen: technology shareweights, USA region
    # Technology interpolation only applies to calibrated technologies.
    # For biomass liquids, allow state shares to shift over time
    # (future techs are different than present techs).
    # Oil refining and biomass liquids shareweights are fixed at calibration values through max model year
    L222.Tech_USAen %>%
      filter(subsector %in% c("oil refining", "biomass liquids")) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L222.TechInterp_USAen

    # L222.TechShrwt_USAen: technology shareweights in each year, USA region
    # Most of these are derived through calibration, but those that can't be are inferred through other data.
    # gas to liquids and coal to liquids are based on the state-wise use of coal and gas feedstocks
    # aviation fuels / biomass liquids is based on refining/biomass liquids
    state_af_bio_shareweights <- L122.out_EJ_state_refining_F %>%
      filter(year == max(year),
             sector %in% c("corn ethanol", "biodiesel")) %>%
      group_by(state) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(af.bio.share.weight = value / max(value)) %>%
      select(state, af.bio.share.weight)

    L222.Tech_USAen %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      # Split the state names out and because the refining tech names have spaces drop the extra
      separate(technology, c("state"), sep = " ", remove = F, extra = "drop") %>%
      left_join_error_no_match(SEDS_refining_feedstock_prod, by = c("state")) %>%
      left_join_error_no_match(state_af_bio_shareweights, by = "state") %>%
      mutate(coal_fract = coal / max(coal), gas_fract = natural_gas / max(natural_gas),
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT,
             # Scaling coal to liquids and gas to liquids shareweights to 2015 resource production levels
             share.weight = if_else(grepl("coal", subsector), coal_fract, share.weight),
             share.weight = if_else(grepl("gas", subsector), gas_fract, share.weight),
             share.weight = if_else(supplysector == "aviation fuels" & subsector == "biomass liquids", af.bio.share.weight, share.weight),
             # Default the base year shareweights to 0. This will be over-ridden in calibration,
             share.weight = if_else(year %in% MODEL_BASE_YEARS, 0, round(share.weight, energy.DIGITS_SHRWT))) %>%
      select(region, supplysector, subsector, technology, year, share.weight) -> L222.TechShrwt_USAen


    # L222.TechCoef_USAen: technology coefficients and market names, USA region
    L222.TechShrwt_USAen %>%
      select(one_of(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
      mutate(minicam.energy.input = substr(technology, 4, nchar(technology)),
             coefficient = gcamusa.DEFAULT_COEFFICIENT,
             market.name = substr(technology, 1, 2)) ->
      L222.TechCoef_USAen

    # L222.Production_USArefining: calibrated refinery production in USA (consuming output of states)
    L122.out_EJ_state_refining_F %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(subsector = if_else(sector == "oil refining", "oil refining", "biomass liquids")) %>%
      group_by(state, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      group_by(subsector, year) %>%
      mutate(state_share = value / sum(value)) %>%
      ungroup() %>%
      replace_na(list(state_share = 0)) %>%
      select(state, subsector, year, state_share) ->
      L222.State_Refining_Shares

    # Start from the calibrated output for the whole USA, aggregate by subsector, and expand-join to states.
    # The calOutputValues are the national output (by subsector) times the state-level shares
    L222.StubTechProd_refining %>%
      filter(region == gcam.USA_REGION,
             subsector %in% c("oil refining", "biomass liquids")) %>%
      group_by(region, supplysector, subsector, year) %>%
      summarise(national_total = sum(calOutputValue)) %>%
      ungroup() %>%
      left_join(L222.Tech_USAen, by = c("region", "supplysector", "subsector")) %>%
      mutate(state = substr(technology, 1, 2)) %>%
      left_join_error_no_match(L222.State_Refining_Shares, by = c("state", "subsector", "year")) %>%
      mutate(calOutputValue = round(national_total * state_share, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L222.Production_USArefining

      # Process energy files from L222.en_transformation.R for use in the USA,
      # slightly differently processing for global tech vs not inputs
      L222.SubsectorLogit_en_USA      <- global_energy_to_USA_nonGlobalTech(L222.SubsectorLogit_en)
      L222.StubTech_en_USA            <- global_energy_to_USA_nonGlobalTech(L222.StubTech_en)
      L222.StubTechCoef_refining_USA  <- global_energy_to_USA_nonGlobalTech(L222.StubTechCoef_refining)
      L222.GlobalTechInterp_en_USA    <- global_energy_to_USA_GlobalTech(L222.GlobalTechInterp_en)
      L222.GlobalTechCoef_en_USA      <- global_energy_to_USA_GlobalTech(L222.GlobalTechCoef_en)
      L222.GlobalTechCost_en_USA      <- global_energy_to_USA_GlobalTech(L222.GlobalTechCost_en)
      L222.GlobalTechShrwt_en_USA     <- global_energy_to_USA_GlobalTech(L222.GlobalTechShrwt_en)
      L222.GlobalTechCapture_en_USA   <- global_energy_to_USA_GlobalTech(L222.GlobalTechCapture_en)
      L222.GlobalTechSCurve_en_USA    <- global_energy_to_USA_GlobalTech(L222.GlobalTechSCurve_en)

      if(!is.null(L222.GlobalTechProfitShutdown_en)) {
        L222.GlobalTechProfitShutdown_en_USA <- global_energy_to_USA_GlobalTech(L222.GlobalTechProfitShutdown_en)
      }

      # TODO: figure out a better strategy.  We need to have at least one technology be available in the final
      # calibration year so we can get a base cost for the absolute cost logit.  Having a share weight of zero
      # at the subsector is sufficient then to ensure we get no production in the calibration years
      L222.GlobalTechShrwt_en_USA %>%
        mutate(share.weight = if_else(technology == "coal to liquids" & year == max(MODEL_BASE_YEARS), 1.0, share.weight),
               share.weight = if_else(technology == "gas to liquids" & year == max(MODEL_BASE_YEARS), 1.0, share.weight),
               share.weight = if_else(sector.name == "aviation fuels biomass liquids" & technology == "cellulosic ethanol" & year == max(MODEL_BASE_YEARS),
                                      1.0, share.weight)) ->
        L222.GlobalTechShrwt_en_USA

      # L222.Supplysector_en_USA: Supplysector information, replace name of supplysector with the subsector names
      # As there is no competition between subsectors, just use the default logit type
      L222.Supplysector_en %>%
        left_join(distinct(L222.SubsectorLogit_en, supplysector, subsector),
                  by = "supplysector") %>%
        global_energy_to_USA_nonGlobalTech() ->
        L222.Supplysector_en_USA

      # L222.SubsectorShrwtFllt_en_USA: Subsector shareweights, there is no competition here, so just fill out with 1s
      # (will be over-ridden by base year calibration where necessary)
      L222.SubsectorLogit_en_USA %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        mutate(year = min(MODEL_YEARS),
               share.weight = gcamusa.DEFAULT_SHAREWEIGHT) ->
        L222.SubsectorShrwtFllt_en_USA

      # L222.StubTechProd_refining_USA: calibrated fuel production by state.
      # Calibrated production is equal to the USA total by each state's share
      L122.out_EJ_state_refining_F %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        group_by(sector, year) %>%
        mutate(state_share = value / sum(value)) %>%
        ungroup() %>%
        replace_na(list(state_share = 0)) %>%
        select(state, stub.technology = sector, year, state_share) ->
        L222.State_Refining_Tech_Shares

      L222.StubTechProd_refining %>%
        filter(region == "USA") %>%
        inner_join(L222.State_Refining_Tech_Shares,
                  by = c("stub.technology", "year")) %>%
        filter(subsector != "oil refining" |
                 subsector == "oil refining" & state %in% oil_refining_states) %>%
        mutate(region = state,
               supplysector = paste(supplysector, subsector),
               calOutputValue = round(calOutputValue * state_share, energy.DIGITS_CALOUTPUT),
               tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
        select(-state, -state_share) %>%
        set_subsector_shrwt() ->
        L222.StubTechProd_refining_USA

      # L222.StubTechMarket_en_USA: market names of inputs to state refining sectors
      # Start with the list of stub-technologies read to each state, expand-join by years and inputs, and
      # set the market names (default USA, replace by grid region and state as indicated)
      L222.StubTech_en_USA %>%
        left_join(L222.GlobalTechCoef_en_USA,
                  by = c(supplysector = "sector.name", subsector = "subsector.name", stub.technology = "technology")) %>%
        select(-coefficient) %>%
        mutate(market.name = gcam.USA_REGION) %>%
        left_join_error_no_match(select(states_subregions, state, grid_region),
                                 by = c("region" = "state")) %>%
        mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                     grid_region, market.name),
               market.name = if_else(minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS,
                                     region, market.name)) %>%
        select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) ->
        L222.StubTechMarket_en_USA

      # L222.CarbonCoef_en_USA: energy carbon coefficients in USA
      # Step 1, process L202.CarbonCoef for joining
      L202.CarbonCoef %>%
        filter(region == gcam.USA_REGION,
               PrimaryFuelCO2Coef.name %in% gcamusa.SECTOR_EN_NAMES) %>%
        left_join(distinct(L222.SubsectorLogit_en, supplysector, subsector),
                  by = c(PrimaryFuelCO2Coef.name = "supplysector")) %>%
        mutate(PrimaryFuelCO2Coef.name = paste(PrimaryFuelCO2Coef.name, subsector)) %>%
        select(-region, -subsector) %>%
        left_join(distinct(L222.Supplysector_en_USA, region, supplysector),
                  by = c(PrimaryFuelCO2Coef.name = "supplysector")) %>%
        select(LEVEL2_DATA_NAMES[["CarbonCoef"]]) ->
        L222.CarbonCoef_en_USA

    # Produce outputs
    L222.DeleteStubTech_USAen %>%
      mutate(region = region) %>%  # strip off attributes so we can re-write title, etc.
      add_title("Removes existing stub technologies in the USA region") %>%
      add_units("NA") %>%
      add_comments("Removes existing stub technologies in the USA region from L222.StubTech_en.") %>%
      add_comments("The supplysector and subsector structure in the sectors defined in gcamusa.SECTOR_EN_NAMES are retained")  %>%
      add_legacy_name("L222.DeleteStubTech_USAen") %>%
      add_precursors("L222.StubTech_en") ->
      L222.DeleteStubTech_USAen

    L222.PassThroughSector_USAen %>%
      add_title("PassThroughSector information to send vintaging info from states to USA") %>%
      add_units("NA") %>%
      add_comments("state, subsector, supplysector, and region fromj L222.Tech_USAen is renamed.") %>%
      add_legacy_name("L222.PassThroughSector_USAen") %>%
      same_precursors_as(L222.Tech_USAen) ->
      L222.PassThroughSector_USAen

    L222.Tech_USAen %>%
      add_title("The technology pass-throughs used to set the proper node name, USA region.") %>%
      add_units("units") %>%
      add_comments("USA supplysector and subsector information from L222.SubsectorLogit_en is") %>%
      add_comments("repeated for all US states and updated.") %>%
      add_legacy_name("L222.Tech_USAen") %>%
      add_precursors("L222.SubsectorLogit_en",
                     "gcam-usa/SEDS_refining_feedstock_prod") ->
      L222.Tech_USAen

    L222.TechShrwt_USAen %>%
      add_title("Technology shareweights in each year, USA region") %>%
      add_units("NA") %>%
      add_comments("L222.Tech_USAen is repeated for model base year and future years and shareweights of 0 and") %>%
      add_comments("1 are added for each, respectively. Overwritten in calibration.") %>%
      add_legacy_name("L222.TechShrwt_USAen") %>%
      same_precursors_as(L222.Tech_USAen) ->
      L222.TechShrwt_USAen

    L222.TechInterp_USAen %>%
      add_title("Technology shareweights, USA region") %>%
      add_units("NA") %>%
      add_comments("Technology interpolation only applies to calibrated technologies.For biomass liquids, ") %>%
      add_comments("allow state shares to shift over time since future techs are different than present techs.") %>%
      add_legacy_name("L222.TechInterp_USAen")  %>%
      same_precursors_as(L222.Tech_USAen) ->
      L222.TechInterp_USAen

    L222.TechCoef_USAen %>%
      add_title("Technology coefficients and market names, USA region") %>%
      add_units("units") %>%
      add_comments("Data from L222.TechShrwt_USAen is renamed and filled out.") %>%
      add_legacy_name("L222.TechCoef_USAen") %>%
      same_precursors_as(L222.TechShrwt_USAen) ->
      L222.TechCoef_USAen

    L222.Production_USArefining %>%
      add_title("Calibrated refinery production in USA (consuming output of states)") %>%
      add_units("NA") %>%
      add_comments("L122.out_EJ_state_refining_F is aggregated to the supplysector/subsector/technology level.") %>%
      add_legacy_name("L222.Production_USArefining") %>%
      add_precursors("L122.out_EJ_state_refining_F") ->
      L222.Production_USArefining

    L222.Supplysector_en_USA %>%
      add_title("Supplysector information, replace name of supplysector with the subsector names") %>%
      add_units("Varies") %>%
      add_comments("L222.Supplysector_en and L222.SubsectorLogit_en is repeated and filtered for use in USA states.") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.Supplysector_en_USA") %>%
      add_precursors("L222.Supplysector_en",
                     "L222.SubsectorLogit_en") ->
      L222.Supplysector_en_USA

    L222.SubsectorShrwtFllt_en_USA %>%
      add_title("Subsector shareweights for energy in USA") %>%
      add_units("NA") %>%
      add_comments("USA energy subsector shareweights. There is no competition here, so shareweights are defaulted to 1.") %>%
      add_comments("Shareweights will be over-ridden by base year calibration.") %>%
      add_legacy_name("L222.SubsectorShrwtFllt_en_USA") %>%
      same_precursors_as(L222.SubsectorLogit_en_USA) ->
      L222.SubsectorShrwtFllt_en_USA

    L222.StubTechProd_refining_USA %>%
      add_title("USA Calibrated fuel production by state.") %>%
      add_units("varies") %>%
      add_comments("L122.out_EJ_state_refining_F downscales L222.StubTechProd_refining to states") %>%
      add_legacy_name("L222.StubTechProd_refining_USA") %>%
      add_precursors("L122.out_EJ_state_refining_F",
                     "L222.StubTechProd_refining") ->
      L222.StubTechProd_refining_USA

    L222.StubTechMarket_en_USA %>%
      add_title("Market names of inputs to state refining sectors") %>%
      add_units("varies") %>%
      add_comments("Data from L222.GlobalTechCoef_en is adjusted for use in US states, depending") %>%
      add_comments("on whether regional markets are used.") %>%
      add_legacy_name("L222.StubTechMarket_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L222.GlobalTechCoef_en") ->
      L222.StubTechMarket_en_USA

    L222.CarbonCoef_en_USA %>%
      add_title("Energy carbon coefficients in USA") %>%
      add_units("varies") %>%
      add_comments("Carbon coefficients from L202.CarbonCoef are updated with USA energy tech shareweights to") %>%
      add_comments("produce energy carbon coefficients in USA.") %>%
      add_legacy_name("L222.CarbonCoef_en_USA") %>%
      add_precursors("L222.SubsectorLogit_en",
                     "L202.CarbonCoef")  ->
      L222.CarbonCoef_en_USA

    L222.SubsectorLogit_en_USA %>%
      add_title("Subsector logit competition info for USA energy states and sectors") %>%
      add_units("NA") %>%
      add_comments("Subsector logit data from L222.SubsectorLogit_en are filtered and repeated") %>%
      add_comments("for USA sectors in each state.") %>%
      add_legacy_name("L222.SubsectorLogit_en_USA") %>%
      add_precursors("L222.SubsectorLogit_en") ->
      L222.SubsectorLogit_en_USA

    L222.StubTech_en_USA %>%
      add_title("Stub technology map for USA energy states and sectors.") %>%
      add_units("NA") %>%
      add_comments("The stub technology table from L222.StubTech_en is filtered and repeated") %>%
      add_comments("for USA energy sectors in each state.") %>%
      add_legacy_name("L222.StubTech_en_USA") %>%
      add_precursors("L222.StubTech_en") ->
      L222.StubTech_en_USA

    L222.StubTechCoef_refining_USA %>%
      add_title("Refining stub tech coefficients for USA energy states and sectors") %>%
      add_units("NA") %>%
      add_comments("Coefficients for refining stub technologies in L222.StubTechCoef_refining are filtered and repeated") %>%
      add_comments("for USA energy sectors in each state.") %>%
      add_legacy_name("L222.StubTechCoef_refining_USA") %>%
      add_precursors("L222.StubTechCoef_refining") ->
      L222.StubTechCoef_refining_USA

    L222.GlobalTechSCurve_en_USA %>%
      add_title("Tech S curve parameters for USA energy sectors.") %>%
      add_units("varies") %>%
      add_comments("S curve parameters from L222.GlobalTechScurve_en are filtered for USA sectors.") %>%
      add_legacy_name("L222.GlobalTechSCurve_en_USA") %>%
      add_precursors("L222.GlobalTechSCurve_en") ->
      L222.GlobalTechSCurve_en_USA

    if(exists("L222.GlobalTechProfitShutdown_en_USA")) {
      L222.GlobalTechProfitShutdown_en_USA %>%
        add_title("Global tech profit shutdown decider and parameters for USA energy sectors") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Profit-based shutdown from L222.GlobalTechProfitShutdown_en_USA are filtered for USA sectors.") %>%
        add_precursors("L222.GlobalTechProfitShutdown_en") ->
        L222.GlobalTechProfitShutdown_en_USA
    } else {
      missing_data() ->
        L222.GlobalTechProfitShutdown_en_USA
    }

    L222.GlobalTechCost_en_USA %>%
      add_title("Tech costs for USA energy sectors.") %>%
      add_units("varies") %>%
      add_comments("Tech cost data from L222.GlobalTechCost_en are filtered for USA sectors.") %>%
      add_legacy_name("L222.GlobalTechCost_en_USA") %>%
      add_precursors("L222.GlobalTechCost_en") ->
      L222.GlobalTechCost_en_USA

    L222.GlobalTechInterp_en_USA %>%
      add_title("Interpolation function key for USA energy sectors.") %>%
      add_units("units") %>%
      add_comments("Interpolation function key from L222.GlobalTechInterp_en are filtered for USA sectors.") %>%
      add_legacy_name("L222.GlobalTechInterp_en_USA") %>%
      add_precursors("L222.GlobalTechInterp_en") ->
      L222.GlobalTechInterp_en_USA

    L222.GlobalTechCoef_en_USA %>%
      add_title("Technology coefficients for USA energy sectors.") %>%
      add_units("NA") %>%
      add_comments("Global technology coefficients from L222.GlobalTechCoef_en are filtered for USA sectors.") %>%
      add_legacy_name("L222.GlobalTechCoef_en_USA") %>%
      add_precursors("L222.GlobalTechCoef_en") ->
      L222.GlobalTechCoef_en_USA

    L222.GlobalTechShrwt_en_USA %>%
      add_title("Technology shareweights for USA energy sectors") %>%
      add_units("NA") %>%
      add_comments("Shareweights from L222.GlobalTechShrwt_en are filtered for USA energy sectors.") %>%
      add_legacy_name("L222.GlobalTechShrwt_en_USA") %>%
      add_precursors("L222.GlobalTechShrwt_en") ->
      L222.GlobalTechShrwt_en_USA

    L222.GlobalTechCapture_en_USA %>%
      add_title("Carbon capture data for USA energy sectors") %>%
      add_units("NA") %>%
      add_comments("Carbon capture data  from L222.GlobalTechCapture_en are filtered for USA energy sectors.") %>%
      add_legacy_name("L222.GlobalTechCapture_en_USA") %>%
      add_precursors("L222.GlobalTechCapture_en") ->
      L222.GlobalTechCapture_en_USA

    return_data(L222.DeleteStubTech_USAen, L222.PassThroughSector_USAen, L222.Tech_USAen,
                L222.TechShrwt_USAen, L222.TechInterp_USAen, L222.TechCoef_USAen, L222.Production_USArefining,
                L222.Supplysector_en_USA, L222.SubsectorShrwtFllt_en_USA, L222.StubTechProd_refining_USA, L222.StubTechMarket_en_USA,
                L222.CarbonCoef_en_USA, L222.GlobalTechSCurve_en_USA, L222.GlobalTechProfitShutdown_en_USA,
                L222.GlobalTechCost_en_USA,
                L222.SubsectorLogit_en_USA,
                L222.StubTech_en_USA,
                L222.StubTechCoef_refining_USA,
                L222.GlobalTechInterp_en_USA,
                L222.GlobalTechCoef_en_USA,
                L222.GlobalTechShrwt_en_USA,
                L222.GlobalTechCapture_en_USA)
  } else {
    stop("Unknown command")
  }
}
