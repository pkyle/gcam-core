# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L225.hydrogen
#'
#' Selects the subsectors to be removed from the hydrogen sectors for GCAM USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.DeleteSubsector_h2_USA}. The corresponding file in the
#' original data system was \code{L225.hydrogen_USA.R} (gcam-usa level2).
#' @details This chunk selects the subsectors to be removed from the hydrogen sectors in GCAM USA on the national level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @author KD September 2017
module_gcamusa_L225.hydrogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/NREL_us_re_capacity_factors",
             FILE = "gcam-usa/A225.structure",
             "L125.Electrolyzer_IdleRatio_Params",
             "L225.Supplysector_h2",
             "L225.SectorUseTrialMarket_h2",
             "L225.SubsectorLogit_h2",
             "L225.SubsectorShrwtFllt_h2",
             "L225.StubTech_h2",
             "L225.GlobalTechCoef_h2",
             "L201.Pop_GCAMUSA",
             "L225.GlobalTechCost_h2",
             "L225.RenewElec_cost",
             "L225.RenewElec_eff"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.DeleteSupplysector_h2_USA",
             "L225.Supplysector_h2_USA",
             "L225.SectorUseTrialMarket_h2_USA",
             "L225.SubsectorLogit_h2_USA",
             "L225.SubsectorShrwtFllt_h2_USA",
             "L225.StubTech_h2_USA",
             "L225.StubTechMarket_h2_USA",
             "L225.DeleteStubTechMinicamEnergyInput_H2_USA",
             "L225.Supplysector_h2_ind_USA",
             "L225.SubsectorLogit_h2_ind_USA",
             "L225.SubsectorShrwtFllt_h2_ind_USA",
             "L225.TechCoef_h2_ind_USA",
             "L225.TechShrwt_h2_ind_USA",
             "L225.StubTechCost_h2_USA_ref",
             "L225.StubTechCost_h2_USA_high",
             "L225.StubTechCost_h2_USA_brkt",
             "L225.InterestRate_PADD",
             "L225.Pop_PADD",
             "L225.GDP_PADD",
             "L225.Supplysector_h2_PADD",
             "L225.SubsectorShrwtFllt_h2_PADD",
             "L225.SubsectorShrwt_h2_PADD",
             "L225.SubsectorLogit_h2_PADD",
             "L225.TechShrwt_h2_PADD",
             "L225.TechCoef_h2_PADD"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- subsector <- supplysector <- sector.name <- subsector.name <- technology <-
      state <- grid_region <- minicam.energy.input <- market.name <- stub.technology <- year <- NULL  # silence package check notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L125.Electrolyzer_IdleRatio_Params <- get_data(all_data, "L125.Electrolyzer_IdleRatio_Params", strip_attributes = TRUE)
    L225.Supplysector_h2 <- get_data(all_data, "L225.Supplysector_h2", strip_attributes = TRUE)
    L225.SectorUseTrialMarket_h2 <- get_data(all_data, "L225.SectorUseTrialMarket_h2", strip_attributes = TRUE)
    L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2", strip_attributes = TRUE)
    L225.SubsectorShrwtFllt_h2 <- get_data(all_data, "L225.SubsectorShrwtFllt_h2", strip_attributes = TRUE)
    L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2", strip_attributes = TRUE)
    L225.GlobalTechCoef_h2 <- get_data(all_data, "L225.GlobalTechCoef_h2", strip_attributes = TRUE)
    L201.Pop_GCAMUSA <- get_data(all_data, "L201.Pop_GCAMUSA", strip_attributes = TRUE)
    L225.GlobalTechCost_h2 <- get_data(all_data, "L225.GlobalTechCost_h2", strip_attributes = TRUE)
    L225.RenewElec_cost <- get_data(all_data, "L225.RenewElec_cost", strip_attributes = TRUE)
    L225.RenewElec_eff <- get_data(all_data, "L225.RenewElec_eff", strip_attributes = TRUE)
    NREL_us_re_capacity_factors <- get_data(all_data, "gcam-usa/NREL_us_re_capacity_factors", strip_attributes = TRUE)
    A225.structure <- get_data(all_data, "gcam-usa/A225.structure")
    # ===================================================

    # A vector of USA PADD region names
    states_subregions %>%
      select(PADD) %>%
      unique %>%
      arrange(PADD) %>%
      unlist ->
      PADD_regions

    # Socioeconomic information in the PADD regions (required for GCAM to run with these regions)

    # L225.InterestRate_PADD: Interest rates in the PADD regions
    tibble(region = PADD_regions,
           interest.rate = socioeconomics.DEFAULT_INTEREST_RATE) ->
      L225.InterestRate_PADD

    # L225.Pop_PADD: Population
    tibble(region = PADD_regions,
           totalPop = 1) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L225.Pop_PADD

    # L225.GDP_PADD: GDP in PADD regions
    tibble(region = PADD_regions,
           GDP = 1)  %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))->
      L225.GDP_PADD

    # Supplysector information for H2 passthru T&D sectors in PADD regions
    A225.structure %>%
      select(-region,-market.name) %>%
      repeat_add_columns(tibble(PADD = PADD_regions)) %>%
      left_join(states_subregions, by = c("PADD")) %>%
      mutate(market.name = state,
             subsector = state) %>%
      select(region = PADD,
             supplysector,subsector,technology,minicam.energy.input,market.name,
             subsector.logit,subsector.logit.type,technology.logit,technology.logit.type,
             output.unit,input.unit,price.unit) -> L225.structure_PADD

    # Supplysector info
    L225.structure_PADD %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
      select(region, supplysector, output.unit, input.unit, price.unit,
             logit.year.fillout, logit.exponent = subsector.logit, logit.type) ->
      L225.Supplysector_h2_PADD

    # Subsector (grid region) shareweights in USA electricity
    L225.structure_PADD %>%
      select(region, supplysector, subsector) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) -> L225.SubsectorShrwtFllt_h2_PADD

    L225.structure_PADD %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES$SubsectorShrwt) -> L225.SubsectorShrwt_h2_PADD


    # NOTE: There is only one tech per subsector in the PADD markets so the logit choice does not matter
    L225.structure_PADD %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
      select(region, supplysector, subsector, logit.year.fillout,
             logit.exponent = technology.logit, logit.type) -> L225.SubsectorLogit_h2_PADD

    # Technology shareweights, USA region
    L225.structure_PADD %>%
      select(region, supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = gcamusa.DEFAULT_SHAREWEIGHT) -> L225.TechShrwt_h2_PADD

    # Technology coefficients and market names
    L225.structure_PADD %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(supplysector != "electricity_net_ownuse") %>%
      mutate(coefficient = gcamusa.DEFAULT_COEFFICIENT) %>%
      select(region, supplysector, subsector, technology, year, minicam.energy.input,
             coefficient, market.name) -> L225.TechCoef_h2_PADD

    L225.StubTechCapFactor_ren_USA <- NREL_us_re_capacity_factors %>%
      rename(state_name = State) %>%
      filter(state_name != 'Average') %>%
      left_join_error_no_match(states_subregions,by = c('state_name')) %>%
      rename(region = state,solar = Rural_Utility_scale_PV,wind = Onshore_Wind) %>%
      select(region,solar,wind) %>%
      tidyr::pivot_longer(cols = !region,names_to='subsector',values_to = 'capacity.factor') %>%
      mutate(stub.technology = 'electrolysis',
             year = 1975) %>%
      gather_years %>%
      complete(nesting(region, subsector,stub.technology,capacity.factor), year = MODEL_YEARS) %>%
      filter(region != 'DC')

    ELECTROLYZER_YEARS <- unique(L125.Electrolyzer_IdleRatio_Params$Year[L125.Electrolyzer_IdleRatio_Params$Year %in% MODEL_YEARS])
    L225.StubTechCapFactor_ren_USA %>%
      filter(year %in% ELECTROLYZER_YEARS) %>%
      mutate(IdleRatio = pmax(1, 1 / (capacity.factor / energy.ELECTROLYZER_RENEWABLE_CAPACITY_RATIO))) %>%
      left_join(L125.Electrolyzer_IdleRatio_Params, by = c(year = "Year")) %>%
      mutate(input.cost = Intercept + Slope * IdleRatio) %>%
      select(Scen, region, subsector, year, input.cost) %>%
      complete(nesting(Scen, region, subsector), year = MODEL_YEARS) %>%
      group_by(Scen, region, subsector) %>%
      mutate(minicam.non.energy.input = "electrolyzer",
             input.cost = approx_fun(year, input.cost, rule = 2),
             input.cost = input.cost * gdp_deflator(1975, 2016) / CONV_GJ_KGH2) %>%
      ungroup() %>%
      left_join(select(L225.GlobalTechCost_h2, -input.cost),
                by = c("subsector" = "subsector.name", "year", "minicam.non.energy.input")) %>%
      rename(supplysector = sector.name, stub.technology = technology) ->
      L225.StubTechCost_h2_electrolyzer_USA


    L225.RenewElec_cost %>%
      left_join_error_no_match(L225.RenewElec_eff, by = c("subsector.name", "year")) %>%
      mutate(intermittent.technology = 'electrolysis') %>%
      left_join(L225.StubTechCapFactor_ren_USA,
                by = c("subsector.name" = "subsector", "intermittent.technology" = "stub.technology", "year")) %>%
      mutate(minicam.non.energy.input = if_else(subsector.name == "solar", "solar panels", "wind turbines"),
             output_kgh2_d = if_else(subsector.name == "solar", energy.SOLAR_ELECTROLYSIS_KGH2_D, energy.WIND_ELECTROLYSIS_KGH2_D),
             cost_75USD_kgH2 = cost_75USD_kW_yr * kWh_elec_per_kgH2 * output_kgh2_d / CONV_DAY_HOURS /
               (output_kgh2_d * capacity.factor / CONV_DAYS_YEAR),
             input.cost = cost_75USD_kgH2 / CONV_GJ_KGH2) %>%
      select(case, region, subsector.name, year, minicam.non.energy.input, input.cost) %>%
      left_join_error_no_match(L225.GlobalTechCost_h2 %>% select(-input.cost, -minicam.non.energy.input),
                               by = c("subsector.name", "year")) %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) ->
      L225.StubTechCost_h2_renewables_USA

    # Combine the electrolyzer and renewable power generation technologies' levelized non-energy costs into a single table
    L225.StubTechCost_h2_USA_ref <- L225.StubTechCost_h2_electrolyzer_USA %>%
      filter(Scen == "bau") %>%
      bind_rows(filter(L225.StubTechCost_h2_renewables_USA, case == "central")) %>%
      mutate(input.cost = round(input.cost, digits = energy.DIGITS_COST)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]])

    L225.StubTechCost_h2_USA_high <- L225.StubTechCost_h2_electrolyzer_USA %>%
      filter(Scen == "high") %>%
      bind_rows(filter(L225.StubTechCost_h2_renewables_USA, case == "adv tech")) %>%
      mutate(input.cost = round(input.cost, digits = energy.DIGITS_COST)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]])

    L225.StubTechCost_h2_USA_brkt <- L225.StubTechCost_h2_electrolyzer_USA %>%
      filter(Scen == "breakthrough") %>%
      bind_rows(filter(L225.StubTechCost_h2_renewables_USA, case == "adv tech")) %>%
      mutate(input.cost = round(input.cost, digits = energy.DIGITS_COST)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]])


    # Delete the hydrogen sectors from the USA region
    L225.DeleteSupplysector_h2_USA <- L225.Supplysector_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["DeleteSupplysector"]])

    L225.Supplysector_h2_USA <- L225.Supplysector_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME))

    L225.SectorUseTrialMarket_h2_USA <- L225.SectorUseTrialMarket_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["SectorUseTrialMarket"]])

    L225.SubsectorLogit_h2_USA <- L225.SubsectorLogit_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) %>%
      filter(!(region == 'DC' & subsector %in% c('solar','wind')))

    L225.SubsectorShrwtFllt_h2_USA <- L225.SubsectorShrwtFllt_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) %>%
      filter(!(region == 'DC' & subsector %in% c('solar','wind')))

    L225.StubTech_h2_USA <- L225.StubTech_h2 %>%
      filter(region == gcam.USA_REGION,
             !(region == 'DC' & subsector %in% c('solar','wind'))) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTech"]]) %>%
      filter(!(region == 'DC' & subsector %in% c('solar','wind')))

    # Assign the market names. Use the USA region as the default, then
    # - re-set grid-region fuel market
    # - re-set state-level fuel markets
    # - re-set upstream hydrogen commodity markets (hack - this replacement will need to be updated when inter-state hydrogen markets are represented)
    L225.StubTechMarket_h2_USA <- L225.GlobalTechCoef_h2 %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      mutate(market.name = gcam.USA_REGION) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTechMarket"]]) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region,PADD),
                               by = c("region" = "state")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% L225.Supplysector_h2_USA$supplysector,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% c("water_td_ind_C","water_td_ind_W","trn_freight_road","onshore wind resource","global solar resource"),
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamusa.H2_TD_MARKETS,PADD,market.name),
             minicam.energy.input = if_else(minicam.energy.input == 'global solar resource','PV_resource',minicam.energy.input)) %>%
      filter(!(region == 'DC' & subsector %in% c('solar','wind'))) #We should eventually do an anti-join for this but for now it's easier to just say DC

    L225.StubTechMarket_h2_USA %>%
      filter(minicam.energy.input == "PV_resource") %>%
      mutate(minicam.energy.input = "global solar resource") %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input) ->
      L225.DeleteStubTechMinicamEnergyInput_H2_USA

    # create "H2 industrial" supplysector in USA region with subsectors/technologies for each state
    L225.Supplysector_h2_ind_USA <- L225.Supplysector_h2_USA %>%
      filter(supplysector == 'H2 industrial') %>%
      mutate(region = gcam.USA_REGION)

    L225.SubsectorLogit_h2_ind_USA <- L225.SubsectorLogit_h2_USA %>%
      filter(supplysector == 'H2 industrial') %>%
      distinct(region,.keep_all=TRUE) %>%
      mutate(subsector = paste0(region,' ',supplysector),
             region = gcam.USA_REGION)

    L225.PopShrwts <- L201.Pop_GCAMUSA %>%
      group_by(year) %>%
      mutate(popShrwt = totalPop / sum(totalPop)) %>%
      ungroup()

    # These share-weights are revised each model time period, according to the population share over time.
    # Full_join is used as an expanding join is wanted here (expanding by year)
    L225.SubsectorShrwtFllt_h2_ind_USA <- L225.SubsectorShrwtFllt_h2_USA %>%
      filter(supplysector == 'H2 industrial') %>%
      distinct(region,year.fillout,.keep_all=TRUE) %>%
      full_join(L225.PopShrwts, by = c('region')) %>%
      mutate(subsector = paste0(region,' ',supplysector),
             region = gcam.USA_REGION,
             share.weight = if_else(as.numeric(share.weight) != 0, round(popShrwt,energy.DIGITS_SHRWT), as.numeric(share.weight)))

    # Full_join is used here in order to expand a global technology table by region (state)
    L225.TechCoef_h2_ind_USA <- L225.GlobalTechCoef_h2 %>%
      filter(sector.name == 'H2 industrial') %>%
      full_join(states_subregions %>%
                  select(state,PADD) %>%
                  mutate(sector.name = 'H2 industrial'),by = c('sector.name')) %>%
      distinct(state,year,.keep_all=TRUE) %>%
      mutate(subsector.name = paste0(state,' ',sector.name),
             technology = paste0(state,' ',sector.name),
             minicam.energy.input = 'H2 industrial',
             region = gcam.USA_REGION,
             market.name = state,
             market.name = if_else(minicam.energy.input %in% gcamusa.H2_TD_MARKETS,PADD,market.name)) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])

    L225.TechShrwt_h2_ind_USA <- L225.TechCoef_h2_ind_USA %>%
      mutate(share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    # ===================================================

    # Produce outputs
    L225.DeleteStubTechMinicamEnergyInput_H2_USA %>%
      add_title("Delete global solar resource Energy Input for PV Technologies") %>%
      add_units("NA") %>%
      add_comments("global solar resource input deleted; will be replaced by PV_resource") %>%
      add_comments("Applies to all states") %>%
      add_legacy_name("L225.DeleteStubTechMinicamEnergyInput_H2_USA") %>%
      add_precursors('L225.Supplysector_h2') ->
      L225.DeleteStubTechMinicamEnergyInput_H2_USA





    L225.DeleteSupplysector_h2_USA %>%
      add_title("Remove hydrogen sectors of USA region for GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("There are no USA hydrogen sectors in GCAM-USA") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.DeleteSupplysector_h2_USA

    L225.Supplysector_h2_USA %>%
      add_title("Supplysector info for hydrogen sectors in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.Supplysector_h2_USA

    L225.SectorUseTrialMarket_h2_USA %>%
      add_title("Supplysector trial market assignments for hydrogen sectors in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SectorUseTrialMarket_h2") ->
      L225.SectorUseTrialMarket_h2_USA

    L225.SubsectorLogit_h2_USA %>%
      add_title("Logit exponents for hydrogen subsectors in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorLogit_h2") ->
      L225.SubsectorLogit_h2_USA

    L225.SubsectorShrwtFllt_h2_USA %>%
      add_title("Subsector shareweight fillout for hydrogen subsectors in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorShrwtFllt_h2") ->
      L225.SubsectorShrwtFllt_h2_USA

    L225.StubTech_h2_USA %>%
      add_title("Stub technology pointers for hydrogen sectors in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.StubTech_h2") ->
      L225.StubTech_h2_USA

    L225.StubTechMarket_h2_USA %>%
      add_title("Stub technology market names for inputs to hydrogen technologies in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L225.GlobalTechCoef_h2") ->
      L225.StubTechMarket_h2_USA

    L225.Supplysector_h2_ind_USA %>%
      add_title("Add back H2 industrial to USA region") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.Supplysector_h2_ind_USA

    L225.SubsectorLogit_h2_ind_USA %>%
      add_title("State-level logit exponents for H2 industrial in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorLogit_h2") ->
      L225.SubsectorLogit_h2_ind_USA

    L225.SubsectorShrwtFllt_h2_ind_USA %>%
      add_title("Subsector shareweight fillout for state-level H2 industrial in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights based on relative population in each state") %>%
      add_precursors("L225.SubsectorShrwtFllt_h2",
                     "L201.Pop_GCAMUSA") ->
      L225.SubsectorShrwtFllt_h2_ind_USA

    L225.TechCoef_h2_ind_USA %>%
      add_title("Technology market names for inputs to state-level H2 industrial technologies in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L225.GlobalTechCoef_h2") ->
      L225.TechCoef_h2_ind_USA

    L225.TechShrwt_h2_ind_USA %>%
      add_title("Technology market names for inputs to state-level H2 industrial technologies in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L225.GlobalTechCoef_h2") ->
      L225.TechShrwt_h2_ind_USA

    L225.StubTechCost_h2_USA_ref %>%
      add_title("State-level green hydrogen production costs (reference scenario)") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      add_precursors("L125.Electrolyzer_IdleRatio_Params",
                     "L225.GlobalTechCost_h2",
                     "L225.GlobalTechCoef_h2",
                     "L225.RenewElec_cost",
                     "L225.RenewElec_eff",
                     "gcam-usa/NREL_us_re_capacity_factors") ->
      L225.StubTechCost_h2_USA_ref

    L225.StubTechCost_h2_USA_high %>%
      add_title("State-level green hydrogen production costs (high tech scenario)") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      same_precursors_as(L225.StubTechCost_h2_USA_ref) ->
      L225.StubTechCost_h2_USA_high

    L225.StubTechCost_h2_USA_brkt %>%
      add_title("State-level green hydrogen production costs (breakthrough scenario)") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      same_precursors_as(L225.StubTechCost_h2_USA_ref) ->
      L225.StubTechCost_h2_USA_brkt

    L225.InterestRate_PADD %>%
      add_title("Interest rates in PADD regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default interest rate") %>%
      add_legacy_name("L225.InterestRate_PADD") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L225.InterestRate_PADD

    L225.Pop_PADD %>%
      add_title("Population in PADD regions") %>%
      add_units("Unitless") %>%
      add_comments("The same value is copied to all model years") %>%
      add_legacy_name("L225.Pop_PADD") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L225.Pop_PADD

    L225.GDP_PADD %>%
      add_title("GDP in PADD regions") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L225.GDP_PADD

    L225.Supplysector_h2_PADD %>%
      #add_title("PADD region Hydrogen T&D Passthrough Sector Information")
      add_units("unitless") %>%
      add_comments("Supply sector information for hydrogen T&D passthrough sectors in the PADD regions") %>%
      add_legacy_name("L225.Supplysector_h2_PADD") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A225.structure") ->
      L225.Supplysector_h2_PADD

    L225.SubsectorLogit_h2_PADD %>%
      #add_title("PADD hydrogen T&D Passthrough Subsector Logits") %>%
      add_units("unitless") %>%
      add_comments("Subsector logits for hydrogen T&D passthrough sectors in the PADD regions") %>%
      add_legacy_name("L225.SubsectorShrwtFllt_h2_PADD") %>%
      same_precursors_as("L225.Supplysector_h2_PADD") ->
      L225.SubsectorLogit_h2_PADD

    L225.SubsectorShrwtFllt_h2_PADD %>%
      #add_title("PADD hydrogen T&D Passthrough Subsector Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Subsector share weights for hydrogen T&D passthrough sectors in the PADD regions") %>%
      add_legacy_name("L225.SubsectorShrwtFllt_h2_PADD") %>%
      same_precursors_as("L225.Supplysector_h2_PADD") ->
      L225.SubsectorShrwtFllt_h2_PADD

    L225.SubsectorShrwt_h2_PADD %>%
      #add_title("PADD hydrogen T&D Passthrough Subsector Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Subsector share weights for hydrogen T&D passthrough sectors in the PADD regions at points of inflexion") %>%
      same_precursors_as("L225.Supplysector_h2_PADD") ->
      L225.SubsectorShrwt_h2_PADD

    L225.TechShrwt_h2_PADD %>%
      #add_title("PADD hydrogen T&D Passthrough Technology Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Technology share weights for hydrogen T&D passthrough sectors in the PADD regions") %>%
      add_legacy_name("L225.TechShrwt_h2_PADD") %>%
      same_precursors_as("L225.Supplysector_h2_PADD_USA") ->
      L225.TechShrwt_h2_PADD

    L225.TechCoef_h2_PADD %>%
      #add_title("PADD hydrogen T&D Passthrough Technology Market Info") %>%
      add_units("unitless") %>%
      add_comments("Hydrogen T&D passthrough technology coefficients and market names in the PADD regions") %>%
      add_legacy_name("L225.TechCoef_h2_PADD") %>%
      same_precursors_as("L225.Supplysector_h2_PADD_USA") ->
      L225.TechCoef_h2_PADD

    return_data(L225.DeleteSupplysector_h2_USA,
                L225.Supplysector_h2_USA,
                L225.SectorUseTrialMarket_h2_USA,
                L225.SubsectorLogit_h2_USA,
                L225.SubsectorShrwtFllt_h2_USA,
                L225.StubTech_h2_USA,
                L225.StubTechMarket_h2_USA,
                L225.DeleteStubTechMinicamEnergyInput_H2_USA,
                L225.Supplysector_h2_ind_USA,
                L225.SubsectorLogit_h2_ind_USA,
                L225.SubsectorShrwtFllt_h2_ind_USA,
                L225.TechCoef_h2_ind_USA,
                L225.TechShrwt_h2_ind_USA,
                L225.StubTechCost_h2_USA_ref,
                L225.StubTechCost_h2_USA_high,
                L225.StubTechCost_h2_USA_brkt,
                L225.InterestRate_PADD,
                L225.Pop_PADD,
                L225.GDP_PADD,
                L225.Supplysector_h2_PADD,
                L225.SubsectorShrwtFllt_h2_PADD,
                L225.SubsectorShrwt_h2_PADD,
                L225.SubsectorLogit_h2_PADD,
                L225.TechShrwt_h2_PADD,
                L225.TechCoef_h2_PADD)
  } else {
    stop("Unknown command")
  }
}
