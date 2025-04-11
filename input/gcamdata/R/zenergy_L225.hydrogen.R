# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L225.hydrogen
#'
#' Provides supply sector information, subsector information, technology information for hydrogen sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.Supplysector_h2}, \code{L225.SubsectorLogit_h2}, \code{L225.SubsectorShrwtFllt_h2}, \code{L225.StubTech_h2}, \code{L225.GlobalTechCoef_h2}, \code{L225.GlobalTechCost_h2}, \code{L225.GlobalTechShrwt_h2}, \code{L225.PrimaryRenewKeyword_h2}, \code{L225.GlobalTechCapture_h2}, \code{L225.StubTechCost_h2}, \code{L225.GlobalTechProfitShutdown_h2}, \code{L225.GlobalTechSCurve_h2}. The corresponding file in the
#' original data system was \code{L225.hydrogen.R} (energy level2).
#' @details Provides supply sector information, subsector information, technology information for hydrogen sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate select
#' @importFrom tidyr complete nesting
#' @author LF Augest 2017
module_energy_L225.hydrogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A25.sector",
             FILE = "energy/A25.subsector_logit",
             FILE = "energy/A25.subsector_shrwt",
             FILE = "energy/A25.globaltech_cost",
             FILE = "energy/A25.globaltech_coef",
             FILE = "energy/A25.globaltech_losses",
             FILE = "energy/A25.globaltech_retirement",
             FILE = "energy/A25.globaltech_shrwt",
             FILE = "energy/A25.globaltech_keyword",
             FILE = "energy/A25.globaltech_co2capture",
             FILE = "energy/GlobalIntTechCapital_elec",
             FILE = "energy/GlobalIntTechOMfixed_elec",
             FILE = "energy/H2ALite_wind_solar_CF",
             FILE = "energy/H2ALite_TEAdata",
             "L125.globaltech_coef",
             "L125.globaltech_coef_adv",
             "L125.globaltech_coef_lotech",
             "L125.globaltech_cost",
             "L125.globaltech_cost_adv",
             "L125.globaltech_cost_lotech",
             "L125.Electrolyzer_IdleRatio_Params",
             "L223.StubTechCapFactor_elec",
             "L223.GlobalIntTechCapital_elec",
             "L223.GlobalIntTechOMfixed_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.Supplysector_h2",
             "L225.SectorUseTrialMarket_h2",
             "L225.SubsectorLogit_h2",
             "L225.SubsectorShrwtFllt_h2",
             "L225.StubTech_h2",
             "L225.GlobalTechCoef_h2",
             "L225.StubTechCoef_h2",
             "L225.StubTechCoef_h2_adv",
             "L225.StubTechCoef_h2_lotech",
             "L225.GlobalTechCost_h2",
             # "L225.GlobalTechCost_h2_adv",
             # "L225.GlobalTechCost_h2_lotech",
             "L225.GlobalTechTrackCapital_h2",
             "L225.GlobalTechShrwt_h2",
             "L225.PrimaryRenewKeyword_h2",
             "L225.AvgFossilEffKeyword_h2",
             "L225.GlobalTechCapture_h2",
             "L225.GlobalTechInputPMult_h2",
             "L225.GlobalTechProfitShutdown_h2",
             "L225.GlobalTechSCurve_h2",
             "L225.StubTechCost_h2",
             "L225.StubTechCost_h2_adv",
             "L225.StubTechCost_h2_lotech",
             "L225.OutputEmissCoeff_h2",
             "L225.RenewElec_cost",
             "L225.RenewElec_eff"))
  } else if(command == driver.MAKE) {

    # Silencing package checks
    region <- coefficient <- cost <- price.unit.conversion <- sector.name <- subsector.name <-
      stub.technology <- capacity.factor <- IdleRatio <- `2040` <- `2020` <- `2050` <-
      intermittent.technology <- capital.overnight <- fixed.charge.rate <- OM.fixed <-
      cost_75USD_kW_yr <- kWh_elec_per_kgH2 <- output_kgh2_d <- cost_75USD_kgH2 <- NULL

    all_data <- list(...)[[1]]

    year.fillout <- technology <- year <- efficiency <- supplysector <- value <-
      subsector <- minicam.energy.input <- input.cost <- minicam.non.energy.input <-
      share.weight <- primary.renewable <- average.fossil.efficiency <-
      remove.fraction <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A25.sector <- get_data(all_data, "energy/A25.sector", strip_attributes = TRUE)
    A25.subsector_logit <- get_data(all_data, "energy/A25.subsector_logit", strip_attributes = TRUE)
    A25.subsector_shrwt <- get_data(all_data, "energy/A25.subsector_shrwt", strip_attributes = TRUE)
    A25.globaltech_coef <- get_data(all_data, "energy/A25.globaltech_coef", strip_attributes = TRUE)
    A25.globaltech_losses <- get_data(all_data, "energy/A25.globaltech_losses", strip_attributes = TRUE)
    A25.globaltech_cost <- get_data(all_data, "energy/A25.globaltech_cost", strip_attributes = TRUE)
    A25.globaltech_shrwt <- get_data(all_data, "energy/A25.globaltech_shrwt", strip_attributes = TRUE)
    A25.globaltech_keyword <- get_data(all_data, "energy/A25.globaltech_keyword", strip_attributes = TRUE)
    A25.globaltech_retirement <- get_data(all_data, "energy/A25.globaltech_retirement", strip_attributes = TRUE)
    A25.globaltech_co2capture <- get_data(all_data, "energy/A25.globaltech_co2capture", strip_attributes = TRUE)

    L125.globaltech_coef <- get_data(all_data, "L125.globaltech_coef", strip_attributes = TRUE)
    L125.globaltech_coef_adv <- get_data(all_data, "L125.globaltech_coef_adv", strip_attributes = TRUE)
    L125.globaltech_coef_lotech <- get_data(all_data, "L125.globaltech_coef_lotech", strip_attributes = TRUE)
    L125.globaltech_cost <- get_data(all_data, "L125.globaltech_cost", strip_attributes = TRUE)
    L125.globaltech_cost_adv <- get_data(all_data, "L125.globaltech_cost_adv", strip_attributes = TRUE)
    L125.globaltech_cost_lotech <- get_data(all_data, "L125.globaltech_cost_lotech", strip_attributes = TRUE)
    L125.Electrolyzer_IdleRatio_Params <- get_data(all_data, "L125.Electrolyzer_IdleRatio_Params", strip_attributes = TRUE)

    ## Updating data sources for capital and O&M costs
    #L223.GlobalIntTechCapital_elec <- get_data(all_data, "L223.GlobalIntTechCapital_elec", strip_attributes = TRUE)
    #L223.GlobalIntTechOMfixed_elec <- get_data(all_data, "L223.GlobalIntTechOMfixed_elec", strip_attributes = TRUE)
    L223.GlobalIntTechCapital_elec <- get_data(all_data, "energy/GlobalIntTechCapital_elec", strip_attributes = TRUE)
    L223.GlobalIntTechOMfixed_elec <- get_data(all_data, "energy/GlobalIntTechOMfixed_elec", strip_attributes = TRUE)
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec", strip_attributes = TRUE)
    H2ALite_wind_solar_CF <- get_data(all_data,"energy/H2ALite_wind_solar_CF", strip_attributes = TRUE)
    H2ALite_TEAdata <- get_data(all_data,"energy/H2ALite_TEAdata", strip_attributes = TRUE)
    # ===================================================

    # 1. Build tables for CSVs
    # 1a. Supply sector information

    # L225.Supplysector_h2: Supply sector information for hydrogen sectors
    A25.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L225.Supplysector_h2

    # H2 liquid truck has a simultaneity that may benefit from using a trial market here
    L225.SectorUseTrialMarket_h2 <- filter(L225.Supplysector_h2, supplysector == "H2 liquid truck") %>%
      mutate(supplysector = "trn_freight_road") %>%
      select(region, supplysector) %>%
      mutate(use.trial.market = 1)

    # 1b. Subsector information

    # L225.SubsectorLogit_h2: Subsector logit exponents of hydrogen sectors
    A25.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) %>%
      filter(!(subsector %in% c("wind", "solar"))) ->
      L225.SubsectorLogit_h2

    # L225.SubsectorShrwtFllt_h2: Subsector shareweights of hydrogen sectors
    A25.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names)%>%
      filter(!(subsector %in% c("wind", "solar"))) ->
      L225.SubsectorShrwtFllt_h2

    # 1c. Technology information

    # L225.StubTech_h2: Identification of stub technologies of hydrogen
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A25.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      filter(!(subsector %in% c("wind", "solar"))) %>%
      rename(stub.technology = technology) ->
      L225.StubTech_h2

    # YZ 3/30/2025 update energy IO coef and non-energy cost for solar-wind-hybrid electrolyzer for non-USA regions
    # replacing the previous stand-alone solar or wind electrolyzer in these regions.
    # Implementation of hybrid electrolyzer in the USA is done and saved in global tech outputs.
    # Implementation for non-USA regions are based on linear models to predict the levelized non-energy
    # cost and input-output coefficient based on explanatory variables of solar and wind CF in each region
    # Note that in the previous version, stand-alone solar or wind electrolyzer cost consist of two parts -
    # 1) electrolyzer cost and 2) solar panel cost (or wind turbine cost)
    # After the update, all the non-energy cost is included in one entry given the H2Alite data; i.e., the
    # 'Energy-free levelized cost [2022$/kg]' column includes all the non-energy cost associated with a hybrid electrolyzer

    # gather data and prepare for regression based on Paul Wolfram's analysis
    H2ALite_TEAdata %>%
      filter(Year == 2040, Scenario == 'med', Technology == 'Hybrid PEM', Region != 'US Average') %>%
      select(region = Region, cost_2022usd_per_kg = `Energy-free levelized cost [2022$/kg]`,
             IO_solar_kwh_per_kg = `Energy use Electricity (Solar) [kWh/kg]`,
             IO_wind_kwh_per_kg = `Energy use Electricity (On-shore wind) [kWh/kg]`) %>%
      left_join_error_no_match(H2ALite_wind_solar_CF %>%
                                 rename(region = State), by = 'region')-> H2Alite_data
    H2Alite_data_prep <- H2Alite_data %>%
      mutate(sum_CF = Wind_CF + Solar_CF) %>% # calculate the sum of solar and wind CFs
      mutate(sum_CF_transform = 1/log10(sum_CF)) %>% # data transformation for better fit (see below)
      mutate(CF_ratio_sol_wind = Solar_CF / Wind_CF) %>% # calculate the fraction of the solar capacity factor over the wind capacity factor
      mutate(H2_IO_ratio_sol_wind = IO_solar_kwh_per_kg / IO_wind_kwh_per_kg) %>% # calculate the fraction of solar power used over wind power used per kg of H2 produced
      mutate(cost_2022usd_per_kg_transf = 1/log10(cost_2022usd_per_kg))# data transformation for better fit (see below)

    Interpol_cost<- L223.StubTechCapFactor_elec %>%
      filter(year == 2040, stub.technology %in% c('wind', 'PV')) %>%
      group_by(region) %>%
      summarise(sum_CF = sum(capacity.factor)) %>%
      ungroup() %>%
      mutate(sum_CF_transform = 1/log10(sum_CF)) %>%
      select(-sum_CF) %>%
      mutate(cost_2022usd_per_kg_transf = as.numeric("")) %>%
      bind_rows(H2Alite_data_prep %>%
                  select(region, sum_CF_transform, cost_2022usd_per_kg_transf))

    Interpol_coef<- L223.StubTechCapFactor_elec %>%
      filter(year == 2040, stub.technology %in% c('wind', 'PV')) %>%
      select(region,stub.technology,capacity.factor) %>%
      spread(stub.technology,capacity.factor) %>%
      mutate(CF_ratio_sol_wind = PV / wind) %>%
      select(region, CF_ratio_sol_wind) %>%
      mutate(H2_IO_ratio_sol_wind = as.numeric("")) %>%
      bind_rows(H2Alite_data_prep %>%
                  select(region, CF_ratio_sol_wind, H2_IO_ratio_sol_wind))

    model_cost_new_regions <- lm(cost_2022usd_per_kg_transf ~ sum_CF_transform, data = Interpol_cost)
    model_coef_new_regions <- lm(H2_IO_ratio_sol_wind ~ CF_ratio_sol_wind, data = Interpol_coef)
    # summary(model_cost_new_regions)
    # summary(model_coef_new_regions)
    Interpol_cost <- Interpol_cost %>%
      mutate(predicted_cost_transform = predict(model_cost_new_regions, Interpol_cost)) %>%
      mutate(predicted_cost_2022usd_per_kg = 10^(1/predicted_cost_transform)) # reverse the data transformation, 1 /log10(x) that we did at the beginning

    Interpol_coef <- Interpol_coef %>%
      mutate(predicted_H2_IO_ratio_sol_wind = predict(model_coef_new_regions, Interpol_coef))
    # YZ end of regression ----

    # YZ 3/31/2025 add solar-wind hybrid electrolysis IO coefficient for non-USA regions based on the regression analysis
    # the regression predicts the ratio of solar energy use intensity over wind energy use intensity for the hybrid electrolysis
    # We use this ratio and the US average sum of solar and wind energy use intensity to calculate the regional energy use
    # intensity for solar and wind in non-USA regions
    # given EI_wind + EI_sol = x; EI_sol / EI_wind = y
    # so EI_wind = x / (1+y); EI_sol = EI_wind * y = x*y / (1+y)
    Interpol_coef %>%
      filter(region %in% GCAM_region_names$region) %>%
      select(region, value = predicted_H2_IO_ratio_sol_wind) %>%
      mutate(subsector.name = 'hybrid') %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(
        L125.globaltech_coef %>%
          filter(subsector.name == 'hybrid',minicam.energy.input %in% c('global solar resource', 'onshore wind resource')) %>%
          group_by(sector.name, subsector.name, technology, year, units) %>%
          summarise(valuesum = sum(value)) %>%
          ungroup(), by = c('subsector.name', 'year')) %>%
      mutate(`onshore wind resource` = valuesum / (1+value),
             `global solar resource` = valuesum*value / (1+value)) %>%
      select(-value, -valuesum) %>%
      gather(minicam.energy.input, value, -region, -subsector.name, -year, -sector.name, -technology, -units) %>%
      # Solar input energy intensity per unit of hydrogen production is relatively small using the current CFs in gcamdata
      # For USA, the predicted solar EI is about 0.45, but the H2Alite data from misho is about  0.85 GJ input per GJ hydrogen output
      # this should be automatically updated when we update the solar CFs in gcamdata
      filter(region != 'USA')-># remove USA, will use global tech coef
      L225.StubTechCoef_h2_electrolyzer_hybrid_pre

    L225.StubTechCoef_h2_electrolyzer_hybrid_pre %>%
      bind_rows(L125.globaltech_coef %>%
                  filter(subsector.name == 'hybrid',minicam.energy.input %in% c('global solar resource', 'onshore wind resource')) %>%
                  mutate(region = "USA") ) -> L225.StubTechCoef_h2_electrolyzer_hybrid_allreg

    L125.globaltech_coef %>% inner_join(L225.StubTechCoef_h2_electrolyzer_hybrid_allreg %>%
      select(sector.name,subsector.name,technology) %>%
      distinct(), by = c('sector.name','subsector.name','technology')) %>%
      anti_join(L225.StubTechCoef_h2_electrolyzer_hybrid_allreg %>%
                  select(sector.name,subsector.name,technology,minicam.energy.input) %>%
                  distinct(), by = c('sector.name','subsector.name','technology','minicam.energy.input')) %>%
      repeat_add_columns(tibble(region = GCAM_region_names$region)) %>%
      bind_rows(L225.StubTechCoef_h2_electrolyzer_hybrid_allreg) %>%
      mutate(coefficient = round(value, digits = energy.DIGITS_COST),
             market.name = region) %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])->L225.StubTechCoef_h2

    # L225.GlobalTechCoef_h2: Energy inputs coefficients of global technologies for hydrogen
    L125.globaltech_coef %>%
      # YZ 3/31/2025 the hybrid electrolyzer uses stub.tech and so exclude them in the global tech
      anti_join(L225.StubTechCoef_h2 %>%
                  select(sector.name = supplysector, subsector.name = subsector, technology = stub.technology) %>%
                  distinct(),
                by = c('sector.name','subsector.name','technology')) %>%
           # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
           rename(#sector.name = supplysector,
                  #subsector.name = subsector,
                  coefficient = value) %>%
      mutate(coefficient = round(coefficient,energy.DIGITS_COEFFICIENT))-> L225.GlobalTechCoef_h2

    L125.globaltech_coef_adv %>%
      filter(subsector.name == "hybrid") %>%
      mutate(region = 'USA') %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology,
             coefficient = value) %>%
      mutate(coefficient = round(coefficient,energy.DIGITS_COEFFICIENT),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])-> L225.StubTechCoef_h2_adv

    L125.globaltech_coef_lotech %>%
      filter(subsector.name == "hybrid") %>%
      mutate(region = 'USA') %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology,
             coefficient = value) %>%
      mutate(coefficient = round(coefficient,energy.DIGITS_COEFFICIENT),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])-> L225.StubTechCoef_h2_lotech

    # L225.GlobalTechCost_h2: Costs of global technologies for hydrogen
    # Costs of global technologies
    L125.globaltech_cost %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(#sector.name = supplysector,
             #subsector.name = subsector,
             input.cost = cost) %>%
      mutate(input.cost = round(input.cost,energy.DIGITS_COST))-> L225.GlobalTechCost_h2

    # YZ 3/31/2025 comment out previous lines because the 'adv' and 'lotech' for USA is included in StubTech
    # L125.globaltech_cost_adv %>%
    #   # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
    #   rename(#sector.name = supplysector,
    #          #subsector.name = subsector,
    #          input.cost = cost) %>%
    #   mutate(input.cost = round(input.cost,energy.DIGITS_COST)) %>%
    #   filter(sector.name=="H2 central production")-> L225.GlobalTechCost_h2_adv
    #
    # L125.globaltech_cost_lotech %>%
    #   # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
    #   rename(#sector.name = supplysector,
    #          #subsector.name = subsector,
    #          input.cost = cost) %>%
    #   mutate(input.cost = round(input.cost,energy.DIGITS_COST))%>%
    #   filter(sector.name=="H2 central production")-> L225.GlobalTechCost_h2_lotech

    # L225.GlobalTechTrackCapital_h2: We want track capital investments for these technologies thus
    # we will change the object type accordingly and add the market name which will track investments
    # and the fraction of the total non-energy cost we should assume is annual investment in capital
    FCR <- (socioeconomics.DEFAULT_INTEREST_RATE * (1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.H2_CAP_PAYMENTS) /
      ((1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.H2_CAP_PAYMENTS -1)
    L225.GlobalTechCost_h2 %>%
      filter(sector.name == "H2 central production") %>%
      mutate(capital.coef = socioeconomics.H2_CAPITAL_RATIO / FCR,
             tracking.market =socioeconomics.EN_CAPITAL_MARKET_NAME,
             # note: vintaging is active so depreciation.rate is ignored
             depreciation.rate = 0) %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechTrackCapital']]) ->
      L225.GlobalTechTrackCapital_h2

    # L225.GlobalTechShrwt_h2: Shareweights of global technologies for hydrogen
    # Shareweights of global technologies
    A25.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(-value) ->
      L225.GlobalTechShrwt_h2

    A25.globaltech_coef %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 2),
             units = 'GJ input / GJ H2') %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      anti_join(L125.globaltech_coef, by = c("sector.name", "subsector.name", "technology", "minicam.energy.input")) %>%
      select(-value,-price.unit.conversion) ->
      L225.GlobalTechCoef_h2_noprod #filter and convert efficiencies to coefficients for only end use and distribution pass-through sectors and technologies

    L225.GlobalTechCoef_h2 <- bind_rows(L225.GlobalTechCoef_h2,L225.GlobalTechCoef_h2_noprod) %>%
      mutate(minicam.energy.input = if_else(grepl("elect_td_trn", minicam.energy.input), "elect_td_trn", minicam.energy.input)) %>%
      group_by(sector.name,subsector.name,technology,minicam.energy.input,units,year) %>%
      summarize(coefficient = sum(coefficient)) %>%
      ungroup()

    A25.globaltech_coef %>%
      filter(!is.na(price.unit.conversion)) %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      mutate(price.unit.conversion = approx_fun(year, price.unit.conversion, rule = 2)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInputPMult"]]) %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) -> L225.GlobalTechInputPMult

    A25.globaltech_cost %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = round(approx_fun(year, value, rule = 2), digits = energy.DIGITS_COST),
             units = '$1975/GJ H2') %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      anti_join(L125.globaltech_cost, by = c("sector.name", "subsector.name", "technology", "minicam.non.energy.input")) %>%
      select(-value) ->
      L225.GlobalTechCost_h2_noprod #get costs for only end use, distribution pass-through sectors, (global) renewable portion of LCOH (e.g., solar panels), and technologies from A25

    L225.GlobalTechCost_h2 <- bind_rows(L225.GlobalTechCost_h2,L225.GlobalTechCost_h2_noprod)

    # The following block uses the wind+solar capacity factors in each region to estimate the levelized cost of the
    # hydrogen electrolyzers.
    # Electrolyzer non-energy costs replace rather than add to the global default
    # values in L225.GlobalTechCost_h2. This is handled in the left_join.
    # In addition we calculate the cost based on H2Fast data for grid electrolysis (replacing all electrolyzer costs with zero in H2A_cost input data)
    # and append this to the stub tech to more seamlessly integrate with 3 scenario structure

    L225.StubTechCost_h2_electrolyzer_grid <- L125.Electrolyzer_IdleRatio_Params %>%
      filter(Year %in% MODEL_YEARS) %>%
      mutate(year = Year,
             IdleRatio = 1 / energy.Grid.Electrolyzer.capacity.factor,
             input.cost = Intercept + Slope * IdleRatio,
             minicam.non.energy.input = 'electrolyzer',
             subsector = 'electricity',
             supplysector = 'H2 central production',
             stub.technology = 'electrolysis') %>%
      complete(nesting(Scen, supplysector, subsector,stub.technology,minicam.non.energy.input), year = MODEL_YEARS) %>%
      group_by(Scen) %>%
      mutate(input.cost = approx_fun(year, input.cost, rule = 2),
             input.cost = input.cost * gdp_deflator(1975, 2020) / CONV_GJ_KGH2) %>%
      ungroup() %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechCost"]],'Scen'),GCAM_region_names) %>%
      mutate(units = '$1975/GJ H2') %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechCost"]],Scen,units))

    # YZ 3/30/2025 comment out original lines
    # L223.StubTechCapFactor_elec %>%
    #   filter(year %in% unique(L125.Electrolyzer_IdleRatio_Params$Year),
    #          stub.technology %in% c("wind", "PV")) %>%
    #   mutate(IdleRatio = pmax(1, 1 / (capacity.factor / energy.ELECTROLYZER_RENEWABLE_CAPACITY_RATIO))) %>%
    #   left_join(L125.Electrolyzer_IdleRatio_Params, by = c(year = "Year"), relationship = "many-to-many") %>%
    #   mutate(input.cost = Intercept + Slope * IdleRatio) %>%
    #   select(Scen, region, subsector, year, input.cost) %>%
    #   complete(nesting(Scen, region, subsector), year = MODEL_YEARS) %>%
    #   group_by(Scen, region, subsector) %>%
    #   mutate(minicam.non.energy.input = "electrolyzer",
    #          input.cost = approx_fun(year, input.cost, rule = 2),
    #          input.cost = input.cost * gdp_deflator(1975, 2020) / CONV_GJ_KGH2) %>%
    #   ungroup() %>%
    #   left_join(select(L225.GlobalTechCost_h2, -input.cost),
    #             by = c("subsector" = "subsector.name", "year", "minicam.non.energy.input")) %>%
    #   rename(supplysector = sector.name, stub.technology = technology) %>%
    #   bind_rows(L225.StubTechCost_h2_electrolyzer_grid) %>%
    #   filter(region!="USA")-> #remove USA, will use global tech cost
    #   L225.StubTechCost_h2_electrolyzer

    Interpol_cost %>%
      filter(region %in% GCAM_region_names$region) %>%
      # convert to unit of 1975$/GJ
      mutate(input.cost = predicted_cost_2022usd_per_kg*gdp_deflator(1975,2022)/CONV_GJ_KGH2,
             subsector = 'hybrid',
             minicam.non.energy.input = "electrolyzer") %>%
      select(region, subsector, minicam.non.energy.input, input.cost) %>%
      # use H2ALite med 2040 cost for all model years and for all scenarios for non-USA regions
      # only bau scenario is being used though
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(tibble(Scen = unique(L225.StubTechCost_h2_electrolyzer_grid$Scen))) %>%
      left_join(select(L225.GlobalTechCost_h2, -input.cost),
                by = c("subsector" = "subsector.name", "year", "minicam.non.energy.input")) %>%
      rename(supplysector = sector.name, stub.technology = technology) %>%
      bind_rows(L225.StubTechCost_h2_electrolyzer_grid) %>%
      filter(region!="USA")-> #remove USA, will use global tech cost
      L225.StubTechCost_h2_electrolyzer

    L225.StubTechCost_h2_electrolyzer_ref <- filter(L225.StubTechCost_h2_electrolyzer, Scen == "bau") %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]])
    #L225.StubTechCost_h2_electrolyzer_high <- filter(L225.StubTechCost_h2_electrolyzer, Scen == "high") %>%
      #select(LEVEL2_DATA_NAMES[["StubTechCost"]])
    #L225.StubTechCost_h2_electrolyzer_brkt <- filter(L225.StubTechCost_h2_electrolyzer, Scen == "breakthrough") %>%
      #select(LEVEL2_DATA_NAMES[["StubTechCost"]])

    # YZ 3/31/2025 comment out previous lines on calculating the solar panel and wind turbine cost because
    # these cost are included in the H2ALite levelized non-energy cost for the hybrid solar-wind tech (processed above)

    # # Estimate the wind turbine and solar panel related aspects of the costs of direct renewable hydrogen electrolysis
    # # These are estimated from the capital costs of wind and solar technologies (generic, global), the region-specific
    # # capacity factors, efficiencies of hydrogen production, and size of the representative hydrogen plant
    # # Updated to process ATB central and advanced cases

    # YZ 4/2/2025 uncomment these because they are still being used in gcamusa (module_gcamusa_L225.hydrogen)
    L225.RenewElec_cost <- L223.GlobalIntTechCapital_elec %>%
      filter(intermittent.technology %in% c("wind", "PV")) %>%
      left_join(L223.GlobalIntTechOMfixed_elec, by = c("sector.name", "subsector.name", "intermittent.technology", "year", "case")) %>%
      mutate(cost_75USD_kW_yr = capital.overnight * fixed.charge.rate + OM.fixed) %>%
      select(subsector.name, intermittent.technology, year, cost_75USD_kW_yr, case)

    L225.RenewElec_eff <- filter(L225.GlobalTechCoef_h2,
                                 subsector.name %in% c("solar", "wind") & !grepl("water", minicam.energy.input)) %>%
      mutate(kWh_elec_per_kgH2 = coefficient * CONV_GJ_KGH2 / CONV_KWH_GJ) %>%
      select(subsector.name, year, kWh_elec_per_kgH2)
    #
    # # Central ATB costs
    # L225.RenewElec_cost %>%
    #   left_join_error_no_match(L225.RenewElec_eff, by = c("subsector.name", "year")) %>% filter(case=="central") %>%
    #   left_join(L223.StubTechCapFactor_elec,
    #             by = c("subsector.name" = "subsector", "intermittent.technology" = "stub.technology", "year")) %>%
    #   mutate(minicam.non.energy.input = if_else(subsector.name == "solar", "solar panels", "wind turbines"),
    #          output_kgh2_d = if_else(subsector.name == "solar", energy.SOLAR_ELECTROLYSIS_KGH2_D, energy.WIND_ELECTROLYSIS_KGH2_D),
    #          cost_75USD_kgH2 = cost_75USD_kW_yr * kWh_elec_per_kgH2 * output_kgh2_d / CONV_DAY_HOURS /
    #            (output_kgh2_d * capacity.factor / CONV_DAYS_YEAR),
    #          input.cost = cost_75USD_kgH2 / CONV_GJ_KGH2) %>%
    #   select(region, subsector.name, year, minicam.non.energy.input, input.cost) %>%
    #   left_join_error_no_match(L225.GlobalTechCost_h2 %>% select(-input.cost, -minicam.non.energy.input),
    #                            by = c("subsector.name", "year")) %>%
    #         rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
    #   select(LEVEL2_DATA_NAMES[["StubTechCost"]]) %>%
    #   filter(region!="USA")-> #remove USA, will use global tech cost
    #   L225.StubTechCost_h2_renewables

    # Advanced ATB costs
    # L225.RenewElec_cost %>%
    #   left_join_error_no_match(L225.RenewElec_eff, by = c("subsector.name", "year")) %>% filter(case=="adv tech") %>%
    #   left_join(L223.StubTechCapFactor_elec,
    #             by = c("subsector.name" = "subsector", "intermittent.technology" = "stub.technology", "year"))%>%
    #   mutate(minicam.non.energy.input = if_else(subsector.name == "solar", "solar panels", "wind turbines"),
    #          output_kgh2_d = if_else(subsector.name == "solar", energy.SOLAR_ELECTROLYSIS_KGH2_D, energy.WIND_ELECTROLYSIS_KGH2_D),
    #          cost_75USD_kgH2 = cost_75USD_kW_yr * kWh_elec_per_kgH2 * output_kgh2_d / CONV_DAY_HOURS /
    #            (output_kgh2_d * capacity.factor / CONV_DAYS_YEAR),
    #          input.cost = cost_75USD_kgH2 / CONV_GJ_KGH2) %>%
    #   select(region, subsector.name, year, minicam.non.energy.input, input.cost) %>%
    #   left_join_error_no_match(L225.GlobalTechCost_h2 %>% select(-input.cost, -minicam.non.energy.input),
    #                            by = c("subsector.name", "year")) %>%
    #   rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
    #   select(LEVEL2_DATA_NAMES[["StubTechCost"]]) ->
    #   L225.StubTechCost_h2_renewables_adv

    # Combine the electrolyzer and renewable power generation technologies' levelized non-energy costs into a single table
    L225.StubTechCost_h2 <- L225.StubTechCost_h2_electrolyzer_ref %>%
      mutate(input.cost = round(input.cost, digits = energy.DIGITS_COST)) %>%
      bind_rows(filter(L125.globaltech_cost, subsector.name %in% c("electricity", "hybrid")) %>%
                 mutate(region="USA") %>%
                 rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology, input.cost=cost) %>%
                 select(-units))

    L225.StubTechCost_h2_adv <- filter(L125.globaltech_cost_adv, subsector.name %in% c("electricity", "hybrid")) %>%
                  mutate(region="USA") %>%
                  rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology, input.cost=cost) %>%
                  select(-units)

    L225.StubTechCost_h2_lotech <- filter(L125.globaltech_cost_lotech, subsector.name %in% c("electricity", "hybrid")) %>%
      mutate(region="USA") %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology, input.cost=cost) %>%
      select(-units)

    # L225.StubTechCost_h2_high <- bind_rows(L225.StubTechCost_h2_electrolyzer_high, L225.StubTechCost_h2_renewables_adv) %>%
    #   mutate(input.cost = round(input.cost, digits = energy.DIGITS_COST))
    #
    # L225.StubTechCost_h2_brkt <- bind_rows(L225.StubTechCost_h2_electrolyzer_brkt, L225.StubTechCost_h2_renewables_adv) %>%
    #   mutate(input.cost = round(input.cost, digits = energy.DIGITS_COST))

    # L225.PrimaryRenewKeyword_h2: Keywords of primary renewable electric generation technologies
    A25.globaltech_keyword %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L225.AllKeyword_h2

    L225.AllKeyword_h2 %>%
      filter(!is.na(primary.renewable)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "primary.renewable") ->
      L225.PrimaryRenewKeyword_h2

    # L225.AvgFossilEffKeyword_h2: Keywords of fossil/bio electric generation technologies
    L225.AllKeyword_h2 %>%
      filter(!is.na(average.fossil.efficiency)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "average.fossil.efficiency") ->
      L225.AvgFossilEffKeyword_h2

    # L225.GlobalTechCapture_h2: CO2 capture fractions from global fertilizer production technologies with CCS
    # Note: No need to consider historical periods or intermittent technologies here
    A25.globaltech_co2capture %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, value, rule = 1),
             remove.fraction = round(remove.fraction,energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction") %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) ->
      L225.GlobalTechCapture_h2


    # Set global technology retirement information for all hydrogen sector technologies
    # ------------------------------------------------------------------------------------

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L225.globaltech_retirement for each of these functions and creates a subset for each option then removes any subsets with 0 rows
    # All of these options have different headers, and all are allowed.
    # Also, technologies that have an additional shutdown rate as a function of their profitability are also defined.
    # Replace years and prepare assumptions into correct format
    A25.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L225.globaltech_retirement_base

    # Copies base year retirement information into all future years and appends back onto itself
    L225.globaltech_retirement_base %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(filter(L225.globaltech_retirement_base, year == max(MODEL_BASE_YEARS))) ->
      L225.globaltech_retirement

    # S-CURVE RETIREMENT
    # Subsets the S-Curve retirement function
    L225.globaltech_retirement %>%
      filter(!is.na(L225.globaltech_retirement$half.life)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
      L225.GlobalTechSCurve_h2

    # PROFIT-BASED SHUTDOWN PARAMETERS
    # Subsets any technologies with a shutdown parameter based on profitability
    L225.globaltech_retirement %>%
      filter(!is.na(L225.globaltech_retirement$median.shutdown.point)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
      L225.GlobalTechProfitShutdown_h2

    # Adjustment to coefficients for losses
    L225.globaltech_losses <- gather_years(A25.globaltech_losses) %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, Non.CO2), year = MODEL_YEARS) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, Non.CO2) %>%
      mutate(multiplier = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      select(-value)

    L225.GlobalTechCoef_h2 <- left_join_error_no_match(L225.GlobalTechCoef_h2, L225.globaltech_losses,
                                        by = c(sector.name = "supplysector", subsector.name = "subsector", "technology", "minicam.energy.input", "year"),
                                        ignore_columns = c("Non.CO2", "multiplier")) %>%
      mutate(coefficient = if_else(is.na(multiplier),
                                   coefficient,
                                   round(coefficient * multiplier, energy.DIGITS_COEFFICIENT))) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])

    # Emissions coefficients
    # Emissions coefficients are read as region-specific data, so the default coefs need to be repeated by all regions
    L225.OutputEmissCoeff_h2 <- L225.globaltech_losses %>%
      mutate(emiss.coeff = round((multiplier - 1) / CONV_GJ_KGH2, emissions.DIGITS_EMISS_COEF)) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble(GCAM_region_names["region"])) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeff"]])

    # ===================================================
    # Produce outputs

    L225.Supplysector_h2 %>%
      add_title("Supply sector information for hydrogen sectors") %>%
      add_units("Unitless") %>%
      add_comments("Expand sector information for all GCAM regions") %>%
      add_legacy_name("L225.Supplysector_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.sector") ->
      L225.Supplysector_h2

    L225.SectorUseTrialMarket_h2 %>%
      add_title("Supply sector trial markets") %>%
      add_units("Boolean") %>%
      add_comments("Read in to help solver deal with simultaneities") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.sector") ->
      L225.SectorUseTrialMarket_h2

    L225.SubsectorLogit_h2 %>%
      add_title("Subsector logit exponents of hydrogen sectors") %>%
      add_units("Unitless") %>%
      add_comments("Expand subsector logit exponents for all GCAM regions") %>%
      add_legacy_name("L225.SubsectorLogit_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.subsector_logit") ->
      L225.SubsectorLogit_h2

    L225.SubsectorShrwtFllt_h2 %>%
      add_title("Subsector shareweights of hydrogen sectors") %>%
      add_units("Unitless") %>%
      add_comments("Expand Subsector shareweights for all GCAM regions") %>%
      add_legacy_name("L225.SubsectorShrwtFllt_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.subsector_shrwt") ->
      L225.SubsectorShrwtFllt_h2

    L225.StubTech_h2 %>%
      add_title("Identification of stub technologies of hydrogen") %>%
      add_units("NA") %>%
      add_comments("Expand stub technologies information for all GCAM regions") %>%
      add_comments("assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)") %>%
      add_legacy_name("L225.StubTech_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.globaltech_shrwt") ->
      L225.StubTech_h2

    L225.GlobalTechCoef_h2 %>%
      add_title("Energy inputs and efficiencies of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_precursors("L125.globaltech_coef",'energy/A25.globaltech_coef', "energy/A25.globaltech_losses") ->
      L225.GlobalTechCoef_h2

    L225.StubTechCoef_h2 %>%
      add_title("Energy inputs and efficiencies of hybrid technologies for hydrogen in USA") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_precursors("L125.globaltech_coef",'energy/A25.globaltech_coef', "energy/H2ALite_wind_solar_CF") ->
      L225.StubTechCoef_h2

    L225.StubTechCoef_h2_lotech %>%
      add_title("Energy inputs and efficiencies of lotech hybrid technologies for hydrogen in USA") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_precursors("L125.globaltech_coef_lotech",'energy/A25.globaltech_coef', "energy/A25.globaltech_losses") ->
      L225.StubTechCoef_h2_lotech

    L225.StubTechCoef_h2_adv %>%
      add_title("Energy inputs and efficiencies of advanced hybrid technologies for hydrogen in USA") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_precursors("L125.globaltech_coef_adv",'energy/A25.globaltech_coef', "energy/A25.globaltech_losses") ->
      L225.StubTechCoef_h2_adv

    L225.GlobalTechCost_h2 %>%
      add_title("Costs of global technologies for hydrogen") %>%
      add_units("$1975 / GJ H2") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_precursors("L125.globaltech_cost",'energy/A25.globaltech_cost')  -> L225.GlobalTechCost_h2

    # L225.GlobalTechCost_h2_adv %>%
    #   add_title("Costs of global technologies for advanced hydrogen") %>%
    #   add_units("$1975 / GJ H2") %>%
    #   add_comments("Interpolated orginal data into all model years") %>%
    #   add_precursors("L125.globaltech_cost_adv",'energy/A25.globaltech_cost')  -> L225.GlobalTechCost_h2_adv
    #
    # L225.GlobalTechCost_h2_lotech %>%
    #   add_title("Costs of global technologies for lotech hydrogen") %>%
    #   add_units("$1975 / GJ H2") %>%
    #   add_comments("Interpolated orginal data into all model years") %>%
    #   add_precursors("L125.globaltech_cost_lotech",'energy/A25.globaltech_cost')  -> L225.GlobalTechCost_h2_lotech

    L225.GlobalTechTrackCapital_h2 %>%
      add_title("Convert non-energy inputs to track the annual capital investments.") %>%
      add_units(("Coefficients")) %>%
      add_comments("Track capital investments for purposes of macro economic calculations") %>%
      add_comments("Note for capital tracking we are interested only in the H2 production techs") %>%
      add_precursors("energy/A25.globaltech_cost") ->
      L225.GlobalTechTrackCapital_h2

    L225.GlobalTechShrwt_h2 %>%
      add_title("Shareweights of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechShrwt_h2") %>%
      add_precursors("energy/A25.globaltech_shrwt") ->
      L225.GlobalTechShrwt_h2

    L225.PrimaryRenewKeyword_h2 %>%
      add_title("Keywords of primary renewable electric generation technologies") %>%
      add_units("NA") %>%
      add_comments("Identify Keywords of primary renewable electric generation technologies for all model years") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L225.PrimaryRenewKeyword_h2") %>%
      add_precursors("energy/A25.globaltech_keyword") ->
      L225.PrimaryRenewKeyword_h2

    L225.AvgFossilEffKeyword_h2 %>%
      add_title("Keywords of fossil/bio electric generation technologies") %>%
      add_units("NA") %>%
      add_comments("Identify Keywords of fossil/bio electric generation technologies for all model years") %>%
      add_legacy_name("L225.AvgFossilEffKeyword_h2") %>%
      add_precursors("energy/A25.globaltech_keyword") ->
      L225.AvgFossilEffKeyword_h2

    L225.GlobalTechCapture_h2 %>%
      add_title("CO2 capture fractions from global fertilizer production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechCapture_h2") %>%
      add_precursors("energy/A25.globaltech_co2capture")->
      L225.GlobalTechCapture_h2

    L225.GlobalTechInputPMult %>%
      add_title("Price conversion from transportation technologies") %>%
      add_comments("converts from $1990/tkm to $1975$/EJ") %>%
      add_units("Unitless") ->
      L225.GlobalTechInputPMult_h2

    L225.GlobalTechSCurve_h2 %>%
      add_title("Global tech lifetime for techs with s-curve retirement function") %>%
      add_units("Lifetime in years, half-life in years") %>%
      add_comments("Filters for any technology that uses an S-curve retirement function") %>%
      add_legacy_name("L225.GlobalTechSCurve_h2") %>%
      add_precursors("energy/A25.globaltech_retirement") ->
      L225.GlobalTechSCurve_h2

    L225.GlobalTechProfitShutdown_h2 %>%
      add_title("Global tech profit shutdown decider and parameters") %>%
      add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
      add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
      add_legacy_name("L225.GlobalTechProfitShutdown_h2") %>%
      add_precursors("energy/A25.globaltech_retirement") ->
      L225.GlobalTechProfitShutdown_h2

    L225.StubTechCost_h2 %>%
      add_title("Regional hydrogen production costs") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      add_precursors("L125.globaltech_cost",
                     "L125.Electrolyzer_IdleRatio_Params",
                     "L223.StubTechCapFactor_elec",
                     "L223.GlobalIntTechCapital_elec",
                     "L223.GlobalIntTechOMfixed_elec",
                     "energy/H2ALite_wind_solar_CF") ->
      L225.StubTechCost_h2

    L225.StubTechCost_h2_adv %>%
      add_title("Regional adv hydrogen production costs") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      add_precursors("L125.globaltech_cost_adv",
                     "L125.Electrolyzer_IdleRatio_Params",
                     "L223.StubTechCapFactor_elec",
                     "L223.GlobalIntTechCapital_elec",
                     "L223.GlobalIntTechOMfixed_elec") ->
      L225.StubTechCost_h2_adv

    L225.StubTechCost_h2_lotech %>%
      add_title("Regional lotech hydrogen production costs") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      add_precursors("L125.globaltech_cost_lotech",
                     "L125.Electrolyzer_IdleRatio_Params",
                     "L223.StubTechCapFactor_elec",
                     "L223.GlobalIntTechCapital_elec",
                     "L223.GlobalIntTechOMfixed_elec") ->
      L225.StubTechCost_h2_lotech

    # L225.StubTechCost_h2_high %>%
    #   add_title("Regional hydrogen production costs in the high technology scenario") %>%
    #   add_units("$1975/GJ") %>%
    #   add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
    #   add_precursors("L125.Electrolyzer_IdleRatio_Params",
    #                  "L223.StubTechCapFactor_elec",
    #                  "ene`rgy/GlobalIntTechCapital_elec",
    #                  "energy/GlobalIntTechOMfixed_elec") ->
    #   L225.StubTechCost_h2_high
    #
    # L225.StubTechCost_h2_brkt %>%
    #   add_title("Regional hydrogen production costs in the breakthrough technology scenario") %>%
    #   add_units("$1975/GJ") %>%
    #   add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
    #   same_precursors_as(L225.StubTechCost_h2_high) ->
    #   L225.StubTechCost_h2_brkt

    L225.OutputEmissCoeff_h2 %>%
      add_title("Hydrogen gas emissions coefficients") %>%
      add_units("kg of H2 per GJ of hydrogen output") %>%
      add_comments("calculated from the assumed losses") %>%
      same_precursors_as(L225.GlobalTechCoef_h2) %>%
      add_precursors("energy/A25.globaltech_losses") ->
      L225.OutputEmissCoeff_h2

    L225.RenewElec_cost %>%
      add_title("cost per kW wind turbines or solar panels") %>%
      add_units("$1975 per kW per year") %>%
      add_comments("harmonized with electricity sector assumptions") %>%
      add_precursors("L223.GlobalIntTechCapital_elec") ->
      L225.RenewElec_cost

    L225.RenewElec_eff %>%
      add_title("efficiency of wind turbines or solar panels") %>%
      add_units("Unitless") %>%
      add_comments("harmonized with electricity sector assumptions") %>%
      add_precursors("energy/A25.globaltech_coef") ->
      L225.RenewElec_eff

    return_data(L225.Supplysector_h2, L225.SectorUseTrialMarket_h2, L225.SubsectorLogit_h2, L225.StubTech_h2,
                L225.GlobalTechCoef_h2, L225.StubTechCoef_h2, L225.StubTechCoef_h2_adv, L225.StubTechCoef_h2_lotech,
                L225.GlobalTechCost_h2,
                L225.GlobalTechTrackCapital_h2, L225.GlobalTechShrwt_h2,
                L225.PrimaryRenewKeyword_h2, L225.AvgFossilEffKeyword_h2,
                L225.GlobalTechCapture_h2, L225.SubsectorShrwtFllt_h2,
                L225.GlobalTechInputPMult_h2,
                L225.GlobalTechSCurve_h2, L225.GlobalTechProfitShutdown_h2,
                L225.StubTechCost_h2, L225.StubTechCost_h2_adv, L225.StubTechCost_h2_lotech,
                L225.OutputEmissCoeff_h2,L225.RenewElec_cost,L225.RenewElec_eff)
  } else {
    stop("Unknown command")
  }
}
