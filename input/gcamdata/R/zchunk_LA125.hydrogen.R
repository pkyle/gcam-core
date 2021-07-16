# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA125.hydrogen
#'
#' Provides supply sector information, subsector information, technology information for hydrogen sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{asdf}.
#' @details Takes inputs from H2A and generates GCAM's assumptions by technology and year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate select if_else
#' @importFrom tidyr complete nesting
#' @author GPK/JF/PW July 2021
module_energy_LA125.hydrogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A22.globaltech_coef",
             FILE = "energy/A25.globaltech_eff",
             FILE = "energy/A25.globaltech_cost",
             FILE = "energy/H2A_2018_prod_data_coef",
             FILE = "energy/H2A_2018_prod_data_cost",
             "L223.GlobalTechCapital_elec",
             "L223.GlobalTechEff_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L125.globaltech_coef",
             "L125.globaltech_cost"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- NULL  # silence package check notes

    # Load required inputs
    A22.globaltech_coef <- get_data(all_data, "energy/A22.globaltech_coef")
    A25.globaltech_coef <- get_data(all_data, "energy/A25.globaltech_eff")
    A25.globaltech_cost <- get_data(all_data, "energy/A25.globaltech_cost")

    H2A_prod_coef <- get_data(all_data, "energy/H2A_2018_prod_data_coef")
    H2A_prod_cost <- get_data(all_data, "energy/H2A_2018_prod_data_cost")

    L223.GlobalTechCapital_elec <- get_data(all_data, "L223.GlobalTechCapital_elec")
    L223.GlobalTechEff_elec <- get_data(all_data, "L223.GlobalTechEff_elec")


    #   4.) GJ fossil / GJ elec
    GJ_fuel_per_GJ_elec <- 3

    #   5.) GJ bio / GJ bio gas
    A22.globaltech_coef %>%
      filter( technology == "biomass gasification" ) %>%
      select('2020') -> GJ_bio_per_GJ_bio_gas

    GJ_bio_per_GJ_bio_gas <- GJ_bio_per_GJ_bio_gas[[1]] # = 1.343


    # ===================================================

    # Process data

    #cost ratio of CCS to non CCS for coal electricity generation.
    L223.GlobalTechCapital_elec %>%
      filter( technology %in% c( "coal (IGCC)", "coal (IGCC CCS)" ),
                     year == 2015 ) %>%
      spread( technology, capital.overnight ) %>%
      rename( coal_IGCC = "coal (IGCC)",
                     coal_IGCC_CCS = "coal (IGCC CCS)" ) %>%
      mutate( IGCC_CCS_no_CCS_2015_ratio = coal_IGCC_CCS / coal_IGCC ) %>%
      select( sector.name, subsector.name, IGCC_CCS_no_CCS_2015_ratio) -> elec_IGCC_2015_cost_ratio


    L223.GlobalTechEff_elec %>%
      filter( technology %in% c( "coal (IGCC)", "coal (IGCC CCS)",
                                        "biomass (IGCC)", "biomass (IGCC CCS)" ),
                     year == 2015 ) %>%
      mutate( technology = if_else( technology %in% c( "coal (IGCC CCS)", "biomass (IGCC CCS)" ),
                                                  "IGCC_CCS",
                                                  if_else( technology %in% c( "coal (IGCC)", "biomass (IGCC)" ),
                                                                  "IGCC_no_CCS", NA_character_ ) ) ) %>%
      spread( technology, efficiency ) %>%
      mutate( IGCC_CCS_no_CCS_2015_ratio = IGCC_CCS / IGCC_no_CCS ) %>%
      select( sector.name, subsector.name, IGCC_CCS_no_CCS_2015_ratio ) -> elec_IGCC_2015_eff_ratio



    elec_IGCC_2015_eff_ratio %>%
      filter( subsector.name == "biomass" ) -> elec_IGCC_2015_eff_ratio_bio

    elec_IGCC_2015_eff_ratio %>%
      filter( subsector.name == "coal" ) -> elec_IGCC_2015_eff_ratio_coal

    # B. Calculate improvement rate of CCS for biomass and coal IGCC electricity technologies
    #    Costs:
    L223.GlobalTechCapital_elec %>%
      filter( technology %in% c( "coal (IGCC)", "coal (IGCC CCS)",
                                        "biomass (IGCC)", "biomass (IGCC CCS)" ),
                     year %in% c( 2015, 2100 ) ) %>%
      mutate( technology = if_else(  technology %in% c( "coal (IGCC)", "biomass (IGCC)" ),
                                                   "without_CCS",
                                                   if_else(  technology %in% c( "coal (IGCC CCS)", "biomass (IGCC CCS)" ),
                                                                    "with_CCS", NA_character_ ) ) ) %>%
      spread( technology, capital.overnight ) %>%
      mutate( CCS_add_cost = with_CCS - without_CCS ) %>%
      select( sector.name, subsector.name, year, CCS_add_cost ) %>%
      spread( year, CCS_add_cost ) %>%
      mutate( max_improvement = ( 1 - ( !!rlang::sym( "2100" ) / !!rlang::sym( "2015" )  ) ),
                     technology = if_else( subsector.name == "coal", "coal (IGCC CCS)",
                                                  if_else( subsector.name == "biomass", "biomass (IGCC CCS)",
                                                                  NA_character_ ) ) ) %>%
      select( sector.name, subsector.name, technology, max_improvement ) -> elec_IGCC_CCS_cost_improvement


    #     Efficiency:
    L223.GlobalTechEff_elec %>%
      filter( technology %in% c( "coal (IGCC)", "coal (IGCC CCS)",
                                        "biomass (IGCC)", "biomass (IGCC CCS)" ),
                     year %in% c( 2015, 2100 ) ) %>%
      mutate( technology = if_else(  technology %in% c( "coal (IGCC)", "biomass (IGCC)" ),
                                                   "without_CCS",
                                                   if_else(  technology %in% c( "coal (IGCC CCS)", "biomass (IGCC CCS)" ),
                                                                    "with_CCS", NA_character_ ) ) ) %>%
      spread( technology, efficiency ) %>%
      mutate( CCS_sub_eff = with_CCS - without_CCS ) %>%
      select( sector.name, subsector.name, year, CCS_sub_eff ) %>%
      spread( year, CCS_sub_eff ) %>%
      mutate( max_improvement = ( 1 - ( !!rlang::sym( "2100" ) / !!rlang::sym( "2015" )  ) ),
                     technology = if_else( subsector.name == "coal", "coal (IGCC CCS)",
                                                  if_else( subsector.name == "biomass", "biomass (IGCC CCS)",
                                                                  NA_character_ ) ) ) %>%
      select( sector.name, subsector.name, technology, max_improvement ) -> elec_IGCC_CCS_eff_improvement


    # C. Costs: Calculate max improvement rate of nuclear power generation capital overnight costs
     L223.GlobalTechCapital_elec %>%
      filter( technology == "Gen_III" ,
                     year %in% c( 2015, 2100 ) ) %>%
      spread( year, capital.overnight ) %>%
      mutate( max_improvement = ( 1 - ( !!rlang::sym( "2100" ) / !!rlang::sym( "2015" )  ) ) ) %>%
      select( sector.name, subsector.name, technology, max_improvement ) -> elec_nuclear_cost_improvement



     #Convert Units

     H2A_prod_cost %>%
       select(-notes)%>%
       gather_years() %>%
       mutate(value = if_else(units == "$2016/kg H2", value * gdp_deflator(1975,2016),
                              if_else(units == "$2005/kg H2", value* gdp_deflator(1975,2005),
                                                         NA_real_ ) ) ) %>%
       mutate(value=value/CONV_GJ_KGH2)%>%
       mutate(units="$1975/GJ H2")-> H2A_prod_cost_conv


     H2A_prod_coef %>%
       select(-notes)%>%
       gather_years()%>%
       mutate(value = if_else(units == "GJ hydrogen output / GJ input", value^-1, #convert efficiency to coef
                              if_else(units == "GJ in /kg H2 out", value/CONV_GJ_KGH2, #convert to per GJ H2 basis
                                      NA_real_ ) ) ) %>%
       mutate(units='GJ input / GJ H2')-> H2A_prod_coef_conv


     H2A_prod_cost_conv %>%
       filter( technology %in% c( "biomass to H2", "coal chemical CCS" ) ) -> existing_coal_bio

     existing_coal_bio %>%
       filter( technology == "biomass to H2" ) -> bio_no_CCS

     bio_no_CCS_impro_2040 <- bio_no_CCS$improvement_to_2040[1]

     bio_no_CCS_max_improv <- bio_no_CCS$max_improvement[1]


     existing_coal_bio %>%
       mutate( value = if_else(subsector.name == "biomass",
                               value*elec_IGCC_2015_cost_ratio$IGCC_CCS_no_CCS_2015_ratio, #inflate bio to bioCCS cost...
                               if_else(subsector.name == "coal", #and deflate coal chemical CCS to coal chemical cost, using ratio of CCS to no CCS costs for IGCC elec
                                       value / elec_IGCC_2015_cost_ratio$IGCC_CCS_no_CCS_2015_ratio,NA_real_) ) ) %>%
       mutate( technology = if_else(subsector.name == "coal",
                                    "coal chemical",
                                    if_else( subsector.name == "biomass",
                                                "biomass to H2 CCS", NA_character_ ))) %>%
       #                     Set coal w/o CCS improvements equal to bio w/o CCS
       mutate(improvement_to_2040 = if_else( technology == "coal chemical",
                                             bio_no_CCS_impro_2040, NA_real_ ),
              max_improvement = if_else( technology == "coal chemical",
                                         bio_no_CCS_max_improv, NA_real_ ))%>%
       select(sector.name, subsector.name, technology, minicam.non.energy.input,
              units, year, value,improvement_to_2040,max_improvement) %>%
       mutate(improvement_to_2040 = approx_fun(year,improvement_to_2040, rule = 2)) %>%
       ungroup()-> add_coal_and_bio



     add_coal_and_bio %>%
       filter(technology=='coal chemical') -> coal_chem_costs_scaled

     coal_chem_costs_scaled %>%
       complete(nesting(sector.name, subsector.name, technology,minicam.non.energy.input), year = sort(unique(c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)))) %>%
       arrange(sector.name, subsector.name, technology, minicam.non.energy.input,year) %>%
       group_by(sector.name, subsector.name, technology, minicam.non.energy.input) %>%
       mutate(improvement_rate = (1-improvement_to_2040[year==2015])^(1/(2040-2015))-1) %>%
       mutate(min_cost=value[year==2015]*(1-max_improvement[year==2015])) %>%
       mutate(cost = if_else(year <= 2015,value[year==2015],value[year==2015]*(1+improvement_rate)^(year-2015))) %>%
       mutate(cost = if_else(cost>=min_cost,cost,min_cost))%>%
       fill(units,.direction='downup') -> coal_chem_costs_GCAM_years


     H2A_prod_cost_conv %>%
       filter(!( technology %in% c( "coal chemical", "biomass to H2 CCS" , "coal chemical CCS") ) ) -> H2A_NE_cost_add_2015_techs


     H2A_NE_cost_add_2015_techs %>%
       mutate( max_improvement = if_else( technology == "thermal splitting",
                                                        elec_nuclear_cost_improvement$max_improvement,
                                                        max_improvement ) ) -> H2A_NE_cost_add_nuclear


     H2A_NE_cost_add_nuclear %>%
       complete(nesting(sector.name, subsector.name, technology,minicam.non.energy.input), year = sort(unique(c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)))) %>%
       arrange(sector.name, subsector.name, technology, minicam.non.energy.input,year) %>%
       group_by(sector.name, subsector.name, technology, minicam.non.energy.input) %>%
       mutate(improvement_to_2040 = approx_fun(year,improvement_to_2040, rule = 2)) %>%
       mutate(max_improvement = approx_fun(year,max_improvement, rule = 2)) %>%
       mutate(improvement_rate = (1-improvement_to_2040)^(1/(2040-2015))-1) %>% #convert improvement by 2040 to annual compound growth rate
       mutate(min_cost = value[year==2015]*(1-max_improvement)) %>%
       mutate(cost = if_else(year <= 2015,value[year==2015],value[year==2015]*(1+improvement_rate)^(year-2015))) %>% #apply calculated CAGR from above to calculate cost declination pathway
       mutate(cost = if_else(cost>=min_cost,cost,min_cost))%>%
       fill(units,.direction='downup') %>% #fillout strings
       bind_rows(coal_chem_costs_GCAM_years) %>% #add back coal chem
       ungroup() -> H2A_NE_cost_GCAM_years


    # E. Create bio + CCS and extend coal w/CCS

    # First, set bio's CCS tech to the same improvement rate as coal's, otherwise bio + CCS gets cheaper than coal + CCS
    elec_IGCC_CCS_cost_improvement %>%
      filter(subsector.name == 'coal') -> coal_elec_IGCC_CCS_cost_improvement

    max_CCS_cost_improvement <- coal_elec_IGCC_CCS_cost_improvement$max_improvement


    add_coal_and_bio %>% # calculate the incremental cost of CCS
      select(-improvement_to_2040,-max_improvement) %>%
      bind_rows(existing_coal_bio %>% select(-improvement_to_2040,-max_improvement)) %>%
      filter(year==2015) %>%
      mutate(value = if_else(subsector.name=='biomass',value[technology=='biomass to H2 CCS']-value[technology=='biomass to H2'],#calculate difference between CCS, no CCS techs to get an incremental cost of CCS
                                if_else(subsector.name=='coal',value[technology=='coal chemical CCS']-value[technology=='coal chemical'],NA_real_))) %>%
      filter(technology %in% c("biomass to H2 CCS" , "coal chemical CCS"))%>%
      complete(nesting(sector.name, subsector.name, technology,minicam.non.energy.input), year = sort(unique(c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)))) %>%
      mutate(max_improvement = max_CCS_cost_improvement) %>%
      arrange(sector.name, subsector.name, technology, minicam.non.energy.input,year) %>%
      group_by(sector.name, subsector.name, technology, minicam.non.energy.input) %>%
      mutate(improvement_rate = (1-max_improvement)^(1/(2100-2015))-1) %>%
      mutate(min_cost = value[year==2015]*(1-max_improvement)) %>%
      mutate(cost = if_else(year <= 2015,value[year==2015],value[year==2015]*(1+improvement_rate)^(year-2015))) %>% #apply calculated CAGR from above to calculate cost declination pathway
      mutate(ccs_incr_cost = if_else(cost>=min_cost,cost,min_cost))%>%
      fill(units,.direction='downup') %>%
      ungroup() %>%
      select(subsector.name,year,ccs_incr_cost)-> ccs_incr_cost



    H2A_NE_cost_GCAM_years %>%
      filter(subsector.name %in% c('biomass','coal'))%>%
      select( sector.name, subsector.name, technology, minicam.non.energy.input,
                     units,cost,year) %>%
      arrange(sector.name, subsector.name, technology, minicam.non.energy.input,year) %>%
      group_by(sector.name, subsector.name, technology, minicam.non.energy.input) %>%
      left_join_error_no_match(ccs_incr_cost,by=c('subsector.name','year')) %>%
      mutate(cost = cost + ccs_incr_cost) %>%
      mutate(technology = if_else(subsector.name == 'biomass','biomass to H2 CCS',
                                  if_else(subsector.name == 'coal','coal chemical CCS',NA_character_))) %>%
      select(-ccs_incr_cost)-> coal_and_bio_w_ccs



    H2A_NE_cost_GCAM_years %>% # join missing CCS technologies with the rest of the data
      select( sector.name, subsector.name, technology, minicam.non.energy.input,
              units,cost,year) %>%
      bind_rows(coal_and_bio_w_ccs) -> L125.globaltech_cost


    write.csv(L125.globaltech_cost,'L125.globaltech_cost.csv')











    # Temporarily create the "return" data objects out of thin air
    L125.globaltech_coef <- H2A_prod_coef
    #L125.globaltech_cost <- H2A_prod_cost

    # ===================================================
    # Produce outputs

    L125.globaltech_coef %>%
      #add_title("Input-output coefficients of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated original data into all model years") %>%
      add_precursors("energy/A22.globaltech_coef", "energy/H2A_2018_prod_data_coef")  ->
      L125.globaltech_coef

    L125.globaltech_cost %>%
      add_title("Costs of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechCost_h2") %>%
      add_precursors("energy/H2A_2018_prod_data_cost")  ->
      L125.globaltech_cost

    return_data(L125.globaltech_coef, L125.globaltech_cost)
  } else {
    stop("Unknown command")
  }
}
