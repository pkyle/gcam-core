# This script calculates the fraction of generation by fuel 
# by horizontal segment in 2005 such that total supply of electricity in each grid region 
# matches total demand of electricity in that grid region.

# TODO: Include a check for negative fractions.

if( !exists( "GCAMUSAPROC_DIR" ) ){
  if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
    GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
  } else {
    stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
  }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "L1237.elec_load_segments_solver_2005.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Creating horizontal generation segments in the states" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

elecS_horizontal_to_vertical_map <- readdata( "GCAMUSA_LEVEL0_DATA","elecS_horizontal_to_vertical_map")
# This table maps horizontal segment names with vertical segment names.

L1236_grid_elec_supply <- readdata( "GCAMUSA_LEVEL1_DATA", "L1236.grid_elec_supply" )

L1234_out_EJ_grid_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA","L1234.out_EJ_grid_elec_F", skip = 5 )
#This table has electricity generation output by fuel aggregated at the grid region level

L1235_elecS_demand_fraction <- readdata( "GCAMUSA_LEVEL1_DATA","L1235.elecS_demand_fraction")
# elecS_demand_fraction is the fraction of demand supplied by the vertical segments by grid region

L1235_elecS_horizontal_vertical <- readdata( "GCAMUSA_LEVEL1_DATA","L1235.elecS_horizontal_vertical")
# This table specifies This table specifies the fraction of supply from horizontal segment available for vertical segment.

# -----------------------------------------------------------------------------
# 2. Perform computations
#Initialize Variables
model_base_year_time_step <- 5 # Need to calculate this programmatically 
script_year = max(model_base_years) - model_base_year_time_step
L1237_grid_elec_supply <- L1236_grid_elec_supply
L1237_elecS_demand_fraction <- L1235_elecS_demand_fraction
L1237_elecS_horizontal_vertical <- L1235_elecS_horizontal_vertical

L1237_grid_elec_supply %>%
  filter(year == script_year) -> L1237_grid_elec_supply_script_year 

L1237_grid_elec_supply %>%
  filter(year == model_base_years[match(script_year, model_base_years) + 1 ]) -> L1237_grid_elec_supply_next_year

L1237_grid_elec_supply %>%
  filter(year != script_year) -> L1237_grid_elec_supply_non_script_year

L1237_grid_elec_supply_script_year %>%
  left_join(L1237_grid_elec_supply_next_year, by = c("grid_region", "segment", "fuel")) %>%
  mutate(fraction.x = fraction.y) %>%
  mutate(generation.x = tot_generation.x * fraction.x) %>%
  select(grid_region, segment, fuel, year = year.x, tot_generation = tot_generation.x, fraction = fraction.x, generation = generation.x) %>%
  bind_rows(L1237_grid_elec_supply_non_script_year) -> L1237_grid_elec_supply

L1234_out_EJ_grid_elec_F %>%
  mutate(fuel = sub("solar CSP", "solar", fuel)) %>%
  mutate(fuel = sub("solar PV", "solar", fuel)) %>%
  group_by(grid_region, sector, year, fuel) %>%
  summarise_at("generation", sum) %>%
  ungroup() %>%
  filter(year %in% model_base_years) %>%
  rename(tot_generation = generation) -> L1237_out_EJ_grid_elec_F

L1237_segment_list <- unique(elecS_horizontal_to_vertical_map$horizontal_segment)
L1237_vertical_segment_list <- unique(elecS_horizontal_to_vertical_map$vertical_segment)
L1237_gridregion_list <- unique(L1237_grid_elec_supply$grid_region)

#Function to check demands and supplys by segment and L1237_region

check_elec_segments <- function (gen_fraction, L1237_region, L1237_segment, L1237_fuel = "gas") {
  #set fraction as specified
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == L1237_fuel 
                              & segment == L1237_segment & year == script_year, gen_fraction)) -> L1237_grid_elec_supply
  
  # If fuel = gas or oil, adjust peak fraction to make sure that sum of fractions is 1
  
  if (L1237_fuel == "gas" | L1237_fuel == "refined liquids") {
    
    L1237_grid_elec_supply %>%
      filter(grid_region == L1237_region & fuel == L1237_fuel & segment != L1237_segment_list[4] & year == script_year) %>% 
      summarise(nonpeak_frac = sum(fraction)) %>% 
      .[['nonpeak_frac']] -> L1237_nonpeak
    
    L1237_grid_elec_supply %>%
      mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == L1237_fuel 
                                & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply
  }
  
  # If fuel = coal, adjust intermediate fraction to make sure sum of fractions is 1
  
  if (L1237_fuel == "coal" | L1237_fuel == "hydro") {
    
    L1237_grid_elec_supply %>%
      filter(grid_region == L1237_region & fuel == L1237_fuel & segment != L1237_segment_list[2] & year == script_year) %>% 
      summarise(non_int_frac = sum(fraction)) %>% 
      .[['non_int_frac']] -> L1237_non_int
    
    L1237_grid_elec_supply %>%
      mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == L1237_fuel 
                                & segment == L1237_segment_list[2] & year == script_year, 1-L1237_non_int)) -> L1237_grid_elec_supply
  }
  
  L1237_grid_elec_supply %>%
    mutate(generation = tot_generation * fraction) -> L1237_grid_elec_supply
  
  #Calculate supply by horizontal segment in each grid region by aggregating all technologies 
  
  L1237_grid_elec_supply %>%
    group_by(grid_region, segment, year) %>%
    summarise_at("generation", sum) %>% 
    ungroup() -> L1237_grid_check
  
  #Calculate demand for each horizontal segment by vertical segments in each grid region 
  
  L1237_grid_elec_supply %>%
    group_by(grid_region, year) %>%
    summarise_at("generation", sum) %>%
    ungroup() %>%
    rename (tot_demand = generation) -> L1237_grid_elec_demand
  
  L1237_grid_check %>%
    left_join(L1237_grid_elec_demand, by = c("grid_region","year")) %>%
    left_join(elecS_horizontal_to_vertical_map, by = c("segment" = "horizontal_segment")) %>%
    left_join (L1237_elecS_demand_fraction , by = c("grid_region", "vertical_segment")) %>%
    mutate(vertical_segment_demand = tot_demand * demand_fraction) -> L1237_grid_elec_demand
  
  L1237_grid_check %>%
    left_join(L1237_grid_elec_demand, by = c("grid_region", "segment", "year")) %>%
    select(grid_region, segment, year, generation.x, vertical_segment_demand) %>%
    rename(generation = generation.x) -> L1237_grid_check
  
  L1237_grid_check %>%
    filter(segment == L1237_segment_list[1] ) -> L1237_grid_check_base
  
  L1237_grid_check %>%
    filter(segment == L1237_segment_list[2] ) -> L1237_grid_check_int
  
  L1237_grid_check %>%
    filter(segment == L1237_segment_list[3] ) -> L1237_grid_check_subpeak
  
  L1237_grid_check %>%
    filter(segment == L1237_segment_list[4] ) -> L1237_grid_check_peak
  
  L1237_grid_check_base %>%
    left_join(L1237_elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
    mutate(horizontal_segment_demand = vertical_segment_demand/off.peak.electricity) %>%
    mutate(base_intermediate = horizontal_segment_demand*intermediate.electricity) %>%
    mutate(base_subpeak = horizontal_segment_demand*subpeak.electricity) %>%
    mutate(base_peak = horizontal_segment_demand*peak.electricity) -> L1237_grid_check_base
  
  L1237_grid_check_int %>%
    left_join(L1237_elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
    left_join(L1237_grid_check_base, by = c("grid_region", "year")) %>%
    mutate(horizontal_segment_demand = (vertical_segment_demand.x - base_intermediate)/intermediate.electricity.x ) %>%
    mutate(int_subpeak = horizontal_segment_demand*subpeak.electricity.x) %>%
    mutate(int_peak = horizontal_segment_demand*peak.electricity.x)-> L1237_grid_check_int
  
  L1237_grid_check_subpeak %>%
    left_join(L1237_elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
    left_join(L1237_grid_check_int, by = c("grid_region", "year")) %>%
    mutate(horizontal_segment_demand = (vertical_segment_demand - base_subpeak - int_subpeak)/subpeak.electricity) %>%
    mutate(subpeak_peak = horizontal_segment_demand*peak.electricity)-> L1237_grid_check_subpeak
  
  L1237_grid_check_peak %>%
    left_join(L1237_elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
    left_join(L1237_grid_check_subpeak, by = c("grid_region", "year")) %>%
    mutate(horizontal_segment_demand = (vertical_segment_demand.x.x - base_peak - int_peak - subpeak_peak)/peak.electricity.x.x) ->  L1237_grid_check_peak  
  
  L1237_grid_check_base %>%
    select(grid_region, segment, year, generation, vertical_segment_demand, horizontal_segment_demand) -> L1237_grid_check_base
  
  L1237_grid_check_int %>%
    select(grid_region, segment = segment.x, year, generation = generation.x, vertical_segment_demand = vertical_segment_demand.x, horizontal_segment_demand) -> L1237_grid_check_int
  
  L1237_grid_check_subpeak %>%
    select(grid_region, segment, year, generation, vertical_segment_demand, horizontal_segment_demand) -> L1237_grid_check_subpeak
  
  L1237_grid_check_peak %>%
    select(grid_region, segment = segment.x.x, year, generation = generation.x.x, vertical_segment_demand = vertical_segment_demand.x.x, horizontal_segment_demand) -> L1237_grid_check_peak
  
  L1237_grid_check_base %>%
    bind_rows(L1237_grid_check_int) %>%
    bind_rows(L1237_grid_check_subpeak) %>%
    bind_rows(L1237_grid_check_peak) -> L1237_grid_check
  
  #Check that supply meets demand
  L1237_grid_check %>%
    mutate(check = horizontal_segment_demand - generation) %>%
    mutate(pct_check = check/generation) -> L1237_grid_check
  
  L1237_grid_check %>%
    filter(grid_region == L1237_region & segment == L1237_segment & year == script_year) %>%
    select(check) %>%
    .[['check']] -> check
  
  check
}

L1237_out_EJ_grid_elec_F %>%
  group_by(grid_region, sector, year) %>%
  summarise_at("tot_generation",sum) %>%
  ungroup() %>%
  rename(grid_total = tot_generation) -> L1237_grid_total

L1237_out_EJ_grid_elec_F %>%
  left_join(L1237_grid_total, by = c("grid_region", "sector", "year")) %>%
  mutate(grid_share_fuel = tot_generation/grid_total) -> L1237_out_EJ_grid_elec_F

#Loop through each gridregion
for (j in 1:length(L1237_gridregion_list)){
  L1237_region <- L1237_gridregion_list[j]

  L1237_out_EJ_grid_elec_F %>%
    filter(grid_region == L1237_region & fuel == "gas" & year == script_year) %>%
    select(grid_share_fuel) %>%
    .[['grid_share_fuel']] -> L1237_gas_frac
  
  L1237_out_EJ_grid_elec_F %>%
    filter(grid_region == L1237_region & fuel == "refined liquids" & year == script_year) %>%
    select(grid_share_fuel) %>%
    .[['grid_share_fuel']] -> L1237_oil_frac
  
  L1237_out_EJ_grid_elec_F %>%
    filter(grid_region == L1237_region & fuel == "coal" & year == script_year) %>%
    select(grid_share_fuel) %>%
    .[['grid_share_fuel']] -> L1237_coal_frac
  
  L1237_out_EJ_grid_elec_F %>%
    filter(grid_region == L1237_region & fuel == "hydro" & year == script_year) %>%
    select(grid_share_fuel) %>%
    .[['grid_share_fuel']] -> L1237_hydro_frac
  
# For oil heavy regions such as Hawaii, solve for oil fractions. This will allow for some oil in baseload and intermediate segments.


  if (L1237_oil_frac > 0.5) {
  
  # Solve for oil fractions
  
  L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[1], "refined liquids")
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids" 
                              & segment == L1237_segment_list[1] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
  
  L1237_grid_elec_supply %>%
    filter(grid_region == L1237_region & fuel == "refined liquids" & segment != L1237_segment_list[4] 
           & year == script_year) %>% 
    summarise(nonpeak_frac = sum(fraction)) %>% 
    .[['nonpeak_frac']] -> L1237_nonpeak
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids"
                              & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply
  
  L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[2], "refined liquids")
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids" 
                              & segment == L1237_segment_list[2] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
  L1237_grid_elec_supply %>%
    filter(grid_region == L1237_region & fuel == "refined liquids" & segment != L1237_segment_list[4] 
           & year == script_year) %>% 
    summarise(nonpeak_frac = sum(fraction)) %>% 
    .[['nonpeak_frac']] -> L1237_nonpeak
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids"
                              & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply
  
  L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[3], "refined liquids")
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids" 
                              & segment == L1237_segment_list[3] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
  
  L1237_grid_elec_supply %>%
    filter(grid_region == L1237_region & fuel == "refined liquids" & segment != L1237_segment_list[4] 
           & year == script_year) %>% 
    summarise(nonpeak_frac = sum(fraction)) %>% 
    .[['nonpeak_frac']] -> L1237_nonpeak
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids"
                              & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply
  
  } else if (L1237_gas_frac > 0.2 & L1237_coal_frac < 0.5) {
  
  # If gridregion has 20% or more gas, solve for gas fractions. To reduce solution error, let's increase coal in base load
  # Let's also assign some oil to base load. 
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "coal" 
                              & segment == L1237_segment_list[1] & year == script_year, 0.95)) %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "coal" 
                              & segment == L1237_segment_list[2] & year == script_year, 0.05)) %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids" 
                              & segment == L1237_segment_list[1] & year == script_year, 0.8)) %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids" 
                              & segment == L1237_segment_list[2] & year == script_year, 0.1)) %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids" 
                              & segment == L1237_segment_list[3] & year == script_year, 0.1)) %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids" 
                              & segment == L1237_segment_list[4] & year == script_year, 0)) -> L1237_grid_elec_supply

   # Solve for gas fractions
  
  L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[1])
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas" 
                              & segment == L1237_segment_list[1] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
  
  L1237_grid_elec_supply %>%
    filter(grid_region == L1237_region & fuel == "gas" & segment != L1237_segment_list[4] 
           & year == script_year) %>% 
    summarise(nonpeak_frac = sum(fraction)) %>% 
    .[['nonpeak_frac']] -> L1237_nonpeak
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas"
                              & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply
  
  L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[2])
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas" 
                              & segment == L1237_segment_list[2] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
  L1237_grid_elec_supply %>%
    filter(grid_region == L1237_region & fuel == "gas" & segment != L1237_segment_list[4] 
           & year == script_year) %>% 
    summarise(nonpeak_frac = sum(fraction)) %>% 
    .[['nonpeak_frac']] -> L1237_nonpeak
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas"
                              & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply
  
  L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[3])
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas" 
                              & segment == L1237_segment_list[3] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
  
  L1237_grid_elec_supply %>%
    filter(grid_region == L1237_region & fuel == "gas" & segment != L1237_segment_list[4] 
           & year == script_year) %>% 
    summarise(nonpeak_frac = sum(fraction)) %>% 
    .[['nonpeak_frac']] -> L1237_nonpeak
  
  L1237_grid_elec_supply %>%
    mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas"
                              & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply

  } else if (L1237_coal_frac > L1237_gas_frac & L1237_coal_frac < 0.55) {

      # For regions with moderate levels of coal such as Southeast grid, Northwest grid, Mid-Atlantic grid
      # and Central Southwest grid, allocate some coal to intermediate. 
    
        # Solve for coal in baseload and assign the remaining to intermediate
    
      L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[1], "coal")
    
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "coal" 
                                & segment == L1237_segment_list[1] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
      L1237_grid_elec_supply %>%
        filter(grid_region == L1237_region & fuel == "coal" & segment != L1237_segment_list[2] & year == script_year) %>% 
        summarise(non_int_frac = sum(fraction)) %>% 
        .[['non_int_frac']] -> L1237_non_int
    
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "coal" 
                                  & segment == L1237_segment_list[2] & year == script_year, 1-L1237_non_int)) -> L1237_grid_elec_supply
      
      # Solve for gas fractions in intermediate, subpeak and peak
    
      L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[2], "gas")
    
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas" 
                                & segment == L1237_segment_list[2] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
      L1237_grid_elec_supply %>%
        filter(grid_region == L1237_region & fuel == "gas" & segment != L1237_segment_list[4] 
              & year == script_year) %>% 
        summarise(nonpeak_frac = sum(fraction)) %>% 
        .[['nonpeak_frac']] -> L1237_nonpeak
    
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas"
                                  & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply
    
      L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[3], "gas")
      
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas" 
                                  & segment == L1237_segment_list[3] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
      
      L1237_grid_elec_supply %>%
        filter(grid_region == L1237_region & fuel == "gas" & segment != L1237_segment_list[4] 
               & year == script_year) %>% 
        summarise(nonpeak_frac = sum(fraction)) %>% 
        .[['nonpeak_frac']] -> L1237_nonpeak
      
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas"
                                  & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply
      
    } else {

      # In regions with high levels of coal such as Central Northeast, Central East, Central Northwest and Sothwest grids, allocate some wind into 
      # intermediate, and subpeak segments first. Second, assign some refined liquids into the subpeak. 
      # Then follow the same steps as above. Note that we also read in different load curves for these regions 
      # with lower peak demand. 

      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "wind" 
                                  & segment == L1237_segment_list[1] & year == script_year, 
                                  0.6)) %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "wind" 
                                  & segment == L1237_segment_list[2] & year == script_year, 
                                  0.25))%>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "wind" 
                                  & segment == L1237_segment_list[3] & year == script_year, 
                                  0.15))-> L1237_grid_elec_supply
      
  
      # Assigning all refined liquids into subpeak 
  
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids" 
                                  & segment == L1237_segment_list[3] & year == script_year, 1)) %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "refined liquids" 
                                  & segment == L1237_segment_list[4] & year == script_year, 0)) -> L1237_grid_elec_supply
      
      # Solve for coal in baseload and assign the remaining to intermediate
      L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[1], "coal")
      
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "coal" 
                                  & segment == L1237_segment_list[1] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
      
      L1237_grid_elec_supply %>%
        filter(grid_region == L1237_region & fuel == "coal" & segment != L1237_segment_list[2] & year == script_year) %>% 
        summarise(non_int_frac = sum(fraction)) %>% 
        .[['non_int_frac']] -> L1237_non_int
      
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "coal" 
                                  & segment == L1237_segment_list[2] & year == script_year, 1-L1237_non_int)) -> L1237_grid_elec_supply
      
        # Solve for gas fractions in intermediate, subpeak and peak
      L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[2], "gas")
      
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas" 
                                  & segment == L1237_segment_list[2] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
      
      L1237_grid_elec_supply %>%
        filter(grid_region == L1237_region & fuel == "gas" & segment != L1237_segment_list[4] 
               & year == script_year) %>% 
        summarise(nonpeak_frac = sum(fraction)) %>% 
        .[['nonpeak_frac']] -> L1237_nonpeak
      
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas"
                                  & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply
      
      
      L1237_solved_fraction <- uniroot(check_elec_segments, c(0,1), L1237_region, L1237_segment_list[3], "gas")
      
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas" 
                                  & segment == L1237_segment_list[3] & year == script_year, L1237_solved_fraction$root)) -> L1237_grid_elec_supply
      
      L1237_grid_elec_supply %>%
        filter(grid_region == L1237_region & fuel == "gas" & segment != L1237_segment_list[4] 
               & year == script_year) %>% 
        summarise(nonpeak_frac = sum(fraction)) %>% 
        .[['nonpeak_frac']] -> L1237_nonpeak
      
      L1237_grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1237_region & fuel == "gas"
                                  & segment == L1237_segment_list[4] & year == script_year, 1-L1237_nonpeak)) -> L1237_grid_elec_supply
    }
}
  

# Re-do check calculations to output

L1237_grid_elec_supply %>%
  mutate(generation = tot_generation * fraction) -> L1237_grid_elec_supply

#Calculate supply by horizontal segment in each grid region by aggregating all technologies 

L1237_grid_elec_supply %>%
  group_by(grid_region, segment, year) %>%
  summarise_at("generation", sum) %>% 
  ungroup() -> L1237_grid_check

#Calculate demand for each horizontal segment by vertical segments in each grid region 

L1237_grid_elec_supply %>%
  group_by(grid_region, year) %>%
  summarise_at("generation", sum) %>%
  ungroup() %>%
  rename (tot_demand = generation) -> L1237_grid_elec_demand

L1237_grid_check %>%
  left_join(L1237_grid_elec_demand, by = c("grid_region","year")) %>%
  left_join(elecS_horizontal_to_vertical_map, by = c("segment" = "horizontal_segment")) %>%
  left_join (L1237_elecS_demand_fraction , by = c("grid_region", "vertical_segment")) %>%
  mutate(vertical_segment_demand = tot_demand * demand_fraction) -> L1237_grid_elec_demand

L1237_grid_check %>%
  left_join(L1237_grid_elec_demand, by = c("grid_region", "segment", "year")) %>%
  select(grid_region, segment, year, generation.x, vertical_segment_demand) %>%
  rename(generation = generation.x) -> L1237_grid_check

L1237_grid_check %>%
  filter(segment == L1237_segment_list[1] ) -> L1237_grid_check_base

L1237_grid_check %>%
  filter(segment == L1237_segment_list[2] ) -> L1237_grid_check_int

L1237_grid_check %>%
  filter(segment == L1237_segment_list[3] ) -> L1237_grid_check_subpeak

L1237_grid_check %>%
  filter(segment == L1237_segment_list[4] ) -> L1237_grid_check_peak

L1237_grid_check_base %>%
  left_join(L1237_elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
  mutate(horizontal_segment_demand = vertical_segment_demand/off.peak.electricity) %>%
  mutate(base_intermediate = horizontal_segment_demand*intermediate.electricity) %>%
  mutate(base_subpeak = horizontal_segment_demand*subpeak.electricity) %>%
  mutate(base_peak = horizontal_segment_demand*peak.electricity) -> L1237_grid_check_base

L1237_grid_check_int %>%
  left_join(L1237_elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
  left_join(L1237_grid_check_base, by = c("grid_region", "year")) %>%
  mutate(horizontal_segment_demand = (vertical_segment_demand.x - base_intermediate)/intermediate.electricity.x ) %>%
  mutate(int_subpeak = horizontal_segment_demand*subpeak.electricity.x) %>%
  mutate(int_peak = horizontal_segment_demand*peak.electricity.x)-> L1237_grid_check_int

L1237_grid_check_subpeak %>%
  left_join(L1237_elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
  left_join(L1237_grid_check_int, by = c("grid_region", "year")) %>%
  mutate(horizontal_segment_demand = (vertical_segment_demand - base_subpeak - int_subpeak)/subpeak.electricity) %>%
  mutate(subpeak_peak = horizontal_segment_demand*peak.electricity)-> L1237_grid_check_subpeak

L1237_grid_check_peak %>%
  left_join(L1237_elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
  left_join(L1237_grid_check_subpeak, by = c("grid_region", "year")) %>%
  mutate(horizontal_segment_demand = (vertical_segment_demand.x.x - base_peak - int_peak - subpeak_peak)/peak.electricity.x.x) ->  L1237_grid_check_peak  

L1237_grid_check_base %>%
  select(grid_region, segment, year, generation, vertical_segment_demand, horizontal_segment_demand) -> L1237_grid_check_base

L1237_grid_check_int %>%
  select(grid_region, segment = segment.x, year, generation = generation.x, vertical_segment_demand = vertical_segment_demand.x, horizontal_segment_demand) -> L1237_grid_check_int

L1237_grid_check_subpeak %>%
  select(grid_region, segment, year, generation, vertical_segment_demand, horizontal_segment_demand) -> L1237_grid_check_subpeak

L1237_grid_check_peak %>%
  select(grid_region, segment = segment.x.x, year, generation = generation.x.x, vertical_segment_demand = vertical_segment_demand.x.x, horizontal_segment_demand) -> L1237_grid_check_peak

L1237_grid_check_base %>%
  bind_rows(L1237_grid_check_int) %>%
  bind_rows(L1237_grid_check_subpeak) %>%
  bind_rows(L1237_grid_check_peak) -> L1237_grid_check

#Check that supply meets demand
L1237_grid_check %>%
  mutate(check = horizontal_segment_demand - generation) %>%
  mutate(pct_check = check/generation) -> L1237_grid_check

# -----------------------------------------------------------------------------
# 3. Output

comments.L1237_grid_elec_supply <- c( "Supply by fuel by horizontal segment in each grid region based on calculated fraction of fuel in the horizontal segments",  "All physical quantitites in EJ" )
comments.L1237_grid_elec_demand <- c( "Demand by segment in each grid region",  "All physical quantitites in EJ" )
comments.L1237_grid_check <- c( "Supplies and Demands by segment",  "All physical quantitites in EJ" )

writedata(L1237_grid_elec_supply, domain="GCAMUSA_LEVEL1_DATA", fn="L1237.grid_elec_supply", comments=comments.L1237_grid_elec_supply )
writedata(L1237_grid_elec_demand, domain="GCAMUSA_LEVEL1_DATA", fn="L1237.grid_elec_demand", comments=comments.L1237_grid_elec_demand)
writedata(L1237_grid_check, domain="GCAMUSA_LEVEL1_DATA", fn="L1237.grid_check", comments=comments.L1237_grid_check)

# Every script should finish with this line
logstop()
