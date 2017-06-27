# This script takes in demand fraction and time fraction and computes load curve related parameters 
# The script also takes in generation by fuel by grid region (in EJ) and calculates an initial estimate of 
# the amount (in EJ) of generation by each fuel in the horizontal segments using an exogenously specified 
# initial estimate of the fraction of generation by fuel by horizontal segment.
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
logstart( "LB1235_elec_load_segments.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))

printlog( "Initial estimate of fraction of generation by fuel by horizontal segment" )
# -----------------------------------------------------------------------------
# 1. Inputs. Need to read these from csv's.
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )

L1234_out_EJ_grid_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA","L1234.out_EJ_grid_elec_F", skip = 5 )
#This table has electricity generation output by fuel aggregated at the grid region level

elecS_demand_fraction <- readdata( "GCAMUSA_LEVEL0_DATA","elecS_demand_fraction")
# elecS_demand_fraction is the fraction of demand supplied by the vertical segments by grid region

elecS_time_fraction <- readdata( "GCAMUSA_LEVEL0_DATA","elecS_time_fraction")
# elecS_time_fraction is the fraction of time for each of vertical segments by grid region

elecS_fuel_fraction <- readdata( "GCAMUSA_LEVEL0_DATA","elecS_fuel_fraction")
#This table has initial estimates of the fraction of generation by fuel by horizontal segment

elecS_horizontal_to_vertical_map <- readdata( "GCAMUSA_LEVEL0_DATA","elecS_horizontal_to_vertical_map")
# This table maps horizontal segment names with vertical segment names.

# -----------------------------------------------------------------------------
# 2. Perform computations

horizontal_segment_list <- unique(elecS_horizontal_to_vertical_map$horizontal_segment)
vertical_segment_list <- unique(elecS_horizontal_to_vertical_map$vertical_segment)

# Computing the fraction of vertical segment supplied by each horizontal segment.These fractions will be used to calculate
# horizontal segment demands to check if supplies match demands in calibration years. Note that these are not the same as
# GCAM I/O coefficients which are calculated subsequently. 

elecS_time_fraction %>%
  mutate(horizontal_segment = horizontal_segment_list[1]) -> elecS_horizontal_vertical_base
  
elecS_time_fraction %>%
  mutate(horizontal_segment = horizontal_segment_list[2]) %>%
  mutate(off.peak.electricity.time = 0) %>%
  mutate(intermediate.sum = intermediate.electricity.time + subpeak.electricity.time + peak.electricity.time) %>%
  mutate(intermediate.electricity.time = intermediate.electricity.time/intermediate.sum) %>%
  mutate(subpeak.electricity.time = subpeak.electricity.time/ intermediate.sum) %>%
  mutate(peak.electricity.time = peak.electricity.time/ intermediate.sum) %>%
  select(-intermediate.sum) -> elecS_horizontal_vertical_intermediate
  
elecS_time_fraction %>%
  mutate(horizontal_segment = horizontal_segment_list[3]) %>%
  mutate(off.peak.electricity.time = 0, intermediate.electricity.time = 0) %>%
  mutate(subpeak.sum = subpeak.electricity.time + peak.electricity.time) %>%
  mutate(subpeak.electricity.time = subpeak.electricity.time/ subpeak.sum) %>%
  mutate(peak.electricity.time = peak.electricity.time/ subpeak.sum) %>%
  select(-subpeak.sum) -> elecS_horizontal_vertical_subpeak

elecS_time_fraction %>%
  mutate(horizontal_segment = horizontal_segment_list[4]) %>%
  mutate(off.peak.electricity.time = 0, intermediate.electricity.time = 0, subpeak.electricity.time = 0) %>%
  mutate(peak.electricity.time = 1) -> elecS_horizontal_vertical_peak

elecS_horizontal_vertical_base %>%
  bind_rows(elecS_horizontal_vertical_intermediate) %>%
  bind_rows(elecS_horizontal_vertical_subpeak) %>%
  bind_rows (elecS_horizontal_vertical_peak) %>%
  arrange(grid_region) %>%
  select(grid_region, horizontal_segment, off.peak.electricity = off.peak.electricity.time, 
         intermediate.electricity = intermediate.electricity.time,
         subpeak.electricity = subpeak.electricity.time, 
         peak.electricity = peak.electricity.time)-> L1235.elecS_horizontal_vertical

# Computing GCAM I/O coefficients
 
# First compute % of load supplied by horizontal segments 
elecS_demand_fraction %>%
  left_join(elecS_time_fraction, by ="grid_region") %>%
  mutate(horizontal_segment = horizontal_segment_list[1]) %>%
  mutate(off.peak.electricity = off.peak.electricity.demand) %>%
  mutate(intermediate.electricity = off.peak.electricity * intermediate.electricity.time/off.peak.electricity.time) %>%
  mutate(subpeak.electricity = off.peak.electricity * subpeak.electricity.time/off.peak.electricity.time) %>%
  mutate(peak.electricity = off.peak.electricity * peak.electricity.time/off.peak.electricity.time) -> elecS_horizontal_vertical_GCAM_coeff_base

elecS_horizontal_vertical_GCAM_coeff_base %>%
  select(grid_region, off.peak.electricity, intermediate.electricity, subpeak.electricity, peak.electricity) -> elecS_horizontal_vertical_GCAM_coeff_base_elec # to be used subsequently in a left join


elecS_horizontal_vertical_GCAM_coeff_base %>%
  mutate(horizontal_segment = horizontal_segment_list[2]) %>%
  mutate(off.peak.electricity = 0) %>%
  mutate(intermediate.electricity = intermediate.electricity.demand - intermediate.electricity) %>%
  mutate(subpeak.electricity = intermediate.electricity * subpeak.electricity.time/intermediate.electricity.time) %>%
  mutate(peak.electricity = intermediate.electricity * peak.electricity.time/intermediate.electricity.time) -> elecS_horizontal_vertical_GCAM_coeff_intermediate

elecS_horizontal_vertical_GCAM_coeff_intermediate %>%
  select(grid_region, off.peak.electricity, intermediate.electricity,subpeak.electricity, peak.electricity) -> elecS_horizontal_vertical_GCAM_coeff_int_elec # to be used subsequently in a left join


elecS_horizontal_vertical_GCAM_coeff_intermediate %>%
  mutate(horizontal_segment = horizontal_segment_list[3]) %>%
  mutate(off.peak.electricity = 0) %>%
  mutate(intermediate.electricity = 0) %>%
  left_join(elecS_horizontal_vertical_GCAM_coeff_base_elec, by = c("grid_region")) %>%
  mutate(subpeak.electricity.x = subpeak.electricity.demand - subpeak.electricity.x - subpeak.electricity.y) %>%
  mutate(peak.electricity.x = subpeak.electricity.x * peak.electricity.time/subpeak.electricity.time) %>%
  select(-off.peak.electricity.y, -intermediate.electricity.y, -subpeak.electricity.y, -peak.electricity.y) %>%
  rename(off.peak.electricity = off.peak.electricity.x,
         intermediate.electricity = intermediate.electricity.x,
         subpeak.electricity = subpeak.electricity.x,
         peak.electricity= peak.electricity.x) -> elecS_horizontal_vertical_GCAM_coeff_subpeak
  
  
elecS_horizontal_vertical_GCAM_coeff_subpeak %>%
  mutate(horizontal_segment = horizontal_segment_list[4]) %>%
  mutate(off.peak.electricity = 0) %>%
  mutate(intermediate.electricity = 0) %>%
  mutate(subpeak.electricity = 0) %>%
  left_join(elecS_horizontal_vertical_GCAM_coeff_int_elec, by = c("grid_region")) %>%
  left_join(elecS_horizontal_vertical_GCAM_coeff_base_elec, by = c("grid_region")) %>%
  mutate(peak.electricity.x = peak.electricity.demand - peak.electricity - peak.electricity.y - peak.electricity.x) %>%
  select(-off.peak.electricity.y, -intermediate.electricity.y, -subpeak.electricity.y, -peak.electricity.y,
         -off.peak.electricity, -intermediate.electricity, -subpeak.electricity, -peak.electricity) %>%
  rename(off.peak.electricity = off.peak.electricity.x,
         intermediate.electricity = intermediate.electricity.x,
         subpeak.electricity = subpeak.electricity.x,
         peak.electricity= peak.electricity.x) -> elecS_horizontal_vertical_GCAM_coeff_peak


elecS_horizontal_vertical_GCAM_coeff_base %>%
  bind_rows(elecS_horizontal_vertical_GCAM_coeff_intermediate) %>%
  bind_rows(elecS_horizontal_vertical_GCAM_coeff_subpeak) %>%
  bind_rows(elecS_horizontal_vertical_GCAM_coeff_peak) %>%
  arrange(grid_region) %>%
  select(grid_region, horizontal_segment, off.peak.electricity, intermediate.electricity,
         subpeak.electricity, peak.electricity)-> L1235.elecS_horizontal_vertical_GCAM_coeff

# The final GCAM coefficients will be obtained by dividing the percentage of load supplied by
# horizontal segments by total load in the vertical segments

L1235.elecS_horizontal_vertical_GCAM_coeff %>%
  group_by(grid_region) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  rename(off.peak.electricity.tot = off.peak.electricity,
         intermediate.electricity.tot = intermediate.electricity,
         subpeak.electricity.tot = subpeak.electricity,
         peak.electricity.tot = peak.electricity) -> L1235.elecS_horizontal_vertical_GCAM_coeff_tot

L1235.elecS_horizontal_vertical_GCAM_coeff %>%
  left_join(L1235.elecS_horizontal_vertical_GCAM_coeff_tot, by="grid_region") %>%
  mutate(off.peak.electricity = off.peak.electricity/ off.peak.electricity.tot) %>%
  mutate(intermediate.electricity = intermediate.electricity/ intermediate.electricity.tot) %>%
  mutate(subpeak.electricity = subpeak.electricity/ subpeak.electricity.tot) %>%
  mutate(peak.electricity = peak.electricity/ peak.electricity.tot) %>%
  select(-off.peak.electricity.tot, -intermediate.electricity.tot,-subpeak.electricity.tot, - peak.electricity.tot) -> L1235.elecS_horizontal_vertical_GCAM_coeff

# Gather the GCAM coefficients in long form
# TODO: Include a check for negative coefficients.

L1235.elecS_horizontal_vertical_GCAM_coeff %>%
  gather(supplysector, coefficient, -grid_region, -horizontal_segment) %>%
  mutate(supplysector = gsub("off.peak.electricity","off peak electricity",supplysector)) %>%
  mutate(supplysector = gsub("intermediate.electricity","intermediate electricity",supplysector)) %>%
  mutate(supplysector = gsub("subpeak.electricity","subpeak electricity",supplysector))%>%
  mutate(supplysector = gsub("peak.electricity","peak electricity",supplysector)) %>%
  mutate(subsector = supplysector, technology = supplysector) %>%
  rename(region = grid_region, minicam.energy.input = horizontal_segment) %>%
  filter(coefficient != 0) %>%
  select(region, supplysector, subsector, technology, minicam.energy.input, coefficient) %>%
  arrange(region) -> L1235.elecS_horizontal_vertical_GCAM_coeff
 
# The rest of the script calculates an initial estimate of the amount (in EJ) of 
# generation by each fuel in the horizontal segments

# Gathering some information in long-form first
elecS_demand_fraction %>%
  gather(vertical_segment, demand_fraction, -grid_region) %>%
  mutate(vertical_segment = gsub("off.peak.electricity.demand","off peak electricity",vertical_segment)) %>%
  mutate(vertical_segment = gsub("intermediate.electricity.demand","intermediate electricity",vertical_segment)) %>%
  mutate(vertical_segment = gsub("subpeak.electricity.demand","subpeak electricity",vertical_segment))%>%
  mutate(vertical_segment = gsub("peak.electricity.demand","peak electricity",vertical_segment))-> L1235.elecS_demand_fraction

elecS_fuel_fraction %>%
  gather(year, fraction, -fuel, -segment) %>%
  mutate(year = gsub("fraction", "",year)) %>%
  mutate(year = as.numeric(year)) -> elecS_fuel_fraction

L1234_out_EJ_grid_elec_F %>%
  mutate(fuel = sub("solar CSP", "solar", fuel)) %>%
  mutate(fuel = sub("solar PV", "solar", fuel)) %>%
  group_by(grid_region, sector, fuel, year) %>%
  summarise_at("generation", sum) %>% 
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% model_base_years) -> L1235_grid_elec_supply

L1235_grid_elec_supply %>%
  left_join(elecS_fuel_fraction, by = c("fuel","year")) %>%
  filter(is.na(fraction) != TRUE) %>%
  mutate(generation = generation * fraction) %>%
  select(grid_region, segment, fuel, year, generation, fraction) -> L1235_grid_elec_supply
  
#Aggregate all technologies to get total gridregional supply by generation segment

L1235_grid_elec_supply %>%
  group_by(grid_region, segment, year) %>%
  summarise_at("generation", sum) %>% 
  ungroup() -> L1235_grid_elec_segments

L1235_grid_elec_segments %>%
  rename(tot_demand = generation) %>%
  group_by(grid_region, year) %>%
  summarise_at("tot_demand", sum) %>% 
  ungroup() ->  L1235_grid_elec_demand

L1235_grid_elec_demand %>%
  left_join(L1235.elecS_demand_fraction, by= c("grid_region")) %>%
  mutate(demand = tot_demand * demand_fraction) %>%
  left_join(elecS_horizontal_to_vertical_map, by = "vertical_segment") -> L1235_grid_elec_demand

L1235_grid_elec_segments %>%
  left_join(L1235_grid_elec_demand, by = c("segment" = "horizontal_segment", "grid_region", "year")) %>%
  mutate(check = demand - generation)-> L1235_grid_elec_segments

# -----------------------------------------------------------------------------
# 3. Output
comments.L1235_grid_elec_segments <- c( "initial estimates of the amount of generation by each fuel in the horizontal segments",  "All physical quantitites in EJ" )
comments.L1235_grid_elec_supply <- c( "Supply by fule by horizontal segment in each grid region based on initial estimates of fraction of fuel in the horizontal segments",  "All physical quantitites in EJ" )
comments.L1235_grid_elec_demand <- c( "Demand by segment in each grid region",  "All physical quantitites in EJ" )
comments.L1235.elecS_horizontal_vertical <- c( "This table specifies the fraction of supply from horizontal segment available for vertical segment",  "For e.g., 33.33% of base load generation is available for the vertical segment off peak electricity" )
comments.L1235.elecS_horizontal_vertical_GCAM_coeff <- c( "GCAM I-O Coefficients from horizontal to vertical segments" )
comments.L1235.elecS_demand_fraction <- c( "Demand fractions by grid region in long form" )


#write tables as CSV files
writedata(L1235_grid_elec_segments, domain="GCAMUSA_LEVEL1_DATA", fn="L1235.grid_elec_segments", comments=comments.L1235_grid_elec_segments )
writedata(L1235_grid_elec_supply, domain="GCAMUSA_LEVEL1_DATA", fn="L1235.grid_elec_supply", comments=comments.L1235_grid_elec_supply)
writedata(L1235_grid_elec_demand, domain="GCAMUSA_LEVEL1_DATA", fn="L1235.grid_elec_demand", comments=comments.L1235_grid_elec_demand)
writedata(L1235.elecS_horizontal_vertical, domain="GCAMUSA_LEVEL1_DATA", fn="L1235.elecS_horizontal_vertical", comments=comments.L1235.elecS_horizontal_vertical)
writedata(L1235.elecS_horizontal_vertical_GCAM_coeff, domain="GCAMUSA_LEVEL1_DATA", fn="L1235.elecS_horizontal_vertical_GCAM_coeff", comments=comments.L1235.elecS_horizontal_vertical_GCAM_coeff)
writedata(L1235.elecS_demand_fraction, domain="GCAMUSA_LEVEL1_DATA", fn="L1235.elecS_demand_fraction", comments=comments.L1235.elecS_demand_fraction)


# Every script should finish with this line
logstop()
