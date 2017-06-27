#This script takes in input by fuel and output by fuel by state and 
#aggregates them into input by fuel and output by fuel by grid region.
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
logstart( "LB1234_elec_gridregions.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))

printlog( "Input and Output by fuel by grid region" )
# -----------------------------------------------------------------------------
#1. Inputs. 

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

L123_in_EJ_state_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA","L123.in_EJ_state_elec_F" )
#This table has fuel input by fuel by state

L123_out_EJ_state_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA","L123.out_EJ_state_elec_F" )
#This table has electricity generation output by fuel by state
  
# -----------------------------------------------------------------------------
# 2. Perform computations
# aggregate input tables

L123_in_EJ_state_elec_F %>%
  gather(year, fuel.input, -state, -fuel, -sector) %>%
  left_join(states_subregions, by ="state") %>%
  select(state, grid_region, sector, fuel, year, fuel.input) %>%
  group_by(grid_region, sector, fuel, year) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  mutate(year = gsub("X","",year)) ->  L1234_in_EJ_grid_elec_F

# aggregate output tables
L123_out_EJ_state_elec_F %>%
  gather(year, generation, -state, -fuel, -sector) %>%
  left_join(states_subregions, by ="state") %>%
  select(state, grid_region, sector, fuel, year, generation) %>%
  group_by(grid_region, sector, fuel, year) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  mutate(year = gsub("X","",year)) ->  L1234_out_EJ_grid_elec_F  
  
# -----------------------------------------------------------------------------
# 3. Output

comments.L1234_in_EJ_grid_elec_F <- c( "Fuel input into electricity by fuel and grid region",  "Units = EJ" )
comments.L1234_out_EJ_grid_elec_F <- c( "Electricity generation by fuel and grid region",  "Units = EJ" )

#write tables as CSV files
writedata( L1234_in_EJ_grid_elec_F, domain="GCAMUSA_LEVEL1_DATA", fn="L1234.in_EJ_grid_elec_F", comments=comments.L1234_in_EJ_grid_elec_F )
writedata( L1234_out_EJ_grid_elec_F, domain="GCAMUSA_LEVEL1_DATA", fn="L1234.out_EJ_grid_elec_F", comments=comments.L1234_out_EJ_grid_elec_F )

# Every script should finish with this line
logstop()
