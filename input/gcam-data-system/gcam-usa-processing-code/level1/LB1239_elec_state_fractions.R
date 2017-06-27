# This script takes in generation by fuel by grid region by horizontal segment and 
# calculates the generation by fuel by state by segment.
# The fraction of generation by fuel by horizontal segment is assumed to be equal for all states within a grid region. 

# Universal header file - provides logging, file support, etc.
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
logstart( "LB1239_elec_state_fractions.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Generation by fuel by state and segment" )

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


printlog( "Generation by fuel by state by horizontal segment" )



# -----------------------------------------------------------------------------
#1. Inputs. 

L123_out_EJ_state_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.out_EJ_state_elec_F" )
#This table has electricity generation output by fuel by state

L1238_grid_elec_supply <- readdata( "GCAMUSA_LEVEL1_DATA", "L1238.grid_elec_supply" )
# -----------------------------------------------------------------------------
# Perform Computations

# Initialize varables
L1239_grid_elec_supply <- L1238_grid_elec_supply

#Initialize state electricity table
L123_out_EJ_state_elec_F %>%
  mutate(fuel = sub("solar CSP", "solar", fuel)) %>%
  mutate(fuel = sub("solar PV", "solar", fuel)) %>%
  group_by(state, sector, fuel) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  gather(year, generation, -state, -fuel, -sector) %>%
  mutate(year = gsub("X","",year)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% model_base_years) %>%
  rename(tot_generation = generation) -> L1239_out_EJ_state_elec_F

L1239_out_EJ_state_elec_F %>%
  left_join(states_subregions, by = "state") %>%
  select(state, grid_region, sector, fuel, year, tot_generation) %>%
  left_join(L1239_grid_elec_supply, by = c("grid_region", "fuel", "year")) %>%
  select(state, grid_region, segment, fuel, year, tot_generation = tot_generation.x, fraction) %>%
  mutate(generation = tot_generation * fraction) -> L1239_state_elec_supply
  
# -----------------------------------------------------------------------------
# 3. Output
comments.L1239_state_elec_supply <- c( "Supply by fuel by horizontal segment in each state based on calculated fraction of fuel in the horizontal segments",  "All physical quantitites in EJ" )

writedata(L1239_state_elec_supply, domain="GCAMUSA_LEVEL1_DATA", fn="L1239.state_elec_supply", comments=comments.L1239_state_elec_supply)

# Every script should finish with this line
logstop()

