# This script creates an add-on file to update nuclear assumptions in GCAM-USA. Specifically, we read in state-specific
# s-curve retirement functions and lifetimes for existing nuclear vintage based on data put together by Dr. Son H. Kim. The data reflect planned
# retirements by state and nuclear power plant. Further, the script updates share-weight assumptions for Gen 3 technology.
# We assume that no new nuclear is deployed in any state through 2035, except in GA, based on current understanding. We do not update
# subsector share-weight assumptions in this add-on.
# Currently the script builds this capability only for multiple load segments. This might need to be
# expanded for the single electricity market

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
logstart( "L2244.nuclear_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Updating GCAM-USA nuclear assumptions" )


# Authors: Gokul Iyer
# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

nuc_gen2 <- readdata( "GCAMUSA_LEVEL0_DATA", "nuc_gen2", skip = 1  )

A23.elecS_tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_associations", skip = 1  )
A23.elecS_tech_availability <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_availability", skip = 1  )
 
if(use_mult_load_segments == "TRUE") {
# -----------------------------------------------------------------------------
# 2. Perform computations

gcamusa_regions <- unique(states_subregions$state)

# Function to evaluate the output fraction for a smooth s-curve retirement function as coded in the model.
evaluate_smooth_s_curve <- function(steepness, half.life, t) {
   s_curve_output_fraction <- 1/ (1+ exp(steepness*(t-half.life)))
   return( s_curve_output_fraction )
}

# The function, smooth_s_curve_approx_error, checks for the error between input nuc_gen2 trajectory data 
# and calculated generation using assumed retirement fuction. 
smooth_s_curve_approx_error <- function(nuc_gen2_data, par) {  
  nuc_gen2_data %>%
  filter(time ==0) -> base_year_data
  
  nuc_gen2_data %>%
    mutate(base_year_data = base_year_data$data) %>%
    mutate(evaluated_data = if_else(time==0, base_year_data,(base_year_data * evaluate_smooth_s_curve( par[1], par[2], nuc_gen2_data$time) ))) %>%
    mutate(error = evaluated_data - data) -> nuc_gen2_data
    return( crossprod( nuc_gen2_data$error, nuc_gen2_data$error ) )
}

# Preparing a table by state, year, and desired nuc_gen2 generation in EJ.
nuc_gen2 %>%
  gather(year, gen, -region, -plant, -Units) %>%
  mutate(year = as.integer(substr(year, 2, 5)))  %>%
  filter(year>= final_historical_year) %>%
  mutate(time = year - final_historical_year) %>%
  group_by(region, year, time) %>%
  summarise(gen = sum(gen)) %>%
  ungroup() %>%
  arrange(region, year) -> L2244.nuc_gen2_gen

# Calculating lifetime for gen2 technology by state based on first year of zero generation in each state

L2244.nuc_gen2_gen %>%
  group_by(region) %>%
  mutate( lag_gen = lag(gen)) %>%
  mutate(isfirst_zero = gen==0 & !is.na(lag_gen) & lag_gen != 0) %>%
  filter(isfirst_zero) %>%
  mutate(lifetime = year - final_historical_year) %>%
  select(region, lifetime) -> L2244.nuc_gen2_lifetime

# Initializing variables to loop into states.
states_list <- unique(L2244.nuc_gen2_gen$region)
L2244.nuc_gen2_s_curve_parameters <- tibble()
L2244.nuc_gen2_s_curve_parameters_state <- tibble()

# Looping into states and finding s-curve parameters such that the error between actual and calculated data is minimized
for (L2244.state in states_list) {
  L2244.nuc_gen2_gen %>%
    filter(region == L2244.state ) %>%
    rename(data=gen) -> L2244.nuc_gen2_gen_state
  
  L2244.nuc_gen2_s_curve_parameters_state_optim <-  optim(par= c (0.1,30), f = smooth_s_curve_approx_error, nuc_gen2_data= L2244.nuc_gen2_gen_state )
 
  L2244.nuc_gen2_gen_state %>%
    mutate(steepness = L2244.nuc_gen2_s_curve_parameters_state_optim$par[1]) %>%
    mutate(half.life = L2244.nuc_gen2_s_curve_parameters_state_optim$par[2]) %>%
    filter(row_number() ==1) %>%
    select(region, steepness, half.life) -> L2244.nuc_gen2_s_curve_parameters_state
  
  
  L2244.nuc_gen2_s_curve_parameters %>%
    bind_rows(L2244.nuc_gen2_s_curve_parameters_state) ->  L2244.nuc_gen2_s_curve_parameters

  }
    
# Need to correct for negative coefficients . This is rather arbitrary assumptions for now since VT is the only
# state with negative coefficinets and they retire capacity too soon. The assumed parameters seemed to make the
# most sense. 

L2244.nuc_gen2_s_curve_parameters %>%
  mutate(half.life= if_else(half.life <= 0, 0, half.life)) %>%
  mutate(steepness = if_else(steepness <= 0, 0.6, steepness)) -> L2244.nuc_gen2_s_curve_parameters

# Preparing table to read in s-curve parameters for base-year nuclear gen2 technology. 

printlog( "L2244.StubTechSCurve_elecS_nuc_gen2:  S-curve shutdown decider for historic U.S. nuclear plants" )

L2244.nuc_gen2_s_curve_parameters %>%
  left_join (A23.elecS_tech_associations %>%
               anti_join(A23.elecS_tech_availability, by = c("Electric.sector.technology" = "stub.technology")) %>%
               select(Electric.sector, subsector, Electric.sector.technology) %>%
               filter(subsector == "nuclear", 
                      !grepl("Gen III", Electric.sector.technology)) %>%
               repeat_and_add_vector('region', states_list), by = c("region" )) %>%
  left_join(L2244.nuc_gen2_lifetime, by ="region" ) %>%
  mutate(year = final_model_base_year) %>%
  select(region , supplysector = Electric.sector, subsector, stub.technology = Electric.sector.technology, year,
         lifetime, steepness, half.life) %>%
  filter(!is.na(lifetime))-> L2244.StubTechSCurve_nuc_gen2


# Preparing table to read in 0 shareweights for Gen 3 technologies in all states through 2030

A23.elecS_tech_associations %>%
  anti_join(A23.elecS_tech_availability, by = c("Electric.sector.technology" = "stub.technology")) %>%
  select(Electric.sector, subsector, Electric.sector.technology) %>%
  filter(grepl("Gen III", Electric.sector.technology)) %>%
  repeat_and_add_vector('region', gcamusa_regions) %>%
  repeat_and_add_vector('year', model_future_years) %>%
  filter(year <= 2030) %>%
  mutate(share.weight = 0) %>%
  select(region, supplysector = Electric.sector, subsector, stub.technology = Electric.sector.technology, year, share.weight) -> L2244.StubTechShrwt_nuc_gen3
  
# Adjusting Gen 3 shareweight for GA which which has a plant under construction that's 
# expected to come online in the 2026-2030 time frame   

L2244.StubTechShrwt_nuc_gen3 %>%
  filter(region != "GA") -> L2244.StubTechShrwt_nuc_gen3_allbutGA

L2244.StubTechShrwt_nuc_gen3 %>%
  filter(region == "GA") %>%
  mutate(share.weight = if_else(year == 2030, 1, share.weight)) -> L2244.StubTechShrwt_nuc_gen3_GA


L2244.StubTechShrwt_nuc_gen3_allbutGA %>%
  bind_rows(L2244.StubTechShrwt_nuc_gen3_GA) %>%
  arrange(region, year) -> L2244.StubTechShrwt_nuc_gen3
  
# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2244.StubTechSCurve_nuc_gen2, "StubTechSCurve", "GCAMUSA_LEVEL2_DATA", "L2244.StubTechSCurve_nuc_gen2", "GCAMUSA_XML_BATCH", "batch_nuclear_USA.xml" )
write_mi_data( L2244.StubTechShrwt_nuc_gen3, "StubTechShrwt", "GCAMUSA_LEVEL2_DATA", "L2244.StubTechShrwt_nuc_gen3", "GCAMUSA_XML_BATCH", "batch_nuclear_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_nuclear_USA.xml", "GCAMUSA_XML_FINAL", "nuclear_USA.xml", "", xml_tag="outFile" )

logstop()
} else {
  logstop() # Need to write script to build tables for single electricity market
}
