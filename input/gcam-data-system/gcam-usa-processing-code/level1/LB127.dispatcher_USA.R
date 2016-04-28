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
logstart( "LB127.dispatcher_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Existing coal and gas electric sector dispatch" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
L123.in_EJ_state_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.in_EJ_state_elec_F" )
L123.out_EJ_state_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.out_EJ_state_elec_F" )
L123.eff_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.eff_R_elec_F_Yh" )
L1231.out_EJ_state_elec_F_tech <- readdata("GCAMUSA_LEVEL1_DATA", "L1231.out_EJ_state_elec_F_tech")
EIA_Gen_Capacity_state <- readdata("GCAMUSA_LEVEL0_DATA", "EIA_Gen_Capacity_state")

# -----------------------------------------------------------------------------
# 2. Perform computations

#Calculate capacity factor for each state based on 2010 EIA capacity data
EIA_Gen_Capacity_state %>%
  mutate(capfactor = generation / (capacity * 8760)) -> EIA_Gen_Capacity_state
EIA_Gen_Capacity_state$capfactor[is.nan(EIA_Gen_Capacity_state$capfactor) | 
                                   is.infinite(EIA_Gen_Capacity_state$capfactor)] <- 0

#Calculate NG Ramp potential and coal curtail potential using CFs and generation
NG_Ramp_Potential <- filter(L1231.out_EJ_state_elec_F_tech, technology == "gas (CC)")[c("state", X_historical_years)] %>%
  arrange(state)
dispatchable_gasCT_gen <- filter(L1231.out_EJ_state_elec_F_tech, technology == "gas (steam/CT)") %>%
  arrange(state)
gas_multiplier <- EIA_Gen_Capacity_state %>%
  filter(fuel == "gas") %>% 
  select(region,capfactor) %>%
  mutate(capfactor = ((capfactor * -1) + NG_ramp_CF_agg)/capfactor)
gas_multiplier$capfactor[is.nan(gas_multiplier$capfactor) | is.infinite(gas_multiplier$capfactor)] <- 0

NG_Ramp_Potential[X_historical_years] <- (NG_Ramp_Potential[X_historical_years] + 0.5 * dispatchable_gasCT_gen[
  X_historical_years]) * gas_multiplier$capfactor[match(NG_Ramp_Potential$state,gas_multiplier$region)] 

#Coal Curtail Potential
Coal_Curtail_Potential <- filter(L1231.out_EJ_state_elec_F_tech, fuel == "coal")[c("state", X_historical_years)] %>%
  arrange(state)
coal_multiplier <- EIA_Gen_Capacity_state %>%
  filter(fuel == "coal") %>% 
  select(region,capfactor) %>%
  mutate(capfactor = (capfactor - coal_curtail_CF_agg)/capfactor)
coal_multiplier$capfactor[is.nan(coal_multiplier$capfactor) | is.infinite(coal_multiplier$capfactor)] <- 0

Coal_Curtail_Potential[X_historical_years] <- Coal_Curtail_Potential[X_historical_years] * 
  coal_multiplier$capfactor[match(Coal_Curtail_Potential$state,coal_multiplier$region)]
stopifnot(identical(NG_Ramp_Potential$state,Coal_Curtail_Potential$state))
#The Dispatch Reordering Potential is the minimum of the above two numbers

L127.Dispatch_Reordering_Potential_USA <- NG_Ramp_Potential
L127.Dispatch_Reordering_Potential_USA[X_historical_years] <- pmin(NG_Ramp_Potential[X_historical_years],Coal_Curtail_Potential[X_historical_years])

#Subtract the coal that would be consumed to generate the Dispatch Reordering Potential exajoules from the total coal consumed 
#Use the US coal electricity efficiency
L127.Remaining_Coal_USA <- L127.Dispatch_Reordering_Potential_USA
arrange(L127.Remaining_Coal_USA, state) -> L127.Remaining_Coal_USA
arrange(subset(L123.in_EJ_state_elec_F, fuel == "coal"),state) -> state_coal_use
stopifnot(identical(L127.Remaining_Coal_USA$state, state_coal_use$state))
L127.Remaining_Coal_USA[X_historical_years] <- state_coal_use[X_historical_years] - 
  sweep(L127.Remaining_Coal_USA[X_historical_years], 2, as.numeric(filter(L123.eff_R_elec_F_Yh, fuel == "coal" & 
                                                                  GCAM_region_ID == 1)[X_historical_years]), "/")

# sanity_check1 <- subset(L123.in_EJ_state_elec_F, fuel == "coal")[c("state","X2010")]
# sanity_check1$left <- L127.Remaining_Coal_USA$X2010[match(sanity_check1$state,L127.Remaining_Coal_USA$state)]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L127.Dispatch_Reordering_Potential_USA <- c( "Energy available for dispatch re-ordering by U.S. state / historical year","Unit = EJ" )
comments.L127.Remaining_Coal_USA <- c( "Calibrated input to coal power (non-dispatchable) by U.S. state / historical year","Unit = EJ" )

#write tables as CSV files
writedata( L127.Dispatch_Reordering_Potential_USA, domain="GCAMUSA_LEVEL1_DATA", fn="L127.Dispatch_Reordering_Potential_USA", comments=comments.L127.Dispatch_Reordering_Potential_USA )
writedata( L127.Remaining_Coal_USA, domain="GCAMUSA_LEVEL1_DATA", fn="L127.Remaining_Coal_USA", comments=comments.L127.Remaining_Coal_USA )

# Every script should finish with this line
logstop()
