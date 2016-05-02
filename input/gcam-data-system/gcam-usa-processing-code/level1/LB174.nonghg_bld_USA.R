
# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "GCAMUSAPROC_DIR" ) ){
  if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
    GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
  } else {
    stop("Could not determine location of usa data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
  }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "LB174.nonghg_bld_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))

printlog( "Input emissions for buildings sectors by input fuel in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
SCC_elc_fuels_mapping <- readdata( "GCAMUSA_MAPPINGS", "SCC_elc_fuels_mapping" )
NEI_pollutant_mapping <- readdata( "GCAMUSA_MAPPINGS", "NEI_pollutant_mapping" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
SEDS_GCAM_fuel <- readdata( "GCAMUSA_MAPPINGS", "SEDS_GCAM_fuel" )
NEI_2011_GCAM_sectors <- readdata( "GCAMUSA_LEVEL0_DATA", "NEI_2011_GCAM_sectors" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#This script assumes the data has been pre-processed. So all that needs to be done is
#to convert to the correct sector/fuel organization and convert units
###NOTE: what is SEDS fuel "Process?"
L174.nonghg_tg_state_bld_F_Yb <- NEI_2011_GCAM_sectors %>%
  #Subset building emissions
  filter(grepl("building",GCAM_sector)) %>%
  #Format to level1 sector naming convention
  mutate(sector = gsub("building_","",GCAM_sector)) %>%
  #Match on GCAM fuel 
  mutate(fuel = SEDS_GCAM_fuel$GCAM_fuel[match(SEDS_Fuel,SEDS_GCAM_fuel$SEDS_Fuel)]) %>%
  ###MISSING VALUES: SEDS fuel "Process." Unknown for now so omit
  na.omit() %>%
  #Match on GCAM pollutant
  mutate(Non.CO2 = NEI_pollutant_mapping$Non.CO2[match(pollutant,NEI_pollutant_mapping$NEI_pollutant)]) %>%
  ###MISSING VALUES: PM filterable. Not needed bc have filt+cond. OK to omit
  na.omit() %>%
  #Convert from short ton to Tg
  mutate(emissions = emissions / conv_t_metric_short / 10^6, unit = "Tg") %>%
  #Organize 
  group_by(state,sector,fuel,Non.CO2) %>%
  summarise(emissions = sum(emissions)) 

colnames(L174.nonghg_tg_state_bld_F_Yb) <- c("state",S_F,"Non.CO2",X_final_historical_year)

stopifnot(!any(is.na(L174.nonghg_tg_state_bld_F_Yb)))
# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L174.nonghg_tg_state_bld_F_Yb <- c( "Buildings sector non-ghg input emissions by U.S. state / sector / fuel / pollutant / year","Unit = Tg" )

#write tables as CSV files
writedata( L174.nonghg_tg_state_bld_F_Yb, domain="GCAMUSA_LEVEL1_DATA", fn="L174.nonghg_tg_state_bld_F_Yb", comments=comments.L174.nonghg_tg_state_bld_F_Yb )

# Every script should finish with this line
logstop()
