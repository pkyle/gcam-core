
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
logstart( "LB175.nonghg_indenergy_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))

printlog( "Input emissions for industrial energy use sector by input fuel in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
NEI_pollutant_mapping <- readdata( "GCAMUSA_MAPPINGS", "NEI_pollutant_mapping" )
SEDS_GCAM_fuel <- readdata( "GCAMUSA_MAPPINGS", "SEDS_GCAM_fuel" )
NEI_2011_GCAM_sectors <- readdata( "GCAMUSA_LEVEL0_DATA", "NEI_2011_GCAM_sectors" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#This script assumes the data has been pre-processed. So all that needs to be done is
#to convert to the correct sector/fuel organization and convert units
###NOTE: what is SEDS fuel "Process?"
printlog( "L175.nonghg_tg_state_indenergy_F_Yb: NEI emissions by state & industrial fuel used in the base year" )
L175.nonghg_tg_state_indenergy_F_Yb <- NEI_2011_GCAM_sectors %>%
  #Subset industrial energy use emissions
  filter(GCAM_sector == "industry_energy") %>%
  #Match on GCAM fuel 
  mutate(fuel = SEDS_GCAM_fuel$GCAM_fuel[match(SEDS_Fuel,SEDS_GCAM_fuel$SEDS_Fuel)]) %>%
  ###MISSING VALUES: SEDS fuel "Process." Unknown for now so omit
  na.omit() %>%
  #Match on GCAM pollutant
  mutate(Non.CO2 = NEI_pollutant_mapping$Non.CO2[match(pollutant,NEI_pollutant_mapping$NEI_pollutant)]) %>%
  ###MISSING VALUES: PM filterable and CO2. OK to omit
  na.omit() %>%
  #Convert from short ton to Tg
  mutate(emissions = emissions / conv_t_metric_short / 10^6, unit = "Tg") %>%
  #Organize
  rename(sector = GCAM_sector) %>%
  group_by(state,sector,fuel,Non.CO2) %>%
  summarise(emissions = sum(emissions)) 

colnames(L175.nonghg_tg_state_indenergy_F_Yb) <- c("state",S_F,"Non.CO2",X_final_historical_year)

stopifnot(!any(is.na(L175.nonghg_tg_state_indenergy_F_Yb)))
# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L175.nonghg_tg_state_indenergy_F_Yb <- c( "Industrial energy use sector non-ghg input emission factor by U.S. state / sector / fuel / pollutant / year","Unit = Tg" )

#write tables as CSV files
writedata( L175.nonghg_tg_state_indenergy_F_Yb, domain="GCAMUSA_LEVEL1_DATA", fn="L175.nonghg_tg_state_indenergy_F_Yb", comments=comments.L175.nonghg_tg_state_indenergy_F_Yb )

# Every script should finish with this line
logstop()
