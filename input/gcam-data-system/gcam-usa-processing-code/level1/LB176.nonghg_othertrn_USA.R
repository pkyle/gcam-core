
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
logstart( "LB176.nonghg_othertrn_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))

printlog( "Input emissions for rail, shipping, and aviation by input fuel in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ENERGY_data", extension = ".R" )
NEI_pollutant_mapping <- readdata( "GCAMUSA_MAPPINGS", "NEI_pollutant_mapping" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
SEDS_GCAMtrantech <- readdata( "GCAMUSA_MAPPINGS", "SEDS_GCAMtrantech" )
NEI_2011_GCAM_sectors <- readdata( "GCAMUSA_LEVEL0_DATA", "NEI_2011_GCAM_sectors" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#This script assumes the data has been pre-processed. So all that needs to be done is
#to convert to the correct sector/fuel organization and convert units
printlog( "L176.nonghg_tg_state_othertrn_F_Yb: base-year input emissions for domestic aviation, ships, and rail transport by fuel" )
L176.nonghg_tg_state_othertrn_F_Yb <- NEI_2011_GCAM_sectors %>%
  #Subset relevant emissions
  filter(GCAM_sector %in% c("trn_domestic ship", "trn_domestic air","trn_rail")) %>%
  #Match on GCAM transportation technology 
  mutate(stub.technology = SEDS_GCAMtrantech$stub.technology[match(SEDS_Fuel,SEDS_GCAMtrantech$SEDS_Fuel)]) %>%
  #Match on GCAM pollutant
  mutate(Non.CO2 = NEI_pollutant_mapping$Non.CO2[match(pollutant,NEI_pollutant_mapping$NEI_pollutant)]) %>%
  ###MISSING VALUES: CO2 and PM filterable. Not needed bc have filt+cond. OK to omit
  na.omit() %>%
  #Convert from short ton to Tg
  mutate(emissions = emissions / conv_t_metric_short / 10^6, unit = "Tg") %>%
  #Organize 
  rename(sector = GCAM_sector) %>%
  group_by(state,sector,stub.technology,Non.CO2) %>%
  summarise(emissions = sum(emissions)) 

colnames(L176.nonghg_tg_state_othertrn_F_Yb) <- c("state","sector","stub.technology","Non.CO2",X_final_historical_year)

stopifnot(!any(is.na(L176.nonghg_tg_state_othertrn_F_Yb)))
# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L176.nonghg_tg_state_othertrn_F_Yb <- c( "Domestic aviation & ship & rail non-ghg input emission by U.S. state / fuel / pollutant / year","Unit = Tg" )

#write tables as CSV files
writedata( L176.nonghg_tg_state_othertrn_F_Yb, domain="GCAMUSA_LEVEL1_DATA", fn="L176.nonghg_tg_state_othertrn_F_Yb", comments=comments.L176.nonghg_tg_state_othertrn_F_Yb )

# Every script should finish with this line
logstop()
