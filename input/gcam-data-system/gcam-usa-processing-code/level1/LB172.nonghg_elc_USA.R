
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
logstart( "LB172.nonghg_elc_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))

printlog( "Emissions factors for electricity by input fuel in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
SEDS_GCAM_fuel <- readdata( "GCAMUSA_MAPPINGS", "SEDS_GCAM_fuel" )
NEI_pollutant_mapping <- readdata( "GCAMUSA_MAPPINGS", "NEI_pollutant_mapping" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
EPA_NEI_2011_elc_nonghg <- readdata( "GCAMUSA_LEVEL0_DATA", "EPA_NEI_2011_elc_nonghg" )
EPA_state_egu_emission_factors_ktPJ <- readdata( "GCAMUSA_LEVEL0_DATA", "EPA_state_egu_emission_factors_ktPJ" )
BC_OC_assumptions <- readdata( "GCAMUSA_LEVEL0_DATA", "BC_OC_assumptions" )
L123.in_EJ_state_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.in_EJ_state_elec_F" )
L123.out_EJ_state_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.out_EJ_state_elec_F" )
NEI_2011_GCAM_sectors <- readdata( "GCAMUSA_LEVEL0_DATA", "NEI_2011_GCAM_sectors" )
# -----------------------------------------------------------------------------
# 2. Perform computations
###NOTE: What is SCC Level Three category "Flares" ?
###NOTE: What to do for Process Gas, Landfill Gas, Solid/Liquid Waste SCC categories?

# 2a. Output emissions 
printlog( "Electricity output emissions based on electricity output by U.S. state" )
#Geothermal emissions will be defined based on electricity output
#Get the geothermal emissions data, aggregate, label pollutants, and convert from short ton to Tg
NEI_geothermal_emissions <- filter(EPA_NEI_2011_elc_nonghg, grepl("Geothermal",fuel)) %>%
  mutate(fuel = "geothermal") %>%
  mutate(Non.CO2 = NEI_pollutant_mapping$Non.CO2[match(pollutant,NEI_pollutant_mapping$NEI_pollutant)],
         emissions = emissions / conv_t_metric_short * 10^-6) %>%
  mutate(sector = "electricity") %>%
  group_by(state, sector,fuel,Non.CO2) %>%
  summarise(emissions = sum(emissions))

###NOTE: save geothermal problem for later
# 2b. Input emissions in the base year
printlog( "L172.nonghg_tg_state_elc_F_Yb: Electricity non-ghg input emissions by fuel and U.S. state in the final base year" )

L172.nonghg_tg_state_elec_F_Yb <- NEI_2011_GCAM_sectors %>%
  #Subset electricity emissions
  filter(GCAM_sector == "elec_heat") %>%
  #Match on GCAM fuel
  mutate(fuel = SEDS_GCAM_fuel$GCAM_fuel[match(SEDS_Fuel,SEDS_GCAM_fuel$SEDS_Fuel)]) %>%
  #Match on NEI pollutants
  mutate(Non.CO2 = NEI_pollutant_mapping$Non.CO2[match(pollutant,NEI_pollutant_mapping$NEI_pollutant)]) %>%
  ###MISSING VALUES: PM filterable. Not needed bc have filt+cond. OK to omit
  na.omit() %>%
  #Convert from short ton to Tg
  mutate(emissions = emissions / conv_t_metric_short / 10^6, unit = "Tg") %>%
  #Organize
  rename(sector = GCAM_sector) %>%
  group_by(state,sector,fuel,Non.CO2) %>%
  summarise(emissions = sum(emissions)) 

colnames(L172.nonghg_tg_state_elec_F_Yb) <- c("state",S_F,"Non.CO2",X_final_historical_year)

# 2c. For future years, use data from EPA-ORD
printlog( "L172.nonghg_tgej_state_elec_F_Yf: Electricity non-co2 emissions coefficients by fuel input and U.S. state in future model years" )
L172.nonghg_tgej_state_elec_F_Yf <- EPA_state_egu_emission_factors_ktPJ %>%
  #Convert to long format
  gather(variable,value,-state_name,-fuel) %>%
  separate(variable,into=c("year","Non.CO2"),sep="_") %>%
  ###NOTE: for now change oil to refined liquids
  mutate(fuel = gsub("oil","refined liquids",fuel)) %>%
  #Match in state code & select relevant columns
  mutate(state = states_subregions$state[match(state_name,states_subregions$state_name)]) %>%
  mutate(sector = "elec_heat") %>%
  select(state, sector,fuel, Non.CO2, year, value) %>%
  spread(year,value)

#Add remaining future years, keeping them constant at 2025 levels
X_remaining_years <- X_future_years[X_future_years %!in% names(L172.nonghg_tgej_state_elec_F_Yf)]
L172.nonghg_tgej_state_elec_F_Yf[X_remaining_years] <- L172.nonghg_tgej_state_elec_F_Yf$X2025

#Select relevant columns
L172.nonghg_tgej_state_elec_F_Yf <- L172.nonghg_tgej_state_elec_F_Yf[c("state","sector","fuel","Non.CO2",X_future_years)]

###MISSING VALUES: omit for now
L172.nonghg_tgej_state_elec_F_Yf %>% na.omit() -> L172.nonghg_tgej_state_elec_F_Yf

stopifnot(!any(is.na(L172.nonghg_tg_state_elec_F_Yb)))
stopifnot(!any(is.na(L172.nonghg_tgej_state_elec_F_Yf)))

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L172.nonghg_tg_state_elec_F_Yb <- c( "Base-year electricity non-ghg input emissions by U.S. state / fuel / pollutant / year","Unit = Tg" )
comments.L172.nonghg_tgej_state_elec_F_Yf <- c( "Future year electricity non-co2 input emissions coefficients by U.S. state / fuel / pollutant / year","Unit = Tg/EJ" )

#write tables as CSV files
writedata( L172.nonghg_tg_state_elec_F_Yb, domain="GCAMUSA_LEVEL1_DATA", fn="L172.nonghg_tg_state_elec_F_Yb", comments=comments.L172.nonghg_tg_state_elec_F_Yb )
writedata( L172.nonghg_tgej_state_elec_F_Yf, domain="GCAMUSA_LEVEL1_DATA", fn="L172.nonghg_tgej_state_elec_F_Yf", comments=comments.L172.nonghg_tgej_state_elec_F_Yf )

# Every script should finish with this line
logstop()
