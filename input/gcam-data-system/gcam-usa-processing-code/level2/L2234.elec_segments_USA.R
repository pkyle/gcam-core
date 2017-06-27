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
logstart( "L2234.elec_segments_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Creating detailed electric sector sectors in the states and grid regions" )

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

A23.elec_delete <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elec_delete" )
A23.elecS_sector <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_sector" )
A23.elecS_sector_vertical <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_sector_vertical" )
A23.elecS_metainfo <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elecS_metainfo" )
A23.elecS_metainfo_vertical <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elecS_metainfo_vertical" )
A23.elecS_subsector_logit <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_subsector_logit" )
A23.elecS_subsector_shrwt <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_subsector_shrwt" )
A23.elecS_subsector_shrwt_interp <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_subsector_shrwt_interp" )
A23.elecS_subsector_shrwt_interpto <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_subsector_shrwt_interpto" )
A23.elecS_globaltech_shrwt <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_globaltech_shrwt")
A23.elecS_globalinttech_shrwt <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_globalinttech_shrwt")
A23.elecS_tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_associations" )
A23.elecS_inttech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_inttech_associations" )
A23.elecS_tech_availability <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_availability" )
A23.elecS_globaltech_non_energy_inputs <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_globaltech_non_energy_inputs" )
A23.elecS_stubtech_energy_inputs <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_stubtech_energy_inputs" )

NREL_us_re_technical_potential <- readdata( "GCAMUSA_LEVEL0_DATA", "NREL_us_re_technical_potential" ) 

L1239_state_elec_supply <- readdata( "GCAMUSA_LEVEL1_DATA", "L1239.state_elec_supply" )

L223.StubTechEff_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechEff_elec_USA", skip = 4 )
L223.StubTechMarket_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechMarket_elec_USA", skip = 4 )
L223.StubTechProd_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechProd_elec_USA", skip = 4 )
L223.StubTechFixOut_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechFixOut_elec_USA", skip = 4 )
L223.StubTechFixOut_hydro_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechFixOut_hydro_USA", skip = 4 )
L223.StubTechMarket_backup_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechMarket_backup_USA", skip = 4 )
L223.StubTechCapFactor_elec_wind_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechCapFactor_elec_wind_USA", skip = 4 )
L223.StubTechCapFactor_elec_solar_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechCapFactor_elec_solar_USA", skip = 4 )

L223.GlobalTechEff_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechEff_elec", skip = 4 )
L223.GlobalIntTechEff_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechEff_elec", skip = 4 )
L223.GlobalTechCapital_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_elec", skip = 4 )
L223.GlobalIntTechCapital_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_elec", skip = 4 )
L223.GlobalTechLifetime_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechLifetime_elec", skip = 4 )
L223.GlobalIntTechLifetime_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechLifetime_elec", skip = 4 )
L223.GlobalTechOMfixed_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMfixed_elec", skip = 4 )
L223.GlobalIntTechOMfixed_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMfixed_elec", skip = 4 )
L223.GlobalTechOMvar_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMvar_elec", skip = 4 )
L223.GlobalIntTechOMvar_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMvar_elec", skip = 4 )
L223.GlobalTechSCurve_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechSCurve_elec", skip = 4 )
L223.GlobalTechProfitShutdown_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechProfitShutdown_elec", skip = 4 )
L223.GlobalTechCapture_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapture_elec", skip = 4 )
L223.GlobalIntTechBackup_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechBackup_elec", skip = 4 )


# -----------------------------------------------------------------------------
# Function to write to all grid regions similar to the "write_to_all_states" function. Might want to move this to 
# the header script.

write_to_all_grid_regions <- function( data, names ){
  grid_regions <- sort( unique( states_subregions$grid_region ) )
  if ( "logit.year.fillout" %in% names ) data$logit.year.fillout <- "start-year"
  if ( "price.exp.year.fillout" %in% names ) data$price.exp.year.fillout <- "start-year"
  data_new <- set_years( data )
  data_new <- repeat_and_add_vector( data_new, "region", grid_regions)
  return( data_new[ names ] ) 
}


# 2. Build state-level tables for CSVs
# 2a. Supplysector information
printlog( "L2234.elecS_supplysector_USA and L2234.elecS_reserve_USA: Supply sector information for detailed electric sectors" )

# Delete old electricity sector in the states 
L2234.elec_delete <- write_to_all_states(A23.elec_delete, c("region","supplysector"))

# Create horizontal generation supplysectors
L2234.Supplysector_elecS_USA <- write_to_all_states( A23.elecS_sector, c( names_Supplysector, "logit.type" ) )

L2234.ElecReserve_elecS_USA <- write_to_all_states( A23.elecS_metainfo, c("region", "supplysector","electricity.reserve.margin", "average.grid.capacity.factor") )

# 2b. Subsector information
printlog( "L2234.elecS_subsectorLogit_USA and L2234.elecS_subsectorshrwt_interp_USA: Subsector characteristics of detailed electric sector" )
L2234.SubsectorLogit_elecS_USA <- write_to_all_states( A23.elecS_subsector_logit, c( names_SubsectorLogit, "logit.type" ) )

 # Note that the subsector share-weights are updated after processing calibration year output values.
# L2234.SubsectorShrwtFllt_elecS_USA <- write_to_all_states( A23.elecS_subsector_shrwtfllt, c("region", "supplysector","subsector","year.fillout","share.weight") )

L2234.SubsectorShrwt_elecS_USA <- write_to_all_states( A23.elecS_subsector_shrwt, c("region", "supplysector","subsector","year", "share.weight") )
L2234.SubsectorShrwtInterp_elecS_USA <- write_to_all_states( A23.elecS_subsector_shrwt_interp, c("region", "supplysector","subsector","apply.to", "from.year","to.year", "interpolation.function") )
L2234.SubsectorShrwtInterpTo_elecS_USA <- write_to_all_states( A23.elecS_subsector_shrwt_interpto, c("region", "supplysector","subsector","apply.to", "from.year","to.year","to.value", "interpolation.function") )

 #Filter out hydro since it is dealt with separately in the fixed output tables
# L2234.SubsectorShrwtFllt_elecS_USA %>%
#   filter(subsector != "hydro") -> L2234.SubsectorShrwtFllt_elecS_USA

L2234.SubsectorShrwt_elecS_USA %>%
  filter(subsector != "hydro") -> L2234.SubsectorShrwt_elecS_USA

L2234.SubsectorShrwtInterp_elecS_USA %>%
  filter(subsector != "hydro") -> L2234.SubsectorShrwtInterp_elecS_USA
  
L2234.SubsectorShrwtInterpTo_elecS_USA %>%
  filter(subsector != "hydro") -> L2234.SubsectorShrwtInterpTo_elecS_USA

# 2c. Technology information
#Shareweights 
printlog( "L2234.GlobalTechShrwt_elecS and L2234.GlobalInttechShrwt_elecS: Shareweights for detailed elecric sector technologies" )

L2234.globaltech_shrwt.melt <- interpolate_and_melt(
  A23.elecS_globaltech_shrwt[ complete.cases( A23.elecS_globaltech_shrwt ), ], model_years, value.name="share.weight" )
L2234.globaltech_shrwt.melt.NAs <- melt( A23.elecS_globaltech_shrwt[ !complete.cases( A23.elecS_globaltech_shrwt ), ],
                                        measure.vars = grep( "X[0-9]{4}", names( A23.elecS_globaltech_shrwt ) ),
                                        value.name = "share.weight" ) %>%
  na.omit() %>%
  mutate(year = as.numeric(substr( variable, 2, 5 )))

L2234.globaltech_shrwt.melt <- rbind( L2234.globaltech_shrwt.melt, L2234.globaltech_shrwt.melt.NAs ) %>%
  rename(sector.name=supplysector,subsector.name=subsector)

L2234.GlobalTechShrwt_elecS <- L2234.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]
stopifnot(!any(is.na(L2234.GlobalTechShrwt_elecS)))

L2234.globalinttech_shrwt.melt <- interpolate_and_melt(
  A23.elecS_globalinttech_shrwt[ complete.cases( A23.elecS_globalinttech_shrwt ), ], model_years, value.name="share.weight" )
L2234.globalinttech_shrwt.melt.NAs <- melt( A23.elecS_globalinttech_shrwt[ !complete.cases( A23.elecS_globalinttech_shrwt ), ],
                                         measure.vars = grep( "X[0-9]{4}", names( A23.elecS_globalinttech_shrwt ) ),
                                         value.name = "share.weight" ) %>%
  na.omit() %>%
  mutate(year = as.numeric(substr( variable, 2, 5 )))

L2234.globalinttech_shrwt.melt <- rbind( L2234.globalinttech_shrwt.melt, L2234.globalinttech_shrwt.melt.NAs ) %>%
  rename(sector.name=supplysector,subsector.name=subsector)

L2234.GlobalIntTechShrwt_elecS <- L2234.globalinttech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]
L2234.GlobalIntTechShrwt_elecS %>%
  rename(intermittent.technology = technology) -> L2234.GlobalIntTechShrwt_elecS

stopifnot(!any(is.na(L2234.GlobalTechShrwt_elecS)))


#Capital Costs of detailed electric sector technologies

printlog( "L2234.GlobalTechCapital_elecS: Capital costs of electricity generation technologies" )

L2234.GlobalTechCapital_elecS <- A23.elecS_tech_associations
L2234.GlobalTechCapital_elecS <- L2234.GlobalTechCapital_elecS[rep(1:nrow(L2234.GlobalTechCapital_elecS), 
                                                                   times = length(unique(L223.GlobalTechCapital_elec$year))),]
L2234.GlobalTechCapital_elecS$year <- rep(unique(L223.GlobalTechCapital_elec$year),each = nrow(A23.elecS_tech_associations))
L2234.GlobalTechCapital_elecS %>% 
  left_join(L223.GlobalTechCapital_elec, by= c("technology","year")) %>%
  filter(capital.overnight != "NA") %>%
  select(-supplysector, -subsector.1, -technology, -sector.name, -subsector.name) -> L2234.GlobalTechCapital_elecS
names(L2234.GlobalTechCapital_elecS) <- c("supplysector", "subsector", "technology", "year", "input.capital", "capital.overnight", "fixed.charge.rate","capacity.factor")

# Read in lower capacity factors for non-baseload technologies. The fracctions are rather arbitrary and based on assumptions
# made in previous versions of the multiple load segment model (GCAM 3.2).

for( i in 1: nrow(L2234.GlobalTechCapital_elecS)) {
  if (L2234.GlobalTechCapital_elecS$supplysector[[i]] == "intermediate generation") {
    L2234.GlobalTechCapital_elecS$capacity.factor[[i]] = 0.627 * L2234.GlobalTechCapital_elecS$capacity.factor[[i]]
  }
  if (L2234.GlobalTechCapital_elecS$supplysector[[i]] == "subpeak generation") {
    L2234.GlobalTechCapital_elecS$capacity.factor[[i]] = 0.301 * L2234.GlobalTechCapital_elecS$capacity.factor[[i]]
  }
  if (L2234.GlobalTechCapital_elecS$supplysector[[i]] == "peak generation") {
    L2234.GlobalTechCapital_elecS$capacity.factor[[i]] = 0.072 * L2234.GlobalTechCapital_elecS$capacity.factor[[i]]
  }
  
}


printlog( "L2234.GlobalIntTechCapital_elecS: Capital costs of intermittent electricity generation technologies" )

L2234.GlobalIntTechCapital_elecS <- A23.elecS_inttech_associations
L2234.GlobalIntTechCapital_elecS <- L2234.GlobalIntTechCapital_elecS[rep(1:nrow(L2234.GlobalIntTechCapital_elecS), 
                                                                   times = length(unique(L223.GlobalIntTechCapital_elec$year))),]
L2234.GlobalIntTechCapital_elecS$year <- rep(unique(L223.GlobalIntTechCapital_elec$year),each = nrow(A23.elecS_inttech_associations))
L2234.GlobalIntTechCapital_elecS <- left_join(L2234.GlobalIntTechCapital_elecS,L223.GlobalIntTechCapital_elec, by= c("intermittent.technology","year"))
L2234.GlobalIntTechCapital_elecS <- select(L2234.GlobalIntTechCapital_elecS, -supplysector, -subsector.1, -intermittent.technology, -sector.name, -subsector.name)
L2234.GlobalIntTechCapital_elecS <- filter(L2234.GlobalIntTechCapital_elecS, capital.overnight != "NA")
names(L2234.GlobalIntTechCapital_elecS) <- c("supplysector", "subsector", "intermittent.technology", "year", "input.capital", "capital.overnight", "fixed.charge.rate","capacity.factor")


#O&M Costs of detailed electric sector technologies

printlog( "L2234.GlobalTechOMfixed_elecS: Fixed OM costs of electricity generation technologies" )

L2234.GlobalTechOMfixed_elecS <- A23.elecS_tech_associations
L2234.GlobalTechOMfixed_elecS <- L2234.GlobalTechOMfixed_elecS[rep(1:nrow(L2234.GlobalTechOMfixed_elecS), 
                                                                   times = length(unique(L223.GlobalTechOMfixed_elec$year))),]
L2234.GlobalTechOMfixed_elecS$year <- rep(unique(L223.GlobalTechOMfixed_elec$year),each = nrow(A23.elecS_tech_associations))
L2234.GlobalTechOMfixed_elecS %>%
  left_join(L223.GlobalTechOMfixed_elec, by= c("technology","year")) %>%
  filter(OM.fixed != "NA") %>%
  select(-supplysector, -subsector.1, -technology, -sector.name, -subsector.name) -> L2234.GlobalTechOMfixed_elecS 
names(L2234.GlobalTechOMfixed_elecS) <- c("supplysector", "subsector", "technology", "year", "input.OM.fixed", "OM.fixed", "capacity.factor")

# Read in lower capacity factors for non-baseload technologies. The fracctions are rather arbitrary and based on assumptions
# made in previous versions of the multiple load segment model (GCAM 3.2).

for( i in 1: nrow(L2234.GlobalTechOMfixed_elecS)) {
  if (L2234.GlobalTechOMfixed_elecS$supplysector[[i]] == "intermediate generation") {
    L2234.GlobalTechOMfixed_elecS$capacity.factor[[i]] = 0.627 * L2234.GlobalTechOMfixed_elecS$capacity.factor[[i]]
  }
  if (L2234.GlobalTechOMfixed_elecS$supplysector[[i]] == "subpeak generation") {
    L2234.GlobalTechOMfixed_elecS$capacity.factor[[i]] = 0.301 * L2234.GlobalTechOMfixed_elecS$capacity.factor[[i]]
  }
  if (L2234.GlobalTechOMfixed_elecS$supplysector[[i]] == "peak generation") {
    L2234.GlobalTechOMfixed_elecS$capacity.factor[[i]] = 0.072 * L2234.GlobalTechOMfixed_elecS$capacity.factor[[i]]
  }
  
}

printlog( "L2234.GlobalIntTechOMfixed_elecS: Fixed OM costs of intermittent electricity generation technologies" )

L2234.GlobalIntTechOMfixed_elecS <- A23.elecS_inttech_associations
L2234.GlobalIntTechOMfixed_elecS <- L2234.GlobalIntTechOMfixed_elecS[rep(1:nrow(L2234.GlobalIntTechOMfixed_elecS), 
                                                                   times = length(unique(L223.GlobalIntTechOMfixed_elec$year))),]
L2234.GlobalIntTechOMfixed_elecS$year <- rep(unique(L223.GlobalIntTechOMfixed_elec$year),each = nrow(A23.elecS_inttech_associations))
L2234.GlobalIntTechOMfixed_elecS <- left_join(L2234.GlobalIntTechOMfixed_elecS,L223.GlobalIntTechOMfixed_elec, by= c("intermittent.technology","year"))
L2234.GlobalIntTechOMfixed_elecS <- select(L2234.GlobalIntTechOMfixed_elecS, -supplysector, -subsector.1, -intermittent.technology, -sector.name, -subsector.name)
L2234.GlobalIntTechOMfixed_elecS <- filter(L2234.GlobalIntTechOMfixed_elecS, input.OM.fixed != "NA")
names(L2234.GlobalIntTechOMfixed_elecS) <- c("supplysector", "subsector", "intermittent.technology", "year", "input.OM.fixed", "OM.fixed", "capacity.factor")


printlog( "L2234.GlobalTechOMvar_elecS: Variable OM costs of electricity generation technologies" )

L2234.GlobalTechOMvar_elecS <- A23.elecS_tech_associations
L2234.GlobalTechOMvar_elecS <- L2234.GlobalTechOMvar_elecS[rep(1:nrow(L2234.GlobalTechOMvar_elecS), 
                                                                   times = length(unique(L223.GlobalTechOMvar_elec$year))),]
L2234.GlobalTechOMvar_elecS$year <- rep(unique(L223.GlobalTechOMvar_elec$year),each = nrow(A23.elecS_tech_associations))
L2234.GlobalTechOMvar_elecS %>% 
  left_join(L223.GlobalTechOMvar_elec, by= c("technology","year")) %>% 
  filter(OM.var != "NA") %>%
  select(-supplysector, -subsector.1, -technology, -sector.name, -subsector.name) -> L2234.GlobalTechOMvar_elecS
names(L2234.GlobalTechOMvar_elecS) <- c("supplysector", "subsector", "technology", "year", "input.OM.var", "OM.var")

printlog( "L2234.GlobalIntTechOMvar_elecS: Var OM costs of intermittent electricity generation technologies" )

L2234.GlobalIntTechOMvar_elecS <- A23.elecS_inttech_associations
L2234.GlobalIntTechOMvar_elecS <- L2234.GlobalIntTechOMvar_elecS[rep(1:nrow(L2234.GlobalIntTechOMvar_elecS), 
                                                                         times = length(unique(L223.GlobalIntTechOMvar_elec$year))),]
L2234.GlobalIntTechOMvar_elecS$year <- rep(unique(L223.GlobalIntTechOMvar_elec$year),each = nrow(A23.elecS_inttech_associations))
L2234.GlobalIntTechOMvar_elecS <- left_join(L2234.GlobalIntTechOMvar_elecS,L223.GlobalIntTechOMvar_elec, by= c("intermittent.technology","year"))
L2234.GlobalIntTechOMvar_elecS <- select(L2234.GlobalIntTechOMvar_elecS, -supplysector, -subsector.1, -intermittent.technology, -sector.name, -subsector.name)
L2234.GlobalIntTechOMvar_elecS <- filter (L2234.GlobalIntTechOMvar_elecS, OM.var !="NA")
names(L2234.GlobalIntTechOMvar_elecS) <- c("supplysector", "subsector", "intermittent.technology", "year", "input.OM.var", "OM.var")

#Efficiencies  
printlog( "L2234.GlobalTechEff_elecS: Efficiencies of detailed electricity generation technologies in future years" )

L2234.GlobalTechEff_elecS <- A23.elecS_tech_associations
L2234.GlobalTechEff_elecS <- L2234.GlobalTechEff_elecS[rep(1:nrow(L2234.GlobalTechEff_elecS), 
                                                                     times = length(unique(L223.GlobalTechEff_elec$year))),]
L2234.GlobalTechEff_elecS$year <- rep(unique(L223.GlobalTechEff_elec$year),each = nrow(A23.elecS_tech_associations))
L2234.GlobalTechEff_elecS %>%
  left_join(L223.GlobalTechEff_elec, by= c("technology","year")) %>%
  filter(efficiency != "NA") %>%
  select(-supplysector, -subsector.1, -technology, -sector.name, -subsector.name) -> L2234.GlobalTechEff_elecS
L2234.GlobalTechEff_elecS <- rename(L2234.GlobalTechEff_elecS, supplysector = Electric.sector, technology = Electric.sector.technology)

printlog( "L2234.GlobalIntTechEff_elecS: Efficiencies of detailed electricity generation intermittent technologies in future years" )

L2234.GlobalIntTechEff_elecS <- A23.elecS_inttech_associations
L2234.GlobalIntTechEff_elecS <- L2234.GlobalIntTechEff_elecS[rep(1:nrow(L2234.GlobalIntTechEff_elecS), 
                                                           times = length(unique(L223.GlobalIntTechEff_elec$year))),]
L2234.GlobalIntTechEff_elecS$year <- rep(unique(L223.GlobalIntTechEff_elec$year),each = nrow(A23.elecS_inttech_associations))
L2234.GlobalIntTechEff_elecS <- left_join(L2234.GlobalIntTechEff_elecS,L223.GlobalIntTechEff_elec, by= c("intermittent.technology","year"))
L2234.GlobalIntTechEff_elecS <- select(L2234.GlobalIntTechEff_elecS, -supplysector, -subsector.1, -intermittent.technology, -sector.name, -subsector.name)
L2234.GlobalIntTechEff_elecS <- filter(L2234.GlobalIntTechEff_elecS, efficiency != "NA")
L2234.GlobalIntTechEff_elecS <- rename(L2234.GlobalIntTechEff_elecS, supplysector = Electric.sector, intermittent.technology = Electric.sector.intermittent.technology)

#Technology Lifetimes 
printlog( "L2234.GlobalTechLifetime_elecS: Lifetimes of detailed electricity generation technologies" )

L2234.GlobalTechLifetime_elecS <- A23.elecS_tech_associations
L2234.GlobalTechLifetime_elecS <- L2234.GlobalTechLifetime_elecS[rep(1:nrow(L2234.GlobalTechLifetime_elecS), 
                                                           times = length(unique(L223.GlobalTechLifetime_elec$year))),]
L2234.GlobalTechLifetime_elecS$year <- rep(unique(L223.GlobalTechLifetime_elec$year),each = nrow(A23.elecS_tech_associations))
L2234.GlobalTechLifetime_elecS <- left_join(L2234.GlobalTechLifetime_elecS,L223.GlobalTechLifetime_elec, by= c("technology","year"))
L2234.GlobalTechLifetime_elecS <- filter(L2234.GlobalTechLifetime_elecS, lifetime != "NA")
L2234.GlobalTechLifetime_elecS <- select(L2234.GlobalTechLifetime_elecS, -supplysector, -subsector.1, -technology, -sector.name, -subsector.name)
L2234.GlobalTechLifetime_elecS <- rename(L2234.GlobalTechLifetime_elecS, supplysector = Electric.sector, technology = Electric.sector.technology)

printlog( "L2234.GlobalIntTechLifetime_elecS: Lifetimes of detailed electricity generation intermittent technologies" )
L2234.GlobalIntTechLifetime_elecS <- A23.elecS_inttech_associations
L2234.GlobalIntTechLifetime_elecS <- L2234.GlobalIntTechLifetime_elecS[rep(1:nrow(L2234.GlobalIntTechLifetime_elecS), 
                                                                 times = length(unique(L223.GlobalIntTechLifetime_elec$year))),]
L2234.GlobalIntTechLifetime_elecS$year <- rep(unique(L223.GlobalIntTechLifetime_elec$year),each = nrow(A23.elecS_inttech_associations))
L2234.GlobalIntTechLifetime_elecS <- left_join(L2234.GlobalIntTechLifetime_elecS,L223.GlobalIntTechLifetime_elec, by= c("intermittent.technology","year"))
L2234.GlobalIntTechLifetime_elecS <- filter(L2234.GlobalIntTechLifetime_elecS, lifetime != "NA")
L2234.GlobalIntTechLifetime_elecS <- select(L2234.GlobalIntTechLifetime_elecS, -supplysector, -subsector.1, -intermittent.technology, -sector.name, -subsector.name)
L2234.GlobalIntTechLifetime_elecS <- rename(L2234.GlobalIntTechLifetime_elecS, supplysector = Electric.sector, intermittent.technology = Electric.sector.intermittent.technology)

# S-curve shut-down decider

printlog( "L2234.GlobalTechSCurve_elecS: S-curve shutdown decider for detailed electricity generation technologies" )

L2234.GlobalTechSCurve_elecS <- A23.elecS_tech_associations
L2234.GlobalTechSCurve_elecS <- L2234.GlobalTechSCurve_elecS[rep(1:nrow(L2234.GlobalTechSCurve_elecS), 
                                                                     times = length(unique(L223.GlobalTechSCurve_elec$year))),]
L2234.GlobalTechSCurve_elecS$year <- rep(unique(L223.GlobalTechSCurve_elec$year),each = nrow(A23.elecS_tech_associations))
L2234.GlobalTechSCurve_elecS <- left_join(L2234.GlobalTechSCurve_elecS,L223.GlobalTechSCurve_elec, by= c("technology","year"))
L2234.GlobalTechSCurve_elecS <- filter(L2234.GlobalTechSCurve_elecS, lifetime != "NA")
L2234.GlobalTechSCurve_elecS <- select(L2234.GlobalTechSCurve_elecS, -supplysector, -subsector.1, -technology, -sector.name, -subsector.name)
L2234.GlobalTechSCurve_elecS <- rename(L2234.GlobalTechSCurve_elecS, supplysector = Electric.sector, technology = Electric.sector.technology)


# Profit shut-down decider
printlog( "L2234.GlobalTechProfitShutdown_elecS: Profit shut-down decider for detailed electricity generation technologies" )

L2234.GlobalTechProfitShutdown_elecS <- A23.elecS_tech_associations
L2234.GlobalTechProfitShutdown_elecS <- L2234.GlobalTechProfitShutdown_elecS[rep(1:nrow(L2234.GlobalTechProfitShutdown_elecS), 
                                                                       times = length(unique(L223.GlobalTechProfitShutdown_elec$year))),]
L2234.GlobalTechProfitShutdown_elecS$year <- rep(unique(L223.GlobalTechProfitShutdown_elec$year),each = nrow(A23.elecS_tech_associations))
L2234.GlobalTechProfitShutdown_elecS <- left_join(L2234.GlobalTechProfitShutdown_elecS,L223.GlobalTechProfitShutdown_elec, by= c("technology","year"))
L2234.GlobalTechProfitShutdown_elecS <- filter(L2234.GlobalTechProfitShutdown_elecS, median.shutdown.point != "NA")
L2234.GlobalTechProfitShutdown_elecS <- select(L2234.GlobalTechProfitShutdown_elecS, -supplysector, -subsector.1, -technology, -sector.name, -subsector.name)
L2234.GlobalTechProfitShutdown_elecS <- rename(L2234.GlobalTechProfitShutdown_elecS, supplysector = Electric.sector, technology = Electric.sector.technology)

# Additional characteristics for CCS technologies
printlog( "L2234.GlobalTechCapture_elecS: Characteristics of CCS technologies in detailed electricity generation sectors" )

L2234.GlobalTechCapture_elecS <- A23.elecS_tech_associations
L2234.GlobalTechCapture_elecS <- L2234.GlobalTechCapture_elecS[rep(1:nrow(L2234.GlobalTechCapture_elecS), 
                                                                                 times = length(unique(L223.GlobalTechCapture_elec$year))),]
L2234.GlobalTechCapture_elecS$year <- rep(unique(L223.GlobalTechCapture_elec$year),each = nrow(A23.elecS_tech_associations))
L2234.GlobalTechCapture_elecS <- left_join(L2234.GlobalTechCapture_elecS,L223.GlobalTechCapture_elec, by= c("technology","year"))
L2234.GlobalTechCapture_elecS <- filter(L2234.GlobalTechCapture_elecS, remove.fraction != "NA")
L2234.GlobalTechCapture_elecS <- select(L2234.GlobalTechCapture_elecS, -supplysector, -subsector.1, -technology, -sector.name, -subsector.name)
L2234.GlobalTechCapture_elecS <- rename(L2234.GlobalTechCapture_elecS, supplysector = Electric.sector, technology = Electric.sector.technology)

# Additional characteristics for intermittent technologies

printlog( "L2234.GlobalIntTechBackup_elecS: Backup characteristics for detailed electricity generation intermittent technologies" )

L2234.GlobalIntTechBackup_elecS <- A23.elecS_inttech_associations
L2234.GlobalIntTechBackup_elecS <- L2234.GlobalIntTechBackup_elecS[rep(1:nrow(L2234.GlobalIntTechBackup_elecS), 
                                                                           times = length(unique(L223.GlobalIntTechBackup_elec$year))),]
L2234.GlobalIntTechBackup_elecS$year <- rep(unique(L223.GlobalIntTechBackup_elec$year),each = nrow(A23.elecS_inttech_associations))
L2234.GlobalIntTechBackup_elecS <- rename(L2234.GlobalIntTechBackup_elecS, technology = intermittent.technology)
L2234.GlobalIntTechBackup_elecS <- left_join(L2234.GlobalIntTechBackup_elecS,L223.GlobalIntTechBackup_elec, by = c("technology","year"))
L2234.GlobalIntTechBackup_elecS <- filter(L2234.GlobalIntTechBackup_elecS, capacity.limit != "NA")
L2234.GlobalIntTechBackup_elecS <- select(L2234.GlobalIntTechBackup_elecS, -supplysector, -subsector.1, -technology, -sector.name, -subsector.name)
L2234.GlobalIntTechBackup_elecS <- rename(L2234.GlobalIntTechBackup_elecS, supplysector = Electric.sector, intermittent.technology = Electric.sector.intermittent.technology)

# Energy inputs 
printlog( "L2234.StubTechMarket_elecS_USA: Energy inputs" )
A23.elecS_inttech_associations %>%
  rename(Electric.sector.technology = Electric.sector.intermittent.technology, technology = intermittent.technology) -> L2234.StubTechMarket_elecS_USA_temp

L2234.StubTechMarket_elecS_USA_temp %>%
  bind_rows(A23.elecS_tech_associations) -> L2234.StubTechMarket_elecS_USA_temp

L2234.StubTechMarket_elecS_USA <- L2234.StubTechMarket_elecS_USA_temp [rep(1:nrow(L2234.StubTechMarket_elecS_USA_temp), 
                                                                     times = length(unique(L223.StubTechMarket_elec_USA$year))),]
L2234.StubTechMarket_elecS_USA$year <- rep(unique(L223.StubTechMarket_elec_USA$year),each = nrow(L2234.StubTechMarket_elecS_USA_temp))
L2234.StubTechMarket_elecS_USA$region <- rep("AK",nrow(L2234.StubTechMarket_elecS_USA))
L2234.StubTechMarket_elecS_USA <- write_to_all_states(L2234.StubTechMarket_elecS_USA, c("region","Electric.sector", "subsector", "Electric.sector.technology", "technology","year"))
L2234.StubTechMarket_elecS_USA <- rename(L2234.StubTechMarket_elecS_USA, stub.technology=technology)
L2234.StubTechMarket_elecS_USA <- left_join(L2234.StubTechMarket_elecS_USA,L223.StubTechMarket_elec_USA, by= c("stub.technology","year","region"))
L2234.StubTechMarket_elecS_USA <- select(L2234.StubTechMarket_elecS_USA, -supplysector, -subsector.y, -stub.technology)
L2234.StubTechMarket_elecS_USA <- filter(L2234.StubTechMarket_elecS_USA, minicam.energy.input != "NA")
L2234.StubTechMarket_elecS_USA <- rename(L2234.StubTechMarket_elecS_USA, supplysector = Electric.sector, subsector = subsector.x, stub.technology = Electric.sector.technology)

# Backup markets

printlog( "L2234.StubTechMarket_backup_elecS_USA: Energy inputs" )

L2234.StubTechMarket_backup_elecS_USA_temp <- A23.elecS_inttech_associations
L2234.StubTechMarket_backup_elecS_USA <- L2234.StubTechMarket_backup_elecS_USA_temp [rep(1:nrow(L2234.StubTechMarket_backup_elecS_USA_temp), 
                                                                           times = length(unique(L223.StubTechMarket_backup_USA$year))),]
L2234.StubTechMarket_backup_elecS_USA$year <- rep(unique(L223.StubTechMarket_backup_USA$year),each = nrow(L2234.StubTechMarket_backup_elecS_USA_temp))
L2234.StubTechMarket_backup_elecS_USA$region <- rep("AK",nrow(L2234.StubTechMarket_backup_elecS_USA))
L2234.StubTechMarket_backup_elecS_USA <- write_to_all_states(L2234.StubTechMarket_backup_elecS_USA, c("region","Electric.sector", "subsector", "Electric.sector.intermittent.technology", "intermittent.technology","year"))
L2234.StubTechMarket_backup_elecS_USA <- rename(L2234.StubTechMarket_backup_elecS_USA, stub.technology=intermittent.technology)
L2234.StubTechMarket_backup_elecS_USA <- left_join(L2234.StubTechMarket_backup_elecS_USA,L223.StubTechMarket_backup_USA, by= c("stub.technology","year","region"))
L2234.StubTechMarket_backup_elecS_USA <- select(L2234.StubTechMarket_backup_elecS_USA, -supplysector, -subsector.y, -stub.technology)
L2234.StubTechMarket_backup_elecS_USA <- filter(L2234.StubTechMarket_backup_elecS_USA, minicam.energy.input != "NA")
L2234.StubTechMarket_backup_elecS_USA <- rename(L2234.StubTechMarket_backup_elecS_USA, supplysector = Electric.sector, subsector = subsector.x, stub.technology = Electric.sector.intermittent.technology)

printlog( "L2234.StubTechElecMarket_backup_elecS_USA: electric sector name" )
L2234.StubTechMarket_backup_elecS_USA %>%
  select(-minicam.energy.input, -market.name) %>%
  left_join(states_subregions, by = c("region" = "state")) %>%
  select(region, supplysector, subsector, stub.technology, year, grid_region) %>%
  rename(electric.sector.market = grid_region) -> L2234.StubTechElecMarket_backup_elecS_USA
  
# Calibration Year Outputs. Note that all technologies in GCAM-USA are calibrated on the output.
printlog( "L2234.StubTechProd_elecS_USA: Calibration outputs" )

L2234.StubTechProd_elecS_USA_temp1 <- A23.elecS_tech_associations
L2234.StubTechProd_elecS_USA_temp2 <- A23.elecS_inttech_associations
L2234.StubTechProd_elecS_USA_temp2  %>%
  rename(Electric.sector.technology = Electric.sector.intermittent.technology, technology = intermittent.technology)%>%
  bind_rows(L2234.StubTechProd_elecS_USA_temp1) -> L2234.StubTechProd_elecS_USA_temp

L2234.StubTechProd_elecS_USA <- L2234.StubTechProd_elecS_USA_temp [rep(1:nrow(L2234.StubTechProd_elecS_USA_temp), 
                                                                       times = length(model_base_years)),]
L2234.StubTechProd_elecS_USA$year <- rep(model_base_years,each = nrow(L2234.StubTechProd_elecS_USA_temp))
L2234.StubTechProd_elecS_USA$region <- rep("AK",nrow(L2234.StubTechProd_elecS_USA))
L2234.StubTechProd_elecS_USA <- write_to_all_states(L2234.StubTechProd_elecS_USA, c("region","Electric.sector", "subsector", "Electric.sector.technology", "technology","year"))

L2234.StubTechProd_elecS_USA %>%
  rename(stub.technology = technology) %>%
  left_join(L223.StubTechProd_elec_USA, by= c("stub.technology","year","region")) %>%
  select(-supplysector, -subsector.y, -stub.technology) %>%
  rename(supplysector = Electric.sector, subsector = subsector.x, stub.technology = Electric.sector.technology) %>%
  filter(subsector != "hydro") %>%
  mutate(share.weight.year = year) %>%
  mutate(calOutputValue = as.double(calOutputValue), subs.share.weight = as.double(subs.share.weight), share.weight = as.double(share.weight)) %>%
  mutate(calOutputValue = if_else(is.na (calOutputValue),0,calOutputValue)) %>%
  mutate(subs.share.weight = if_else(is.na(subs.share.weight),0,subs.share.weight)) %>% # Note that this is latter over-written to read in zero share-weights for subsectors in base-years with zero base-year calibration values and 1 for subsectors with any calibration values. 
  mutate(share.weight = if_else(is.na(share.weight), 0,share.weight ))-> L2234.StubTechProd_elecS_USA 

L1239_state_elec_supply %>%
  select(state, fuel, segment, year, fraction)%>%
  rename(region = state, supplysector = segment, subsector = fuel) %>%
  mutate(year = as.numeric(year)) -> L2234_fuelfractions_segment_USA 

L2234.StubTechProd_elecS_USA %>%
  left_join(L2234_fuelfractions_segment_USA, by = c("region", "supplysector", "subsector", "year")) %>%
  mutate(calOutputValue = calOutputValue*fraction) %>%
  select(-fraction) %>%
  mutate(calOutputValue = if_else(is.na (calOutputValue),0,calOutputValue)) -> L2234.StubTechProd_elecS_USA

    # Adjust subsector share-weights to read in zero share-weights for subsectors and technologies 
    # in base-years with zero base-year calibration values

L2234.StubTechProd_elecS_USA %>%
  group_by(region, supplysector, subsector, year) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  select(region, supplysector, subsector, year, calOutputValue) %>%
  rename(subsector.cal.value = calOutputValue) %>%
  left_join(L2234.StubTechProd_elecS_USA, by = c("region", "supplysector", "subsector", "year")) %>%
  mutate(subs.share.weight = as.double(subs.share.weight), share.weight = as.double(share.weight)) %>%
  mutate(subs.share.weight = if_else(subsector.cal.value == 0, 0,1)) %>%
  mutate(share.weight = if_else(calOutputValue == 0, 0,1 )) -> L2234.StubTechProd_elecS_USA


    # Update future subsector share-weights as follows:
      # 1. For coal, gas and oil - fix share-weights to calibration values. This does not need any update 
      # to the L2234.SubsectorShrwtInterp_elecS_USA table
      # 2. For nuclear - use zero  shareweights if there is no calibration year value. Interpolate to a fixed value for states
      # that have nuclear. 
      # 3. For biomass, solar, wind, geothermal, rooftop_PV and storage - interpolate to a fixed value in a future year
      # The fixed value in the above formulations are read in separately in L2234.SubsectorShrwt_elecS_USA
      # Note that we read in a final available year for investments in technologies in particular segments that we think are
      # do not make sense in L2234.StubTechAvail_elecS_USA

L2234.StubTechProd_elecS_USA %>%
  filter (year == as.numeric(set_years("final-calibration-year"))) -> L2234.StubTechProd_elecS_USA_final_cal_year

L2234.SubsectorShrwt_elecS_USA %>%
  mutate(stub.technology = if_else(supplysector == "base load generation", "nuc_base_Gen II", 
                                   if_else(supplysector == "intermediate generation", "nuc_int_Gen II",
                                           if_else(supplysector == "subpeak generation", "nuc_subpeak_Gen II",
                                                   "nuc_peak_Gen II"))) ) %>%
  left_join(L2234.StubTechProd_elecS_USA_final_cal_year, by = c("region", "supplysector","subsector", "stub.technology")) %>%
  rename (share.weight = share.weight.x, year = year.x) %>%
  mutate(share.weight = as.double(share.weight)) %>%
  mutate (subsector.cal.value = if_else(is.na(subsector.cal.value), share.weight, subsector.cal.value)) %>%
  mutate(share.weight = if_else(subsector.cal.value == 0, 0, share.weight)) %>%
  select(region, supplysector, subsector, year, share.weight) ->  L2234.SubsectorShrwt_elecS_USA

L2234.SubsectorShrwtInterpTo_elecS_USA %>%
  mutate(stub.technology = if_else(supplysector == "base load generation", "nuc_base_Gen II", 
                                   if_else(supplysector == "intermediate generation", "nuc_int_Gen II",
                                           if_else(supplysector == "subpeak generation", "nuc_subpeak_Gen II",
                                                   "nuc_peak_Gen II"))) ) %>%
  left_join(L2234.StubTechProd_elecS_USA_final_cal_year, by = c("region", "supplysector","subsector", "stub.technology")) %>%
  mutate(to.value = as.double(to.value)) %>%
  mutate (subsector.cal.value = if_else(is.na(subsector.cal.value), to.value, subsector.cal.value)) %>%
  mutate(to.value = if_else(subsector.cal.value == 0, 0, to.value)) %>%
  select(region, supplysector, subsector, apply.to, from.year, to.year, to.value, interpolation.function) ->  L2234.SubsectorShrwtInterpTo_elecS_USA


# Get the L2234.StubTechProd_elecS_USA in the right form without subsector.cal.value
  
L2234.StubTechProd_elecS_USA %>%
  select(region, supplysector, subsector, stub.technology, year, calOutputValue, share.weight.year,
         subs.share.weight, share.weight) -> L2234.StubTechProd_elecS_USA

# Fixed Output calibration for hydro
printlog( "L2234.StubTechFixOut_elecS_USA: Calibration year fixed outputs for hydro" )

L2234.StubTechFixOut_elecS_USA_temp <- A23.elecS_tech_associations
L2234.StubTechFixOut_elecS_USA <- L2234.StubTechFixOut_elecS_USA_temp [rep(1:nrow(L2234.StubTechFixOut_elecS_USA_temp), 
                                       times = length(unique(L223.StubTechFixOut_elec_USA$year))),]
L2234.StubTechFixOut_elecS_USA$year <- rep(unique(L223.StubTechFixOut_elec_USA$year),each = nrow(L2234.StubTechFixOut_elecS_USA_temp))
L2234.StubTechFixOut_elecS_USA$region <- rep("AK",nrow(L2234.StubTechFixOut_elecS_USA))
L2234.StubTechFixOut_elecS_USA <- write_to_all_states(L2234.StubTechFixOut_elecS_USA, c("region","Electric.sector", "subsector", "Electric.sector.technology", "technology","year"))

L2234.StubTechFixOut_elecS_USA %>%
  filter(subsector == "hydro") %>%
  select(-technology) %>%
  rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
  left_join(L223.StubTechFixOut_elec_USA, by = c("region", "subsector", "year")) %>%
  select(-supplysector.y, stub.technology.y) %>%
  rename(supplysector = supplysector.x, stub.technology = stub.technology.x) %>%
  left_join(L2234_fuelfractions_segment_USA, by = c("region", "supplysector", "subsector", "year")) %>%
  mutate(fixedOutput = fixedOutput*fraction) %>%
  select(-stub.technology.y, -fraction) -> L2234.StubTechFixOut_elecS_USA

# Fixed Output for hydro in future years 
printlog( "L2234.StubTechFixOut_hydro_elecS_USA: Fixed Output for hydro in future years" )

L2234.StubTechFixOut_hydro_elecS_USA_temp <- A23.elecS_tech_associations
L2234.StubTechFixOut_hydro_elecS_USA <- L2234.StubTechFixOut_hydro_elecS_USA_temp [rep(1:nrow(L2234.StubTechFixOut_hydro_elecS_USA_temp), 
                                                                           times = length(unique(L223.StubTechFixOut_hydro_USA$year))),]
L2234.StubTechFixOut_hydro_elecS_USA$year <- rep(unique(L223.StubTechFixOut_hydro_USA$year),each = nrow(L2234.StubTechFixOut_hydro_elecS_USA_temp))
L2234.StubTechFixOut_hydro_elecS_USA$region <- rep("AK",nrow(L2234.StubTechFixOut_hydro_elecS_USA))
L2234.StubTechFixOut_hydro_elecS_USA <- write_to_all_states(L2234.StubTechFixOut_hydro_elecS_USA, c("region","Electric.sector", "subsector", "Electric.sector.technology", "technology","year"))

L2234_fuelfractions_segment_USA %>%
  filter(year == 2010, subsector == "hydro") -> L2234_fuelfractions_segment_USA_hydro_temp
L2234_fuelfractions_segment_USA_hydro <- L2234_fuelfractions_segment_USA_hydro_temp[rep(1:nrow(L2234_fuelfractions_segment_USA_hydro_temp),
                                                                                        times = length(model_years) -3),]
L2234_fuelfractions_segment_USA_hydro$year <- rep(model_years[4:length(model_years)],
                                                  each = nrow(L2234_fuelfractions_segment_USA_hydro_temp))

L2234.StubTechFixOut_hydro_elecS_USA %>%
  filter(subsector =="hydro") %>%
  select(-technology) %>%
  rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
  left_join(L223.StubTechFixOut_hydro_USA, by =c("region", "subsector", "year"))  %>%
  select(-supplysector.y, -stub.technology.y) %>%
  rename(supplysector = supplysector.x, stub.technology = stub.technology.x) %>%
  left_join(L2234_fuelfractions_segment_USA_hydro, by = c("region", "supplysector", "subsector", "year")) %>%
  mutate(fixedOutput = fixedOutput * fraction) %>%
  select(-fraction)  -> L2234.StubTechFixOut_hydro_elecS_USA
  
# Efficiencies for biomass, coal, oil, and gas technologies in calibration years 
printlog( "L2234.StubTechEff_elecS_USA: Efficiencies of coal, oil and gas technologies in calibration years" )


L2234.StubTechProd_elecS_USA %>%
  filter(subsector %in% c("coal", "gas", "refined liquids", "biomass")) %>%
  select(region, supplysector, subsector, stub.technology, year) %>%
  left_join(L2234.StubTechMarket_elecS_USA, by = c("region","supplysector", "subsector", "stub.technology", "year")) %>%
  left_join(A23.elecS_tech_associations, by = c("supplysector" = "Electric.sector", "subsector", "stub.technology" = "Electric.sector.technology")) %>%
  left_join(L223.StubTechEff_elec_USA, by = c("region", "subsector", "technology" = "stub.technology" , "year", "minicam.energy.input")) %>%
  select(region, supplysector.x, subsector, stub.technology, year, minicam.energy.input, efficiency, market.name.x) %>%
  rename(supplysector = supplysector.x, market.name=market.name.x) %>%
  filter(efficiency != "NA") -> L2234.StubTechEff_elecS_USA


# Capacity factors by state for wind and solar

printlog( "L2234.StubTechCapFactor_elecS_wind_USA and L2234.StubTechCapFactor_elecS_solar_USA: Capacity factors by state for wind and solar" )

A23.elecS_inttech_associations %>% 
  rename(stub.technology = intermittent.technology) %>%
  left_join(L223.StubTechCapFactor_elec_wind_USA, by = "stub.technology") %>%
  select(region, Electric.sector, subsector.x, Electric.sector.intermittent.technology, year, 
         input.capital, capacity.factor.capital,input.OM.fixed, capacity.factor.OM) %>%
  rename(supplysector = Electric.sector, subsector = subsector.x, stub.technology = Electric.sector.intermittent.technology) %>%
  filter(subsector != "solar") %>%
  filter(capacity.factor.capital != "NA") -> L2234.StubTechCapFactor_elecS_wind_USA
  
A23.elecS_inttech_associations %>% 
  rename(stub.technology = intermittent.technology) %>%
  left_join(L223.StubTechCapFactor_elec_solar_USA, by = "stub.technology") %>%
  select(region, Electric.sector, subsector.x, Electric.sector.intermittent.technology, year, 
         input.capital, capacity.factor.capital,input.OM.fixed, capacity.factor.OM) %>%
  rename(supplysector = Electric.sector, subsector = subsector.x, stub.technology = Electric.sector.intermittent.technology) %>%
  filter(subsector != "wind") %>%
  filter(capacity.factor.capital != "NA") -> L2234.StubTechCapFactor_elecS_solar_USA


# Final availble year for technologies (this is to make sure that, for example, no new coal comes in in the peak segment)

printlog( "L2234.StubTechAvail_elecS_USA: Final available year for technologies" )

L2234.StubTechAvail_elecS_USA <- write_to_all_states(A23.elecS_tech_availability, c("region","supplysector", "subsector", "stub.technology","initial.available.year","final.available.year"))

# Remove geothermal in states with no geothermal resource

  #Indicate states where geothermal subsector and technologies will not be created

NREL_us_re_technical_potential %>%
  filter (Geothermal_Hydrothermal_GWh == 0) %>%
  select(State) %>%
  rename(state_name = State) %>%
  left_join(states_subregions, by = "state_name") %>%
  mutate (geothermal_resource = "none") %>%
  select(state, geothermal_resource) %>%
  rename(region = state) -> geo_states_noresource 

  # Remove geothermal subsector from tables

L2234.geo.tables <- list(L2234.SubsectorLogit_elecS_USA = L2234.SubsectorLogit_elecS_USA,
                               L2234.SubsectorShrwt_elecS_USA = L2234.SubsectorShrwt_elecS_USA,
                               L2234.SubsectorShrwtInterp_elecS_USA = L2234.SubsectorShrwtInterp_elecS_USA,
                               L2234.SubsectorShrwtInterpTo_elecS_USA = L2234.SubsectorShrwtInterpTo_elecS_USA,
                               L2234.StubTechEff_elecS_USA = L2234.StubTechEff_elecS_USA,
                               L2234.StubTechAvail_elecS_USA = L2234.StubTechAvail_elecS_USA,
                               L2234.StubTechProd_elecS_USA = L2234.StubTechProd_elecS_USA)

for (i in 1:length (L2234.geo.tables)) {
  L2234.geo.tables[[i]] %>%
    left_join(geo_states_noresource, by = "region") %>%
    mutate(geothermal_resource = paste(geothermal_resource, subsector, sep = "-")) %>%
    filter (geothermal_resource != "none-geothermal") %>%
    select (-geothermal_resource) -> L2234.geo.tables[[i]] 
  
}

   # Assign the tables back to the original dataframes

L2234.SubsectorLogit_elecS_USA <- L2234.geo.tables[[1]]
L2234.SubsectorShrwt_elecS_USA <- L2234.geo.tables[[2]]
L2234.SubsectorShrwtInterp_elecS_USA <- L2234.geo.tables[[3]]
L2234.SubsectorShrwtInterpTo_elecS_USA <- L2234.geo.tables[[4]]
L2234.StubTechEff_elecS_USA <- L2234.geo.tables[[5]]
L2234.StubTechAvail_elecS_USA <- L2234.geo.tables[[6]]
L2234.StubTechProd_elecS_USA <- L2234.geo.tables[[7]]

# Create tables for non-energy and energy inputs for any new technologies such as battery 
# and append them with corresponding tables

 # Non-energy inputs for additional technologies such as battery
L2234.GlobalTech_non_energy_elecS <- A23.elecS_globaltech_non_energy_inputs

L2234.GlobalTech_non_energy_elecS %>%
  mutate(input.capital = "capital") %>%
  select(supplysector, subsector, technology, period, input.capital, capital.cost, fcr, capacity.factor) %>%
  rename(capital.overnight = capital.cost, fixed.charge.rate = fcr, year = period) -> L2234.GlobalTechCapital_elecS_additonal

L2234.GlobalTechCapital_elecS %>%
  bind_rows(L2234.GlobalTechCapital_elecS_additonal) -> L2234.GlobalTechCapital_elecS
  
L2234.GlobalTech_non_energy_elecS %>%
  mutate(input.OM.fixed = "OM-fixed") %>%
  select(supplysector, subsector, technology, period, input.OM.fixed, fixed.om, capacity.factor.om) %>%
  rename(OM.fixed = fixed.om, year = period, capacity.factor = capacity.factor.om ) -> L2234.GlobalTechOMfixed_elecS_additonal

L2234.GlobalTechOMfixed_elecS %>%
  bind_rows(L2234.GlobalTechOMfixed_elecS_additonal) -> L2234.GlobalTechOMfixed_elecS

L2234.GlobalTech_non_energy_elecS %>%
  mutate(input.OM.var = "OM-var") %>%
  select(supplysector, subsector, technology, period, input.OM.var, variable.om) %>%
  rename(OM.var = variable.om, year = period) -> L2234.GlobalTechOMvar_elecS_additonal

L2234.GlobalTechOMvar_elecS %>%
  bind_rows(L2234.GlobalTechOMvar_elecS_additonal) -> L2234.GlobalTechOMvar_elecS


L2234.GlobalTech_non_energy_elecS %>%
  select(supplysector, subsector, technology, period, lifetime) %>%
  rename(year = period) -> L2234.GlobalTechLifetime_elecS_additonal

L2234.GlobalTechLifetime_elecS %>%
  bind_rows(L2234.GlobalTechLifetime_elecS_additonal) -> L2234.GlobalTechLifetime_elecS

L2234.GlobalTech_non_energy_elecS %>%
  select(supplysector, subsector, technology, period, lifetime, steepness, half.life) %>%
  rename(year = period) -> L2234.GlobalTechSCurve_elecS_additonal

L2234.GlobalTechSCurve_elecS %>%
  bind_rows(L2234.GlobalTechSCurve_elecS_additonal) -> L2234.GlobalTechSCurve_elecS

  # Energy Inputs for additional technologies such as battery

L2234.StubTech_energy_elecS_USA <- write_to_all_states( A23.elecS_stubtech_energy_inputs, c("region", "supplysector","subsector","stub.technology", 
                                                                                                        "period", "minicam.energy.input", "market.name", "efficiency") )

L2234.StubTech_energy_elecS_USA %>%
  left_join(states_subregions, by = c("region"  = "state")) %>%
  mutate( market.name = if_else(market.name == "grid_region", grid_region, region)) %>%
  select(region, supplysector, subsector, stub.technology, period, minicam.energy.input,  efficiency, market.name) %>%
  rename(year = period) -> L2234.StubTechEff_elecS_USA_additional

L2234.StubTechEff_elecS_USA %>%
  bind_rows(L2234.StubTechEff_elecS_USA_additional) -> L2234.StubTechEff_elecS_USA


# 3. Build csvs for grid region sectors and append them to state-level tables where possible.

# Delete old electriicty sector in grid regions
L2234.elec_delete_grid <- write_to_all_grid_regions (A23.elec_delete, c("region","supplysector"))
L2234.elec_delete %>%
  bind_rows(L2234.elec_delete_grid) -> L2234.elec_delete

# Create horizontal and vertical supplysectors in grid regions
L2234.Supplysector_elecS_grid <- write_to_all_grid_regions(A23.elecS_sector, c( names_Supplysector, "logit.type" ))
L2234.Supplysector_elecS_USA %>%
  bind_rows(L2234.Supplysector_elecS_grid) -> L2234.Supplysector_elecS_USA

L2234.ElecReserve_elecS_grid <- write_to_all_grid_regions(A23.elecS_metainfo, c("region", "supplysector","electricity.reserve.margin", "average.grid.capacity.factor"))
L2234.ElecReserve_elecS_USA %>%
  bind_rows(L2234.ElecReserve_elecS_grid) -> L2234.ElecReserve_elecS_USA

# Logits for subsectors in grid regions 
L2234.Supplysector_elecS_USA %>%
  left_join(states_subregions, by = c("region" = "state")) %>%
  filter(grid_region != "NA") %>%
  select(region, supplysector, grid_region) %>%
  mutate(subsector = paste(region,supplysector, sep = " ")) %>%
  select(grid_region, supplysector, subsector) %>%
  rename(region = grid_region) %>%
  mutate(logit.year.fillout = set_years("start-year")) %>%
  mutate(logit.exponent = -6) %>%
  mutate(logit.type = "relative-cost-logit") -> L2234.SubsectorLogit_elecS_grid

L2234.SubsectorLogit_elecS_grid <- L2234.SubsectorLogit_elecS_grid[order(L2234.SubsectorLogit_elecS_grid$region),]

L2234.SubsectorLogit_elecS_USA %>%
   bind_rows(L2234.SubsectorLogit_elecS_grid) -> L2234.SubsectorLogit_elecS_USA
  
# Shareweights for subsectors in grid regions 
L2234.SubsectorLogit_elecS_grid %>%
  select(region, supplysector, subsector) %>%
  mutate(year.fillout = set_years("initial-future-year")) %>%
  mutate(share.weight = 1) -> L2234.SubsectorShrwtFllt_elecS_grid

L2234.SubsectorLogit_elecS_grid %>%
  select(region, supplysector, subsector) %>%
  mutate(apply.to = "share-weight") %>%
  mutate(from.year = set_years ("final-calibration-year")) %>%
  mutate(to.year = set_years ("end-year") ) %>%
  mutate(interpolation.function = "fixed" ) -> L2234.SubsectorShrwtInterp_elecS_grid

# Shareweights for technologies in grid region sectors. This is a new table that needs to created shareweights for state-level 
# technologies are read in the global-technology-database.

L2234.SubsectorLogit_elecS_grid %>%
  select(region, supplysector, subsector) %>%
  mutate(technology = subsector) %>%
  mutate(year = model_years[[1]]) %>%
  mutate (share.weight = 1) -> L2234.TechShrwt_elecS_grid_temp

L2234.TechShrwt_elecS_grid <- L2234.TechShrwt_elecS_grid_temp[rep(1:nrow(L2234.TechShrwt_elecS_grid_temp), 
                                                                          times = length(unique(model_years))),]
L2234.TechShrwt_elecS_grid$year <- rep(model_years, each = nrow(L2234.TechShrwt_elecS_grid_temp))

# Specify inputs for technologies in grid regions
L2234.TechShrwt_elecS_grid %>%
  select(-share.weight) %>%
  mutate(minicam.energy.input = supplysector) %>%
  mutate(market.name = substr(subsector,1,2)) %>%
  filter(market.name %in% states_subregions$state) -> L2234.TechMarket_elecS_grid


# Coefficients for technologies in grid region sectors. 

 # Coefficients for generation sectors are 1.
L2234.TechMarket_elecS_grid %>%
  mutate(coefficient = 1) %>%
  select(region, supplysector, subsector, technology, year,
        minicam.energy.input, coefficient, market.name)-> L2234.TechCoef_elecS_grid

# Calibration years outputs for technologies in grid regions
L2234.StubTechFixOut_elecS_USA %>%
  left_join(states_subregions, by = c("region" = "state")) %>%
  select(region, grid_region, supplysector, subsector, stub.technology, year, fixedOutput) %>%
  group_by (region, grid_region, supplysector, year) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() -> L2234.TechFixOut_elecS_grid

L2234.StubTechProd_elecS_USA %>%
  left_join(states_subregions, by = c("region" = "state")) %>%
  select(region, grid_region, supplysector, subsector, stub.technology, year, calOutputValue) %>%
  group_by (region, grid_region,  supplysector, year) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup()  %>%
  left_join (L2234.TechFixOut_elecS_grid, by = c("region", "grid_region", "supplysector", "year")) -> L2234.TechProd_elecS_grid

for(i in 1: nrow(L2234.TechProd_elecS_grid)) {
  if (is.na(L2234.TechProd_elecS_grid[i,"fixedOutput"])) {
    L2234.TechProd_elecS_grid[i, "fixedOutput"] <- 0
  }
}

L2234.TechProd_elecS_grid %>%
  mutate(calOutputValue = calOutputValue + fixedOutput) %>%
  select(-fixedOutput) %>%
  mutate (subsector = paste(region, supplysector, sep = " ")) %>%
  mutate (technology = subsector) %>%
  mutate(share.weight.year = year) %>%
  mutate(share.weight.year = as.numeric(share.weight.year) ) %>%
  mutate(subs.share.weight = 1) %>%
  mutate(share.weight = 1) %>%
  select(grid_region, supplysector, subsector, technology, year, calOutputValue, share.weight.year, subs.share.weight, share.weight) %>%
  rename(region = grid_region) -> L2234.TechProd_elecS_grid

# Adjust subsector and technology shareweights in Calibation table to zero if calibration output is zero.

L2234.TechProd_elecS_grid %>%
  group_by(region, supplysector, subsector, year) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  select(region, supplysector, subsector, year, calOutputValue) %>%
  rename(subsector.cal.value = calOutputValue) %>%
  left_join(L2234.TechProd_elecS_grid, by = c("region", "supplysector", "subsector", "year")) %>%
  mutate(subs.share.weight = as.double(subs.share.weight), share.weight = as.double(share.weight)) %>%
  mutate(subs.share.weight = if_else(subsector.cal.value == 0, 0,1)) %>%
  mutate(share.weight = if_else(calOutputValue == 0, 0,1 )) %>%
  select(region, supplysector, subsector, technology, year, calOutputValue,
         share.weight.year, subs.share.weight, share.weight)-> L2234.TechProd_elecS_grid


# Setting up files for pass-through sector structure

#Set up equivalent sector and technology tag names. 

printlog( "L2234.SectorNodeEquiv: Sets up equivalent sector tag names to avoid having to partition input tables" )
L2234.SectorNodeEquiv <- data.frame( t( c( "SectorXMLTags", "supplysector", "pass-through-sector" ) ) )

printlog( "L2234.TechNodeEquiv: Sets up equivalent technology tag names to avoid having to partition input tables" )
L2234.TechNodeEquiv <- data.frame( t( c( "TechnologyXMLTags", "technology", "intermittent-technology", "pass-through-technology" ) ) )

# printlog( "Create a L2234.PassThroughSector_elecS_USA table" )
# Create a L2234.PassThroughSector_elecS_USA table. 
# The marginal revenue sector is the region's electricity sector whereas the marginal revenue market is the grid region.

L2234.Supplysector_elecS_USA %>%
  left_join(states_subregions, by = c("region" = "state")) %>%
  filter(is.na(grid_region) != "TRUE") %>%
  select(region, supplysector, grid_region) %>%
  rename(passthrough.sector = supplysector, marginal.revenue.market = grid_region ) %>%
  mutate(marginal.revenue.sector = passthrough.sector) %>%
  select(region, passthrough.sector,marginal.revenue.sector, marginal.revenue.market) -> L2234.PassThroughSector_elecS_USA
  
printlog( "Create a L2234.PassThroughTech_elecS_grid table" )
#Create a L223.PassThroughTech_elec_FERC dataframe (to be converted into a csv table later). This one should contain 
#region, supplysector, subsector, technology for the grid regions to which electricity produced in states is passed through. 
#Note that the "technology" in this data-frame will be called "passthrough technology"

L2234.TechShrwt_elecS_grid %>%
  select(-year, -share.weight) -> L2234.PassThroughTech_elecS_grid

# Specify Sector and Subsector Logit tables

L2234.SectorLogitTables_elecS_USA <- get_logit_fn_tables( L2234.Supplysector_elecS_USA, names_SupplysectorLogitType, base.header="Supplysector_",
                                                          include.equiv.table=T, write.all.regions=F )

L2234.SubsectorLogitTables_elecS_USA <- get_logit_fn_tables( L2234.SubsectorLogit_elecS_USA, names_SubsectorLogitType, base.header="SubsectorLogit_",
                                                             include.equiv.table=F, write.all.regions=F )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L2234.elec_delete, "DeleteSupplysector", "GCAMUSA_LEVEL2_DATA", "L2234.elec_delete", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )

#Writing Passthrough Sector files first. Equivalent tag names specified earlier should take care of consistency across xml files
#as long as the L223.SectorNodeEquiv and L223TechNodeEquiv files are read in first in the xml batch file (batch_electricity_USA.xml)

write_mi_data( L2234.SectorNodeEquiv, "EQUIV_TABLE", "GCAMUSA_LEVEL2_DATA", "L2234.SectorNodeEquiv", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.TechNodeEquiv, "EQUIV_TABLE", "GCAMUSA_LEVEL2_DATA", "L2234.TechNodeEquiv", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.PassThroughSector_elecS_USA, "PassThroughSector", "GCAMUSA_LEVEL2_DATA", "L2234.PassThroughSector_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.PassThroughTech_elecS_grid, "PassThroughTech", "GCAMUSA_LEVEL2_DATA", "L2234.PassThroughTech_elecS_grid", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )

printlog( "NOTE: writing out the tables in this step as well")
L2234.tables <- list( L2234.Supplysector_elecS_USA = L2234.Supplysector_elecS_USA,
                     L2234.ElecReserve_elecS_USA = L2234.ElecReserve_elecS_USA,
                     L2234.SubsectorLogit_elecS_USA = L2234.SubsectorLogit_elecS_USA,
                     L2234.SubsectorShrwtInterp_elecS_USA = L2234.SubsectorShrwtInterp_elecS_USA,
                     L2234.SubsectorShrwtInterpTo_elecS_USA = L2234.SubsectorShrwtInterpTo_elecS_USA,
                     L2234.SubsectorShrwt_elecS_USA = L2234.SubsectorShrwt_elecS_USA,
                     L2234.StubTechEff_elecS_USA = L2234.StubTechEff_elecS_USA,
                     L2234.StubTechCapFactor_elecS_solar_USA = L2234.StubTechCapFactor_elecS_solar_USA,
                     L2234.StubTechCapFactor_elecS_wind_USA = L2234.StubTechCapFactor_elecS_wind_USA,
                     L2234.SubsectorLogit_elecS_grid = L2234.SubsectorLogit_elecS_grid,
                     L2234.SubsectorShrwtFllt_elecS_grid = L2234.SubsectorShrwtFllt_elecS_grid,
                     L2234.SubsectorShrwtInterp_elecS_grid = L2234.SubsectorShrwtInterp_elecS_grid)

# The logit functions should be processed before any other table that needs to read logit exponents
L2234.tables <- c( read_logit_fn_tables( "ENERGY_LEVEL2_DATA", "L223.Supplysector_", skip=4, include.equiv.table=T ),
                  read_logit_fn_tables( "ENERGY_LEVEL2_DATA", "L223.SubsectorLogit_", skip=4, include.equiv.table=F ),
                  L2234.tables )

for( i in 1:length( L2234.tables ) ){
  if( !is.null( L2234.tables[[i]] ) ){
    objectname <- paste0( names( L2234.tables[i] ), "_USA" )
    if( substr( objectname, 6, 16 ) == "EQUIV_TABLE" || nrow( subset( L2234.tables[[i]], region == "USA" ) ) == 0 ) {
      # Just use the object as is
      object <- L2234.tables[[i]]
    } else {
      object <- write_to_all_states( subset( L2234.tables[[i]], region == "USA" ), names( L2234.tables[[i]] ) )
    }
    if( subs %in% names( object ) ) object <- subset( object, !paste( region, subsector ) %in% geo_states_noresource )
    #Re-set markets from USA to regional markets, if called for in the GCAM-USA assumptions
    if( use_regional_fuel_markets & "market.name" %in% names( object ) ){
      object$market.name[ object[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
        match( object$region[ object[[input]] %in% regional_fuel_markets], states_subregions$state ) ]
    }
    assign( objectname, object )
    curr_table_name <- names( L2234.tables )[i]
    IDstringendpoint <- if( grepl( "_", curr_table_name ) & !grepl( 'EQUIV_TABLE', curr_table_name ) & !grepl( '-logit$', curr_table_name ) ) {
      regexpr( "_", curr_table_name, fixed = T ) - 1
    } else {
      nchar( curr_table_name )
    }
    IDstring <- substr( curr_table_name, 6, IDstringendpoint )
  }
}

for( curr_table in names ( L2234.SectorLogitTables_elecS_USA ) ) {
  write_mi_data( L2234.SectorLogitTables_elecS_USA[[ curr_table ]]$data, L2234.SectorLogitTables_elecS_USA[[ curr_table ]]$header,
                 "GCAMUSA_LEVEL2_DATA", paste0("L2234.", L2234.SectorLogitTables_elecS_USA[[ curr_table ]]$header ), "GCAMUSA_XML_BATCH",
                 "batch_elec_segments_USA.xml" )
}
write_mi_data( L2234.Supplysector_elecS_USA, IDstring="Supplysector", domain="GCAMUSA_LEVEL2_DATA", fn="L2234.Supplysector_elecS_USA",
               batch_XML_domain="GCAMUSA_XML_BATCH", batch_XML_file="batch_elec_segments_USA.xml" ) 


write_mi_data( L2234.ElecReserve_elecS_USA, "ElecReserve", "GCAMUSA_LEVEL2_DATA", "L2234.ElecReserve_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )

for( curr_table in names ( L2234.SubsectorLogitTables_elecS_USA ) ) {
  write_mi_data( L2234.SubsectorLogitTables_elecS_USA[[ curr_table ]]$data, L2234.SubsectorLogitTables_elecS_USA[[ curr_table ]]$header,
                 "GCAMUSA_LEVEL2_DATA", paste0("L2234.", L2234.SubsectorLogitTables_elecS_USA[[ curr_table ]]$header ), "GCAMUSA_XML_BATCH",
                 "batch_elec_segments_USA.xml" )
}
 write_mi_data( L2234.SubsectorLogit_elecS_USA, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L2234.SubsectorLogit_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" ) 

# Note that we want to write subsector share-weights after calibration year share-weights are read in

write_mi_data( L2234.GlobalTechShrwt_elecS, "GlobalTechShrwt", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechShrwt_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalIntTechShrwt_elecS, "GlobalIntTechShrwt", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalIntTechShrwt_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalTechCapital_elecS, "GlobalTechCapital", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechCapital_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalIntTechCapital_elecS, "GlobalIntTechCapital", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalIntTechCapital_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalTechOMfixed_elecS, "GlobalTechOMfixed", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechOMfixed_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalIntTechOMfixed_elecS, "GlobalIntTechOMfixed", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalIntTechOMfixed_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalTechOMvar_elecS, "GlobalTechOMvar", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechOMvar_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalIntTechOMvar_elecS, "GlobalIntTechOMvar", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalIntTechOMvar_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalTechEff_elecS, "GlobalTechEff", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechEff_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalIntTechEff_elecS, "GlobalIntTechEff", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalIntTechEff_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalTechLifetime_elecS, "GlobalTechLifetime", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechLifetime_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalIntTechLifetime_elecS, "GlobalIntTechLifetime", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalIntTechLifetime_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalTechProfitShutdown_elecS, "GlobalTechProfitShutdown", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechProfitShutdown_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalTechSCurve_elecS, "GlobalTechSCurve", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechSCurve_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalTechCapture_elecS, "GlobalTechCapture", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalTechCapture_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.GlobalIntTechBackup_elecS, "GlobalIntTechBackup", "GCAMUSA_LEVEL2_DATA", "L2234.GlobalIntTechBackup_elecS", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )

write_mi_data( L2234.StubTechMarket_elecS_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L2234.StubTechMarket_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.StubTechMarket_backup_elecS_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L2234.StubTechMarket_backup_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.StubTechElecMarket_backup_elecS_USA, "StubTechElecMarket", "GCAMUSA_LEVEL2_DATA", "L2234.StubTechElecMarket_backup_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.StubTechProd_elecS_USA, "StubTechProd", "GCAMUSA_LEVEL2_DATA", "L2234.StubTechProd_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )

# Write subsector share-weights for states after calibration year share-weights which are read in the L2234.StubTechProd_elecS_USA table

write_mi_data( L2234.SubsectorShrwt_elecS_USA, "SubsectorShrwt", "GCAMUSA_LEVEL2_DATA", "L2234.SubsectorShrwt_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.SubsectorShrwtInterp_elecS_USA, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L2234.SubsectorShrwtInterp_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.SubsectorShrwtInterpTo_elecS_USA, "SubsectorInterpTo", "GCAMUSA_LEVEL2_DATA", "L2234.SubsectorShrwtInterpTo_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )

write_mi_data( L2234.StubTechCapFactor_elecS_wind_USA, "StubTechCapFactor", "GCAMUSA_LEVEL2_DATA", "L2234.StubTechCapFactor_elecS_wind_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.StubTechCapFactor_elecS_solar_USA, "StubTechCapFactor", "GCAMUSA_LEVEL2_DATA", "L2234.StubTechCapFactor_elecS_solar_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.StubTechFixOut_elecS_USA, "StubTechFixOut", "GCAMUSA_LEVEL2_DATA", "L2234.StubTechFixOut_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.StubTechEff_elecS_USA, "StubTechEff", "GCAMUSA_LEVEL2_DATA", "L2234.StubTechEff_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.StubTechFixOut_hydro_elecS_USA, "StubTechFixOut", "GCAMUSA_LEVEL2_DATA", "L2234.StubTechFixOut_hydro_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.StubTechAvail_elecS_USA, "StubTechAvail", "GCAMUSA_LEVEL2_DATA", "L2234.StubTechAvail_elecS_USA", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )


write_mi_data( L2234.TechShrwt_elecS_grid, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L2234.TechShrwt_elecS_grid", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.TechCoef_elecS_grid, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L2234.TechCoef_elecS_grid", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.TechProd_elecS_grid, "Production", "GCAMUSA_LEVEL2_DATA", "L2234.TechProd_elecS_grid", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )

# Write subsector share-weights for grid region sectors after calibration year share-weights which are read in the L2234.TechProd_elecS_grid table
write_mi_data( L2234.SubsectorShrwtFllt_elecS_grid, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L2234.SubsectorShrwtFllt_elecS_grid", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )
write_mi_data( L2234.SubsectorShrwtInterp_elecS_grid, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L2234.SubsectorShrwtInterp_elecS_grid", "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_segments_USA.xml", "GCAMUSA_XML_FINAL", "elec_segments_USA.xml", "", xml_tag="outFile" )

logstop()