# This script creates the vertical segment supplysectors in the grid regions and also the domestic supply and 
# electricity trade sectors.

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
logstart( "L2235.electricity_FERC_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for GCAM USA electricity sectors with demand resolved at the grid region level" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )

#NOTE: this code file only builds the electric sector model input if the demand is being resolved at the level of the grid regions
if( use_regional_elec_markets ){	
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

A232.structure <- readdata( "GCAMUSA_ASSUMPTIONS", "A232.structure" )
A23.elecS_sector_vertical <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_sector_vertical" )
A23.elecS_metainfo_vertical <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_metainfo_vertical" )

L1235_elecS_demand_fraction <- readdata( "GCAMUSA_LEVEL1_DATA","L1235.elecS_demand_fraction")
# elecS_demand_fraction is the fraction of demand supplied by the vertical segments by grid region
L1235.elecS_horizontal_vertical_GCAM_coeff <- readdata( "GCAMUSA_LEVEL1_DATA", "L1235.elecS_horizontal_vertical_GCAM_coeff" )

L123.in_EJ_state_ownuse_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.in_EJ_state_ownuse_elec" )
L123.out_EJ_state_ownuse_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.out_EJ_state_ownuse_elec" )
L126.in_EJ_state_td_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L126.in_EJ_state_td_elec" )
L126.out_EJ_state_td_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L126.out_EJ_state_td_elec" )
L132.out_EJ_state_indchp_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L132.out_EJ_state_indchp_F" )
L223.Supplysector_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.Supplysector_elec", skip = 4 )
L1232.out_EJ_sR_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L1232.out_EJ_sR_elec" )

# -----------------------------------------------------------------------------
# Function to write to all grid regions
write_to_all_grid_regions <- function( data, names ){
  grid_regions <- sort( unique( states_subregions$grid_region ) )
  if ( "logit.year.fillout" %in% names ) data$logit.year.fillout <- "start-year"
  if ( "price.exp.year.fillout" %in% names ) data$price.exp.year.fillout <- "start-year"
  data_new <- set_years( data )
  data_new <- repeat_and_add_vector( data_new, "region", grid_regions)
  return( data_new[ names ] ) 
}


# 2. Perform computations
L2235_elecS_demand_fraction <- L1235_elecS_demand_fraction
grid_regions <- sort( unique( states_subregions$grid_region ) )

printlog( "PART 1: THE USA REGION" )
# Remove the USA electricity sector, and replace with electricity trade
printlog( "L2235.DeleteSupplysector_USAelec: Removing the electricity sectors of the USA region (incl. net_ownuse)" )
L2235.DeleteSupplysector_USAelec <- data.frame( region = "USA", supplysector = c( "electricity", "electricity_net_ownuse" ) )
write_mi_data( L2235.DeleteSupplysector_USAelec, "DeleteSupplysector", "GCAMUSA_LEVEL2_DATA", "L2235.DeleteSupplysector_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )

printlog( "Create vertical segment supplysectors in grid regions" )
# Creating vertical segments in grid regions
L2235.Supplysector_elecS_grid_vertical <- write_to_all_grid_regions(A23.elecS_sector_vertical, c( names_Supplysector, "logit.type" ))

L2235.ElecReserve_elecS_grid_vertical <- write_to_all_grid_regions(A23.elecS_metainfo_vertical, c("region", "supplysector","electricity.reserve.margin", "average.grid.capacity.factor"))

printlog("Reading in subsector logits and share-weights for vertical segments")
# Subsector share-weights for vertical segment supplysectors
L2235.Supplysector_elecS_grid_vertical %>%
  select(region, supplysector) %>%
  mutate(subsector = supplysector) %>%
  mutate(logit.year.fillout = set_years("start-year")) %>%
  mutate(logit.exponent = -3) %>%
  mutate(logit.type = "relative-cost-logit") -> L2235.SubsectorLogit_elecS_grid_vertical

L2235.SubsectorLogit_elecS_grid_vertical %>%
  select(region, supplysector, subsector) %>%
  mutate(year.fillout = set_years("start-year")) %>%
  mutate(share.weight = 1) -> L2235.SubsectorShrwtFllt_elecS_grid_vertical

L2235.SubsectorLogit_elecS_grid_vertical %>%
  select(region, supplysector, subsector) %>%
  mutate(apply.to = "share-weight") %>%
  mutate(from.year = set_years ("final-calibration-year")) %>%
  mutate(to.year = set_years ("end-year") ) %>%
  mutate(interpolation.function = "fixed" ) -> L2235.SubsectorShrwtInterp_elecS_grid_vertical

printlog("Technology shareweights")

L2235.SubsectorLogit_elecS_grid_vertical %>%
  select(region, supplysector, subsector) %>%
  mutate(technology = subsector) %>%
  mutate(year = model_years[[1]]) %>%
  mutate (share.weight = 1) -> L2235.TechShrwt_elecS_grid_vertical_temp

L2235.TechShrwt_elecS_grid_vertical <- L2235.TechShrwt_elecS_grid_vertical_temp[rep(1:nrow(L2235.TechShrwt_elecS_grid_vertical_temp), 
                                                                                    times = length(unique(model_years))),]
L2235.TechShrwt_elecS_grid_vertical$year <- rep(model_years, each = nrow(L2235.TechShrwt_elecS_grid_vertical_temp))


printlog("Technology inputs")

L2235_elecS_demand_fraction %>%
  mutate(supplysector = "electricity") %>%
  mutate (subsector = supplysector, technology = supplysector) %>%
  rename (region = grid_region, minicam.energy.input = vertical_segment) %>%
  mutate(market.name = region) %>%
  mutate(year= as.numeric(set_years("start-year"))) %>%
  select (-demand_fraction) -> L2235.TechMarket_elecS_grid_vertical_electricity_temp

L2235.TechMarket_elecS_grid_vertical_electricity <- L2235.TechMarket_elecS_grid_vertical_electricity_temp[rep(1:nrow(L2235.TechMarket_elecS_grid_vertical_electricity_temp),
                                                                                                              times = length(model_years)),]

L2235.TechMarket_elecS_grid_vertical_electricity$year <- rep(model_years, each = nrow(L2235.TechMarket_elecS_grid_vertical_electricity_temp))


L1235.elecS_horizontal_vertical_GCAM_coeff %>%
  mutate(market.name = region) %>%
  mutate(year= as.numeric(set_years("start-year"))) %>%
  select (-coefficient) %>%
  arrange(region)-> L2235.TechMarket_elecS_grid_vertical_temp


L2235.TechMarket_elecS_grid_vertical <- L2235.TechMarket_elecS_grid_vertical_temp[rep(1:nrow(L2235.TechMarket_elecS_grid_vertical_temp),
                                                                                      times = length(model_years)),]

L2235.TechMarket_elecS_grid_vertical$year <- rep(model_years, each = nrow(L2235.TechMarket_elecS_grid_vertical_temp))

L2235.TechMarket_elecS_grid_vertical %>% 
  bind_rows(L2235.TechMarket_elecS_grid_vertical_electricity) -> L2235.TechMarket_elecS_grid_vertical 

#Coefficients for horizontal to vertical segments. 

L2235.TechMarket_elecS_grid_vertical_electricity %>%
  left_join(L2235_elecS_demand_fraction, by = c("region" = "grid_region", 
                                        "minicam.energy.input" = "vertical_segment")) %>% 
  rename(coefficient = demand_fraction) -> L2235.TechCoef_elecS_grid_vertical_electricity

L2235.TechMarket_elecS_grid_vertical %>%
  left_join(L1235.elecS_horizontal_vertical_GCAM_coeff, by = c("region", "supplysector","subsector", "technology" ,
                                                     "minicam.energy.input")) %>%
  filter(coefficient != "NA") %>%
  select(region, supplysector, subsector, technology, year,
         minicam.energy.input, coefficient, market.name) %>%
  bind_rows(L2235.TechCoef_elecS_grid_vertical_electricity) -> L2235.TechCoef_elecS_grid_vertical
  


printlog( "L2235.Supplysector_USAelec: supplysector for electricity sector in the USA region, including logit exponent between grid regions" )
#All of the supplysector information is the same as before, except the logit exponent
A232.USAstructure <- subset( A232.structure, region == "USA" )
L2235.Supplysector_USAelec <- data.frame(
      A232.USAstructure[ names( A232.structure ) %in% names_Supplysector ],
      logit.year.fillout = min( model_base_years ), logit.exponent = A232.USAstructure$subsector.logit,
      logit.type = A232.USAstructure$subsector.logit.type )

# Append vertical segments to the supplysector and subsector logit tables 
L2235.Supplysector_elecS_grid_vertical %>%
  mutate(logit.year.fillout = as.numeric(logit.year.fillout)) %>%
  bind_rows(L2235.Supplysector_USAelec) -> L2235.Supplysector_USAelec


L2235.SectorLogitTables_USAelec <- get_logit_fn_tables( L2235.Supplysector_USAelec, names_SupplysectorLogitType,
      base.header="Supplysector_", include.equiv.table=T, write.all.regions=F )
L2235.Supplysector_USAelec <- L2235.Supplysector_USAelec[, names_Supplysector ]


#No need to read in subsector logit exponents, which are applied to the technology competition
printlog( "L2235.SubsectorShrwtFllt_USAelec: subsector (grid region) shareweights in USA electricity trade" )
L2235.SubsectorShrwtFllt_USAelec <- repeat_and_add_vector( A232.USAstructure[ names( A232.USAstructure ) %in% names_Subsector ], "grid_region", grid_regions )
for( i in 1:length( grid_regions ) ){
	L2235.SubsectorShrwtFllt_USAelec$subsector[i] <- sub( "grid_region", L2235.SubsectorShrwtFllt_USAelec$grid_region[i], L2235.SubsectorShrwtFllt_USAelec$subsector[i] )
}
L2235.SubsectorShrwtFllt_USAelec$year.fillout <- min( model_base_years )
L2235.SubsectorShrwtFllt_USAelec$share.weight <- 1
L2235.SubsectorShrwtFllt_USAelec <- L2235.SubsectorShrwtFllt_USAelec[ names_SubsectorShrwtFllt ]

printlog( "L2235.SubsectorInterp_USAelec: subsector (grid region) shareweights in USA electricity" )
printlog( "NOTE: this just carries the base year shareweights forward; regions that don't export in the base year don't export at all" )
L2235.SubsectorInterp_USAelec <- data.frame(
      L2235.SubsectorShrwtFllt_USAelec[ names_Subsector ],
      apply.to = "share-weight", from.year = max( model_base_years ), to.year = max( model_years ),
      interpolation.function = "fixed" )

# NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
L2235.SubsectorLogit_USAelec <- data.frame(
      L2235.SubsectorShrwtFllt_USAelec[ names_Subsector ],
      logit.year.fillout = min( model_base_years ), logit.exponent = A232.USAstructure$technology.logit,
      logit.type=A232.USAstructure$technology.logit.type )

L2235.SubsectorLogit_elecS_grid_vertical %>%
  mutate(logit.year.fillout = as.numeric(logit.year.fillout)) %>%
  bind_rows(L2235.SubsectorLogit_USAelec) -> L2235.SubsectorLogit_USAelec


L2235.SubsectorLogitTables_USAelec <- get_logit_fn_tables( L2235.SubsectorLogit_USAelec, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
L2235.SubsectorLogit_USAelec <- L2235.SubsectorLogit_USAelec[, names_SubsectorLogit ]

printlog( "L2235.TechShrwt_USAelec: technology shareweights, USA region" )
L2235.TechShrwt_USAelec <- repeat_and_add_vector( A232.USAstructure[ names_Tech ], "grid_region", grid_regions )

for( i in 1:length( grid_regions ) ){
	L2235.TechShrwt_USAelec$subsector[i] <- sub( "grid_region", L2235.TechShrwt_USAelec$grid_region[i], L2235.TechShrwt_USAelec$subsector[i] )
	L2235.TechShrwt_USAelec$technology[i] <- sub( "grid_region", L2235.TechShrwt_USAelec$grid_region[i], L2235.TechShrwt_USAelec$technology[i] )
}
L2235.TechShrwt_USAelec <- repeat_and_add_vector( L2235.TechShrwt_USAelec, Y, model_years )
L2235.TechShrwt_USAelec$share.weight <- 1
L2235.TechShrwt_USAelec <- L2235.TechShrwt_USAelec[ c( names_TechYr, "share.weight", "grid_region" ) ]

printlog( "L2235.TechCoef_USAelec: technology coefficients and market names, USA region")
L2235.TechCoef_USAelec <- L2235.TechShrwt_USAelec
L2235.TechCoef_USAelec[[input]] <- A232.USAstructure[[input]]
L2235.TechCoef_USAelec$coefficient <- 1
L2235.TechCoef_USAelec$market.name <- L2235.TechCoef_USAelec$grid_region
L2235.TechCoef_USAelec <- L2235.TechCoef_USAelec[ names_TechCoef ]

printlog( "Compiling flows of electricity in each FERC region: generation, cogeneration, ownuse, and consumption by all sectors" )
L2235.out_EJ_sR_elec.melt <- interpolate_and_melt( L1232.out_EJ_sR_elec, model_base_years, value.name = "generation" )
L2235.out_EJ_sR_elec.melt$region <- "USA"
L2235.elec_flows_FERC <- subset( L2235.TechShrwt_USAelec, year %in% model_base_years )
L2235.elec_flows_FERC$generation <- L2235.out_EJ_sR_elec.melt$generation[
      match( vecpaste( L2235.elec_flows_FERC[ c( "grid_region", Y ) ] ),
             vecpaste( L2235.out_EJ_sR_elec.melt[ c( "grid_region", Y ) ] ) ) ]

printlog( "Cogeneration is not included in the grid region totals; need to add it here for balance" )
L2235.out_EJ_state_indchp_F.melt <- interpolate_and_melt( L132.out_EJ_state_indchp_F, model_base_years, value.name = "cogeneration" )
L2235.out_EJ_state_indchp_F.melt$region <- states_subregions$grid_region[
      match( L2235.out_EJ_state_indchp_F.melt$state, states_subregions$state ) ]
L2235.out_EJ_sR_indchp_F.melt <- aggregate( L2235.out_EJ_state_indchp_F.melt["cogeneration"],
      by=as.list( L2235.out_EJ_state_indchp_F.melt[ c( reg, Y ) ] ), sum )
L2235.elec_flows_FERC$cogeneration <- L2235.out_EJ_sR_indchp_F.melt$cogeneration[
      match( vecpaste( L2235.elec_flows_FERC[ c( "grid_region", Y ) ] ),
             vecpaste( L2235.out_EJ_sR_indchp_F.melt[ c( "region", Y ) ] ) ) ]

#Subtract own use of electricity prior to calculating net exports
printlog( "Calculating own use in each FERC region" )
L2235.net_EJ_state_ownuse_elec <- L123.in_EJ_state_ownuse_elec
L2235.net_EJ_state_ownuse_elec[ X_historical_years ] <- L123.in_EJ_state_ownuse_elec[ X_historical_years ] - L123.out_EJ_state_ownuse_elec[ X_historical_years ]

L2235.net_EJ_state_ownuse_elec.melt <- interpolate_and_melt( L2235.net_EJ_state_ownuse_elec, model_base_years, value.name = "ownuse" )
L2235.net_EJ_state_ownuse_elec.melt$grid_region <- states_subregions$grid_region[
      match( L2235.net_EJ_state_ownuse_elec.melt$state, states_subregions$state ) ]
L2235.net_EJ_sR_ownuse_elec.melt <- aggregate( L2235.net_EJ_state_ownuse_elec.melt["ownuse"],
      by=as.list( L2235.net_EJ_state_ownuse_elec.melt[ c( "grid_region", Y ) ] ), sum )
L2235.elec_flows_FERC$ownuse <- L2235.net_EJ_sR_ownuse_elec.melt$ownuse[
      match( vecpaste( L2235.elec_flows_FERC[ c( "grid_region", Y ) ] ),
             vecpaste( L2235.net_EJ_sR_ownuse_elec.melt[ c( "grid_region", Y ) ] ) ) ]

printlog( "Indicating the sum of all demands in each FERC region (equal to the input to the elect_td sectors)" )
L2235.in_EJ_state_td_elec.melt <- interpolate_and_melt( L126.in_EJ_state_td_elec, model_base_years, value.name = "consumption" )
L2235.in_EJ_state_td_elec.melt$grid_region <- states_subregions$grid_region[
      match( L2235.in_EJ_state_td_elec.melt$state, states_subregions$state ) ]
L2235.in_EJ_sR_td_elec.melt <- aggregate( L2235.in_EJ_state_td_elec.melt[ "consumption" ],
      by=as.list( L2235.in_EJ_state_td_elec.melt[ c( "grid_region", Y ) ] ), sum )

L2235.elec_flows_FERC$consumption <- L2235.in_EJ_sR_td_elec.melt$consumption[
      match( vecpaste( L2235.elec_flows_FERC[ c( "grid_region", Y ) ] ),
             vecpaste( L2235.in_EJ_sR_td_elec.melt[ c( "grid_region", Y ) ] ) ) ]

printlog( "Calculating net exports: generation + cogeneration - ownuse - consumption" )
L2235.elec_flows_FERC$net.exports <- with( L2235.elec_flows_FERC, generation + cogeneration - ownuse - consumption )

printlog( "Split net exports into gross imports and exports")
L2235.elec_flows_FERC$imports <- with( L2235.elec_flows_FERC, pmax( 0, -1 * net.exports ) )
L2235.elec_flows_FERC$exports <- with( L2235.elec_flows_FERC, pmax( 0, net.exports ) )

#Calculate consumption from domestic sources: total consumption minus imports
L2235.elec_flows_FERC$net.supply <- with( L2235.elec_flows_FERC, consumption - imports )

printlog( "L2235.Production_exports_USAelec: calibrated exports of electricity from grid regions to shared USA region" )
L2235.Production_exports_USAelec <- L2235.elec_flows_FERC[ names_TechYr ]
L2235.Production_exports_USAelec$calOutputValue <- round( L2235.elec_flows_FERC$exports, digits_calOutput )
L2235.Production_exports_USAelec$share.weight.year <- L2235.Production_exports_USAelec$year
L2235.Production_exports_USAelec <- set_subsector_shrwt( L2235.Production_exports_USAelec )
L2235.Production_exports_USAelec$tech.share.weight <- ifelse( L2235.Production_exports_USAelec$calOutputValue == 0, 0, 1 )

printlog( "PART 2: THE FERC REGIONS" )

printlog( "L223.InterestRate_FERC: Interest rates in the FERC grid regions" )
L2235.InterestRate_FERC <- data.frame( region = grid_regions, interest.rate = default_interest.rate )

printlog( "L223.Pop_FERC: Population" )
L2235.Pop_FERC <- data.frame(
  region = rep( grid_regions, times = length( model_years ) ),
  year = sort( rep( model_years, times = length( grid_regions ) ) ),
  totalPop = 1 )

printlog( "L223.BaseGDP_FERC: Base GDP in FERC grid regions" )
L2235.BaseGDP_FERC <- data.frame( region = grid_regions, baseGDP = 1 )

printlog( "L223.LaborForceFillout_FERC: labor force in the grid regions" )
L2235.LaborForceFillout_FERC <- data.frame(
  region = grid_regions,
  year.fillout = min( model_base_years ),
  laborforce = default_laborforce )

printlog( "L2235.Supplysector_elec_FERC: supplysector information for electricity passthru sectors in the FERC regions" )
A232.FERCstructure <- repeat_and_add_vector( subset( A232.structure, region == "grid_region" ), "region", grid_regions )
for( i in 1:nrow( A232.FERCstructure ) ){
	for( j in 2:ncol( A232.FERCstructure ) ){
        if( grepl( 'grid_region', A232.FERCstructure[i,j] ) ) {
            A232.FERCstructure[i,j] <- sub( "grid_region", A232.FERCstructure$region[i], A232.FERCstructure[i,j] )
        }
	}
}
L2235.Supplysector_elec_FERC <- A232.FERCstructure[ names( A232.FERCstructure  ) %in% names_Supplysector ]
L2235.Supplysector_elec_FERC$logit.year.fillout <- min( model_base_years )
L2235.Supplysector_elec_FERC$logit.exponent <- A232.FERCstructure$subsector.logit
L2235.Supplysector_elec_FERC$logit.type <- A232.FERCstructure$subsector.logit.type
L2235.SectorLogitTables_elec_FERC <- get_logit_fn_tables( L2235.Supplysector_elec_FERC, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=F, write.all.regions=F )
L2235.Supplysector_elec_FERC <- L2235.Supplysector_elec_FERC[, names_Supplysector ]

printlog( "L2235.SubsectorShrwtFllt_elec_FERC: subsector (grid region) shareweights in USA electricity" )
L2235.SubsectorShrwtFllt_elec_FERC <- A232.FERCstructure[ names_Subsector ]
L2235.SubsectorShrwtFllt_elec_FERC$year.fillout <- min( model_base_years )
L2235.SubsectorShrwtFllt_elec_FERC$share.weight <- 1

printlog( "L2235.SubsectorInterp_elec_FERC: subsector (grid region) shareweights in USA electricity" )
L2235.SubsectorInterp_elec_FERC <- data.frame(
      L2235.SubsectorShrwtFllt_elec_FERC[ names_Subsector ],
      apply.to = "share-weight", from.year = max( model_base_years ), to.year = max( model_years ),
      interpolation.function = "fixed" )

# NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
L2235.SubsectorLogit_elec_FERC <- data.frame(
      L2235.SubsectorShrwtFllt_elec_FERC[ names_Subsector ],
      logit.year.fillout = min( model_base_years ), logit.exponent = A232.FERCstructure$technology.logit,
      logit.type=A232.FERCstructure$technology.logit.type )
L2235.SubsectorLogitTables_elec_FERC <- get_logit_fn_tables( L2235.SubsectorLogit_elec_FERC, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
L2235.SubsectorLogit_elec_FERC <- L2235.SubsectorLogit_elec_FERC[, names_SubsectorLogit ]

printlog( "L2235.TechShrwt_elec_FERC: technology shareweights, USA region" )
L2235.TechShrwt_elec_FERC <- repeat_and_add_vector( A232.FERCstructure[ names_Tech ], Y, model_years )
L2235.TechShrwt_elec_FERC$share.weight <- 1

printlog( "L2235.TechCoef_elec_FERC: technology coefficients and market names")
L2235.TechCoef_elec_FERC <- repeat_and_add_vector( A232.FERCstructure, Y, model_years )[ c( names_TechYr, input, "market.name" ) ]
#Own use will be done separately; extract it from the table here
L2235.TechCoef_elecownuse_FERC <- subset( L2235.TechCoef_elec_FERC, supplysector == "electricity_net_ownuse" )
L2235.TechCoef_elec_FERC <- subset( L2235.TechCoef_elec_FERC, supplysector != "electricity_net_ownuse" )
L2235.TechCoef_elec_FERC$coefficient <- 1
L2235.TechCoef_elec_FERC <- L2235.TechCoef_elec_FERC[ names_TechCoef ]

printlog( "L2235.TechCoef_elecownuse_FERC: own use coefficients in the grid regions" )
L2235.elec_flows_FERC$ownuse_coef <- with( L2235.elec_flows_FERC, ( generation + cogeneration ) / (generation + cogeneration - ownuse ) )
L2235.TechCoef_elecownuse_FERC$coefficient <- L2235.elec_flows_FERC$ownuse_coef[
      match( vecpaste( L2235.TechCoef_elecownuse_FERC[ c( reg, Y ) ] ),
             vecpaste( L2235.elec_flows_FERC[ c( "grid_region", Y ) ] ) ) ]
L2235.TechCoef_elecownuse_FERC$coefficient[ L2235.TechCoef_elecownuse_FERC$year %in% model_future_years ] <-
      L2235.TechCoef_elecownuse_FERC$coefficient[ L2235.TechCoef_elecownuse_FERC$year == max( model_base_years ) ]
L2235.TechCoef_elecownuse_FERC <- L2235.TechCoef_elecownuse_FERC[ names_TechCoef ]

printlog( "L2235.Production_imports_FERC: calibrated electricity imports (from USA region)" )
L2235.Production_imports_FERC <- subset( L2235.TechCoef_elec_FERC, year %in% model_base_years & market.name == "USA" )[ names_TechYr ]
L2235.Production_imports_FERC$calOutputValue <-
      round( L2235.elec_flows_FERC$imports[
          match( vecpaste( L2235.Production_imports_FERC[ c( reg, Y ) ] ),
                 vecpaste( L2235.elec_flows_FERC[ c( "grid_region", Y ) ] ) ) ],
      digits_calOutput )
L2235.Production_imports_FERC$share.weight.year <- L2235.Production_imports_FERC$year
L2235.Production_imports_FERC <- set_subsector_shrwt( L2235.Production_imports_FERC )
L2235.Production_imports_FERC$tech.share.weight <- ifelse( L2235.Production_imports_FERC$calOutputValue == 0, 0, 1 )

printlog( "L2235.Production_elec_gen_FERC: calibrated net electricity generation (from within grid region)" )
L2235.Production_elec_gen_FERC <- subset( L2235.TechCoef_elec_FERC, year %in% model_base_years & market.name != "USA" )[ names_TechYr ]
L2235.Production_elec_gen_FERC$calOutputValue <- round(
      L2235.elec_flows_FERC$net.supply[
          match( vecpaste( L2235.Production_elec_gen_FERC[ c( reg, Y ) ] ),
                 vecpaste( L2235.elec_flows_FERC[ c( "grid_region", Y ) ] ) ) ],
      digits_calOutput )
L2235.Production_elec_gen_FERC$share.weight.year <- L2235.Production_elec_gen_FERC$year
L2235.Production_elec_gen_FERC <- set_subsector_shrwt( L2235.Production_elec_gen_FERC )
L2235.Production_elec_gen_FERC$tech.share.weight <- ifelse( L2235.Production_elec_gen_FERC$calOutputValue == 0, 0, 1 )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L2235.InterestRate_FERC, "InterestRate", "GCAMUSA_LEVEL2_DATA", "L2235.InterestRate_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.Pop_FERC, "Pop", "GCAMUSA_LEVEL2_DATA", "L2235.Pop_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.BaseGDP_FERC, "BaseGDP", "GCAMUSA_LEVEL2_DATA", "L2235.BaseGDP_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.LaborForceFillout_FERC, "LaborForceFillout", "GCAMUSA_LEVEL2_DATA", "L2235.LaborForceFillout_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )

for( curr_table in names ( L2235.SectorLogitTables_USAelec ) ) {
write_mi_data( L2235.SectorLogitTables_USAelec[[ curr_table ]]$data, L2235.SectorLogitTables_USAelec[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L2235.", L2235.SectorLogitTables_USAelec[[ curr_table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_FERC_USA.xml" )
}
write_mi_data( L2235.Supplysector_USAelec, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L2235.Supplysector_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.ElecReserve_elecS_grid_vertical, "ElecReserve", "GCAMUSA_LEVEL2_DATA", "L2235.ElecReserve_elecS_grid_vertical", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )


for( curr_table in names ( L2235.SubsectorLogitTables_USAelec ) ) {
write_mi_data( L2235.SubsectorLogitTables_USAelec[[ curr_table ]]$data, L2235.SubsectorLogitTables_USAelec[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L2235.", L2235.SubsectorLogitTables_USAelec[[ curr_table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_FERC_USA.xml" )
}
write_mi_data( L2235.SubsectorLogit_USAelec, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L2235.SubsectorLogit_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.SubsectorShrwtFllt_USAelec, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L2235.SubsectorShrwtFllt_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.SubsectorInterp_USAelec, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L2235.SubsectorInterp_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.SubsectorShrwtFllt_elecS_grid_vertical, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L2235.SubsectorShrwtFllt_elecS_grid_vertical", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.SubsectorShrwtInterp_elecS_grid_vertical, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L2235.SubsectorShrwtInterp_elecS_grid_vertical", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )

write_mi_data( L2235.TechShrwt_USAelec, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L2235.TechShrwt_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.TechCoef_USAelec, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L2235.TechCoef_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.Production_exports_USAelec, "Production", "GCAMUSA_LEVEL2_DATA", "L2235.Production_exports_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.TechShrwt_elecS_grid_vertical, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L2235.TechShrwt_elecS_grid_vertical", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.TechCoef_elecS_grid_vertical, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L2235.TechCoef_elecS_grid_vertical", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )

for( curr_table in names ( L2235.SectorLogitTables_elec_FERC ) ) {
write_mi_data( L2235.SectorLogitTables_elec_FERC[[ curr_table ]]$data, L2235.SectorLogitTables_elec_FERC[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L2235.", L2235.SectorLogitTables_elec_FERC[[ curr_table ]]$header, "_FERC" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_FERC_USA.xml" )
}
write_mi_data( L2235.Supplysector_elec_FERC, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L2235.Supplysector_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )

for( curr_table in names ( L2235.SubsectorLogitTables_elec_FERC ) ) {
write_mi_data( L2235.SubsectorLogitTables_elec_FERC[[ curr_table ]]$data, L2235.SubsectorLogitTables_elec_FERC[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L2235.", L2235.SubsectorLogitTables_elec_FERC[[ curr_table ]]$header, "_FERC" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_FERC_USA.xml" )
}
write_mi_data( L2235.SubsectorLogit_elec_FERC, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L2235.SubsectorLogit_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.SubsectorShrwtFllt_elec_FERC, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L2235.SubsectorShrwtFllt_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.SubsectorInterp_elec_FERC, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L2235.SubsectorInterp_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )

write_mi_data( L2235.TechShrwt_elec_FERC, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L2235.TechShrwt_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.TechCoef_elec_FERC, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L2235.TechCoef_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.TechCoef_elecownuse_FERC, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L2235.TechCoef_elecownuse_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )

write_mi_data( L2235.Production_imports_FERC, "Production", "GCAMUSA_LEVEL2_DATA", "L2235.Production_imports_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )
write_mi_data( L2235.Production_elec_gen_FERC, "Production", "GCAMUSA_LEVEL2_DATA", "L2235.Production_elec_gen_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_electricity_FERC_USA.xml", "GCAMUSA_XML_FINAL", "electricity_FERC_USA.xml", "", xml_tag="outFile" )

} #close out from use_regional_elec_markets

logstop()
