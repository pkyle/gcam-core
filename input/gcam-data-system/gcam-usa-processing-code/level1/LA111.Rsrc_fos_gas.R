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
logstart( "LA111.Rsrc_fos_gas.R" )
printlog( "Develop state level natural gas resources at the state level." )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
USGS_gas_supply_Quad <- readdata( "GCAMUSA_LEVEL0_DATA", "USGS_gas_supply_Quad" )
USGS_basin_state_mapping <- readdata( "GCAMUSA_LEVEL0_DATA", "USGS_basin_state_mapping" )
BOEM_gas_supply_EJ <- readdata( "GCAMUSA_LEVEL0_DATA", "BOEM_gas_supply_EJ" )
L111.Prod_EJ_R_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L111.Prod_EJ_R_F_Yh" )
L111.RsrcCurves_EJ_R_Ffos <- readdata( "ENERGY_LEVEL1_DATA", "L111.RsrcCurves_EJ_R_Ffos" )
EIA_gas_A_MMcf <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_gas_A_MMcf" )
EIA_gas_offshore_MMcf <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_gas_offshore_MMcf" )
EIA_gas_shale_MMcf <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_gas_shale_MMcf" )
EIA_gas_coalbed_MMcf <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_gas_coalbed_MMcf" )
EIA_gas_US_T_MMcf <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_gas_US_T_MMcf" )

# -----------------------------------------------------------------------------
# 2. Perform computations

# NOTE: THIS FUNCTION STILL NEEDS TO BE RE-WRITTEN IN DPLYR
rename_col <- function(d, from.name, to.name) {
    names( d )[ names ( d ) == from.name ] <- to.name
    return( d )
}

# NOTE: THIS FUNCTION STILL NEEDS TO BE RE-WRITTEN IN DPLYR
# Extend state gas production backwards using the state shares from the earliest
# available year and scaling by the total US production of that type. Note if the
# year is before any data that we have then constant production is assumed.
extend_back_with_us <- function(state.d, us.d, type.name) {
    earliest.state.year <- names(state.d)[2]
    us.d <- us.d[us.d$type == type.name, ]
    last.us.year <- names( us.d )[2]
    prev.us.hist.years <- X_historical_years[ 1:( match( last.us.year, X_historical_years ) -1 ) ]
    if(any(names(us.d) == earliest.state.year)) {
        # US data precedes state data so create scalar from last state year
        # historical years before US data will be the same as last US data
        us.d[, prev.us.hist.years ] <- us.d[, last.us.year ]
        us.d[, 2:ncol(us.d)] <- us.d[, 2:ncol(us.d)] / us.d[, earliest.state.year]
    } else {
        # state data precedes US data so just set the scalars to 1 so that
        # historical years before the first state year will just be the same
        # as the last state year
        us.d[, prev.us.hist.years ] <- 1
    }
    for(X_year in X_historical_years) {
        if(!any(names(state.d) == X_year)) {
            state.d[, X_year] <- state.d[, earliest.state.year] * us.d[, X_year]
        }
    }
    return(state.d)
}

# NOTE: THIS FUNCTION STILL NEEDS TO BE RE-WRITTEN IN DPLYR
# we could really use a better method
assign_tight_gas <- function(state.d, us.d, tg.supply) {
    # convert US production by type to shares
    last.us.year <- names( us.d )[2]
    prev.us.hist.years <- X_historical_years[ 1:( match( last.us.year, X_historical_years ) -1 ) ]
    us.d[, prev.us.hist.years] <- us.d[, last.us.year]
    us.d <- melt( us.d, id.vars="type", variable.name="year" )
    us.d.total <- aggregate( value ~ year, us.d, FUN=sum )
    us.d.total <- rename_col( us.d.total, "value", "total" )
    us.d <- merge( us.d, us.d.total )
    us.d$share <- us.d$value / us.d$total
    us.d <- subset( us.d, year %in% state.d$year )

    ak.d <- subset( state.d, region == "AK", select=c( "year", "total" ) )
    ak.d <- rename_col( ak.d, "total", "Alaska" )
    state.d.sum <- aggregate(cbind(offshore, shale, coalbed, conventional, remainder) ~ year, subset(state.d, region != "AK"), FUN=sum)
    state.d.sum <- merge( state.d.sum, ak.d )
    state.d.sum$total <- with( state.d.sum, offshore + shale + coalbed + conventional + remainder + Alaska )
    # Compute shares just for comparison
    share.comp <- state.d.sum
    share.comp[, 2:7] <- share.comp[, 2:7] / share.comp$total
    share.comp$total <- NULL
    share.comp <- melt( share.comp, id.vars="year", variable.name="type" )
    share.comp$type <- as.character(share.comp$type)
    share.comp <- rename_col(share.comp, "value", "share.calc" )
    share.comp[ share.comp$type == "offshore", "type" ] <- "Lower 48 offshore"
    share.comp[ share.comp$type == "shale", "type" ] <- "Shale Gas"
    share.comp[ share.comp$type == "coalbed", "type" ] <- "Coalbed methane"
    share.comp[ share.comp$type == "conventional", "type" ] <- "Lower 48 onshore conventional"
    share.comp[ share.comp$type == "remainder", "type" ] <- "Tight gas"
    share.comp <- merge( share.comp, us.d[,c("year", "type", "share")] ) 
    share.comp$share.diff <- share.comp$share.calc - share.comp$share
    tg.to.move <- merge( us.d.total, share.comp[ share.comp$type == "Tight gas", c( "year", "share.diff" ) ] )
    tg.to.move$gas.diff <- tg.to.move$total * tg.to.move$share.diff

    tg.supply <- tg.supply[tg.supply$state != "AK" & tg.supply$state %in% unique(state.d[state.d$remainder > 0, "region"]), ]
    tg.supply$total <- sum(tg.supply$grade.1)
    tg.supply$share.supply <- tg.supply$grade.1 / tg.supply$total
    tg.to.move <- merge( tg.to.move, tg.supply[, c("state", "share.supply") ] )
    tg.to.move$tg.move <- tg.to.move$gas.diff * tg.to.move$share.supply
    tg.to.move <- rename_col(tg.to.move, "state", "region")
    state.d <- merge( state.d, tg.to.move[, c("region", "year", "tg.move") ], all=TRUE )
    state.d[is.na(state.d$tg.move), "tg.move"] <- 0
    state.d$tight <- state.d$tg.move
    state.d$remainder <- state.d$remainder - state.d$tg.move
    # TODO: what to do about remainder that went negative, we will be short by
    # this amount and we will have no conventional production.
    state.d[state.d$remainder < 0, "tight" ] <- state.d[state.d$remainder < 0, "tight" ] + state.d[state.d$remainder < 0, "remainder" ]
    state.d[state.d$remainder < 0, "remainder" ] <- 0
    state.d$conventional <- state.d$conventional + state.d$remainder
    state.d$remainder <- NULL
    state.d$tg.move <- NULL
    return(state.d)
}

# Process USGS onshore supply curves and map to states.
printlog( "Map USGS supply to states." )
# double check basin shares sum to 1.
USGS_basin_state_mapping %>% group_by(basin) %>%
  summarize(fraction = sum(fraction)) -> L111.USGS_basin_share
stopifnot( L111.USGS_basin_share$fraction == 1 )

# Map supply from basins to states.
L111.gas_prob_names <- names( USGS_gas_supply_Quad )[ grep('^F\\d\\d$', names( USGS_gas_supply_Quad ), perl=TRUE ) ]
USGS_basin_state_mapping %>% left_join(USGS_gas_supply_Quad, by = c("basin")) -> L111.gas_supply_state_T_EJ

# Note the conversion from quadrillion BTU to EJ is the same as BTU to KJ
L111.gas_supply_state_T_EJ %>% mutate_at(.vars = L111.gas_prob_names, .funs = funs(. * fraction * conv_btu_kJ)) %>%
  group_by(state, resource) %>%
  summarise_at(vars(L111.gas_prob_names), sum, na.rm = TRUE) -> L111.gas_supply_state_T_EJ

# The supply is given as total cumulative production at a given probability, we
# need the grades as the additional supply available in that grade.
# TODO: code the mutate more flexibly (if possible)
L111.gas_supply_state_T_EJ %>% mutate(grade.1 = F95, grade.2 = F50 - F95, grade.3 = F05 - F50) %>%
  select(-one_of(L111.gas_prob_names)) -> L111.gas_supply_state_T_EJ

printlog( "Add in offshore resources" )
L111.gas_supply_state_T_EJ %>% filter(grade.1 > 0 | grade.2 > 0 | grade.3 > 0 ) %>%
  bind_rows(BOEM_gas_supply_EJ %>% mutate(resource = "offshore gas") %>%
              rename(state = region)) -> L111.gas_supply_state_T_EJ

# Add in "other" unconventional gas resources
# Create state shares from USGS data
L111.gas_supply_state_T_EJ %>% filter(resource %in% c("coalbed methane", "shale gas", "tight gas")) %>%
  mutate(total = grade.1 + grade.2 + grade.3) %>%
  group_by(state) %>%
  summarise(total = sum(total)) %>%
  mutate(region_sum = sum(total)) %>%
  mutate(share = total / region_sum) %>%
  select(-total, -region_sum) -> L111.unconv_gas_supply_downscale

# Sum all natural gas resources to national total (USGS data). Subtract from energy data system  
# natural gas resource curve (L111.RsrcCurves_EJ_R_Ffos) to get remaining natural gas resources.
L111.gas_supply_state_T_EJ %>% mutate(GCAM_region_ID = USA_regID) %>%
  group_by(GCAM_region_ID) %>% summarise("USGS_total" = sum(grade.1, grade.2, grade.3)) -> L111.gas_supply_USA_T_EJ
L111.RsrcCurves_EJ_R_Ffos %>% filter(GCAM_region_ID == USA_regID) %>%
  filter(resource == "natural gas") %>%
  mutate(grade = as.numeric(gsub("grade", "", grade))) %>%
  mutate(cum_avail = cumsum(available)) %>%
  left_join(L111.gas_supply_USA_T_EJ, by = c("GCAM_region_ID")) %>%
  mutate(cum_avail_remain = cum_avail - USGS_total) %>%
  filter(cum_avail_remain >= 0) %>%
  mutate(available = ifelse(grade == min(grade), cum_avail_remain, available)) %>%
  select(-cum_avail, -USGS_total, -cum_avail_remain) %>% 
  mutate(grade = paste("grade", grade, sep = ' ')) -> L111.RsrcCurves_EJ_R_gas
# Apply state shares to remaining natural gas resources
# Rename the resource to indicate it is "other" unspecified resource
unconv_gas_supply_states <- unique(L111.unconv_gas_supply_downscale$state)
L111.RsrcCurves_EJ_R_gas %>% repeat_and_add_vector('state', unconv_gas_supply_states) %>% 
  left_join(L111.unconv_gas_supply_downscale, by = c("state")) %>%
  mutate(available = available * share) %>%
  mutate(resource = "unconventional gas other") %>%
  select(state, resource, grade, extractioncost, available) -> L111.unconv_gas_supply_state_EJ

# Process historical gas production.  We need to downscale IEA US level total
# natural gas production to split by state and gas resource type. For this we
# have *some* data: US level production by type and state level production by:
# total, offshore, shale, and coalbed.  We will need to back out tight gas and
# conventional production at the state level.

# Extend state gas production backwards using the state shares from the earliest
# available year and scaling by the total US production of that type
printlog( "Extend state production backwards." )
# NOTE:  extend_back_with_us FUNCTION MUST BE RE-WRITTEN BEFORE THIS CAN BE RE-WRITTEN IN DPLYR
L111.EIA_gas_offshore_MMcf <- extend_back_with_us( EIA_gas_offshore_MMcf, EIA_gas_US_T_MMcf, "Lower 48 offshore" )
L111.EIA_gas_shale_MMcf <- extend_back_with_us( EIA_gas_shale_MMcf, EIA_gas_US_T_MMcf, "Shale Gas" )
L111.EIA_gas_coalbed_MMcf <- extend_back_with_us( EIA_gas_coalbed_MMcf, EIA_gas_US_T_MMcf, "Coalbed methane" )

# Combine EIA production data, drop the aggregate US rows for now to avoid having to keep it consistent
printlog( "Combine EIA production data" )
EIA_gas_A_MMcf %>% gcam_interp(historical_years, rule=2 ) %>%
  gather(year, total, -region) %>%
  filter(year %in% X_historical_years) -> L111.EIA_gas_A_MMcf
L111.EIA_gas_offshore_MMcf %>% gather(year, offshore, -region) %>%
  filter(year %in% X_historical_years) -> L111.EIA_gas_offshore_MMcf
L111.EIA_gas_shale_MMcf %>% gather(year, shale, -region) %>%
  filter(year %in% X_historical_years) -> L111.EIA_gas_shale_MMcf
L111.EIA_gas_coalbed_MMcf %>% gather(year, coalbed, -region) %>%
  filter(year %in% X_historical_years) -> L111.EIA_gas_coalbed_MMcf
L111.EIA_gas_A_MMcf %>% full_join(L111.EIA_gas_offshore_MMcf, by = c("region", "year")) %>%
  left_join(L111.EIA_gas_shale_MMcf, by = c("region", "year")) %>%
  left_join(L111.EIA_gas_coalbed_MMcf, by = c("region", "year")) %>%
  replace_na(list(total = 0, offshore = 0, shale = 0, coalbed = 0)) %>%
  filter(region != "US (Agg)") -> L111.EIA_gas_MMcf

# Make adjustments and back out conventional and tight gas production using US EIA production data.
printlog( "Calculate conventional and tight gas production" )
L111.EIA_gas_MMcf %>% mutate(conventional = 0) %>%
  mutate(remainder = total - offshore - shale - coalbed) %>%
  mutate(remainder = ifelse(grepl("OCS", region), 0, remainder)) -> L111.EIA_gas_MMcf
L111.gas_supply_state_T_EJ %>% filter(resource == "tight gas", grade.1 > 0) %>% 
  select(state, grade.1) -> L111.pot_tight_gas_regions

# States that don't have any tight gas reserves probably weren't producing it
# so label it as conventional.
L111.EIA_gas_MMcf %>% left_join(L111.pot_tight_gas_regions, by = c("region" = "state")) %>%
  mutate(conventional = if_else(is.na(grade.1), conventional + remainder, conventional)) %>%
  mutate(remainder = if_else(is.na(grade.1), 0, remainder)) %>%
  select(-grade.1) -> L111.EIA_gas_MMcf
# AEO doesn't have AK producing any shale nor tight so assume it is all conventional
L111.EIA_gas_MMcf %>% mutate(conventional = if_else(region == "AK", conventional + remainder, conventional)) %>%
  mutate(remainder = if_else(region == "AK", 0, remainder)) -> L111.EIA_gas_MMcf

# A bunch of steps and assumptions to assign tight gas
# NOTE:  assign_tight_gas FUNCTION MUST BE RE-WRITTEN BEFORE THIS CAN BE RE-WRITTEN IN DPLYR
L111.EIA_gas_MMcf <- assign_tight_gas( L111.EIA_gas_MMcf, EIA_gas_US_T_MMcf, L111.pot_tight_gas_regions )

# It seems state offshore resource are accounted under conventional in the USGS
# resource supply so move them there.
L111.EIA_gas_MMcf %>% mutate(conventional = if_else(!grepl('OCS', region), conventional + offshore, conventional)) %>%
  mutate(offshore = if_else(!grepl('OCS', region), 0, offshore)) -> L111.EIA_gas_MMcf

# Addional hacks due to mismatch of calibration data and supply data
L111.EIA_gas_MMcf %>% mutate(tight = if_else(region == "MT", tight + shale, tight)) %>%
  mutate(shale = if_else(region == "MT", 0, shale)) %>%
  mutate(conventional = if_else(region == "ND", conventional + shale, conventional)) %>%
  mutate(shale = if_else(region == "ND", 0, shale)) %>%
  mutate(tight = if_else(region == "AL", tight + coalbed, tight)) %>%
  mutate(coalbed = if_else(region == "AL", 0, coalbed)) %>%
  mutate(tight = if_else(region == "VA", tight + coalbed, tight)) %>%
  mutate(coalbed = if_else(region == "VA", 0, coalbed)) %>%
  mutate(conventional = if_else(region == "LA", conventional + coalbed, conventional)) %>%
  mutate(coalbed = if_else(region == "LA", 0, coalbed)) -> L111.EIA_gas_MMcf

L111.EIA_gas_MMcf %>% select(-total) %>%
  gather(type, value, -region,- year) -> L111.EIA_gas_MMcf
L111.EIA_gas_MMcf %>% mutate(value = value * conv_MMcf_EJ) %>%
  rename("eia.prod" = value) %>%
  filter(eia.prod > 0) %>%
  mutate(type = if_else(type != "coalbed", paste(type, "gas", sep = ' '), type)) %>%
  mutate(type = if_else(type == "coalbed", "coalbed methane", type)) -> L111.EIA_gas_EJ

# Convert EIA data to shares
printlog( "Convert EIA wellhead production data to shares" )
L111.EIA_gas_EJ %>% left_join(L111.EIA_gas_EJ %>% group_by(year) %>% 
                                 summarise(eia.total = sum(eia.prod)), by = c("year")) %>%
  mutate(share = eia.prod / eia.total) -> L111.EIA_gas_EJ

# Get GCAM USA data for natural gas production
printlog( "Get USA natural gas production" )
L111.Prod_EJ_R_F_Yh %>% filter(GCAM_region_ID == USA_regID, fuel %in% c( "natural gas", "unconventional gas" )) %>%
  gather(year, value, -one_of(R_S_F)) %>%
  group_by(year) %>%
  summarise(us.prod = sum(value)) -> L111.USA_gas_EJ

# Use shares to downscale the GCAM USA data to states
printlog( "Downscale USA natural gas production to state" )
L111.EIA_gas_EJ %>% left_join(L111.USA_gas_EJ, by = c("year")) %>%
  mutate(prod = us.prod * share) %>%
  rename(depresource = type) %>%
  mutate(subresource = depresource) -> L111.gas_prod_state_T_Yh_EJ

# Format for output
L111.gas_prod_state_T_Yh_EJ %>% select(region, depresource, subresource, year, prod) %>%
  spread(year, prod) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter(region != "US (Agg)") %>%
  rename(state = region) %>%
  mutate(state = if_else(state == "Gulf Coast OCS", "Gulf of Mexico OCS", state)) -> L111.gas_prod_state_T_Yh_EJ

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L111.gas_supply_state_T_EJ <- c( "Natural gas supply by state / resource type", "Unit = EJ" )
comments.L111.unconv_gas_supply_state_EJ <- c( "Remaining unconventional natural gas supply by state", "Unit = EJ" )
comments.L111.gas_prod_state_T_Yh_EJ <- c( "Downscaled to state US natural gas primary production by resource type / year","Unit = EJ" )

#write tables as CSV files
writedata( L111.gas_supply_state_T_EJ, domain="GCAMUSA_LEVEL1_DATA", fn="L111.gas_supply_state_T_EJ", comments=comments.L111.gas_supply_state_T_EJ )
writedata( L111.unconv_gas_supply_state_EJ, domain="GCAMUSA_LEVEL1_DATA", fn="L111.unconv_gas_supply_state_EJ", comments=comments.L111.unconv_gas_supply_state_EJ )
writedata( L111.gas_prod_state_T_Yh_EJ, domain="GCAMUSA_LEVEL1_DATA", fn="L111.gas_prod_state_T_Yh_EJ", comments=comments.L111.gas_prod_state_T_Yh_EJ )

# Every script should finish with this line
logstop()
