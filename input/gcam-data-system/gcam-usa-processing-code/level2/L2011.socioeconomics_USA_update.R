# This script creates add-on files to update GCAM-USA socioeconomics to:
# 1. Match 2015 state-level population to history, 
# 2. Match state-level GDP to history through 2015, and
# 3. Apply heterogeneous labor productivity growth rates across states for the future through 2040.  
# The labor productivity growth rate assumptions are derived from the AEO-2016 GDP and population assumptions (obtained via email)
# We apply the same growth rate to states within an AEO region. AEO regions correspond to census regions.
# Post-2040, labor productivity growth rates are linearly interpolated from 2040 values to the national value in 2100.
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
logstart( "L2011.socioeconomics_USA_update.R" )
printlog( "GCAM-USA population and GDP update" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

Census_pop_hist <- readdata( "GCAMUSA_LEVEL0_DATA", "Census_pop_hist" )
Census_pop_10_15 <- readdata( "GCAMUSA_LEVEL0_DATA", "Census_pop_10_15" )
PRIMA_pop <- readdata( "GCAMUSA_LEVEL0_DATA", "PRIMA_pop" )
BEA_GDP_87_96_97USD_state <- readdata( "GCAMUSA_LEVEL0_DATA", "BEA_GDP_87_96_97USD_state" )
BEA_GDP_97_16_09USD_state <- readdata( "GCAMUSA_LEVEL0_DATA", "BEA_GDP_97_16_09USD_state" )
AEO_2016_GDP_regional <- readdata( "GCAMUSA_LEVEL0_DATA", "AEO_2016_GDP_regional" )
AEO_2016_pop_regional <- readdata( "GCAMUSA_LEVEL0_DATA", "AEO_2016_pop_regional" )
state_census_region <- readdata( "GCAMUSA_MAPPINGS", "state_census_region" )
L100.gdp_mil90usd_ctry_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L100.gdp_mil90usd_ctry_Yh" )
L201.LaborProductivity_SSP2 <- readdata( "SOCIO_LEVEL2_DATA", "L201.LaborProductivity_SSP2" , skip = 4)
L201.Pop_GCAMUSA <- readdata( "GCAMUSA_LEVEL2_DATA", "L201.Pop_GCAMUSA" , skip = 4)

# -----------------------------------------------------------------------------
# 2. Perform computations

gcamusa_state_names <- unique(states_subregions$state_name)
gcamusa_states <- unique(states_subregions$state)

printlog( "L2011.Pop_USA: Historical population by state from the U.S. Census Bureau, through 2016" )
# Merge historical population datasets
Census_pop_hist %>% 
  left_join(Census_pop_10_15 %>% select(-X2010), by = c(state)) -> L2011.Pop_USA

printlog( "L2011.Pop_USA_updated: Updated model year populations for GCAM USA from 2015 historical data and PRIMA pop" )
# Segment out 2015 population data
L2011.Pop_USA %>% 
  gather(year, totalPop, -state) %>%  
  rename(region = state) %>%
  mutate(totalPop = totalPop * conv_ones_thous) %>% 
  mutate(year = ifelse(grepl("X", year), as.numeric(gsub("X", "", year)), as.numeric(year))) %>%
  filter(year == 2015) -> L2011.Pop_USA_2015

#Future population projection by state, applying PRIMA pop ratios from 2020-2100 to 2015 historical data
states_subregions %>% 
  select(state, state_name) %>%
  left_join(PRIMA_pop %>% rename(state_name = state), by = c("state_name")) %>%
  select(state, X_model_future_years) %>%
  gather(year, pop, -state, -X2015) %>%
  mutate(pop_ratio = pop / X2015) %>%
  select(state, year, pop_ratio) -> PRIMA_pop_ratio

L2011.Pop_USA_2015 %>% 
  select(-year) %>%
  rename(pop_2015 = totalPop) %>%
  left_join(PRIMA_pop_ratio, by = c("region" = "state")) %>%
  mutate(totalPop = pop_2015 * pop_ratio) %>%
  mutate(year = ifelse(grepl("X", year), as.numeric(gsub("X", "", year)), as.numeric(year))) %>%
  select(region, year, totalPop) -> L2011.Pop_USA_future

# Merge 2015 and future projection population series
L2011.Pop_USA_2015 %>% 
  bind_rows(L2011.Pop_USA_future) %>%
  arrange(region) %>%
  mutate(totalPop = round(totalPop,3)) -> L2011.Pop_USA_updated

printlog( "L2011.BaseGDP_USA: GCAM USA base GDP by state" )
# NOTE: Bureau of Economic Analysis does not have state-level GDP data available for years prior to 1987. 
# Hence, 1987 state-level GDP are input as GCAM USA base GDP. Labor productivity growth rate for 1990 is
# calculated as 15 year annualized growth rate off of the assumed 1975 GDP. This assumption implies that
# our 1975 GDP numbers will be incorrect. However, GDP numbers from 1990 will be correct and match history.

# First, covert 1987 GDP to 1975$ to keep units consistent during labor productivity growth calculations.

BEA_GDP_87_96_97USD_state %>% 
  select(Area, X1987) %>% 
  rename(GDP = X1987) %>% 
  mutate(year = "X1975") %>%
  mutate(GDP = GDP * conv_1997_1975_USD) %>% 
  select(Area, year, GDP) -> L2011.GDP_state_1975

# Then, convert to 1990$ to be input into the model as baseGDP.
L2011.GDP_state_1975 %>% 
  mutate(baseGDP = GDP * (1/conv_1990_1975_USD)) %>%
  left_join(states_subregions %>% select(state, state_name), by = c(Area = "state_name")) %>%
  rename(region = state) %>% 
  select(region, baseGDP) %>%
  mutate(baseGDP = round(baseGDP,3)) -> L2011.BaseGDP_USA

printlog( "L2011.pcGDP_state_2015: State per-capita GDP historical data series (through 2015)" )
# Convert BEA data to 1975US$. 
BEA_GDP_87_96_97USD_state %>% 
  select(-Fips) %>% 
  gather(year, GDP, -Area) %>%
  mutate(GDP = GDP * conv_1997_1975_USD) %>%
  filter(GDP != 0) -> L2011.GDP_state_1996

BEA_GDP_97_16_09USD_state %>% 
  select(-Fips) %>% 
  gather(year, GDP, -Area) %>%
  mutate(GDP = GDP * conv_2009_1975_USD) -> L2011.GDP_state_2016

# Bind historical datasets, join state abbreviations, join population data, convert to per-capita GDP (thousand 1975$ / person).
# NOTE:  1975 per-capita GDP is 1987 GDP divided by 1975 population. As explained earlier, this does not affect 1990 numbers 
# since labor productivity growth rates are calculated off of the "wrong" 1975 GDP/capita to match true 1990 GDP. 
# Hence only 1975 GDP would be incorect and we don't care about that anyway. 

L2011.GDP_state_1975 %>% 
  bind_rows(L2011.GDP_state_1996, L2011.GDP_state_2016) %>%
  left_join(states_subregions %>% select(state, state_name), by = c(Area = "state_name")) %>%
  select(state, year, GDP) %>%
  left_join(L2011.Pop_USA %>% gather(year, pop, -state), by = c("state", "year")) %>%
  mutate(pcGDP = (GDP / pop) * conv_mil_thous) %>%
  select(state, year, pcGDP) -> L2011.pcGDP_state_hist

# Select model base years + 2015
L2011.pcGDP_state_hist %>% 
  filter(year %in% c(X_model_base_years, "X2015")) -> L2011.pcGDP_state_2015

printlog( "L2011.pcGDP_AEOreg_2040: Future per-capita GDP (through 2040), based on AEO data" )
# AEO GDP data is past 12 months, presented quarterly. Hence we filter quarter 4 (Q4) because that represents 
# the annual GDP for a given year. We then convert to million 1975$ for consistency.

AEO_2016_GDP_regional %>% 
  gather(region, GDP, -c(Year, Quarter)) %>%
  rename(year = Year) %>%
  filter(Quarter == 4) %>%
  mutate(GDP = GDP * conv_2005_1975_USD * conv_bil_mil) %>%
  mutate(region = gsub("\\.", " ", region)) %>%
  mutate(region = gsub("\\Mid Atlantic", "Middle Atlantic", region)) %>%
  select(-Quarter) -> GDP_AEOreg

# AEO population data is past 12 months, presented quarterly - filter Q4
AEO_2016_pop_regional %>% 
  gather(region, pop, -c(Year, Quarter)) %>%
  rename(year = Year) %>%
  filter(Quarter == 4) %>%
  mutate(region = gsub("\\.", " ", region)) %>%
  mutate(region = gsub("\\Mid Atlantic", "Middle Atlantic", region)) %>%
  select(-Quarter) -> Pop_AEOreg

# Calculate per-capita GDP
GDP_AEOreg %>% 
  left_join(Pop_AEOreg, by = c("region", "year")) %>%
  mutate(pcGDP = (GDP / pop) * conv_ones_thous) %>%
  select(region, year, pcGDP) %>%
  filter(year %in% model_future_years) -> L2011.pcGDP_AEOreg_2040

printlog( "L2011.LaborProductivity_USA_udpated: Labor productivity growth rate for GCAMUSA")
# Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
# Calculate the growth rate in per-capita GDP
L2011.pcGDP_state_2015 %>% 
  left_join(L2011.pcGDP_state_2015 %>% spread(state, pcGDP) %>% 
                                       mutate_at(vars(-year), lag, na.rm = TRUE) %>% 
                                       gather(state, pcGDPlag, -year), 
                                     by = c("state", "year")) %>%
  mutate(pcGDPratio = pcGDP / pcGDPlag) %>%
  select(state, year, pcGDPratio) %>% 
  mutate(year = ifelse(grepl("X", year), as.numeric(gsub("X", "", year)), as.numeric(year))) -> L2011.pcGDPratio_state_2015

L2011.pcGDP_AEOreg_2040 %>% 
  left_join(L2011.pcGDP_AEOreg_2040 %>% spread(region, pcGDP) %>%
                                       mutate_at(vars(-year), lag, na.rm = TRUE) %>%
                                       gather(region, pcGDPlag, -year), 
                                     by = c("region", "year")) %>%
  mutate(pcGDPratio = pcGDP / pcGDPlag) %>%
  select(region, year, pcGDPratio) -> L2011.pcGDPratio_AEOreg_2040

L2011.pcGDPratio_AEOreg_2040 %>% 
  left_join(state_census_region %>% select(state, census_region), by = c("region" = "census_region")) %>%
  select(state, year, pcGDPratio) -> L2011.pcGDPratio_state_2040

# Annualize the ratios to return annual growth rates
L2011.pcGDPratio_state_2015 %>% 
  group_by(state) %>%
  mutate(time = year - lag(year, n = 1L)) %>% 
  mutate(laborproductivity = (pcGDPratio ^ (1 / time)) - 1) %>% 
  select(state, year, laborproductivity) %>% 
  filter(year != model_base_years[1]) -> L2011.LaborProductivity_USA_2015

L2011.pcGDPratio_state_2040 %>% 
  group_by(state) %>% 
  mutate(time = year - lag(year, n = 1L)) %>% 
  mutate(laborproductivity = (pcGDPratio ^ (1 / time)) - 1) %>% 
  select(state, year, laborproductivity) %>%
  filter(year >= 2020) -> L2011.LaborProductivity_USA_2040

# Labor productivity post-2040 is linearly interpolated from 2040 state-level values to the 
# USA-region value in 2100
L201.LaborProductivity_SSP2 %>% 
  filter(region == "USA", year == 2100) %>%
  repeat_and_add_vector('state', gcamusa_states) %>%
  mutate(lp2100 = laborproductivity) %>%
  select(state, lp2100) %>%
  left_join(L2011.LaborProductivity_USA_2040 %>% filter(year == 2040) %>%
              mutate(lp2040 = laborproductivity) %>%
              select(state, lp2040), 
                     by = c("state")) %>%
  repeat_and_add_vector('year', model_future_years) %>%
  filter(year >= 2045) %>%
  mutate(elapsedtime = year - 2040) %>% 
  mutate(laborproductivity = lp2040 + (elapsedtime * ((lp2100 - lp2040) / (2100 - 2040)))) %>%
  select(state, year, laborproductivity)-> L2011.LaborProductivity_USA_EOC

# Combine annual growth rates into a single table
L100.gdp_mil90usd_ctry_Yh %>% 
  filter(iso == "usa" ) %>%
  repeat_and_add_vector('year', model_years) %>%
  repeat_and_add_vector('state', gcamusa_states) %>%
  select(state, year) -> states_years
states_years %>% 
  left_join(L2011.LaborProductivity_USA_2015 %>% 
                             bind_rows(L2011.LaborProductivity_USA_2040, L2011.LaborProductivity_USA_EOC), 
                           by = c("state", "year")) %>%
  filter(laborproductivity != "NA") -> L2011.LaborProductivity_USA_updated


# Add USA-region udpates
# Updated USA-region population
L201.Pop_GCAMUSA %>%
  filter(year %in% model_base_years) %>%
  bind_rows(L2011.Pop_USA_updated) %>% 
  group_by(year) %>%
  summarise(totalPop = sum(totalPop)) %>%
  ungroup() %>%
  mutate(region = "USA") %>%
  select(region, year, totalPop) -> L2011.Pop_updated_USA_national

# Updated USA-region base GDP
L2011.BaseGDP_USA %>%
  summarise(baseGDP = sum(baseGDP)) %>%
  mutate(region = "USA") %>%
  select(region, baseGDP) -> L2011.BaseGDP_updated_USA_national

# Updated USA-region labor productivity (GDP)
# First, calculate state GDP from parameters
# Combine parameters into single table
L201.Pop_GCAMUSA %>%
  filter(year %in% model_base_years) %>%
  bind_rows(L2011.Pop_USA_updated) %>% 
  left_join(L2011.LaborProductivity_USA_updated %>%
              rename(region = state), 
            by = c("region", "year")) %>%
  left_join(L2011.BaseGDP_USA %>%
              mutate(year = 1975),
            by = c("region", "year")) %>%
  rename(GDP = baseGDP) -> L2011.GDP_USA

L2011.GDP_USA %>%
  distinct(year) %>%
  filter(year != model_base_years[1]) -> L2011.GDP_USA_years
years <- unique(L2011.GDP_USA_years$year)

for (y in years) {
  
  # Calculate GDP
  L2011.GDP_USA %>%
    group_by(region) %>%
    mutate(time = year - lag(year, n = 1L),
           lag_pop = lag(totalPop, n = 1L),
           lag_GDP = lag(GDP, n = 1L)) %>%
    ungroup() %>%
    filter(year == y) %>%
    mutate(GDP = totalPop * ((1 + laborproductivity)^time) * (lag_GDP / lag_pop)) -> L2011.GDP_USA_temp
  
  # Add back into table
  L2011.GDP_USA %>%
    filter(year != y) %>%
    bind_rows(L2011.GDP_USA_temp %>%
                select(-time, -lag_pop, -lag_GDP)) %>% 
    mutate(year = as.numeric(year)) %>%
    arrange(region, year) -> L2011.GDP_USA

}

# Second, aggregate to USA-region
L2011.GDP_USA %>%
  group_by(year) %>%
  summarise(totalPop = sum(totalPop), 
            GDP = sum(GDP)) %>%
  ungroup() %>%
  mutate(region = "USA") %>%
  select(region, year, totalPop, GDP) -> L2011.GDP_updated_USA_national

# Third, calculate labor productivity growth
L2011.GDP_updated_USA_national %>%
  mutate(pcGDP = GDP / totalPop) %>%
  group_by(region) %>%
  mutate(lag_pcGDP = lag(pcGDP, n = 1L), 
         pcGDPratio = pcGDP / lag_pcGDP, 
         time = year - lag(year, n = 1L),
         laborproductivity = (pcGDPratio ^ (1 / time)) - 1) %>%
  ungroup() %>%
  select(region, year, laborproductivity) %>% 
  filter(year != model_base_years[1]) -> L2011.LaborProductivity_updated_USA_national

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2011.Pop_USA_updated, "Pop", "GCAMUSA_LEVEL2_DATA", "L2011.Pop_USA_updated", "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA_update.xml" )
write_mi_data( L2011.BaseGDP_USA, "BaseGDP", "GCAMUSA_LEVEL2_DATA", "L2011.BaseGDP_USA", "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA_update.xml" ) 
write_mi_data( L2011.LaborProductivity_USA_updated, "LaborProductivity", "GCAMUSA_LEVEL2_DATA", "L2011.LaborProductivity_USA_updated", "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA_update.xml" ) 

write_mi_data( L2011.Pop_updated_USA_national, "Pop", "GCAMUSA_LEVEL2_DATA", "L2011.Pop_updated_USA_national", "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA_update.xml" )
write_mi_data( L2011.BaseGDP_updated_USA_national, "BaseGDP", "GCAMUSA_LEVEL2_DATA", "L2011.BaseGDP_updated_USA_national", "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA_update.xml" ) 
write_mi_data( L2011.LaborProductivity_updated_USA_national, "LaborProductivity", "GCAMUSA_LEVEL2_DATA", "L2011.LaborProductivity_updated_USA_national", "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA_update.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA_update.xml", "GCAMUSA_XML_FINAL", "socioeconomics_USA_update.xml", "", xml_tag="outFile" )

logstop()
