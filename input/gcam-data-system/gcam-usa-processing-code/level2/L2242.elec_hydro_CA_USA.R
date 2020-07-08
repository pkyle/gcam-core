# This script updates hydro-electricity fixed output to match historical data for 2015 and AEO-2018 for the future. This was designed
# for CA only inititally and hence variable names continue to have "CA" label. That needs to be cleaned.
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
logstart( "L2242.elec_hydro_CA_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input to match hydro with EIA/AEO-2018" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

EIA_CA_hydro_elec_gen <- readdata( "GCAMUSA_LEVEL0_DATA","EIA_CA_hydro_elec_gen" , skip = 4 )
AEO_2018_hydro <- readdata( "GCAMUSA_LEVEL0_DATA","AEO_2018_hydro" , skip = 4 )

if(use_mult_load_segments == "TRUE") {
  L2234.StubTechFixOut_elecS_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.StubTechFixOut_elecS_USA" , skip = 4 )
  A23.elecS_tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_associations" )
} else{
  L223.StubTechFixOut_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L223.StubTechFixOut_elec_USA" , skip = 4 )
}
 
# -----------------------------------------------------------------------------
# 2. Perform computations
# NONE OF THESE METHODS WORK FOR ANY BASE YEAR OTHER THAN 2010
# copying the 2015 data backwards to 2010
# wtf

printlog( "L2242.CA_hydro_2010_EIAratio: Ratio of 2010 GCAM hydro fixedOutput to 2010 EIA net generation for hydro" )
# Isolate 2010 hydro fixedOutput
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechFixOut_elecS_USA %>%
    filter(subsector == "hydro", year == max(year)) %>%
    filter(fixedOutput!=0) -> L2242.hydro_2010_fixedOutput
} else{
  L223.StubTechFixOut_elec_USA %>%
    filter(subsector == "hydro", year == 2010) %>%
    filter(fixedOutput!=0)-> L2242.hydro_2010_fixedOutput
}

# Collapse EIA data to get annual hydro net generation for 2010, 2015
EIA_CA_hydro_elec_gen %>%
  # select(-source.key) %>%
  # gather(year, EIA, -state, -sector, -units) %>% 
  # rename(region = state) %>% 
  # mutate(year = as.numeric(gsub("X", "", year))) %>%
  filter(year %in% c(2010, 2015)) %>%
  mutate(EIA = as.numeric(EIA)) %>%
  mutate(EIA = if_else(is.na(EIA), 0, EIA)) %>% 
  mutate(EIA = EIA * conv_MWh_EJ) %>%
  mutate(units = "EJ") -> L2242.hydro_EIA

# Compute ratio of 2015 EIA to 2010 EIA
L2242.hydro_EIA %>%
  filter(year == "2010") %>%
  rename(EIA_2010 = EIA) %>%
  select(-year, -units) -> L2242.hydro_EIA_2010

L2242.hydro_EIA %>%
  filter(year == "2015") %>%
  rename(EIA_2015 = EIA) ->  L2242.hydro_EIA_2015

L2242.hydro_EIA_2015 %>%
  select(-year, -units) %>%
  left_join(L2242.hydro_EIA_2010, by = c("state")) %>%
  mutate(EIA_2015_2010_ratio = EIA_2015/EIA_2010) -> L2242.hydro_EIA_2015_2010_ratio

# Apply the ratio to GCAM 2010 fixed output to calculate 2015 fixed output 

L2242.hydro_2010_fixedOutput %>%
  left_join(L2242.hydro_EIA_2015_2010_ratio %>% select(state,EIA_2015_2010_ratio), by = c("region" = "state")) %>%
  mutate(fixedOutput_2015 = fixedOutput * EIA_2015_2010_ratio) %>%
  mutate(fixedOutput = fixedOutput_2015, year = 2015, share.weight.year = year) %>%
  select(-EIA_2015_2010_ratio, -fixedOutput_2015) -> L2242.hydro_fixedOutput_2015

# Compute ratio of AEO-2018 hydro generation relative to EIA 2015 at the national level

L2242.hydro_EIA_2015 %>%
 # group_by(year) %>%
  summarise(hydro_EIA_US_2015 = sum (EIA_2015)) %>%
  ungroup() %>%
  mutate(year = 2015, unit = "EJ", region = "USA") -> L2242.hydro_EIA_US_2015


AEO_2018_hydro %>%
  mutate(AEO = AEO * conv_TWh_EJ) %>%
  left_join(L2242.hydro_EIA_US_2015 %>% select(region, hydro_EIA_US_2015), by = c("region")) %>%
  mutate(AEO_2015_ratio = AEO/hydro_EIA_US_2015) -> AEO_2018_hydro
  
# Apply the national level ratio of AEO future years to 2015 to GCAM fixed output and create table through 2050. 
# The same ratio is assumed for all states

AEO_2018_hydro %>%
  select(region, year, AEO_2015_ratio) %>%
  repeat_and_add_vector("region", states_subregions$state) %>%
  left_join(L2242.hydro_fixedOutput_2015 %>% select(region, fixedOutput), by = c("region")) %>%
  rename(fixedOutput_2015 = fixedOutput) %>%
  filter(is.na(fixedOutput_2015) == FALSE) %>%
  mutate (fixedOutput = fixedOutput_2015 * AEO_2015_ratio) %>%
  filter(year %in% model_future_years) %>%
  mutate (supplysector = "base load generation", subsector = "hydro", stub.technology = "hydro_base", share.weight.year = year, subs.share.weight =0, tech.share.weight = 0) %>%
  select(region, supplysector, subsector, stub.technology, year, fixedOutput, share.weight.year, subs.share.weight, tech.share.weight) %>%
  bind_rows (L2242.hydro_fixedOutput_2015) %>%
  arrange(region, year) -> L2242.StubTechFixOut_hydro_USA_2050

# Copy 2050 values for the remaining years

L2242.StubTechFixOut_hydro_USA_2050 %>% 
  select(-share.weight.year) %>%
  complete(year = model_future_years, nesting(region, supplysector, subsector, stub.technology)) %>%
  group_by(region, supplysector, subsector, stub.technology) %>%
  mutate(fixedOutput = replace(fixedOutput, year > 2050, fixedOutput[year == 2050]),
         fixedOutput = if_else(is.na(fixedOutput), lead(fixedOutput), fixedOutput)) %>%
  mutate(subs.share.weight =0, tech.share.weight =0) %>%
  mutate(share.weight.year = year) %>%
  select(region, supplysector, subsector, stub.technology, year, fixedOutput, share.weight.year, subs.share.weight, tech.share.weight ) %>%
  arrange(region, year) -> L2242.StubTechFixOut_hydro_USA 

# # Compute ratio of 2010 GCAM hydro fixedOutput to EIA 2010 "Net generation for conventional hydroelectric"
# L2242.hydro_2010_fixedOutput %>% 
#   left_join(L2242.hydro_EIA, by = c("region" = "state", "year")) %>%
#   mutate(EIA_ratio_2010 = fixedOutput / EIA) %>%
#   select(-year, -fixedOutput, -share.weight.year, -EIA) -> L2242.hydro_2010_EIAratio
# 
# 
# printlog( "L2242.StubTechFixOut_hydro_USA: hydroelectricity fixedOutput (2015-2100)" )
# # Apply 2010 GCAM:EIA ratio to EIA 2015 generation data
# L2242.hydro_2010_EIAratio %>% 
#   left_join(L2242.hydro_EIA %>%
#               filter(year == 2015), 
#             by = c("region")) %>%
#   mutate(fixedOutput = EIA * EIA_ratio_2010) %>%
#   select(-EIA_ratio_2010, -year, -EIA) -> L2242.CA_hydro_2015_fixedOutput
# 
# # Build StubTechFixOut table
# L2242.CA_hydro_2015_fixedOutput %>% 
#   repeat_and_add_vector('year', model_future_years) %>%
#   mutate(share.weight.year = year) %>%
#   select(region, supplysector, subsector, stub.technology, year, fixedOutput, 
#          share.weight.year, subs.share.weight, tech.share.weight) -> L2242.StubTechFixOut_hydro_CA_USA

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2242.StubTechFixOut_hydro_USA, "StubTechFixOut", "GCAMUSA_LEVEL2_DATA", "L2242.StubTechFixOut_hydro_USA", "GCAMUSA_XML_BATCH", "batch_elec_hydro_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_hydro_USA.xml", "GCAMUSA_XML_FINAL", "elec_hydro_USA.xml", "", xml_tag="outFile" )

logstop()
