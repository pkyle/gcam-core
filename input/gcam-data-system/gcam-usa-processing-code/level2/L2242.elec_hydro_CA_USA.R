# This script updates California's hydro-electricity fixed output.
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
printlog( "Model input for USA regional natural gas to demand USA natural gas prouction" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

A23.elecS_tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_associations" )
EIA_CA_hydro_elec_gen <- readdata( "GCAMUSA_LEVEL0_DATA","EIA_CA_hydro_elec_gen" , skip = 4 )

if(use_mult_load_segments == "TRUE") {
  L2234.StubTechFixOut_elecS_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L2234.StubTechFixOut_elecS_USA" , skip = 4 )
} else{
  L223.StubTechFixOut_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L223.StubTechFixOut_elec_USA" , skip = 4 )
}
 
# -----------------------------------------------------------------------------
# 2. Perform computations

printlog( "L2242.CA_hydro_2010_EIAratio: Ratio of 2010 GCAM hydro fixedOutput to 2010 EIA net generation for hydro" )
# Isolate CA 2010 hydro fixedOutput
if(use_mult_load_segments == "TRUE") {
  L2234.StubTechFixOut_elecS_USA %>%
    filter(region == "CA", subsector == "hydro", year == 2010) -> L2242.CA_hydro_2010_fixedOutput
} else{
  L223.StubTechFixOut_elec_USA %>%
    filter(region == "CA", subsector == "hydro", year == 2010) -> L2242.CA_hydro_2010_fixedOutput
}

# Collapse EIA sectors to get annual CA hydro net generation for 2010, 2015
EIA_CA_hydro_elec_gen %>%
  select(-source.key) %>%
  gather(year, EIA, -state, -sector, -units) %>% 
  rename(region = state) %>% 
  mutate(year = as.numeric(gsub("X", "", year))) %>%
  filter(year %in% c(2010, 2015)) %>%
  mutate(EIA = as.numeric(EIA)) %>%
  mutate(EIA = if_else(is.na(EIA), 0, EIA)) %>% 
  mutate(EIA = EIA * conv_GWh_EJ) %>%
  mutate(units = "EJ") %>%
  group_by(region, year) %>%
  summarise(EIA = sum(EIA)) -> L2242.CA_hydro_EIA

# Compute ratio of 2010 GCAM hydro fixedOutput to EIA 2010 "Net generation for conventional hydroelectric"
L2242.CA_hydro_2010_fixedOutput %>% 
  left_join(L2242.CA_hydro_EIA, by = c("region", "year")) %>%
  mutate(EIA_ratio_2010 = fixedOutput / EIA) %>%
  select(-year, -fixedOutput, -share.weight.year, -EIA) -> L2242.CA_hydro_2010_EIAratio


printlog( "L2242.StubTechFixOut_hydro_CA_USA: CA hydroelectricity fixedOutput (2015-2100)" )
# Apply 2010 GCAM:EIA ratio to EIA 2015 generation data
L2242.CA_hydro_2010_EIAratio %>% 
  left_join(L2242.CA_hydro_EIA %>%
              filter(year == 2015), 
            by = c("region")) %>%
  mutate(fixedOutput = EIA * EIA_ratio_2010) %>%
  select(-EIA_ratio_2010, -year, -EIA) -> L2242.CA_hydro_2015_fixedOutput

# Build StubTechFixOut table
L2242.CA_hydro_2015_fixedOutput %>% 
  repeat_and_add_vector('year', model_future_years) %>%
  mutate(share.weight.year = year) %>%
  select(region, supplysector, subsector, stub.technology, year, fixedOutput, 
         share.weight.year, subs.share.weight, tech.share.weight) -> L2242.StubTechFixOut_hydro_CA_USA

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2242.StubTechFixOut_hydro_CA_USA, "StubTechFixOut", "GCAMUSA_LEVEL2_DATA", "L2242.StubTechFixOut_hydro_CA_USA", "GCAMUSA_XML_BATCH", "batch_elec_hydro_CA_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_hydro_CA_USA.xml", "GCAMUSA_XML_FINAL", "elec_hydro_CA_USA.xml", "", xml_tag="outFile" )

logstop()
