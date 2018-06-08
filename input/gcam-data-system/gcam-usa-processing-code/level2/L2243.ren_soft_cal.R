# This script creates tables to set up state-level fixed output for renewables (wind and solar generation) in 
# GCAM-USA with multiple load segments to "soft calibrate" with historical generation in 2015 based on EIA data
# Need to expand to have this feature for single electricity market version.

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
logstart( "L2243.ren_soft_cal.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "L2243.ren_soft_cal.R: Model input for soft calibrating renewables in GCAM-USA" )

# Authors: Gokul Iyer
# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )

eia_ren <- readdata( "GCAMUSA_LEVEL0_DATA","EIA_net_gen_renewables" , skip = 4 )
eia_tech_mapping <- readdata( "GCAMUSA_MAPPINGS", "EIA_tech_mapping" )

StubTechProd_USA <- readdata( "GCAMUSA_LEVEL2_DATA","L223.StubTechProd_elec_USA" , skip = 4 )

L223.GlobalIntTechEff_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechEff_elec", skip = 4 )
L223.StubTechMarket_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechMarket_elec_USA", skip = 4 )
L2238.StubTechEff_PV_USA_reeds <- readdata( "GCAMUSA_LEVEL2_DATA", "L2238.StubTechEffFlag_PV_USA_reeds", skip = 4 )
L2239.StubTechEff_CSP_USA_reeds <- readdata( "GCAMUSA_LEVEL2_DATA", "L2239.StubTechEffFlag_CSP_USA_reeds", skip = 4 )

L223.GlobalIntTechBackup_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechBackup_elec", skip = 4 )
L223.StubTechMarket_backup_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechMarket_backup_USA", skip = 4 )



if(use_mult_load_segments == "TRUE") {
  A23.elec_tech_ren_soft_cal <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elec_tech_associations_ren_soft_cal", skip = 1 ) %>%
    filter(grepl("generation", supplysector) | supplysector == "elect_td_bld")
} else{
  A23.elec_tech_ren_soft_cal <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elec_tech_associations_ren_soft_cal", skip =1 ) %>%
    filter(grepl("electricity", supplysector) | supplysector == "elect_td_bld")
}
  
 
# -----------------------------------------------------------------------------
# 2. Perform computations

# Preparing the data

eia_ren %>%
  select(-source.key) %>%
  filter(!is.na(units)) %>%
  separate(description, c("state", "fuel"), sep = " : ", remove = TRUE) %>%
  filter(!is.na(fuel)) %>%
  gather(year, value, -state, -fuel, -units) %>%
  group_by(state, units, year, value) %>%
  distinct(fuel) %>%
  ungroup() %>%
  mutate(value = as.numeric(value),
         value = if_else(is.na(value), 0, value),
         units = "GWh") %>%
  mutate(year = gsub("X","",year)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(state = gsub("Of","of", state)) %>% # Frustratingly, EIA uses "Of" for District of Columbia while we use "of"
  left_join(states_subregions %>% select(state, state_name), by = c("state" = "state_name")) %>%
  select(-state)%>%
  rename (state = state.y) %>%
  left_join(eia_tech_mapping, by = "fuel") %>%
  filter(year == 2015) %>%
  mutate(value = value *conv_GWh_EJ) %>%
  select(state, stub.technology = gcam_tech, year, gen2015 = value, -units) -> L2243.ren_gen_2015


# Using 2010 renewable generation data from GCAM-USA data-system, calculate the difference between 2015 and
# 2010 generation by technology.In states with positive differences, we will then read in the differences as 
# fixed output for "fake" technologies in corresponding subsectors. We'll keep the fixed outputs
# constant over time. The fixed outputs would then represent calibrating the effect of whatever factors 
# contributed to increased wind and solar deployment in 2015 relative to 2010.This is a simple approach to
# mimic the effects of an RPS. 

  StubTechProd_USA %>%
    filter(year==2010) %>%
    select(region, stub.technology, gen2010 = calOutputValue) %>%
    filter(stub.technology %in% unique(L2243.ren_gen_2015$stub.technology)) %>%
    right_join(L2243.ren_gen_2015, by = c("region" = "state", "stub.technology")) %>% #Need a right join here because LHS does not have rooftop_pv but RHS does
    replace(is.na(.), 0) %>%
    mutate(fixedOutput = gen2015 - gen2010) %>%
    mutate(fixedOutput = if_else(fixedOutput<0, 0, fixedOutput)) %>%
    select(-gen2010, -gen2015) %>%
    left_join(A23.elec_tech_ren_soft_cal, by = c("stub.technology" = "technology")) %>%
    select(region, supplysector, subsector, stub.technology = new.technology, year, fixedOutput) %>%
    repeat_and_add_vector("year", model_future_years) %>%
    mutate(share.weight.year = model_base_years[1], subs.share.weight = 0, tech.share.weight =0) -> L2243.StubTechFixOut_ren_soft_cal_2015 # Note we're reading in share-weight year as 1975 to not over-write subsector shareweights that are read in in the electricity sector.
    
# Create table to read in share-weights in global technology database
  A23.elec_tech_ren_soft_cal %>%
    select(sector.name = supplysector, subsector, intermittent.technology = new.technology)%>%
    mutate(year = model_years[1], share.weight = 0) %>%
    repeat_and_add_vector("year",model_years) -> L2243.GlobalTechShrwt_ren_soft_cal_2015

# # Create table to read in resources and back-up. Note that this won't affect electricity price calculations in 2015. 
# #  But will matter for resource cost and back up cost calculations in future periods. 
# # For example, in 2020, the onshore wind resource cost for 2020 vintages will be based off of 
# # total onshore wind generation in that period including the fixed output that we read in. Also, back-up
# # cost calculations will be based off of intermittent renewable share including fixed outputs.
#   
#   A23.elec_tech_ren_soft_cal %>%
#     left_join(L223.GlobalIntTechEff_elec, by = c("technology" = "intermittent.technology")) %>%
#     select(-technology, -sector.name, -subsector.name) %>%
#     rename(intermittent.technology = new.technology) -> L2243.GlobalIntTechEff_ren_soft_cal_2015
#     
# # Read in resource markets for states
#   
#   A23.elec_tech_ren_soft_cal %>%
#     left_join(L223.StubTechMarket_elec_USA, by = c("technology" = "stub.technology")) %>%
#     mutate(efficiency = 1) %>%
#     select(region, supplysector = supplysector.x, subsector = subsector.x, stub.technology = new.technology, year, minicam.energy.input, efficiency, market.name) -> L2243.StubTechEff_ren_soft_cal_2015
# 
# # Over-write PV and CSP resource curves for states that have them
#   L2243.StubTechEff_ren_soft_cal_2015 %>%
#     left_join(L2238.StubTechEff_PV_USA_reeds, by = c("region", "supplysector", "subsector")) %>%
#     replace_na(list(minicam.energy.input.y =0), minicam.energy.input.y) %>%
#     mutate(minicam.energy.input.x = if_else(minicam.energy.input.y == "PV_resource", "PV_resource", minicam.energy.input.x)) %>%
#     select(region, supplysector, subsector, stub.technology = stub.technology.x, year=year.x,minicam.energy.input= minicam.energy.input.x, efficiency = efficiency.x, market.name = market.name.x) %>%
#     arrange(region, year) -> L2243.StubTechEff_ren_soft_cal_2015
# 
#   L2243.StubTechEff_ren_soft_cal_2015 %>%
#     left_join(L2239.StubTechEff_CSP_USA_reeds, by = c("region", "supplysector", "subsector")) %>%
#     replace_na(list(minicam.energy.input.y =0), minicam.energy.input.y) %>%
#     mutate(minicam.energy.input.x = if_else(minicam.energy.input.y == "CSP_resource", "CSP_resource", minicam.energy.input.x)) %>%
#     select(region, supplysector, subsector, stub.technology = stub.technology.x, year=year.x,minicam.energy.input= minicam.energy.input.x, efficiency = efficiency.x, market.name = market.name.x) %>%
#     arrange(region, year) -> L2243.StubTechEff_ren_soft_cal_2015
#   
#   
# # Create table to read in back-up parameters
#   A23.elec_tech_ren_soft_cal %>%
#     left_join(L223.GlobalIntTechBackup_elec, by= c("subsector" = "subsector.name", "technology")) %>%
#     select(-technology, - sector.name) %>%
#     rename(technology = new.technology) -> L2243.GlobalIntTechBackup_ren_soft_cal_2015
#   
#   L223.StubTechMarket_backup_USA %>%
#     left_join (A23.elec_tech_ren_soft_cal, by = c("stub.technology" = "technology"))  %>%
#     filter(!is.na(new.technology)) %>%
#     select(-supplysector.x, -subsector.x, - stub.technology) %>%
#     select(region, supplysector = supplysector.y, subsector = subsector.y, stub.technology = new.technology, year, minicam.energy.input, market.name) -> L2243.StubTechMarket_backup_ren_soft_cal_2015
#   

# Create table to read in Primary renewable Keyword for the new technologies
  L2243.GlobalTechShrwt_ren_soft_cal_2015 %>%
    select(-share.weight) %>%
    mutate(primary.renewable = paste(subsector, "-elect")) %>%
    mutate(primary.renewable = if_else(subsector == "rooftop_pv", "solar-elect", primary.renewable) ) -> L2243.PrimaryRenewKeyword_ren_soft_cal_2015
  
 
# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2243.StubTechFixOut_ren_soft_cal_2015, "StubTechFixOut", "GCAMUSA_LEVEL2_DATA", "L2243.StubTechFixOut_ren_soft_cal_2015", "GCAMUSA_XML_BATCH", "batch_ren_soft_cal_USA.xml" )
write_mi_data( L2243.GlobalTechShrwt_ren_soft_cal_2015, "GlobalTechShrwt", "GCAMUSA_LEVEL2_DATA", "L2243.GlobalTechShrwt_ren_soft_cal_2015", "GCAMUSA_XML_BATCH", "batch_ren_soft_cal_USA.xml" )
# write_mi_data( L2243.GlobalIntTechEff_ren_soft_cal_2015, "GlobalIntTechEff", "GCAMUSA_LEVEL2_DATA", "L2243.GlobalIntTechEff_ren_soft_cal_2015", "GCAMUSA_XML_BATCH", "batch_ren_soft_cal_USA.xml" )
# write_mi_data( L2243.StubTechEff_ren_soft_cal_2015, "StubTechEff", "GCAMUSA_LEVEL2_DATA", "L2243.StubTechEff_ren_soft_cal_2015", "GCAMUSA_XML_BATCH", "batch_ren_soft_cal_USA.xml" )
# write_mi_data( L2243.GlobalIntTechBackup_ren_soft_cal_2015, "GlobalIntTechBackup", "GCAMUSA_LEVEL2_DATA", "L2243.GlobalIntTechBackup_ren_soft_cal_2015", "GCAMUSA_XML_BATCH", "batch_ren_soft_cal_USA.xml" )
# write_mi_data( L2243.StubTechMarket_backup_ren_soft_cal_2015, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L2243.StubTechMarket_backup_ren_soft_cal_2015", "GCAMUSA_XML_BATCH", "batch_ren_soft_cal_USA.xml" )
write_mi_data( L2243.PrimaryRenewKeyword_ren_soft_cal_2015, "PrimaryRenewKeyword", "GCAMUSA_LEVEL2_DATA", "L2243.PrimaryRenewKeyword_ren_soft_cal_2015", "GCAMUSA_XML_BATCH", "batch_ren_soft_cal_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_ren_soft_cal_USA.xml", "GCAMUSA_XML_FINAL", "ren_soft_cal_USA.xml", "", xml_tag="outFile" )

logstop()
