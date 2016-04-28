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

logstart( "L273.en_ghg_emissions_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))


printlog( "GHG emissions parameters for energy technologies in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
A27.tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A27.tech_associations" )
L123.in_EJ_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.in_EJ_R_elec_F_Yh" )
L123.out_EJ_state_ownuse_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.out_EJ_state_ownuse_elec" )
L1231.in_EJ_state_elec_F_tech <- readdata( "GCAMUSA_LEVEL1_DATA", "L1231.in_EJ_state_elec_F_tech" )
L1322.in_EJ_state_Fert_Yh <- readdata( "GCAMUSA_LEVEL1_DATA", "L1322.in_EJ_state_Fert_Yh" )
L201.en_ghg_emissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L201.en_ghg_emissions", skip = 4)
L201.ghg_res <- readdata( "EMISSIONS_LEVEL2_DATA", "L201.ghg_res", skip = 4)
L241.nonco2_tech_coeff <- readdata( "EMISSIONS_LEVEL2_DATA", "L241.nonco2_tech_coeff", skip = 4)
L241.hfc_all <- readdata( "EMISSIONS_LEVEL2_DATA", "L241.hfc_all", skip = 4)
L241.pfc_all <- readdata( "EMISSIONS_LEVEL2_DATA", "L241.pfc_all", skip = 4)
L252.ResMAC_fos <- readdata( "EMISSIONS_LEVEL2_DATA", "L252.ResMAC_fos", skip = 4)
L252.MAC_higwp <- readdata( "EMISSIONS_LEVEL2_DATA", "L252.MAC_higwp", skip = 4)
L222.StubTech_en_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L222.StubTech_en_USA", skip = 4 )
L223.StubTech_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTech_elec_USA", skip = 4 )
L232.StubTechCalInput_indenergy_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L232.StubTechCalInput_indenergy_USA", skip = 4 )
L244.StubTechCalInput_bld <- readdata( "GCAMUSA_LEVEL2_DATA", "L244.StubTechCalInput_bld", skip = 4 )
L244.GlobalTechEff_bld <- readdata( "GCAMUSA_LEVEL2_DATA", "L244.GlobalTechEff_bld", skip = 4 )

# -----------------------------------------------------------------------------

# 2. Build tables for CSVs
# 2a. Resource emission coefficients
printlog( "L273.res_ghg_tech_coeff_USA: GHG emissions for energy resources in all U.S. states" )
#For resources all that needs to be done is to write the USA coefficients for every state
L273.res_ghg_tech_coeff_USA <- L201.ghg_res %>%
  filter(region == "USA") %>%
  select(-region) %>%
  repeat_and_add_vector("region",states_subregions$state) %>%
  select(region,depresource,Non.CO2,emiss.coef) 

# 2b. Input Emissions coefficients
printlog( "L273.en_ghg_tech_coeff_USA: GHG emissions coefficients for energy technologies in U.S. states" )
#Write the USA coefficients for every state. 
###NOTE: filtering out H2 production emissions for now because energy is not available on state level
#Refining first:
L241.ref_ghg_tech_coeff_USA <- L241.nonco2_tech_coeff %>%
  filter(region == "USA" & Non.CO2 %in% c("N2O","CH4") & supplysector == "refining")

#Match the refining emission factors to the corresponding technologies in the states. Matching on subsector
#and stub technology because the names of the sectors are different
L273.ref_ghg_tech_coeff_USA <- L222.StubTech_en_USA %>%
  #filter(subsector %in% L241.ref_ghg_tech_coeff_USA$subsector) %>%
  #rename(stub.technology = technology) %>%
  repeat_and_add_vector("year",unique(L241.ref_ghg_tech_coeff_USA$year)) %>%
  repeat_and_add_vector("Non.CO2",unique(L241.ref_ghg_tech_coeff_USA$Non.CO2)) %>%
  mutate(emiss.coeff = L241.ref_ghg_tech_coeff_USA$emiss.coeff[match(paste(subsector,stub.technology,year,Non.CO2),
                                                                     vecpaste(L241.ref_ghg_tech_coeff_USA[c("subsector","stub.technology","year","Non.CO2")]))]) %>%
  #MISSING VALUES: various tech/year combinations. OK to omit
  na.omit()

#Write electricity emission coefficients to states for technologies shared by GCAMUSA and emissions data
L241.elc_ghg_tech_coeff_USA <- L241.nonco2_tech_coeff %>%
  filter(region == "USA" & Non.CO2 %in% c("N2O","CH4") & supplysector == "electricity") 

L273.elc_ghg_tech_coeff_USA <- L223.StubTech_elec_USA %>%
  filter(paste(subsector,stub.technology) %in% vecpaste(L241.elc_ghg_tech_coeff_USA[c("subsector","stub.technology")])) %>%
  repeat_and_add_vector("year",unique(L241.elc_ghg_tech_coeff_USA$year)) %>%
  repeat_and_add_vector("Non.CO2",unique(L241.elc_ghg_tech_coeff_USA$Non.CO2)) %>%
  mutate(emiss.coeff = L241.elc_ghg_tech_coeff_USA$emiss.coeff[match(paste(stub.technology,year,Non.CO2),
                                                                     vecpaste(L241.elc_ghg_tech_coeff_USA[c("stub.technology","year","Non.CO2")]))])

#Subset the technologies that compete in the coal-gas dispatcher, and assign their emissions
disp_ghg_tech_coeff_USA <- L273.elc_ghg_tech_coeff_USA %>%
  filter(stub.technology %in% A27.tech_associations$elec_technology) %>%
  mutate(supplysector = A27.tech_associations$supplysector[match(stub.technology,A27.tech_associations$elec_technology)]) %>%
  mutate(subsector = A27.tech_associations$subsector[match(stub.technology,A27.tech_associations$elec_technology)]) %>%
  mutate(stub.technology = A27.tech_associations$technology[match(stub.technology,A27.tech_associations$elec_technology)]) 

#Up to now, we've just written the tech coefficients that were already present in the emissions data for
#the dispatcher. But since there is nothing written out for the final base year we will miss emissions in this
#year.So will have to use input emissions and fuel input on the USA level to come up with a technology coefficient
#for the dispatcher technologies, and write this out for the first year that the dispatcher becomes available.
L201.disp_ghg_emissions_USA <- L201.en_ghg_emissions %>%
  filter(stub.technology %in% A27.tech_associations$elec_technology & 
           year == max(year[year %in% model_base_years]) & region == "USA")

#Grab the relevant fuel inputs to electricity from the energy data
L123.in_EJ_USA_elec_F_Ydisp <- L123.in_EJ_R_elec_F_Yh %>%
  gather(year,fuel_input,-GCAM_region_ID,-sector,-fuel) %>%
  mutate(year = as.numeric(substr(year,2,5))) %>%
  filter(GCAM_region_ID == 1 & year %in% L201.disp_ghg_emissions_USA$year & 
           fuel %in% L201.disp_ghg_emissions_USA$subsector)

#Compute technology coefficients, write them to states, use them in the year that the dispatcher is available
add_disp_ghg_tech_coeff_USA <- L201.disp_ghg_emissions_USA %>%
  mutate(emiss.coeff = input.emissions / L123.in_EJ_USA_elec_F_Ydisp$fuel_input[match(subsector,L123.in_EJ_USA_elec_F_Ydisp$fuel)]) %>%
  mutate(year = final_model_base_year, 
         supplysector = A27.tech_associations$supplysector[match(stub.technology,A27.tech_associations$elec_technology)],
         subsector = A27.tech_associations$subsector[match(stub.technology,A27.tech_associations$elec_technology)],
         stub.technology = A27.tech_associations$technology[match(stub.technology,A27.tech_associations$elec_technology)]) %>%
  select(-region) %>%
  repeat_and_add_vector("region",states_subregions$state)

#Select relevant columns
add_disp_ghg_tech_coeff_USA <- add_disp_ghg_tech_coeff_USA[ names(disp_ghg_tech_coeff_USA) ]

#Bind the dispatcher technology coefficients into one table
L273.disp_ghg_tech_coeff_USA <- bind_rows(add_disp_ghg_tech_coeff_USA, disp_ghg_tech_coeff_USA)

#Bind the dispatcher rows to the electricity data frame
L273.elcdisp_ghg_tech_coeff_USA <- bind_rows(L273.elc_ghg_tech_coeff_USA,L273.disp_ghg_tech_coeff_USA)

#Bind rows containing refining and electricity emission coefficients into one table, and organize
L273.en_ghg_tech_coeff_USA <- bind_rows(L273.ref_ghg_tech_coeff_USA,L273.elcdisp_ghg_tech_coeff_USA) %>%
  mutate(emiss.coeff = round(emiss.coeff,digits_emissions)) %>%
  arrange(region,supplysector,subsector,stub.technology,year,Non.CO2)

# 2c. Input Emissions 
printlog( "L273.en_ghg_emissions_USA: Calibrated input emissions of N2O and CH4 by U.S. state" )
#Filter the emissions data into USA, and transportation is calibrated elsewhere 
en_ghg_emissions_USA <- L201.en_ghg_emissions %>% 
  filter(region == "USA" & !grepl("trn",supplysector)) %>%
  spread(Non.CO2, input.emissions) %>%
###NOTE: emissions from coal use in commercial buildings "other" category does not have an equivalent representation 
#in the fifty state data. For now move these emissions over to comm heating
  mutate(supplysector = ifelse(grepl("comm",supplysector) & subsector == "coal","comm heating",supplysector))

#Organize the state fuel input data
#Electricity
elec_fuel_input_state <- L1231.in_EJ_state_elec_F_tech %>%
  gather(year,fuel_input,-state,-sector,-fuel,-technology) %>%
  mutate(year = as.numeric(substr(year,2,5)),sector = "electricity") %>%
  filter(year %in% en_ghg_emissions_USA$year & technology %in% en_ghg_emissions_USA$stub.technology)

#Fertilizer
fert_fuel_input_state <- L1322.in_EJ_state_Fert_Yh %>%
  gather(year,fuel_input,-state,-sector,-fuel) %>%
  mutate(year = as.numeric(substr(year,2,5)), technology = fuel) %>%
  filter(year %in% en_ghg_emissions_USA$year)

#Industry
ind_fuel_input_state <- L232.StubTechCalInput_indenergy_USA %>%
  filter(year %in% en_ghg_emissions_USA$year &
         paste(supplysector,subsector,stub.technology) %in% 
           vecpaste(en_ghg_emissions_USA[c("supplysector","subsector","stub.technology")])) %>%
  select(region,supplysector,subsector,stub.technology,year,calibrated.value) %>%
  rename(fuel_input = calibrated.value, sector = supplysector, fuel = subsector,
         technology = stub.technology, state = region)

#Bind the fuel input tables into one
fuel_input_state <- bind_rows(elec_fuel_input_state,fert_fuel_input_state,ind_fuel_input_state)  

#Create aggregate fuel input table
fuel_input_USA <- fuel_input_state %>%
  group_by(sector,fuel,technology,year) %>% summarise(fuel_input = sum(fuel_input))

#Compute state shares for each category in the fuel input table
en_ghg_emissions_state <- fuel_input_state %>%
  mutate(fuel_input_share = fuel_input / fuel_input_USA$fuel_input[match(paste(sector,fuel,technology,year),
                                                                         vecpaste(fuel_input_USA[c("sector","fuel","technology","year")]))]) %>%
  #Share out CH4 and N2O emissions by state based on the fuel input shares
  mutate(CH4 = fuel_input_share * en_ghg_emissions_USA$CH4[match(paste(sector,fuel,technology,year),
                                                                 vecpaste(en_ghg_emissions_USA[c("supplysector","subsector","stub.technology","year")]))]) %>%
  mutate(N2O = fuel_input_share * en_ghg_emissions_USA$N2O[match(paste(sector,fuel,technology,year),
                                                                 vecpaste(en_ghg_emissions_USA[c("supplysector","subsector","stub.technology","year")]))]) %>%
  rename(region = state, supplysector = sector, subsector = fuel, stub.technology = technology) %>%
  select(region,supplysector,subsector,stub.technology,year,CH4,N2O)

#Buildings: First subset the heating and cooling demands
bld_fuel_input_state <- L244.StubTechCalInput_bld %>%
  filter(year %in% en_ghg_emissions_USA$year & subsector %in% en_ghg_emissions_USA$subsector) %>%
  #Add a sector column to match with the emissions data
  mutate(sector = ifelse(supplysector %in% c("comm heating","comm cooling","resid heating","resid cooling"),
                         supplysector,ifelse(grepl("comm",supplysector),"comm others","resid others"))) %>%
  select(region,sector,supplysector,subsector,stub.technology,year,calibrated.value)

#Create aggregate table for total nation fuel inputs by emissions category
bld_fuel_input_agg <- bld_fuel_input_state %>%
  group_by(sector,subsector,year) %>% summarise(calibrated.value = sum(calibrated.value))
  
#Compute shares of national and sector total for each fuel input technology
bld_ghg_emissions_state <- bld_fuel_input_state %>%
  mutate(share = calibrated.value / bld_fuel_input_agg$calibrated.value[match(paste(sector,subsector,year),
                                                                              vecpaste(bld_fuel_input_agg[c("sector","subsector","year")]))]) %>%
  #Match on the emissions by state and sector using the state/technology shares
  mutate(CH4 = share * en_ghg_emissions_USA$CH4[match(paste(sector,subsector,year),
                                              vecpaste(en_ghg_emissions_USA[c("supplysector","subsector","year")]))],
         N2O = share * en_ghg_emissions_USA$N2O[match(paste(sector,subsector,year),
                                              vecpaste(en_ghg_emissions_USA[c("supplysector","subsector","year")]))]) %>%
  select(region,supplysector,subsector,stub.technology,year,CH4,N2O)


#Combine the buildings and other energy input emissions tables and convert to long format
L273.en_ghg_emissions_USA <- bind_rows(en_ghg_emissions_state,bld_ghg_emissions_state) %>%
  gather(Non.CO2,input.emissions,-region,-supplysector,-subsector,-stub.technology,-year,convert=TRUE) %>%
  arrange(region,supplysector,subsector,stub.technology,Non.CO2,year)

#Format for csv file
L273.en_ghg_emissions_USA <- L273.en_ghg_emissions_USA[ c( names_StubTechYr, "Non.CO2","input.emissions" ) ]
L273.en_ghg_emissions_USA$input.emissions <- round( L273.en_ghg_emissions_USA$input.emissions, digits_emissions )


# 2d. Output emissions
printlog( "L273.out_ghg_emissions_USA: Output emissions of GHGs in U.S. states" )
###NOTE: This table will contain PFC and HFC emissions for now, from building cooling and electricity own use.
###the energy data is available at the state level and will be used to share out the emissions
L241.hfc_pfc_USA <- bind_rows(L241.hfc_all,L241.pfc_all) %>%
  filter(region == "USA")

#Emissions from electricity own use
L241.hfc_pfc_elec_ownuse <- filter(L241.hfc_pfc_USA, supplysector == "electricity_net_ownuse")

#Emissions from building cooling
L241.hfc_pfc_bld <- filter(L241.hfc_pfc_USA, supplysector %in% c("resid cooling","comm cooling"))
  
#Electricity net own use output by state
L123.out_EJ_state_ownuse_elec.long <- L123.out_EJ_state_ownuse_elec %>%
  #Convert to long format
  gather(year,value,-state,-sector,-fuel) %>%
  mutate(year = as.numeric(substr(year,2,5))) %>%
  #Subset relevant years
  filter(year %in% L241.hfc_pfc_USA$year) %>%
  mutate(supplysector = "electricity domestic supply") %>%
  mutate(subsector = "electricity_net_ownuse") %>%
  mutate(stub.technology = subsector)

#Compute aggregate USA electricity own use output
L123.out_EJ_ownuse_elec_agg <- L123.out_EJ_state_ownuse_elec.long %>%
  group_by(sector,fuel,year) %>%
  summarise(value = sum(value))

#Match state shares onto elec own use table 
L273.out_ghg_emissions_elec_ownuse <- L123.out_EJ_state_ownuse_elec.long %>%
  mutate(share = value / L123.out_EJ_ownuse_elec_agg$value[match(year,L123.out_EJ_ownuse_elec_agg$year)]) %>%
  #Add column with pollutant identifier
  repeat_and_add_vector("Non.CO2",unique(L241.hfc_pfc_elec_ownuse$Non.CO2)) %>%
  #Match on output emissions, sharing out to states
  mutate(output.emissions = share * L241.hfc_pfc_elec_ownuse$input.emissions[match(paste(year,Non.CO2),
                                                                                   vecpaste(L241.hfc_pfc_elec_ownuse[c("year","Non.CO2")]))]) %>%
  select(state,supplysector,subsector,stub.technology,year,Non.CO2,output.emissions) %>%
  #The electricity ownuse emissions must be written out at the grid region level
  mutate(grid_region = states_subregions$grid_region[match(state,states_subregions$state)]) %>%
  group_by(grid_region,supplysector,subsector,stub.technology,year,Non.CO2) %>%
  summarise(output.emissions = sum(output.emissions)) %>%
  rename(region = grid_region)

#To compute building service output, multiply the building energy use by efficiency
L244.output_bld_cool <- L244.StubTechCalInput_bld %>%
  filter(paste(supplysector,subsector,year) %in% vecpaste(L241.hfc_pfc_bld[c("supplysector","subsector","year")])) %>%
  mutate(service_output = calibrated.value * L244.GlobalTechEff_bld$efficiency[match(
    paste(supplysector,subsector,stub.technology,year),
    vecpaste(L244.GlobalTechEff_bld[c("sector.name","subsector.name","technology","year")]))]) %>%
  select(region,supplysector,subsector,stub.technology,year,service_output)

#Compute aggregate USA building cooling service output for each subsector
L244.output_bld_cool_agg <- L244.output_bld_cool %>%
  group_by(supplysector,subsector,year) %>%
  summarise(service_output = sum(service_output))

#Match shares onto service output table
L273.out_ghg_emissions_bld_cool <- L244.output_bld_cool %>%
  mutate(share = service_output / L244.output_bld_cool_agg$service_output[match(
    paste(supplysector,subsector,year),vecpaste(L244.output_bld_cool_agg[c("supplysector","subsector","year")]))]) %>%
  #Add column identifying pollutant
  repeat_and_add_vector("Non.CO2",unique(L241.hfc_pfc_bld$Non.CO2)) %>%
  #Match on output emissions, sharing out to states and technologies
  mutate(output.emissions = share * L241.hfc_pfc_bld$input.emissions[
    match(paste(supplysector,subsector,year,Non.CO2),
          vecpaste(L241.hfc_pfc_bld[c("supplysector","subsector","year","Non.CO2")]))]) %>%
  select(region,supplysector,subsector,stub.technology,year,Non.CO2,output.emissions)

#Combine output emissions into one table and organize
L273.out_ghg_emissions_USA <- bind_rows(L273.out_ghg_emissions_elec_ownuse,L273.out_ghg_emissions_bld_cool) %>%
  arrange(Non.CO2,supplysector)

# 2e. MAC curves
printlog( "L273.ResMAC_fos_USA: fossil resource MAC curves for all U.S. states" )
#The MAC curves will be identical to those for the USA
L273.ResMAC_fos_USA <- L252.ResMAC_fos %>%
  filter(region == "USA") %>%
  select(-region) %>%
  repeat_and_add_vector("region",states_subregions$state) %>%
  select(region,depresource,Non.CO2,mac.control,tax,mac.reduction)

printlog( "L273.MAC_higwp_USA: abatement from HFCs and PFCs in all U.S. states" )
#The MAC curves will be identical to those for the USA. 
L252.MAC_higwp_USA <- filter(L252.MAC_higwp, region == "USA")

#For building cooling, have to add more specific technologies on the state level. This means that both 
#of the building cooling techs in GCAM-USA will have identical MAC curves
L273.MAC_higwp_bld_cool <- L252.MAC_higwp_USA %>%
  filter(supplysector %in% L273.out_ghg_emissions_bld_cool$supplysector) %>%
  repeat_and_add_vector("state_technology",unique(L273.out_ghg_emissions_bld_cool$stub.technology)) %>%
  #Now replace the existing stub technology column with the one containing the state-level techs
  select(-stub.technology) %>%
  rename(stub.technology = state_technology) %>%
  select(-region) %>%
  repeat_and_add_vector("region",states_subregions$state)

#Electricity own use will be written out at the grid region level
L273.MAC_higwp_elec_ownuse <- filter(L252.MAC_higwp_USA, supplysector %in% L273.out_ghg_emissions_elec_ownuse$subsector) %>%
  mutate(supplysector = "electricity domestic supply") %>%
  select(-region) %>%
  repeat_and_add_vector("region",states_subregions$grid_region)

#Bind the MAC curve tables and organize
L273.MAC_higwp_USA <- bind_rows(L273.MAC_higwp_bld_cool,L273.MAC_higwp_elec_ownuse)
L273.MAC_higwp_USA <- L273.MAC_higwp_USA[names(L252.MAC_higwp)]
  
#Check for missing values
stopifnot(!any(is.na(L273.res_ghg_tech_coeff_USA)))
stopifnot(!any(is.na(L273.en_ghg_tech_coeff_USA)))
stopifnot(!any(is.na(L273.en_ghg_emissions_USA)))
stopifnot(!any(is.na(L273.out_ghg_emissions_USA)))
stopifnot(!any(is.na(L273.ResMAC_fos_USA)))
stopifnot(!any(is.na(L273.MAC_higwp_USA)))

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
#write_mi_data( L273.res_ghg_tech_coeff_USA, "ResEmissCoef", "GCAMUSA_LEVEL2_DATA", "L273.res_ghg_tech_coeff_USA", "GCAMUSA_XML_BATCH", "batch_all_energy_emissions_USA.xml" ) 
write_mi_data( L273.en_ghg_tech_coeff_USA, "InputEmissCoeff", "GCAMUSA_LEVEL2_DATA", "L273.en_ghg_tech_coeff_USA", "GCAMUSA_XML_BATCH", "batch_ghg_emissions_USA.xml" ) 
write_mi_data( L273.en_ghg_emissions_USA, "InputEmissions", "GCAMUSA_LEVEL2_DATA", "L273.en_ghg_emissions_USA", "GCAMUSA_XML_BATCH", "batch_ghg_emissions_USA.xml" ) 
write_mi_data( L273.out_ghg_emissions_USA, "StbTechOutputEmissions", "GCAMUSA_LEVEL2_DATA", "L273.out_ghg_emissions_USA", "GCAMUSA_XML_BATCH", "batch_ghg_emissions_USA.xml" ) 
#write_mi_data( L273.ResMAC_fos_USA, "ResMAC", "GCAMUSA_LEVEL2_DATA", "L273.ResMAC_fos_USA", "GCAMUSA_XML_BATCH", "batch_ghg_emissions_USA.xml" ) 
write_mi_data( L273.MAC_higwp_USA, "MAC", "GCAMUSA_LEVEL2_DATA", "L273.MAC_higwp_USA", "GCAMUSA_XML_BATCH", "batch_ghg_emissions_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_ghg_emissions_USA.xml", "GCAMUSA_XML_FINAL", "ghg_emissions_USA.xml", "", xml_tag="outFile" )

logstop()
