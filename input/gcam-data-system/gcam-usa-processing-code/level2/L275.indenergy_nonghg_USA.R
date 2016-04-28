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

logstart( "L275.indenergy_nonghg_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))


printlog( "Non-GHG input emissions parameters for industrial energy use sector in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
indliq_emiss_factor_trends <- readdata( "GCAMUSA_LEVEL0_DATA","indliq_emiss_factor_trends" )
L175.nonghg_tg_state_indenergy_F_Yb <- readdata( "GCAMUSA_LEVEL1_DATA", "L175.nonghg_tg_state_indenergy_F_Yb" )
L232.StubTech_ind_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L232.StubTech_ind_USA", skip = 4 )
L232.StubTechCalInput_indenergy_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L232.StubTechCalInput_indenergy_USA", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#The technology information in the industrial energy use sector has the same level of detail as the 
#NEI data so all that remains to be done is to assign the emissions to states and compute coefficients
#and averages if needed
# 2a. Compute aggregate state/fuel emission factors  
printlog( "Calculating industry fuel use emission factors in the base year for U.S. states" )

#Fuel use is divided between cogen and non-cogen. Aggregate these to subsector level
agg_fuel_input_indenergy_Ybf <- L232.StubTechCalInput_indenergy_USA %>%
  filter(year == final_model_base_year) %>%
  group_by(region,supplysector,subsector,year) %>%
  summarise(fuel_input = sum(calibrated.value)) 

#Take the table containing the emissions, match on the fuel inputs, and compute the emission coefficients
agg_nonghg_indenergy_emiss_coeffs_F_Ybf <- L175.nonghg_tg_state_indenergy_F_Yb %>%
  rename_("input.emissions" = X_final_model_base_year) %>%
  mutate(fuel_input = agg_fuel_input_indenergy_Ybf$fuel_input[match(
    paste(state,fuel),vecpaste(agg_fuel_input_indenergy_Ybf[c("region","subsector")]))]) %>%
  mutate(emiss.coeff = input.emissions / fuel_input) 
###MISSING VALUES: NAs and Infs generated. Keep these for now

#Compute USA average emission factors for industry fuel inputs, weighted by fuel input
avg_indenergy_emiss_coeffs_Yb <- agg_nonghg_indenergy_emiss_coeffs_F_Ybf %>%
  filter(!(is.na(emiss.coeff) | is.infinite(emiss.coeff))) %>%
  group_by(fuel,Non.CO2) %>%
  summarise(emiss.coeff = weighted.mean(emiss.coeff,fuel_input))

# 2b. Assign emission coefficients to the industry energy use technologies in the base year
printlog( "L275.nonghg_indenergy_tech_coeff_USA: emission factors for industry energy use technologies in the base year in all U.S. states" )
L275.nonghg_indenergy_tech_coeff_USA.NAs <- L232.StubTech_ind_USA %>%
  filter(supplysector == "industrial energy use" & subsector %in% L175.nonghg_tg_state_indenergy_F_Yb$fuel) %>%
  mutate(year = final_model_base_year) %>%
  repeat_and_add_vector("region",states_subregions$state) %>%
  repeat_and_add_vector("Non.CO2",unique(L175.nonghg_tg_state_indenergy_F_Yb$Non.CO2)) %>%
  distinct() %>%
  mutate(emiss.coeff = agg_nonghg_indenergy_emiss_coeffs_F_Ybf$emiss.coeff[match(
    paste(region,subsector,Non.CO2),vecpaste(agg_nonghg_indenergy_emiss_coeffs_F_Ybf[c("state","fuel","Non.CO2")]))])
  ###MISSING VALUES: NAs and Infs generated where, will replace with averages
  ###Do not add missing values for NH3; filter out where NH3 emiss coeff is NA
L275.nonghg_indenergy_tech_coeff_USA.NAs<-subset(L275.nonghg_indenergy_tech_coeff_USA.NAs, !(Non.CO2=="NH3" & is.na(emiss.coeff)))


#Build a table containing the data points where averages will be used
L275.nonghg_indenergy_tech_coeff_USA.missing <- L275.nonghg_indenergy_tech_coeff_USA.NAs %>%
  filter(is.na(emiss.coeff) | is.infinite(emiss.coeff)) %>%
  mutate(emiss.coeff = avg_indenergy_emiss_coeffs_Yb$emiss.coeff[match(
    paste(subsector,Non.CO2),
    vecpaste(avg_indenergy_emiss_coeffs_Yb[c("fuel","Non.CO2")]))]) %>%
  filter(!is.na(emiss.coeff)) %>%
  mutate("unit" = "Tg/EJ")

#Write the table containing the averages
write.csv(L275.nonghg_indenergy_tech_coeff_USA.missing, "level2/L275.nonghg_indenergy_tech_coeff_USA.missing.csv",row.names=FALSE)

#Add the averages to the table containing the rest of the emission coefficients, and format for model input
L275.nonghg_indenergy_tech_coeff_USA <- L275.nonghg_indenergy_tech_coeff_USA.NAs %>%
  mutate(emiss.coeff = ifelse(is.na(emiss.coeff) | is.infinite(emiss.coeff),avg_indenergy_emiss_coeffs_Yb$emiss.coeff[
    match(paste(subsector,Non.CO2),
          vecpaste(avg_indenergy_emiss_coeffs_Yb[c("fuel","Non.CO2")]))],emiss.coeff)) %>%
  select(region,supplysector,subsector,stub.technology,year,Non.CO2,emiss.coeff) %>%
  mutate(emiss.coeff = round(emiss.coeff,digits_emissions)) %>%
  arrange(Non.CO2,supplysector,subsector,stub.technology,region)

# 2c. Apply trends to emission factors from industrial petroleum use
printlog( "Applying trends to emission coefficients for industrial petroleum use technologies" )
#Trends out to 2035 will be applied to emissions factors from industrial petroleum use. Get the trend multipliers
indliq_emiss_factor_trends.long <- indliq_emiss_factor_trends %>%
  #Convert to long format for easier matching
  gather(year,multiplier,-Non.CO2) %>%
  #Re-format year column and only use half the trend. This is based on an assumption that half of industrial
  #petroleum use is from off-highway vehicles
  mutate(year = as.numeric(substr(year,2,5)), multiplier = 0.5 * (1 + multiplier) )

#Apply the trends to the base-year emission factors from NEI
L275.nonghg_indliq_tech_coeff_USA <- L275.nonghg_indenergy_tech_coeff_USA %>%
  ###NOTE: no trend available for ammonia
  filter(subsector == "refined liquids" & Non.CO2 %in% indliq_emiss_factor_trends.long$Non.CO2) %>%
  repeat_and_add_vector("year",unique(indliq_emiss_factor_trends.long$year)) %>%
  mutate(emiss.coeff = emiss.coeff * indliq_emiss_factor_trends.long$multiplier[
         match(paste(year,Non.CO2),vecpaste(indliq_emiss_factor_trends.long[c("year","Non.CO2")]))]) %>%
  mutate(emiss.coeff = round(emiss.coeff,digits_emissions)) %>%
  arrange(Non.CO2,supplysector,subsector,stub.technology,region)


stopifnot(!any(is.na(L275.nonghg_indenergy_tech_coeff_USA)))
stopifnot(!any(is.na(L275.nonghg_indliq_tech_coeff_USA)))

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L275.nonghg_indenergy_tech_coeff_USA, "InputEmissCoeff", "GCAMUSA_LEVEL2_DATA", "L275.nonghg_indenergy_tech_coeff_USA", "GCAMUSA_XML_BATCH", "batch_indenergy_emissions_USA.xml" ) 
write_mi_data( L275.nonghg_indliq_tech_coeff_USA, "InputEmissCoeff", "GCAMUSA_LEVEL2_DATA", "L275.nonghg_indliq_tech_coeff_USA", "GCAMUSA_XML_BATCH", "batch_indenergy_emissions_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_indenergy_emissions_USA.xml", "GCAMUSA_XML_FINAL", "indenergy_emissions_USA.xml", "", xml_tag="outFile" )

logstop()




