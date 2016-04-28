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

logstart( "L272.elc_nonghg_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))


printlog( "Non-GHG emissions parameters for electricity technologies in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
A27.tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A27.tech_associations" )
BC_OC_assumptions <- readdata( "GCAMUSA_LEVEL0_DATA", "BC_OC_assumptions" )
L123.in_EJ_state_elec_F <- readdata ( "GCAMUSA_LEVEL1_DATA", "L123.in_EJ_state_elec_F" )
L172.nonghg_tg_state_elec_F_Yb <- readdata( "GCAMUSA_LEVEL1_DATA", "L172.nonghg_tg_state_elec_F_Yb" )
L172.nonghg_tgej_state_elec_F_Yf <- readdata ( "GCAMUSA_LEVEL1_DATA", "L172.nonghg_tgej_state_elec_F_Yf" )
L223.StubTech_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTech_elec_USA", skip = 4 )
EPA_state_egu_emission_factors_ktPJ_post2015 <- readdata( "GCAMUSA_LEVEL0_DATA", "EPA_state_egu_emission_factors_ktPJ_post2015")

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Compute aggregate state/fuel emission factors  
printlog( "Calculating electricity fuel emission factors in the base year for U.S. states" )

#First clean up the fuel inputs
agg_fuel_input_elec_Ybf <- L123.in_EJ_state_elec_F %>%
  #convert to long format
  gather(year,fuel_input,-state,-sector,-fuel) %>%
  #filter only base year emissions
  filter(year == X_final_model_base_year)

#Take the table containing the emissions, match on the fuel inputs, and compute the emission coefficients
agg_nonghg_elec_emiss_coeffs_F_Ybf <- L172.nonghg_tg_state_elec_F_Yb %>%
  rename_("input.emissions" = X_final_model_base_year) %>%
  mutate(fuel_input = agg_fuel_input_elec_Ybf$fuel_input[match(
    paste(state,fuel),vecpaste(agg_fuel_input_elec_Ybf[c("state","fuel")]))]) %>%
  mutate(emiss.coeff = input.emissions / fuel_input) 
  ###MISSING VALUES: NAs and Infs generated. Keep these for now

#Compute USA average emission factors for elec fuel inputs, weighted by fuel input
avg_elec_emiss_coeffs_Yb <- agg_nonghg_elec_emiss_coeffs_F_Ybf %>%
  filter(!(is.na(emiss.coeff) | is.infinite(emiss.coeff))) %>%
  group_by(fuel,Non.CO2) %>%
  summarise(emiss.coeff = weighted.mean(emiss.coeff,fuel_input))

# 2b. Assign emission coefficients to the electricity technologies in the base year
printlog( "L272.nonghg_elec_tech_coeff_Yb_USA: emission factors for electricity technologies in the base year in all U.S. states" )

nonghg_elec_tech_coeff_Yb_USA.NAs <- L223.StubTech_elec_USA %>%
  #Add year and pollutant column
  mutate(year = final_model_base_year) %>%
  repeat_and_add_vector("Non.CO2",unique(L172.nonghg_tg_state_elec_F_Yb$Non.CO2)) %>%
  #Match on emission factors
  mutate(emiss.coeff = agg_nonghg_elec_emiss_coeffs_F_Ybf$emiss.coeff[match(
    paste(region,subsector,Non.CO2),vecpaste(agg_nonghg_elec_emiss_coeffs_F_Ybf[c("state","fuel","Non.CO2")]))])
  ###MISSING VALUES: where there is missing emissions data and/or fuel input data. Replace with averages
  
#Build a table containing the data points where averages will be used
L272.nonghg_elec_tech_coeff_USA.missing <- nonghg_elec_tech_coeff_Yb_USA.NAs %>%
  filter(is.na(emiss.coeff) | is.infinite(emiss.coeff)) %>%
  mutate(emiss.coeff = avg_elec_emiss_coeffs_Yb$emiss.coeff[match(
    paste(subsector,Non.CO2),
    vecpaste(avg_elec_emiss_coeffs_Yb[c("fuel","Non.CO2")]))]) %>%
  filter(!(is.na(emiss.coeff) | is.infinite(emiss.coeff))) %>%
  mutate("unit" = "Tg/EJ")

#Write the table containing the averages
write.csv(L272.nonghg_elec_tech_coeff_USA.missing, "level2/L272.nonghg_elec_tech_coeff_USA.missing.csv",row.names=FALSE)

#Match on the averages to the original table
nonghg_elec_tech_coeff_Yb_USA.noBCOC <- nonghg_elec_tech_coeff_Yb_USA.NAs %>%
  mutate(emiss.coeff = ifelse(is.na(emiss.coeff) | is.infinite(emiss.coeff),avg_elec_emiss_coeffs_Yb$emiss.coeff[match(
    paste(subsector,Non.CO2),vecpaste(avg_elec_emiss_coeffs_Yb[c("fuel","Non.CO2")]))],emiss.coeff)) %>%
  ###MISSING VALUES: remaining NAs are for renewables. We don't care about these so omit
  na.omit()

#There is no data for BC/OC in the base year, so use fractions of PM2.5 to calculate BC/OC emissions.
printlog( "Compute BC/OC emission factors in the base year based on PM2.5 emission factors" )
#Compute BC emiss coeffs first
BC_elec_tech_coeff_Yb_USA <- nonghg_elec_tech_coeff_Yb_USA.noBCOC %>%
  filter(Non.CO2 == "PM2.5") %>%
  #Match on the BC fractions of PM2.5 emissions
  mutate(BC_fraction = BC_OC_assumptions$BC_fraction[match(subsector,BC_OC_assumptions$fuel)]) %>%
  #Compute emissions coefficients for BC 
  mutate(emiss.coeff = emiss.coeff * BC_fraction,Non.CO2 = "BC") %>%
  select(-BC_fraction)

OC_elec_tech_coeff_Yb_USA <- nonghg_elec_tech_coeff_Yb_USA.noBCOC %>%
  filter(Non.CO2 == "PM2.5") %>%
  #Match on the OC fractions of PM2.5 emissions
  mutate(OC_fraction = BC_OC_assumptions$OC_fraction[match(subsector,BC_OC_assumptions$fuel)]) %>%
  #Compute emissions coefficients for OC
  mutate(emiss.coeff = emiss.coeff * OC_fraction,Non.CO2 = "OC") %>%
  select(-OC_fraction)

#Bind the BC/OC coefficients into one table then add them to the rest of the emissions coefficients
BCOC_elec_tech_coeff_Yb_USA <- bind_rows(BC_elec_tech_coeff_Yb_USA,OC_elec_tech_coeff_Yb_USA)
nonghg_elec_tech_coeff_Yb_USA <- bind_rows(nonghg_elec_tech_coeff_Yb_USA.noBCOC,BCOC_elec_tech_coeff_Yb_USA)

#Also write out the relevant coefficients for technologies in the existing coal-gas dispatcher
nonghg_disp_tech_coeff_USA <- nonghg_elec_tech_coeff_Yb_USA %>%
  filter(stub.technology %in% A27.tech_associations$elec_technology) %>%
  mutate(supplysector = A27.tech_associations$supplysector[match(stub.technology,A27.tech_associations$elec_technology)],
         subsector = A27.tech_associations$subsector[match(stub.technology,A27.tech_associations$elec_technology)],
         stub.technology = A27.tech_associations$technology[match(stub.technology,A27.tech_associations$elec_technology)])

#Bind the table containing the dispatcher coefficients and the table containing the elec coeffs together
L272.nonghg_elec_tech_coeff_Yb_USA <- bind_rows(nonghg_elec_tech_coeff_Yb_USA,nonghg_disp_tech_coeff_USA)

stopifnot(!any(is.na(L272.nonghg_elec_tech_coeff_Yb_USA)))
stopifnot(!any(is.infinite(L272.nonghg_elec_tech_coeff_Yb_USA$emiss.coeff)))

# 2c. Electricity technology emission coefficients in future years
printlog( "L272.nonghg_elec_tech_coeff_Yf_USA: emission factors for electricity technologies in future years in all U.S. states")

#Clean up the level1 emission factors 
L172.nonghg_tgej_state_elec_F_Yf.long <- L172.nonghg_tgej_state_elec_F_Yf %>%
  gather(year,emiss.coeff,-state,-sector,-fuel,-Non.CO2) %>%
  mutate(year = as.numeric(substr(year,2,5)))

#Create a table to contain future emission factors, starting with exhaustive list of all technologies
L272.nonghg_elec_tech_coeff_Yf_USA.NAs <- L223.StubTech_elec_USA %>%
  #Add on years from available data
  repeat_and_add_vector("year",unique(L172.nonghg_tgej_state_elec_F_Yf.long$year)) %>%
  #Add on pollutants
  repeat_and_add_vector("Non.CO2",unique(L172.nonghg_tgej_state_elec_F_Yf$Non.CO2)) %>%
  #Match on emission factors for post-2015
  mutate(emiss.coeff = EPA_state_egu_emission_factors_ktPJ_post2015$emiss.coeff[match(
      paste(subsector,stub.technology,Non.CO2), 
      vecpaste(EPA_state_egu_emission_factors_ktPJ_post2015[c("fuel","technology","Non.CO2")]))]) %>%
  ###MISSING VALUES: All emission factors for renewable energy technologies and all NH3 emission factors
  #For NH3, use data from EPA-ORD
  mutate(emiss.coeff = ifelse(is.na(emiss.coeff) & Non.CO2== "NH3", 
      L172.nonghg_tgej_state_elec_F_Yf.long$emiss.coeff[match(paste(region, subsector, Non.CO2, year), 
      vecpaste(L172.nonghg_tgej_state_elec_F_Yf.long[c("state","fuel","Non.CO2", "year")]))], emiss.coeff)) 
  ###MISSING VALUES: only in NH3. Replace what we can with averages 

#Compute USA average emission factors for elec fuel inputs in future years
avg_elec_emiss_coeffs_Yf <- L172.nonghg_tgej_state_elec_F_Yf.long %>%
  group_by(fuel,year,Non.CO2) %>%
  summarise(emiss.coeff = mean(emiss.coeff))

#Build a table containing the data points where averages will be used
L272.nonghg_elec_tech_coeff_Yf_USA.missing <- L272.nonghg_elec_tech_coeff_Yf_USA.NAs %>%
  filter(is.na(emiss.coeff) | is.infinite(emiss.coeff)) %>%
  mutate(emiss.coeff = avg_elec_emiss_coeffs_Yf$emiss.coeff[match(
    paste(subsector,year,Non.CO2),
    vecpaste(avg_elec_emiss_coeffs_Yf[c("fuel","year","Non.CO2")]))],emiss.coeff) %>%
  filter(!(is.na(emiss.coeff) | is.infinite(emiss.coeff))) %>%
  mutate("unit" = "Tg/EJ")

#Write the table containing the averages
write.csv(L272.nonghg_elec_tech_coeff_Yf_USA.missing, "level2/L272.nonghg_elec_tech_coeff_Yf_USA.missing.csv",row.names=FALSE)

#Replace missing values with averages in the future data
L272.nonghg_elec_tech_coeff_Yf_USA <- L272.nonghg_elec_tech_coeff_Yf_USA.NAs %>%
  mutate(emiss.coeff = ifelse(is.na(emiss.coeff),avg_elec_emiss_coeffs_Yf$emiss.coeff[match(
    paste(subsector,year,Non.CO2),
    vecpaste(avg_elec_emiss_coeffs_Yf[c("fuel","year","Non.CO2")]))],emiss.coeff)) %>%
  ###MISSING VALUES: nuclear and renewables. We don't care about these so OK to omit
  na.omit()

stopifnot(!any(is.na(L272.nonghg_elec_tech_coeff_Yf_USA)))
stopifnot(!any(is.infinite(L272.nonghg_elec_tech_coeff_Yf_USA$emiss.coeff)))

#2d. Electricity emission factors in all years
printlog( "L272.nonghg_elec_tech_coeff_USA: emission factors for electricity technologies in all U.S. states" )

#Organize and format for write-out
L272.nonghg_elec_tech_coeff_USA <- bind_rows(L272.nonghg_elec_tech_coeff_Yb_USA,L272.nonghg_elec_tech_coeff_Yf_USA) %>%
  distinct() %>%
  mutate(emiss.coeff = round(emiss.coeff,digits_emissions)) %>%
  arrange(region,Non.CO2,supplysector,subsector,stub.technology,year)


stopifnot(!any(is.na(L272.nonghg_elec_tech_coeff_USA)))
stopifnot(!any(is.infinite(L272.nonghg_elec_tech_coeff_USA$emiss.coeff)))

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L272.nonghg_elec_tech_coeff_USA, "InputEmissCoeff", "GCAMUSA_LEVEL2_DATA", "L272.nonghg_elec_tech_coeff_USA", "GCAMUSA_XML_BATCH", "batch_elec_emissions_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_emissions_USA.xml", "GCAMUSA_XML_FINAL", "elec_emissions_USA.xml", "", xml_tag="outFile" )

logstop()




