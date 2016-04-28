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

logstart( "L276.othertrn_nonghg_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))


printlog( "Non-GHG input emissions parameters for domestic aviation, ships, and rail in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
L176.nonghg_tg_state_othertrn_F_Yb <- readdata( "GCAMUSA_LEVEL1_DATA", "L176.nonghg_tg_state_othertrn_F_Yb" )
L254.StubTranTech_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L254.StubTranTech_USA", skip = 4 )
L254.StubTranTechCalInput_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L254.StubTranTechCalInput_USA", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Compute technology shares based on level of detail available in NEI emissions data (tech/fuel)
printlog( "Technology/fuel shares of each rail technology in the USA based on fuel input" )
#In the states, rail fuel input is separated into freight and passenger
#The emissions are given for the "rail" sector, so they must be shared out to the technologies
#The rail emissions are broken out into technology and fuel, so first aggregate the fuel inputs to this level of detail
agg_CalInput_rail_F_Ybf <- L254.StubTranTechCalInput_USA %>%
  ###NOTE: there is no NG fuel input data for these technologies, so omit these emissions for now
  filter(year == final_model_base_year & grepl("Rail",tranSubsector) & 
           stub.technology %in% L176.nonghg_tg_state_othertrn_F_Yb$stub.technology) %>%
  mutate(sector = "trn_rail") %>%
  group_by(region,sector,stub.technology) %>%
  summarise(calibrated.value = sum(calibrated.value))
  
#Compute shares for each rail technology
rail_fuel_input_tech_shares <- L254.StubTranTechCalInput_USA %>%
  filter(year == final_model_base_year & grepl("Rail",tranSubsector) & 
           stub.technology %in% L176.nonghg_tg_state_othertrn_F_Yb$stub.technology) %>%
  mutate(sector = "trn_rail") %>%
  mutate(tech_share = calibrated.value / agg_CalInput_rail_F_Ybf$calibrated.value[match(paste(region,stub.technology),
         vecpaste(agg_CalInput_rail_F_Ybf[c("region","stub.technology")]))]) %>%
  select(region,supplysector,tranSubsector,stub.technology,year,sector,tech_share)

# 2b. Share out NEI emissions to the rail technologies using the technology shares
printlog( "L276.rail_nonghg_emissions_USA: Base-year input emissions for rail technologies in the USA" )

#Start with all possible permutations in the states  
L276.rail_nonghg_emissions_USA <- L254.StubTranTech_USA %>%
  filter(grepl("Rail",tranSubsector) & stub.technology %in% L176.nonghg_tg_state_othertrn_F_Yb$stub.technology) %>%
  #Add on a column containing the final model base year
  mutate(year = final_model_base_year) %>%
  #Add the pollutants contained in the NEI data
  repeat_and_add_vector("Non.CO2",unique(L176.nonghg_tg_state_othertrn_F_Yb$Non.CO2)) %>%
  #Match on the tech shares
  mutate(tech_share = rail_fuel_input_tech_shares$tech_share[match(paste(region,tranSubsector,stub.technology),
         vecpaste(rail_fuel_input_tech_shares[c("region","tranSubsector","stub.technology")]))]) %>%
  mutate(sector = "trn_rail") %>%
  #Match on the NEI input emissions
  mutate(input.emissions = tech_share * L176.nonghg_tg_state_othertrn_F_Yb[[X_final_model_base_year]][
         match(paste(region,sector,stub.technology,Non.CO2),
               vecpaste(L176.nonghg_tg_state_othertrn_F_Yb[c("state","sector","stub.technology","Non.CO2")]))]) %>%
  ###MISSING VALUES: No emissions from rail in Hawaii. Leave them in for now
  select(-sector,-tech_share) %>%
  distinct()

# 2c. Format domestic aviation and shipping emissions
printlog( "L276.air_ship_nonghg_emissions_USA: Base-year input emissions for domestic aviation and shipping technologies in the USA" )

L276.air_ship_nonghg_emissions_USA <- L254.StubTranTech_USA %>%
  filter(tranSubsector %in% c("Domestic Ship","Domestic Aviation") &
           stub.technology %in% L176.nonghg_tg_state_othertrn_F_Yb$stub.technology) %>%
  mutate(year = final_model_base_year) %>%
  repeat_and_add_vector("Non.CO2",unique(L176.nonghg_tg_state_othertrn_F_Yb$Non.CO2)) %>%
  mutate(sector = ifelse(tranSubsector == "Domestic Ship","trn_domestic ship","trn_domestic air")) %>%
  #Match on the emissions
  mutate(input.emissions = L176.nonghg_tg_state_othertrn_F_Yb[[X_final_model_base_year]][
    match(paste(region,sector,stub.technology,Non.CO2),
          vecpaste(L176.nonghg_tg_state_othertrn_F_Yb[c("state","sector","stub.technology","Non.CO2")]))]) %>%
  ###MISSING VALUES: many permutations. Leave them in for now
  select(-sector) %>%
  distinct()

#Bind the air, ship, and rail emissions into a single table
printlog( "L276.othertrn_nonghg_emissions_USA: base-year input emissions for domestic ship, aviation, and rail technologies in the USA" )

L276.othertrn_nonghg_emissions_USA <- bind_rows(L276.rail_nonghg_emissions_USA, L276.air_ship_nonghg_emissions_USA)

# 2c. Compute emission tech coefficients: divide emissions by fuel use
printlog( "L276.nonghg_othertrn_tech_coeff_USA: Base-year input emissions coefficients for domestic air, ship, and rail technologies in the USA" )
L276.nonghg_othertrn_tech_coeff_USA.NAs <- L276.othertrn_nonghg_emissions_USA %>%
  #Add on fuel inputs to the emissions table
  mutate(fuel_input = L254.StubTranTechCalInput_USA$calibrated.value[
         match(paste(region,supplysector,tranSubsector,stub.technology,year),
               vecpaste(L254.StubTranTechCalInput_USA[c("region","supplysector","tranSubsector","stub.technology","year")]))]) %>%
  #Compute tech coefficients
  mutate(emiss.coeff = input.emissions / fuel_input)
  ###MISSING VALUES: where there are no emissions, or emissions but no fuel input. A lot of these are domestic shipping
  ###emissions in landlocked states, so could ignore, but use averages anyway

#The missing values may correspond to emissions that actually have data in other states. So compute averages
#weighted by fuel use and use the averages in the states with missing data
avg_othertrn_tech_coeffs <- L276.nonghg_othertrn_tech_coeff_USA.NAs %>%
  filter(!(is.na(emiss.coeff) | is.infinite(emiss.coeff))) %>%
  group_by(supplysector,tranSubsector,stub.technology,Non.CO2) %>%
  summarise(emiss.coeff = weighted.mean(emiss.coeff,fuel_input))

#Build a table containing the data points where averages will be used
L276.nonghg_othertrn_tech_coeff_USA.missing <- L276.nonghg_othertrn_tech_coeff_USA.NAs %>%
  filter(is.na(emiss.coeff) | is.infinite(emiss.coeff)) %>%
  mutate(emiss.coeff = avg_othertrn_tech_coeffs$emiss.coeff[match(
         paste(supplysector,tranSubsector,stub.technology,Non.CO2),
         vecpaste(avg_othertrn_tech_coeffs[c("supplysector","tranSubsector","stub.technology","Non.CO2")]))]) %>%
  filter(!(is.na(emiss.coeff) | is.infinite(emiss.coeff))) %>%
  mutate("unit" = "Tg/EJ")

#Write the table containing the averages
write.csv(L276.nonghg_othertrn_tech_coeff_USA.missing, "level2/L276.nonghg_othertrn_tech_coeff_USA.missing.csv",row.names=FALSE)

#Add the averages to the table containing the rest of the emission coefficients, and format for model input
L276.nonghg_othertrn_tech_coeff_USA <- L276.nonghg_othertrn_tech_coeff_USA.NAs %>%
  mutate(emiss.coeff = ifelse(is.na(emiss.coeff) | is.infinite(emiss.coeff),L276.nonghg_othertrn_tech_coeff_USA.missing$emiss.coeff[
    match(paste(region,supplysector,tranSubsector,stub.technology,Non.CO2),
          vecpaste(L276.nonghg_othertrn_tech_coeff_USA.missing[c("region","supplysector","tranSubsector","stub.technology","Non.CO2")]))],emiss.coeff)) %>%
  ###MISSING VALUES: NH3 emissions from aviation. No emissions for any of the states. OK to omit
  filter(!(is.na(emiss.coeff) | is.infinite(emiss.coeff))) %>%
  select(region,supplysector,tranSubsector,stub.technology,year,Non.CO2,emiss.coeff) %>%
  distinct() %>%
  mutate(emiss.coeff = round(emiss.coeff,digits_emissions)) %>%
  arrange(region,Non.CO2,supplysector,tranSubsector,stub.technology) %>%
  filter(!(Non.CO2=="CO2"))

stopifnot(!any(is.na(L276.nonghg_othertrn_tech_coeff_USA)))
stopifnot(!any(is.infinite(L276.nonghg_othertrn_tech_coeff_USA$emiss.coeff)))
# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L276.nonghg_othertrn_tech_coeff_USA, "InputEmissCoeff", "GCAMUSA_LEVEL2_DATA", "L276.nonghg_othertrn_tech_coeff_USA", "GCAMUSA_XML_BATCH", "batch_othertrn_emissions_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_othertrn_emissions_USA.xml", "GCAMUSA_XML_FINAL", "othertrn_emissions_USA.xml", "", xml_tag="outFile" )

logstop()




