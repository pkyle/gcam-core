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

logstart( "L274.bld_nonghg_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))


printlog( "Non-GHG input emissions parameters for buildings technologies in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
calibrated_techs_bld_usa <- readdata( "GCAMUSA_MAPPINGS", "calibrated_techs_bld_usa" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
L174.nonghg_tg_state_bld_F_Yb <- readdata( "GCAMUSA_LEVEL1_DATA", "L174.nonghg_tg_state_bld_F_Yb" )
L244.StubTech_bld <- readdata( "GCAMUSA_LEVEL2_DATA", "L244.StubTech_bld", skip = 4 )
L244.StubTechCalInput_bld <- readdata( "GCAMUSA_LEVEL2_DATA", "L244.StubTechCalInput_bld", skip = 4 )
# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Compute technology shares based on level of detail available in NEI emissions data (sector/fuel)
printlog( "Sector/fuel shares of each buildings sector technology based on fuel input" )
#The emissions are broken out into sector and fuel, so first aggregate the fuel inputs to this level of detail
agg_CalInput_bld_S_F_Ybf <- L244.StubTechCalInput_bld %>%
  filter(year == final_model_base_year) %>%
  mutate(sector = ifelse(grepl("comm",supplysector),"comm","resid")) %>%
  #The subsector is the same as the fuel in the buildings sector so use this as id variable
  group_by(region,sector,subsector) %>%
  summarise(calibrated.value = sum(calibrated.value))
  
#Compute technology shares for each buildings sector technology, and match on the emissions
bld_fuel_input_tech_shares <- L244.StubTechCalInput_bld %>%
  filter(year == final_model_base_year) %>%
  mutate(sector = ifelse(grepl("comm",supplysector),"comm","resid")) %>%
  mutate(S_F_tech_share = calibrated.value / agg_CalInput_bld_S_F_Ybf$calibrated.value[match(paste(region,sector,subsector),
         vecpaste(agg_CalInput_bld_S_F_Ybf[c("region","sector","subsector")]))]) %>%
  ###MISSING VALUES: where agg fuel input is zero. Set to zero for now
  mutate(S_F_tech_share = ifelse(is.na(S_F_tech_share),0,S_F_tech_share)) %>%
  select(region,sector,supplysector,subsector,stub.technology,S_F_tech_share)

# 2b. Share out NEI emissions to the buildings technologies using the sector/fuel technology shares
printlog( "Base-year input emissions for buildings sector technologies in the USA")
#Start with a table containing all of the technology combinations
names(L244.StubTech_bld)[4]="stub.technology"
L274.bld_nonghg_emissions_USA <- L244.StubTech_bld %>%
  mutate(sector = ifelse(grepl("comm",supplysector),"comm","resid"), year = final_model_base_year) %>%
  #Add on pollutants in the NEI data
  repeat_and_add_vector("Non.CO2",unique(L174.nonghg_tg_state_bld_F_Yb$Non.CO2)) %>%
  repeat_and_add_vector("region",states_subregions$state) %>%
  #Filter to only include tech emissions with available data
  filter(paste(sector,subsector,Non.CO2) %in% vecpaste(L174.nonghg_tg_state_bld_F_Yb[c(S_F,"Non.CO2")])) %>%
  #Match on the tech shares
  mutate(S_F_tech_share = bld_fuel_input_tech_shares$S_F_tech_share[match(paste(region,supplysector,subsector,stub.technology),
         vecpaste(bld_fuel_input_tech_shares[c(R_sup_sub_stubtech)]))]) %>%
  #Match on the NEI input emissions
  mutate(input.emissions = S_F_tech_share * L174.nonghg_tg_state_bld_F_Yb[[X_final_model_base_year]][
         match(paste(region,sector,subsector,Non.CO2),vecpaste(L174.nonghg_tg_state_bld_F_Yb[c(state_S_F,"Non.CO2")]))]) %>%
  ###MISSING VALUES: there are no emissions in some state permutations that have fuel use in the base year. Leave them in
  select(-sector,-S_F_tech_share) %>%
  distinct()

# 2c. Compute emission tech coefficients: divide emissions by fuel use
printlog( "Base-year input emissions coefficients for buildings sector technologies in the USA" )
L274.nonghg_bld_tech_coeff_USA.NAs <- L274.bld_nonghg_emissions_USA %>%
  #Add on fuel inputs to the emissions table
  mutate(fuel_input = L244.StubTechCalInput_bld$calibrated.value[
         match(paste(region,supplysector,subsector,stub.technology,year),
               vecpaste(L244.StubTechCalInput_bld[c(R_sup_sub_stubtech,"year")]))]) %>%
  #Compute tech coefficients. ###MISSING VALUES: corresponding to NAs in the emissions table
  mutate(emiss.coeff = input.emissions / fuel_input)

#The missing values may correspond to emissions that actually have data in other states. So compute averages
#weighted by fuel use and use the averages in the states with missing emissions data
avg_bld_tech_coeffs <- L274.nonghg_bld_tech_coeff_USA.NAs %>%
  na.omit() %>%
  group_by(supplysector,subsector,stub.technology,Non.CO2) %>%
  summarise(emiss.coeff = weighted.mean(emiss.coeff,fuel_input))

#Build a table containing the data points where averages will be used
L274.nonghg_bld_tech_coeff_USA.missing <- L274.nonghg_bld_tech_coeff_USA.NAs %>%
  filter(is.na(emiss.coeff)) %>%
  mutate(emiss.coeff = avg_bld_tech_coeffs$emiss.coeff[match(
         paste(supplysector,subsector,stub.technology,Non.CO2),
         vecpaste(avg_bld_tech_coeffs[c(sup_sub_stubtech,"Non.CO2")]))]) %>%
  filter(!is.na(emiss.coeff)) %>%
  mutate("unit" = "Tg/EJ")

#Write the table containing the averages
write.csv(L274.nonghg_bld_tech_coeff_USA.missing, "level2/L274.nonghg_bld_tech_coeff_USA.missing.csv",row.names=FALSE)

#Add the averages to the table containing the rest of the emission coefficients, and format for model input
L274.nonghg_bld_tech_coeff_USA <- L274.nonghg_bld_tech_coeff_USA.NAs %>%
    mutate(emiss.coeff = ifelse(is.na(emiss.coeff),L274.nonghg_bld_tech_coeff_USA.missing$emiss.coeff[
          match(paste(region,supplysector,subsector,stub.technology,Non.CO2),
          vecpaste(L274.nonghg_bld_tech_coeff_USA.missing[c(R_sup_sub_stubtech,"Non.CO2")]))],emiss.coeff)) %>%
  ###MISSING VALUES: are where there is neither emissions nor fuel input data. OK to omit
    select(region,supplysector,subsector,stub.technology,year,Non.CO2,emiss.coeff)%>%
    na.omit() %>%
    distinct() %>%
    mutate(emiss.coeff = round(emiss.coeff,digits_emissions)) %>%
    arrange(region,Non.CO2,supplysector,subsector,stub.technology)

stopifnot(!any(is.na(L274.nonghg_bld_tech_coeff_USA)))
stopifnot(!any(is.infinite(L274.nonghg_bld_tech_coeff_USA$emiss.coeff)))
# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L274.nonghg_bld_tech_coeff_USA, "InputEmissCoeff", "GCAMUSA_LEVEL2_DATA", "L274.nonghg_bld_tech_coeff_USA", "GCAMUSA_XML_BATCH", "batch_bld_emissions_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_bld_emissions_USA.xml", "GCAMUSA_XML_FINAL", "bld_emissions_USA.xml", "", xml_tag="outFile" )

logstop()




