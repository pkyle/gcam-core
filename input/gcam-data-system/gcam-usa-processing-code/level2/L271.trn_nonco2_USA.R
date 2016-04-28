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

logstart( "L271.trn_nonco2_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))


printlog( "Non-GHG emissions parameters for transportation technologies in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
MARKAL_UCD_class <- readdata( "GCAMUSA_MAPPINGS", "MARKAL_UCD_class" )
MARKAL_UCD_LDV_fuel <- readdata( "GCAMUSA_MAPPINGS", "MARKAL_UCD_LDV_fuel" )
MARKAL_UCD_HDV_fuel <- readdata( "GCAMUSA_MAPPINGS", "MARKAL_UCD_HDV_fuel" )
state_census_region <- readdata( "GCAMUSA_MAPPINGS", "state_census_region" )
L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y <- readdata( "GCAMUSA_LEVEL1_DATA", "L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y" )
L171.nonco2_tgej_state_trn_SMarkal_F_Y <- readdata( "GCAMUSA_LEVEL1_DATA", "L171.nonco2_tgej_state_trn_SMarkal_F_Y" )
L254.GlobalTranTechSCurve <- readdata( "ENERGY_LEVEL2_DATA", "L254.GlobalTranTechSCurve", skip = 4 )
L254.StubTranTech_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L254.StubTranTech_USA", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs

printlog( "L271.nonco2_trn_tech_coeff_USA: Pollutant emissions for transportation technologies in all U.S. states" )
#LDV emission coefficients
L254.StubTranTech_USA_LDV <- filter(L254.StubTranTech_USA, supplysector == "trn_pass_road_LDV_4W" ) %>%
  mutate(MARKAL_class = MARKAL_UCD_class$MARKAL_class[match(tranSubsector,MARKAL_UCD_class$UCD_class)],
         MARKAL_LDV_fuel = MARKAL_UCD_LDV_fuel$MARKAL_LDV_fuel[match(stub.technology,MARKAL_UCD_LDV_fuel$UCD_LDV_fuel)]) %>%
  repeat_and_add_vector(vector="year",vector_values=c("X2005","X2010",X_future_years))

#Subset relevant classes and fuels from emission factor table
L171.nonco2_tgej_state_LDV_S_F_Y <- filter(L171.nonco2_tgej_state_trn_SMarkal_F_Y, class %in% 
                                              L254.StubTranTech_USA_LDV$MARKAL_class & fuel %in%
                                              L254.StubTranTech_USA_LDV$MARKAL_LDV_fuel)
###NOTE: add rest of the future years here, keeping them constant at 2050 levels
L171.nonco2_tgej_state_LDV_S_F_Y[X_future_years[9:18]] <- L171.nonco2_tgej_state_LDV_S_F_Y["X2050"]

#Melt the data frame
L171.nonco2_tgej_state_LDV_S_F_Y <- gather(L171.nonco2_tgej_state_LDV_S_F_Y, 
                                            year,value,-state,-class,-fuel,-pollutant)

#Match the emission coefficients onto the stub technology table
L271.nonco2_LDV_USA <- L254.StubTranTech_USA_LDV %>%
  repeat_and_add_vector(vector="Non.CO2",unique(L171.nonco2_tgej_state_LDV_S_F_Y$pollutant)) %>%
  mutate(emiss.coeff = L171.nonco2_tgej_state_LDV_S_F_Y$value[match(
    paste(region,MARKAL_class,MARKAL_LDV_fuel,year,Non.CO2),
    vecpaste(L171.nonco2_tgej_state_LDV_S_F_Y[c("state","class","fuel","year","pollutant")]))]) %>%
  ###MISSING VALUES: EV emission factors for pollutants where there is no data, and NH3 EFs for CNG vehicles
  ###where there is also no data. OK to remove for now
  na.omit() %>%
  #Reformat the year column and round the coefficient to five decimal places
  mutate(year=as.numeric(substr(year,2,5)),emiss.coeff=round(emiss.coeff,5)) %>%
  #Select relevant columns 
  select(region,supplysector,tranSubsector,stub.technology,year,Non.CO2,emiss.coeff)

#HDV emission coefficients
L254.StubTranTech_USA_HDV <- filter(L254.StubTranTech_USA, supplysector %in% c("trn_pass_road","trn_freight_road")) %>%
  mutate(MARKAL_class = MARKAL_UCD_class$MARKAL_class[match(tranSubsector,MARKAL_UCD_class$UCD_class)],
         MARKAL_HDV_fuel = MARKAL_UCD_HDV_fuel$MARKAL_HDV_fuel[match(stub.technology,MARKAL_UCD_HDV_fuel$UCD_HDV_fuel)]) %>%
  repeat_and_add_vector(vector="year",vector_values=c("X2005","X2010",X_future_years))

#Subset relevant classes and fuels from emission factor table
L171.nonco2_tgej_state_HDV_S_F_Y <- filter(L171.nonco2_tgej_state_trn_SMarkal_F_Y, class %in% 
                                              L254.StubTranTech_USA_HDV$MARKAL_class & fuel %in%
                                              L254.StubTranTech_USA_HDV$MARKAL_HDV_fuel)

###NOTE: add rest of the future years here, keeping them constant at 2050 levels
L171.nonco2_tgej_state_HDV_S_F_Y[X_future_years[9:18]] <- L171.nonco2_tgej_state_HDV_S_F_Y["X2050"]

#Melt the data frame
L171.nonco2_tgej_state_HDV_S_F_Y <- gather(L171.nonco2_tgej_state_HDV_S_F_Y, 
                                            year,value,-state,-class,-fuel,-pollutant)

#Match the emission coefficients onto the stub technology table
L271.nonco2_HDV_USA <- L254.StubTranTech_USA_HDV %>%
  repeat_and_add_vector(vector="Non.CO2",unique(L171.nonco2_tgej_state_HDV_S_F_Y$pollutant)) %>%
  mutate(emiss.coeff = L171.nonco2_tgej_state_HDV_S_F_Y$value[match(
    paste(region,MARKAL_class,MARKAL_HDV_fuel,year,Non.CO2),
    vecpaste(L171.nonco2_tgej_state_HDV_S_F_Y[c("state","class","fuel","year","pollutant")]))]) %>%
  #Rows with missing values are CNG-fueled vehicle emission factors for technologies where there is no data.
  #For now it's OK to remove
  filter(!is.na(emiss.coeff)) %>%
  #Reformat the year column and round the coefficient to five decimal places
  mutate(year=as.numeric(substr(year,2,5)),emiss.coeff=round(emiss.coeff,5)) %>%
  #Select relevant columns 
  select(region,supplysector,tranSubsector,stub.technology,year,Non.CO2,emiss.coeff)

#Bind the LDV and HDV tables and clean up
L271.nonco2_trn_tech_coeff_USA <- bind_rows(L271.nonco2_HDV_USA,L271.nonco2_LDV_USA) %>%
  #Re-name the pollutants to match names of existing gases
  mutate(Non.CO2 = gsub("VOC","NMVOC",Non.CO2)) %>%
  mutate(Non.CO2 = gsub("NOX","NOx",Non.CO2)) %>%
  arrange(region,supplysector,tranSubsector,stub.technology,year,Non.CO2)

printlog( "L271.nonco2_trn_emiss_control_USA: Emissions coeff controls for transportation technologies in all U.S. states" )
#Need vehicle lifetimes in order to calculate final-emissions-coeff parameter.
#These are written out for future years so assume the base year lifetimes are equal to those in 2010
#No idea if this is correct
L254.GlobalTranTechSCurve.melt <- L254.GlobalTranTechSCurve %>%
  mutate(year = paste("X",year,sep="")) %>%
  spread(year,lifetime) %>%
  mutate(X2005=X2010) %>%
  gather(year,lifetime,-sector.name,-subsector.name,-tranTechnology,-steepness,-half.life) %>%
  filter(!is.na(lifetime)) %>%
  mutate(year=as.numeric(substr(year,2,5)))

#Create table for emissions control
L254.StubTranTech_USA_LDV.coeff <- L254.StubTranTech_USA_LDV %>%
  repeat_and_add_vector(vector="Non.CO2",unique(L171.nonco2_tgej_state_LDV_S_F_Y$pollutant)) %>%
  mutate(emiss.coeff = L171.nonco2_tgej_state_LDV_S_F_Y$value[match(
    paste(region,MARKAL_class,MARKAL_LDV_fuel,Non.CO2,year),
    vecpaste(L171.nonco2_tgej_state_LDV_S_F_Y[c("state","class","fuel","pollutant","year")]))]) %>%
  #MISSING VALUES: EV emission factors for pollutants where there is no data, and NH3 emission factors
  #for CNG vehicles, for which there is no data. OK to remove for now
  na.omit() %>%
  rename(MARKAL_fuel = MARKAL_LDV_fuel)

L254.StubTranTech_USA_HDV.coeff <- L254.StubTranTech_USA_HDV %>%
  repeat_and_add_vector(vector="Non.CO2",unique(L171.nonco2_tgej_state_HDV_S_F_Y$pollutant)) %>%
  mutate(emiss.coeff = L171.nonco2_tgej_state_HDV_S_F_Y$value[match(
    paste(region,MARKAL_class,MARKAL_HDV_fuel,Non.CO2,year),
    vecpaste(L171.nonco2_tgej_state_HDV_S_F_Y[c("state","class","fuel","pollutant","year")]))]) %>%
  #MISSING VALUES: CNG-fueled vehicle emission factors for technologies where there is no data.
  #For now it's OK to remove
  na.omit() %>%
  rename(MARKAL_fuel = MARKAL_HDV_fuel)

#Combine LDV and HDV tables
L254.StubTranTech_USA_trn <- bind_rows(L254.StubTranTech_USA_LDV.coeff,L254.StubTranTech_USA_HDV.coeff) %>%
  mutate(year=as.numeric(substr(year,2,5))) %>%
  filter(year %in% L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y$vintage) %>%
  #Add on lifetime for each technology that can be used to calculate final emissions coefficient
  mutate(lifetime = L254.GlobalTranTechSCurve.melt$lifetime[match(paste(tranSubsector,stub.technology,year),
    vecpaste(L254.GlobalTranTechSCurve.melt[c("subsector.name","tranTechnology","year")]))]) 

#MISSING VALUES: It seems that "Bus" does not have a lifetime. Give it 25 years for now
L254.StubTranTech_USA_trn[is.na(L254.StubTranTech_USA_trn$lifetime),"lifetime"] <- 25

#Add on start year and end year, census region, and id column for applying linear fits to estimate
#final emissions coefficient
L254.StubTranTech_USA_trn <- mutate(L254.StubTranTech_USA_trn, start.year = year, end.year = year + lifetime, 
  census_region = state_census_region$census_region_id[match(region,state_census_region$state)],
  id = paste(MARKAL_class, MARKAL_fuel, start.year,Non.CO2,census_region))

#Prepare table containing emissions factor degradation data
degrates <- subset(L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y, paste(class,fuel) %in%
                     vecpaste(L254.StubTranTech_USA_trn[c("MARKAL_class","MARKAL_fuel")])) %>%
  mutate(id = paste(class,fuel,vintage,pollutant,region)) 

#Create a list that contains linear fits for each degradation rate id, with labels
model_list <- list()
for(i in 1:length(unique(degrates$id))){
  model_list[[i]] <- lm(value ~ year, data = filter(degrates, id == unique(degrates$id)[i]))
  names(model_list)[i] <- unique(degrates$id)[i]
}

#Extract the needed information (unique end year and id) from the emission control table and 
#compute the linear prediction using the model corresponding to each id
prediction_table <- L254.StubTranTech_USA_trn %>%
  select(end.year, id) %>%
  distinct() %>%
  mutate(final.emissions.coefficient = 0) %>%
  rename(year = end.year)
for(i in 1:nrow(prediction_table)){
  prediction_table[i,"final.emissions.coefficient"] <- predict.lm(
    model_list[[prediction_table[i,]$id]],
    newdata=prediction_table[i,"year"])
}

#Add the final emissions coefficients onto the emissions control table
L271.nonco2_trn_emiss_control_USA <- L254.StubTranTech_USA_trn %>%
  mutate(final.emissions.coefficient = prediction_table$final.emissions.coefficient[match(
    paste(end.year,id),vecpaste(prediction_table[c("year","id")]))])

#Some final emissions coefficients are negative. In these cases, change the final value to the last known
negative_degrates <- subset(degrates, id %in% 
                              filter(L271.nonco2_trn_emiss_control_USA, final.emissions.coefficient < 0)$id)
unique_neg_degrate_list <- list()
for(i in 1:length(unique(negative_degrates$id))){
  unique_neg_degrate_list[[i]] <- negative_degrates[negative_degrates$id == unique(negative_degrates$id)[i] &
    negative_degrates$year == max(negative_degrates[negative_degrates$id == unique(negative_degrates$id)[i],
                                                    "year"]),]
}

last_avail_coeff <- bind_rows(unique_neg_degrate_list)
if(nrow(last_avail_coeff) > 0){
L271.nonco2_trn_emiss_control_USA[L271.nonco2_trn_emiss_control_USA$final.emissions.coefficient < 0,
  "final.emissions.coefficient"] <- last_avail_coeff$value[match(L271.nonco2_trn_emiss_control_USA[
    L271.nonco2_trn_emiss_control_USA$final.emissions.coefficient < 0,]$id,last_avail_coeff$id)]
}

L271.nonco2_trn_emiss_control_USA <- L271.nonco2_trn_emiss_control_USA %>%
  #Add linear control object and round to 5 decimal places
  mutate(linear.control = "Emissions Coefficient Degradation",
         final.emissions.coefficient = round(final.emissions.coefficient,5)) %>%
  select(region,supplysector,tranSubsector,stub.technology,year,Non.CO2,linear.control,start.year,end.year,final.emissions.coefficient)

#Add on the remaining future years, keeping the final emissions coefficient constant at last known data point
remaining_future_years <- bind_rows(L254.StubTranTech_USA_LDV.coeff,L254.StubTranTech_USA_HDV.coeff) %>%
  mutate(year=as.numeric(substr(year,2,5))) %>%
  filter(year >= 2050) %>%
  #Add on lifetime for each technology that can be used to calculate final emissions coefficient
  mutate(lifetime = L254.GlobalTranTechSCurve.melt$lifetime[match(paste(tranSubsector,stub.technology,year),
                                                                  vecpaste(L254.GlobalTranTechSCurve.melt[c("subsector.name","tranTechnology","year")]))]) 

#MISSING VALUES: It seems that "Bus" does not have a lifetime. Give it 25 years for now
remaining_future_years[is.na(remaining_future_years$lifetime),"lifetime"] <- 25

#Subset the table containing the 2045 final emissions coefficients to match onto the future years
#Also add an id column for easier matching
emiss_control_2045 <- filter(L271.nonco2_trn_emiss_control_USA, year == 2045) %>%
  mutate(id = paste(region,supplysector,tranSubsector,stub.technology,Non.CO2))

#Add on start year, end year, linear control object, and id for easier matching
remaining_future_years <- remaining_future_years %>%
  mutate(start.year = year, end.year = year + lifetime, linear.control =  "Emissions Coefficient Degradation",
         id = paste(region,supplysector,tranSubsector,stub.technology,Non.CO2)) %>%
  #Match on the 2045 final emissions coefficients
  mutate(final.emissions.coefficient = emiss_control_2045$final.emissions.coefficient[match(id,emiss_control_2045$id)]) %>%
  select(region,supplysector,tranSubsector,stub.technology,year,Non.CO2,linear.control,start.year,end.year,final.emissions.coefficient)

#Bind the future years to the existing emissions control table
L271.nonco2_trn_emiss_control_USA <- bind_rows(L271.nonco2_trn_emiss_control_USA, remaining_future_years) %>%
  #Re-name pollutants to match names of GCAM gases
  mutate(Non.CO2 = gsub("VOC","NMVOC",Non.CO2)) %>%
  mutate(Non.CO2 = gsub("NOX","NOx",Non.CO2)) %>%
  arrange(region,supplysector,tranSubsector,stub.technology,year,Non.CO2)

stopifnot(!any(is.na(L271.nonco2_trn_tech_coeff_USA)))
stopifnot(!any(is.na(L271.nonco2_trn_emiss_control_USA)))

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L271.nonco2_trn_tech_coeff_USA, "TrnInputEmissCoeff", "GCAMUSA_LEVEL2_DATA", "L271.nonco2_trn_tech_coeff_USA", "GCAMUSA_XML_BATCH", "batch_transport_emissions_USA.xml" ) 
write_mi_data( L271.nonco2_trn_emiss_control_USA, "LinearCtrlInc", "GCAMUSA_LEVEL2_DATA", "L271.nonco2_trn_emiss_control_USA", "GCAMUSA_XML_BATCH", "batch_transport_emissions_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_transport_emissions_USA.xml", "GCAMUSA_XML_FINAL", "transport_emissions_USA.xml", "", xml_tag="outFile" )

logstop()



