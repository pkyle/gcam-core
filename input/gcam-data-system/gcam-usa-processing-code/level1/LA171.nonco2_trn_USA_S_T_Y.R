
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
logstart( "LA171.nonco2_trn_USA_S_T_Y.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))

printlog( "Emissions factors for transportation by MARKAL technology in the USA" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
GREET2014_LDV_CNG_EFs_tgEJ <- readdata( "GCAMUSA_LEVEL0_DATA", "GREET2014_LDV_CNG_EFs_tgEJ" )
MARKAL_LDV_EFs_gpm <- readdata( "GCAMUSA_LEVEL0_DATA", "MARKAL_LDV_EFs_gpm" )
MARKAL_HDV_EFs_gpm <- readdata( "GCAMUSA_LEVEL0_DATA", "MARKAL_HDV_EFs_gpm" )
MARKAL_LDV_eff_vmtMJ <- readdata( "GCAMUSA_LEVEL0_DATA", "MARKAL_LDV_eff_vmtMJ" )
MARKAL_HDV_eff_vmtMJ <- readdata( "GCAMUSA_LEVEL0_DATA", "MARKAL_HDV_eff_vmtMJ" )
MOVES_vehicle_age_fractions <- readdata( "GCAMUSA_LEVEL0_DATA", "MOVES_vehicle_age_fractions" )
MOVES_source_type_pop <- readdata( "GCAMUSA_LEVEL0_DATA", "MOVES_source_type_pop" )
MOVES_src_type_reg_class_fractions <- readdata( "GCAMUSA_LEVEL0_DATA", "MOVES_src_type_reg_class_fractions" )
state_census_region <- readdata( "GCAMUSA_MAPPINGS", "state_census_region" )
MARKAL_fuel_name_code <- readdata( "GCAMUSA_MAPPINGS", "MARKAL_fuel_name_code" )
MARKAL_MOVES_class <- readdata( "GCAMUSA_MAPPINGS", "MARKAL_MOVES_class" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. The base year vehicle emissions must incorporate past vintages. Here we will assign age fractions to 
# each vehicle vintage in the base year from the MOVES assumptions
printlog( "Assign vintage age fractions for vehicles in the calibration years" )
#Select calibration-year vintages from the MOVES data
MOVES_trn_age_fractions_Yb <- MOVES_vehicle_age_fractions %>%
  filter(yearID %in% c(2005,2010)) %>%
  mutate(Vintage = yearID - ageID) %>%
  #Assign vintages into five-year bins since emissions factor data is provided in 5 year increments
  #Assign 1990 vintage to all years prior to 1990
  mutate(Vintage = ifelse(yearID == 2005 & Vintage <= 1990,1990,
                          ifelse(Vintage <= 1995,1995,
                                 ifelse(Vintage <= 2000,2000,
                                        ifelse(Vintage <= 2005,2005,2010))))) %>%
  #Aggregate so the age fractions still add up to one
  group_by(sourceTypeID,yearID,Vintage) %>%
  summarise(ageFraction = sum(ageFraction))

#MARKAL car classes all received emissions factors from one MOVES source category
MARKAL_MOVES_class_car <- filter(MARKAL_MOVES_class, MARKAL_Class %in% c("Mini car", "Full size car", "Compact car"))
MOVES_car_age_fractions_Yb <- MOVES_trn_age_fractions_Yb %>%
  filter(sourceTypeID %in% MARKAL_MOVES_class_car$MOVES_Source_Type) %>%
  repeat_and_add_vector("Class",MARKAL_MOVES_class_car$MARKAL_Class)

#MARKAL "Commercial truck" class uses data from MOVES regulatory classes, which in turn map to source IDs.
#Thus, will have to weight the source ID-sorted age fractions by the number of vehicles in each source ID
#category that correspond to the relevant regulatory classes, and take an average.
#First identify the regulatory classes corresponding to the "Commercial truck" MARKAL class
MOVES_comm_truck_reg_classes <- filter(MARKAL_MOVES_class,MARKAL_Class == "Commercial truck")$MOVES_Reg_Class

#Subset the table mapping the regulatory classes to MOVES source IDs
MOVES_comm_truck_reg_src_fractions <- MOVES_src_type_reg_class_fractions %>%
  filter(regClassID %in% MOVES_comm_truck_reg_classes & modelYearID %in% MOVES_trn_age_fractions_Yb$yearID)

#Subset the table containing vehicle populations by source ID based on the relevant regulatory classes
MOVES_comm_truck_srcID_wts <- MOVES_source_type_pop %>%
  filter(yearID %in% MOVES_trn_age_fractions_Yb$yearID & sourceTypeID %in%
           MOVES_comm_truck_reg_src_fractions$sourceTypeID) %>%
  #Match on the weights, which are the fractions of vehicles in each source ID category corresponding to the
  #relevant regulatory classes
  mutate(weight = sourceTypePopulation * MOVES_comm_truck_reg_src_fractions$stmyFraction[
    match(paste(yearID,sourceTypeID),
          vecpaste(MOVES_comm_truck_reg_src_fractions[c("modelYearID","sourceTypeID")]))])

#Subset the relevant age fraction data, and use the weights to compute the average age fractions
MOVES_comm_truck_age_fractions_Yb <- MOVES_trn_age_fractions_Yb %>%
  filter(sourceTypeID %in% MOVES_comm_truck_srcID_wts$sourceTypeID) %>%
  mutate(weight = MOVES_comm_truck_srcID_wts$weight[match(paste(sourceTypeID,yearID),
                                                          vecpaste(MOVES_comm_truck_srcID_wts[c("sourceTypeID","yearID")]))]) %>%
  group_by(yearID,Vintage) %>%
  summarise(ageFraction = weighted.mean(ageFraction,weight)) %>%
  mutate(Class = "Commercial truck")

#For MARKAL classes including SUVs, Minivans, Pickups, buses, and heavy duty trucks, data from multiple
#MOVES source categories were used, so the age fractions must be multiplied by shares of each source type ID 
#out of the total vehicle population of the source IDs used in the corresponding MARKAL class. This will
#be done in a big loop 

MARKAL_class_loop_list <- c("Minivan","Pickup","Large SUV","Small SUV","Bus","Heavy duty long haul truck",
                            "Heavy duty short haul truck")

MOVES_age_fractions_list <- list()

for(i in 1:length(MARKAL_class_loop_list)){
  MOVES_srcIDs <- filter(MARKAL_MOVES_class,MARKAL_Class == MARKAL_class_loop_list[i])$MOVES_Source_Type
  
  #First compute the total vehicle population for these source IDs in the relevant years
  MOVES_agg_pop <- MOVES_source_type_pop %>%
    filter(sourceTypeID %in% MOVES_srcIDs & yearID %in% MOVES_trn_age_fractions_Yb$yearID) %>%
    group_by(yearID) %>%
    summarise(sourceTypePopulation = sum(sourceTypePopulation))
  
  #Compute shares of the source ID categories out of the corresponding total vehicle population
  MOVES_pop_shares <- MOVES_source_type_pop %>%
    filter(sourceTypeID %in% MOVES_srcIDs & yearID %in% MOVES_trn_age_fractions_Yb$yearID) %>%
    mutate(share = sourceTypePopulation / MOVES_agg_pop$sourceTypePopulation[match(yearID,MOVES_agg_pop$yearID)])
  
  #Now multiply the source ID shares by the age fractions to yield new age fractions for the aggregate MARKAL category
  MOVES_age_fractions_list[[i]] <- MOVES_trn_age_fractions_Yb %>%
    filter(sourceTypeID %in% MOVES_srcIDs) %>%
    mutate(ageFraction = ageFraction * MOVES_pop_shares$share[match(paste(sourceTypeID,yearID),
                                                                    vecpaste(MOVES_pop_shares[c("sourceTypeID","yearID")]))]) %>%
    group_by(yearID,Vintage) %>%
    summarise(ageFraction = sum(ageFraction)) %>%
    repeat_and_add_vector("Class",MARKAL_class_loop_list[i])
}

#Bind the vehicle age fraction tables together from the list
MOVES_rest_age_fractions_Yb <- bind_rows(MOVES_age_fractions_list)

#Bind together the tables containing the age fractions of the LDVs in the base years
MARKAL_trn_age_fractions_Yb <- bind_rows(MOVES_rest_age_fractions_Yb,MOVES_car_age_fractions_Yb,
                                         MOVES_comm_truck_age_fractions_Yb) %>%
  select(-sourceTypeID) %>%
  rename(year = yearID) 

# 2b. LDVs
printlog("Light-duty vehicle emissions factors")
#Melt the raw data
MARKAL_LDV_EFs_gpm.melt <- MARKAL_LDV_EFs_gpm %>%
  gather(variable,value,-Class,-Fuel,-Vintage,convert=T) %>%
  separate(variable,into=c("pollutant","year","region"),sep="\\.",convert = T) %>%
  mutate(pollutant = gsub("PM2_5","PM2.5",pollutant)) %>%
  ###NOTE: filtering out some fuels for now for lack of efficiency/service demand data, and also the CO2 data
  filter(pollutant %!in% "CO2" & Fuel %!in% c("B20","PH10G","PH10E")) 

#Gather a table for use in calculating degradation of EFs for each vintage
L171.LDV_USA_emiss_degrates <- MARKAL_LDV_EFs_gpm.melt %>%
  filter(Vintage >= 2010 & Vintage != 2050 & !is.na(value)) 

#Clean up and subset base year vintages
MARKAL_LDV_EFs_gpm_Yb <- MARKAL_LDV_EFs_gpm.melt %>%
  filter(year %in% model_base_years & Vintage <= year) %>%
  filter(!(Vintage == 1990 & year ==2010)) %>%
  ###MISSING VALUES: emission factors pollutants with no data for ELC vehicles, and 2010 EFs for 1990 vintages
  na.omit()

#Match base year age fractions onto the table containing the EFs in the base years
MARKAL_LDV_EFs_gpm_Yb$ageFraction <- MARKAL_trn_age_fractions_Yb$ageFraction[match(
  vecpaste(MARKAL_LDV_EFs_gpm_Yb[c("Class","Vintage","year")]),
  vecpaste(MARKAL_trn_age_fractions_Yb[c("Class","Vintage","year")]))]

#The base year EFs will be weighted means of the vintaged data using the age fractions as weights
MARKAL_LDV_EFs_gpm_Yb.avg <- MARKAL_LDV_EFs_gpm_Yb %>%
  group_by(region,Class,Fuel,pollutant,year) %>%
  summarise(value = weighted.mean(value,ageFraction)) %>%
  mutate(year = paste("X",year,sep=""))

#Clean up and subset future vintages for emissions coefficients
MARKAL_LDV_EFs_gpm_Yf_NAs <- MARKAL_LDV_EFs_gpm.melt %>%
  filter(Vintage >= 2015 & Vintage == year & !is.na(value)) %>%
  mutate(year = paste("X",year,sep="")) %>%
  spread(year,value)

#Select each future vintage year with the data for its start year, and join this data to a new data frame
###NOTE: this is admittedly an atrocious method but it does the job
year_list <- list()
MARKAL_years <- seq(2015,2050,by=5)
for(i in 1:length(MARKAL_years)){
  year_list[[i]] <- filter(MARKAL_LDV_EFs_gpm_Yf_NAs,Vintage == MARKAL_years[i]) %>%
    select_("region","Class","Fuel","pollutant",paste("X",MARKAL_years[i],sep=""))
}
MARKAL_LDV_EFs_gpm_Yf <- full_join(year_list[[1]],year_list[[2]]) %>%
  full_join(year_list[[3]]) %>% full_join(year_list[[4]]) %>% full_join(year_list[[5]]) %>%
  full_join(year_list[[6]]) %>% full_join(year_list[[7]]) %>% full_join(year_list[[8]])

#Convert to long format for easy unit conversion
MARKAL_LDV_EFs_gpm_Yf.long <- gather(MARKAL_LDV_EFs_gpm_Yf, year,value,-region,-Class,-Fuel,-pollutant)

#Add on the base year emission factors
MARKAL_LDV_EFs_gpm_Y.long <- bind_rows(MARKAL_LDV_EFs_gpm_Yb.avg,MARKAL_LDV_EFs_gpm_Yf.long)

#Prepare efficiency data
MARKAL_LDV_eff_vmtMJ <- MARKAL_LDV_eff_vmtMJ %>%
  ###NOTE: there is no NG emissions data for LDVs and use 100mi range for EVs
  filter(Technology %in% c("Electric 100mi","Flex fuel","Flex fuel plugin-hybrid 20mi",
                           "Flex fuel plugin-hybrid 40mi", "ICE","Plugin-hybrid 20mi","Plugin-hybrid 40mi") &
           Fuel %!in% c("CNG or gasoline","CNG","LPG","LPG or gasoline")) %>%
  #Manually interpolate to add 2005
  mutate(X2005 = 2*X2010-X2015) %>%
  select(-Technology,-Unit) %>%
  gather(year,value,-Class,-Fuel) 

#Use MARKAL vehicle fuel intensities to convert to mass emissions per fuel input
MARKAL_LDV_EFs_tgEJ_Y.long <- MARKAL_LDV_EFs_gpm_Y.long %>%
  mutate(Fuel_name = MARKAL_fuel_name_code$Fuel_name[match(Fuel,MARKAL_fuel_name_code$Fuel_code)]) %>%
  mutate(value = value * MARKAL_LDV_eff_vmtMJ$value[match(paste(Class,Fuel_name,year),
                                                          vecpaste(MARKAL_LDV_eff_vmtMJ[c("Class","Fuel","year")]))]) %>%
  ###MISSING VALUES: where there is no fuel intensity data for Mini car fuels other than 
  ###gasoline or electricity. Remove for now
  na.omit()

#Import LDV CNG emission factors from GREET data and copy for all categories in the LDV table
GREET_LDV_CNG_EFs_tgEJ_Y <- GREET2014_LDV_CNG_EFs_tgEJ %>%
  repeat_and_add_vector("region",unique(MARKAL_LDV_EFs_tgEJ_Y.long$region)) %>%
  repeat_and_add_vector("Class",unique(MARKAL_LDV_EFs_tgEJ_Y.long$Class)) %>%
  repeat_and_add_vector("year",unique(MARKAL_LDV_EFs_tgEJ_Y.long$year)) %>%
  mutate(Fuel_name = "Natural Gas")

#Bind CNG data onto MARKAL table
#MARKAL_LDV_EFs_tgEJ_Y.melt <- bind_rows(MARKAL_LDV_EFs_tgEJ_Y.long, GREET_LDV_CNG_EFs_tgEJ_Y)
#Spread to wide format
MARKAL_LDV_EFs_tgEJ_Y <- bind_rows(MARKAL_LDV_EFs_tgEJ_Y.long, GREET_LDV_CNG_EFs_tgEJ_Y) %>%
  spread(key=year,value) %>%
  #MISSING VALUES: ELC Minivans in 2005 and all vehicles using E85 in 2005. Fill with 2010 data for now
  #Also fill in zeroes in 2005 with 2010 data
  mutate(X2005 = ifelse(is.na(X2005),X2010,
                        ifelse(X2005 == 0,X2010,X2005))) %>%
  select(-Fuel_name) %>%
  arrange(region,Class,Fuel,pollutant)

#Also convert table containing emission coefficient degradation paths
L171.LDV_USA_emiss_degrates_tgEJ <- L171.LDV_USA_emiss_degrates %>%
  mutate(Fuel_name = MARKAL_fuel_name_code$Fuel_name[match(Fuel,MARKAL_fuel_name_code$Fuel_code)],
         year=paste("X",year,sep="")) %>%
  mutate(value = value * MARKAL_LDV_eff_vmtMJ$value[match(paste(Class,Fuel_name,year),
                                                          vecpaste(MARKAL_LDV_eff_vmtMJ[c("Class","Fuel","year")]))]) %>%                                              
  ###MISSING VALUES: where there is no fuel intensity data for Mini car fuels other than 
  ###gasoline or electricity. Remove for now
  na.omit()

#Add a Vintage column to the CNG data and bind it to the table containing the degradation paths
GREET_LDV_CNG_EFs_tgEJ_Y.vintage <- GREET_LDV_CNG_EFs_tgEJ_Y %>%
  repeat_and_add_vector("Vintage",unique(L171.LDV_USA_emiss_degrates_tgEJ$Vintage))

L171.LDV_USA_emiss_degrates_tgEJ <- bind_rows(L171.LDV_USA_emiss_degrates_tgEJ,GREET_LDV_CNG_EFs_tgEJ_Y.vintage)

# 2c. HDVs
printlog("Heavy duty vehicle emission factors")
#Melt the raw data
MARKAL_HDV_EFs_gpm.melt <- MARKAL_HDV_EFs_gpm %>%
  gather(variable,value,-Class,-Fuel,-Vintage,convert=T) %>%
  separate(variable,into=c("pollutant","year","region"),sep="\\.",convert=T) %>%
  mutate(pollutant = gsub("PM2_5","PM2.5",pollutant)) %>%
  ###NOTE: filtering out fuels with no intensity data, and also the CO2 data
  filter(pollutant %!in% "CO2" & Fuel %in% c("B0","B20","CNG"))

#Gather a table for use in calculating degradation of EFs for each vintage
L171.HDV_USA_emiss_degrates <- MARKAL_HDV_EFs_gpm.melt %>%
  filter(Vintage >= 2010 & Vintage != 2050 & !is.na(value))

#Clean up and subset base year vintages
MARKAL_HDV_EFs_gpm_Yb <- MARKAL_HDV_EFs_gpm.melt %>%
  filter(year %in% model_base_years & Vintage <= year) %>%
  filter(!(year == 2010 & Vintage == 1990)) %>%
  na.omit()

#Match base year age fractions onto the table containing the EFs in the base years
MARKAL_HDV_EFs_gpm_Yb$ageFraction <- MARKAL_trn_age_fractions_Yb$ageFraction[match(
  vecpaste(MARKAL_HDV_EFs_gpm_Yb[c("Class","Vintage","year")]),
  vecpaste(MARKAL_trn_age_fractions_Yb[c("Class","Vintage","year")]))]

#The base year EFs will be weighted means of the vintaged data using the age fractions as weights
MARKAL_HDV_EFs_gpm_Yb.avg <- MARKAL_HDV_EFs_gpm_Yb %>%
  group_by(region,Class,Fuel,pollutant,year) %>%
  summarise(value = weighted.mean(value,ageFraction)) %>%
  mutate(year = paste("X",year,sep=""))

#Clean up and subset future vintages for emissions coefficients
MARKAL_HDV_EFs_gpm_Yf_NAs <- MARKAL_HDV_EFs_gpm.melt %>%
  filter(Vintage >= 2015 & Vintage == year & !is.na(value)) %>%
  mutate(year=paste("X",year,sep="")) %>%
  spread(year,value)

#Select each vintage year with the data for its start year, and join this data to a new data frame
###NOTE: this is admittedly an atrocious method but it does the job
year_list <- list()
for(i in 1:length(MARKAL_years)){
  year_list[[i]] <- filter(MARKAL_HDV_EFs_gpm_Yf_NAs,Vintage == MARKAL_years[i]) %>%
    select_("region","Class","Fuel","pollutant",paste("X",MARKAL_years[i],sep=""))
}
MARKAL_HDV_EFs_gpm_Yf <- full_join(year_list[[1]],year_list[[2]]) %>%
  full_join(year_list[[3]]) %>% full_join(year_list[[4]]) %>% full_join(year_list[[5]]) %>%
  full_join(year_list[[6]]) %>% full_join(year_list[[7]]) %>% full_join(year_list[[8]])

#Convert to long format
MARKAL_HDV_EFs_gpm_Yf.long <- MARKAL_HDV_EFs_gpm_Yf %>%
  gather(year,value,-region,-Class,-Fuel,-pollutant,convert=T)

#Add on the base year emission factors
MARKAL_HDV_EFs_gpm_Y.long <- bind_rows(MARKAL_HDV_EFs_gpm_Yb.avg,MARKAL_HDV_EFs_gpm_Yf.long)

#Prepare efficiency data
MARKAL_HDV_eff_vmtMJ <- MARKAL_HDV_eff_vmtMJ %>%
  ###NOTE: no emissions data for hydrogen or LPG
  filter(Fuel %in% c("B0","Biodiesel (B20)","CNG") & Technology %in% c("Existing","Conventional","Flex fuel")) %>%
  select(-Technology,-Unit) %>%
  gather(year,value,-Class,-Fuel,convert=T) %>%
  filter(!is.na(value)) %>%
  spread(year,value)

#Fill in 2005 data using manual linear interpolation
MARKAL_HDV_eff_vmtMJ[!complete.cases(MARKAL_HDV_eff_vmtMJ),"X2005"] <- 2 *
  MARKAL_HDV_eff_vmtMJ[!complete.cases(MARKAL_HDV_eff_vmtMJ),"X2010"] - 
  MARKAL_HDV_eff_vmtMJ[!complete.cases(MARKAL_HDV_eff_vmtMJ),"X2015"]

#Melt the HDV efficiency data for easy matching
MARKAL_HDV_eff_vmtMJ.melt <- gather(MARKAL_HDV_eff_vmtMJ,year,value,-Class,-Fuel)

#Use MARKAL vehicle fuel intensities to convert to mass emissions per fuel input
MARKAL_HDV_EFs_tgEJ_Y.long <- MARKAL_HDV_EFs_gpm_Y.long %>%
  mutate(Fuel_name = MARKAL_fuel_name_code$Fuel_name[match(Fuel,MARKAL_fuel_name_code$Fuel_code)]) %>%
  mutate(value = value * MARKAL_HDV_eff_vmtMJ.melt$value[match(paste(Class,Fuel_name,year),
                                                               vecpaste(MARKAL_HDV_eff_vmtMJ.melt[c("Class","Fuel","year")]))])
                                                                
#Spread to wide format 
MARKAL_HDV_EFs_tgEJ_Y <- MARKAL_HDV_EFs_tgEJ_Y.long %>%
  spread(key=year,value) %>% 
  select(-Fuel_name) %>%
  arrange(region,Class,Fuel,pollutant)

#Also convert table containing emission coefficient degradation paths
L171.HDV_USA_emiss_degrates_tgEJ <- L171.HDV_USA_emiss_degrates %>%
  mutate(Fuel_name = MARKAL_fuel_name_code$Fuel_name[match(Fuel,MARKAL_fuel_name_code$Fuel_code)],
         year=paste("X",year,sep="")) %>%
  mutate(value = value * MARKAL_HDV_eff_vmtMJ.melt$value[match(paste(Class,Fuel_name,year),
                                                               vecpaste(MARKAL_HDV_eff_vmtMJ.melt[c("Class","Fuel","year")]))])
                                                                
#2d. Apportion to states
#Combine LDV and HDV tables
MARKAL_trn_EFs_tgEJ_Y <- bind_rows(MARKAL_LDV_EFs_tgEJ_Y,MARKAL_HDV_EFs_tgEJ_Y)
L171.trn_USA_emiss_degrates <- bind_rows(L171.HDV_USA_emiss_degrates_tgEJ,L171.LDV_USA_emiss_degrates_tgEJ)

#For now keep the degradation rates at the census region level to save time in level2 processing
L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y <- L171.trn_USA_emiss_degrates %>%
  mutate(year = as.numeric(substr(year,2,5))) %>%
  rename(class=Class, fuel=Fuel, vintage = Vintage) %>%
  select(region, class, fuel, vintage, pollutant, year, value) %>%
  arrange(region, class, fuel, vintage, pollutant)

#Write the emission factors to each state, assuming they are uniform in their census regions
EF_region_list <- list()
for(i in unique(MARKAL_trn_EFs_tgEJ_Y$region)){
  EF_region_list[[i]] <- repeat_and_add_vector(
  select(filter(MARKAL_trn_EFs_tgEJ_Y,region==i),-region), "state",
  filter(state_census_region,census_region_id == i)$state)
}
L171.nonco2_tgej_state_trn_SMarkal_F_Y <- bind_rows(EF_region_list) %>%
  rename(class = Class, fuel = Fuel)

L171.nonco2_tgej_state_trn_SMarkal_F_Y <- L171.nonco2_tgej_state_trn_SMarkal_F_Y[
  c("state","class","fuel","pollutant","X2005","X2010",X_future_years[1:8])] %>%
  arrange(state,class,fuel,pollutant)

stopifnot(!any(is.na(L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y)))
stopifnot(!any(is.na(L171.nonco2_tgej_state_trn_SMarkal_F_Y)))

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y <- c( "Transportation non-co2 emission factor by U.S. census region / MARKAL vehicle class / fuel / vintage / pollutant / year","Unit = Tg / EJ" )
comments.L171.nonco2_tgej_state_trn_SMarkal_F_Y <- c( "Transportation non-co2 emission factor by U.S. state / MARKAL vehicle class / fuel / pollutant / year","Unit = Tg / EJ" )

#write tables as CSV files
writedata( L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y, domain="GCAMUSA_LEVEL1_DATA", fn="L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y", comments=comments.L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y )
writedata( L171.nonco2_tgej_state_trn_SMarkal_F_Y, domain="GCAMUSA_LEVEL1_DATA", fn="L171.nonco2_tgej_state_trn_SMarkal_F_Y", comments=comments.L171.nonco2_tgej_state_trn_SMarkal_F_Y )

# Every script should finish with this line
logstop()




