## This scripts read in and process UCS EW3 Database - Main Data
## Author: Lu Liu
## Date of creation: March 28th, 2017

if( !exists( "GCAMUSAPROC_DIR" ) ){
  if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
    GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
  } else {
    stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
  }
}

mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}


# -----------------------------------------------------------------------------
# 1. Read files
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "LA.Process_UCS_data_ref.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )

file <- readdata( "GCAMUSA_LEVEL0_DATA", "UCS_Database", skip = 1 )
file.df <- data.frame(file)
# Remove data with NA input
file.df <- subset(file.df,file.df$Fuel!="#N/A")
file.df <- subset(file.df,file.df$Reported.Water.Source..Type.!="Unknown")
# Change water type to freshwater and seawater only  
file.df$Reported.Water.Source..Type. <- mgsub(c("Surface Water","Municipal","Groundwater","Unknown Freshwater",
       "Waste Water","GW/Waste Water","GW/Surface Water",
       "GW/Municipal","GW/fresh"),rep("fresh",9),file.df$Reported.Water.Source..Type.)
file.df$Reported.Water.Source..Type. <- mgsub(c("Ocean","Unknown Ocean","Unknown seawater"),rep("seawater",3),file.df$Reported.Water.Source..Type.)

# Change syntax to match GCAM syntax
file.df$Fuel <- gsub("Coal", "coal", file.df$Fuel)
file.df$Fuel <- gsub("Hydropower", "hydro", file.df$Fuel)
file.df$Fuel <- gsub("Natural Gas", "gas", file.df$Fuel)
file.df$Fuel <- gsub("Oil", "refined liquids", file.df$Fuel)
file.df$Fuel <- gsub("Nuclear", "nuclear", file.df$Fuel)
file.df$Fuel <- gsub("Solar", "solar", file.df$Fuel)
file.df$Fuel <- gsub("Geothermal", "geothermal", file.df$Fuel)
file.df$Fuel <- gsub("Biomass", "biomass", file.df$Fuel)
file.df$Fuel <- gsub("Wind", "wind", file.df$Fuel)

file.df$Cooling.Technology <- gsub("Once-Through", "once through", file.df$Cooling.Technology)
file.df$Cooling.Technology <- gsub("None", "none", file.df$Cooling.Technology)
file.df$Cooling.Technology <- gsub("Recirculating", "recirculating", file.df$Cooling.Technology)
file.df$Cooling.Technology <- gsub("Dry Cooled", "dry cooling", file.df$Cooling.Technology)
file.df$Cooling.Technology <- gsub("Cooling Pond", "cooling pond", file.df$Cooling.Technology)

# Read in state to NEMS region mapping
mapping <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
mapping.df <- data.frame(cbind(mapping$state,mapping$NEMS))
colnames(mapping.df) <- c("state", "NEMS")

# Create a data frame for complete set of technology
elec_tech_water_map <- readdata("GCAMUSA_MAPPINGS", "elec_tech_water_map" )
complete_tech <- subset(elec_tech_water_map, select = c("plant_type","cooling_system","water_type","fuel","technology"))
# Remove duplicates
complete_tech <- complete_tech[!duplicated(complete_tech), ]

complete_tech <- do.call("rbind",replicate(51,complete_tech,simplif = FALSE))

complete_tech["State"]<- sort(rep(unique(file.df$State),90)) # 90 rows of technology combination
complete_tech$NEMS <- mapping.df[match(complete_tech$State,mapping.df$state),"NEMS"]

# Create a matrix to record cooling share data later
cooling_share <- matrix(data=NA, ncol = 39, nrow = 51*90)
cooling_share_future <- matrix(data=NA, ncol = 3, nrow = 51*90)

# Read in table of states that have offshore wind
L120.RsrcCurves_EJ_R_offshore_wind_USA <- readdata( "GCAMUSA_LEVEL1_DATA", "L120.RsrcCurves_EJ_R_offshore_wind_USA", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations

offshore_wind_states <- unique(L120.RsrcCurves_EJ_R_offshore_wind_USA$region)

for (year in 1970:2008)
{
  file.df.data <- subset(file.df,file.df$First.Year.of.Operation <= year)
                  
  # Aggregate capacity by state, fuel and cooling tech
  capacity_tech <- aggregate(file.df.data$Nameplate.Capacity..MW.,
          by = list(file.df.data$State,
                    file.df.data$Fuel,
                    file.df.data$Generation.Technology,
                    file.df.data$Cooling.Technology,
                    file.df.data$Reported.Water.Source..Type.),
          sum)
  capacity_tech["Fuel"] <- capacity_tech$Group.2 # Copy fuel names to a new column
  
  # Change fuel names to GCAM syntax

  capacity_tech$Group.2 <- gsub("biomass", "fossil non-coal",capacity_tech$Group.2)

  capacity_tech$Group.2 <- replace(as.character(capacity_tech$Group.2),capacity_tech$Group.2=="gas"& capacity_tech$Group.3=="CC",
          "combined cycle")
  capacity_tech$Group.2 <- replace(as.character(capacity_tech$Group.2),capacity_tech$Group.2=="refined liquids"& capacity_tech$Group.3=="CC",
                                   "CC")
  capacity_tech$Group.2 <- replace(as.character(capacity_tech$Group.2),capacity_tech$Group.2=="solar"& capacity_tech$Group.3=="SU",
                                   "CSP")
  capacity_tech$Group.2 <- gsub("solar", "no cooling", capacity_tech$Group.2)
  capacity_tech$Group.2 <- gsub("hydro" , "no cooling",capacity_tech$Group.2)
  capacity_tech$Group.2 <- gsub("wind" , "no cooling",capacity_tech$Group.2)
  
  # Exclude conbustion turbine technology and wind (no water requirment)
  capacity_tech <- subset(capacity_tech,capacity_tech$Group.3!= "CT")
  capacity_tech <- subset(capacity_tech,capacity_tech$Group.3!= "WN")
  
  # Rename ST NG and Oil to fossil non-coal
  capacity_tech$Group.2 <- gsub("gas", "fossil non-coal",capacity_tech$Group.2)
  capacity_tech$Group.2 <- gsub("refined liquids","fossil non-coal",capacity_tech$Group.2)
  
  # Mannually correct some mistake in the UCS data (some dry cooling power plants are
  # mistakenly assigned Ocean water source)
  capacity_tech[capacity_tech$Group.4=="dry cooling","Group.5"] = "fresh"
  
  # Sort data by state, fuel and cooling tech
  capacity_tech <- capacity_tech[order(capacity_tech$Group.1,
                                       capacity_tech$Group.2,
                                       capacity_tech$Group.4,
                                       capacity_tech$Group.5,
                                       capacity_tech$Fuel),] 
  # Re-aggregate capacity by state, fuel and cooling tech
  capacity_tech <- aggregate(capacity_tech$x,
                             by = list(capacity_tech$Group.1,
                                       capacity_tech$Group.2,
                                       capacity_tech$Group.4,
                                       capacity_tech$Group.5,
                                       capacity_tech$Fuel
                                       ),
                             sum)
  # Aggregate capacity by state and fuel 
  capacity_tech_state <- aggregate(capacity_tech$x,
                             by = list(capacity_tech$Group.1,
                                       capacity_tech$Group.2,
                                       capacity_tech$Group.5),
                             sum)
  
  # Compute share of cooling by state and fuel
  capacity_tech$x <- capacity_tech$x / 
                    capacity_tech_state[match(paste(as.character(capacity_tech$Group.1),as.character(capacity_tech$Group.2),as.character(capacity_tech$Group.5)),
                                              paste(as.character(capacity_tech_state$Group.1), as.character(capacity_tech_state$Group.2),as.character(capacity_tech_state$Group.3)),
                                              nomatch = NA_integer_),
                                        "x"]
  
  # Reorder the table
  capacity_tech <- capacity_tech[order(capacity_tech$Group.1,
                                       capacity_tech$Group.2,
                                       capacity_tech$Group.3,
                                       capacity_tech$Group.4,
                                       capacity_tech$Group.5),] 

  # Find corresponding cooling share and add it to the complete tech list
  cooling_share[,year-1969] <- capacity_tech[match(paste(as.character(complete_tech$State),complete_tech$plant_type,
                  complete_tech$cooling_system,complete_tech$water_type,complete_tech$fuel),
            paste(as.character(capacity_tech$Group.1),as.character(capacity_tech$Group.2),
                  as.character(capacity_tech$Group.3),as.character(capacity_tech$Group.4),
                  as.character(capacity_tech$Group.5)),
            nomatch = NA_integer_),"x"]
}
# Replace all NA with 0
cooling_share[is.na(cooling_share)] <- 0
# Convert to data frame
cooling_share.df <- data.frame(cooling_share)
colnames(cooling_share.df) <- seq(1970,2008)


# Create cooling share for future years
file.df.data_future <- subset(file.df,file.df$First.Year.of.Operation > 1999)
file.df.data_future[,"NEMS"] <- mapping.df[match(file.df.data_future$State,mapping.df$state),"NEMS"]

# Aggregate capacity by NEMS, fuel and cooling tech
capacity_tech_future <- aggregate(file.df.data_future$Nameplate.Capacity..MW.,
                             by = list(file.df.data_future$NEMS,
                                       file.df.data_future$Fuel,
                                       file.df.data_future$Generation.Technology,
                                       file.df.data_future$Cooling.Technology,
                                       file.df.data_future$Reported.Water.Source..Type.),
                             sum)
capacity_tech_future["Fuel"] <- capacity_tech_future$Group.2
  
# Change fuel names to GCAM syntax
capacity_tech_future$Group.2 <- gsub("biomass" , "fossil non-coal",capacity_tech_future$Group.2)
capacity_tech_future$Group.2 <- replace(as.character(capacity_tech_future$Group.2),capacity_tech_future$Group.2=="gas"& capacity_tech_future$Group.3=="CC",
                                   "combined cycle")
capacity_tech_future$Group.2 <- replace(as.character(capacity_tech_future$Group.2),capacity_tech_future$Group.2=="refined liquids"& capacity_tech_future$Group.3=="CC",
                                   "CC")
capacity_tech_future$Group.2 <- replace(as.character(capacity_tech_future$Group.2),capacity_tech_future$Group.2=="solar"& capacity_tech_future$Group.3=="SU",
                                   "CSP")
capacity_tech_future$Group.2 <- gsub("solar", "no cooling", capacity_tech_future$Group.2)
capacity_tech_future$Group.2 <- gsub("hydro", "no cooling", capacity_tech_future$Group.2)
capacity_tech_future$Group.2 <- gsub("wind", "no cooling", capacity_tech_future$Group.2)
  

# Exclude conbustion turbine technology and wind (no water requirment)
capacity_tech_future <- subset(capacity_tech_future,capacity_tech_future$Group.3!= "CT")
capacity_tech_future <- subset(capacity_tech_future,capacity_tech_future$Group.3!= "WN")
  
# Rename ST NG and Oil to fossil non-coal
capacity_tech_future$Group.2 <- gsub("gas", "fossil non-coal", capacity_tech_future$Group.2)
capacity_tech_future$Group.2 <- gsub("refind liquids", "fossil non-coal", capacity_tech_future$Group.2)

# Mannually correct some mistake in the UCS data (some dry cooling power plants are
# mistakenly assigned Ocean water source)
capacity_tech_future[capacity_tech_future$Group.3=="dry cooling","Group.4"] = "fresh"
  
# Sort data by NEMS, fuel and cooling tech
capacity_tech_future <- capacity_tech_future[order(capacity_tech_future$Group.1,
                                       capacity_tech_future$Group.2,
                                       capacity_tech_future$Group.3,
                                       capacity_tech_future$Group.4,
                                       capacity_tech_future$Group.5,
                                       capacity_tech_future$Fuel),] 
# Re-aggregate capacity by NEMS, fuel and cooling tech
capacity_tech_future <- aggregate(capacity_tech_future$x,
                             by = list(capacity_tech_future$Group.1,
                                       capacity_tech_future$Group.2,
                                       capacity_tech_future$Group.4,
                                       capacity_tech_future$Group.5,
                                       capacity_tech_future$Fuel
                                       ),
                             sum)
# Aggregate capacity by NEMS and fuel
capacity_tech_state_future <- aggregate(capacity_tech_future$x,
                                   by = list(capacity_tech_future$Group.1,
                                             capacity_tech_future$Group.2,
                                             capacity_tech_future$Group.5),
                                   sum)
  
# Compute share of cooling by fuel only
capacity_tech_future_US <- aggregate(capacity_tech_future$x,
                                     by = list(
                                       capacity_tech_future$Group.5,
                                       capacity_tech_future$Group.3,
                                       capacity_tech_future$Group.4
                                     ),
                                     sum)
capacity_tech_future_US_total <- aggregate(capacity_tech_future$x,
                                           by = list(capacity_tech_future$Group.5),
                                           sum)
capacity_tech_future_US$x <- capacity_tech_future_US$x / 
  capacity_tech_future_US_total[
    match(capacity_tech_future_US$Group.1,capacity_tech_future_US_total$Group.1),"x"
    ]

# Compute share of cooling by NEMS and fuel and plant type
capacity_tech_future$x <- capacity_tech_future$x / 
capacity_tech_state_future[match(paste(as.character(capacity_tech_future$Group.1),as.character(capacity_tech_future$Group.2),as.character(capacity_tech_future$Group.5)),
                                  paste(as.character(capacity_tech_state_future$Group.1), as.character(capacity_tech_state_future$Group.2),as.character(capacity_tech_state_future$Group.3)),
                                  nomatch = NA_integer_),
                        "x"]

# Create a data frame for complete set of technology for NEMS
complete_tech_US <- subset(elec_tech_water_map, select = c("plant_type","cooling_system","water_type","fuel","technology"))
complete_tech_US <- complete_tech_US[!duplicated(complete_tech_US), ]

complete_tech_US <- do.call("rbind",replicate(13,complete_tech_US,simplif = FALSE)) #13 NEMS regions

complete_tech_US["NEMS"] <- rep(unique(mapping.df$NEMS),90)

# Match NEMS cooling share to the complete tech NEMS table
complete_tech_US$x <- capacity_tech_future[match(paste(complete_tech_US$NEMS,complete_tech_US$plant_type,
                                     complete_tech_US$cooling_system,complete_tech_US$water_type,
                                     complete_tech_US$fuel),
                               paste(capacity_tech_future$Group.1,capacity_tech_future$Group.2,
                                     capacity_tech_future$Group.3,capacity_tech_future$Group.4,
                                     capacity_tech_future$Group.5),
                               nomatch = NA_integer_),
                     "x"]

complete_tech_US$y <- capacity_tech_future_US[match(paste(complete_tech_US$fuel,
                                                       complete_tech_US$cooling_system,complete_tech_US$water_type),
                                                 paste(capacity_tech_future_US$Group.1,capacity_tech_future_US$Group.2,
                                                       capacity_tech_future_US$Group.3),
                                                 nomatch = NA_integer_),
                                           "x"]


complete_tech_US[is.na(complete_tech_US$x),"x"] <- 0
complete_tech_US[is.na(complete_tech_US$y),"y"] <- 0

manual_mapping_x <- aggregate(complete_tech_US$x,
          by = list(complete_tech_US$NEMS,
                    complete_tech_US$fuel,
                    complete_tech_US$plant_type),
          sum)
index_x <- match(paste(complete_tech_US$NEMS,complete_tech_US$fuel,complete_tech_US$plant_type),
               paste(manual_mapping_x$Group.1,manual_mapping_x$Group.2,manual_mapping_x$Group.3))


manual_mapping_y <- aggregate(complete_tech_US$y,
                            by = list(complete_tech_US$NEMS,
                                      complete_tech_US$fuel),
                            sum)
index_y <- match(paste(complete_tech_US$NEMS,complete_tech_US$fuel),
               paste(manual_mapping_y$Group.1,manual_mapping_y$Group.2))


for (i in 1:length(complete_tech_US[,1])){
  if (manual_mapping_x[index_x[i],"x"] == 0 && manual_mapping_y[index_y[i],"x"] == 0){
    complete_tech_US[i,"z"] = 1}
  else if (manual_mapping_x[index_x[i],"x"] == 0 && manual_mapping_y[index_y[i],"x"] > 0){
        complete_tech_US[i,"z"] = complete_tech_US[i,"y"]
           }
      else
           {
             complete_tech_US[i,"z"] = complete_tech_US[i,"x"]
           }
}

complete_tech_US <- complete_tech_US[order(complete_tech_US$NEMS,
                                           complete_tech_US$plant_type,
                                           complete_tech_US$cooling_system,
                                           complete_tech_US$water_type,
                                           complete_tech_US$fuel,
                                           complete_tech_US$technology), ]

# Mannually correct mismatch syntax
complete_tech_US[complete_tech_US$plant_type=="no cooling","cooling_system"] = "none"
complete_tech_US[complete_tech_US$plant_type=="no cooling","water_type"] = "fresh"
complete_tech_US <- unique(complete_tech_US)


# Reorder the table
capacity_tech_future <- capacity_tech_future[order(capacity_tech_future$Group.1,
                                       capacity_tech_future$Group.2,
                                       capacity_tech_future$Group.3,
                                       capacity_tech_future$Group.4,
                                       capacity_tech_future$Group.5), ] 


  
# Find corresponding cooling share and add it to the complete tech list
cooling_share_future[,1] <- complete_tech_US[match(paste(as.character(complete_tech$NEMS),complete_tech$plant_type,
                                                         complete_tech$cooling_system,complete_tech$water_type,
                                                         complete_tech$fuel),
                                                   paste(as.character(complete_tech_US$NEMS),as.character(complete_tech_US$plant_type),
                                                         as.character(complete_tech_US$cooling_system),as.character(complete_tech_US$water_type),
                                                         as.character(complete_tech_US$fuel)),
                                                   nomatch = NA_integer_),"z"]
cooling_share_future[,2] <- cooling_share_future[,1]
cooling_share_future[,3] <- cooling_share_future[,1]

# Replace all NA with 0
cooling_share_future[is.na(cooling_share_future)] <- 0
# Convert to data frame
cooling_share_future.df <- data.frame(cooling_share_future)
colnames(cooling_share_future.df) <- c(2010,2020,2100)


# Combine the two data frames
complete_tech_cooling_share <- cbind(complete_tech,cooling_share.df,cooling_share_future.df)

# Mannually misvalue nuclear cooling share so that it will be fixed in the for loop
complete_tech_cooling_share[complete_tech_cooling_share$fuel=="nuclear","2010"] <- complete_tech_cooling_share[complete_tech_cooling_share$fuel=="nuclear","2008"]+2

# Create a mapping for future technology
fuel_mapping <- data.frame(matrix(NA, ncol = , nrow = 6))
colnames(fuel_mapping) <- c("Future_fuel")
fuel_mapping$Future_fuel <- c("CC (CCS)", "combined cycle (CCS)",
                              "conv (CCS)", "conv pul (CCS)",
                              "IGCC","IGCC (CCS)")


for (i in 1:length(complete_tech_cooling_share[,1]))
{
  if (is.element(complete_tech_cooling_share[i,"plant_type"],fuel_mapping$Future_fuel) == T)
  {
    complete_tech_cooling_share[i,"2010"] <- complete_tech_cooling_share[match(paste(complete_tech_cooling_share$State[i],
                                            complete_tech_cooling_share$fuel[i],
                                            complete_tech_cooling_share$cooling_system[i],
                                            complete_tech_cooling_share$water_type[i]),
                                      paste(complete_tech_cooling_share$State,complete_tech_cooling_share$fuel,
                                            complete_tech_cooling_share$cooling_system,complete_tech_cooling_share$water_type)),
                                "2010"]
    complete_tech_cooling_share[i,"2020"] <- complete_tech_cooling_share[i,"2010"]
    complete_tech_cooling_share[i,"2100"] <- complete_tech_cooling_share[i,"2010"]
  }

    
}

# Mannualy fix the issue of more once-through cooling in the future for some states 
# and technology because of limited data between 2000-2008

#index <- rownames(complete_tech_cooling_share[complete_tech_cooling_share[complete_tech_cooling_share$water_type=="fresh"&complete_tech_cooling_share$cooling_system=="once through","2010"] >
  #complete_tech_cooling_share[complete_tech_cooling_share$water_type=="fresh"&complete_tech_cooling_share$cooling_system=="once through","2008"],])

problem_set <- complete_tech_cooling_share[complete_tech_cooling_share$water_type=="fresh"&complete_tech_cooling_share$cooling_system=="once through",]

index <- rownames(problem_set[problem_set[,"2010"] > problem_set[,"2008"],])


for (i in 1:length(index))
{

  # Mannually change the recirculating cooling share corresponding to the same fuel technology to 1-whatever's remained
  recir <- match(paste(complete_tech_cooling_share[index[i],"plant_type"],complete_tech_cooling_share[index[i],"water_type"],
                       complete_tech_cooling_share[index[i],"fuel"],complete_tech_cooling_share[index[i],"State"],"recirculating",complete_tech_cooling_share[index[i],"technology"]),
                 paste(complete_tech_cooling_share$plant_type,complete_tech_cooling_share$water_type,
                       complete_tech_cooling_share$fuel,complete_tech_cooling_share$State,complete_tech_cooling_share$cooling_system,complete_tech_cooling_share$technology))  
  
  saline <- match(paste(complete_tech_cooling_share[index[i],"plant_type"],"seawater",
                        complete_tech_cooling_share[index[i],"fuel"],complete_tech_cooling_share[index[i],"State"],"once through",complete_tech_cooling_share[index[i],"technology"]),
                 paste(complete_tech_cooling_share$plant_type,complete_tech_cooling_share$water_type,
                       complete_tech_cooling_share$fuel,complete_tech_cooling_share$State,complete_tech_cooling_share$cooling_system,complete_tech_cooling_share$technology))  
  
  dry <- match(paste(complete_tech_cooling_share[index[i],"plant_type"],"fresh",
                     complete_tech_cooling_share[index[i],"fuel"],complete_tech_cooling_share[index[i],"State"],"dry cooling",complete_tech_cooling_share[index[i],"technology"]),
                  paste(complete_tech_cooling_share$plant_type,complete_tech_cooling_share$water_type,
                        complete_tech_cooling_share$fuel,complete_tech_cooling_share$State,complete_tech_cooling_share$cooling_system,complete_tech_cooling_share$technology))  
  pond <- match(paste(complete_tech_cooling_share[index[i],"plant_type"],"fresh",
                      complete_tech_cooling_share[index[i],"fuel"],complete_tech_cooling_share[index[i],"State"],"cooling pond",complete_tech_cooling_share[index[i],"technology"]),
               paste(complete_tech_cooling_share$plant_type,complete_tech_cooling_share$water_type,
                     complete_tech_cooling_share$fuel,complete_tech_cooling_share$State,complete_tech_cooling_share$cooling_system,complete_tech_cooling_share$technology))  
  onceT <- match(paste(complete_tech_cooling_share[index[i],"plant_type"],"fresh",
                       complete_tech_cooling_share[index[i],"fuel"],complete_tech_cooling_share[index[i],"State"],"once through",complete_tech_cooling_share[index[i],"technology"]),
                paste(complete_tech_cooling_share$plant_type,complete_tech_cooling_share$water_type,
                      complete_tech_cooling_share$fuel,complete_tech_cooling_share$State,complete_tech_cooling_share$cooling_system,complete_tech_cooling_share$technology))  
  
# Cooling share assumption slight differs for nuclear
  if (complete_tech_cooling_share[recir,"fuel"] == "nuclear") {
    complete_tech_cooling_share[recir,"2020"] <- 0.9
  }else{
  complete_tech_cooling_share[recir,"2020"] <- 0.85}
  # Mannually change these once-through cooling share to 0 in 2100 
  complete_tech_cooling_share[saline,"2020"] <- 0.05
  if (is.na(dry) == T){
    print("Nuclear no dry cooling")
  }else{
  complete_tech_cooling_share[dry,"2020"] <- 0.05} 
  
  complete_tech_cooling_share[pond,"2020"] <- 0.05 
  complete_tech_cooling_share[onceT,"2020"] <- 0 

}
complete_tech_cooling_share[,"2010"] <- complete_tech_cooling_share[,"2008"]
complete_tech_cooling_share[,"2100"] <- complete_tech_cooling_share[,"2020"]


# Mannually assume all hydro power have cooling share of 1
complete_tech_cooling_share[complete_tech_cooling_share$fuel=="hydro",8:48] <- 1

# Mannually assume all future CSP have cooling share of 1
complete_tech_cooling_share[complete_tech_cooling_share$plant_type=="CSP",48:49] <- 1 

# Mannually assume all historical Gen III have share of 0
complete_tech_cooling_share[complete_tech_cooling_share$technology=="Gen_III",8:47] <- 0 

# Mannually assume all wind_offshore have cooling share of 1
complete_tech_cooling_share %>%
  filter(technology == "wind") %>%
  filter(State %in% offshore_wind_states) %>%
  mutate(technology = "wind_offshore") -> offshore_wind_cooling_share

complete_tech_cooling_share %>%
  bind_rows(offshore_wind_cooling_share) -> complete_tech_cooling_share

# -----------------------------------------------------------------------------
# 3. Write all output
writedata( complete_tech_cooling_share, domain="GCAMUSA_LEVEL1_DATA", fn="LA1233.CoolingSystemShares_RG3_ref")

logstop()






