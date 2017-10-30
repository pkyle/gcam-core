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
logstart( "LA.Process_UCS_data_frozen.R" )
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
# -----------------------------------------------------------------------------
# 2. Perform computations

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
  capacity_tech["NEMS"] <- mapping.df[match(capacity_tech$Group.1,mapping.df$state),"NEMS"]
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
  capacity_tech_hist <- aggregate(capacity_tech$x,
                                  by = list(capacity_tech$Group.1,
                                            capacity_tech$Group.2,
                                            capacity_tech$Group.4,
                                            capacity_tech$Group.5,
                                            capacity_tech$Fuel
                                  ),
                                  sum)
  
  # Re-aggregate capacity by NEMS, fuel and cooling tech
  capacity_tech_future <- aggregate(capacity_tech$x,
                                    by = list(capacity_tech$NEMS,
                                              capacity_tech$Group.4,
                                              capacity_tech$Group.5,
                                              capacity_tech$Fuel
                                    ),
                                    sum)
  # Re-aggregate capacity by fuel and cooling tech
  capacity_tech_future_nation <- aggregate(capacity_tech$x,
                                           by = list(
                                             capacity_tech$Group.4,
                                             capacity_tech$Group.5,
                                             capacity_tech$Fuel
                                           ),
                                           sum)
  
  
  # Aggregate capacity by state and fuel and technology
  capacity_tech_state <- aggregate(capacity_tech_hist$x,
                                   by = list(capacity_tech_hist$Group.1,
                                             capacity_tech_hist$Group.2,
                                             capacity_tech_hist$Group.5),
                                   sum)
  
  # Aggregate capacity by NEMS and fuel and technology
  capacity_tech_NEMS <- aggregate(capacity_tech_future$x,
                                  by = list(capacity_tech_future$Group.1,
                                            capacity_tech_future$Group.4),
                                  sum)
  # Aggregate capacity by fuel and technology
  capacity_tech_nation <- aggregate(capacity_tech_future_nation$x,
                                    by = list(capacity_tech_future_nation$Group.3),
                                    sum)
  
  
  
  # Compute share of cooling by state and fuel and technology
  capacity_tech_hist$x <- capacity_tech_hist$x / 
    capacity_tech_state[match(paste(as.character(capacity_tech_hist$Group.1),as.character(capacity_tech_hist$Group.2),as.character(capacity_tech_hist$Group.5)),
                              paste(as.character(capacity_tech_state$Group.1), as.character(capacity_tech_state$Group.2),as.character(capacity_tech_state$Group.3)),
                              nomatch = NA_integer_),
                        "x"]
  
  # Reorder the table
  capacity_tech_hist <- capacity_tech_hist[order(capacity_tech_hist$Group.1,
                                                 capacity_tech_hist$Group.2,
                                                 capacity_tech_hist$Group.3,
                                                 capacity_tech_hist$Group.4,
                                                 capacity_tech_hist$Group.5),] 
  
  
  # Compute share of cooling by NEMS and fuel 
  capacity_tech_future$x <- capacity_tech_future$x / 
    capacity_tech_NEMS[match(paste(as.character(capacity_tech_future$Group.1),as.character(capacity_tech_future$Group.4)),
                             paste(as.character(capacity_tech_NEMS$Group.1), as.character(capacity_tech_NEMS$Group.2)),
                             nomatch = NA_integer_),
                       "x"]
  
  # Reorder the table
  capacity_tech_future <- capacity_tech_future[order(capacity_tech_future$Group.1,
                                                     capacity_tech_future$Group.2,
                                                     capacity_tech_future$Group.3,
                                                     capacity_tech_future$Group.4),] 
  
  # Compute share of cooling by NEMS and fuel 
  capacity_tech_future_nation$x <- capacity_tech_future_nation$x / 
    capacity_tech_nation[match(capacity_tech_future_nation$Group.3,
                               as.character(capacity_tech_nation$Group.1),
                               nomatch = NA_integer_),
                         "x"]
  
  # Find corresponding cooling share and add it to the complete tech list
  cooling_share[,year-1969] <- capacity_tech_hist[match(paste(as.character(complete_tech$State),complete_tech$plant_type,
                                                              complete_tech$cooling_system,complete_tech$water_type,complete_tech$fuel),
                                                        paste(as.character(capacity_tech_hist$Group.1),as.character(capacity_tech_hist$Group.2),
                                                              as.character(capacity_tech_hist$Group.3),as.character(capacity_tech_hist$Group.4),
                                                              as.character(capacity_tech_hist$Group.5)),
                                                        nomatch = NA_integer_),"x"]
}
# Replace all NA with 0
cooling_share[is.na(cooling_share)] <- 0
# Convert to data frame
cooling_share.df <- data.frame(cooling_share)
colnames(cooling_share.df) <- seq(1970,2008)


# Fixed cooling share
# Find corresponding cooling share and add it to the complete tech list

cooling_share_future[,1] <- capacity_tech_future[match(paste(as.character(complete_tech$NEMS),
                                                             complete_tech$cooling_system,complete_tech$water_type,complete_tech$fuel),
                                                       paste(as.character(capacity_tech_future$Group.1),as.character(capacity_tech_future$Group.2),
                                                             as.character(capacity_tech_future$Group.3),as.character(capacity_tech_future$Group.4)),
                                                       nomatch = NA_integer_),"x"]

cooling_share_future[,2] <- capacity_tech_future_nation[match(paste(complete_tech$cooling_system,complete_tech$water_type,complete_tech$fuel),
                                                              paste(as.character(capacity_tech_future_nation$Group.1),as.character(capacity_tech_future_nation$Group.2),
                                                                    as.character(capacity_tech_future_nation$Group.3)),
                                                              nomatch = NA_integer_),"x"]


# Convert to data frame
cooling_share_future.df <- data.frame(cooling_share_future)
colnames(cooling_share_future.df) <- c(2010,2020,2100)

# Combine the two data frames
complete_tech_cooling_share <- cbind(complete_tech,cooling_share.df,cooling_share_future.df)

complete_tech_cooling_share[is.na(complete_tech_cooling_share[,"2010"]),"2010"] <- 0
complete_tech_cooling_share[is.na(complete_tech_cooling_share[,"2020"]),"2020"] <- 0

complete_tech_cooling_share_identify <- aggregate(complete_tech_cooling_share[,"2010"],
                                                  by = list(complete_tech_cooling_share$NEMS,
                                                            complete_tech_cooling_share$fuel),
                                                  sum)
index <- match(paste(complete_tech_cooling_share$NEMS,complete_tech_cooling_share$fuel),
               paste(complete_tech_cooling_share_identify$Group.1, complete_tech_cooling_share_identify$Group.2),
               "2010")


for (i in 1:length(index)){
  rownumber <- which(paste(complete_tech_cooling_share$NEMS,complete_tech_cooling_share$fuel)
                     %in%
                       paste(complete_tech_cooling_share_identify[index[i],"Group.1"],
                             complete_tech_cooling_share_identify[index[i],"Group.2"]) 
  )
  
  if (is.na(complete_tech_cooling_share_identify[index[i],"x"]) == T)
  {
    complete_tech_cooling_share[rownumber,"2100"] <-  complete_tech_cooling_share[rownumber,"2050"]
  }
  
  else
  {
    complete_tech_cooling_share[rownumber,"2100"] <- complete_tech_cooling_share[rownumber,"2010"]
  }
}


# Mannually assume all future CSP have cooling share of 1
complete_tech_cooling_share[complete_tech_cooling_share$plant_type=="CSP",47:49] <- 1 
complete_tech_cooling_share[,"2010"] <- complete_tech_cooling_share[,"2008"]
complete_tech_cooling_share[,"2020"] <- complete_tech_cooling_share[,"2100"]

# Mannually assume all PV have cooling share of 1
complete_tech_cooling_share[complete_tech_cooling_share$fuel=="solar PV"&complete_tech_cooling_share$water_type=="fresh",8:49] <- 1

# -----------------------------------------------------------------------------

# 3. Write all output
writedata( complete_tech_cooling_share, domain="GCAMUSA_LEVEL1_DATA", fn="LA1233.CoolingSystemShares_RG3_frozen")

logstop()








