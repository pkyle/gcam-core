# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
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
logstart( "LB1233.Elec_water.R" )

printlog( "Water consumption by state, fuel, technology, and cooling system type" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
UCS_tech_names <- readdata( "GCAMUSA_MAPPINGS", "UCS_tech_names" )
UCS_water_types <- readdata( "GCAMUSA_MAPPINGS", "UCS_water_types" )
LA1233.CoolingSystemShares_RG3_ref <- readdata( "GCAMUSA_LEVEL1_DATA", "LA1233.CoolingSystemShares_RG3_ref" )
LA1233.CoolingSystemShares_RG3_frozen <- readdata( "GCAMUSA_LEVEL1_DATA", "LA1233.CoolingSystemShares_RG3_frozen" )
elec_tech_water_map <- readdata( "GCAMUSA_MAPPINGS", "elec_tech_water_map" )
Macknick_elec_water_m3MWh <- readdata( "GCAMUSA_LEVEL0_DATA", "Macknick_elec_water_m3MWh" )

# -----------------------------------------------------------------------------
# 2. Perform computation for both reference and frozen scenarios
LA1233.CoolingSystemShares_RG3_future_ref <- gcam_interp( LA1233.CoolingSystemShares_RG3_ref, future_years )
LA1233.CoolingSystemShares_RG3_future_frozen <- gcam_interp( LA1233.CoolingSystemShares_RG3_frozen, future_years )
# Remove seawater from calculation
######## Reference scenario

LA1233.CoolingSystemShares_RG3_future_ref <- subset(LA1233.CoolingSystemShares_RG3_future_ref, LA1233.CoolingSystemShares_RG3_future_ref$water_type!="seawater")
LA1233.CoolingSystemShares_RG3_future_frozen <- subset(LA1233.CoolingSystemShares_RG3_future_frozen, LA1233.CoolingSystemShares_RG3_future_frozen$water_type!="seawater")
  
L1233.wdraw_coef_R_elec_F_tech_Yh_ref <- data.frame(matrix(NA, ncol = 61, nrow = length(LA1233.CoolingSystemShares_RG3_future_ref[,1])))
colnames(L1233.wdraw_coef_R_elec_F_tech_Yh_ref) <- c("state","sector","fuel","plant_type",c(paste("X",seq(1971,2008),sep=""),paste("X",seq(2010,2100,5),sep=""))) 
L1233.wdraw_coef_R_elec_F_tech_Yh_ref$state <- LA1233.CoolingSystemShares_RG3_future_ref$State
L1233.wdraw_coef_R_elec_F_tech_Yh_ref$sector <- rep("electricity",length(LA1233.CoolingSystemShares_RG3_future_ref[,1]))
L1233.wdraw_coef_R_elec_F_tech_Yh_ref$fuel <- LA1233.CoolingSystemShares_RG3_future_ref$fuel
L1233.wdraw_coef_R_elec_F_tech_Yh_ref$plant_type <- LA1233.CoolingSystemShares_RG3_future_ref$plant_type
L1233.wdraw_coef_R_elec_F_tech_Yh_ref$technology <- LA1233.CoolingSystemShares_RG3_future_ref$technology

L1233.wcons_coef_R_elec_F_tech_Yh_ref <- data.frame(matrix(NA, ncol = 61, nrow = length(LA1233.CoolingSystemShares_RG3_future_ref[,1])))
colnames(L1233.wcons_coef_R_elec_F_tech_Yh_ref) <- c("state","sector","fuel","plant_type",c(paste("X",seq(1971,2008),sep=""),paste("X",seq(2010,2100,5),sep=""))) 
L1233.wcons_coef_R_elec_F_tech_Yh_ref$state <- LA1233.CoolingSystemShares_RG3_future_ref$State
L1233.wcons_coef_R_elec_F_tech_Yh_ref$sector <- rep("electricity",length(LA1233.CoolingSystemShares_RG3_future_ref[,1]))
L1233.wcons_coef_R_elec_F_tech_Yh_ref$fuel <- LA1233.CoolingSystemShares_RG3_future_ref$fuel
L1233.wcons_coef_R_elec_F_tech_Yh_ref$plant_type <- LA1233.CoolingSystemShares_RG3_future_ref$plant_type
L1233.wcons_coef_R_elec_F_tech_Yh_ref$technology <- LA1233.CoolingSystemShares_RG3_future_ref$technology

Macknick_elec_water_m3MWh[,"matching_tech"] <- elec_tech_water_map[match(Macknick_elec_water_m3MWh$technology,elec_tech_water_map$technology),"plant_type"]


data_draw <- matrix(data = NA, ncol = 57, nrow = 1275)
data_cons <- matrix(data = NA, ncol = 57, nrow = 1275)
n <-1
for (x in c(seq(1971,2008),seq(2010,2100,5)))
{
  for (i in 1:length(LA1233.CoolingSystemShares_RG3_future_ref[,1]))
  {
    if (is.na(match(paste(LA1233.CoolingSystemShares_RG3_future_ref[i,"plant_type"],LA1233.CoolingSystemShares_RG3_future_ref[i,"cooling_system"]),
                    paste(Macknick_elec_water_m3MWh$matching_tech,Macknick_elec_water_m3MWh$cooling_system))) == F)
    {
      L1233.wdraw_coef_R_elec_F_tech_Yh_ref[i,paste("X",toString(x),sep="")] <- Macknick_elec_water_m3MWh[match(paste(LA1233.CoolingSystemShares_RG3_future_ref[i,"plant_type"],LA1233.CoolingSystemShares_RG3_future_ref[i,"cooling_system"], LA1233.CoolingSystemShares_RG3_future_ref[i,"fuel"]),
                                                                                                                paste(Macknick_elec_water_m3MWh$matching_tech,Macknick_elec_water_m3MWh$cooling_system,Macknick_elec_water_m3MWh$fuel)),"water_withdrawals"] * LA1233.CoolingSystemShares_RG3_future_ref[i,paste("X",x,sep="")]*
        conv_m3_bm3 / conv_MWh_EJ
      L1233.wcons_coef_R_elec_F_tech_Yh_ref[i,paste("X",toString(x),sep="")] <- Macknick_elec_water_m3MWh[match(paste(LA1233.CoolingSystemShares_RG3_future_ref[i,"plant_type"],LA1233.CoolingSystemShares_RG3_future_ref[i,"cooling_system"],LA1233.CoolingSystemShares_RG3_future_ref[i,"fuel"]),
                                                                                                                paste(Macknick_elec_water_m3MWh$matching_tech,Macknick_elec_water_m3MWh$cooling_system,Macknick_elec_water_m3MWh$fuel)),"water_consumption"] * LA1233.CoolingSystemShares_RG3_future_ref[i,paste("X",x,sep="")]*
        conv_m3_bm3 / conv_MWh_EJ
    }else
    {
      L1233.wdraw_coef_R_elec_F_tech_Yh_ref[i,paste("X",toString(x),sep="")] <- 0 
      L1233.wcons_coef_R_elec_F_tech_Yh_ref[i,paste("X",toString(x),sep="")] <- 0 
    }
  }
  data_draw[,n] <- aggregate(L1233.wdraw_coef_R_elec_F_tech_Yh_ref[,paste("X",toString(x),sep="")],
                             by = list(as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_ref$state),
                                       as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_ref$fuel),
                                       as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_ref$plant_type),
                                       as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_ref$technology)
                             ),
                             sum)[,"x"]
  data_cons[,n] <- aggregate(L1233.wcons_coef_R_elec_F_tech_Yh_ref[,paste("X",toString(x),sep="")],
                             by = list(as.character(L1233.wcons_coef_R_elec_F_tech_Yh_ref$state),
                                       as.character(L1233.wcons_coef_R_elec_F_tech_Yh_ref$fuel),
                                       as.character(L1233.wcons_coef_R_elec_F_tech_Yh_ref$plant_type),
                                       as.character(L1233.wcons_coef_R_elec_F_tech_Yh_ref$technology)
                             ),
                             sum)[,"x"]
  n <- n+1
  
}

L1233.wdraw_coef_R_elec_F_tech_Yh_ref_data <- aggregate(L1233.wdraw_coef_R_elec_F_tech_Yh_ref[,"X1971"],
                                                        by = list(as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_ref$state),
                                                                  as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_ref$fuel),
                                                                  as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_ref$plant_type),
                                                                  as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_ref$technology)
                                                        ),
                                                        sum)[,cbind("Group.1","Group.2","Group.3","Group.4")]
data_draw.df <- data.frame(data_draw)
L1233.wdraw_coef_R_elec_F_tech_Yh_ref_data<- cbind(L1233.wdraw_coef_R_elec_F_tech_Yh_ref_data,data_draw.df)
colnames(L1233.wdraw_coef_R_elec_F_tech_Yh_ref_data) <- c("state","fuel","plant_type","technology",c(paste("X",seq(1971,2008),sep=""),paste("X",seq(2010,2100,5),sep=""))) 



L1233.wcons_coef_R_elec_F_tech_Yh_ref_data <- aggregate(L1233.wcons_coef_R_elec_F_tech_Yh_ref[,"X1971"],
                                                        by = list(as.character(L1233.wcons_coef_R_elec_F_tech_Yh_ref$state),
                                                                  as.character(L1233.wcons_coef_R_elec_F_tech_Yh_ref$fuel),
                                                                  as.character(L1233.wcons_coef_R_elec_F_tech_Yh_ref$plant_type),
                                                                  as.character(L1233.wcons_coef_R_elec_F_tech_Yh_ref$technology)
                                                        ),
                                                        sum)[,cbind("Group.1","Group.2","Group.3","Group.4")]


data_cons.df <- data.frame(data_cons)
L1233.wcons_coef_R_elec_F_tech_Yh_ref_data <- cbind(L1233.wcons_coef_R_elec_F_tech_Yh_ref_data,data_cons.df)
colnames(L1233.wcons_coef_R_elec_F_tech_Yh_ref_data) <- c("state","fuel","plant_type","technology",c(paste("X",seq(1971,2008),sep=""),paste("X",seq(2010,2100,5),sep=""))) 

######## Frozen scenario
L1233.wdraw_coef_R_elec_F_tech_Yh_frozen <- data.frame(matrix(NA, ncol = 61, nrow = length(LA1233.CoolingSystemShares_RG3_future_frozen[,1])))
colnames(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen) <- c("state","sector","fuel","plant_type",c(paste("X",seq(1971,2008),sep=""),paste("X",seq(2010,2100,5),sep=""))) 
L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$state <- LA1233.CoolingSystemShares_RG3_future_frozen$State
L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$sector <- rep("electricity",length(LA1233.CoolingSystemShares_RG3_future_frozen[,1]))
L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$fuel <- LA1233.CoolingSystemShares_RG3_future_frozen$fuel
L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$plant_type <- LA1233.CoolingSystemShares_RG3_future_frozen$plant_type
L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$technology <- LA1233.CoolingSystemShares_RG3_future_frozen$technology

L1233.wcons_coef_R_elec_F_tech_Yh_frozen <- data.frame(matrix(NA, ncol = 61, nrow = length(LA1233.CoolingSystemShares_RG3_future_frozen[,1])))
colnames(L1233.wcons_coef_R_elec_F_tech_Yh_frozen) <- c("state","sector","fuel","plant_type",c(paste("X",seq(1971,2008),sep=""),paste("X",seq(2010,2100,5),sep=""))) 
L1233.wcons_coef_R_elec_F_tech_Yh_frozen$state <- LA1233.CoolingSystemShares_RG3_future_frozen$State
L1233.wcons_coef_R_elec_F_tech_Yh_frozen$sector <- rep("electricity",length(LA1233.CoolingSystemShares_RG3_future_frozen[,1]))
L1233.wcons_coef_R_elec_F_tech_Yh_frozen$fuel <- LA1233.CoolingSystemShares_RG3_future_frozen$fuel
L1233.wcons_coef_R_elec_F_tech_Yh_frozen$plant_type <- LA1233.CoolingSystemShares_RG3_future_frozen$plant_type
L1233.wcons_coef_R_elec_F_tech_Yh_frozen$technology <- LA1233.CoolingSystemShares_RG3_future_frozen$technology

data_draw <- matrix(data = NA, ncol = 57, nrow = 1275)
data_cons <- matrix(data = NA, ncol = 57, nrow = 1275)
n <-1
for (x in c(seq(1971,2008),seq(2010,2100,5)))
{
  for (i in 1:length(LA1233.CoolingSystemShares_RG3_future_frozen[,1]))
  {
    if (is.na(match(paste(LA1233.CoolingSystemShares_RG3_future_frozen[i,"plant_type"],LA1233.CoolingSystemShares_RG3_future_frozen[i,"cooling_system"]),
                    paste(Macknick_elec_water_m3MWh$matching_tech,Macknick_elec_water_m3MWh$cooling_system))) == F)
    {
      L1233.wdraw_coef_R_elec_F_tech_Yh_frozen[i,paste("X",toString(x),sep="")] <- Macknick_elec_water_m3MWh[match(paste(LA1233.CoolingSystemShares_RG3_future_frozen[i,"plant_type"],LA1233.CoolingSystemShares_RG3_future_frozen[i,"cooling_system"], LA1233.CoolingSystemShares_RG3_future_frozen[i,"fuel"]),
                                                                                                                   paste(Macknick_elec_water_m3MWh$matching_tech,Macknick_elec_water_m3MWh$cooling_system,Macknick_elec_water_m3MWh$fuel)),"water_withdrawals"] * LA1233.CoolingSystemShares_RG3_future_frozen[i,paste("X",x,sep="")]*
        conv_m3_bm3 / conv_MWh_EJ
      L1233.wcons_coef_R_elec_F_tech_Yh_frozen[i,paste("X",toString(x),sep="")] <- Macknick_elec_water_m3MWh[match(paste(LA1233.CoolingSystemShares_RG3_future_frozen[i,"plant_type"],LA1233.CoolingSystemShares_RG3_future_frozen[i,"cooling_system"],LA1233.CoolingSystemShares_RG3_future_frozen[i,"fuel"]),
                                                                                                                   paste(Macknick_elec_water_m3MWh$matching_tech,Macknick_elec_water_m3MWh$cooling_system,Macknick_elec_water_m3MWh$fuel)),"water_consumption"] * LA1233.CoolingSystemShares_RG3_future_frozen[i,paste("X",x,sep="")]*
        conv_m3_bm3 / conv_MWh_EJ
    }else
    {
      L1233.wdraw_coef_R_elec_F_tech_Yh_frozen[i,paste("X",toString(x),sep="")] <- 0 
      L1233.wcons_coef_R_elec_F_tech_Yh_frozen[i,paste("X",toString(x),sep="")] <- 0 
    }
  }
  data_draw[,n] <- aggregate(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen[,paste("X",toString(x),sep="")],
                             by = list(as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$state),
                                       as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$fuel),
                                       as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$plant_type),
                                       as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$technology)
                             ),
                             sum)[,"x"]
  data_cons[,n] <- aggregate(L1233.wcons_coef_R_elec_F_tech_Yh_frozen[,paste("X",toString(x),sep="")],
                             by = list(as.character(L1233.wcons_coef_R_elec_F_tech_Yh_frozen$state),
                                       as.character(L1233.wcons_coef_R_elec_F_tech_Yh_frozen$fuel),
                                       as.character(L1233.wcons_coef_R_elec_F_tech_Yh_frozen$plant_type),
                                       as.character(L1233.wcons_coef_R_elec_F_tech_Yh_frozen$technology)
                             ),
                             sum)[,"x"]
  n <- n+1
  
}

L1233.wdraw_coef_R_elec_F_tech_Yh_frozen_data <- aggregate(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen[,"X1971"],
                                                           by = list(as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$state),
                                                                     as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$fuel),
                                                                     as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$plant_type),
                                                                     as.character(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$technology)
                                                           ),
                                                           sum)[,cbind("Group.1","Group.2","Group.3","Group.4")]
data_draw.df <- data.frame(data_draw)
L1233.wdraw_coef_R_elec_F_tech_Yh_frozen_data<- cbind(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen_data,data_draw.df)
colnames(L1233.wdraw_coef_R_elec_F_tech_Yh_frozen_data) <- c("state","fuel","plant_type","technology",c(paste("X",seq(1971,2008),sep=""),paste("X",seq(2010,2100,5),sep=""))) 



L1233.wcons_coef_R_elec_F_tech_Yh_frozen_data <- aggregate(L1233.wcons_coef_R_elec_F_tech_Yh_frozen[,"X1971"],
                                                           by = list(as.character(L1233.wcons_coef_R_elec_F_tech_Yh_frozen$state),
                                                                     as.character(L1233.wcons_coef_R_elec_F_tech_Yh_frozen$fuel),
                                                                     as.character(L1233.wcons_coef_R_elec_F_tech_Yh_frozen$plant_type),
                                                                     as.character(L1233.wcons_coef_R_elec_F_tech_Yh_frozen$technology)
                                                           ),
                                                           sum)[,cbind("Group.1","Group.2","Group.3","Group.4")]


data_cons.df <- data.frame(data_cons)
L1233.wcons_coef_R_elec_F_tech_Yh_frozen_data <- cbind(L1233.wcons_coef_R_elec_F_tech_Yh_frozen_data,data_cons.df)
colnames(L1233.wcons_coef_R_elec_F_tech_Yh_frozen_data) <- c("state","fuel","plant_type","technology",c(paste("X",seq(1971,2008),sep=""),paste("X",seq(2010,2100,5),sep=""))) 
# -----------------------------------------------------------------------------

# 3. Output

#Add comments for each table
comments.L1233.wdraw_coef_R_elec_F_tech_Yh_ref_data <- c( "Water withdrawal coef for electricity generation by region / water type","Unit = km3 (bm3) / EJ" )
comments.L1233.wcons_coef_R_elec_F_tech_Yh_ref_data <- c( "Water consumption coef for electricity generation by region / water type","Unit = km3 (bm3) / EJ" )

comments.L1233.wdraw_coef_R_elec_F_tech_Yh_frozen_data <- c( "Water withdrawal coef for electricity generation by region / water type","Unit = km3 (bm3) / EJ" )
comments.L1233.wcons_coef_R_elec_F_tech_Yh_frozen_data <- c( "Water consumption coef for electricity generation by region / water type","Unit = km3 (bm3) / EJ" )

#write tables as CSV files
writedata( L1233.wdraw_coef_R_elec_F_tech_Yh_ref_data, domain="GCAMUSA_LEVEL1_DATA", fn="L1233.wdraw_coef_R_elec_F_tech_Yh_ref", comments=comments.L1233.wdraw_coef_R_elec_F_tech_Yh_ref_data )
writedata( L1233.wcons_coef_R_elec_F_tech_Yh_ref_data, domain="GCAMUSA_LEVEL1_DATA", fn="L1233.wcons_coef_R_elec_F_tech_Yh_ref", comments=comments.L1233.wcons_coef_R_elec_F_tech_Yh_ref_data )

writedata( L1233.wdraw_coef_R_elec_F_tech_Yh_frozen_data, domain="GCAMUSA_LEVEL1_DATA", fn="L1233.wdraw_coef_R_elec_F_tech_Yh_frozen", comments=comments.L1233.wdraw_coef_R_elec_F_tech_Yh_frozen_data )
writedata( L1233.wcons_coef_R_elec_F_tech_Yh_frozen_data, domain="GCAMUSA_LEVEL1_DATA", fn="L1233.wcons_coef_R_elec_F_tech_Yh_frozen", comments=comments.L1233.wcons_coef_R_elec_F_tech_Yh_frozen_data )

# Every script should finish with this line
logstop()


  