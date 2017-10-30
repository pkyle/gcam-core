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
logstart( "L2000.Unlimited_water_supply.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Unlimited water supply input" )

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )  
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
Un_water_supply <- readdata( "GCAMUSA_LEVEL0_DATA", "Unlimited_water_supply", skip = 1 )
L2000.Un_water_supply <- gcam_interp( Un_water_supply, model_years, rule=2 )
L2000.Un_water_supply <- interpolate_and_melt( L2000.Un_water_supply, model_years, "price" ) 
L2000.Un_water_supply <- L2000.Un_water_supply[order(L2000.Un_water_supply$region, 
                                                     L2000.Un_water_supply$year),]

L2000.Un_water_supply <- L2000.Un_water_supply[c("region", "unlimited.resource", "output.unit", "price.unit","market","capacity.factor","year","price")]

write_mi_data( L2000.Un_water_supply, "UnlimitedWaterSupply","GCAMUSA_LEVEL2_DATA", "L2000.Un_water_supply", "GCAMUSA_XML_BATCH", "batch_water_supply_state.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_water_supply_state.xml", "GCAMUSA_XML_FINAL", "unlimited_water_supply_state.xml", "", xml_tag="outFile" )

L2000.Un_water_supply_USA <- L2000.Un_water_supply
L2000.Un_water_supply_USA$region <- "USA"
L2000.Un_water_supply_USA$market <- "USA"
L2000.Un_water_supply_USA <- unique(L2000.Un_water_supply_USA)

write_mi_data( L2000.Un_water_supply_USA, "UnlimitedWaterSupply","GCAMUSA_LEVEL2_DATA", "L2000.Un_water_supply_USA", "GCAMUSA_XML_BATCH", "batch_water_supply_USA.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_water_supply_USA.xml", "GCAMUSA_XML_FINAL", "unlimited_water_supply_USA.xml", "", xml_tag="outFile" )


logstop()
