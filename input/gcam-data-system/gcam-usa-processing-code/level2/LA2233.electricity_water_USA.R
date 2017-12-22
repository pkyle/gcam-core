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
logstart( "L223.electricity_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for GCAM USA electricity sectors with demand resolved nationally (1 region)" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )

# Added elec water coefficient in here
L1233.wdraw_coef_R_elec_F_tech_Yh_ref <- readdata( "GCAMUSA_LEVEL1_DATA", "L1233.wdraw_coef_R_elec_F_tech_Yh_ref" )
L1233.wcons_coef_R_elec_F_tech_Yh_ref <- readdata( "GCAMUSA_LEVEL1_DATA", "L1233.wcons_coef_R_elec_F_tech_Yh_ref" )
L1233.wdraw_coef_R_elec_F_tech_Yh_frozen <- readdata( "GCAMUSA_LEVEL1_DATA", "L1233.wdraw_coef_R_elec_F_tech_Yh_frozen" )
L1233.wcons_coef_R_elec_F_tech_Yh_frozen <- readdata( "GCAMUSA_LEVEL1_DATA", "L1233.wcons_coef_R_elec_F_tech_Yh_frozen" )
A23.elecS_inttech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_inttech_associations", skip = 1 )
A23.elecS_tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elecS_tech_associations", skip = 1 )
#elec_tech_water_map <- readdata( "GCAMUSA_MAPPINGS", "elec_tech_water_segments" )
NREL_us_re_technical_potential <- readdata( "GCAMUSA_LEVEL0_DATA", "NREL_us_re_technical_potential" )
L223.StubTechMarket_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechMarket_elec_USA", skip = 4 )
A23.elec_tech_associations_coal_retire <- readdata( "GCAMUSA_ASSUMPTIONS", "A23.elec_tech_associations_coal_retire" ) %>%
  filter(grepl("generation", Electric.sector))

# -----------------------------------------------------------------------------
# 2. Perform computations
# Created a mapping file
names(A23.elecS_inttech_associations)[3] <- "Electric.sector.technology"
names(A23.elecS_inttech_associations)[6] <- "technology"
elec_tech_water_map <- rbind(A23.elecS_tech_associations,A23.elecS_inttech_associations)
names(elec_tech_water_map)[4] <- "sector"
names(elec_tech_water_map)[5] <- "fuel"
elec_tech_water_map <- elec_tech_water_map [c("sector", "fuel", "technology", "Electric.sector","subsector","Electric.sector.technology")]
# Added elec water coefficient in here

######### Ref scenario
L1233.wdraw_coef_R_elec_F_tech_Yh_ref$fuel <- gsub("solar CSP", "solar", L1233.wdraw_coef_R_elec_F_tech_Yh_ref$fuel)
L1233.wdraw_coef_R_elec_F_tech_Yh_ref$fuel <- gsub("solar PV", "solar", L1233.wdraw_coef_R_elec_F_tech_Yh_ref$fuel)
L1233.wcons_coef_R_elec_F_tech_Yh_ref$fuel <- gsub("solar CSP", "solar", L1233.wcons_coef_R_elec_F_tech_Yh_ref$fuel)
L1233.wcons_coef_R_elec_F_tech_Yh_ref$fuel <- gsub("solar PV", "solar", L1233.wcons_coef_R_elec_F_tech_Yh_ref$fuel)

L2233.StubTech_WaterCoef_ref.wdraw <- L1233.wdraw_coef_R_elec_F_tech_Yh_ref
L2233.StubTech_WaterCoef_ref.wdraw$minicam.energy.input <- "water withdrawals"
L2233.StubTech_WaterCoef_ref.wcons <- L1233.wcons_coef_R_elec_F_tech_Yh_ref
L2233.StubTech_WaterCoef_ref.wcons$minicam.energy.input <- "water consumption"


L2233.StubTech_WaterCoef_ref <- rbind( L2233.StubTech_WaterCoef_ref.wdraw, L2233.StubTech_WaterCoef_ref.wcons )
L2233.StubTech_WaterCoef_ref <- gcam_interp( L2233.StubTech_WaterCoef_ref, model_years, rule=2 )
L2233.StubTech_WaterCoef_ref$plant_type <- NULL

L2233.StubTech_WaterCoef_ref <- interpolate_and_melt( L2233.StubTech_WaterCoef_ref, model_years, "coefficient" ) 


L2233.StubTech_WaterCoef_ref <- merge( L2233.StubTech_WaterCoef_ref, unique( elec_tech_water_map[ c( S_F_tech, grep( 'Electric.', names( elec_tech_water_map ), value=T ) ) ] ) )
names(L2233.StubTech_WaterCoef_ref)[1] <- "subsector"
names(L2233.StubTech_WaterCoef_ref)[3] <- "region"
names(L2233.StubTech_WaterCoef_ref)[2] <- "fuel"
names( L2233.StubTech_WaterCoef_ref ) <- sub( 'Electric.', '', names( L2233.StubTech_WaterCoef_ref ) )
names(L2233.StubTech_WaterCoef_ref)[9] <- "supplysector"

names(L2233.StubTech_WaterCoef_ref)[10] <- "technology"

L2233.StubTech_WaterCoef_ref$market.name <- L2233.StubTech_WaterCoef_ref$region
L2233.StubTech_WaterCoef_ref <- L2233.StubTech_WaterCoef_ref[, names_TechCoef ]


#Indicate states where geothermal electric technologies will not be created
NREL_us_re_technical_potential$state <- states_subregions$state[ match( NREL_us_re_technical_potential$State, states_subregions$state_name ) ]
geo_states_noresource <- paste( states[ states %in% NREL_us_re_technical_potential$state[ NREL_us_re_technical_potential$Geothermal_Hydrothermal_GWh == 0 ] ], "geothermal" )

# delete state that doesn't have geothermal technology
L2233.StubTech_WaterCoef_ref <- L2233.StubTech_WaterCoef_ref[paste(L2233.StubTech_WaterCoef_ref$region, L2233.StubTech_WaterCoef_ref$subsector) 
                                                                   %!in% geo_states_noresource,]

# Split conv_coal_pul into slow retire and fast retire conv_coal_pul
L2233.StubTech_WaterCoef_ref_coal_split <- merge( L2233.StubTech_WaterCoef_ref, A23.elec_tech_associations_coal_retire)
L2233.StubTech_WaterCoef_ref_coal_split$Electric.sector <- NULL # delete excessive column
L2233.StubTech_WaterCoef_ref_coal_split$technology <- L2233.StubTech_WaterCoef_ref_coal_split$Electric.sector.technology
L2233.StubTech_WaterCoef_ref_coal_split$Electric.sector.technology <- NULL

# Remove duplicate entries
L2233.StubTech_WaterCoef_ref <- subset(L2233.StubTech_WaterCoef_ref,(!L2233.StubTech_WaterCoef_ref$technology %in% c("coal_base_conv pul","coal_int_conv pul",
                                                                                                           "coal_peak_conv pul",
                                                                                                           "coal_subpeak_conv pul"))) 
L2233.StubTech_WaterCoef_ref <- rbind(L2233.StubTech_WaterCoef_ref_coal_split,L2233.StubTech_WaterCoef_ref)
L2233.StubTech_WaterCoef_ref <- L2233.StubTech_WaterCoef_ref[c("region", "supplysector", "subsector", "technology","year","minicam.energy.input","coefficient","market.name")]
L2233.StubTech_WaterCoef_ref <- L2233.StubTech_WaterCoef_ref[order(L2233.StubTech_WaterCoef_ref$region, 
                                                                         L2233.StubTech_WaterCoef_ref$year),]


######## Frozen scenario
L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$fuel <- gsub("solar CSP", "solar", L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$fuel)
L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$fuel <- gsub("solar PV", "solar", L1233.wdraw_coef_R_elec_F_tech_Yh_frozen$fuel)
L1233.wcons_coef_R_elec_F_tech_Yh_frozen$fuel <- gsub("solar CSP", "solar", L1233.wcons_coef_R_elec_F_tech_Yh_frozen$fuel)
L1233.wcons_coef_R_elec_F_tech_Yh_frozen$fuel <- gsub("solar PV", "solar", L1233.wcons_coef_R_elec_F_tech_Yh_frozen$fuel)



L2233.StubTech_WaterCoef_frozen.wdraw <- L1233.wdraw_coef_R_elec_F_tech_Yh_frozen
L2233.StubTech_WaterCoef_frozen.wdraw$minicam.energy.input <- "water withdrawals"
L2233.StubTech_WaterCoef_frozen.wcons <- L1233.wcons_coef_R_elec_F_tech_Yh_frozen


L2233.StubTech_WaterCoef_frozen.wcons$minicam.energy.input <- "water consumption"


L2233.StubTech_WaterCoef_frozen <- rbind( L2233.StubTech_WaterCoef_frozen.wdraw, L2233.StubTech_WaterCoef_frozen.wcons )
L2233.StubTech_WaterCoef_frozen <- gcam_interp( L2233.StubTech_WaterCoef_frozen, model_years, rule=2 )
L2233.StubTech_WaterCoef_frozen$plant_type <- NULL

L2233.StubTech_WaterCoef_frozen <- interpolate_and_melt( L2233.StubTech_WaterCoef_frozen, model_years, "coefficient" ) 


L2233.StubTech_WaterCoef_frozen <- merge( L2233.StubTech_WaterCoef_frozen, unique( elec_tech_water_map[ c( S_F_tech, grep( 'Electric.', names( elec_tech_water_map ), value=T ) ) ] ) )
names(L2233.StubTech_WaterCoef_frozen)[1] <- "subsector"
names(L2233.StubTech_WaterCoef_frozen)[3] <- "region"
names(L2233.StubTech_WaterCoef_frozen)[2] <- "fuel"
names( L2233.StubTech_WaterCoef_frozen ) <- sub( 'Electric.', '', names( L2233.StubTech_WaterCoef_frozen ) )
names(L2233.StubTech_WaterCoef_frozen)[9] <- "supplysector"

names(L2233.StubTech_WaterCoef_frozen)[10] <- "technology"

L2233.StubTech_WaterCoef_frozen$market.name <- L2233.StubTech_WaterCoef_frozen$region
L2233.StubTech_WaterCoef_frozen <- L2233.StubTech_WaterCoef_frozen[, names_TechCoef ]

# delete state that doesn't have geothermal technology
L2233.StubTech_WaterCoef_frozen <- L2233.StubTech_WaterCoef_frozen[paste(L2233.StubTech_WaterCoef_frozen$region, L2233.StubTech_WaterCoef_frozen$subsector) 
                                                                   %!in% geo_states_noresource,]

# Split conv_coal_pul into slow retire and fast retire conv_coal_pul
L2233.StubTech_WaterCoef_frozen_coal_split <- merge( L2233.StubTech_WaterCoef_frozen, A23.elec_tech_associations_coal_retire)
L2233.StubTech_WaterCoef_frozen_coal_split$Electric.sector <- NULL # delete excessive column
L2233.StubTech_WaterCoef_frozen_coal_split$technology <- L2233.StubTech_WaterCoef_frozen_coal_split$Electric.sector.technology
L2233.StubTech_WaterCoef_frozen_coal_split$Electric.sector.technology <- NULL

# Remove duplicate entries
L2233.StubTech_WaterCoef_frozen <- subset(L2233.StubTech_WaterCoef_frozen,(!L2233.StubTech_WaterCoef_frozen$technology %in% c("coal_base_conv pul","coal_int_conv pul",
                                                                                                                              "coal_peak_conv pul",
                                                                                                                              "coal_subpeak_conv pul"))) 
L2233.StubTech_WaterCoef_frozen <- rbind(L2233.StubTech_WaterCoef_frozen_coal_split,L2233.StubTech_WaterCoef_frozen)

L2233.StubTech_WaterCoef_frozen <- L2233.StubTech_WaterCoef_frozen[c("region", "supplysector", "subsector", "technology","year","minicam.energy.input","coefficient","market.name")]
L2233.StubTech_WaterCoef_frozen <- L2233.StubTech_WaterCoef_frozen[order(L2233.StubTech_WaterCoef_frozen$region, 
                                                        L2233.StubTech_WaterCoef_frozen$year),]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
# Write out water xml file
comments.L2233.StubTech_WaterCoef_ref <- c("Weighted water coefficient for reference scenario and load segment classification")
comments.L2233.StubTech_WaterCoef_frozen <- c("Weighted water coefficient for frozen scenario and load segment classification")

write_mi_data( L2233.StubTech_WaterCoef_ref, domain = "GCAMUSA_LEVEL2_DATA", fn = "LA2233.StubTech_WaterCoef_ref", comments=comments.L2233.StubTech_WaterCoef_ref, "StubTechCoef","GCAMUSA_XML_BATCH", "batch_electricity_water_USA_ref_segment.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_electricity_water_USA_ref_segment.xml", "GCAMUSA_XML_FINAL", "water_elec_USA_ref_segment.xml", "", xml_tag="outFile" )
write_mi_data( L2233.StubTech_WaterCoef_frozen, domain = "GCAMUSA_LEVEL2_DATA", fn = "LA2233.StubTech_WaterCoef_frozen", comments=comments.L2233.StubTech_WaterCoef_frozen, "StubTechCoef","GCAMUSA_XML_BATCH", "batch_electricity_water_USA_frozen_segment.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_electricity_water_USA_frozen_segment.xml", "GCAMUSA_XML_FINAL", "water_elec_USA_frozen_segment.xml", "", xml_tag="outFile" )

logstop()
