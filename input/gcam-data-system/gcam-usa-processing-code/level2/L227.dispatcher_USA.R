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
logstart( "L227.dispatcher_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Existing coal and gas electric sector dispatch" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
A27.sector <- readdata( "GCAMUSA_ASSUMPTIONS", "A27.sector" )
A27.subsector_logit <- readdata( "GCAMUSA_ASSUMPTIONS", "A27.subsector_logit" )
A27.subsector_shrwt <- readdata( "GCAMUSA_ASSUMPTIONS", "A27.subsector_shrwt" )
A27.globaltech_shrwt <- readdata( "GCAMUSA_ASSUMPTIONS", "A27.globaltech_shrwt")
A27.globaltech_cost <- readdata( "GCAMUSA_ASSUMPTIONS", "A27.globaltech_cost")
A27.tech_associations <- readdata( "GCAMUSA_ASSUMPTIONS", "A27.tech_associations" )
L223.StubTechEff_elec_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechEff_elec_USA", skip = 4 )
L223.GlobalTechCapital_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_elec", skip = 4 )
L223.GlobalTechLifetime_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechLifetime_elec", skip = 4 )
L223.GlobalTechOMfixed_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMfixed_elec", skip = 4 )
L223.GlobalTechOMvar_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMvar_elec", skip = 4 )
L223.GlobalTechSCurve_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechSCurve_elec", skip = 4 )
L223.GlobalTechProfitShutdown_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalTechProfitShutdown_elec", skip = 4 )
L223.StubTechCalInput_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.StubTechCalInput_elec", skip = 4 )
L127.Dispatch_Reordering_Potential_USA <- readdata( "GCAMUSA_LEVEL1_DATA", "L127.Dispatch_Reordering_Potential_USA" )
L127.Remaining_Coal_USA <- readdata( "GCAMUSA_LEVEL1_DATA", "L127.Remaining_Coal_USA")



# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Supplysector information
printlog( "L227.Supplysector_disp_USA: Supply sector information for Coal-Gas Dispatch sector" )
L227.Supplysector_disp_USA <- write_to_all_states( A27.sector, names_Supplysector )
stopifnot(!any(is.na(L227.Supplysector_disp_USA)))

# 2b. Subsector information
printlog( "L227.SubsectorLogit_disp_USA: Subsector logit exponents of Coal-Gas Dispatch sector" )
L227.SubsectorLogit_disp_USA <- write_to_all_states( A27.subsector_logit, names_SubsectorLogit )

printlog( "L227.SubsectorShrwt_disp_USA and L227.SubsectorShrwtFllt_disp_USA: Subsector shareweights of electricity dispatch sector" )
if( any( !is.na( A27.subsector_shrwt$year ) ) ){
  L227.SubsectorShrwt_disp_USA <- write_to_all_states( A27.subsector_shrwt[ !is.na( A27.subsector_shrwt$year ), ], names_SubsectorShrwt )
  stopifnot(!any(is.na(L227.SubsectorShrwt_disp_USA)))
}
if( any( !is.na( A27.subsector_shrwt$year.fillout ) ) ){
  L227.SubsectorShrwtFllt_disp_USA <- write_to_all_states( A27.subsector_shrwt[ !is.na( A27.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
  stopifnot(!any(is.na(L227.SubsectorShrwtFllt_disp_USA)))
}

# 2c. Technology information
#Shareweights of global technologies
printlog( "L227.GlobalTechShrwt_disp_USA: Shareweights of global dispatch technologies" )
L227.globaltech_shrwt.melt <- interpolate_and_melt(
  A27.globaltech_shrwt[ complete.cases( A27.globaltech_shrwt ), ], model_years, value.name="share.weight" )
L227.globaltech_shrwt.melt.NAs <- melt( A27.globaltech_shrwt[ !complete.cases( A27.globaltech_shrwt ), ],
                                        measure.vars = grep( "X[0-9]{4}", names( A27.globaltech_shrwt ) ),
                                        value.name = "share.weight" ) %>%
  na.omit() %>%
  mutate(year = as.numeric(substr( variable, 2, 5 )))
L227.globaltech_shrwt.melt <- rbind( L227.globaltech_shrwt.melt, L227.globaltech_shrwt.melt.NAs ) %>%
  rename(sector.name=supplysector,subsector.name=subsector)
L227.GlobalTechShrwt_disp <- L227.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]
stopifnot(!any(is.na(L227.GlobalTechShrwt_disp)))

#Costs of global technologies
#Costs of techs in the dispatcher are set with standard non-energy inputs (not capital + O&M costs)
printlog( "L227.GlobalTechCost_elec: Capital costs of global electricity generation technologies" )
L227.globaltech_cost.melt <- interpolate_and_melt( A27.globaltech_cost, model_years, value.name="input.cost", 
                                                   digits = digits_cost ) %>%
  rename(sector.name = supplysector, subsector.name=subsector)
L227.GlobalTechCost_disp <- L227.globaltech_cost.melt[ names_GlobalTechCost ]
stopifnot(!any(is.na(L227.GlobalTechCost_disp)))

#Efficiencies of stub techs
printlog( "L227.StubTechEff_disp_USA: Efficiencies of dispatch technologies" )
L227.StubTechEff_disp_USA <- subset(L223.StubTechEff_elec_USA, paste(supplysector, subsector, stub.technology) %in%
                                  paste(A27.tech_associations$elec_supplysector, A27.tech_associations$elec_subsector, 
                                        A27.tech_associations$elec_technology))

L227.StubTechEff_disp_USA[c(supp, subs, "stub.technology" ) ] <- A27.tech_associations[
  match( vecpaste( L227.StubTechEff_disp_USA[c(supp, subs, "stub.technology" ) ] ),
         vecpaste( A27.tech_associations[c("elec_supplysector", "elec_subsector", "elec_technology" ) ] ) ),
  s_s_t]

#Because the pass through techs are not assigned a lifetime, the efficiencies from the final base year are
# copied forward to all future years
L227.StubTechEff_disp_USA_fy <- repeat_and_add_vector( subset( L227.StubTechEff_disp_USA, year == max( model_base_years ) ),
                                                       "year", model_future_years )
L227.StubTechEff_disp_USA <- rbind( L227.StubTechEff_disp_USA, L227.StubTechEff_disp_USA_fy )
stopifnot(!any(is.na(L227.StubTechEff_disp_USA)))

#Other global tech characteristics
printlog( "Electric sector technologies: inherit all characteristics from corresponding tech in the electricity sector")
elec_coal_tech <- "coal (conv pul)"
elec_disp_coal_tech <- A27.globaltech_shrwt$technology[ paste( A27.globaltech_shrwt$supplysector, A27.globaltech_shrwt$subsector )
                                                         %in% paste( L223.GlobalTechCapital_elec$sector.name, L223.GlobalTechCapital_elec$subsector.name ) ]

L227.GlobalTech.list <- list(  L223.GlobalTechCapital_elec = L223.GlobalTechCapital_elec,
                               L223.GlobalTechLifetime_elec = L223.GlobalTechLifetime_elec,
                               L223.GlobalTechOMfixed_elec = L223.GlobalTechOMfixed_elec,
                               L223.GlobalTechOMvar_elec = L223.GlobalTechOMvar_elec,
                               L223.GlobalTechSCurve_elec = L223.GlobalTechSCurve_elec,
                               L223.GlobalTechProfitShutdown_elec = L223.GlobalTechProfitShutdown_elec )
                               

#Rename, select relevant year and technology, and write these files out
for( i in 1:length( L227.GlobalTech.list ) ){
  if(grepl("L223.",names( L227.GlobalTech.list[i] )) == TRUE){
    objectname <- sub( "L223.", "L227.", names( L227.GlobalTech.list[i] ) )
  }
  
  objectname <- sub( "_elec", "_disp", objectname )
  tech_name <- names( L227.GlobalTech.list[[i]] )[ grepl( "technology", names( L227.GlobalTech.list[[i]] ) ) ]
  object <- L227.GlobalTech.list[[i]][
    L227.GlobalTech.list[[i]][[tech_name]] == elec_coal_tech &
      L227.GlobalTech.list[[i]][["year"]] == max( model_base_years ), ]
  if( nrow( object ) != 0 ){
    object[[tech_name]] <- elec_disp_coal_tech
    assign( objectname, object )
    IDstring <- substr( objectname, 6, regexpr( "_disp", objectname, fixed = T ) - 1 )
    if(any(is.na(object)) == TRUE){
      stop(paste(objectname,"contains missing values!",sep=" "))
    }
    write_mi_data( object, IDstring, "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", "batch_dispatcher_USA.xml" )
  }
}

#Calibrated inputs to dispatch stub techs
#Take the electric sector inputs from an existing table for the relevant base year and technology
printlog("L227.StubTechCalInput_elec_USA and L227.StubTechCalInput_elec_disp_USA: calibrated inputs to dispatcher
         related technologies")
L227.StubTechCalInput_elec_USA <- filter(L223.StubTechCalInput_elec, region == "USA" & 
                                           year == max( model_base_years) & stub.technology == elec_coal_tech ) %>%
  select(-region) %>%
  write_to_all_states(names_StubTechCalInput) -> L227.StubTechCalInput_elec_disp_USA

#Replace the energy input in the dispatcher stub technology with the energy from the correct supplysector
L227.StubTechCalInput_elec_disp_USA[[input]] <- A27.sector[[supp]]
L227.StubTechCalInput_elec_disp_USA[["stub.technology"]] <- elec_disp_coal_tech

#Match in the corresponding energy inputs to each sector table
L227.StubTechCalInput_elec_USA %>% 
  mutate(calibrated.value = L127.Remaining_Coal_USA[[X_final_model_base_year]][
    match(region, L127.Remaining_Coal_USA$state)]) %>%
  #Round to correct number of digits
  mutate(calibrated.value = round(calibrated.value, digits_calOutput)) %>%
  #Re-set subsector share weights
  set_subsector_shrwt(value.name="calibrated.value") %>%
  #Re-set technology share weights
  mutate(tech.share.weight = ifelse(calibrated.value > 0, 1, 0 )) -> L227.StubTechCalInput_elec_USA

L227.StubTechCalInput_elec_disp_USA %>% 
  mutate(calibrated.value = L127.Dispatch_Reordering_Potential_USA[[X_final_model_base_year]][
    match(region, L127.Dispatch_Reordering_Potential_USA$state)]) %>%
  #Round to correct number of digits
  mutate(calibrated.value = round(calibrated.value, digits_calOutput)) %>%
  #Re-set subsector share weights
  set_subsector_shrwt(value.name="calibrated.value") %>%
  #Re-set technology share weights
  mutate(tech.share.weight = ifelse(calibrated.value > 0, 1, 0 )) -> L227.StubTechCalInput_elec_disp_USA

stopifnot(!any(is.na(c(L227.StubTechCalInput_elec_USA,L227.StubTechCalInput_elec_disp_USA))))

#Availability of dispatcher
printlog( "L227.GlobalTechAvail_disp: Availability of dispatcher within the power sector")
L227.GlobalTechAvail_disp <- data.frame(
  A27.globaltech_shrwt[ A27.globaltech_shrwt$supplysector == "electricity", s_s_t ],
  initial.available.year = max( model_base_years ),
  final.available.year = max( model_base_years ) ) %>%
  rename_(sector.name = supp, subsector.name=subs)
stopifnot(!any(is.na(L227.GlobalTechAvail_disp)))

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L227.Supplysector_disp_USA, IDstring="Supplysector", domain="GCAMUSA_LEVEL2_DATA", fn="L227.Supplysector_disp_USA",
               batch_XML_domain="GCAMUSA_XML_BATCH", batch_XML_file="batch_dispatcher_USA.xml" ) 
write_mi_data( L227.SubsectorLogit_disp_USA, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L227.SubsectorLogit_disp_USA", "GCAMUSA_XML_BATCH", "batch_dispatcher_USA.xml" ) 

if( exists( "L227.SubsectorShrwt_disp_USA" ) ) {
  write_mi_data( L227.SubsectorShrwt_disp_USA, "SubsectorShrwt", "GCAMUSA_LEVEL2_DATA", "L227.SubsectorShrwt_disp_USA",
                 "GCAMUSA_XML_BATCH", "batch_dispatcher_USA.xml" )
}
if( exists( "L227.SubsectorShrwtFllt_disp_USA" ) ) {
  write_mi_data( L227.SubsectorShrwtFllt_disp_USA, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L227.SubsectorShrwtFllt_disp_USA",
                 "GCAMUSA_XML_BATCH", "batch_dispatcher_USA.xml" )
} 

write_mi_data( L227.GlobalTechAvail_disp, "GlobalTechAvail", "GCAMUSA_LEVEL2_DATA", "L227.GlobalTechAvail_disp", "GCAMUSA_XML_BATCH", "batch_dispatcher_USA.xml" ) 
write_mi_data( L227.GlobalTechShrwt_disp, "GlobalTechShrwt", "GCAMUSA_LEVEL2_DATA", "L227.GlobalTechShrwt_disp", "GCAMUSA_XML_BATCH", "batch_dispatcher_USA.xml" ) 
write_mi_data( L227.GlobalTechCost_disp, "GlobalTechCost", "GCAMUSA_LEVEL2_DATA", "L227.GlobalTechCost_disp", "GCAMUSA_XML_BATCH", "batch_dispatcher_USA.xml" ) 
write_mi_data( L227.StubTechEff_disp_USA, "StubTechEff", "GCAMUSA_LEVEL2_DATA", "L227.StubTechEff_disp_USA", "GCAMUSA_XML_BATCH", "batch_dispatcher_USA.xml" ) 
write_mi_data( L227.StubTechCalInput_elec_disp_USA, IDstring="StubTechCalInput", domain="GCAMUSA_LEVEL2_DATA", fn="L227.StubTechCalInput_elec_disp_USA",
               batch_XML_domain="GCAMUSA_XML_BATCH", batch_XML_file="batch_dispatcher_USA.xml" ) 
write_mi_data( L227.StubTechCalInput_elec_USA, "StubTechCalInput", "GCAMUSA_LEVEL2_DATA", "L227.StubTechCalInput_elec_USA", "GCAMUSA_XML_BATCH", "batch_dispatcher_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_dispatcher_USA.xml", "GCAMUSA_XML_FINAL", "dispatcher_USA.xml", "", xml_tag="outFile" )

logstop()


