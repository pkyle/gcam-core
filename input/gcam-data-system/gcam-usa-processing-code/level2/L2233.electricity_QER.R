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
 logstart( "L2233.electricity_QER.R" )
 adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
 adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
 printlog( "L2233.electricity_QER.R: Electricity sector QER add-ons" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A23.globalinttech <- readdata( "ENERGY_ASSUMPTIONS", "A23.globalinttech" )
A23.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_shrwt" )
L223.GlobalIntTechCapital_elec <- readdata("ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_elec", skip=4)
L223.GlobalTechCapital_elec <- readdata("ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_elec", skip=4)
A23.globaltech_capital_QER <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elec_overnight_costs_QER", skip=4)
A23.itc_QER <- readdata("GCAMUSA_ASSUMPTIONS", "A23.itc_QER", skip=2)
A23.globaltech_capital_QER_adv <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elec_overnight_costs_QER_adv", skip=4)
A23.OM_QER_adv <- readdata("GCAMUSA_ASSUMPTIONS", "A23.elec_OM_QER_adv", skip=3)
A23.ptc_QER <- readdata("GCAMUSA_ASSUMPTIONS", "A23.ptc_QER",skip=4)
A23.ptc_inttech_QER <- readdata("GCAMUSA_ASSUMPTIONS", "A23.ptc_inttech_QER", skip=4)

conv_2013_1975 <- 0.293288
conv_2014_2013 <- 0.983834

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Build GCAM capital cost table from QER data
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L223.GlobalTech_elec <- A23.globaltech_shrwt[names(A23.globaltech_shrwt) %in% names_Tech]

inttechs <- unique(A23.globalinttech$technology)

# QER capital costs 
printlog( "L2233.globaltech_capital_QER: Build table of QER capital costs")
L2233.globaltech_capital_QER <- repeat_and_add_vector(L223.GlobalTech_elec, "year", model_years)

# Match QER costs onto GCAM table and convert to 1975$
L2233.globaltech_capital_QER$overnight.cost <- A23.globaltech_capital_QER$overnight.cost[match(
                                            vecpaste(L2233.globaltech_capital_QER[c("year", "technology")]),
                                            vecpaste(A23.globaltech_capital_QER[c("year","GCAM.technology")]))] * conv_2013_1975

# Historical years are equal to 2010 costs
L2233.globaltech_capital_QER$overnight.cost <- ifelse(L2233.globaltech_capital_QER$year<2010, 
                                           L2233.globaltech_capital_QER$overnight.cost[L2233.globaltech_capital_QER$year==2010], 
                                           L2233.globaltech_capital_QER$overnight.cost)


# Future years are equal to 2045 costs
L2233.globaltech_capital_QER$overnight.cost <- ifelse(L2233.globaltech_capital_QER$year>2040, 
                                           L2233.globaltech_capital_QER$overnight.cost[L2233.globaltech_capital_QER$year==2040], 
                                           L2233.globaltech_capital_QER$overnight.cost)


# 2b. Build tables for overnight cost and fcr
# Get intermittent capital data
printlog("L2233.GlobalIntTechCapital_elec_QER: Building intermittent technology capital cost")
L2233.GlobalIntTechCapital_elec_QER <- L2233.globaltech_capital_QER[L2233.globaltech_capital_QER$technology %in% inttechs,]
names(L2233.GlobalIntTechCapital_elec_QER)[names(L2233.GlobalIntTechCapital_elec_QER)=="technology"]<- "intermittent.technology"

L2233.GlobalIntTechCapital_elec_QER <- na.omit(L2233.GlobalIntTechCapital_elec_QER)
L2233.GlobalIntTechCapital_elec_QER$input.capital <- "capital"

# Get intermittent fcr w/ ITC 
printlog("L2233.GlobalIntTechFCR_elec_qer: Building intermittent technology FCR csv")
L2233.GlobalIntTechFCR_elec_qer <- L2233.globaltech_capital_QER[L2233.globaltech_capital_QER$technology %in% inttechs,]
names(L2233.GlobalIntTechFCR_elec_qer)[names(L2233.GlobalIntTechFCR_elec_qer)=="technology"]<- "intermittent.technology"

L2233.GlobalIntTechFCR_elec_qer$input.capital <- "capital"
L2233.GlobalIntTechFCR_elec_qer <- L2233.GlobalIntTechFCR_elec_qer[names(L2233.GlobalIntTechFCR_elec_qer) %!in% "overnight.cost"]

L2233.GlobalIntTechFCR_elec_qer$fixed.charge.rate <- ifelse(!is.na(match(vecpaste(L2233.GlobalIntTechFCR_elec_qer[c("intermittent.technology", "year")]),
                                                           vecpaste(A23.itc_QER[c("GCAM.technology", "year")]))), 
                                                           L223.GlobalIntTechCapital_elec$fixed.charge.rate[match(vecpaste(L2233.GlobalIntTechFCR_elec_qer[c("intermittent.technology", "year")]),
                                                                                                vecpaste(L223.GlobalIntTechCapital_elec[c("intermittent.technology", "year")]))] * 
                                                (1 - A23.itc_QER$itc[match(vecpaste(L2233.GlobalIntTechFCR_elec_qer[c("intermittent.technology", "year")]),
                                                                            vecpaste(A23.itc_QER[c("GCAM.technology", "year")]))]),NA)

L2233.GlobalIntTechFCR_elec_qer <- na.omit(L2233.GlobalIntTechFCR_elec_qer)


# Get overnight capital for other techs
printlog("L2233.GlobalTechCapital_elec_QER: other elec technology capital cost")
L2233.GlobalTechCapital_elec_QER <- L2233.globaltech_capital_QER[L2233.globaltech_capital_QER$technology %!in% inttechs,]
L2233.GlobalTechCapital_elec_QER$input.capital <- "capital"
L2233.GlobalTechCapital_elec_QER <- na.omit(L2233.GlobalTechCapital_elec_QER)

# Get fcr (w/ itc) for other techs
printlog("L2233.GlobalTechFCR_elec_qer: other elec technology FCR")
L2233.GlobalTechFCR_elec_qer <- L2233.globaltech_capital_QER[L2233.globaltech_capital_QER$technology %!in% inttechs,]
L2233.GlobalTechFCR_elec_qer <- L2233.GlobalTechFCR_elec_qer[names(L2233.GlobalTechFCR_elec_qer)!="overnight.cost"]
L2233.GlobalTechFCR_elec_qer$input.capital <- "capital"

L2233.GlobalTechFCR_elec_qer$fixed.charge.rate <- ifelse(!is.na(match(vecpaste(L2233.GlobalTechFCR_elec_qer[c("technology", "year")]),
                                                           vecpaste(A23.itc_QER[c("GCAM.technology", "year")]))), 
                                                         L223.GlobalTechCapital_elec$fixed.charge.rate[match(vecpaste(L2233.GlobalTechFCR_elec_qer[c("technology", "year")]),
                                                                                                vecpaste(L223.GlobalTechCapital_elec[c("technology", "year")]))] * 
                                                (1 - A23.itc_QER$itc[match(vecpaste(L2233.GlobalTechFCR_elec_qer[c("technology", "year")]),
                                                                            vecpaste(A23.itc_QER[c("GCAM.technology", "year")]))]),NA)

L2233.GlobalTechFCR_elec_qer <- na.omit(L2233.GlobalTechFCR_elec_qer)

# 2c. Generate csvs for advanced QER costs
printlog("L2233.globaltech_capital_QER_adv: Advanced QER capital costs")
L2233.globaltech_capital_QER_adv <- repeat_and_add_vector(L223.GlobalTech_elec, "year", model_years)

# Match QER costs onto GCAM table and convert to 1975$
L2233.globaltech_capital_QER_adv$overnight.cost <- A23.globaltech_capital_QER_adv$overnight.cost[match(
  vecpaste(L2233.globaltech_capital_QER_adv[c("year", "technology")]),
  vecpaste(A23.globaltech_capital_QER_adv[c("year","GCAM.technology")]))] * conv_2013_1975

  
# 2010 extended backward to past years
L2233.globaltech_capital_QER_adv$overnight.cost <- ifelse(L2233.globaltech_capital_QER_adv$year<2010, 
                                               L2233.globaltech_capital_QER_adv$overnight.cost[L2233.globaltech_capital_QER_adv$year==2010], 
                                               L2233.globaltech_capital_QER_adv$overnight.cost)

# Future years are equal to 2045 years
L2233.globaltech_capital_QER_adv$overnight.cost <- ifelse(L2233.globaltech_capital_QER_adv$year>2040, 
                                               L2233.globaltech_capital_QER_adv$overnight.cost[L2233.globaltech_capital_QER_adv$year==2040], 
                                               L2233.globaltech_capital_QER_adv$overnight.cost)

# QER Cap cost adv
printlog("L2233.GlobalIntTechCapital_elec_QER_adv: Advanced intermittent tech QER capital costs")

L2233.GlobalIntTechCapital_elec_QER_adv <- L2233.globaltech_capital_QER_adv[L2233.globaltech_capital_QER_adv$technology %in% inttechs,]
names(L2233.GlobalIntTechCapital_elec_QER_adv)[names(L2233.GlobalIntTechCapital_elec_QER_adv)=="technology"]<- "intermittent.technology"

L2233.GlobalIntTechCapital_elec_QER_adv <- na.omit(L2233.GlobalIntTechCapital_elec_QER_adv)
L2233.GlobalIntTechCapital_elec_QER_adv$input.capital <- "capital"

printlog("L2233.GlobalTechCapital_elec_QER_adv: Advanced intermittent tech QER capital costs")

L2233.GlobalTechCapital_elec_QER_adv <- L2233.globaltech_capital_QER_adv[L2233.globaltech_capital_QER_adv$technology %!in% inttechs,]
L2233.GlobalTechCapital_elec_QER_adv <- na.omit(L2233.GlobalTechCapital_elec_QER_adv)
L2233.GlobalTechCapital_elec_QER_adv$input.capital <- "capital"


#OM Adv Costs
printlog("L2233.GlobalIntTechFOM_elec_qer_adv: Advanced intermittent tech QER FO&M costs")

L2233.GlobalIntTechFOM_elec_qer_adv <- L2233.globaltech_capital_QER[L2233.globaltech_capital_QER$technology %in% inttechs,]
names(L2233.GlobalIntTechFOM_elec_qer_adv)[names(L2233.GlobalIntTechFOM_elec_qer_adv)=="technology"]<- "intermittent.technology"

L2233.GlobalIntTechFOM_elec_qer_adv <- L2233.GlobalIntTechFOM_elec_qer_adv[names(L2233.GlobalIntTechFOM_elec_qer_adv) != "overnight.cost"]
L2233.GlobalIntTechFOM_elec_qer_adv$input.OM.fixed <- "OM-fixed"


L2233.GlobalIntTechFOM_elec_qer_adv$OM.fixed <- A23.OM_QER_adv$OM.fixed[match(vecpaste(L2233.GlobalIntTechFOM_elec_qer_adv[c("year", "intermittent.technology")]),
                                                                  vecpaste(A23.OM_QER_adv[c("year","GCAM.technology")]))]


L2233.GlobalIntTechFOM_elec_qer_adv$OM.fixed <- ifelse(L2233.GlobalIntTechFOM_elec_qer_adv$year < 2015, L2233.GlobalIntTechFOM_elec_qer_adv$OM.fixed[L2233.GlobalIntTechFOM_elec_qer_adv$year==2015],
                                    ifelse(L2233.GlobalIntTechFOM_elec_qer_adv$year > 2040, L2233.GlobalIntTechFOM_elec_qer_adv$OM.fixed[L2233.GlobalIntTechFOM_elec_qer_adv$year==2040], L2233.GlobalIntTechFOM_elec_qer_adv$OM.fixed)) * conv_2014_2013 * conv_2013_1975

L2233.GlobalIntTechFOM_elec_qer_adv <- na.omit(L2233.GlobalIntTechFOM_elec_qer_adv)


# OM fixed other techs
printlog("L2233.GlobalTechFOM_elec_qer_adv: Advanced tech QER FO&M costs")

L2233.GlobalTechFOM_elec_qer_adv <- L2233.globaltech_capital_QER[L2233.globaltech_capital_QER$technology %!in% inttechs,]
L2233.GlobalTechFOM_elec_qer_adv <- L2233.GlobalTechFOM_elec_qer_adv[names(L2233.GlobalTechFOM_elec_qer_adv) != "overnight.cost"]
L2233.GlobalTechFOM_elec_qer_adv$input.OM.fixed <- "OM-fixed"

L2233.GlobalTechFOM_elec_qer_adv$OM.fixed <- A23.OM_QER_adv$OM.fixed[match(vecpaste(L2233.GlobalTechFOM_elec_qer_adv[c("year", "technology")]),
                                                                  vecpaste(A23.OM_QER_adv[c("year","GCAM.technology")]))]


L2233.GlobalTechFOM_elec_qer_adv$OM.fixed <- ifelse(L2233.GlobalTechFOM_elec_qer_adv$year < 2015, L2233.GlobalTechFOM_elec_qer_adv$OM.fixed[L2233.GlobalTechFOM_elec_qer_adv$year==2015],
                                    ifelse(L2233.GlobalTechFOM_elec_qer_adv$year > 2040, L2233.GlobalTechFOM_elec_qer_adv$OM.fixed[L2233.GlobalTechFOM_elec_qer_adv$year==2040], L2233.GlobalTechFOM_elec_qer_adv$OM.fixed)) * conv_2014_2013 * conv_2013_1975

L2233.GlobalTechFOM_elec_qer_adv <- na.omit(L2233.GlobalTechFOM_elec_qer_adv)

# Variable OM
printlog("L2233.GlobalTechVOM_elec_qer_adv: Advanced tech QER variable O&M costs")

L2233.GlobalTechVOM_elec_qer_adv <- L2233.globaltech_capital_QER[L2233.globaltech_capital_QER$technology %!in% inttechs,]
L2233.GlobalTechVOM_elec_qer_adv <- L2233.GlobalTechVOM_elec_qer_adv[names(L2233.GlobalTechVOM_elec_qer_adv) != "overnight.cost"]
L2233.GlobalTechVOM_elec_qer_adv$input.OM.var <- "OM-var"

L2233.GlobalTechVOM_elec_qer_adv$OM.var <- A23.OM_QER_adv$OM.variable[match(vecpaste(L2233.GlobalTechVOM_elec_qer_adv[c("year", "technology")]),
                                                                  vecpaste(A23.OM_QER_adv[c("year","GCAM.technology")]))]

L2233.GlobalTechVOM_elec_qer_adv$OM.var <- ifelse(L2233.GlobalTechVOM_elec_qer_adv$year < 2015, L2233.GlobalTechVOM_elec_qer_adv$OM.var[L2233.GlobalTechVOM_elec_qer_adv$year==2015],
                                    ifelse(L2233.GlobalTechVOM_elec_qer_adv$year > 2040, L2233.GlobalTechVOM_elec_qer_adv$OM.var[L2233.GlobalTechVOM_elec_qer_adv$year==2040], L2233.GlobalTechVOM_elec_qer_adv$OM.var))* conv_2014_2013 * conv_2013_1975

L2233.GlobalTechVOM_elec_qer_adv <- na.omit(L2233.GlobalTechVOM_elec_qer_adv)

# Rearrange columns for capital costs
L2233.GlobalTechCapital_elec_QER <- L2233.GlobalTechCapital_elec_QER[c("supplysector", "subsector", "technology","year","input.capital","overnight.cost")]
L2233.GlobalIntTechCapital_elec_QER <- L2233.GlobalIntTechCapital_elec_QER[c("supplysector","subsector","intermittent.technology","year","input.capital","overnight.cost")]

L2233.GlobalTechCapital_elec_QER_adv <- L2233.GlobalTechCapital_elec_QER_adv[c("supplysector", "subsector", "technology","year","input.capital","overnight.cost")]
L2233.GlobalIntTechCapital_elec_QER_adv <- L2233.GlobalIntTechCapital_elec_QER_adv[c("supplysector", "subsector", "intermittent.technology","year","input.capital","overnight.cost")]

# PTC Costs




# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

# Capital Costs
write_mi_data(L2233.GlobalTechCapital_elec_QER, "GlobalTechCapitalOnly", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechCapital_elec_QER", "GCAMUSA_XML_BATCH", "batch_elec_capital_QER.xml")
write_mi_data(L2233.GlobalIntTechCapital_elec_QER, "GlobalIntTechCapitalOnly", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechCapital_elec_QER", "GCAMUSA_XML_BATCH", "batch_elec_capital_QER.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_capital_QER.xml", "GCAMUSA_XML_FINAL", "elec_capital_QER.xml", "", xml_tag="outFile" )

write_mi_data(L2233.GlobalTechCapital_elec_QER_adv, "GlobalTechCapitalOnly", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechCapital_elec_QER_adv", "GCAMUSA_XML_BATCH", "batch_elec_capital_QER_adv.xml")
write_mi_data(L2233.GlobalIntTechCapital_elec_QER_adv, "GlobalIntTechCapitalOnly", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechCapital_elec_QER_adv", "GCAMUSA_XML_BATCH", "batch_elec_capital_QER_adv.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_capital_QER_adv.xml", "GCAMUSA_XML_FINAL", "elec_capital_QER_adv.xml", "", xml_tag="outFile" )

# FCR ITC
write_mi_data(L2233.GlobalTechFCR_elec_qer, "GlobalTechFCROnly", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechFCR_elec_QER", "GCAMUSA_XML_BATCH", "batch_elec_itc_QER.xml")
write_mi_data(L2233.GlobalIntTechFCR_elec_qer, "GlobalIntTechFCROnly", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechFCR_elec_QER", "GCAMUSA_XML_BATCH", "batch_elec_itc_QER.xml")
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_itc_QER.xml", "GCAMUSA_XML_FINAL", "elec_itc_QER.xml", "", xml_tag="outFile" )

# O&M Adv
write_mi_data(L2233.GlobalIntTechFOM_elec_qer_adv, "GlobalIntTechOMfixedOnly", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechFOM_elec_qer_adv", "GCAMUSA_XML_BATCH", "batch_elec_om_QER_adv.xml")
write_mi_data(L2233.GlobalTechFOM_elec_qer_adv, "GlobalTechOMfixedOnly", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechFOM_elec_qer_adv", "GCAMUSA_XML_BATCH", "batch_elec_om_QER_adv.xml")
write_mi_data(L2233.GlobalTechVOM_elec_qer_adv, "GlobalTechOMvar", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechVOM_elec_qer_adv", "GCAMUSA_XML_BATCH", "batch_elec_om_QER_adv.xml")

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_elec_om_QER_adv.xml", "GCAMUSA_XML_FINAL", "elec_om_QER_adv.xml", "", xml_tag="outFile" )

# PTC
write_mi_data(A23.ptc_QER, "GlobalTechRegPriceAdj", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalTechPTC_QER", "GCAMUSA_XML_BATCH", "batch_elec_ptc_QER.xml")
write_mi_data(A23.ptc_inttech_QER, "GlobalIntTechRegPriceAdj", "GCAMUSA_LEVEL2_DATA", "L2233.GlobalIntTechPTC_QER", "GCAMUSA_XML_BATCH", "batch_elec_ptc_QER.xml")
insert_file_into_batchxml("GCAMUSA_XML_BATCH", "batch_elec_ptc_QER.xml", "GCAMUSA_XML_FINAL", "elec_ptc_QER.xml", "", xml_tag="outFile")


logstop()
