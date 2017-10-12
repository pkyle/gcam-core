#---------------------------------------------------------------------
#UNIT CONVERSIONS
conv_ones_mil <- 1e-6
conv_ones_thous <- 1e-3
conv_thous_mil <- 1e-3
conv_mil_bil <- 1e-3
conv_bil_mil <- 1e3
conv_mil_thous <- 1e3
conv_bil_thous <- 1e6
conv_thous_bil <- 1e-6
conv_C_CO2 <- 44/12
conv_CO2_C <- 12/44

#Dollar conversion factors
conv_1990_2005_USD <- 1.383
conv_1990_2007_USD <- 1.470
conv_1990_2010_USD <- 1.510
conv_1990_1975_USD <- 0.4649
conv_1996_1975_USD <- 0.4049
conv_1997_1975_USD <- 0.3983
conv_1998_1975_USD <- 0.3939
conv_1999_1975_USD <- 0.3883
conv_2000_1975_USD <- 0.380
conv_2001_1975_USD <- 0.3711
conv_2002_1975_USD <- 0.3647
conv_2003_1975_USD <- 0.3571
conv_2004_1975_USD <- 0.3472
conv_2005_1975_USD <- 0.3362
conv_2006_1975_USD <- 0.3257
conv_2007_1975_USD <- 0.317
conv_2008_1975_USD <- 0.3104
conv_2009_1975_USD <- 0.3104
conv_2013_1975_USD <- 0.2933

#mass conversions
conv_kg_Mt <- 1e-9
conv_t_Mt <- 1e-6
conv_kt_Mt <- 1e-3
conv_Pg_to_Tg <- 1000
conv_t_kg <- 1e3
conv_kg_t <- 1e-3
conv_g_kg <- 1e-3
conv_t_metric_short <- 1.10231

#calorie conversions
conv_Mcal_Pcal <- 1e-9
conv_Pcal_Gcal <- 1e6
conv_Pcal_Mcal <- 1e9

#volume conversions
conv_m3_bm3 <- 1e-9
conv_Mm3_km3 <- 1e-3
conv_bcm_bcf <- 35.3147

#area conversions
conv_Ha_bm2 <- 1e-5
conv_Ha_m2 <- 1e4
conv_m2_acr <- 1/4046.85
conv_km2_bm2 <- 1e-3
conv_bm2_m2 <- 1e9
conv_milft2_bm2 <- 0.0929/1e3     #million square feet to billion square meters
conv_milft2_m2 <- 0.0929 * 1e6
conv_ft2_bm2 <- 0.0929/1e9        #square feet to billion square meters
conv_ft2_m2 <- 0.0929


#yield conversions
conv_tha_kgm2 <- 0.1

#cotton seed and lint conversion
conv_cotton_lint <- 0.4

#energy unit conversions
conv_kbbl_bbl <- 1000
conv_bbl_tonne_RFO <- 1 / 6.66
conv_bbl_tonne_distillate <- 1 / 7.46
conv_days_year <- 1 / 365.25
conv_year_hours <- 8766
conv_tonne_GJ_RFO <- 40.87
conv_tonne_GJ_distillate <- 42.91
conv_GJ_EJ <- 1e-9
conv_kwh_GJ <- 0.0036
conv_TWh_EJ <- 3.6e-3
conv_GWh_EJ <- 3.6e-6
conv_MWh_EJ <- 3.6e-9
conv_MWh_GJ <- 3.6
conv_MJ_btu <- 947.777
conv_EJ_GJ <- 1e9
conv_btu_kJ <- 1.0551
conv_mtoe_EJ <- 0.04187
conv_ft3_btu_gas <- 983
conv_btu_EJ <- conv_btu_kJ*1e-15

#convert volume of natural gas into energy
conv_MMcf_EJ <- 0.0000010550559

#from billion barrels a day to EJ per year
conv_bbld_EJyr <- 6.119 * 365.25 * 1e-3

conv_Tbtu_EJ <- 0.0010551         #trillion btu to EJ
conv_kbtu_EJ <- 1.0551e-12        #thousand btu to EJ

#from volume natural gas to EJ
conv_bcm_EJ <- .03662134
conv_bcf_EJ <- 0.001037

#from MMcf natural gas to bcm 
conv_MMcf_bcm <- conv_ft2_m2 * 0.3048 / 1000

#from MT LNG to bcf natural gas
conv_MT_bcf_LNG <- 45.909
