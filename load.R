# For the N manure project
#
# Load libraries and query GCAM output
# Jillian Sturtevant, July 2026

# Change
#  INSTALL_PACKAGES to TRUE to install required packages
#  QUERY_GCAM to TRUE to re-query GCAM output
#  PATH_TO_GCAM to change the path to GCAM output
#  FIGS_DIR to change figures sub-directory e.g., figures/v1, v2, etc.

# set bools
INSTALL_PACKAGES <- FALSE # set to TRUE to install required packages
QUERY_GCAM <- TRUE # set to TRUE to re-query GCAM output

# install packages ----

if(INSTALL_PACKAGES){
  install.packages('ggplot2')
  install.packages('dplyr')
  install.packages('tidyr')
  install.packages('readr')
  install.packages('grid')
  install.packages('maps')
  install.packages("patchwork")
  install.packages("purrr")
  install.packages("ggtext")
  install.packages("egg")
  install.packages("scales")
  install.packages("ggrepel")

  install.packages("devtools")

  # install JGCRI packages from GitHub
  devtools::install_github("JGCRI/rgcam", build_vignettes = TRUE)
  devtools::install_github("JGCRI/rmap", build_vignettes = TRUE)
}

# load libraries ----
library(rgcam)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(grid)
library(maps)
library(patchwork)
library(rmap)
library(purrr)
library(ggtext)
library(egg)
library(scales)
library(ggrepel)

# paths ----
DATA_DIR <- "data/"
FIGS_DIR <- "figures/v1/" # change the subfolder here if desired

# create figs dir if it doesn't exist
if (!dir.exists(FIGS_DIR)) {
  dir.create(FIGS_DIR)
  paste0("Created directory: ", FIGS_DIR)
  }

# constants ----
CONV_USD_1975_2020 <- 3.8
CONV_KG_T <- 1000
H2_GJ_kg <- 0.1202
DAYS_PER_YEAR <- 365.25
CONV_PCAL_MCAL <- 1e9
CONV_NH3_N <- 14/17    # convert mass of NH3 to mass of N

HIST_YEARS <- c(1975, 1990, 2005, 2010, 2015, 2020, 2025)

# query GCAM ----
OUTPUTFILE <- "Nmanure.proj"

if (QUERY_GCAM) {
  # query variables
  PATH_TO_GCAM <- "output"
  
  SCENARIOS <- c("Reference")

  QUERYFILE <- "queries_nmanure.xml"

  # query
  conn <- localDBConn(PATH_TO_GCAM, "database_basexdb")
  Nmanure_proj <- addScenario(conn, OUTPUTFILE, SCENARIOS, QUERYFILE, clobber = TRUE)
}

# load project ----
Nmanure_proj <- loadProject(OUTPUTFILE)
# Nmanure_proj <- loadProject("Nmanure.proj")

print(paste0("Queries: ", listQueries(Nmanure_proj)))

# "Queries: population by region"                    "Queries: meat and dairy prices"                                                         
# "Queries: GDP MER by region"                       "Queries: ammonia production by region"           
# "Queries: ammonia production by tech"              "Queries: ammonia and N fertilizer prices"        
# "Queries: ammonia domestic supply"                 "Queries: ag production by crop type"             
# "Queries: ag commodity prices"                     "Queries: fertilizer consumption by region"       
# "Queries: fertilizer consumption by crop type"     "Queries: fertilizer consumption by ag tech"      
# "Queries: meat and dairy production by tech"       "Queries: feed consumption by region"             
# "Queries: feed consumption by meat and dairy tech" "Queries: feed sources"                           
# "Queries: feed prices"                             "Queries: food consumption by type (specific)"    
# "Queries: food demand prices"                      "Queries: food demand per capita"     

