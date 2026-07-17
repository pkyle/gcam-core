# This analysis script generates figures for the ammonia and food security paper
#
# originally from AGU2024 scenario set
# by Page Kyle, December 2024
#
# Edited by 


# load environment ----
source("load.R"); paste0("Figures in ", FIGS_DIR)

a<- getQuery(Nmanure_proj, "meat and dairy production by tech")

# load data ----
ghg_gwp <- read_csv(paste0(DATA_DIR, "ghg_gwp.csv"))
region_mapping <- read_csv(paste0(DATA_DIR, "region_mapping.csv"))

# Nmanure_proj <- loadProject("Nmanure.proj")
#
# listQueries(food_ammonia_proj)

# analysis constants ----
ANALYSIS_YEARS <- c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)
ANALYSIS_YEARS_FUTURE <- c(2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)
ANALYSIS_REGIONS <- c("Africa_Southern", "Brazil", "China", "India", "USA", "Indonesia")

# plot vars ----
# TODO: rename refined liquids to petroleum
FIGS_SAVE <- TRUE  # set to TRUE to save figures to FIGS_DIR

# plots ----

###############################################################################%
# 1) Query: Meat & Dairy Production Technology  ----
meat_dairy_prod_tech <- getQuery(Nmanure_proj, "meat and dairy production by tech") %>%
  filter(year %in% ANALYSIS_YEARS)

# Fig1, Manure production (Mt) on Mixed Land ----

Meatdairy_prodmanure_mixed <- filter(meat_dairy_prod_tech, subsector == "Mixed" & output == "manure" & year ==2025)

# Reorder 'region' by total value (descending)
Meatdairy_prodmanure_mixed <- Meatdairy_prodmanure_mixed %>%
  group_by(region) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  # Join back to original data to keep all columns
  right_join(Meatdairy_prodmanure_mixed, by = "region") %>%
  mutate(region = factor(region, levels = unique(region)))

# Manure Production (Mt) raw numbers
ggplot(Meatdairy_prodmanure_mixed, aes(x = region, y = value, fill = sector)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "Manure Production (Mt), Mixed, 2025", fill = "Livestock Type") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt labels 45 degrees
  )

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Manure_Prod_Mix_2025.png"), height = 6, width = 8, units = "in")}


# Manure Production (Mt) percentage
ggplot(Meatdairy_prodmanure_mixed, aes(x = region, y = value, fill = sector)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "", y = "Manure Production (Mt), Mixed, 2025", fill = "Livestock Type") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt labels 45 degrees
  )

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Manure_Prod_Mix_Percent_2025.png"), height = 6, width = 8, units = "in")}


#Fig2, Manure production (Mt) on Pastoral Land ----

Meatdairy_prodmanure_pastoral <- filter(meat_dairy_prod_tech, subsector == "Pastoral" & output == "manure" & year ==2025)

# Reorder 'region' by total value (descending)
Meatdairy_prodmanure_pastoral <- Meatdairy_prodmanure_pastoral %>%
  group_by(region) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  # Join back to original data to keep all columns
  right_join(Meatdairy_prodmanure_pastoral, by = "region") %>%
  mutate(region = factor(region, levels = unique(region)))

# Manure Production (Mt) raw numbers
ggplot(Meatdairy_prodmanure_pastoral, aes(x = region, y = value, fill = sector)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "Manure Production (Mt), Pastoral, 2025", fill = "Livestock Type") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt labels 45 degrees
  )

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Manure_Prod_Past_2025.png"), height = 6, width = 8, units = "in")}


# Manure Production (Mt) percentage
ggplot(Meatdairy_prodmanure_pastoral, aes(x = region, y = value, fill = sector)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "", y = "Manure Production (Mt), Pastoral, 2025", fill = "Livestock Type") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt labels 45 degrees
  )

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Manure_Prod_Past_Percent_2025.png"), height = 6, width = 8, units = "in")}


#Fig3, Manure production (Mt) Mixed vs Pastoral ----

Meatdairy_prodmanure_compare<- filter(meat_dairy_prod_tech, output == "manure" & year ==2025)

# 
ggplot(Meatdairy_prodmanure_compare, aes(x = region, y = value, fill = subsector)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "", y = "Manure Production (Mt) 2025", fill = "Land Type") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt labels 45 degrees
  )

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Manure_Prod_Percent_2025.png"), height = 6, width = 8, units = "in")}


#Fig4, Manure production (Mt) over time, USA

manure_mixed_allyears<- filter(meat_dairy_prod_tech, subsector == "Mixed" & output == "manure")

#make year numeric so it plots correctly
manure_mixed_allyears$year <- as.numeric(as.character(manure_mixed_allyears$year))

manure_mixed_allyears1 <- manure_mixed_allyears %>%
  group_by(region,sector,year) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

Region_names <- unique(manure_mixed_allyears1$region)

ggplot(manure_mixed_allyears1, aes(x = year, y = total_value, color = sector, group = sector)) +
  geom_point(size = 1.5) +    
  geom_line(size = 1) +        
  labs(x = "",y = "Manure Production (Mt)",color = "Livestock Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ region)

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Manure_Prod_M_allyears.png"), height = 11, width = 14, units = "in")}


# 2) Query: fertilizer consumption by crop type  ----
fert_consump_crop_type <- getQuery(Nmanure_proj, "fertilizer consumption by crop type") %>%
  filter(year %in% ANALYSIS_YEARS)

#Fig5, Crop Fertilizer Demands, line plots for all regions ----

fert_consump_crop_type1 <- fert_consump_crop_type %>%
  group_by(region,sector,year) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

Region_names <- unique(fert_consump_crop_type1$region)

ggplot(fert_consump_crop_type1, aes(x = year, y = total_value, color = sector, group = sector)) +
  geom_point(size = 1.5) +    
  geom_line(size = 1) +        
  labs(x = "",y = "Fertilizer Consumption by Crop (Mt N)",color = "Crop Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ region)

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Fert_consump_Crop_allyears.png"), height = 11, width = 14, units = "in")}

#Fig6, Crop Fertilizer Demands, stacked bar chart ----
fert_consump_crop_type2<- filter(fert_consump_crop_type, year == "2025")

ggplot(fert_consump_crop_type2, aes(x = region, y = value, fill = sector)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "", y = "Fertilizer Consumption by Crop (Mt N) 2025", fill = "Crop Type") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt labels 45 degrees
  )

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Fert_consump_Crop_Percent_2025.png"), height = 6, width = 8, units = "in")}



#Fig7, Total manure prod (Mt N) vs Total fertilizer consumption (Mt N) ----

#manure_mixed_allyears<- filter(meat_dairy_prod_tech, subsector == "Mixed" & output == "manure")

fert_consump_crop_type_R_Y<- fert_consump_crop_type %>%
  group_by(region,year) %>%
  summarise(Fertilizer_Consumption = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

manure_mixed_allyears_R_Y <- manure_mixed_allyears %>%
  group_by(region,year) %>%
  summarise(Manure_Production = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

fertconsumpt_vs_manureprod <- manure_mixed_allyears_R_Y %>%
  inner_join(fert_consump_crop_type_R_Y, by = c("region", "year")) 

fertconsumpt_vs_manureprod_long <- fertconsumpt_vs_manureprod %>%
  pivot_longer(
    cols = c(Manure_Production, Fertilizer_Consumption),  
    names_to = "MtN",               
    values_to = "value")

Region_names <- unique(manure_mixed_allyears1$region)

ggplot(fertconsumpt_vs_manureprod_long, aes(x = year, y = value, color = MtN, group = MtN)) +
  geom_point(size = 1.5) +    
  geom_line(size = 1) +        
  labs(x = "",y = "Mt Nitrogen",color = "Totals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ region)

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "ManureProd_vs_FertConsp_MtN.png"), height = 11, width = 14, units = "in")}


#Fig8, Total manure prod (Mt N) vs Total fertilizer consumption (Mt N) histograms ----

fert_consump_crop_type_2025<- fert_consump_crop_type %>%
  filter(year == "2025") %>%
 # rename(Fertilizer_Consumption = value) %>%
  select(-Units, -scenario, -sector,-input,-year)

manure_mixed_allyears_2025<- manure_mixed_allyears %>%
  filter(year == "2025") %>%
#  rename(Manure_Production = value) %>%
  select(-Units, -scenario, -sector,-subsector,-technology,-output,-year)

fert_consump_crop_type_2025$dataset <- "Fertilizer Consumption"
manure_mixed_allyears_2025$dataset <- "Manure Production"

df_all <- bind_rows(fert_consump_crop_type_2025, manure_mixed_allyears_2025)

ggplot(df_all, aes(x = value, fill = dataset, color = dataset)) +
  geom_density(alpha = 0.4, adjust = 1) +  # adjust controls smoothness
  labs(x = "Mt Nitrogen",y = "Density",fill = "Dataset for 2025",color = "Dataset for 2025") +
  theme_minimal() +
  facet_wrap(~ region, scales = "free")

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "ManureProd_vs_FertConsump_MtN_2025.png"), height = 11, width = 14, units = "in")}


# 3) Query: fertilizer consumption by region  ----
fert_consump_tech <- getQuery(Nmanure_proj, "fertilizer consumption by region") %>%
  filter(year %in% ANALYSIS_YEARS)

# 4) Query: ammonia and N fertilizer prices  ----
fert_consump_tech <- getQuery(Nmanure_proj, "ammonia and N fertilizer prices") %>%
  filter(year %in% ANALYSIS_YEARS)

# 5) Query: ammonia and N fertilizer prices  ----
fert_consump_tech <- getQuery(Nmanure_proj, "ammonia production by region") %>%
  filter(year %in% ANALYSIS_YEARS)

# 6) Query: ammonia and N fertilizer prices  ----
fert_consump_tech <- getQuery(Nmanure_proj, "ammonia production by tech") %>%
  filter(year %in% ANALYSIS_YEARS)

# 7) Query: ammonia and N fertilizer prices  ----
fert_consump_tech <- getQuery(Nmanure_proj, "ammonia domestic supply") %>%
  filter(year %in% ANALYSIS_YEARS)

# 8) Query: ag production by crop type  ----
crop_production <- getQuery(Nmanure_proj, "ag production by crop type") %>%
  filter(year %in% ANALYSIS_YEARS)

# 9) Query: ag commodity prices  ----
crop_production <- getQuery(Nmanure_proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS)

# 10) Query: food demand per capita  ----
fert_consump_tech <- getQuery(Nmanure_proj, "food demand per capita") %>%
  filter(year %in% ANALYSIS_YEARS)

# 11) Query: food consumption by type  ----
fert_consump_tech <- getQuery(Nmanure_proj, "food consumption by type") %>%
  filter(year %in% ANALYSIS_YEARS)

# 12) Query: food demand prices  ----
fert_consump_tech <- getQuery(Nmanure_proj, "food demand prices") %>%
  filter(year %in% ANALYSIS_YEARS)

# 13) Query: feed sources  ----
fert_consump_tech <- getQuery(Nmanure_proj, "feed sources") %>%
  filter(year %in% ANALYSIS_YEARS)

# 14) Query: feed prices  ----
fert_consump_tech <- getQuery(Nmanure_proj, "feed prices") %>%
  filter(year %in% ANALYSIS_YEARS)

# 15) Query: feed consumption by region  ----
fert_consump_tech <- getQuery(Nmanure_proj, "feed consumption by region") %>%
  filter(year %in% ANALYSIS_YEARS)

# ============================================================================#


# END OF SCRIPT ----
