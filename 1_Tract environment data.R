#########
# Description: R script for harmonizing and cleaning census tract environmental variables. 
# Author: Wei Xu
# Organization: Medical College of Wisconsin
# Date: Jun, 2023-December, 2023
#########

library(stringr)
library(haven)
library(tidyverse)
library(data.table)
library(gtools)
library(readxl)
library(sf)
library(tidycensus)
library(tigris)

# setting up working directory
setwd("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Data")


# tract variables cleanup -----

## EJScreen -----

# The 2021 public release started to include data based on the 2020 census tract
# geographies. Public releases prior to 2016 were based on census block groups only.
# 2016 is the most recent year EJS data is based on 2010 census geography but
# the original data is at the census block group level.

ejscreen2016 <- read_csv("EJScreen/2016/EJSCREEN_Full_V3_USPR_TSDFupdate.csv")
names(ejscreen2016)

# keep relevant columns
ejscreen2016_1 <- ejscreen2016 |> 
  select(ID,DSLPM,CANCER,RESP,PTRAF,PWDIS,PNPL,PRMP,PTSDF,OZONE,PM25)

# block group values for PTRAF,PWDIS,PNPL,PRMP,PTSDF are not the same within
# tracts, values for other variables are. keep variables for which tract
# and block group values are the same. 

ejscreen2016_2 <- ejscreen2016 |> 
  select(ID, DSLPM, OZONE, PM25) |>
  mutate(
    statefips = substr(ID, 1, 2),
    countyfips = substr(ID, 3, 5),
    tractfips = substr(ID, 6, 11)
  ) |> 
  filter(!statefips %in% c("02","15","72","78","69","60","66")) |>
  select(-ID) |>
  distinct() |> 
  mutate(geoId = paste0(statefips, countyfips, tractfips))

# save cleaned up EJScreen 2016 data
write.csv(ejscreen2016_2, "EJScreen/2016/ejscreen2016.csv")



## EJI -----

# 
eji <- read.csv("EJI/United States.csv") %>% 
  select(STATEFP, COUNTYFP, TRACTCE, E_ROAD, E_RAIL, E_AIRPRT, 
         E_LEAD, E_PARK, E_WLKIND, E_NPL, E_TRI, E_PM, E_OZONE, 
         E_TSD, E_RMP, E_COAL, E_IMPWTR, E_HOUAGE) %>% 
  mutate(
    statefips = str_pad(STATEFP, 2, pad = "0"),
    countyfips = str_pad(COUNTYFP, 3, pad = "0"),
    tractfips = str_pad(TRACTCE, 6, pad = "0"),
    geoId = paste0(statefips, countyfips, tractfips)
  ) %>% 
  relocate(geoId, .after = tractfips) |> 
  select(-c(1:3))

write_csv(eji, "CLEANED/eji.csv")


## FEMA NRI -----

NRI_Table_CensusTracts <- read_csv("NRI/NRI_Table_CensusTracts2020/NRI_Table_CensusTracts.csv")
unique(NRI_Table_CensusTracts$STATEFIPS) |> 
  sort()

# data dictionary
NRIDataDictionary <- read_csv("NRI/NRI_Table_CensusTracts2020/NRIDataDictionary.csv")

fema_nri <- NRI_Table_CensusTracts |> 
  mutate(
    statefips = substr(TRACTFIPS, 1, 2),
    countyfips = substr(TRACTFIPS, 3, 5),
    tractfips = substr(TRACTFIPS, 6, 11)) |> 
  rename(geoId = TRACTFIPS) |> 
  select(geoId,
         statefips, countyfips, tractfips, 
         CWAV_AFREQ, # cold wave
         HWAV_AFREQ, # heat wave
         DRGT_AFREQ, # drought
         ERQK_AFREQ, # earthquake
         HAIL_AFREQ, # hail
         # HRCN_AFREQ, # Hurricane, # 16965 missing
         LNDS_AFREQ, # landslide
         LTNG_AFREQ, # lightning
         RFLD_AFREQ, # riverine flooding
         SWND_AFREQ, # strong wind
         TRND_AFREQ, # tornado
         # TSUN_AFREQ, # Tsunami, # 65072 missing
         # VLCN_AFREQ, # Volcanic Activity, # 70007 missing
         WFIR_AFREQ, # Wildfire,
         WNTW_AFREQ)  # Winter Weather  

str(fema_nri)

saveRDS(fema_nri, "NRI/NRI_Table_CensusTracts2020/fema_nri.rds")


## EnviroAtlas -----

# ambient air toxics concentration
# original data were obtained from https://enviroatlas.epa.gov/enviroatlas/interactivemap/
# only 2000 tract entries, some forest and water body areas do not have data values
air_enviroatlas <- read_csv("EnviroAtlas/air toxics - Ambient concentration (μg_m3).csv")
air_enviroatlas_cleaned <- air_enviroatlas |> 
  select(GEOID10, starts_with("AC"))|> 
  mutate(
    statefips = substr(GEOID10, 1,2),
    countyfips = substr(GEOID10, 3,5),
    tractfips = substr(GEOID10, 6,11)
  ) |> 
  rename(geoId = GEOID10)
names(air_enviroatlas_cleaned)
write_csv(air_enviroatlas_cleaned, "CLEANED/air_enviroatlas_cleaned.csv")

# tract residential address vacancy rate for 2014
# original data were obtained from: https://enviroatlas.epa.gov/enviroatlas/interactivemap/
res_vacancy <- read_csv("EnviroAtlas/CONUS_metrics_Sep2021_CSV/Residential_Vacancy_Rate_Tract10.csv")
res_vacancy_cleaned <- res_vacancy |> 
  select(TRFIPS, Res_2014_r) |> 
  rename(geoId = TRFIPS) |> 
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5),
    tractfips = substr(geoId, 6,11)
  ) |> 
  mutate(
    Res_2014_r = case_when(
      Res_2014_r %in% c("None", "-9999.0") ~ NA_character_,
      TRUE ~ Res_2014_r
    ),
    res_vacancy_rate = as.numeric(Res_2014_r)
  ) 

# save cleaned up residential vacancy data
write_csv(res_vacancy_cleaned, "EnviroAtlas/res_vacancy.csv")



## CACES -------------------------------------------------------------------
# Original data were obtained from https://www.caces.us/data

# census tract yearly air pollution levels for 2015-2019
air_caces <- read_csv("CACES/uwc17111251957599d63adf9cb01c7609c4b307b5b97790e.csv")

# unique air pollutants
unique(air_caces$pollutant) # "no2", "o3", "pm10", "pm25", "so2", "co", "pnc"

# years for which data is available for each pollutants
air_caces |> filter(pollutant == "no2") |> pull(year) |> unique() # 2015-2019
air_caces |> filter(pollutant == "o3") |> pull(year) |> unique() # 2015-2019
air_caces |> filter(pollutant == "pm10") |> pull(year) |> unique() # 2015-2019
air_caces |> filter(pollutant == "pm25") |> pull(year) |> unique() # 2015-2019
air_caces |> filter(pollutant == "so2") |> pull(year) |> unique() # 2015-2019
air_caces |> filter(pollutant == "co") |> pull(year) |> unique() # 2015-2019
air_caces |> filter(pollutant == "pnc") |> pull(year) |> unique() # 2017

air_caces_wide <- air_caces |> 
  mutate(geoId = str_pad(fips, 11, pad = "0")) |> 
  group_by(geoId, pollutant) |> 
  # create yearly mean values
  summarise(value = mean(pred_wght)) |> 
  select(geoId, pollutant, value) |> 
  pivot_wider(names_from = pollutant, values_from = value) |> 
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5),
    tractfips = substr(geoId, 6,11)
  ) 

# save cleaned up air pollution data from CACES
write_csv(air_caces_wide, "CLEANED/air_caces_cleaned.csv")


##  NCI walkability -----

WalkabilityIndex_Tract_2019 <- read.csv("NCI/walkability/WalkabilityIndex_Tract_2019.csv")

walkindex <- WalkabilityIndex_Tract_2019 |> 
  select(TractID2010, NatWalkInd) |> 
  mutate(
    geoId = str_pad(TractID2010, 11, pad = "0"),
    statefips = substr(geoId, 1, 2),
    countyfips = substr(geoId, 3, 5),
    tractfips = substr(geoId, 6, 11)
  )

write_csv(walkindex, "CLEANED/nci_walkindex.csv")


## food access -----
# Original data were obtained from USDA: https://www.ers.usda.gov/data-products/food-access-research-atlas
foodaccess <- read_excel("food access/FoodAccessResearchAtlasData2019.xlsx", 
                         sheet = "Food Access Research Atlas")

# lapop10share and lapop20share have too many missing values, not used
food_access_cleaned <- foodaccess |> 
  select(CensusTract, LATracts_half, LATracts1, lapophalfshare, lapop1share) |> 
  mutate(
    statefips = substr(CensusTract, 1,2),
    countyfips = substr(CensusTract, 3,5),
    tractfips = substr(CensusTract, 6,11),
    
    # recode NULL with NA
    lapophalfshare = case_when(
      lapophalfshare == "NULL" ~ NA_character_,
      TRUE ~ lapophalfshare
    ),
    lapop1share = case_when(
      lapop1share == "NULL" ~ NA_character_,
      TRUE ~ lapop1share
    ),
    lapophalfshare = as.numeric(lapophalfshare),
    lapop1share = as.numeric(lapop1share)
  ) |> 
  rename(geoId = CensusTract)

write_csv(food_access_cleaned, "CLEANED/food_access_cleaned.csv")


## NaNDA ----- 

#Census tract data on parks, street connectivity, and broadband internet were
#obtained from the National Neighborhood Data Archive (NaNDA). Original data
#were obtained from https://www.icpsr.umich.edu/web/ICPSR/series/1920/studies

### parks ------
parks_nanda <- read_dta("NaNDA/parks by tract/117921-V1/nanda_parks_tract_2018_01P.dta")

parks_nanda_cleaned <- parks_nanda |> 
  select(tract_fips10, count_open_parks, prop_park_area_tract) |> 
  mutate(
    statefips = substr(tract_fips10, 1,2),
    countyfips = substr(tract_fips10, 3,5),
    tractfips = substr(tract_fips10, 6,11)
  ) |> 
  rename(geoId = tract_fips10)

write_csv(parks_nanda_cleaned, "CLEANED/parks_nanda_cleaned.csv")

### street connectivity -----
stconnct_tract <- read_dta("NaNDA/street connectivity/110641-V1/nanda_stconnct_tract_2010_02P.dta")

stconnct_tract1 <- stconnct_tract |> 
  select(tract_fips10, strNetDensity, conNodeRatio, gamma, alpha) |> 
  mutate(
    statefips = substr(tract_fips10, 1,2),
    countyfips = substr(tract_fips10, 3,5),
    tractfips = substr(tract_fips10, 6,11)
  ) |> 
  rename(geoId = tract_fips10)

write_csv(stconnct_tract1, "CLEANED/stconnct_nanda_cleaned.csv")

### broadband internet-----
load("NaNDA/Broadband Internet/DS0001/38567-0001-Data.rda")

broadband_tract <- da38567.0001 |> 
  select(TRACT_FIPS10, YEAR, AVG_DOWNLOAD_SPEED, AVG_UPLOAD_SPEED) |> 
  filter(YEAR %in% c(2015:2019)) |> 
  group_by(TRACT_FIPS10) |> 
  summarise(
    avg_download_speed_1519 = mean(AVG_DOWNLOAD_SPEED),
    avg_upload_speed_1519 = mean(AVG_UPLOAD_SPEED)
  ) |> 
  # mean download and upload speeds for years 2015 to 2019 were calculated
  mutate(
    statefips = substr(TRACT_FIPS10, 1,2),
    countyfips = substr(TRACT_FIPS10, 3,5),
    tractfips = substr(TRACT_FIPS10, 6,11)
  ) |>   
  rename(geoId = TRACT_FIPS10)

write_csv(broadband_tract, "CLEANED/broadband_nanda_cleaned.csv")



## climate and economic justic screening tool -----

# Original data were obtained from https://screeningtool.geoplatform.gov/en/downloads#3/33.47/-97.5   

communities <- read_csv("Climate and Economic Justice Screening Tool/communities.csv")
community_codebook <- read_csv("Climate and Economic Justice Screening Tool/1/1.0-codebook.csv")

## Two tracts have energy burden values greater than 100, with the greatest value being 1322
communities_cleaned <- communities |> 
  select(1, starts_with("Housing"), contains("plumbing"), 
         contains("Energy"), contains("risk index"),
         "Share of properties at risk of flood in 30 years", 
         "Share of properties at risk of fire in 30 years") |> 
  select(-contains("percentile")) |> 
  rename(
    geoId=1,
    pct_housing_burden =2,
    pct_no_plumbing=3,
    energy_burden=4,
    agriculture_loss_rate=5,
    building_loss_rate=6,
    pop_loss_rate=7,
    pct_prop_flood=8,
    pct_prop_fire=9
  ) |> 
  mutate(
    geoId = str_pad(geoId, 11, pad = "0"),
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5),
    tractfips = substr(geoId, 6,11)
  )

write_csv(communities_cleaned, "CLEANED/community_cleaned.csv")


## tree canopy -----

# census tract percent tree canopy data for year 2016 were obtained from USGS
# National Land Cover Database (NLCD)

pct_tree2016_tract2010 <- read_dta("tree canopy/NLCD tree canopy/pct_tree2016_tract2010.dta")

pct_tree2016 <- pct_tree2016_tract2010 |> 
  mutate(
    geoId = str_pad(geoid10, 11, pad = "0"),
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5),
    tractfips = substr(geoId, 6,11)
  ) |> 
  rename(pct_tree = mean) |> 
  select(-count, -geoid10)

write_csv(pct_tree2016, "CLEANED/pct_tree2016.csv")


## flood plain -----

# Original data were obtained from
# https://www.arcgis.com/home/webmap/viewer.html?url=https%3A%2F%2Fenviroatlas.epa.gov%2Farcgis%2Frest%2Fservices%2FSupplemental%2FEstimated_floodplain_CONUS_WM%2FImageServer&source=sd
# pct of pixels within tracts that are estimated 100-year floodplains for 2016

flood_plain <- read.csv("EPA/flood plain/Estimated_floodplain_CONUS/tract zonal statistics.csv")
names(flood_plain)

flood_plain_cleaned <- flood_plain |> 
  mutate(
    geoId = str_pad(GEOID10, 11, pad = "0"),
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5),
    tractfips = substr(geoId, 6,11),
    
    # pct cells in flood plain
    pct_flood_plain = X_sum/X_count
  ) |> 
  select(geoId, statefips, countyfips, tractfips, pct_flood_plain)

write_csv(flood_plain_cleaned, "CLEANED/flood_plain_cleaned.csv")


## Drought -----------------------------------------------------------------

# Original data were obtained from https://droughtmonitor.unl.edu 

setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Data/Drought/U.S. Drought monitor/non consecutive weeks")
df_list <- list.files(pattern="\\.csv$", recursive = TRUE)
droughtfiles <- lapply(df_list, read_csv)

non_consecutive_df <- droughtfiles |>
  reduce(left_join, by = "FIPS") |>
  select(FIPS, starts_with("NonConsecutiveWeeks")) |>
  select(1:6) |>
  # 260 weeks and 5 days btw 1/1/2015 and 12/31/2019
  mutate(
    pct_NonConsecutiveWeeks_d0 = NonConsecutiveWeeks_d0 / 260,
    pct_NonConsecutiveWeeks_d1 = NonConsecutiveWeeks_d1 / 260,
    pct_NonConsecutiveWeeks_d2 = NonConsecutiveWeeks_d2 / 260,
    pct_NonConsecutiveWeeks_d3 = NonConsecutiveWeeks_d3 / 260,
    pct_NonConsecutiveWeeks_d4 = NonConsecutiveWeeks_d4 / 260,

    statefips = substr(FIPS,1,2),
    countyfips = substr(FIPS, 3,5)
  )
write_csv(non_consecutive_df[, c(7:13)], "non_consecutive_drought.csv")


## light pollution ---------------------------------------------------------

# Original SIIRS raster data were obtained from https://eogdata.mines.edu/products/vnl/
# census tract zonal averages were calculated using ArcGIS based on 2010 census tract boundaries

# average values by tract, then averaged across 2015 and 2019
setwd("~/mcw.edu/Structural Racism Geometrics - Innovation and New Directions/EnvironmentalRacismMetrics/Data/CLEANED")
tract_zonal_2019 <- read_csv("Light pollution/VIIRS/V21 annual average masked/tract_zonal_2019.csv")[, c(4, 16)] |> 
  rename(light_2019mean = 2)
tract_zonal_2018 <- read_csv("Light pollution/VIIRS/V21 annual average masked/tract_zonal_2018.csv")[, c(4, 16)] |> 
  rename(light_2018mean = 2)
tract_zonal_2017 <- read_csv("Light pollution/VIIRS/V21 annual average masked/tract_zonal_2017.csv")[, c(4, 16)] |> 
  rename(light_2017mean = 2)
tract_zonal_2016 <- read_csv("Light pollution/VIIRS/V21 annual average masked/tract_zonal_2016.csv")[, c(4, 16)] |> 
  rename(light_2016mean = 2)
tract_zonal_2015 <- read_csv("Light pollution/VIIRS/V21 annual average masked/tract_zonal_2015.csv")[, c(1:4, 16)] |> 
  rename(light_2015mean = 5)

# create yearly average for 2015 to 2019
VIIRS_tracts <- list(tract_zonal_2015, tract_zonal_2016, tract_zonal_2017,
                           tract_zonal_2018, tract_zonal_2019) |> 
  reduce(left_join, by = "GEOID10") |> 
  mutate(light_15_19 = (light_2015mean+light_2016mean+light_2017mean+light_2018mean+light_2019mean) / 5) |> 
  rename(
    statefips = STATEFP10,
    countyfips = COUNTYFP10,
    tractfips = TRACTCE10,
    geoId = GEOID10
  )

write_csv(VIIRS_tracts[,c(1:4, 10)], "Light pollution/VIIRS/VIIRS_tracts.csv")



# Creating master file -------

setwd("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Data/CLEANED")

# load cleaned up tract environmental data
cdceph_merged <- readRDS("cdceph_merged.rds") # CDCEPH data is cleaned using a separate R script.
noise_nps <- read_csv("noise_nps.csv")
air_caces_cleaned <- read_csv("air_caces_cleaned.csv")
VIIRS_tracts <- read_csv("VIIRS_tracts.csv")
pct_tree2016 <- read_csv("pct_tree2016.csv")
community_cleaned <- read_csv("community_cleaned.csv")
eji <- read_csv("eji.csv")
broadband_nanda_cleaned <- read_csv("broadband_nanda_cleaned.csv")
stconnct_nanda_cleaned <- read_csv("stconnct_nanda_cleaned.csv")
housing_cleaned <- read_csv("housing_acs15_19.csv")
res_vacancy <- read_csv("res_vacancy.csv")[-2]
parks_nanda_cleaned <- read_csv("parks_nanda_cleaned.csv")
flood_plain_cleaned <- read_csv("flood_plain_cleaned.csv")
food_access_cleaned <- read_csv("food_access_cleaned.csv")
nci_walkindex <- read_csv("nci_walkindex.csv")[-1]
ejscreen2016 <- read_csv("ejscreen2016.csv")[-1]
fema_nri <- readRDS("fema_nri.rds")

# combine data sets
dfs_list <- list(noise_nps, air_caces_cleaned, VIIRS_tracts, 
                 pct_tree2016, community_cleaned, eji, 
                 cdceph_merged, res_vacancy, ejscreen2016,
                 broadband_nanda_cleaned, flood_plain_cleaned,
                 stconnct_nanda_cleaned, housing_cleaned, 
                 parks_nanda_cleaned, food_access_cleaned,
                 nci_walkindex, fema_nri) 

master_df <- Reduce(function(x, y) 
  merge(x, y, by = c("geoId", "statefips", "countyfips", "tractfips"), all=TRUE), dfs_list)
names(master_df)

# remove non-contiguous US areas
master_df_conus <- master_df |> 
  filter(!statefips %in% c("02","15","60","66","69","72","78")) |> 
  rename(GEOID10 = geoId) |> 
  mutate(GEOID10 = as.character(GEOID10),
         DSLPM = case_when(
           DSLPM == "None" ~ "0",
           .default = DSLPM
         )) 
class(master_df_conus$DSLPM)
master_df_conus$DSLPM <- as.numeric(master_df_conus$DSLPM)

# save master tract environmental file
write.csv(master_df_conus, "master_df_conus.csv")




# missingness -------------------------------------------------------------

# read environmental master file
master_df_conus <- read_csv("master_df_conus.csv")[-1]

# read tract shapefile for contiguous US
us_tracts2010 <- read_sf("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Shapefiles/Census tract/us_tract_contiguous_2010.shp") |> 
  relocate(GISJOIN, .after = GEOID10)

master_df_out <- master_df_conus |> 
  filter(!GEOID10 %in% us_tracts2010$GEOID10)
# n=292 tracts not in the 2010 census tract shapefile

master_df_shp <- master_df_conus |> 
  filter(GEOID10 %in% us_tracts2010$GEOID10) |> 
  mutate(
    GISJOIN = paste0("G", statefips, "0", countyfips, "0", tractfips)
  )


## inspect missing values ----- 

# https://stackoverflow.com/questions/62915873/how-to-fill-missing-values-based-on-neighboring-polygons-in-r
# https://search.r-project.org/CRAN/refmans/multiUS/html/KNNimp.html

p_missing <- unlist(lapply(master_df_shp, function(x) sum(is.na(x))))
sort(p_missing[p_missing > 0], decreasing = TRUE)

p_missing <- unlist(lapply(master_df_shp, function(x) sum(is.na(x))))/nrow(master_df_shp)
sort(p_missing[p_missing > 0], decreasing = TRUE)

# inspect missingness
colSums(is.na(master_df_shp))

# save environmental data master file, linked to tract shapefile GEOID
write.csv(master_df_shp, "~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Data/master_df_final.csv")



