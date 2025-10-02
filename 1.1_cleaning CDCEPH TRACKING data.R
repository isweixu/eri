# This file contains code used to clean up the tract environmental data obtained
# from the CDCEPH.

# https://github.com/CDCgov/EPHTrackR 

devtools::install_github("CDCgov/EPHTrackR", dependencies = TRUE)

library(EPHTrackR)
library(dplyr)
library(tidyr)
library(tidyverse)

# Data by Population-Based Geographies
# https://www.cdc.gov/nceh/tracking/topics/geographies.htm 

# Some data sets have too few cases to show on a map at finer resolution
# geo-political geographies. This could be because there are only a few cases
# reported for that area, or that it is a rare health condition (such as
# childhood cancer). If there is a small number of case counts, data is
# suppressed or hidden at the geo-political boundary level, such as county
# level, to protect the privacy of individuals affected by these conditions.
# This is especially true in counties and census tracts with low populations. To
# visualize these data sets in a meaningful way, the Tracking Network has
# developed population-based geographies. This can be done by combining census
# tracts or counties to meet a minimum population threshold to display the data.
# Population-based geographies are useful to see trends based on populations for
# many environmental, community, and health data, especially in areas where
# there are high or low populations in counties.

devtools::install_github("CDCgov/EPHTrackR", dependencies = TRUE)

# setting up environment
tracking_api_token("690A4977-5001-4937-8A37-5A86F5D8D117", install=T)

# reload your environment so you can use the token without restarting R.
readRenviron("~/.Renviron")

# Token can be viewed by running
Sys.getenv("TRACKING_API_TOKEN")

# setting working directory
setwd("~/mcw.edu/Structural Racism Geometrics - Innovation and New Directions/EnvironmentalRacismMetrics/Data/CDC NEPHTN")

# list of variables available
measures_inventory <- list_measures()
View(measures_inventory)
saveRDS(measures_inventory, "measures_inventory.rds")

# get a list of state abbr
# states <- state.abb[-which(state.abb %in% c('HI','AK') )]
# states1 <- states[1:25]
measureid = 883
temporalItems = 2019
strats = "State x Census Tract"

# create functions for extracting data from CDCEPH tracking system
data_extract <- function(measureid, strats, years){
  df1 <- get_data(measure=measureid, 
                  strat_level = strats,
                  geoItems = c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN","IA","KS"),
                  temporalItems = years)[[1]]
  df2 <- get_data(measure=measureid, 
                  strat_level = strats,
                  geoItems = c("KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV"),
                  temporalItems = years)[[1]]
  df3 <- get_data(measure=measureid, 
                  strat_level = strats,
                  geoItems = c("NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC"),
                  temporalItems = years)[[1]]
  df4 <- get_data(measure=measureid, 
                  strat_level = strats,
                  geoItems = c("SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
                  temporalItems = years)[[1]]
  
  df <- bind_rows(df1, df2, df3, df4) 
  return(df)
}


# Park access ------

# all residents
list_StratificationLevels(measure=1285)[[1]]
list_GeographicTypes(measure= 1285)[[1]]
list_TemporalItems(measure= 1285)[[1]] # 2010, 2015, 2020
list_GeographicItems(measure= 1285)[[1]]

# AK data not available
parks_tract <- get_data(measure=1285, 
                       strat_level = "State x Census Tract x Distance",
                       geoItems = c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN","IA","KS"),
                       temporalItems = 2015)[[1]]
parks_tract1 <- get_data(measure=1285, 
                        strat_level = "State x Census Tract x Distance",
                        geoItems = c("KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV"),
                        temporalItems = 2015)[[1]]
parks_tract2 <- get_data(measure=1285, 
                         strat_level = "State x Census Tract x Distance",
                         geoItems = c("NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC"),
                         temporalItems = 2015)[[1]]
parks_tract3 <- get_data(measure=1285, 
                         strat_level = "State x Census Tract x Distance",
                         geoItems = c("SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
                         temporalItems = 2015)[[1]]

# combine data
parks_tracts <- bind_rows(parks_tract, parks_tract1, parks_tract2, parks_tract3)  |>  
  select(title, geoId, Geo_Type, parentGeoId, parentGeoAbbreviation, 
         date, dataValue, "Distance to Parks", measureName, ) |> 
  mutate(dataValue = as.numeric(dataValue)) |> 
  select(geoId, Geo_Type, date, dataValue, 'Distance to Parks') |> 
  pivot_wider(names_from = 'Distance to Parks',
              values_from = dataValue) 
names(parks_tracts)[4:5] <- c("parks_0.5mile", "parks_1mile")

parks_tracts1 <- parks_tracts|> 
  group_by(geoId, Geo_Type) |> 
  summarise(
    parks_0.5mile = mean(parks_0.5mile),
    parks_1mile = mean(parks_1mile)
  ) |> 
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5),
    tractfips = substr(geoId, 6, 11)
  )
saveRDS(parks_tracts1, "park access/dist_to_parks_all.rds")


# NDVI ------
list_StratificationLevels(measure=1197)[[1]]
list_GeographicTypes(measure= 1197)[[1]]
list_TemporalItems(measure= 1197)[[1]]
list_GeographicItems(measure= 1197)[[1]]

ndvi_tract <- data_extract(1197, "State x Census Tract", c(2015:2019))
names(ndvi_tract)
ndvi_tract1 <- ndvi_tract %>% 
  select(geo, geoId, Geo_Type, parentGeoAbbreviation, date, dataValue, measureName)
saveRDS(ndvi_tract1, "NDVI/ndvi_tracts.rds")

# 2015-2019 average
ndvi_tracts_mean <- ndvi_tracts |> 
  mutate(dataValue = as.numeric(dataValue)) |> 
  group_by(geoId, Geo_Type) |> 
  summarise(
    ndvi_avg = mean(dataValue)
  ) |> 
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5),
    tractfips = substr(geoId, 6,11)
  )
saveRDS(ndvi_tracts_mean, "NDVI/ndvi_tracts_mean.rds")

# Precipitation -----------------------------------------------------------

# absolute thresholds
abs_precip_tract <- data_extract(576, "State x Census Tractx Absolute Threshold", c(2015:2019))
# names(abs_precip_tract)
abs_precip_tract1 <- abs_precip_tract %>% 
  select(geo, geoId, Geo_Type, parentGeoAbbreviation, date, 
         'Absolute Threshold', dataValue, measureName)

# calculate mean annual days of precipitation based on absolute thresholds (n=4) across 2015-2019
abs_precip_tracts_wide <- abs_precip_tract1 |> 
  rename(abs_threshold = 6) |> 
  mutate(dataValue = as.numeric(dataValue)) |> 
  group_by(geo, geoId, Geo_Type, abs_threshold) |> 
  summarise(days = mean(dataValue)) |> # 2015-2019 average
  pivot_wider(names_from = abs_threshold,
              values_from = days) |> 
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5),
    tractfips = substr(geoId, 6,11)
  )
names(abs_precip_tracts_wide)[4:7] <- c("abs_precip_0.01",
                                        "abs_precip_1",
                                        "abs_precip_2",
                                        "abs_precip_3")
saveRDS(abs_precip_tracts_wide, "precipitation/abs_precip_tracts_wide.rds")

# relative thresholds
rel_precip_tract <- data_extract(576, "State x Census Tract x Relative Threshold", c(2015:2019))

# calculate mean annual days of precipitation based on relative thresholds (n=4) across 2015-2019
rel_precip_tracts_wide <- rel_precip_tract |> 
  select(geo, geoId, Geo_Type, parentGeoAbbreviation, date, 
         'Relative Threshold', dataValue, measureName) |> 
  rename(rel_threshold = 6) |> 
  mutate(dataValue = as.numeric(dataValue)) |> 
  group_by(geoId, Geo_Type, rel_threshold) |> 
  summarise(days = mean(dataValue)) |> 
  pivot_wider(names_from = rel_threshold,
              values_from = days) |> 
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5),
    tractfips = substr(geoId, 6,11)
  )
names(rel_precip_tracts_wide)[3:6] <- c("rel_precip_90", 
                                          "rel_precip_95",
                                          "rel_precip_98",
                                          "rel_precip_99")
saveRDS(rel_precip_tracts_wide, "precipitation/rel_precip_tracts_wide.rds")

precip_tracts <- merge(abs_precip_tracts_wide, rel_precip_tracts_wide)[, -c(2, 6)]
saveRDS(precip_tracts, "precipitation/precip_tracts_merged.rds")

# Imperviousness -------------------------------------------------------------------

list_StratificationLevels(measure=720)[[1]]
list_GeographicTypes(measure= 720)[[1]]
list_TemporalItems(measure= 720)[[1]] # 2011, 2016, 2021
list_GeographicItems(measure= 720)[[1]]

# extract 2016 data
impervious_tract <- data_extract(720, "State x Census Tract", 2016) |> 
  select(geo, geoId, Geo_Type, parentGeoAbbreviation, date, dataValue, measureName) |> 
  distinct() |> 
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5),
    tractfips = substr(geoId, 6,11),
    pct_imperviousness = as.numeric(dataValue)
  )
saveRDS(impervious_tract[, c(2, 8:11)], "imperviousness/impervious_tract.rds")


# Sunlight & Ultraviolet --------------------------------------------------

list_GeographicTypes(measure= 721)[[1]]
list_TemporalItems(measure= 721)[[1]]

# Annual Average Sunlight Exposure Measured by Solar Irradiance (kJ/m2) averaged across 2015-2019
sunlight_county <- data_extract(721, "State x County", c(2015:2019))
sunlight_county1 <- sunlight_county %>% 
  select(geoId, Geo_Type, parentGeoAbbreviation, date, dataValue) %>% 
  rename(solar_irradiance = dataValue) |> 
  mutate(solar_irradiance = as.numeric(solar_irradiance)) |> 
  group_by(geoId) |> 
  summarise(solar_irradiance = mean(solar_irradiance))
saveRDS(sunlight_county1, "sunlight_uv/sunlight_county15_19.rds")

# Annual Average Daily Dose of UV Irradiance (J/m2)
uv_county <- data_extract(723, "State x County", c(2015:2019))
uv_county1 <- uv_county %>% 
  select(geoId, Geo_Type, parentGeoAbbreviation, date, dataValue) %>% 
  rename(uv_irradiance = dataValue)|> 
  mutate(uv_irradiance = as.numeric(uv_irradiance)) |> 
  group_by(geoId) |> 
  summarise(uv_irradiance = mean(uv_irradiance))
saveRDS(uv_county1, "sunlight_uv/uv_county15_19.rds")

# Annual Average UV Irradiance at Noon (mW/m2)
uvnoon_county <- data_extract(725, "State x County", c(2015:2019))
uvnoon_county1 <- uvnoon_county %>%
  select(geoId, Geo_Type, parentGeoAbbreviation, date, dataValue) %>%
  rename(uvnoon_irradiance = dataValue)|>
  mutate(uvnoon_irradiance = as.numeric(uvnoon_irradiance)) |> 
  group_by(geoId) |> 
  summarise(uvnoon_irradiance = mean(uvnoon_irradiance))
saveRDS(uvnoon_county1, "sunlight_uv/uvnoon_county15_19.rds")

sunlight_uv <- list(sunlight_county1, uv_county1, uvnoon_county1) |> 
  reduce(full_join) |> 
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5)
  )
saveRDS(sunlight_uv[, -1], "sunlight_uv/sunlight_uv_merged.rds")


# Historical Tornado Occurrences ------------------------------------------

# note: ef0_15_19 - ef5_15_19 variables are for 2015 to 2019, but the tracts are based on 2020 geographies

# Average Annual Number of Tornados by intensity group (2015-2019)
stratas <- list_StratificationLevels(measure=1294)[[1]]
temp <- list_TemporalItems(measure= 1294)

tornado_ef <- data_extract(1294, "State x Census Tract x Intensity Category", c(2015:2019))
tornado_ef_15_19_tract <- tornado_ef |> 
  select(geoId, Geo_Type, date, "Individual Tornado Intensity Categories (EF-scale)", dataValue) |> 
  rename(intensity_cat=4, number=5) |> 
  mutate(number = as.numeric(number)) |> 
  group_by(geoId, Geo_Type, intensity_cat) |> 
  summarise(no2015_19 = mean(number)) |> 
  pivot_wider(names_from = intensity_cat, 
              values_from = no2015_19) |> 
  mutate(
    statefips = substr(geoId, 1, 2),
    countyfips = substr(geoId, 3, 5),
    tractfips = substr(geoId, 6, 11)
  )
names(tornado_ef_15_19_tract)[3:8] <- c("ef0_15_19", "ef1_15_19", "ef2_15_19", 
                                        "ef3_15_19", "ef4_15_19", "ef5_15_19")
saveRDS(tornado_ef_15_19_tract, "tornado/tornado_ef_15_19_tract.rds")  

# Average Annual Number of Tornados by intensity group (1986-present/2021)
tornado_efgroup_tract <- data_extract(1295, "State x Census Tract x Intensity Category Groups", c(2021))
tornado_efgroup_86_21_tract <- tornado_efgroup_tract |>  
  select(geoId, Geo_Type, full_stratification, dataValue) |> 
  pivot_wider(names_from = full_stratification, values_from = dataValue) |> 
  mutate(
    statefips = substr(geoId, 1, 2),
    countyfips = substr(geoId, 3, 5),
    tractfips = substr(geoId, 6, 11)
  )
names(tornado_efgroup_86_21_tract)[3:7] <- c("ef0_5", "ef1_5", "ef2_5", "ef3_5", "ef4_5")
saveRDS(tornado_efgroup_86_21_tract, "tornado/tornado_efgroup_86_21_tract.rds")

# Avg. Tornadoes per 1000 km2, 1986-2021
# same tracts have multiple values, cannot differentiate, not using
# tornado_alt_tract <- data_extract(1295, "State x Census Tract x Alternative Metrics", c(2021))
# tornado_alt_tract1 <- tornado_alt_tract |> 
#   select(geoId, Geo_Type, date, dataValue) |> 
#   distinct() |> 
#   rename(avg_tornado_km = 3)|> 
#   mutate(
#     statefips = substr(geoId, 1, 2),
#     countyfips = substr(geoId, 3, 5),
#     tractfips = substr(geoId, 6, 11)
#   )
# saveRDS(tornado_alt_tract1, "tornado/tornado_alt_tract.rds")

# Total Tornado-related Injuries & Fatalities (1986-present)
list_StratificationLevels(measure=1293)[[1]]
tornado_injury <- data_extract(1293, "State x County x Casualty Type", c(2021))
tornado_injury_county <- tornado_injury |> 
  select(geoId, Geo_Type, `Casualty Type`, dataValue) |> 
  pivot_wider(names_from = `Casualty Type`, values_from = dataValue) |> 
  rename(tornado_fatality = 3, tornado_injury = 4) |> 
  mutate(
    statefips = substr(geoId, 1, 2),
    countyfips = substr(geoId, 3, 5)
  )
saveRDS(tornado_injury_county, "tornado/tornado_injury_county.rds")

tornado_merged <- list(tornado_ef_15_19_tract, tornado_efgroup_86_21_tract) |> 
  reduce(full_join, by=c("statefips", "countyfips", "tractfips")) |> 
  select(statefips, countyfips, tractfips, 3:8, 14:18)

tornado_merged1 <- merge(tornado_merged, tornado_injury_county[,c("statefips", "countyfips", 
                                                            "tornado_fatality","tornado_injury")])
saveRDS(tornado_merged1, "tornado/tornado_merged.rds")

# Wildland fires ----------------------------------------------------------

# Areas and Populations Vulnerable to Predicted Surface Smoke from Wildland Fires
# real-time statistics 

list_StratificationLevels(measure=731)[[1]]
list_TemporalItems(measure= 731)[[1]]

wildfire_tract <- data_extract(731, "State x Census Tract x Alternative Metrics", c(2021))
wildfire_tract1 <- wildfire_tract |> 
  select(geo, geoId, Geo_Type, parentGeoAbbreviation, date, dataValue, measureName) %>% 
  distinct()

 
# Internet access -----
# data were based on the 2015-2019 ACS data and obtained from NHGIS
internet_tract <- read_csv("~/Library/CloudStorage/OneDrive-SharedLibraries-mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Data/CDC NEPHTN/internet access/nhgis0062_ds244_20195_tract.csv")
names(internet_tract)

# calculate pct households without internet access
internet_tract_cleaned <- internet_tract |> 
  select(STATEA, COUNTYA, TRACTA, AL1YE001, AL1YE013) |> 
  rename(
    statefips = STATEA,
    countyfips = COUNTYA,
    tractfips = TRACTA
  ) |> 
  mutate(
    perc_no_internet = AL1YE013 / AL1YE001,
    geoId = paste0(statefips, countyfips, tractfips)
  )

saveRDS(internet_tract_cleaned, "~/mcw.edu/Structural Racism Geometrics - Innovation and New Directions/EnvironmentalRacismMetrics/Data/CDC NEPHTN/internet access/internet_tract_cleaned.rds")

# Drought ----

# CDCEPH tracking system does not have drought or precipitation data at tract level

# Percent of Weeks a County Was in Drought by Year (SPEI)
list_StratificationLevels(measure=1141)[[1]]
list_TemporalItems(measure= 1141)[[1]]

drought1 <- data_extract(1141, "State x County x Cumulative Severity", c(2015:2019))
drought1_1 <- drought1 |>
  select(geoId, full_stratification, date, dataValue)  |>
  mutate(
    dataValue = as.numeric(dataValue)
  ) |>
  group_by(geoId, full_stratification) |>
  summarise(
    pct_weeks = mean(dataValue) # average across 2015-2019
  ) |>
  pivot_wider(names_from = full_stratification,
              values_from = pct_weeks) |>
  rename(
    pct_drought_mod_spei_15_19 = 3,
    pct_drought_ext_spei_15_19 = 2,
    pct_drought_sev_spei_15_19 = 4
  )|>
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5)
  )

# Percent of Weeks a County Was in Drought by Year (USDM)
list_StratificationLevels(measure=1147)[[1]]
list_TemporalItems(measure= 1147)[[1]]

drought2 <- data_extract(1147, "State x County x Cumulative Severity", c(2015:2019))
drought2_1 <- drought2 |>
  select(geoId, full_stratification, date, dataValue)  |>
  mutate(
    dataValue = as.numeric(dataValue)
  ) |>
  group_by(geoId, full_stratification) |>
  summarise(
    pct_weeks = mean(dataValue) # average across 2015-2019
  ) |>
  pivot_wider(names_from = full_stratification,
              values_from = pct_weeks) |>
  rename(
    pct_drought_mod_usdm_15_19 = 3,
    pct_drought_ext_usdm_15_19 = 2,
    pct_drought_sev_usdm_15_19 = 4
  )|>
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5)
  )

# percent of week by SPEI, 1980-present
list_StratificationLevels(measure=1138)[[1]]
list_TemporalItems(measure= 1138)[[1]]
drought3 <- data_extract(1138, "State x County x Cumulative Severity", 2021)
drought3_1 <- drought3 |>
  select(geoId, full_stratification, dataValue)|>
  mutate(
    dataValue = as.numeric(dataValue)
  ) |>
  pivot_wider(names_from = full_stratification,
              values_from = dataValue) |>
  rename(
    perc_drought_mod_spei_1980over = 2,
    perc_drought_sev_spei_1980over = 3,
    perc_drought_ext_spei_1980over = 4
  )|>
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5)
  )

# percent of week by USDM, 2000-present
drought4 <- data_extract(1144, "State x County x Cumulative Severity", c(2021))
drought4_1 <- drought4 |>
  select(geoId, full_stratification, dataValue)|>
  mutate(
    dataValue = as.numeric(dataValue)
  ) |>
  pivot_wider(names_from = full_stratification,
              values_from = dataValue) |>
  rename(
    perc_drought_mod_usdm_2000over = 2,
    perc_drought_sev_usdm_2000over = 3,
    perc_drought_ext_usdm_2000over = 4
  )|>
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5)
  )

# merge data
drought_cdceph <- list(drought1_1, drought2_1, drought3_1, drought4_1) |> 
  reduce(full_join) |> 
  relocate(statefips, .after = geoId,
           countyfips, .after = statefips)
names(drought_cdceph)

saveRDS(drought_cdceph, "~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Data/CDC NEPHTN/drought/drought_cdceph.rds")

# Air toxins -----

list_StratificationLevels(measure=478)[[1]]
list_TemporalItems(measure= 478)[[1]]

air_toxins <- data_extract(478, "State x County x Pollutant", c(2017, 2018))
saveRDS(air_toxins, "~/mcw.edu/Structural Racism Geometrics - Innovation and New Directions/EnvironmentalRacismMetrics/Data/CDC NEPHTN/air toxins/air_toxins.rds")

air_toxins_cleaned <- air_toxins |> 
  select(geoId, date, Pollutant, dataValue) |> 
  pivot_wider(names_from = Pollutant, values_from = dataValue) |> 
  # Pollutant: 1,3-butadiene, most county do not have 2017 values, keep only 2018 data
  filter(date == 2018)|> 
  mutate(
    statefips = substr(geoId, 1,2),
    countyfips = substr(geoId, 3,5)
  ) |> 
  rename(
    benzene=3, formaldehyde=4, acetaldehyde=5, 
    carbon_tetrachloride=6, butadiene=7
  )

saveRDS(air_toxins_cleaned, "~/mcw.edu/Structural Racism Geometrics - Innovation and New Directions/EnvironmentalRacismMetrics/Data/CDC NEPHTN/air toxins/air_toxins_cleaned.rds")


# Heat -----
# census tract heat data from CDCEPH have too many tracts with missing values 

list_GeographicTypes(measure = 1427)
list_StratificationLevels(measure=99)[[1]]
list_TemporalItems(measure= 1427)[[1]]
list_StratificationTypes(measure = 1427)[[1]][[1]] 

heat_days_abs <- data_extract(1427, "State x Census Tract x Temperature/Heat Index x Absolute Threshold", 2015)

# heat_days_abs <- get_data(measure = 1427,
#                           strat_level = "State x Census Tract x Temperature/Heat Index x Absolute Threshold",
#                           temporalItems = 2015,
#                           geoItems = c(4,8,9,12))
#
# heat_days_abs <- data_extract(1427, "State x Census Tract x Temperature/Heat Index x Absolute Threshold", 2015)
#
# # Absolute Threshold: 90 degrees F
# get_data(measure = 1427, strat_level = "ST_CTC_TH_AT", temporalItems = 2015, stratItems = c("AbsoluteThresholdId=1")) -> heatdays
#

# data_ad <- get_data(measure=99,
#                     strat_level = "ST_AG_GN",
#                     temporalItems = 2005,
#                     stratItems = c("GenderId=1","AgeBandId=3")) ## $ operator is invalid for atomic vectors, package bug


# merge CDCEPH tracking data -----

setwd("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Data/CDC NEPHTN")

# read CDCEPH data at census tract level
impervious_tracts <- readRDS("imperviousness/impervious_tract.rds")
ndvi_tracts <- readRDS("NDVI/ndvi_tracts_mean.rds")
parks_tracts <- readRDS("park access/dist_to_parks_all.rds")
precip_tracts <- readRDS("precipitation/precip_tracts_merged.rds")
internet_tracts <- readRDS("internet access/internet_tract_cleaned.rds")
# tornado data is based on 2020 tract geographies, not used
# tornado_tracts <- readRDS("tornado/tornado_merged.rds")

# read CDCEPH data at county level
drought_county <- readRDS("drought/drought_cdceph.rds")
sunlight_uv_county <- readRDS("sunlight_uv/sunlight_uv_merged.rds")

# merge tract level variables
cdceph_tract <- list(impervious_tracts, ndvi_tracts, parks_tracts, 
               internet_tracts, precip_tracts) |> 
  reduce(full_join, by=c("statefips","countyfips", "tractfips")) |> 
  select(statefips, countyfips, tractfips, 
         starts_with("ef"), starts_with("rel"), starts_with("abs"),
         starts_with("parks"), pct_imperviousness, ndvi_avg, perc_no_internet)

# merge tract values with county sunlight&uv and drought values
cdceph1 <- list(cdceph_tract, sunlight_uv_county, drought_county) |> 
  reduce(full_join) |> 
  mutate(geoId = paste0(statefips, countyfips, tractfips),
         # 2015-2019 average relative precipitation
         rel_precip_90 = case_when(
           statefips == 11 ~ 25,
           TRUE ~ rel_precip_90
         ),
         rel_precip_95 = case_when(
           statefips == 11 ~ 12.2,
           TRUE ~ rel_precip_95
         ),
         rel_precip_98 = case_when(
           statefips == 11 ~ 4.8,
           TRUE ~ rel_precip_98
         ),
         rel_precip_99 = case_when(
           statefips == 11 ~ 2.2,
           TRUE ~ rel_precip_99
         ),
         
         # 2015-2019 average absolute precipitation
         abs_precip_0.01 = case_when(
           statefips == 11 ~ 138.4,
           TRUE ~ abs_precip_0.01
         ),
         abs_precip_1 = case_when(
           statefips == 11 ~ 9,
           TRUE ~ abs_precip_1
         ),
         abs_precip_2 = case_when(
           statefips == 11 ~ 0.8,
           TRUE ~ abs_precip_2
         ),
         abs_precip_3 = case_when(
           statefips == 11 ~ 0.2,
           TRUE ~ abs_precip_3
         ),
         
         solar_irradiance = case_when(
           statefips == 11 ~ 4097.8,
           TRUE ~ solar_irradiance
         ),
         uv_irradiance = case_when(
           statefips == 11 ~ 3336.4,
           TRUE ~ uv_irradiance
         ),
         uvnoon_irradiance = case_when(
           statefips == 11 ~ 165.6,
           TRUE ~ uvnoon_irradiance
         )
  ) |> 
  filter(!statefips %in% c("02", "15", "72"))

saveRDS(cdceph1, "~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Data/CDC NEPHTN/cdceph_merged.rds")
