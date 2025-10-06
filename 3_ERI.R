# This file contains code for creating state and MSA level ERI values. The term ERI and MCC are used exchangably. 

# author: Wei Xu
# organization: Medical College of Wisconsin
# date: August, 2025

library(readr)
library(tidyverse)
library(sf)


# setting working directory
setwd("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics")


# Prepare data files ------

# read tract file with EEI, PMR, and MDI merged
tract_env_race_redline <- read_csv("Exploratory analysis/pca missing values/tract_env_race_redline.csv")

# read state, cbsa, county sf files
load("Shapefiles/st_cbsa_ct_sf.RData")

# load mcc function
source("Exploratory analysis/R codes/functions.R")


# Calculate state MCC ------

states <- unique(tract_env_race_redline$statefips)
states # n=49

state_mcc <- data.frame()
for (i in states) {
  t <- mcc(tract_env_race_redline |> 
             filter(statefips == i),
           "pct_nnhwhite",
           "Redline",
           "environment_s")
  t1 <- cbind(i, t)
  state_mcc <- rbind(state_mcc, t1)
}
names(state_mcc) <- c("GEOID", "mcc", "LL", "UL", "P")

## MCC pollution -----
state_mcc_pollution <- data.frame()
for (i in states) {
  t <- mcc(tract_env_race_redline |> 
             filter(statefips == i),
           "pct_nnhwhite",
           "Redline",
           "pollution_s")
  t1 <- cbind(i, t)
  state_mcc_pollution <- rbind(state_mcc_pollution, t1)
}
names(state_mcc_pollution) <- c("GEOID", "mcc_pollution", "LL", "UL", "P")

## MCC hazard -----
state_mcc_hazard <- data.frame()
for (i in states) {
  t <- mcc(tract_env_race_redline |> 
             filter(statefips == i),
           "pct_nnhwhite",
           "Redline",
           "hazard_s")
  t1 <- cbind(i, t)
  state_mcc_hazard <- rbind(state_mcc_hazard, t1)
}
names(state_mcc_hazard) <- c("GEOID", "mcc_hazard", "LL", "UL", "P")


## MCC built and natural env -----
state_mcc_builtenv <- data.frame()
for (i in states) {
  t <- mcc(tract_env_race_redline |> 
             filter(statefips == i),
           "pct_nnhwhite",
           "Redline",
           "builtenv_s")
  t1 <- cbind(i, t)
  state_mcc_builtenv <- rbind(state_mcc_builtenv, t1)
}
names(state_mcc_builtenv) <- c("GEOID", "mcc_builtenv", "LL", "UL", "P")


## MCC climate -----
state_mcc_climate <- data.frame()
for (i in states) {
  t <- mcc(tract_env_race_redline |> filter(statefips == i),
           "pct_nnhwhite",
           "Redline",
           "climate_s")
  t1 <- cbind(i, t)
  state_mcc_climate <- rbind(state_mcc_climate, t1)
}
names(state_mcc_climate) <- c("GEOID", "mcc_climate", "LL", "UL", "P")


## MCC housing -----
state_mcc_housing <- data.frame()
for (i in states) {
  t <- mcc(tract_env_race_redline |> filter(statefips == i),
           "pct_nnhwhite",
           "Redline",
           "housing_s")
  t1 <- cbind(i, t)
  state_mcc_housing <- rbind(state_mcc_housing, t1)
}
names(state_mcc_housing) <- c("GEOID", "mcc_housing", "LL", "UL", "P")


# merge domain specific state mcc values 
state_mcc_merged <- purrr::reduce(
  list(
    state_mcc_builtenv[, 1:2],
    state_mcc_climate[, 1:2],
    state_mcc_hazard[, 1:2],
    state_mcc_housing[, 1:2],
    state_mcc_pollution[, 1:2],
    state_mcc[, 1:2]
  ),
  dplyr::left_join,
  by = 'GEOID'
)
str(state_mcc_merged)

# save state overall and domain specific ERI 
saveRDS(state_mcc_merged, "Exploratory analysis/pca missing values/state_mcc_merged.rds")


# Calculate full MSA MCC -----

cbsa_list <- tract_env_race_redline |>
  distinct(cbsa_code, cbsa_title, metro_micro_type) |>
  # filter(metro_micro_type == "Metropolitan Statistical Area") |>
  drop_na(cbsa_code)
nrow(cbsa_list) # n=925 unique CBSA codes

# correlation between environment and poc
r1 <- tract_env_race_redline |>
  group_by(cbsa_code, cbsa_title) |>
  dplyr::summarize(cor(environment_s, pct_nnhwhite, use = "complete.obs")) |>
  rename(r_env_poc = 3)

# correlation between environment and mdi
r2 <- tract_env_race_redline |>
  group_by(cbsa_code, cbsa_title) |>
  dplyr::summarize(cor(environment_s, Redline, use = "complete.obs")) |>
  rename(r_env_redline = 3)

# correlation between poc and mdi
r3 <- tract_env_race_redline |>
  group_by(cbsa_code, cbsa_title) |>
  dplyr::summarize(cor(pct_nnhwhite, Redline, use = "complete.obs")) |>
  rename(r_poc_redline = 3)

# mean eei poc and mdi at cbsa level
r4 <- tract_env_race_redline |>
  group_by(cbsa_code, cbsa_title) |>
  drop_na(c("environment_s", "pct_nnhwhite", "Redline")) |>
  dplyr::summarize(
    mean_pollution = mean(pollution_s),
    mean_hazard = mean(hazard_s),
    mean_builtenv = mean(builtenv_s),
    mean_climate = mean(climate_s),
    mean_housing = mean(housing_s),
    mean_env = mean(environment_s),
    mean_poc = mean(pct_nnhwhite),
    mean_mdi = mean(Redline)
  )

# get CBSA level mcc values
cbsa_mcc <- data.frame()
for (i in cbsa_list$cbsa_title) {
  # ERI
  
  t_pollution <- mcc(tract_env_race_redline |>
             filter(cbsa_title == i),
           "pollution_s",
           "pct_nnhwhite",
           "Redline") |> 
    select(1) |> 
    rename(mcc_pollution = 1)
  t_hazard <- mcc(tract_env_race_redline |>
             filter(cbsa_title == i),
           "hazard_s",
           "pct_nnhwhite",
           "Redline")|> 
    select(1) |> 
    rename(mcc_hazard = 1)
  t_builtenv <- mcc(tract_env_race_redline |>
             filter(cbsa_title == i),
           "builtenv_s",
           "pct_nnhwhite",
           "Redline")|> 
    select(1) |> 
    rename(mcc_builtenv = 1)
  t_climate <- mcc(tract_env_race_redline |>
             filter(cbsa_title == i),
           "climate_s",
           "pct_nnhwhite",
           "Redline")|> 
    select(1) |> 
    rename(mcc_climate = 1)
  t_housing <- mcc(tract_env_race_redline |>
             filter(cbsa_title == i),
           "housing_s",
           "pct_nnhwhite",
           "Redline")|> 
    select(1) |> 
    rename(mcc_housing = 1)
  t_env <- mcc(tract_env_race_redline |>
             filter(cbsa_title == i),
           "environment_s",
           "pct_nnhwhite",
           "Redline")|> 
    select(1) |> 
    rename(mcc_overall = 1)
  
  t1 <- cbind(cbsa_list |> 
                filter(cbsa_title == i), t_pollution, t_hazard, t_builtenv, t_climate, t_housing, t_env)
  cbsa_mcc <- rbind(cbsa_mcc, t1)
}

# merge ERI and two-way correlation coefficients
cbsa_mcc_merged <- cbsa_mcc |>
  left_join(r1) |>
  left_join(r2) |>
  left_join(r3) |> 
  left_join(r4)

saveRDS(cbsa_mcc_merged,"Exploratory analysis/pca missing values/cbsa_mcc_merged.rds")


# partial MSA MCC -------------------------------------------------------------

# This portion of the script calculates the MSA ERI values based on the counties
# used for calcuating MSA race-specific mortality rates

# read tract merged EEI, PMR, and MDI data
tract_env_race_redline <- read_csv("Exploratory analysis/pca missing values/tract_env_race_redline.csv")

# load mcc function
source("Exploratory analysis/R codes/functions.R")

# read CBSA to county crosswalk
cbsa_county_xwalk <- read_csv("Shapefiles/2017 cbsa/cbsa_xw041224.csv") |>
  mutate(GEOID = paste0(statefips, countyfips),
         cbsa_code = as.character(cbsa_code))
str(cbsa_county_xwalk)

# get CBSA codes in AK and HI
cbsa_ak_hi <- cbsa_county_xwalk |>
  filter(statefips %in% c("02", "15")) |>
  pull(cbsa_code)

# county flags for calculating partial MSA mortality rates
county_m1519_all <- read_csv("MRR/county_m1519_all.csv") |>
  mutate(statefips = substr(GEOID, 1, 2)) |>
  filter(!statefips %in% c("02", "15")) # remove Alaska and Hawaii
str(county_m1519_all)


# read new partial MSA mortality rate measures
MSA_Mortality_CDCPOP_201519_covar <- read_csv("MRR/MSA_Mortality_CDCPOP_201519_covar.csv") |>
  mutate(CBSAFP = as.character(CBSAFP)) |>
  filter(!CBSAFP %in% cbsa_ak_hi)
names(MSA_Mortality_CDCPOP_201519_covar)
#note: this file has county id and four columns for NHBlack, Black, White, and
#Hispanic with a value of 0 or 1. Zero means the county was excluded from the
#calculation of the MSA-level for a specific race/ethnicity. Wei, you can link
#this file to the tract-level dataset and choose the one with 1 for both two
#racial/ethnic groups for a certain rate ratio, then re-calculate the MSA-level
#ERI.

# calculate percentage of counties and populations included in mortality rate and ratio measures
MSA_Mortality_CDCPOP_201519_covar <- MSA_Mortality_CDCPOP_201519_covar |>
  mutate(
    # pct counties in MSA
    pct_countyn_b = n_Black / numcounty,
    pct_countyn_nhb = n_NHBlack / numcounty,
    pct_countyn_nhw = n_NHWhite / numcounty,
    pct_countyn_hisp = n_Hispanic / numcounty,
    
    # pct population in MSA
    pct_pop_b = pop_Black / Black,
    pct_pop_nhb = pop_NHBlack / NHBlack,
    pct_pop_nhw = pop_NHWhite / NHWhite,
    pct_pop_hisp = pop_Hispanic / Hispanic
  )

# range of percentage of counties in MSA
range(MSA_Mortality_CDCPOP_201519_covar$pct_countyn_nhw) # 1 to 1
range(MSA_Mortality_CDCPOP_201519_covar$pct_countyn_nhb, na.rm = T) # 0.2 to 1
range(MSA_Mortality_CDCPOP_201519_covar$pct_countyn_b, na.rm = T) # 0.2 to 1
range(MSA_Mortality_CDCPOP_201519_covar$pct_countyn_hisp, na.rm = T) # 0.14 to 1

# range of percentage of populations in MSA
range(MSA_Mortality_CDCPOP_201519_covar$pct_pop_nhw) # 1 to 1
range(MSA_Mortality_CDCPOP_201519_covar$pct_pop_nhb, na.rm = T) # 0.05 to 1
range(MSA_Mortality_CDCPOP_201519_covar$pct_pop_b, na.rm = T) # 0.05 to 1
range(MSA_Mortality_CDCPOP_201519_covar$pct_pop_hisp, na.rm = T) # 0.13 to 1


# calculate the number of MSAs with different percentage of counties and populations

# Black
nrow(MSA_Mortality_CDCPOP_201519_covar |> filter(pct_pop_b >= 0.9)) / nrow(MSA_Mortality_CDCPOP_201519_covar) # 94.4%
nrow(MSA_Mortality_CDCPOP_201519_covar |> filter(pct_pop_b >= 0.8)) / nrow(MSA_Mortality_CDCPOP_201519_covar) # 96.6%

# NHB
nrow(MSA_Mortality_CDCPOP_201519_covar |> filter(pct_pop_nhb >= 0.9)) / nrow(MSA_Mortality_CDCPOP_201519_covar) # 94.4%
nrow(MSA_Mortality_CDCPOP_201519_covar |> filter(pct_pop_nhb >= 0.8)) / nrow(MSA_Mortality_CDCPOP_201519_covar) # 96.0%

# Hispanic
nrow(MSA_Mortality_CDCPOP_201519_covar |> filter(pct_pop_hisp >= 0.9)) / nrow(MSA_Mortality_CDCPOP_201519_covar) # 85.4%
nrow(MSA_Mortality_CDCPOP_201519_covar |> filter(pct_pop_hisp >= 0.8)) / nrow(MSA_Mortality_CDCPOP_201519_covar) # 90.7%

saveRDS(MSA_Mortality_CDCPOP_201519_covar,"MRR/msa_mrr_pctsumm.rds")



# function for calculating MSA ERI based on partial counties for which mortality rate
# for a racial group is calculated

# names(MSA_Mortality_CDCPOP_201519_covar)
# race_rate <- "ageadj_Black"
# race_flg <- "n_Blac"
# race_abbr <- "b"

partmsa_mcc_race <- function(race_rate, race_flg, race_abbr) {
  # get CBSAs with mortality rate for the racial group
  msa_ids <- MSA_Mortality_CDCPOP_201519_covar |>
    filter(!is.na(!!sym(race_rate))) |>
    pull(CBSAFP)

  # calculate MSA ERI values based on partial counties within MSAs
  partmsa_mcc <- data.frame()
  for (i in msa_ids) {
    # get county list for the MSA from the cbsa to county crosswalk
    county_ids <- cbsa_county_xwalk |>
      filter(cbsa_code == i) |>
      pull(GEOID)

    # get tracts in the counties used for calculate mortality rates for partial MSA
    partcounty_ids <- county_m1519_all |>
      filter(GEOID %in% county_ids) |>
      filter(!!sym(race_flg) == 1) |>
      pull(GEOID) # county GEOIDs

    dt <- tract_env_race_redline |>
      mutate(GEOID_cnty = paste0(statefips, countyfips)) |>
      filter(GEOID_cnty %in% partcounty_ids) |>
      drop_na(c("pct_nnhwhite", "Redline", "environment_s"))

    # calculate n of tracts
    tract_n <- nrow(dt)
    t <- mcc(dt, "pct_nnhwhite", "Redline", "environment_s")
    t1 <- cbind(i, t, tract_n)
    partmsa_mcc <- rbind(partmsa_mcc, t1)
  }

  names(partmsa_mcc) <- c("cbsa", paste0("mcc_", race_abbr), "LL", "UL", "P", "tract_n") # n=373
  return(partmsa_mcc)
}

partmsa_mcc_nhw <- partmsa_mcc_race("ageadj_NHWhite", "n_NHWh", "nhw")
partmsa_mcc_b <- partmsa_mcc_race("ageadj_Black", "n_Blac", "b")
# CBSA=30860 have only 2 tracts, NA value for mcc is generated
partmsa_mcc_nhb <- partmsa_mcc_race("ageadj_NHBlack", "n_NHBl", "nhb")
partmsa_mcc_hisp <- partmsa_mcc_race("ageadj_Hispanic", "n_Hisp", "hisp")

# merge partial MSA ERI values
partmsa_mcc <- partmsa_mcc_nhw |> select(cbsa, mcc_nhw) |> 
  left_join(partmsa_mcc_b |> select(cbsa, mcc_b), by = "cbsa") |>
  left_join(partmsa_mcc_nhb |> select(cbsa, mcc_nhb), by = "cbsa") |>
  left_join(partmsa_mcc_hisp |> select(cbsa, mcc_hisp), by = "cbsa") |> 
  mutate(
    b_nhb_diff = ifelse(mcc_b == mcc_nhb, 0, 1)
  )

# save partial MSA ERI values
saveRDS(partmsa_mcc,"Exploratory analysis/pca missing values/partmsa_mcc.rds")




