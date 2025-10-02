# this file creates census tract environmental exposure index (EEI).
# the output file merges tract EEI, PMR, and MDI


library(ggcorrplot)
library(tidyverse)
library(readr)
library(tidycensus)
library(tidyr)
library(ggcorrplot)
library(stringr)
library(viridis)
library(tigris)
library(sf)
library(Hmisc)
library(nFactors)
library(psych)
library(patchwork)
library(ltm)

options(scipen=999)

setwd("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics")

# Percent Minority Residents (PMR)  ----------------------------------------------------------------

# load ACS 2015-2019 race and ethnicity data for tracts
nhgis0076_ds244_20195_tract <- read_csv("Exploratory analysis/race_acs1519/nhgis0076_csv/nhgis0076_ds244_20195_tract.csv")

# ALUKE001: Total
# ALUKE003: Not Hispanic or Latino: White alone

pctnnhwhite_tract <- nhgis0076_ds244_20195_tract |> 
  dplyr::select(GEOID, ALUKE001, ALUKE003) |> 
  mutate(
    GEOID = as.character(GEOID),
    pct_nnhwhite = 1-(ALUKE003/ALUKE001)
  )

# tracts without any population
nrow(pctnnhwhite_tract |> filter(ALUKE001 == 0)) # n=703
nrow(pctnnhwhite_tract |> filter(is.na(pct_nnhwhite))) # 703

# save tract PMR data
write.csv(pctnnhwhite_tract, "Exploratory analysis/pca missing values/pctnnhwhite_tract.csv") 


# Mortgage Disinvestment Index (MDI)  ----------------------------------------------------------

redlining_tract10Avg_1519all <- read_csv("HMDA/redlining_tract10Avg_1519all.csv")

redlining_tab <- redlining_tract10Avg_1519all |> group_by(STATEFP10) |> count()
redlining_tab <- redlining_tract10Avg_1519all |> group_by(st) |> count()
redlining_tab <- redlining_tract10Avg_1519all |> group_by(MSA_name) |> count()

redlining_cleaned <- redlining_tract10Avg_1519all |> 
  mutate(
    statefips = substr(TractID10, 1,2),
    countyfips = substr(TractID10, 3,5),
    tractfips = substr(TractID10, 6,11),
  ) |> 
  select(TractID10, statefips, countyfips, tractfips, Redline, type) |> 
  rename(GEOID=TractID10) |> 
  arrange(statefips, countyfips, tractfips)

# read tract-cbsa crosswalk file
cbsa_xw041224 <- read_csv("2017 cbsa/cbsa_xw041224.csv")

# merge tract redlining data with tract-cbsa crosswalk file
redlining_cleaned1 <- redlining_cleaned |> 
  left_join(cbsa_xw041224)

write.csv(redlining_cleaned1, "HMDA/redlining_tract10Avg_1519all_cleaned.csv")

# check for tracts with redlining estimates but no environmental data
tracts_outlier <- redlining_tracts_lower48 |> 
  filter(!TractID10 %in% master_df_final$GEOID10)
# n=0, all good!

redlining <- read_csv("HMDA/redlining_tract10Avg_1519all_cleaned.csv")[-1]





# read tract environmental master file ------------

master_df_final <- read_csv("Data/master_df_final.csv")

# inspect missingness
tab_missing <- colSums(is.na(master_df_final)) |>  
  as.data.frame() |> 
  rownames_to_column("var") |> 
  rename(n_missing=2) |> 
  mutate(pct_missing = n_missing/72271)


# PCA with missing values ------

# some references:
# https://github.com/hredestig/pcamethods 
# https://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html 

BiocManager::install("pcaMethods", force = TRUE)
browseVignettes("pcaMethods")
library(pcaMethods)


## housing -----------------------------------------------------------------

housing_vars <- master_df_final |> 
  dplyr::select(GEOID10, pct_renterhh, pct_10units, pct_mobilehome, pct_crowding,
         pct_nocar, pct_groupquarter, res_vacancy_rate, 
         energy_burden, pct_housing_burden, pct_no_plumbing)

# ## energy burden: two tracts with values > 100, the greatest value being 1322!!! 
# this variable is not used
housing_vars |> filter(energy_burden > 1) |> nrow()
# n=55231 tracts where energy burden is > 1

housing_vars |> filter(energy_burden == 0) |> nrow()
# n=1168

# tracts with all NA values, n=169
# these tracts will be removed for PCA for other environmental domains
rows_na <- housing_vars %>% 
  filter_at(vars(pct_renterhh, pct_10units, pct_mobilehome, pct_crowding,
                 pct_nocar, pct_groupquarter, res_vacancy_rate, 
                 pct_housing_burden, pct_no_plumbing),
            all_vars(is.na(.)))

housing_vars_p <- housing_vars |>
  # removed energy burden variable due to data quality concern
  dplyr::select(-energy_burden) |>
  filter(!GEOID10 %in% rows_na$GEOID10) |>
  mutate(
    pct_housing_burden = pct_housing_burden / 100
    )
nrow(housing_vars_p)
# n=72,102 remaining tracts

# count records with missing values
tab_missing <- colSums(is.na(housing_vars_p)) |> 
  as.data.frame() |> 
  rownames_to_column("var") |> 
  rename(n_missing=2) |> 
  filter(!row_number() %in% c(1)) |> 
  mutate(pct_missing = n_missing/72102)

# Compute a correlation matrix
corr_fun(housing_vars_p[,-1])

## distribution plots
housing_vars_p |>
  pivot_longer(!GEOID10, values_to = "pct", names_to = "var") |>
  ggplot(aes(x=pct)) +
  geom_histogram() +
  facet_wrap(~ var, scales = "free")

# standardize data
housing_vars_s <- housing_vars_p |>
  dplyr::select(
    pct_renterhh, pct_10units, pct_mobilehome, pct_crowding,
    pct_nocar, pct_groupquarter, res_vacancy_rate, 
    pct_housing_burden, pct_no_plumbing
  ) |> 
  scale(center=TRUE, scale=TRUE) |>
  as.matrix()
nrow(housing_vars_s) # n=72101

# check internal consistency
psych::alpha(housing_vars_s |> 
                 as.data.frame() |> 
                 drop_na(), check.keys = T) # alpha=0.73

# PCA with the svdImpute method
resSVDI_housing <- pcaMethods::pca(housing_vars_s, method="svdImpute", nPcs=5)
resSVDI_housing@loadings
resSVDI_housing@R2
resSVDI_housing@R2cum # nPCA=4

# weights of the PCs (>70% variance explained)
pc_weights <- resSVDI_housing@R2[1:4] / sum(resSVDI_housing@R2[1:4])
pc_weights

# merge PC scores with the original data
housing_vars_p1 <- cbind(housing_vars_p, resSVDI_housing@scores)

# check anchor variable and PC correlations
rcorr(as.matrix(housing_vars_p1 |> 
                  dplyr::select(pct_renterhh, PC1, PC2, PC3, PC4, PC5)))

# pct_renterhh              PC1   PC2  PC3  PC4   PC5
# pct_renterhh         1.00 0.91 -0.01 0.06 -0.01 -0.09
# PC1                  0.91 1.00  0.00 0.00 0.00  0.00
# PC2                 -0.01 0.00  1.00 0.00 0.00  0.00
# PC3                  0.06 0.00  0.00 1.00 0.00  0.00
# PC4                  -0.01 0.00  0.00 0.00 1.00  0.00
# PC5                 -0.09 0.00  0.00 0.00 0.00  1.00


# merge PC scores with the original data
housing_vars_p1 <- housing_vars_p1 |> 
  # flip the signs to make them positive
  mutate(PC2= -PC2, PC4=-PC4)

# calculate domain-specific index by summing PC scores multiplied by PC weights
for (i in 1:nrow(housing_vars_p1)){
  housing_vars_p1$housing_s[i] <- sum(housing_vars_p1[i, 11:14] * pc_weights)
}

write_csv(housing_vars_p1, "Exploratory analysis/pca missing values/pca results/housing_pca.csv")




## built environment -----

built_env <- master_df_final |> 
  dplyr::select(GEOID10, E_PARK, 
         parks_0.5mile, parks_1mile,
         count_open_parks, prop_park_area_tract, ndvi_avg,
         NatWalkInd, 
         avg_download_speed_1519, 
         avg_upload_speed_1519,
         perc_no_internet, 
         strNetDensity,
         conNodeRatio,
         gamma, alpha,
         # LATracts1, LATracts_half, # flag variables
         lapophalfshare, lapop1share, # pct_missing = 6.7% & 27.8%
         pct_tree, pct_imperviousness) |> 
  filter(GEOID10 %in% housing_vars_p$GEOID10)

# count records with missing values
colSums(is.na(built_env)) |> 
  as.data.frame() |> 
  rownames_to_column("var") |> 
  rename(n_missing=2) |> 
  filter(!row_number() %in% c(1)) |> 
  mutate(pct_missing = n_missing/72102)

corr_fun(built_env[,-1])

# alpha, gamma, avg_upload_speed_1519, stnetdensity,
# E_PARK is highly correlated with parks_0.5mile and parks_1mile but has fewer missing values

corr_fun(built_env |> 
           dplyr::select(E_PARK, parks_0.5mile, parks_1mile, 
                         count_open_parks, prop_park_area_tract, ndvi_avg, pct_tree)
         )

corr_fun(built_env |> dplyr::select(NatWalkInd, strNetDensity,
                              conNodeRatio, gamma, alpha))
# finalize variables
built_env1 <- built_env |> 
  dplyr::select(GEOID10, E_PARK, 
         prop_park_area_tract, ndvi_avg,
         NatWalkInd, avg_download_speed_1519, 
         perc_no_internet, 
         lapophalfshare, 
         pct_tree, pct_imperviousness) 

# correlation matrix
corr_fun(built_env1[,-1])

# standardize data set
built_env_s <- built_env1 |>
  dplyr::select(-GEOID10) |> 
  scale() |>
  as.matrix()

# check internal consistency
psych::alpha(built_env_s |> 
                 as.data.frame() |> 
                 drop_na(), check.keys = T) # alpha = 0.78

# PCA with the svdImpute method
resSVDI_builtenv <- pcaMethods::pca(built_env_s, method="svdImpute", nPcs=5)
resSVDI_builtenv@loadings
resSVDI_builtenv@R2
resSVDI_builtenv@R2cum

# weights of the first four PCs (>70% variance explained)
pc_weights <- resSVDI_builtenv@R2[1:4] / sum(resSVDI_builtenv@R2[1:4])
pc_weights

# merge PC scores with the original data
built_env1 <- cbind(built_env1, resSVDI_builtenv@scores)

# inspect correlation between PCs and anchor variable (pct_imperviousness)
rcorr(as.matrix(built_env1 |> 
                  dplyr::select(pct_imperviousness, PC1, PC2, PC3, PC4)))

# PC1, r=0.91; PC2, r=0.13; PC3, r=0.04; PC4, r=0.01
# no change needed

# calculate domain-specific index by summing PC scores multiplied by PC weights
for (i in 1:nrow(built_env1)){
  built_env1$builtenv_s[i] <- sum(resSVDI_builtenv@scores[i, 1:4] * pc_weights)
}

write_csv(built_env1, "Exploratory analysis/pca missing values/pca results/builtenv_pca.csv")


## air pollution -----------------------------------------------------------

pollution_vars <- master_df_final |> 
  select(GEOID10, 
         no2, o3, pm10, pm25, so2, co, pnc, # CACES
         E_OZONE, E_PM, # EJI
         PM25, DSLPM, OZONE # EJScreen2016
         ) |> 
  filter(GEOID10 %in% housing_vars_p$GEOID10)

# inspect missingness
tab_missing <- colSums(is.na(pollution_vars)) |> 
  as.data.frame() |> 
  rownames_to_column("var") |> 
  rename(n_missing=2) |> 
  filter(!row_number() %in% c(1)) |> 
  mutate(pct_missing = n_missing/72103)

# correlation matrix of all air pollution domain variables
rcorr(as.matrix((pollution_vars[,-1])))

# correlation matrix of all PM variables
pollution_vars |>
  select(pm25, PM25, E_PM, pm10, pnc, DSLPM) |>
  corr_fun()
# PM25 is from 2012 (EJScreen 2016 release), not used
# DSLPM is from 2012, not used
# E_PM is from 2014-2016, Annual mean days above PM2.5 regulatory standard
# pm25 from CECAS, 2015-2019 annual average
# corr(E_PM, pm25) = 0.83

# correlation matrix of all OZONE variables
corr_fun(pollution_vars |> select(o3, OZONE, E_OZONE))
# o3, from CECAS, 2015-2019 annual average
# E_OZONE - Annual mean days above O3 regulatory standard btw 2014-2016
# OZONE - EJScreen, Annual average of top ten daily maximum 8-hour air concentrations in parts per billion. data year=2012


# finalize variables
pollution_vars_1 <- pollution_vars |> 
  select(GEOID10, no2, o3, pm10, pm25, so2, co, pnc, DSLPM) 

# inspect missingness
rows_na <- pollution_vars %>%
  filter_at(vars(no2, o3, pm10, pm25, so2, co, pnc),
            all_vars(is.na(.))) # n=0

# correlation matrix of finalized variables
corr_fun(pollution_vars_1[,-1])

# Standardize variables 
pollution_vars_s <- pollution_vars_1 |>
  select(-GEOID10) |>
  scale() |>
  as.matrix()

# check internal consistency
psych::alpha(pollution_vars_1 |> 
               select(-GEOID10) |> 
               drop_na(), check.keys = T) # alpha = 0.85

# PCA with the svdImpute method
resSVDI_pollution <- pcaMethods::pca(pollution_vars_s, method="svdImpute", nPcs=5)
resSVDI_pollution@loadings
resSVDI_pollution@R2
resSVDI_pollution@R2cum #keep the first three PCs

# weights of the first three PCs (>70% variance explained)
pc_weights <- resSVDI_pollution@R2[1:3] / sum(resSVDI_pollution@R2[1:3])
pc_weights

# merge PC scores with the original data
pollution_vars_1 <- cbind(pollution_vars_1, resSVDI_pollution@scores)

# inspect correlation between PCs and anchor variable 
rcorr(as.matrix(pollution_vars_1 |> select(no2, starts_with("PC"))))

# no2  PC1  PC2  PC3   PC4   PC5
# no2  1.00 0.94 0.14 0.03 -0.16 -0.06
# PC1  0.94 1.00 0.00 0.00  0.00  0.00
# PC2  0.14 0.00 1.00 0.00  0.00  0.00
# PC3  0.03 0.00 0.00 1.00  0.00  0.00
# PC4 -0.16 0.00 0.00 0.00  1.00  0.00
# PC5 -0.06 0.00 0.00 0.00  0.00  1.00

# no need to flip PC signs


# calculate domain-specific index by summing PC scores multiplied by PC weights
for (i in 1:nrow(pollution_vars_1)){
  pollution_vars_1$pollution_s[i] <- sum(resSVDI_pollution@scores[i, 1:3] * pc_weights)
}

write_csv(pollution_vars_1, "Exploratory analysis/pca missing values/pca results/pollution_pca.csv")


## hazards -----------------------------------------------------------------

hazard_vars <- master_df_final |> 
  select(GEOID10, E_NPL, E_TSD, E_COAL, E_LEAD, E_TRI, E_RAIL, 
         E_ROAD, E_AIRPRT, E_RMP, E_IMPWTR, light_15_19,
         # PTRAF, PNPL, PTSDF, PRMP, PWDIS, 
         existing_noise_mean, pct_pre1960) |> 
  filter(GEOID10 %in% housing_vars_p$GEOID10)

# count missing values
tab_missing <- colSums(is.na(hazard_vars)) |> 
  as.data.frame() |> 
  rownames_to_column("var") |> 
  rename(n_missing=2) |> 
  filter(!row_number() %in% c(1)) |> 
  mutate(pct_missing = n_missing/72102)

# Compute a correlation matrix
corr_fun(hazard_vars[,-1])
# pairwise correlations are all below 0.8

# standardize data 
hazard_vars_s <- hazard_vars |>
  select(-GEOID10) |>
  scale() |>
  as.matrix()

# check internal consistency
psych::alpha(hazard_vars_s |> 
               as.data.frame() |> 
               drop_na(),
             check.keys=TRUE) # alpha=0.73

# PCA with the svdImpute method
resSVDI_hazard <- pcaMethods::pca(hazard_vars_s, method="svdImpute", nPcs=7)
resSVDI_hazard@loadings
resSVDI_hazard@R2
resSVDI_hazard@R2cum 

# weights of the first 7 PCs (>70% variance explained)
pc_weights <- resSVDI_hazard@R2[1:7] / sum(resSVDI_hazard@R2[1:7])
pc_weights

# E_TRI has the strongest loading on PC1 (0.42)

# merge PC scores with the original data
hazard_vars <- cbind(hazard_vars, resSVDI_hazard@scores)

# inspect correlation between PCs and anchor variable 
rcorr(as.matrix(hazard_vars |> select(E_TRI, PC1:PC7)))

# E_TRI       PC1  PC2  PC3  PC4   PC5   PC6  PC7
# E_TRI  1.00 0.81 0.16 0.02 0.02 -0.04 -0.05 0.01
# PC1    0.81 1.00 0.00 0.00 0.00  0.00  0.00 0.00
# PC2    0.16 0.00 1.00 0.00 0.00  0.00  0.00 0.00
# PC3    0.02 0.00 0.00 1.00 0.00  0.00  0.00 0.00
# PC4    0.02 0.00 0.00 0.00 1.00  0.00  0.00 0.00
# PC5   -0.04 0.00 0.00 0.00 0.00  1.00  0.00 0.00
# PC6   -0.05 0.00 0.00 0.00 0.00  0.00  1.00 0.00
# PC7    0.01 0.00 0.00 0.00 0.00  0.00  0.00 1.00

# round(cor(dt |> select(pct_imperviousness, PC1, PC2, PC3, PC4, PC5, PC6, PC7), 
#                   use = "pairwise.complete.obs"), 2)

# flip the signs of PC5 and PC6 to make them positive
hazard_vars <- hazard_vars |> 
  mutate(PC5 = -PC5, PC6 = -PC6)

# calculate tract domain-specific index by summing PC scores multiplied by PC weights
for (i in 1:nrow(hazard_vars)){
  hazard_vars$hazard_s[i] <- sum(hazard_vars[i, 15:21] * pc_weights)
}

write_csv(hazard_vars, "Exploratory analysis/pca missing values/pca results/hazard_pca.csv")


## climate -----------------------------------------------------------------

# drought, precipitation, tornado factors all have multiple indicators
# tornado measures from CDCEPH are not based on 2010 geographies, not used

climate_vars <- master_df_final |> 
  select(GEOID10, 
         contains("drought"), 
         ends_with("irradiance"),
         # starts_with("ef"), # tornado measures
         ends_with("_AFREQ"),
         starts_with("rel"), 
         starts_with("abs"),
         agriculture_loss_rate,
         building_loss_rate, pct_flood_plain,
         pop_loss_rate, pct_prop_flood, pct_prop_fire)|> 
  filter(GEOID10 %in% housing_vars_p$GEOID10)
str(climate_vars)

# count records with missing values
tab_missing <- colSums(is.na(climate_vars)) |> 
  as.data.frame() |> 
  rownames_to_column("var") |> 
  rename(n_missing=2) |> 
  filter(!row_number() %in% c(1)) |> 
  mutate(pct_missing = n_missing/72103)

# Compute a correlation matrix of all climate variables
corr_fun(climate_vars[,-1])


### drought -----

# select drought variables
drought_vars <- climate_vars |> 
  select(GEOID10, contains("drought"), starts_with("DRGT"))

tab_missing <- colSums(is.na(drought_vars)) |> 
  as.data.frame() |> 
  rownames_to_column("var") |> 
  rename(n_missing=2) |> 
  # filter(!row_number() %in% c(1)) |> 
  mutate(pct_missing = n_missing/72102)


### precipitation -------------------------

precip_vars <- master_df_final |> 
  select(GEOID10, contains("precip"), RFLD_AFREQ, LNDS_AFREQ, 
         pct_prop_flood, pct_flood_plain)

tab_missing <- colSums(is.na(precip_vars)) |> 
  as.data.frame() |> 
  rownames_to_column("var") |> 
  rename(n_missing=2) |> 
  # filter(!row_number() %in% c(1)) |> 
  mutate(pct_missing = n_missing/72102)

# correlation matrix
precip_vars |> 
  select(-1) |> 
  corr_fun()


### finalize climate variables -----
climate_vars <- master_df_final |> 
  select(GEOID10,
         CWAV_AFREQ, # cold wave
         HWAV_AFREQ, # heat wave
         DRGT_AFREQ, # drought
         ERQK_AFREQ, # earthquake
         HAIL_AFREQ, # hail
         LNDS_AFREQ, # landslide
         LTNG_AFREQ, # lightning
         RFLD_AFREQ, # riverine flooding
         SWND_AFREQ, # strong wind
         TRND_AFREQ, # tornado
         WFIR_AFREQ, # Wildfire,
         WNTW_AFREQ,
         solar_irradiance, 
         uvnoon_irradiance,
         uv_irradiance) |> 
  filter(GEOID10 %in% housing_vars_p$GEOID10)

# count records with missing values
tab_missing <- colSums(is.na(climate_vars)) |> 
  as.data.frame() |> 
  rownames_to_column("var") |> 
  rename(n_missing=2) |> 
  filter(!row_number() %in% c(1)) |>
  mutate(pct_missing = n_missing/72102)

# Compute a correlation matrix
corr_fun(climate_vars[,-1])

# refine variable selections
climate_vars <- master_df_final |> 
  select(GEOID10,
         CWAV_AFREQ, # cold wave
         HWAV_AFREQ, # heat wave
         DRGT_AFREQ, # drought
         ERQK_AFREQ, # earthquake
         HAIL_AFREQ, # hail
         LNDS_AFREQ, # landslide
         RFLD_AFREQ, # riverine flooding
         SWND_AFREQ, # strong wind
         TRND_AFREQ, # tornado
         WFIR_AFREQ, # Wildfire,
         WNTW_AFREQ, 
         solar_irradiance) |> 
  filter(GEOID10 %in% housing_vars_p$GEOID10)

corr_fun(climate_vars[,-1])

# Standardize data 
climate_vars_s <- climate_vars |>
  select(-GEOID10) |>
  scale() |>
  as.matrix()

# check internal consistency
psych::alpha(climate_vars_s |> 
               as.data.frame() |> 
               drop_na(),
               check.keys=TRUE) # alpha=0.72

# PCA with the svdImpute method
resSVDI_climate <- pcaMethods::pca(climate_vars_s, method="svdImpute", nPcs=5)
resSVDI_climate@loadings
resSVDI_climate@R2
resSVDI_climate@R2cum # keep all 5 PCs

# weights of the first 5 PCs (>70% variance explained)
pc_weights <- resSVDI_climate@R2[1:5] / sum(resSVDI_climate@R2[1:5])
pc_weights

# ERQK_AFREQ has strongest loading on PC1 (-0.45)

# merge PC scores with the original data
climate_vars <- cbind(climate_vars, resSVDI_climate@scores)

# inspect correlation between PCs and anchor variable
rcorr(as.matrix(climate_vars |> select(ERQK_AFREQ, PC1:PC5)))

# ERQK_AFREQ              PC1   PC2   PC3  PC4  PC5
# ERQK_AFREQ       1.00 -0.85 -0.05 -0.08 0.03 0.01
# PC1             -0.85  1.00  0.00  0.00 0.00 0.00
# PC2             -0.05  0.00  1.00  0.00 0.00 0.00
# PC3             -0.08  0.00  0.00  1.00 0.00 0.00
# PC4              0.03  0.00  0.00  0.00 1.00 0.00
# PC5              0.01  0.00  0.00  0.00 0.00 1.00

# because ERQK_AFREQ is negatively loaded on PC1, flip its sign
# also flip the signs of PC2 and PC3 because they are negatively correlated with ERQK_AFREQ
climate_vars <- climate_vars |> 
  mutate(PC1 = -PC1, PC2 = -PC2, PC3 = -PC3)

# calculate domain-specific index by summing PC scores multiplied by PC weights
for (i in 1:nrow(climate_vars)){
  climate_vars$climate_s[i] <- sum(climate_vars[i, 14:18] * pc_weights)
}

write_csv(climate_vars, "Exploratory analysis/pca missing values/pca results/climate_pca.csv")


## merge domain specific environmental scores ------

climate_pca <- read_csv("Exploratory analysis/pca missing values/pca results/climate_pca.csv") |> 
  select(GEOID10, climate_s)
builtenv_pca <- read_csv("Exploratory analysis/pca missing values/pca results/builtenv_pca.csv") |> 
  select(GEOID10, builtenv_s)
pollution_pca <- read_csv("Exploratory analysis/pca missing values/pca results/pollution_pca.csv") |> 
  select(GEOID10, pollution_s)
housing_pca <- read_csv("Exploratory analysis/pca missing values/pca results/housing_pca.csv") |> 
  select(GEOID10, housing_s)
hazard_pca <- read_csv("Exploratory analysis/pca missing values/pca results/hazard_pca.csv") |> 
  select(GEOID10, hazard_s)

tract_env_pca <- climate_pca |> 
  left_join(builtenv_pca) |> 
  left_join(pollution_pca) |> 
  left_join(hazard_pca) |> 
  left_join(housing_pca) |> 
  rowwise() |> 
  mutate(environment_s = sum(c_across(c('pollution_s', 'housing_s', 'hazard_s',
                                         'climate_s', 'builtenv_s')), na.rm=TRUE))

write.csv(tract_env_pca, "Exploratory analysis/pca missing values/tract_env_pca.csv")



# merge environment, race, redlining data sets  ----------------------------------------------------------------

race_tract <- read_csv("Exploratory analysis/pca missing values/pctnnhwhite_tract.csv")[-1] |> 
  rename(GEOID1=GEOID) |> 
  mutate(GEOID = str_sub(GEOID1, 8, 18)) |> 
  select(GEOID, pct_nnhwhite)

redlining <- read_csv("HMDA/redlining_tract10Avg_1519all_cleaned.csv")[-1]

tract_env_pca <- read_csv("Exploratory analysis/pca missing values/tract_env_pca.csv")[-1]|> 
  rename(GEOID = GEOID10)

# merge environment, race, redlining data
tract_env_race_redline <- tract_env_pca |> 
  left_join(race_tract) |> 
  left_join(redlining) 

skimr::skim(tract_env_race_redline)
# 1 tract is missing redlining measure 
# 200 tracts are missing pct_nnhwhite

tract_env_race_redline <- tract_env_race_redline |> 
  drop_na(Redline, pct_nnhwhite) 
nrow(tract_env_race_redline) # n=71901

write.csv(tract_env_race_redline, "Exploratory analysis/pca missing values/tract_env_race_redline.csv")

