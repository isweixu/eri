## Description: This R script contains the code for creating supplementary tables. 
## Author: Wei Xu 
## Organization: Medical College of Wisconsin 
## Date: September, 2025


# Supplementary table 8 ------------------------
# State average of EEI, PMR, MDI and overall and domain-speciifc ERI

# read tract environmental data
tract_env_race_redline <- read_csv("Exploratory analysis/pca missing values/tract_env_race_redline.csv")
skimr::skim(tract_env_race_redline)

states <- fips_codes |> 
  dplyr::select(state, state_code, state_name) |> 
  rename(GEOID = state_code) |> 
  distinct()

# read state overall and domain specific mcc values
state_mcc_merged <- readRDS("Exploratory analysis/pca missing values/state_mcc_merged.rds")

# merge state EEI, PMR, MDI with state mcc values
state_mcc_eei_pmr_mdi <- state_mcc_merged |> 
  rename(
    ERI = mcc,
    ERI_builtenv = mcc_builtenv,
    ERI_climate = mcc_climate,
    ERI_hazard = mcc_hazard,
    ERI_housing = mcc_housing,
    ERI_pollution = mcc_pollution
  ) |> 
  left_join(states) |> 
  left_join(tract_env_race_redline |> 
              group_by(statefips) |> 
              dplyr::summarize(
                EEI = mean(environment_s, na.rm = T),
                EEI_builtenv = mean(builtenv_s, na.rm = T),
                EEI_climate = mean(climate_s, na.rm = T),
                EEI_hazard = mean(hazard_s, na.rm = T),
                EEI_housing = mean(housing_s, na.rm = T),
                EEI_pollution = mean(pollution_s, na.rm = T),
                PMR = mean(pct_nnhwhite, na.rm = T), 
                MDI = mean(Redline, na.rm = T)) |> 
              dplyr::select(statefips, starts_with("EEI"), PMR, MDI) |> 
              rename(GEOID=statefips)) |> 
  dplyr::select(GEOID, state, state_name,
                starts_with("ERI"), 
                starts_with("EEI"), 
                PMR, MDI)

write.csv(state_mcc_eei_pmr_mdi, "Exploratory analysis/pca missing values/state_mcc_eei_pmr_mdi.csv")


# Supplementary table 9 ---------------------
# MSA average of tract EEI, PMR, MDI, overall and domain specific ERI 

cbsa_mcc_merged <- readRDS("Exploratory analysis/pca missing values/cbsa_mcc_merged.rds")

# calculate mean eei, pmr, mdi at MSA level
metro_eei_pmr_mdi <- tract_env_race_redline |> 
  group_by(cbsa_code, cbsa_title, metro_micro_type) |> 
  dplyr::summarize(
    EEI = mean(environment_s, na.rm = T),
    EEI_builtenv = mean(builtenv_s, na.rm = T),
    EEI_climate = mean(climate_s, na.rm = T),
    EEI_hazard = mean(hazard_s, na.rm = T),
    EEI_housing = mean(housing_s, na.rm = T),
    EEI_pollution = mean(pollution_s, na.rm = T),
    PMR = mean(pct_nnhwhite, na.rm = T), 
    MDI = mean(Redline, na.rm = T)) |> 
  filter(metro_micro_type == "Metropolitan Statistical Area") 
head(metro_eei_pmr_mdi)

# merge MSA mean EEI, PMR, MDI with MSA mcc values
metro_mcc_eei_pmr_mdi <- cbsa_mcc_merged |> 
  select(1:9) |> 
  filter(metro_micro_type == "Metropolitan Statistical Area") |>
  rename(
    ERI = mcc_overall,
    ERI_builtenv = mcc_builtenv,
    ERI_climate = mcc_climate,
    ERI_hazard = mcc_hazard,
    ERI_housing = mcc_housing,
    ERI_pollution = mcc_pollution
  ) |> 
  left_join(metro_eei_pmr_mdi)

write.csv(metro_mcc_eei_pmr_mdi, "Exploratory analysis/pca missing values/metro_mcc_eei_pmr_mdi.csv")



# Supplementary table 10 ------------------------
# MSA-specific correlations of EEI, MDI, and PMR

tract_env_race_redline <- read_csv("Exploratory analysis/pca missing values/tract_env_race_redline.csv")

msa_correlations <- tract_env_race_redline |>
  filter(metro_micro_type == "Metropolitan Statistical Area") |>
  group_by(cbsa_code,cbsa_title) |>
  summarise(cor_eei_pmr = cor(environment_s, pct_nnhwhite, use = "pairwise.complete.obs"),
            cor_eei_mdi = cor(environment_s, Redline, use = "pairwise.complete.obs"),
            cor_pmr_mdi = cor(pct_nnhwhite, Redline, use = "pairwise.complete.obs")) 

write_csv(msa_correlations, "Exploratory analysis/pca missing values/msa_correlations.csv")


