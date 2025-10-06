# This script contains code for MSA-level mortality analyses of the Environmental Racism Index (ERI) paper
# author: Wei Xu
# organization: Medical College of Wisconsin
# date: August, 2025

{
  library(ggcorrplot)
  library(tidyverse)
  library(readr)
  library(tidycensus)
  library(ggcorrplot)
  library(stringr)
  library(viridis)
  library(tigris)
  library(haven)
  library(sf)
  library(Hmisc)
  library(nFactors)
  library(psych)
  library(skimr)
  library(patchwork)
  library(lme4)
}

options(scipen=999)

setwd("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics")

# read MSA mortality metrics and covariates
MSA_Mortality_CDCPOP_201519_covar <- read_dta("MRR/MSA_Mortality_CDCPOP_201519_covar.dta")|>
  rename(cbsa_code = CBSAFP)|>
  mutate(NHBNHW_ratio = ageadj_NHBlack / ageadj_NHWhite)
names(MSA_Mortality_CDCPOP_201519_covar)

# read CBSA ERI values based on all counties within CBSAs
cbsa_mcc_merged <- readRDS("Exploratory analysis/pca missing values/cbsa_mcc_merged.rds") |> 
  mutate(cbsa_code = as.character(cbsa_code)) 

# merge CBSA ERI  with mortality, keep only metropolitan statistical areas
msa_mcc_mrr <- cbsa_mcc_merged |>
  filter(metro_micro_type == "Metropolitan Statistical Area") |>
  left_join(MSA_Mortality_CDCPOP_201519_covar)

nrow(msa_mcc_mrr) # n=379
nrow(msa_mcc_mrr |> filter(is.na(ageadj_Black))) # n=6
nrow(msa_mcc_mrr |> filter(is.na(ageadj_Hispanic))) # n=9

MSA_Mortality_CDCPOP_201519_covar |> 
  filter(!cbsa_code %in% msa_mcc_mrr$cbsa_code) -> msa_mismatch
# mortality file has two MSAs in AK and two other MSAs in HI


# regressions w/ full MSA ERI -----

## simple regression -------

# List of formulas for the models
formulas <- list(ageadj_NHWhite~mcc_overall,
                 ageadj_Black~mcc_overall,
                 ageadj_NHBlack~mcc_overall,
                 ageadj_Hispanic~mcc_overall,
                 
                 BNHW_ratio~mcc_overall,
                 NHBNHW_ratio~mcc_overall,
                 HNHW_ratio~mcc_overall)

# Fit the models
mortality_r <- sapply(formulas, function(f) as.character(f[[2]]))
models <- lapply(formulas, function(formula) lm(formula, data = msa_mcc_mrr))

# Extract and tidy model results
model_results_lm <- models |>
  map_df(tidy, .id = "model") |>
  filter(term=="mcc_overall") |>
  mutate(dependent_variable = mortality_r,
         model_v = "unadjusted")
print(model_results_lm)

# get confidence intervals
summary(models[[6]]); confint(models[[6]])

## adjusted models -------

### models with MSA covariates --------

formulas_adj <- list(ageadj_NHWhite~mcc_overall+MedianHHInc+perc_pov+popden17,
                 ageadj_Black~mcc_overall+MedianHHInc+perc_pov+popden17,
                 ageadj_NHBlack~mcc_overall+MedianHHInc+perc_pov+popden17,
                 ageadj_Hispanic~mcc_overall+MedianHHInc+perc_pov+popden17,
                 
                 BNHW_ratio~mcc_overall+MedianHHInc+perc_pov+popden17,
                 NHBNHW_ratio~mcc_overall+MedianHHInc+perc_pov+popden17,
                 HNHW_ratio~mcc_overall+MedianHHInc+perc_pov+popden17)

# Fit the models
mortality_r <- sapply(formulas_adj, function(f) as.character(f[[2]]))
models_adj <- lapply(formulas_adj, function(formula) lm(formula, data = msa_mcc_mrr))

# Extract and tidy model results
model_results_adj <- models_adj |>
  map_df(tidy, .id = "model") |>
  filter(term=="mcc_overall") |>
  mutate(dependent_variable = mortality_r,
         model_v= "adjusted") 
print(model_results_adj)

summary(models_adj[[2]]); confint(models_adj[[2]])
summary(models_adj[[3]]); confint(models_adj[[3]])
summary(models_adj[[4]]); confint(models_adj[[4]])


### models with census regions random effects -------

msa_mcc_mrr <- msa_mcc_mrr |>
  mutate(
    mhi = MedianHHInc/1000,
  )
table(msa_mcc_mrr$region)


#### ICC -----

# MR
lm1 <- lmer(ageadj_NHWhite ~  (1 | region), data = msa_mcc_mrr)
performance::icc(lm1) # 17.8%

lm2 <- lmer(ageadj_Black ~  (1 | region), data = msa_mcc_mrr)
performance::icc(lm2) # 28.5%

lm3 <- lmer(ageadj_NHBlack ~  (1 | region), data = msa_mcc_mrr)
performance::icc(lm3) # 18.1%

lm4 <- lmer(ageadj_Hispanic ~  (1 | region), data = msa_mcc_mrr)
performance::icc(lm4) # 17.8%

# MRR
lm5 <- lmer(BNHW_ratio ~  (1 | region), data = msa_mcc_mrr)
performance::icc(lm5) # 13.8%

lm6 <- lmer(NHBNHW_ratio ~  (1 | region), data = msa_mcc_mrr)
performance::icc(lm6) # 5.3%

lm7 <- lmer(HNHB_ratio ~  (1 | region), data = msa_mcc_mrr)
performance::icc(lm7) # 27.9%


#### estimating mixed effect models 

# formulas_mixed <- list(ageadj_NHWhite ~ mcc_overall+mhi+perc_pov+popden17 + (1 | region),
#                      ageadj_Black ~ mcc_overall+mhi+perc_pov+popden17 + (1 | region),
#                      ageadj_NHBlack ~ mcc_overall+mhi+perc_pov+popden17 + (1 | region),
#                      ageadj_Hispanic ~ mcc_overall+mhi+perc_pov+popden17 + (1 | region),
#                      
#                      BNHW_ratio ~ mcc_overall+mhi+perc_pov+popden17 + (1 | region),
#                      NHBNHW_ratio ~ mcc_overall+mhi+perc_pov+popden17 + (1 | region),
#                      HNHW_ratio ~ mcc_overall+mhi+perc_pov+popden17 + (1 | region))
# 
# # Fit the models
# mortality_r <- sapply(formulas_mixed, function(f) as.character(f[[2]]))
# models_mixed <- lapply(formulas_mixed, function(formula) lme4::lmer(formula, data = msa_mcc_mrr))
# 
# # Extract and tidy model results
# model_results_mixed <- models_mixed |>
#   map_df(tidy, .id = "model") |>
#   filter(term=="mcc_overall") |>
#   mutate(dependent_variable = mortality_r,
#          model_v= "mixed") 
# print(model_results_mixed)
# 
# 
# summary(models_mixed[[1]]); confint(models_mixed[[1]]); 
# anova(models_mixed[[1]])


# MR, covariates and random effects
lm1 <- lmer(ageadj_NHWhite ~ mcc_overall+mhi+perc_pov+popden17 + (1 | region),
            data = msa_mcc_mrr)
performance::icc(lm1)
summary(lm1) 
confint(lm1) 

lm2 <- lmer(ageadj_Black~mcc_overall+mhi+perc_pov+popden17+ (1 | region),
            data = msa_mcc_mrr)
performance::icc(lm2)
summary(lm2) 

lm3 <- lmer(ageadj_NHBlack~mcc_overall+mhi+perc_pov+popden17+ (1 | region),
            data = msa_mcc_mrr)
performance::icc(lm3)
summary(lm3) 

lm4 <- lmer(ageadj_Hispanic~mcc_overall+mhi+perc_pov+popden17+ (1 | region),
            data = msa_mcc_mrr)
performance::icc(lm4)
summary(lm4) 


# MRR, covariates and random effects

lm1 <- lmer(BNHW_ratio~mcc_overall+mhi+perc_pov+popden17+ (1 | region),
            data = msa_mcc_mrr)
summary(lm1) 
confint(lm1) 

lm2 <- lmer(NHBNHW_ratio~mcc_overall+mhi+perc_pov+popden17+ (1 | region),
            data = msa_mcc_mrr)
summary(lm2) 
confint(lm2) 

lm3 <- lmer(HNHW_ratio~mcc_overall+mhi+perc_pov+popden17+ (1 | region),
            data = msa_mcc_mrr)
summary(lm3) 
confint(lm3) 



# regressions w/ partial MSA ERI -------------------------------------------------------------

## read MSA mortality rate ratios
MSA_Mortality_CDCPOP_201519_covar <- read_dta("MRR/MSA_Mortality_CDCPOP_201519_covar.dta")|>
  rename(cbsa = CBSAFP,
         cbsa_name = cbsa)|>
  mutate(NHBNHW_ratio = ageadj_NHBlack / ageadj_NHWhite)
str(MSA_Mortality_CDCPOP_201519_covar)

# There are several variables starting with n_*. The variable numcounty tells
# you how many counties are within a specific MSA. The variable n_NHBlack tells
# how many counties are included for calculating the adjusted rate for NHBlack.
# By dividing n_NHBlack by numcounty, we could know how much % of counties are
# included for the calculation. (a difference can be calculated for similar
# purpose)


### total population data seems to be five-year average but
### the race and ethnicity pop seem to be five year total.

# summarize pct county and pop in calculating rates
pct_summary <- MSA_Mortality_CDCPOP_201519_covar |>
  dplyr::select(cbsa_code, starts_with("n_"), starts_with("pop"), numcounty, totpop17) |>
  janitor::clean_names() |>
  mutate(
    pct_cnty_non_white = n_non_white / numcounty,
    pct_cnty_nhb = n_nh_black / numcounty,
    pct_cnty_b = n_black / numcounty,
    pct_cnty_hispanic = n_hispanic / numcounty,
    pct_pop_non_white = pop_non_white / totpop17,
    pct_pop_black = pop_black / totpop17,
    pct_pop_nh_black = pop_nh_black / totpop17,
    pct_pop_hispanic = pop_hispanic / totpop17)

# save partial MSA county number and population summaries
saveRDS(pct_summary, "~/Library/CloudStorage/OneDrive-SharedLibraries-mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/MRR/partmsa_pct_summary.rds")

# read partial MSA MCC values
partmsa_mcc <- readRDS("~/Library/CloudStorage/OneDrive-SharedLibraries-mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Exploratory analysis/pca missing values/partmsa_mcc.rds")
str(partmsa_mcc)

# merge two files
partmsa_mcc_mrr <- partmsa_mcc |>
  left_join(MSA_Mortality_CDCPOP_201519_covar)
str(partmsa_mcc_mrr)
saveRDS(partmsa_mcc_mrr, "MRR/partmsa_mcc_mrr.rds")

## simple regression -------

summary(lm(ageadj_NHWhite~mcc_nhw, data = partmsa_mcc_mrr))
summary(lm(ageadj_Black~mcc_b, data = partmsa_mcc_mrr))
summary(lm(ageadj_NHBlack~mcc_nhb, data = partmsa_mcc_mrr))
summary(lm(ageadj_Hispanic~mcc_hisp, data = partmsa_mcc_mrr))

summary(lm(BNHW_ratio~mcc_b, data = partmsa_mcc_mrr))
summary(lm(NHBNHW_ratio~mcc_nhb, data = partmsa_mcc_mrr))
summary(lm(HNHW_ratio~mcc_hisp, data = partmsa_mcc_mrr))




# End -----



# restrict to MSAs where over 80% black population included in mortality rates
summary(lm(ageadj_NHWhite ~ mcc_b, data = partmsa_mcc_mrr |> filter(pct_pop_black >=0.8)))
summary(lm(ageadj_Black ~ mcc_b, data = partmsa_mcc_mrr |> filter(pct_pop_black >=0.8)))
summary(lm(BNHW_ratio ~ mcc_b, data = partmsa_mcc_mrr |> filter(pct_pop_black >=0.8)))

# restrict to MSAs where over 30 deaths are included for calculating black mortality rates
summary(lm(ageadj_NHWhite ~ mcc_b, data = partmsa_mcc_mrr |> filter(deaths_Black >=30)))
summary(lm(ageadj_Black ~ mcc_b, data = partmsa_mcc_mrr |> filter(deaths_Black >=30)))
summary(lm(BNHW_ratio ~ mcc_b, data = partmsa_mcc_mrr |> filter(deaths_Black >=30)))



# restrict to mcc values with >=30 tracts
lm1 <- lm(BNHW_ratio~mcc, data = partmsa_mcc_mrr |>
            filter(tract_n >= 30))
summary(lm1)

# restrict to mcc values with >=30 tracts and 100% counties
lm1 <- lm(BNHW_ratio~mcc, data = partmsa_mcc_mrr |>
            filter(tract_n >= 30 & pct_county_bnhw == 1))
summary(lm1)





# County ------------------------------------------------------------------

# read top 250 county mortality rates
county250_Mortality_201519 <- read_csv("~/Library/CloudStorage/OneDrive-SharedLibraries-mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/MRR/county250_Mortality_201519.csv") |>
  rename(GEOID=countyfp)
names(county250_Mortality_201519)

# read county mcc values, n=456
county_mcc <- readRDS("~/Library/CloudStorage/OneDrive-SharedLibraries-mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Exploratory analysis/pca missing values/county_mcc.rds")
names(county_mcc)

# merge county mortality rates and mcc values
county_df <- county250_Mortality_201519 |>
  left_join(county_mcc)


library(broom)
library(dplyr)
library(purrr)

# List of formulas for the models
formulas <- list(ageadj_NHWhite~mcc,
                 ageadj_Black~mcc,
                 ageadj_NonWhite~mcc,
                 ageadj_NHBlack~mcc,
                 ageadj_Hispanic~mcc,
                 
                 crude_NHWhite~mcc,
                 crude_Black~mcc,
                 crude_NonWhite~mcc,
                 crude_NHBlack~mcc,
                 crude_Hispanic~mcc,
                 
                 NonWNHW_ratio~mcc,
                 BNHW_ratio~mcc,
                 HNHW_ratio~mcc,
                 HNHB_ratio~mcc)

# Fit the models
mortality_r <- sapply(formulas, function(f) as.character(f[[2]]))
models <- lapply(formulas, function(formula) lm(formula, data = county_df))

# Extract and tidy model results
model_results <- models |>
  map_df(tidy, .id = "model") |>
  filter(term=="mcc") |>
  mutate(dependent_variable = mortality_r)
print(model_results)

# only age adjusted mortality rates of non-white and non-hispanic black are significantly
# associated with mcc at the county level.
# mcc is positively associated with BNHW_ratio.



# New mortality rates -----------------------------------------------------

# read partial msa mrr values
partmsa_pct_summary <- readRDS(
  "~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/MRR/msa_mrr_pctsumm.rds"
) |>
  rename(cbsa_code = CBSAFP) |>
  mutate(NHBNHW_ratio = ageadj_NHBlack / ageadj_NHWhite)

names(partmsa_pct_summary)

# read full cbsa mcc values
msa_mcc_merged <- readRDS("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Exploratory analysis/pca missing values/msa_mcc_merged.rds") |>
  filter(metro_micro_type == "Metropolitan Statistical Area") |>
  mutate(cbsa_code = as.character(cbsa_code))

names(msa_mcc_merged)

# merge full msa mcc and partial msa mrr data
msa_mcc_partmrr <- msa_mcc_merged |>
  left_join(partmsa_pct_summary)
str(msa_mcc_partmrr)

# scatterplots btw full msa mcc and partial msa mrr
ggplot(msa_mcc_partmrr, aes(x=mcc_overall, y=ageadj_NHWhite)) +
  geom_point(size =1) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(x="ERI", y="age-adjusted mortality rate", title = "(a) Non-Hispanic White")+
  theme_bw() -> p1

ggplot(msa_mcc_partmrr, aes(x=mcc_overall, y=ageadj_Black)) +
  geom_point(size =1) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(x="ERI", y="age-adjusted mortality rate", title = "(b) Black")+
  theme_bw() -> p2

ggplot(msa_mcc_partmrr, aes(x=mcc_overall, y=ageadj_Hispanic)) +
  geom_point(size =1) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(x="ERI", y="age-adjusted mortality rate", title = "(c) Hispanic")+
  theme_bw() -> p3

p1|p2|p3
ggsave("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/MRR/graphs/fullmsa_mcc_partmsa_mr.pdf", width = 10, height = 6)


# scatterplots btw full msa mcc and partial msa mrr
ggplot(msa_mcc_partmrr, aes(x=mcc_overall, y=BNHW_ratio)) +
  geom_point(size =1) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(x="ERI", y="MRR", title = "(a) Black vs. Non-Hispanic White")+
  theme_bw() -> r1

ggplot(msa_mcc_partmrr, aes(x=mcc_overall, y=HNHW_ratio)) +
  geom_point(size =1) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(x="ERI", y="MRR", title = "(b) Hispanic vs. Non-Hispanic White")+
  theme_bw() -> r3

r1|r3
ggsave("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/MRR/graphs/fullmsa_mcc_partmsa_mrr.pdf", width = 10, height = 6)


# simple regression
summary(lm(ageadj_NHWhite~mcc_overall, data = msa_mcc_partmrr))
# summary(lm(ageadj_NHBlack~mcc_overall, data = msa_mcc_partmrr))
summary(lm(ageadj_Black~mcc_overall, data = msa_mcc_partmrr))
summary(lm(ageadj_Hispanic~mcc_overall, data = msa_mcc_partmrr))

summary(lm(BNHW_ratio~mcc_overall, data = msa_mcc_partmrr))
# summary(lm(NHBNHW_ratio~mcc_overall, data = msa_mcc_partmrr))
summary(lm(HNHW_ratio~mcc_overall, data = msa_mcc_partmrr))


# restrict to MSAs where over 80% black population included in mortality rates
summary(lm(ageadj_NHWhite ~ mcc_overall, data = msa_mcc_partmrr |> filter(pct_pop_b >=0.8)))
summary(lm(ageadj_Black ~ mcc_overall, data = msa_mcc_partmrr |> filter(pct_pop_b >=0.8)))
summary(lm(BNHW_ratio ~ mcc_overall, data = msa_mcc_partmrr |> filter(pct_pop_b >=0.8)))


# restrict to MSAs where over 30 deaths are included for calculating black mortality rates
summary(lm(ageadj_NHWhite ~ mcc_overall, data = msa_mcc_partmrr |> filter(deaths_Black >=30)))
summary(lm(ageadj_Black ~ mcc_overall, data = msa_mcc_partmrr |> filter(deaths_Black >=30)))
summary(lm(BNHW_ratio ~ mcc_overall, data = msa_mcc_partmrr |> filter(deaths_Black >=30)))



## regressions based on partial msa mcc values ------

# read partial msa mcc based on BNHW MRR
partmsa_bnhw_mcc <- readRDS("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/Exploratory analysis/pca missing values/partmsa_bnhw_mcc.rds")
names(partmsa_bnhw_mcc) # n=373

msa_mrr_pctsumm <- readRDS("~/mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/MRR/msa_mrr_pctsumm.rds") |>
  rename(cbsa_code = CBSAFP)
names(msa_mrr_pctsumm)

msa_mrr_pctsumm |>
  filter(!cbsa_code %in% partmsa_bnhw_mcc$cbsa_code) -> t # n=5, these 5 MSAs do not have NHB mortality data

# merge partial MSA mcc and mrr measures
msa_partmcc_partmrr <- partmsa_bnhw_mcc |>
  left_join(msa_mrr_pctsumm)
str(msa_partmcc_partmrr)


# List of formulas for the models
formulas <- list(ageadj_NHWhite~mcc_bnhw,
                 ageadj_Black~mcc_bnhw,
                 BNHW_ratio~mcc_bnhw)

# Fit the models
mortality_r <- sapply(formulas, function(f) as.character(f[[2]]))
models <- lapply(formulas, function(formula) lm(formula, data = msa_partmcc_partmrr))

# Extract and tidy model results
model_results <- models |>
  map_df(broom::tidy, .id = "model") |>
  filter(term=="mcc_bnhw") |>
  mutate(dependent_variable = mortality_r,
         pct_black = "all")
print(model_results)


### restrict to MSAs where over 80% of black population are included in calculate BNHW ratio
models2 <- lapply(formulas, function(formula) lm(formula, data = msa_partmcc_partmrr |> filter(pct_pop_b >= 0.8)))

# Extract and tidy model results
model_results2 <- models2 |>
  map_df(broom::tidy, .id = "model") |>
  filter(term=="mcc_bnhw") |>
  mutate(dependent_variable = mortality_r,
         pct_black = ">=0.8")
print(model_results2)


### restrict to MSAs where over 30 black deaths are included in calculate BNHW ratio
models3 <- lapply(formulas, function(formula) lm(formula, data = msa_partmcc_partmrr |> filter(deaths_Black >= 30)))

# Extract and tidy model results
model_results3 <- models3 |>
  map_df(broom::tidy, .id = "model") |>
  filter(term=="mcc_bnhw") |>
  mutate(dependent_variable = mortality_r,
         deaths_Black = ">=30")
print(model_results3)

# combine
bnhw_ratio_results <- bind_rows(model_results, model_results2)


# restrict to mcc values with >=30 tracts
lm1 <- lm(BNHW_ratio~mcc, data = msa_mcc_partmrr |>
            filter(tract_n >= 30))
summary(lm1)

# restrict to mcc values with >=30 tracts and 100% counties
lm1 <- lm(BNHW_ratio~mcc, data = msa_mcc_partmrr |>
            filter(tract_n >= 30 & pct_county_bnhw == 1))
summary(lm1)


tt <- partmsa_pct_summary |>
  select(cbsa, deaths_Black, pop_Black, ageadj_Black, Black, pct_countyn_b, pct_pop_b) |>
  arrange(deaths_Black)
