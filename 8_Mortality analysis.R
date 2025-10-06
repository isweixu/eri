## Description: This R script contains the code for MSA-level mortality
## analyses. based on full MSA ERI metrics as well as sensitivity analyses based
## on partial MSA ERI metrics. 
## Author: Wei Xu 
## Organization: Medical College of Wisconsin 
## Date: September, 2025


library(broom)
library(tidyverse)
library(readr)
library(viridis)
library(tigris)
library(haven)
library(psych)
library(skimr)
library(patchwork)
library(lme4)


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

# merge CBSA ERI  with mortality, keep only MSA rows
msa_mcc_mrr <- cbsa_mcc_merged |>
  filter(metro_micro_type == "Metropolitan Statistical Area") |>
  left_join(MSA_Mortality_CDCPOP_201519_covar)

# insepct missingness with mortality rates
nrow(msa_mcc_mrr) # n=379
nrow(msa_mcc_mrr |> filter(is.na(ageadj_Black))) # n=6
nrow(msa_mcc_mrr |> filter(is.na(ageadj_Hispanic))) # n=9

# inspect CBSA code mismatch
MSA_Mortality_CDCPOP_201519_covar |> 
  filter(!cbsa_code %in% msa_mcc_mrr$cbsa_code) -> msa_mismatch
# Mortality file has two MSAs in AK and two MSAs in HI.
# These four MSAs were removed for subsequent analyses.


# Regressions w/ full MSA ERI -----

## Simple regression -------

# List of formulas for the models
formulas <- list(ageadj_NHWhite~mcc_overall,
                 ageadj_Black~mcc_overall,
                 ageadj_NHBlack~mcc_overall,
                 ageadj_Hispanic~mcc_overall,
                 
                 BNHW_ratio~mcc_overall,
                 NHBNHW_ratio~mcc_overall,
                 HNHW_ratio~mcc_overall)

# Fit the models
mortality_r <- sapply(formulas_adj, function(f) as.character(f[[2]])) # mortality outcomes
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



## Multiple regressions -------

### models with MSA covariates --------

formulas_adj <- list(ageadj_NHWhite~mcc_overall+MedianHHInc+perc_pov+popden17,
                 ageadj_Black~mcc_overall+MedianHHInc+perc_pov+popden17,
                 ageadj_NHBlack~mcc_overall+MedianHHInc+perc_pov+popden17,
                 ageadj_Hispanic~mcc_overall+MedianHHInc+perc_pov+popden17,
                 
                 BNHW_ratio~mcc_overall+MedianHHInc+perc_pov+popden17,
                 NHBNHW_ratio~mcc_overall+MedianHHInc+perc_pov+popden17,
                 HNHW_ratio~mcc_overall+MedianHHInc+perc_pov+popden17)

# Fit the models
mortality_r <- sapply(formulas_adj, function(f) as.character(f[[2]])) # mortality outcomes
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



# Regressions w/ partial MSA ERI -------------------------------------------------------------

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

# merge mortality and partial MSA ERI files
partmsa_mcc_mrr <- partmsa_mcc |>
  left_join(MSA_Mortality_CDCPOP_201519_covar)
str(partmsa_mcc_mrr)

# save merged data
saveRDS(partmsa_mcc_mrr, "MRR/partmsa_mcc_mrr.rds")



## Simple regression -------

partmsa_mcc_mrr <- readRDS("~/Library/CloudStorage/OneDrive-SharedLibraries-mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics/MRR/partmsa_mcc_mrr.rds")

names(partmsa_mcc_mrr)

summary(lm(ageadj_NHWhite~mcc_nhw, data = partmsa_mcc_mrr))
summary(lm(ageadj_Black~mcc_b, data = partmsa_mcc_mrr))
summary(lm(ageadj_NHBlack~mcc_nhb, data = partmsa_mcc_mrr))
summary(lm(ageadj_Hispanic~mcc_hisp, data = partmsa_mcc_mrr))

summary(lm(BNHW_ratio~mcc_b, data = partmsa_mcc_mrr))
summary(lm(NHBNHW_ratio~mcc_nhb, data = partmsa_mcc_mrr))
summary(lm(HNHW_ratio~mcc_hisp, data = partmsa_mcc_mrr))



## Multiple regressions -------

### models with MSA covariates --------

formulas_adj <- list(ageadj_NHWhite~mcc_nhw+MedianHHInc+perc_pov+popden17,
                     ageadj_Black~mcc_b+MedianHHInc+perc_pov+popden17,
                     ageadj_NHBlack~mcc_nhb+MedianHHInc+perc_pov+popden17,
                     ageadj_Hispanic~mcc_hisp+MedianHHInc+perc_pov+popden17,
                     
                     BNHW_ratio~mcc_b+MedianHHInc+perc_pov+popden17,
                     NHBNHW_ratio~mcc_nhb+MedianHHInc+perc_pov+popden17,
                     HNHW_ratio~mcc_hisp+MedianHHInc+perc_pov+popden17)

# Fit the models
mortality_r <- sapply(formulas_adj, function(f) as.character(f[[2]])) # mortality outcomes
models_adj <- lapply(formulas_adj, function(formula) lm(formula, data = partmsa_mcc_mrr))

# Extract and tidy model results
model_results_adj <- models_adj |>
  map_df(tidy, .id = "model") |>
  mutate(dependent_variable = rep(mortality_r, each=5),
         model_name= "adjusted") 
print(model_results_adj)
