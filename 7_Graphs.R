# Description: This script contains code for creating all main plots of the Environmental Racism Index (ERI) paper
# Author: Wei Xu
# Organization: Medical College of Wisconsin
# Date: August, 2025


library(ggcorrplot)
library(tidyverse)
library(readr)
library(tidycensus)
library(ggcorrplot)
library(stringr)
library(viridis)
library(tigris)
library(sf)
library(Hmisc)
library(nFactors)
library(psych)
library(skimr)
library(patchwork)

options(scipen=999)

setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-mcw.edu/Structural Racism Geometrics - Environmental Racism Metrics - Environmental Racism Metrics/EnvironmentalRacismMetrics")

# Prepare data files --------

# read tract EEI, PMR, MDI values
df_merged <- read_csv("Exploratory analysis/pca missing values/tract_env_race_redline.csv")
names(df_merged)

# load state, cbsa, and county sf files
load("Shapefiles/st_cbsa_ct_sf.RData")

# Merge tract sf file with tract variables 
tract_env_sf <- tract_cont_sf |> 
  left_join(df_merged) |> 
  rename(
    EEI = environment_s,
    MDI = Redline,
    PMR = pct_nnhwhite
  ) |> 
  mutate(statefips = as.numeric(statefips)) |> 
  left_join(crosswalkr::stcrosswalk |> 
              rename(statefips = stfips)) 
names(tract_env_sf)

# save the tract sf file
saveRDS(tract_env_sf, "Exploratory analysis/pca missing values/tract_env_sf_082025.rds")



# Figure1, tract EEI, PMR, and MDI for 4 select MSAs -----

# read tract sf file
tract_env_sf <- readRDS("Exploratory analysis/pca missing values/tract_env_sf_082025.rds")
names(tract_env_sf)

# list largest MSAs in each census region
msa_list <- c("35620", # New York-Newark-Jersey City, NY-NJ-PA
              "16980", # Chicago-Naperville-Elgin, IL-IN-WI
              "19100",  # Dallas-Fort Worth-Arlington, TX
              "31080" # Los Angeles-Long Beach-Anaheim, CA
)

# inspect the range of MDI value of these MSAs
tract_env_sf |> 
  filter(cbsa_code %in% msa_list) |> 
  group_by(cbsa_code) |> 
  summarise(min_mdi = min(MDI, na.rm = TRUE),
            max_mdi = max(MDI, na.rm = TRUE))


# create a function for plotting MSA EEI, PMR, and MDI
# used quintiles for more visual variation
plot_msa <- function(msa_code, fill_variable) {
  
  # subset MSA data
  msa_tract <- tract_env_sf |> 
    filter(cbsa_code == msa_code) 
  
  # get MSA name for subtitle
  msa_name <- msa_tract$cbsa_title[1]
  
  msa_tract <- msa_tract |>
    mutate(
      # create a new variable for 10 categories using ntile function (equal frequency)
      var_cat = ntile(!!sym(fill_variable), 10),
      var_cat = factor(
        var_cat,
        levels = 1:10,
        labels = c("1 (lowest)", "2", "3", "4", "5", "6", "7", "8", "9", "10 (highest)")
      )
    )
  
  # plotting MSA tracts with categories and remove NA tracts
  ggplot() +
    geom_sf(data = msa_tract |> 
              filter(!is.na(var_cat)), aes(fill = var_cat), color = NA) +
    coord_sf(datum = NA) +
    scale_fill_brewer(palette = "Spectral", direction = -1)+
    theme_bw() +
    labs(subtitle = msa_name, fill=fill_variable) +
    theme(plot.subtitle = element_text(size = 9, face = "bold"))
}


# plot all four MSAs
plot_all_msas <- function(fill_variable) {
  # create a list of plots for each MSA
  plot_list <- lapply(msa_list, function(msa_code) {
    plot_msa(msa_code, fill_variable)
  })
  
  # combine all plots into one
  combined_plot <- wrap_plots(plotlist = plot_list, ncol = 4)
  
  # combine all plots into one with shared legend
  combined_plot <- combined_plot +
    plot_layout(guides = "collect") &
    theme(legend.position = "right")
  
  return(combined_plot)
}

# combine EEI, PMR, and MDI plots
plot_all_msas("EEI")/
  plot_all_msas("PMR")/
  plot_all_msas("MDI")

# save the combined plot
ggsave("Exploratory analysis/pca missing values/graphs/fig1_4msas_equal_n.pdf", width = 19, height = 13)




# Figure 2, state ERI ------

# local domain specific and overall ERI values
state_mcc_merged <- readRDS("Exploratory analysis/pca missing values/state_mcc_merged.rds")

st_cont_sf |> 
  filter(!GEOID %in% c("02", "15", "72")) |> 
  left_join(state_mcc_merged) |> 
  sf::st_transform(crs = 5070) -> state_sf_mcc

ggplot(state_sf_mcc, aes(fill = mcc_pollution)) +
  geom_sf() + theme_bw() + labs(title = "(a) Air pollution") +
  scale_fill_distiller(palette = "Spectral", name = "ERI") -> p1
ggplot(state_sf_mcc, aes(fill = mcc_housing)) +
  geom_sf() + theme_bw() + labs(title = "(b) Housing") +
  scale_fill_distiller(palette = "Spectral", name = "ERI") -> p2
ggplot(state_sf_mcc, aes(fill = mcc_builtenv)) +
  geom_sf() + theme_bw() + labs(title = "(c) Built & natural environment") +
  scale_fill_distiller(palette = "Spectral", name = "ERI") -> p3
ggplot(state_sf_mcc, aes(fill = mcc_hazard)) +
  geom_sf() + theme_bw() + labs(title = "(d) Hazards") +
  scale_fill_distiller(palette = "Spectral", name = "ERI") -> p4
ggplot(state_sf_mcc, aes(fill = mcc_climate)) +
  geom_sf() + theme_bw() + labs(title = "(e) Climate") +
  scale_fill_distiller(palette = "Spectral", name = "ERI") -> p5
ggplot(state_sf_mcc, aes(fill = mcc)) +
  geom_sf() + theme_bw() + labs(title = "(f) Overall environment") +
  scale_fill_distiller(palette = "Spectral", name = "ERI") -> p6

# combine plots
ggpubr::ggarrange(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2, 
                  common.legend = TRUE, legend="bottom")

# save combined plots
ggsave("Exploratory analysis/pca missing values/graphs/fig2_state_mcc_6fig.pdf", width = 10, height = 11)





# Figure 3, MSA ERI ------

# read MSA ERI values
msa_mcc_merged <- readRDS("Exploratory analysis/pca missing values/cbsa_mcc_merged.rds") |> 
  filter(metro_micro_type == "Metropolitan Statistical Area")

# read CBSA sf file
msa_mcc_sf <- cbsa_cont_sf |> 
  right_join(msa_mcc_merged) |> 
  st_transform(crs = 5070)

## plotting MSA overall and domain-specific ERI values
ggplot() +
  geom_sf(msa_mcc_sf, mapping=aes(fill = mcc_pollution), color= "grey", size = 0.1)+
  geom_sf(data=st_cont_sf, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = "Spectral", name="ERI")+
  labs(title = "(a) Air pollution")+
  theme(legend.position = "bottom") -> p1
ggplot() +
  geom_sf(msa_mcc_sf, mapping=aes(fill = mcc_housing), color= "grey", size = 0.1)+
  geom_sf(data=st_cont_sf, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = "Spectral", name="ERI")+
  labs(title = "(b) Housing")+
  theme(legend.position = "bottom") -> p2
ggplot() +
  geom_sf(msa_mcc_sf, mapping=aes(fill = mcc_builtenv), color= "grey", size = 0.1)+
  geom_sf(data=st_cont_sf, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = "Spectral", name="ERI")+
  labs(title = "(c) Built & natural environment")+
  theme(legend.position = "bottom") -> p3
ggplot() +
  geom_sf(msa_mcc_sf, mapping=aes(fill = mcc_hazard), color= "grey", size = 0.1)+
  geom_sf(data=st_cont_sf, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = "Spectral", name="ERI")+
  labs(title = "(d) Hazards")+
  theme(legend.position = "bottom") -> p4
ggplot() +
  geom_sf(msa_mcc_sf, mapping=aes(fill = mcc_climate), color= "grey", size = 0.1)+
  geom_sf(data=st_cont_sf, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = "Spectral", name="ERI")+
  labs(title = "(e) Climate")+
  theme(legend.position = "bottom") -> p5
ggplot() +
  geom_sf(msa_mcc_sf, mapping=aes(fill = mcc_overall), color= "grey", size = 0.1)+
  geom_sf(data=st_cont_sf, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = "Spectral", name="ERI")+
  labs(title = "(f) Overall environment")+
  theme(legend.position = "bottom") -> p6

# combine plots
ggpubr::ggarrange(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2, 
                  common.legend = TRUE, legend="bottom")

# save combined plots
ggsave("Exploratory analysis/pca missing values/graphs/fig3_metro_mcc_6fig.pdf", width = 10, height = 11)





# Figure 4, correlation matrix plot of state and metro EEI, PMR, MDI, overall and domain specific MRI ---------------------

#### state correlation matrix 
state_mcc_eei_pmr_mdi <- read_csv("Exploratory analysis/pca missing values/Appendix 4_state_mcc_eei_pmr_mdi.csv")[-1]
corr_state <- round(cor(state_mcc_eei_pmr_mdi |> 
                          select(-GEOID, -state, -state_name) |> 
                          relocate(ERI, .before = ERI_builtenv)), 3)
head(corr_state)

# p values
p.mat <- ggcorrplot::cor_pmat(state_mcc_eei_pmr_mdi|> 
                                select(-GEOID, -state, -state_name) |> 
                                relocate(ERI, .before = ERI_builtenv))
p.mat

# correlation matrix plot
corr_state_plot <- ggcorrplot::ggcorrplot(corr_state, hc.order = F, type = "lower",
                                          p.mat = p.mat, insig = "blank", lab = TRUE) 
corr_state_plot


### metro correlation matrix
metro_mcc_eei_pmr_mdi <- read_csv("Exploratory analysis/pca missing values/metro_mcc_eei_pmr_mdi.csv")[-1]
names(metro_mcc_eei_pmr_mdi)

metro_mcc_eei_pmr_mdi_simp <- metro_mcc_eei_pmr_mdi |>
  select(ERI, ERI_builtenv, ERI_climate, ERI_hazard, ERI_housing, ERI_pollution,
         EEI, EEI_builtenv, EEI_climate, EEI_hazard, EEI_housing, EEI_pollution,
         PMR, MDI)

corr_metro <- round(cor(metro_mcc_eei_pmr_mdi_simp), 3)
corr_metro

p.mat.metro <- ggcorrplot::cor_pmat(metro_mcc_eei_pmr_mdi_simp)
p.mat.metro

# combine state and metro correlation matrix plot
corr_metro_plot<- ggcorrplot::ggcorrplot(corr_metro, hc.order = F, type = "lower",
                                         p.mat=p.mat.metro, insig = "blank", lab = TRUE) 

# combine state and MSA correlation matrices
ggpubr::ggarrange(corr_state_plot, corr_metro_plot, 
                  ncol = 2, nrow = 1, 
                  labels = c("(a) State ", "(b) Metropolitan Statistical Area"))

# save combined graphs
ggsave("Exploratory analysis/pca missing values/graphs/fig4_corr_state_metro.pdf", width = 18, height = 10)

