# Explore the impact of covariate transformations on ATE and CATE
# Program 1: create a subset of numom dataset expolorations

# Data stores in the shared folder DRLearner-variable-transformation
# Created by: Ya-Hui Yu (03-05-2025)

# Load required packages--------------------------------------------------------
pacman::p_load(
  rio,
  here,
  skimr,
  tidyverse,
  lmtest,
  sandwich,
  broom,
  SuperLearner,
  ggplot2,
  haven,
  fastDummies,
  glmnet
)

remotes::install_github("tlverse/tlverse")
library(tmle3)
library (sl3)

# Load original dataset (N = 8259)----------------------------------------------

# Version 2 dataset (n = 8259) : include newly created WWEIA variables
data <- read_dta(here("data","numom_diet_foodEM_v2.dta"))
var.labels <- lapply(data, attr, "label")

# Select food groups for this project-------------------------------------------

food_group <- data %>%
  select(matches("^d_gr_wweia_sub_|^d_gr_wweia_subx_"))

selected_food <- skim(food_group) %>%
  arrange(desc(numeric.sd)) %>%
  slice(1:17) %>%
  pull(skim_variable) %>%
  setdiff(c("d_gr_wweia_subx_greenveg","d_gr_wweia_subx_nongreenveg"))

# Select  covariates for this project-------------------------------------------
# Covariates [Follow the steps in the diet heterogeneity paper]
# Renaming the physical activity variables (these are the new versions)
# so they match the rest of the code
data <- data %>% rename(pa_totmetwk_v1 = pa_totmetwk_new_v1)
data <- data %>% rename(pa_totmetwk3_v1 = pa_totmetwk3_new_v1)
data <- data %>% rename(pa_totmetwk4_v1 = pa_totmetwk4_new_v1)

data$eod <- (recode(data$eod_situation, `0` = 0, `1` = 1, `2` = 2, `3` = 3,
                    `4` = 4, `5` = 5, `6` = 5, `7` = 5, `8` =5, `9` = 5))

data$momeduc <- recode(as.factor(data$momeduc), `1` = 1, `2` = 1, `3` = 1,
                       `4` = 2, `5` = 2, `6` = 3, `7` = 4, `8` = 4)

data$clust2_new <- recode(data$clust4, `1` = 0, `3` = 0, `4` =0, `2` = 1)

# Short list of variables for this analysis
covariates <- c("insurpub", "momrace4",  "momeduc", "married","publicsite_num",
                "momage",  "pa_totmetwk_v1", "bmiprepreg", "puqe_tot",
                "clust2_new")

outcomes <- c("negoutcome")

# 8259 obs, 26 vars:
data_a <- data %>%
  dplyr::select(all_of(outcomes), all_of(covariates),all_of(selected_food))

# this file is uploaded to OneDrive folder
write_csv(data_a, here("numom_var-transformation.csv"))
