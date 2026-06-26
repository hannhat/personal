
### Setup -------

## Load packages -----
library(here)
library(dplyr)
library(ggplot2)
library(stats)

## Set working directory -----
setwd(here::here())

## Load data -----
data_cca <- readRDS("data/build/social_mobility_complete.rds")

## Source functions -----
source("code/functions/VR_interventional_effects.R")


### Descriptive -------

## Origin-Destination distributions -----

# Total
origin_destination_odds <- data_cca %>% 
  group_by(fclass_str) %>%
   mutate(count = n()) %>%
  ungroup() %>%
  group_by(fclass_str, class_str, count) %>%
  summarize(n = n()) %>%
  mutate(odds = n / count) %>%
  select(fclass_str, class_str, count, odds) %>%
  pivot_wider(names_from = "class_str", values_from = "odds")

# By sex + cohort
origin_destination_odds_sc <- data_cca %>% 
  group_by(fclass, sex, cohort) %>%
  mutate(frequency = n()) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(fclass_str, class_str, sex, cohort, count) %>%
  summarize(n = n()) %>%
  mutate(odds = n / count) %>%
  select(fclass_str, class_str, count, odds, sex, cohort) %>%
  pivot_wider(names_from = "class_str", values_from = "odds") %>%
  replace(is.na(.), 0)

## Education-Destination distributions -----

# Marginal, by sex + cohort
educ_destination_odds_sc <- data_cca %>% 
  group_by(educ, sex, cohort) %>%
  mutate(frequency = n()) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(educ, class_str, sex, cohort, count) %>%
  summarize(n = n()) %>%
  mutate(odds = n / count) %>%
  select(educ, class_str, count, odds, sex, cohort) %>%
  pivot_wider(names_from = "class_str", values_from = "odds") %>%
  replace(is.na(.), 0)

# Conditional on father's class, by sex + cohort
educ_dest_by_fclass_odds_sc <- data_cca %>% 
  group_by(fclass, educ, sex, cohort) %>%
  mutate(frequency = n()) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(fclass, educ, class_str, sex, cohort, count) %>%
  summarize(n = n()) %>%
  mutate(odds = n / count) %>%
  select(fclass, educ, class_str, count, odds, sex, cohort) %>%
  pivot_wider(names_from = "class_str", values_from = "odds") %>%
  replace(is.na(.), 0)

# Conditional on father's class, total
educ_dest_by_fclass_odds <- data_cca %>% 
  group_by(fclass, educ) %>%
  mutate(frequency = n()) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(fclass, educ, class_str, count) %>%
  summarize(n = n()) %>%
  mutate(odds = n / count) %>%
  select(fclass, educ, class_str, count, odds) %>%
  pivot_wider(names_from = "class_str", values_from = "odds") %>%
  replace(is.na(.), 0)

## Total and indirect log-odds, 1970 men -----

total_odds <- origin_destination_odds_sc %>%
  filter(sex == 0, cohort == 1970) %>%
  select(-sex, -cohort)

educ_dest_odds <- data_cca %>%
  filter(sex == 0, cohort == 1970) %>%
  group_by(educ, class_str) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(educ) %>%
  mutate(prob = count / sum(count)) %>%
  select(-count) %>%
  pivot_wider(names_from = "class_str", values_from = "prob") %>%
  ungroup()
  
orig_educ_odds <- data_cca %>%
  filter(sex == 0, cohort == 1970) %>%
  group_by(educ, fclass_str) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(fclass_str) %>%
  mutate(prob = count / sum(count)) %>%
  select(-count) %>%
  pivot_wider(names_from = "educ", values_from = "prob") %>%
  ungroup()

orig_educ_odds[is.na(orig_educ_odds)] <- 0
educ_dest_odds[is.na(educ_dest_odds)] <- 0

### Single mediator: Education -------

## Continuous outcome: class (with classes 3-5 combined) -----

# Matrix of VR-effects, with rows representing the actual social class (1-5)
# and columns representing the social class the counterfactual conditional
# education distribution was set to
mat <- matrix(nrow = 5, ncol = 5)
for (i in 1:5)
  for (j in 1:5) {
    out <- vr_effect_main(monte_carlo = TRUE, 
                   mc_draws = 100000, 
                   B = 100, 
                   outcome = "class_mod", 
                   exposure = "fclass_mod", 
                   reference = i, 
                   counterfactual = j, 
                   mediators = "educ", 
                   controls = c("sex", "cohort"),
                   intervention = "single") 
    mat[i,j] <- out[1]
  }
educ_vr_effects_matrix_alt <- as.data.frame(mat)

### Two mediators: IQ + education -------

# Matrix of joint VR-effects
mat <- matrix(nrow = 5, ncol = 5)
for (i in 1:5)
  for (j in 1:5) {
    out <- vr_effect_main(monte_carlo = TRUE, 
                          mc_draws = 100000, 
                          B = 1, 
                          outcome = "class_mod", 
                          exposure = "fclass_mod", 
                          reference = i, 
                          counterfactual = j, 
                          mediators = c("IQ", "educ"), 
                          controls = c("sex", "cohort"),
                          intervention = "joint") 
    mat[i,j] <- out[1]
  }
joint_2vr_effects_matrix <- as.data.frame(mat)

# Matrix of education VR-effects
mat <- matrix(nrow = 5, ncol = 5)
for (i in 1:5)
  for (j in 1:5) {
    out <- vr_effect_main(monte_carlo = TRUE, 
                          mc_draws = 100000, 
                          B = 1, 
                          outcome = "class_mod", 
                          exposure = "fclass_mod", 
                          reference = i, 
                          counterfactual = j, 
                          mediators = c("IQ", "educ"), 
                          intervention = "first") 
    mat[i,j] <- out[1]
  }
iq_2vr_effects_matrix <- as.data.frame(mat)

# Matrix of IQ VR-effects
mat <- matrix(nrow = 5, ncol = 5)
for (i in 1:5)
  for (j in 1:5) {
    out <- vr_effect_main(monte_carlo = TRUE, 
                          mc_draws = 100000, 
                          B = 1, 
                          outcome = "class_mod", 
                          exposure = "fclass_mod", 
                          reference = i, 
                          counterfactual = j, 
                          mediators = c("IQ", "educ"), 
                          intervention = "second") 
    mat[i,j] <- out[1]
  }
educ_2vr_effects_matrix <- as.data.frame(mat)














