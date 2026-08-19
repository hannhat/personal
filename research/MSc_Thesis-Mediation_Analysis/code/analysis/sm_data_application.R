
### Setup -------

## Load packages -----
library(here)
library(dplyr)
library(ggplot2)
library(stats)

## Set working directory -----
setwd(here::here())

## Load and subset data -----
data_cca <- readRDS("data/build/social_mobility_complete.rds")
data_1970m <- data_cca %>% filter(sex == 0, cohort == "1970")

## Source functions -----
source("code/functions/mc_mediation_analysis.R")


##### Universal Model Parameters -----------

data <- data1970m

# Linear regression models
mediator_types <- c("continuous", "continuous")

# Monte Carlo, bootstrap parameters
mc_draws <- 1000000
bootstrap_reps <- 500
confint <- 0.95

# "Treated", "untreated" exposure levels
reference <- 5
counterfactual <- 1 

# ----------------------------------------------------------------------------

##### Binary effects -----------

te_model <- "class_bin ~ fclass" # Total effect model: simple linear regression
exposure <- "fclass" # Father's social class 1-5 (from original encoding: 3-5 combined, 6 -> 4, 7 -> 5)
mediators <- c("IQ", "educ")
outcome <- "class_bin"

# "Treated", "untreated" exposure levels
reference <- 7
counterfactual <- 1

outcome_type <- "binary"
data <- data %>% mutate(class_bin = ifelse(class == 1, 1, 0))

### Vanderweele-Robinson/Jackson-Vanderweele Approach -------

# Approach-specific parameters
estimand <- "ATU" # Specific to the interpretation of the VR-interventional effects
effect_type <- "VR"

## Mediator and outcome model specification -----

# Specification 1: Linear additive models, no interactions
outcome_model_1c <- "class_bin ~ fclass + IQ + educ"
mediator_models_1c <- c("IQ ~ fclass", "educ ~ fclass + IQ")

vr_model1 <- main(data, outcome_model_1c, outcome_type, mediator_models_1c, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  estimand, 
                  mc_draws, bootstrap_reps, confint)

# Specification 2: Fully interacted 
outcome_model_2c <- "class_mod_bin ~ fclass + IQ + educ + IQ*educ + IQ*fclass + educ*fclass + IQ*educ*fclass"
mediator_models_2c <- c("IQ ~ fclass", "educ ~ fclass + IQ + IQ*fclass")

vr_model2 <- main(data, outcome_model_2c, outcome_type, mediator_models_2c, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  estimand, 
                  mc_draws, bootstrap_reps, confint)

# ----------------------------------------------------------------------------

# Matrix of all possible VR-effects 

# Binary 
te_model <- "class_mod_bin ~ fclass_mod" # Total effect model: simple linear regression
exposure <- "fclass_mod" # Father's social class 1-5 (from original encoding: 3-5 combined, 6 -> 4, 7 -> 5)
mediators <- c("IQ", "educ")
outcome <- "class_mod_bin"

outcome_type <- "binary"

# Fully additive model, no interactions
outcome_model_1c <- "class_bin ~ fclass + IQ + educ"
mediator_models_1c <- c("IQ ~ fclass", "educ ~ fclass + IQ")

mc_draws <- 1000000
bootstrap_reps <- 100
te_matrix <- matrix(data = NA, nrow = 7, ncol = 7)
iie_m1_matrix <- matrix(data = NA, nrow = 7, ncol = 7)
iie_m2m_matrix <- matrix(data = NA, nrow = 7, ncol = 7)
iie_m2c_matrix <- matrix(data = NA, nrow = 7, ncol = 7)
iie_joint_matrix <- matrix(data = NA, nrow = 7, ncol = 7)
iie_m2m_matrix_prop <- matrix(data = NA, nrow = 7, ncol = 7)
iie_m2c_matrix_prop <- matrix(data = NA, nrow = 7, ncol = 7)

for (i in 1:7) {
  for (j in 1:7) {
    reference <- i
    counterfactual <- j
    vr_model <- main(data, outcome_model_1c, outcome_type, mediator_models_1c, 
                      mediator_types, te_model, exposure, mediators,
                      outcome, reference, counterfactual, effect_type, 
                      estimand, 
                      mc_draws, bootstrap_reps, confint)
    te_sig <- ""
    iie_m1_sig <- ""
    iie_m2m_sig <- ""
    iie_m2c_sig <- ""
    iie_joint_sig <- ""
    if ((vr_model[1,3] > 0 & vr_model[1,4] > 0) | 
        (vr_model[1,3] < 0 & vr_model[1,4] < 0)) {
      te_sig <- "*"
    } 
    if ((vr_model[2,3] > 0 & vr_model[2,4] > 0) | 
        (vr_model[2,3] < 0 & vr_model[2,4] < 0)) {
      iie_m1_sig <- "*"
    }
    if ((vr_model[3,3] > 0 & vr_model[3,4] > 0) | 
        (vr_model[3,3] < 0 & vr_model[3,4] < 0)) {
      iie_m2m_sig <- "*"
    }
    if ((vr_model[4,3] > 0 & vr_model[4,4] > 0) | 
        (vr_model[4,3] < 0 & vr_model[4,4] < 0)) {
      iie_m2c_sig <- "*"
    }
    if ((vr_model[5,3] > 0 & vr_model[5,4] > 0) | 
        (vr_model[5,3] < 0 & vr_model[5,4] < 0)) {
      iie_joint_sig <- "*"
    }
    
    te_matrix[i,j] <- paste0(vr_model[1,2], te_sig)
    iie_m1_matrix[i,j] <- paste0(vr_model[2,2],iie_m1_sig)
    iie_m2m_matrix[i,j] <- paste0(vr_model[3,2],iie_m2m_sig)
    iie_m2c_matrix[i,j] <- paste0(vr_model[4,2],iie_m2c_sig)
    iie_joint_matrix[i,j] <- paste0(vr_model[5,2],iie_joint_sig)
    
    iie_m2m_matrix_prop[i,j] <- paste0(vr_model[3,2]/vr_model[1,2], iie_m2m_sig)
    iie_m2c_matrix_prop[i,j] <- paste0(vr_model[4,2]/vr_model[1,2], iie_m2c_sig)
  }
}

# Regression and matrix objects extracted to LaTeX via the `xtable` package




# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# UNUSED FOR TABLES IN DISSERTATION
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

### Vansteelandt-Daniel Approach -------

# Approach-specific parameters
estimand <- "ATE"
effect_type <- "VD"

## Mediator and outcome model specification -----

# Specification 1: Linear additive models, no interactions
outcome_model_1b <- "class_bin ~ fclass + IQ + educ"
mediator_models_1b <- c("IQ ~ fclass", "educ ~ fclass + IQ")

vd_model1 <- main(data, outcome_model_1b, outcome_type, mediator_models_1b, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  estimand, 
                  mc_draws, bootstrap_reps, confint)

# Specification 2: Fully interacted
outcome_model_2b <- "class_bin ~ fclass + IQ + educ + IQ*educ + IQ*fclass + educ*fclass + IQ*educ*fclass"
mediator_models_2b <- c("IQ ~ fclass", "educ ~ fclass + IQ + IQ*fclass")

vd_model2 <- main(data, outcome_model_2b, outcome_type, mediator_models_2b, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  estimand, mc_draws, bootstrap_reps, confint)

### Lin-Vanderweele Approach -------

# Approach-specific parameters
estimand <- "ATE" # ATU, ATT are also possible here
effect_type <- "LV"

## Mediator and outcome model specification -----

# Specification 1: Linear additive models, no interactions
outcome_model_1a <- "class_bin ~ fclass + IQ + educ"
mediator_models_1a <- c("IQ ~ fclass", "educ ~ fclass + IQ")

lv_model1 <- main(data, outcome_model_1a, outcome_type, mediator_models_1a, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  estimand, 
                  mc_draws, bootstrap_reps, confint)

# Specification 2: Fully interacted
outcome_model_2a <- "class_bin ~ fclass + IQ + educ + IQ*educ + IQ*fclass + educ*fclass + IQ*educ*fclass"
mediator_models_2a <- c("IQ ~ fclass", "educ ~ fclass + IQ + IQ*fclass")

lv_model2 <- main(data, outcome_model_2a, outcome_type, mediator_models_2a, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  estimand, mc_draws, bootstrap_reps, confint)

# ----------------------------------------------------------------------------

##### Continuous effects -----------
outcome_type <- "continuous"

te_model <- "class_mod ~ fclass_mod" # Total effect model: simple linear regression
exposure <- "fclass_mod" # Father's social class 1-5 (from original encoding: 3-5 combined, 6 -> 4, 7 -> 5)
mediators <- c("IQ", "educ")
outcome <- "class_mod"

### Vanderweele-Robinson/Jackson-Vanderweele Approach -------

# Approach-specific parameters
estimand <- "ATU" # Specific to the interpretation of the VR-interventional effects
effect_type <- "VR"

## Mediator and outcome model specification -----

# Specification 1: Linear additive models, no interactions
outcome_model_1c <- "class_mod ~ fclass_mod + IQ + educ"
mediator_models_1c <- c("IQ ~ fclass_mod", "educ ~ fclass_mod + IQ")

vr_model1_cont <- main(data, outcome_model_1c, outcome_type, mediator_models_1c, 
                       mediator_types, te_model, exposure, mediators,
                       outcome, reference, counterfactual, effect_type, 
                       estimand, 
                       mc_draws, bootstrap_reps, confint)

# Specification 2: Fully interacted 
outcome_model_2c <- "class_mod ~ fclass_mod + IQ + educ + IQ*educ + IQ*fclass_mod + educ*fclass_mod + IQ*educ*fclass_mod"
mediator_models_2c <- c("IQ ~ fclass_mod", "educ ~ fclass_mod + IQ + IQ*fclass_mod")

vr_model2_cont <- main(data, outcome_model_2c, outcome_type, mediator_models_2c, 
                       mediator_types, te_model, exposure, mediators,
                       outcome, reference, counterfactual, effect_type, 
                       estimand, 
                       mc_draws, bootstrap_reps, confint)


