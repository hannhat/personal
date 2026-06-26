
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
data_1970m <- data_cca %>% mutate(sex == 0, cohort == "1970")

## Source functions -----
source("code/functions/mc_mediation_analysis.R")


### Universal Model Parameters ------- 

data <- data1970m
te_model <- "class_mod ~ fclass_mod" # Total effect model: simple linear regression
exposure <- "fclass_mod" # Father's social class 1-5 (from original encoding: 3-5 combined, 6 -> 4, 7 -> 5)
mediators <- c("IQ", "educ")
outcome <- "class_mod"

# "Treated", "untreated" exposure levels
reference <- 5
counterfactual <- 1 

# Linear regression models
mediator_types <- c("continuous", "continuous")
outcome_type <- "continuous"

# Monte Carlo, bootstrap parameters
mc_draws <- 1000000
bootstrap_reps <- 500
confint <- 0.95

# ----------------------------------------------------------------------------

### Lin-Vanderweele Approach -------

# Approach-specific parameters
estimand <- "ATE" # ATU, ATT are also possible here
effect_type <- "LV"
vd_conditional_model = NULL

## Mediator and outcome model specification -----

# Specification 1: Linear additive models, no interactions
outcome_model_1a <- "class_mod ~ fclass_mod + IQ + educ"
mediator_models_1a <- c("IQ ~ fclass_mod", "educ ~ fclass_mod + IQ")

lv_model1 <- main(data, outcome_model_1a, outcome_type, mediator_models_1a, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  vd_conditional_model, estimand, 
                  mc_draws, bootstrap_reps, confint)

# Specification 2: fclass/IQ outcome model interaction
outcome_model_2a <- "class_mod ~ fclass_mod + IQ + educ + fclass_mod*IQ"
mediator_models_2a <- c("IQ ~ fclass_mod", "educ ~ fclass_mod + IQ")

lv_model2 <- main(data, outcome_model_2a, outcome_type, mediator_models_2a, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  vd_conditional_model, estimand, 
                  mc_draws, bootstrap_reps, confint)

# Specification 3: fclass/IQ education mediator and outcome model interaction  
outcome_model_3a <- "class_mod ~ fclass_mod + IQ + educ + fclass_mod*IQ"
mediator_models_3a <- c("IQ ~ fclass_mod", "educ ~ fclass_mod + IQ + fclass*IQ")

lv_model3 <- main(data, outcome_model_3a, outcome_type, mediator_models_3a, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  vd_conditional_model, estimand, 
                  mc_draws, bootstrap_reps, confint)

### Vansteelandt-Daniel Approach -------

# Approach-specific parameters
estimand <- "ATE"
effect_type <- "VD"

## Mediator and outcome model specification -----

# Specification 1: Linear additive models, no interactions
outcome_model_1b <- "class_mod ~ fclass_mod + IQ + educ"
mediator_models_1b <- c("IQ ~ fclass_mod", "educ ~ fclass_mod")
vd_conditional_model_1b <- "educ ~ fclass_mod + IQ" # Needs to be additionally specified

vd_model1 <- main(data, outcome_model_1b, outcome_type, mediator_models_1b, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  vd_conditional_model_1b, estimand, 
                  mc_draws, bootstrap_reps, confint)

# Specification 2: Linear additive models, mediator-mediator outcome interaction
outcome_model_2b <- "class_mod ~ fclass_mod + IQ + educ + IQ*educ"
mediator_models_2b <- c("IQ ~ fclass_mod", "educ ~ fclass_mod")
vd_conditional_model_2b <- "educ ~ fclass_mod + IQ" # Needs to be additionally specified

vd_model2 <- main(data, outcome_model_2b, outcome_type, mediator_models_2b, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  vd_conditional_model_2b, estimand, 
                  mc_draws, bootstrap_reps, confint)

### Vanderweele-Robinson/Jackson-Vanderweele Approach -------

# Approach-specific parameters
estimand <- "ATU" # Specific to the interpretation of the VR-interventional effects
effect_type <- "VR"
vd_conditional_model <- NULL

## Mediator and outcome model specification -----

# Specification 1: Linear additive models, no interactions
outcome_model_1c <- "class_mod ~ fclass_mod + IQ + educ"
mediator_models_1c <- c("IQ ~ fclass_mod", "educ ~ fclass_mod + IQ")

vr_model1 <- main(data, outcome_model_1c, outcome_type, mediator_models_1c, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  vd_conditional_model, estimand, 
                  mc_draws, bootstrap_reps, confint)

# Specification 2: fclass/IQ outcome model interaction
outcome_model_2c <- "class_mod ~ fclass_mod + IQ + educ + fclass_mod*IQ"
mediator_models_2c <- c("IQ ~ fclass_mod", "educ ~ fclass_mod + IQ")

vr_model2 <- main(data, outcome_model_2c, outcome_type, mediator_models_2c, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  vd_conditional_model, estimand, 
                  mc_draws, bootstrap_reps, confint)

# Specification 3: fclass/IQ education mediator and outcome model interaction;  
outcome_model_3c <- "class_mod ~ fclass_mod + IQ + educ + fclass_mod*IQ"
mediator_models_3c <- c("IQ ~ fclass_mod", "educ ~ fclass_mod + IQ + fclass_mod*IQ")

vr_model3 <- main(data, outcome_model_3c, outcome_type, mediator_models_3c, 
                  mediator_types, te_model, exposure, mediators,
                  outcome, reference, counterfactual, effect_type, 
                  vd_conditional_model, estimand, 
                  mc_draws, bootstrap_reps, confint)










