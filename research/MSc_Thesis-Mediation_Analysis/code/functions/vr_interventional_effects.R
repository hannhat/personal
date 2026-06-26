
### Setup -------

## Load packages -----
library(here)
library(dplyr)
library(ggplot2)
library(stats)

## Set working directory -----
setwd(here::here())


### Functions -------

## Main -------

vr_effect_point <- function(data,
                            exposure, # column
                            reference, # element of exposure column
                            counterfactual, # element of exposure column
                            outcome, # column
                            mediators, # vector of columns
                            intervention, # "single", "first", "second" (WIP), "joint", "m1", "m2"
                            controls, # vector of columns
                            outcome_type, # "continuous", "binary" (WIP)
                            outcome_model, # Allows (linear) outcome model to be directly specified; if NULL, simple linear model is run
                            mediator_model, # Mediator model specification
                            mediator_model1, # First mediator model in joint mediator estimation
                            mediator_model2, # Second mediator model in joint mediator estimation
                            monte_carlo, # boolean: determines whether MC integration is used
                            mc_draws) { # Number of Monte Carlo draws
  
  # Determining the type of VR-interventional effect being estimated
  if (intervention == "single") {
    mediator <- mediators
    mediator_str <- mediator
  }
  else if (intervention == "first") {
    mediator <- mediators[1]
    mediator_str <- mediator
  } 
  else if (intervention == "second") {
    controls <- c(controls, mediators[1])
    mediator <- mediators[2]
    mediator_str <- mediator
  } 
  else { # Can only be used with MC integration approach
    mediator_str <- paste(mediators, collapse = " + ")
  }
  
  # String cleaning
  if (is.null(outcome_model)) {
    if (!is.null(controls)) {
      controls_str <- paste(controls, collapse = " + ")
      outcome_model <- paste(outcome, " ~ ", mediator_str, " + ", exposure, " + ",
                             controls_str)
    }
    else {
      outcome_model <- paste(outcome, " ~ ", mediator_str, " + ", exposure)    
    }
  }
  
  ## Mediator-outcome regression -----
  if (outcome_type == "continuous") {
    model <- glm(formula = outcome_model, 
                 data = data)
    coef_med <- model$coefficients[mediator][[1]] 
  }
  else if (outcome_type == "binary") {
    model <- glm(formula = outcome_model, data = 
                   data, 
                 family = binomial)
    coef_med <- model$coefficients[mediator][[1]] 
  }
  
  ## Exposure-mediator model -----
  
  if (intervention != "joint") {
    if (is.null(mediator_model)) {
      if (!is.null(controls)) {
        controls_str <- paste(controls, collapse = " + ")
        mediator_model <- paste(mediator, " ~ ", exposure, " + ",
                                controls_str)
      }
      else {
        mediator_model <- paste(mediator, " ~ ", exposure)    
      } 
    }
  }
  
  # [WIP] Monte Carlo approach ---
  if (monte_carlo == TRUE) {
    model_data <- data %>% filter(.data[[exposure]] == reference)
    if (intervention %in% c("single", "first", "second")) {
      exp_med_model <- glm(formula = mediator_model, data = data)
      # Sampling from the reference distribution
      mc_data <- slice_sample(model_data, n = mc_draws, replace = TRUE) %>%
        mutate(!!exposure := as.character(counterfactual)) 
      # the sampled values come from the c.f. mediator conditional distribution
      
      # Linear mediator model  
      mu <- predict.glm(exp_med_model, mc_data)   # conditional mean per row
      sigma_hat <- sigma(exp_med_model)  # residual SD from the fit
      mc_data[[mediator]] <- rnorm(nrow(mc_data), mean = mu, sd = sigma_hat)
      
      mc_data_fnl <- mc_data %>% # Switching the mediator back to the correct value
        mutate(!!exposure := as.character(reference))
      
      if (outcome_type == "continuous") {
        vr_effect <- mean(predict.glm(model, mc_data_fnl)) - mean(model_data[[outcome]])
      }
      else if (outcome_type == "binary") {
        p1 <- mean(predict(model, mc_data_fnl, type = "response"))   # exposure=ref, mediator~cf
        p0 <- mean(predict(model, model_data,  type = "response"))   # exposure=ref, mediator~ref
        vr_effect <- (p1 / (1 - p1)) / (p0 / (1 - p0))
      }
    }
    else {
      
      if (intervention == "joint") {
        
        # Pursuing sequential MC strategy since the mediator ordering is known
        if ((is.null(mediator_model1)) & is.null(mediator_model2)) {
          if (!is.null(controls)) {
            controls_str <- paste(controls, collapse = " + ")
            mediator_model1 <- paste(mediators[1], " ~ ", exposure, " + ", 
                                     controls_str)
            mediator_model2 <- paste(mediators[2], " ~ ", mediators[1], " + ",
                                     exposure, " + ",  
                                     controls_str)
          }
          else {
            formula_med1 <- paste(mediators[1], " ~ ", exposure)
            formula_med2 <- paste(mediators[2], " ~ ", mediators[1], " + ", exposure)    
          }
        }
        
        exp_med_model1 <- glm(formula = mediator_model1, data = data)   
        exp_med_model2 <- glm(formula = mediator_model2, data = data)
        
        ## Mediator 1 ---
        mc_data <- slice_sample(model_data, n = mc_draws, replace = TRUE) %>%
          mutate(!!exposure := as.character(counterfactual)) 
        mu <- predict.glm(exp_med_model1, mc_data)   
        sigma_hat <- sigma(exp_med_model1)                       
        mc_data[[mediators[1]]] <- rnorm(nrow(mc_data), mean = mu, sd = sigma_hat)
        
        ## Mediator 2 ---
        mu <- predict.glm(exp_med_model2, mc_data) 
        sigma_hat <- sigma(exp_med_model2)                      
        mc_data[[mediators[2]]] <- rnorm(nrow(mc_data), mean = mu, sd = sigma_hat)
        
        mc_data_fnl <- mc_data %>% # Switching the mediator back to the correct value
          mutate(!!exposure := as.character(reference)) 
        vr_effect <- mean(predict.glm(model, mc_data_fnl)) - mean(model_data[[outcome]])
      }
      
      else if (intervention == "m1") {
        
        # Independent draws
        if ((is.null(mediator_model1)) & is.null(mediator_model2)) {
          if (!is.null(controls)) {
            controls_str <- paste(controls, collapse = " + ")
            mediator_model1 <- paste(mediators[1], " ~ ", exposure, " + ", 
                                     controls_str)
            mediator_model2 <- paste(mediators[2], " ~ ", exposure, " + ",  
                                     controls_str)
          }
          else {
            formula_med1 <- paste(mediators[1], " ~ ", exposure)
            formula_med2 <- paste(mediators[2], " ~ ", mediators[1], " + ", exposure)    
          }
        }
        
        exp_med_model1 <- glm(formula = mediator_model1, data = data)   
        exp_med_model2 <- glm(formula = mediator_model2, data = data)
        
        ## Mediator 1 ---
        mc_data <- slice_sample(model_data, n = mc_draws, replace = TRUE) %>%
          mutate(!!exposure := as.character(counterfactual)) 
        mu <- predict.glm(exp_med_model1, mc_data)   
        sigma_hat <- sigma(exp_med_model1)                       
        mc_data[[mediators[1]]] <- rnorm(nrow(mc_data), mean = mu, sd = sigma_hat)
        
        ## Mediator 2 ---
        mc_data2 <- mc_data %>%
          mutate(!!exposure := as.character(reference)) 
        mu <- predict.glm(exp_med_model2, mc_data2) 
        sigma_hat <- sigma(exp_med_model2)                      
        mc_data[[mediators[2]]] <- rnorm(nrow(mc_data2), mean = mu, sd = sigma_hat)
        
        ## Prediction ---
        mc_data_fnl <- mc_data %>% # Switching the mediator back to the correct value
          mutate(!!exposure := as.character(reference)) 
        vr_effect <- mean(predict.glm(model, mc_data_fnl)) - mean(model_data[[outcome]])
      }
      
      else if (intervention == "m2") {
        
        if ((is.null(mediator_model1)) & is.null(mediator_model2)) {
          if (!is.null(controls)) {
            controls_str <- paste(controls, collapse = " + ")
            mediator_model1 <- paste(mediators[1], " ~ ", exposure, " + ", 
                                     controls_str)
            mediator_model2 <- paste(mediators[2], " ~ ", exposure, " + ",  
                                     controls_str)
          }
          else {
            formula_med1 <- paste(mediators[1], " ~ ", exposure)
            formula_med2 <- paste(mediators[2], " ~ ", mediators[1], " + ", exposure)    
          }
        }
        
        exp_med_model1 <- glm(formula = mediator_model1, data = data)   
        exp_med_model2 <- glm(formula = mediator_model2, data = data)
        
        ## Mediator 1 ---
        mc_data <- slice_sample(model_data, n = mc_draws, replace = TRUE) %>%
          mutate(!!exposure := as.character(reference)) 
        mu <- predict.glm(exp_med_model1, mc_data)   
        sigma_hat <- sigma(exp_med_model1)                       
        mc_data[[mediators[1]]] <- rnorm(nrow(mc_data), mean = mu, sd = sigma_hat)
        
        ## Mediator 2 ---
        mc_data2 <- slice_sample(model_data, n = mc_draws, replace = TRUE) %>%
          mutate(!!exposure := as.character(counterfactual)) 
        mu <- predict.glm(exp_med_model2, mc_data2) 
        sigma_hat <- sigma(exp_med_model2)                      
        mc_data[[mediators[2]]] <- rnorm(nrow(mc_data2), mean = mu, sd = sigma_hat)
        
        mc_data_fnl <- mc_data %>% # Switching the mediator back to the correct value
          mutate(!!exposure := as.character(reference)) 
        vr_effect <- mean(predict.glm(model, mc_data_fnl)) - mean(model_data[[outcome]])
      }
      
    }
  }
  
  # Regression approach (assuming linear outcome model w/ no interactions only) ---
  else {
    exp_med_model <- glm(formula = mediator_model, data = data)
    exposure_str <- paste0(exposure, as.character(counterfactual))
    coef_exp <- exp_med_model$coefficients[exposure_str][[1]]
    
    if (outcome_type == "continuous") {
      vr_effect <- coef_exp*coef_med
    }
    else if (outcome_type == "binary") {
      vr_effect <- exp(coef_exp*coef_med)
    }
  }
  
  return(vr_effect)
}

vr_effect_bootstrap <- function(data, exposure, reference,
                                counterfactual, outcome,
                                mediators, intervention, controls, 
                                outcome_type, outcome_model,
                                mediator_model,
                                mediator_model1,
                                mediator_model2,
                                monte_carlo, mc_draws,
                                confint, B) {
  
  size <- dim(data)[1]
  
  est_lst <- c()
  for (i in 1:B) {
    boot_data <- slice_sample(data, n = size, replace = TRUE)
    est <- vr_effect_point(boot_data, exposure, reference, counterfactual,
                           outcome, mediators, intervention, controls,
                           outcome_type, 
                           outcome_model,
                           mediator_model,
                           mediator_model1,
                           mediator_model2, 
                           monte_carlo, mc_draws)
    est_lst <- c(est_lst, est)
  }
  
  lb <- quantile(est_lst, (1 - confint) / 2)
  ub <- quantile(est_lst, 1 - (1 - confint) / 2)

  ci <- c(lb, ub)
  
  return(ci)
}


vr_effect_main <- function(data = data_cca,
                           exposure = "fclass_str",
                           reference = "High.sal",
                           counterfactual = "Low.sal",
                           outcome = "class",
                           mediators = "educ",
                           intervention = "single",
                           controls = c("sex", "cohort"),
                           outcome_type = "continuous",
                           outcome_model = NULL,
                           mediator_model = NULL,
                           mediator_model1 = NULL,
                           mediator_model2 = NULL,
                           monte_carlo = FALSE,
                           mc_draws = NULL,
                           confint = 0.95,
                           B = 500) {
  
  data_cln <- data %>%
    mutate(!!exposure := relevel(factor(.data[[exposure]]), ref = reference))
  #%>%
    #filter(.data[[exposure]] %in% c(reference, counterfactual)) %>%
    #mutate(exposure0 = case_when(
    #  .data[[exposure]] == reference ~ 0,
    #  .data[[exposure]] == counterfactual ~ 1
    #))
  
  est <- vr_effect_point(data_cln, exposure, reference, counterfactual,
                         outcome, mediators, intervention, controls,
                         outcome_type, outcome_model,
                         mediator_model,
                         mediator_model1,
                         mediator_model2, monte_carlo, mc_draws)
  ci  <- vr_effect_bootstrap(data_cln, exposure, reference, counterfactual,
                             outcome, mediators, intervention, controls,
                             outcome_type, outcome_model,
                             mediator_model,
                             mediator_model1,
                             mediator_model2, monte_carlo, mc_draws, confint, B)
  
  cat("VR-Interventional Effect:", est, "\n")
  cat("Percentile Bootstrap", confint*100, "% CI:", "[ ", ci[1], ", ", ci[2], "]")
  
  out <- c(est, ci[1], ci[2])
  return(out)
}











