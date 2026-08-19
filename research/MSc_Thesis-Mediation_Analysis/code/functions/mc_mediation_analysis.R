
### Setup -------

## Load packages -----
library(here)
library(dplyr)
library(stats)
library(marginaleffects)

## Set working directory -----
setwd(here::here())


### Functions -------

fit_models <- function(data,
                       outcome_model,
                       outcome_type,
                       mediator_models,
                       mediator_types) {

if (outcome_type == "continuous") {
  o_model <- glm(formula = outcome_model, 
               data = data)
}
  else if (outcome_type == "binary") {
    o_model <- glm(formula = outcome_model, 
                   data = data,
                   family = binomial)
  }
  
N <- length(mediator_models) + 1  
model_lst <- vector("list", N)

for (i in 1:length(mediator_models)) {
  if (mediator_types[i] == "continuous") {
    m_model <- glm(formula = mediator_models[i], 
                   data = data)
  }
  else if (mediator_types[i] == "binary") {
    m_model <- glm(formula = mediator_models[i], 
                   data = data,
                   family = binomial)
  }
  model_lst[[i]] <- m_model
}
model_lst[[N]] <- o_model

return(model_lst)
}


mc_estimation <- function(data,
                          model_lst,
                          exposure,
                          mediators,
                          outcome,
                          reference,
                          counterfactual,
                          outcome_type,
                          effect_type,
                          estimand,
                          mc_draws) {
# Only configured for 1 or 2 mediators for now
  
  # Filter based on the estimand/effect type
  if (estimand == "ATU") {
    model_data <- data %>% filter(.data[[exposure]] == reference) 
  }
  else if (estimand == "ATT") {
    model_data <- data %>% filter(.data[[exposure]] == counterfactual) 
  }
  else {
    model_data <- data %>% 
      filter(.data[[exposure]] %in% c(reference, counterfactual)) 
  }
  
  # Total effect estimation ---
  if (outcome_type == "continuous") {
    y1 <- mean((data %>% filter(.data[[exposure]] == counterfactual))$class_mod)
    y0 <- mean((data %>% filter(.data[[exposure]] == reference))$class_mod)
    te_out <- y1 - y0
  }
  else if (outcome_type == "binary") {
    y1 <- mean((data %>% filter(.data[[exposure]] == counterfactual))$class_mod_bin)
    y0 <- mean((data %>% filter(.data[[exposure]] == reference))$class_mod_bin)
    te_out <- y1 - y0
  }
  
  ## Main effect estimation module -----
  if (length(mediators) == 1) {
    
    if (effect_type == "VR") {
      ## MC estimation of interventional distribution estimand -----
      mc_data_o <- slice_sample(model_data, n = mc_draws, replace = TRUE)
      
      # Mediator ---
      # M(0)
      mu_m0 <- predict.glm(model_lst[[1]], 
                           mc_data_o %>% mutate(!!exposure := reference))   
      sigma_hat_m0 <- sigma(model_lst[[1]])
      m0_draw <- rnorm(mc_draws, mean = mu_m0, sd = sigma_hat_m0)
      
      # M(1)
      mu_m1 <- predict.glm(model_lst[[1]], 
                           mc_data_o %>% mutate(!!exposure := counterfactual))   
      sigma_hat_m1 <- sigma(model_lst[[1]])
      m1_draw <- rnorm(mc_draws, mean = mu_m1, sd = sigma_hat_m1)
      
      # Outcome ---
      # Y(0,M(0))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := reference,
               !!mediators[1] := m0_draw)
      y00_hat <- mean(predict.glm(model_lst[[2]], mc_data))
      
      # Y(0,M(1))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := reference,
               !!mediators[1] := m1_draw)
      y01_hat <- mean(predict.glm(model_lst[[2]], mc_data))
      
      # Y(1,M(1))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_draw)
      y11_hat <- mean(predict.glm(model_lst[[2]], mc_data))
      
      
      ## Effect estimation -----
    
      # Interventional total effect (rTE)
      ite_out <- y11_hat - y00_hat
      
      # VR-Interventional effect
      vre_out <- y01_hat - y00_hat
      
      results <- c(te_out, ite_out, vre_out) 
    }
      
    else {
      ## MC estimation of interventional distribution estimand -----
      mc_data_o <- slice_sample(model_data, n = mc_draws, replace = TRUE)
      
      # Mediator ---
      # M(0)
      mu_m0 <- predict.glm(model_lst[[1]], 
                           mc_data_o %>% mutate(!!exposure := reference))   
      sigma_hat_m0 <- sigma(model_lst[[1]])
      m0_draw <- rnorm(mc_draws, mean = mu_m0, sd = sigma_hat_m0)
      
      # M(1)
      mu_m1 <- predict.glm(model_lst[[1]], 
                           mc_data_o %>% mutate(!!exposure := counterfactual))   
      sigma_hat_m1 <- sigma(model_lst[[1]])
      m1_draw <- rnorm(mc_draws, mean = mu_m1, sd = sigma_hat_m1)
      
      # Outcome ---
      # Y(0,M(0))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := reference,
               !!mediators[1] := m0_draw)
      mu_y00 <- predict.glm(model_lst[[2]], mc_data) 
      sigma_hat_y00 <- sigma(model_lst[[2]])
      y00_hat <- mean(rnorm(mc_draws, mean = mu_y00, sd = sigma_hat_y00))
      
      # Y(1,M(0))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m0_draw)
      mu_y10 <- predict.glm(model_lst[[2]], mc_data) 
      sigma_hat_y10 <- sigma(model_lst[[2]])
      y10_hat <- mean(rnorm(mc_draws, mean = mu_y10, sd = sigma_hat_y10))
      
      # Y(1,M(1))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_draw)
      mu_y11 <- predict.glm(model_lst[[2]], mc_data) 
      sigma_hat_y11 <- sigma(model_lst[[2]])
      y11_hat <- mean(rnorm(mc_draws, mean = mu_y11, sd = sigma_hat_y11))
      
      
      ## Effect estimation -----
      
      # Interventional total effect (rTE)
      ite_out <- y11_hat - y00_hat
      
      # Interventional direct effect
      ide_out <- y10_hat - y00_hat
      
      # Interventional indirect effect
      iie_out <- y11_hat - y10_hat
      
      results <- c(te_out, ite_out, ide_out, iie_out) 
    }

  }
  
  else if(length(mediators) == 2) {
    
    if (effect_type == "LV") {
      ## MC estimation of interventional distribution estimand -----
      mc_data_o <- slice_sample(model_data, n = mc_draws, replace = TRUE)
      
      # Mediator 1 ---
      # M1(0)
      mu_m1_0 <- predict.glm(model_lst[[1]], 
                           mc_data_o %>% mutate(!!exposure := reference))   
      sigma_hat_m1_0 <- sigma(model_lst[[1]])
      m1_0_draw <- rnorm(mc_draws, mean = mu_m1_0, sd = sigma_hat_m1_0) # default M1
      m1_0_draw_v2 <- rnorm(mc_draws, mean = mu_m1_0, sd = sigma_hat_m1_0) # default M1
      
      # M1(1)
      mu_m1_1 <- predict.glm(model_lst[[1]], 
                           mc_data_o %>% mutate(!!exposure := counterfactual))   
      sigma_hat_m1_1 <- sigma(model_lst[[1]])
      m1_1_draw <- rnorm(mc_draws, mean = mu_m1_1, sd = sigma_hat_m1_1) 
      m1_1_draw_v2 <- rnorm(mc_draws, mean = mu_m1_1, sd = sigma_hat_m1_1) 
      
      # Mediator 2 ---
      # M2(0,M1(0))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := reference,
               !!mediators[1] := m1_0_draw_v2)
      mu_m2_00 <- predict.glm(model_lst[[2]], mc_data) 
      sigma_hat_m2_00 <- sigma(model_lst[[2]])
      m2_00_draw <- rnorm(mc_draws, mean = mu_m2_00, sd = sigma_hat_m2_00)
      
      # M2(1,M1(0))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_0_draw_v2)
      mu_m2_10 <- predict.glm(model_lst[[2]], mc_data) 
      sigma_hat_m2_10 <- sigma(model_lst[[2]])
      m2_10_draw <- rnorm(mc_draws, mean = mu_m2_10, sd = sigma_hat_m2_10)
      
      # M2(1,M1(1))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_1_draw_v2)
      mu_m2_11 <- predict.glm(model_lst[[2]], mc_data) 
      sigma_hat_m2_11 <- sigma(model_lst[[2]])
      m2_11_draw <- rnorm(mc_draws, mean = mu_m2_11, sd = sigma_hat_m2_11)
      
      # Outcome ---
      # Y(0,M1(0),M2(0,M1(0)))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := reference,
               !!mediators[1] := m1_0_draw,
               !!mediators[2] := m2_00_draw)
      y0000_hat <- mean(predict.glm(model_lst[[3]], mc_data, type = "response"))
      
      # Y(1,M1(0),M2(0,M1(0)))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_0_draw,
               !!mediators[2] := m2_00_draw)
      y1000_hat <- mean(predict.glm(model_lst[[3]], mc_data, type = "response"))
      
      # Y(1,M1(1),M2(0,M1(0)))
      # 'Recanting witness' problem
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_1_draw,
               !!mediators[2] := m2_00_draw)
      y1100_hat <- mean(predict.glm(model_lst[[3]], mc_data, type = "response")) 
      
      # Y(1,M1(1),M2(1,M1(0)))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_1_draw,
               !!mediators[2] := m2_10_draw)
      y1110_hat <- mean(predict.glm(model_lst[[3]], mc_data, type = "response"))
      
      # Y(1,M1(1),M2(1,M1(1)))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_1_draw,
               !!mediators[2] := m2_11_draw)
      y1111_hat <- mean(predict.glm(model_lst[[3]], mc_data, type = "response"))
      
      
      ## Effect estimation -----
      
      # Interventional total effect (rTE)
      ite_out <- y1111_hat - y0000_hat
      
      # Interventional direct effect
      ide_out <- y1000_hat - y0000_hat
      
      # Interventional indirect effect through M1 but not M2
      iie_m1_out <- y1100_hat - y1000_hat
      
      # Interventional indirect effect through M2 but not M1
      iie_m2_out <- y1110_hat - y1100_hat
      
      # Interventional indirect effect through M1 and M2
      iie_m12_out <- y1111_hat - y1110_hat
      
      results <- c(te_out, ite_out, ide_out, 
                   iie_m1_out, iie_m2_out, iie_m12_out)
    }
    
    else if (effect_type == "VD") {
      ## MC estimation of interventional distribution estimand -----
      mc_data_o <- slice_sample(model_data, n = mc_draws, replace = TRUE)
      
      # Mediator 1 ---
      # M1(0)
      mu_m1_0 <- predict.glm(model_lst[[1]], 
                             mc_data_o %>% mutate(!!exposure := reference))   
      sigma_hat_m1_0 <- sigma(model_lst[[1]])
      m1_0_draw <- rnorm(mc_draws, mean = mu_m1_0, sd = sigma_hat_m1_0)
      m1_0_draw2 <- rnorm(mc_draws, mean = mu_m1_0, sd = sigma_hat_m1_0)
      
      # M1(1)
      mu_m1_1 <- predict.glm(model_lst[[1]], 
                             mc_data_o %>% mutate(!!exposure := counterfactual))   
      sigma_hat_m1_1 <- sigma(model_lst[[1]])
      m1_1_draw <- rnorm(mc_draws, mean = mu_m1_1, sd = sigma_hat_m1_1)
      
      # Mediator 2 ---
      
      # M2(0)|M1(0)
      mu_m12_00 <- predict.glm(model_lst[[2]], 
                             mc_data_o %>% mutate(!!exposure := reference,
                                                !!mediators[1] := m1_0_draw))   
      sigma_hat_m12_00 <- sigma(model_lst[[2]])
      m12_00_draw <- rnorm(mc_draws, mean = mu_m12_00, sd = sigma_hat_m12_00)
      
      # M2(1)|M1(1)
      mu_m12_11 <- predict.glm(model_lst[[2]], 
                               mc_data_o %>% mutate(!!exposure := counterfactual,
                                                  !!mediators[1] := m1_1_draw))   
      sigma_hat_m12_11 <- sigma(model_lst[[2]])
      m12_11_draw <- rnorm(mc_draws, mean = mu_m12_11, sd = sigma_hat_m12_11)
      
      # Outcome ---
      # Joint: Y(0,M1(0),M2(0))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := reference,
               !!mediators[1] := m1_0_draw,
               !!mediators[2] := m12_00_draw)
      y000_hat_joint <- mean(predict.glm(model_lst[[3]], mc_data, , type = "response"))
      
      # Joint: Y(1,M1(0),M2(0,M1(0)))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_0_draw,
               !!mediators[2] := m12_00_draw)
      y100_hat_joint <- mean(predict.glm(model_lst[[3]], mc_data, , type = "response"))
      
      # Product of marginals: Y(1,M1(0),M2(0))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_0_draw2, # Independence between M(1), M(2)
               !!mediators[2] := m12_00_draw)
      y100_hat_pm <- mean(predict.glm(model_lst[[3]], mc_data, , type = "response"))

      # Y(1,M1(1),M2(0))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_1_draw,
               !!mediators[2] := m12_00_draw)
      y110_hat <- mean(predict.glm(model_lst[[3]], mc_data, , type = "response")) 
      
      # Y(1,M1(0),M2(1))
      mc_data <- mc_data_o %>%
        mutate(!!exposure := counterfactual,
               !!mediators[1] := m1_0_draw,
               !!mediators[2] := m12_11_draw)
      y101_hat <- mean(predict.glm(model_lst[[3]], mc_data, type = "response"))
      
      ## Effect estimation -----
      
      # Interventional direct effect
      ide_out <- y100_hat_joint - y000_hat_joint
      
      # Interventional indirect effect through M1 but not M2
      iie_m1_out <- y110_hat - y100_hat_pm
      
      # Interventional indirect effect through M2 but not M1
      iie_m2_out <- y101_hat - y100_hat_pm
      
      # Remainder
      iie_m12_out <- te_out - (ide_out + iie_m1_out + iie_m2_out)
      
      results <- c(te_out, ide_out, 
                   iie_m1_out, iie_m2_out, iie_m12_out)
      
    }
    else if (effect_type == "VR") {
      ## MC estimation of interventional distribution estimand -----
      mc_data_o <- slice_sample(model_data, n = mc_draws, replace = TRUE)
      
      # Mediator 1 marginal ---
      # M1(0)
      mu_m1_0 <- predict.glm(model_lst[[1]], 
                             mc_data_o %>% mutate(!!exposure := reference))   
      sigma_hat_m1_0 <- sigma(model_lst[[1]])
      m1_0_draw <- rnorm(mc_draws, mean = mu_m1_0, sd = sigma_hat_m1_0)
      
      # M1(1)
      mu_m1_1 <- predict.glm(model_lst[[1]], 
                             mc_data_o %>% mutate(!!exposure := counterfactual))   
      sigma_hat_m1_1 <- sigma(model_lst[[1]])
      m1_1_draw <- rnorm(mc_draws, mean = mu_m1_1, sd = sigma_hat_m1_1)
      
      # Mediator 2 conditional ---
      # M2(0)|M1
      mu_m2c_0 <- predict.glm(model_lst[[2]], 
                             mc_data_o %>% mutate(!!exposure := reference))   
      sigma_hat_m2c_0 <- sigma(model_lst[[2]])
      m2c_0_draw <- rnorm(mc_draws, mean = mu_m2c_0, sd = sigma_hat_m2c_0)
      
      # M2(1)|M1
      mu_m2c_1 <- predict.glm(model_lst[[2]], 
                              mc_data_o %>% mutate(!!exposure := counterfactual))   
      sigma_hat_m2c_1 <- sigma(model_lst[[2]])
      m2c_1_draw <- rnorm(mc_draws, mean = mu_m2c_1, sd = sigma_hat_m2c_1)
      
      # Mediator 2 marginal ---
      # M2(0)|T=0
      mu_m2m_0 <- predict.glm(model_lst[[2]], 
                             mc_data_o %>% mutate(!!exposure := reference,
                                                !!mediators[1] := m1_1_draw))   
      sigma_hat_m2m_0 <- sigma(model_lst[[2]])
      m2m_0_draw <- rnorm(mc_draws, mean = mu_m2m_0, sd = sigma_hat_m2m_0)
      
      # M2(1)|T=1
      mu_m2m_1 <- predict.glm(model_lst[[2]], 
                             mc_data_o %>% mutate(!!exposure := counterfactual,
                                                !!mediators[1] := m1_1_draw))   
      sigma_hat_m2m_1 <- sigma(model_lst[[2]])
      m2m_1_draw <- rnorm(mc_draws, mean = mu_m2m_1, sd = sigma_hat_m2m_1)
      
      # Keeping M1(0) and M2(0)|M1 code snippets for now; if it is determined
      # I don't need to compute interventional effect for Y(0,0,0) then can drop
      
      # Review joint intervention
      
      ## Effect estimation -----
      
      # Interventional indirect effect through equalizing M1 
      mc_data <- mc_data_o %>%
        mutate(!!exposure := reference,
               !!mediators[1] := m1_1_draw,
               !!mediators[2] := m2m_0_draw)
      iie_m1 <- mean(predict.glm(model_lst[[3]], mc_data, type = "response"))
      
      # Interventional indirect effect through equalizing marginal of M2 
      mc_data <- mc_data_o %>%
        mutate(!!exposure := reference,
               !!mediators[2] := m2m_1_draw)
      iie_m2m <- mean(predict.glm(model_lst[[3]], mc_data, type = "response"))

      
      # Interventional indirect effect through equalizing M2 conditional on M1
      mc_data <- mc_data_o %>%
        mutate(!!exposure := reference,
               !!mediators[2] := m2c_1_draw)
      iie_m2c <- mean(predict.glm(model_lst[[3]], mc_data, type = "response"))
      
      # Interventional indirect effect through jointly equalizing (M1,M2)
      mc_data <- mc_data_o %>%
        mutate(!!exposure := reference,
               !!mediators[1] := m1_1_draw,
               !!mediators[2] := m2m_1_draw)
      iie_joint <- mean(predict.glm(model_lst[[3]], mc_data, type = "response"))
      
      iie_m1_out <- iie_m1 - y0
      iie_m2m_out <- iie_m2m - y0
      iie_m2c_out <- iie_m2c - y0
      iie_joint_out <- iie_joint - y0
      
      results <- c(te_out, iie_m1_out, iie_m2m_out, iie_m2c_out, iie_joint_out)
      
    }
    
  }
  
  return(results)
}


main <- function(data,
                 outcome_model,
                 outcome_type,
                 mediator_models,
                 mediator_types,
                 te_model, # Total effect model: should generally be "outcome ~ exposure + [controls]"
                 exposure,
                 mediators,
                 outcome,
                 reference,
                 counterfactual,
                 effect_type, # natural, LV, VD, VR
                 estimand, # ATE, ATU, ATT 
                 mc_draws,
                 bootstrap_reps,
                 confint) {
  
  # Point estimates ---
  model_lst <- fit_models(data,
                        outcome_model,
                        outcome_type,
                        mediator_models,
                        mediator_types)
  pe <- mc_estimation(data,
                       model_lst,
                       exposure,
                       mediators,
                       outcome,
                       reference,
                       counterfactual,
                       outcome_type,
                       effect_type,
                       estimand,
                       mc_draws)
   
  # Bootstrapped standard errors ---
  if (length(mediators) == 1) {
    if (effect_type == "VR") {
      cols <- 3
      results <- data.frame(Effect = c("TE", "rTE", "VR-IE"))
    }
    else {
      cols <- 4
      results <- data.frame(Effect = c("TE", "rTE", "rDE", "rIE")) 
    }
  }
  else if (length(mediators) == 2) {
    if (effect_type == "LV") {
      cols <- 6 # to set later
      results <- data.frame(Effect = c("TE", "rTE", "rDE", 
                                       "rIE_M1", "rIE_M2", "r_IE_M12"))
    }
    else if (effect_type == "VD") {
      cols <- 5 # to set later
      results <- data.frame(Effect = c("TE", "rDE", 
                                       "rIE_M1", "rIE_M2", "Remainder")) 
    }
    else if (effect_type == "VR") {
      cols <- 5 # to set later
      results <- data.frame(Effect = c("TE", "IIE_M1", 
                                       "IIE_M2_Marginal", "IIE_M2_Conditional", 
                                       "IIE_Joint")) 
    }
  }
  
  boot_tbl <- matrix(data = NA, nrow = bootstrap_reps, ncol = cols)
  N <- dim(data1970m)[1]
  for (i in 1:bootstrap_reps) {
    # Sampling with replacement
    boot_data <- slice_sample(data, n = N, replace = TRUE)
    
    model_lst_i <- fit_models(boot_data,
                            outcome_model,
                            outcome_type,
                            mediator_models,
                            mediator_types)
    pe_i <- mc_estimation(boot_data,
                        model_lst_i,
                        exposure,
                        mediators,
                        outcome,
                        reference,
                        counterfactual,
                        outcome_type,
                        effect_type,
                        estimand,
                        mc_draws)
    boot_tbl[i,] <- pe_i
  }
  
  # Construction of output table ---
  results["Estimate"] <- pe
    
  lbs <- apply(boot_tbl, 2, quantile, probs = (1 - confint) / 2, na.rm = TRUE)
  ubs <- apply(boot_tbl, 2, quantile, probs = 1 - (1 - confint) / 2, na.rm = TRUE)

  results[paste0(confint*100,"% CI LB")] <- lbs
  results[paste0(confint*100,"% CI UB")] <- ubs
  
  results <- results %>% mutate(across(is.numeric, round, digits=3))
  
  return(results)
}


# To-do: 
#
# 1. Get estimates, comparisons under simple linear, simple interaction models for LV vs. VD effects;
#    find a nice way to compare multiple estimates using the continuous outcome variable
# 2. Think about how to implement VR interventional effects for single, multiple mediators
# 3. Code up the binary outcome/mediator sensitivities; prioritize binary outcome, test a sensitivity from
#    Kuha et al (2021)
# 4. Explore copula sensitivity analysis for the LV effects







