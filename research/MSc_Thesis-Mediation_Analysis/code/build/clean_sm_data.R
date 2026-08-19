
### Setup -------

## Load packages -----
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

## Set working directory -----
setwd(here::here())

## Load data -----
 
path <- "data/input/social_class_mobility_data/"
social_mobility_data <- readRDS(paste0(path, "social_mobility_data.rds"))
workhistory1 <- read_dta(paste0(path, "workhistory_1946.dta")) %>%
  mutate(cohort = 1946)
workhistory2 <- read_dta(paste0(path, "workhistory_1958.dta")) %>%
  mutate(cohort = 1958)
workhistory3 <- read_dta(paste0(path, "workhistory_1970.dta")) %>%
  mutate(cohort = 1970)


### Data cleaning -------

# Cleaning social mobility RDS ---
sm_data1 <- social_mobility_data %>%
  filter(.imp == 0) %>%
  mutate(id = as.factor(id),
         cohort = as.factor(cohort))

# Adding IQ variable from work history data ---
wk_full <- bind_rows(workhistory1 %>% mutate(cohort = "1946",
                                             ed37 = educ),
                     workhistory2 %>% mutate(cohort = "1958"),
                     workhistory3 %>% mutate(cohort = "1970")) %>%
  mutate(id = row_number()) %>%
  select(cohort, id, sex, fclass, class38, ed37, IQ)

matched3 <- sm_data1 %>%
  mutate(in_sm = 1,
         id = .id) %>%
  full_join(wk_full %>% mutate(in_wh = 1,
                               id = as.factor(id)), 
            by = c("id", "cohort")) %>%
  mutate(in_both = ifelse(!is.na(in_sm) & !is.na(in_wh), 1, 0),
         sex.x = ifelse(sex.x == "male", 0, 1),
         fclass_str = fclass.x,
         class_str = class38.x,
         class38.x = case_when(class38.x == "High.sal" ~ 1,
                               class38.x == "Low.sal" ~ 2,
                               class38.x == "Interm" ~ 3,
                               class38.x == "Small.emp" ~ 4,
                               class38.x == "Low.sup" ~ 5,
                               class38.x == "Semi-rout" ~ 6,
                               class38.x == "Routine" ~ 7,
                               T ~ NA),
         fclass.x = case_when(fclass.x == "High.sal" ~ 1,
                              fclass.x == "Low.sal" ~ 2,
                              fclass.x == "Interm" ~ 3,
                              fclass.x == "Small.emp" ~ 4,
                              fclass.x == "Low.sup" ~ 5,
                              fclass.x == "Semi-rout" ~ 6,
                              fclass.x == "Routine" ~ 7,
                              T ~ NA))

matched <- matched3 %>%
  select(id, cohort, weight, sex.y, fclass.y, class38.y, ed37, 
         IQ, class_str, fclass_str, in_wh, in_both) %>%
  rename(sex = sex.y, fclass = fclass.y, class = class38.y,
         educ = ed37) %>%
  mutate(class = ifelse(class >= 1 & class <= 7, class, NA))
matched <- matched %>% filter(in_both == 1)

data_cca <- matched %>% 
  drop_na() %>%
  select(-in_wh, -in_both) %>%
  mutate(cohort = as.factor(cohort))

# Adding custom variables ---
data_cca <- data_cca %>%
  # Adding the distance metric for mobility discussed in Kuha et al (2021) ("credible" continuous outcome)
  mutate(class_mod = case_when(class >= 3 & class <= 5 ~ 3,
                               class > 5 ~ class - 2,
                               T ~ class),
         fclass_mod = case_when(fclass >= 3 & fclass <= 5 ~ 3,
                                fclass > 5 ~ fclass - 2,
                                T ~ fclass))

### Output ------

saveRDS(data_cca, file = "data/build/social_mobility_complete.rds")



