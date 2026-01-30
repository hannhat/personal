# Reset environment -------------------------------------------------------
rm(list = ls())

source("~/personal/sabermetrics_playground/model_playground/code/header.R")
# Code --------------------------------------------------------------------

## Import
import <- read_csv(f.import("/fangraphs/fg_pitching_2020_2024.csv")) %>%
  as_tibble() %>%
  clean_names()


## Build

# Selecting relevant stats
build <- import %>%
  filter(pitches >= 500, season <= 2023) %>%
  group_by(name) %>%
  mutate(count = n()) %>% 
  ungroup() %>%
  filter(count == 4) %>%
  select(season, name, age, era, era_2, fip, fip_2, ip, pitches, 
         whip, babip, x_fip, x_fip_2, pitching, stuff, location) %>%
  arrange(name, season)

build_diff <- build %>%
  select(-c(season, name)) %>%
  mutate(across(everything(),  ~ coalesce(.x -lag(.x), .x)))
colnames(build_diff) <- paste(colnames(build_diff), "diff", sep = "_")

build_nxt <- build %>%
  select(-c(season, name)) %>%
  mutate(across(everything(),  ~ coalesce(lead(.x), .x)))
colnames(build_nxt) <- paste(colnames(build_nxt), "nxt", sep = "_")

full_build <- cbind(build, build_diff, build_nxt)


## Analysis

# Basic predictive regression results
build_nxt <- full_build %>% filter(season != 2023)

nxt_stats_df <- tibble()
for (var in c("pitching", "stuff", "location", "era", "x_fip_2")) {
  reg <- lm(formula = paste0("x_fip_2_nxt ~", var), build_nxt)
  xfip_stats <- cbind(tidy(reg) %>% filter(term == var),
                 glance(reg) %>% select(r.squared, sigma)) 
  reg2 <- lm(paste0("era_nxt ~", var), build_nxt)
  era_stats <- cbind(tidy(reg2) %>% filter(term == var),
                      glance(reg2) %>% select(r.squared, sigma))
  era_stats$y_term <- "era"
  xfip_stats$y_term <- "x_fip_2"
  stats <- rbind(xfip_stats, era_stats)
  nxt_stats_df <- rbind(nxt_stats_df, stats)
}
nxt_stats_df <- nxt_stats_df %>%
  relocate(y_term, .before = term)

# Basic descriptive regression results
curr_stats_df <- tibble()
for (var in c("pitching", "stuff", "location", "x_fip_2")) {
  reg2 <- lm(paste0("era ~", var), build_nxt)
  era_stats <- cbind(tidy(reg2) %>% filter(term == var),
                     glance(reg2) %>% select(r.squared, sigma))
  curr_stats_df <- rbind(curr_stats_df, era_stats)
}

# Descriptive correlations 
corr_build <- full_build %>%
  select(-c(season, name, age, contains("_diff"), contains("_nxt"))) %>%
  relocate(era_2, .before = era)
curr_corrs_era <- cor(corr_build[-1], corr_build$era_2)

corr_build <- full_build %>%
  select(-c(season, name, age, contains("_diff"), contains("_nxt"))) %>%
  relocate(x_fip_2, .before = era)
curr_corrs_xfip <- cor(corr_build[-1], corr_build$x_fip_2)

# Predictive correlations
corr_build <- full_build %>%
  filter(season != 2023) %>%
  rename(era_2_fut = era_2_nxt) %>%
  select(-c(season, name, age, contains("_diff"), contains("_nxt"))) %>%
  relocate(era_2_fut, .before = era)
future_corrs_era <- cor(corr_build[-1], corr_build$era_2_fut)

corr_build <- full_build %>%
  filter(season != 2023) %>%
  rename(x_fip_2_fut = x_fip_2_nxt) %>%
  select(-c(season, name, age, contains("_diff"), contains("_nxt"))) %>%
  relocate(x_fip_2_fut, .before = era)
future_corrs_xfip <- cor(corr_build[-1], corr_build$x_fip_2_fut)