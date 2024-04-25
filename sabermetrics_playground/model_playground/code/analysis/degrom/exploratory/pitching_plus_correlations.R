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
  filter(count < 4)
