# ============================================================
# 00_setup.R
# Install and load all required packages
# Run this first before any other script
# ============================================================

# --- Package installation ---
packages <- c(
  "baseballr",     # FanGraphs/Baseball Savant/Retrosheet data access
  "tidyverse",     # dplyr, ggplot2, tidyr, purrr, stringr, readr
  "rvest",         # HTML scraping for FanGraphs projection pages
  "httr",          # HTTP requests
  "jsonlite",      # JSON parsing
  "janitor",       # clean_names(), data cleaning helpers
  "glue",          # string interpolation
  "fs",            # file system helpers
  "here",          # project-relative paths
  "progressr",     # progress bars for long scraping loops
  "furrr",         # parallel purrr (optional, speeds up scraping)
  "ggrepel",       # non-overlapping labels in ggplot2
  "patchwork",     # combine ggplot2 plots
  "lme4",          # mixed effects models for EDA
  "broom",         # tidy model outputs
  "broom.mixed",   # tidy lme4 outputs
  "corrplot"       # correlation plots
)

install.packages(packages, dependencies = TRUE)


# baseballr from CRAN (stable version)
# If you need the dev version: remotes::install_github("BillPetti/baseballr")

lapply(packages, library, character.only = TRUE)

# --- Directory structure ---
dirs <- c(
  "data/raw/projections",
  "data/raw/standings",
  "data/raw/rosters",
  "data/processed",
  "outputs/figures",
  "outputs/tables"
)
fs::dir_create(dirs)

message("Setup complete. All packages loaded and directories created.")