# Constant paths ----------------------------------------------------------
DATAROOT <- '~/personal/sabermetrics_playground/model_playground/data'

# Options -----------------------------------------------------------------
options(scipen = 999)
options(stringsAsFactors = FALSE)
options(mc.cores = 4)

# Path functions ----------------------------------------------------------
data_root <- DATAROOT
f.import <- function(path) {
  filepath <- paste0(data_root, '/import/', path)
}
f.datasets <- function(path) {
  filepath <- paste0(data_root, '/datasets/', path)
}
f.export <- function(path) {
  filepath <- paste0(data_root, '/export/', path)
}

# Libraries --------------------------------------------------------------
library(scales)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)
library(openxlsx)
library(readr)
library(haven)
library(purrr)
library(tidyr)
library(dplyr)
library(stargazer)
library(fs)
library(glue)
library(zoo)
library(janitor)
library(ivreg)
library(quantreg)
library(lmtest)
library(sandwich)
library(clubSandwich)
library(broom)