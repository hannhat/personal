################################################################################
# When the Church Votes Left
# Guadalupe Tuñón | Aug 2025
# This script contains the analysis on the PT's local activity.
# Generates: 
## Main paper: 
#### Table 3 - The Effect of JPII Bishops on the Local Expansion of the PT
################################################################################

# Basics
rm(list = ls()); set.seed(1234); options(scipen = 999)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, estimatr, modelsummary, here, purrr, tidyr, tibble)

# (Optional) Anchor project root once per machine:
# here::i_am("code/1. descriptives.R")

# Output directories
main_dir <- here::here("output", "main_paper")
app_dir <- here::here("output", "appendix")
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)


# Data

# Diodata ---
diodata <- readr::read_csv(here::here("data/diodata.csv"), show_col_types = FALSE) %>% 
  mutate(I_YEAR = ifelse(I_YEAR < 1978, 1978, I_YEAR))

# Affiliation DATA - muni level ---
panel.yr.muni <- readr::read_csv(here::here("data/panel_filiaco_mun_1970_pt.csv"),  show_col_types = FALSE) 

# merge, code treatment, and subset to relevant sample (omit archdioceses)
panel.yr.muni <- panel.yr.muni %>%
  left_join(diodata, by = "CE_1978") %>%
  mutate(
    I_cons = as.numeric(year > I_YEAR),
    cons   = as.numeric(year > JPIIAPT_YEAR)
  ) %>%
  filter(!is.na(cons), CE_TYPE != "A")


# --- PT office activation (threshold on new members) --------------------------

# robust start-year per municipality
data_off <- panel.yr.muni %>%
  dplyr::group_by(`cod.1970`) %>%
  dplyr::summarise(
    year_start = if (sum(new_members >= 5, na.rm = TRUE) > 0) {
      min(year[new_members >= 5], na.rm = TRUE)
    } else {
      NA_integer_
    },
    .groups = "drop"
  )

# join + correct had_office logic
panel.yr.muni <- panel.yr.muni %>%
  dplyr::left_join(data_off, by = "cod.1970") %>%
  dplyr::mutate(had_office = as.integer(!is.na(year_start) & year > year_start))


# --- Helpers ------------------------------------------------------------------
run_itt <- function(df, y) {
  estimatr::lm_robust(as.formula(paste0(y, " ~ I_cons")),
    data = df, clusters = CE_1978,
    fixed_effects = ~ cod.1970 + year,
    se_type = "stata"
  )
}

run_cace <- function(df, y) {
  estimatr::iv_robust(as.formula(paste0(y, " ~ cons | I_cons")),
    data = df, clusters = CE_1978,
    fixed_effects = ~ cod.1970 + year,
    se_type = "stata"
  )
}


fmt_mean <- function(x) sprintf("%.3f", x)
fmt_int  <- function(x) format(as.integer(x), big.mark = " ", trim = TRUE, scientific = FALSE)

make_row <- function(label, values_vec, cols) {
  tibble(term = label) %>%
    dplyr::bind_cols(tibble::as_tibble(setNames(as.list(values_vec), cols)))
}

# --- Column metadata (2 outcomes, same sample) --------------------------------

meta <- tibble(
  col              = sprintf("(%d)", 1:2),
  df               = list(panel.yr.muni, panel.yr.muni),
  y                = c("had_office", "new_members"),
  outcome_measure  = c("Binary", "Count")
) %>%
  mutate(stats = map2(df, y, \(d, yv) {
    keep <- !is.na(d[[yv]]) & !is.na(d[["I_cons"]]) & !is.na(d[["cod.1970"]]) & !is.na(d[["year"]])
    d2 <- d[keep, , drop = FALSE]
    tibble(
      outcome_mean = mean(d2[[yv]]),
      n_obs        = nrow(d2),
      n_cl         = dplyr::n_distinct(d2$CE_1978)
    )
  })) %>%
  unnest(stats)

# --- Fit models ---------------------------------------------------------------

CACE_models <- setNames(map2(meta$df, meta$y, run_cace), meta$col)
ITT_models  <- setNames(map2(meta$df, meta$y, run_itt),  meta$col)

# --- Panels as data frames ----------------------------------------------------

df_A <- modelsummary(
  CACE_models,
  coef_map = c("cons"   = "JPII Bishop"),
  gof_omit = ".*",
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = TRUE,
  output = "data.frame"
) %>%
  select(-part, -statistic)

df_B <- modelsummary(
  ITT_models,
  coef_map = c("I_cons" = "Mandated JPII Bishop"),
  gof_omit = ".*",
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = TRUE,
  output = "data.frame"
) %>%
  select(-part, -statistic)


# --- Bottom panel (computed) --------------------------------------------------

rows <- list(
  "Outcome Measure"          = meta$outcome_measure,
  "Outcome Mean"             = fmt_mean(meta$outcome_mean),
  "Municipality FE"          = rep("Yes", nrow(meta)),
  "Year FE"                  = rep("Yes", nrow(meta)),
  "Num. Obs."                = fmt_int(meta$n_obs),
  "Num. of Clusters"         = fmt_int(meta$n_cl)
)

bottom_rows <- purrr::imap_dfr(rows, ~ make_row(.y, .x, cols = meta$col))

# --- Stitch, render, save -----------------------------------------------------

sep_A <- tibble(term = "\\textbf{Panel A: 2SLS}")
sep_B <- tibble(term = "\\textbf{Panel B: Reduced Form (ITT)}")

full_df <- dplyr::bind_rows(sep_A, df_A, sep_B, df_B, bottom_rows)

modelsummary::datasummary_df(
  full_df,
  title  = "Table 3: The Effect of JPII Bishops on the Local Expansion of the PT",
  escape = FALSE,
  add_header_above = c(" " = 1, "Outcomes: PT Local Activity" = nrow(meta))
)

# Save
modelsummary::datasummary_df(
  full_df,
  output = file.path(main_dir, "Table3.tex"),
  title  = "Table 3: The Effect of JPII Bishops on the Local Expansion of the PT",
  escape = FALSE,  # allow bold panel labels
  add_header_above = c(" " = 1, "Outcomes: PT Local Activity" = nrow(meta))
)
