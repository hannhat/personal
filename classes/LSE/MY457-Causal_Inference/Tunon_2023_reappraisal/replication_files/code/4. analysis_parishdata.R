################################################################################
# When the Church Votes Left
# Guadalupe Tuñón | Aug 2025
# This script contains the parish data analysis.
# Generates: 
## Main paper: 
#### Table 2 - The Effect of JPII Bishops on Priest Turnover
################################################################################

# Basics
rm(list = ls()); set.seed(1234); options(scipen = 999)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, estimatr, modelsummary, here, purrr, tidyr, tibble)

# Paths
# (optional, run once per machine)
# here::i_am("code/4. analysis_parishdata.R")  

# Output directories
main_dir <- here::here("output/main_paper")
app_dir  <- here::here("output/appendix")
if (!dir.exists(main_dir)) dir.create(main_dir, recursive = TRUE)
if (!dir.exists(app_dir))  dir.create(app_dir,  recursive = TRUE)

# Data -------------------------------------------------------------------------
diodata <- readr::read_csv(here::here("data/diodata.csv"), show_col_types = FALSE)

muni_priests_data <- readr::read_csv(here::here("data/muni_parish_database.csv"),
                                     show_col_types = FALSE)

# Merge & construct treatments
panel.yr.muni <- muni_priests_data %>%
  left_join(diodata, by = "CE_1978") %>%
  mutate(
    I_cons = as.numeric(AC_year > I_YEAR),
    cons   = as.numeric(AC_year > JPIIAPT_YEAR)
  )

# RS subsample (kept raw; column filters applied later)
panel.yr.muni.RS <- filter(panel.yr.muni, UF == "RS")


# Helpers ----------------------------------------------------------------------
run_itt <- function(df, y) {
  estimatr::lm_robust(
    as.formula(paste0(y, " ~ I_cons")),
    data = df, clusters = CE_1978,
    fixed_effects = ~ cod.1970 + AC_year,
    se_type = "stata"
  )
}
run_cace <- function(df, y) {
  estimatr::iv_robust(
    as.formula(paste0(y, " ~ cons | I_cons")),
    data = df, clusters = CE_1978,
    fixed_effects = ~ cod.1970 + AC_year,
    se_type = "stata"
  )
}
fmt_mean <- function(x) sprintf("%.3f", x)
fmt_int  <- function(x) format(as.integer(x), big.mark = " ", trim = TRUE, scientific = FALSE)

keep_cols <- function(df, k) {
  wanted <- c("term", sprintf("(%d)", seq_len(k)))
  dplyr::select(df, dplyr::any_of(wanted))
}

# Column datasets (match paper layout) -----------------------------------------
# Base full sample (drop archdioceses & ensure pre-1978 parishes)
panel_all <- panel.yr.muni %>%
  filter(CE_TYPE != "A", parishes_1965 > 0, parishes_1970 > 0, parishes_1977 > 0)

cols_named <- list(
  `(1)` = panel_all,                                                           # Binary — All
  `(2)` = panel_all,                                                           # Share  — All
  `(3)` = filter(panel_all, parishes_1977 == 1),                               # Binary — Single
  `(4)` = filter(panel_all, parishes_1977 == 1, parishes_1977_order == 0),     # Binary — Single (Secular)
  `(5)` = filter(panel.yr.muni.RS, parishes_1970 > 0, parishes_1977 > 0,
                 AC_year > 1970, prog_priest_77 == 1),                         # Binary — RS Progr.
  `(6)` = filter(panel.yr.muni.RS, parishes_1970 > 0, parishes_1977 > 0,
                 AC_year > 1970, prog_priest_77 == 0)                          # Binary — RS Non progr.
)

ys <- c("turnover_priest_01", "turnover_priest_sh",
        "turnover_priest_01", "turnover_priest_01",
        "turnover_priest_01", "turnover_priest_01")

# Metadata block (computed once) -----------------------------------------------
meta <- tibble(
  col             = names(cols_named),
  df              = unname(cols_named),
  y               = ys,
  outcome_measure = if_else(ys == "turnover_priest_sh", "Share", "Binary"),
  sample_parishes = c("All","All","Single","Single","All","All"),
  priest_type     = c("All","All","All","Secular","Progr.","Non progr."),
  states          = c("All","All","All","All","RS","RS")
) |>
  mutate(stats = map2(df, y, \(d, yv) {
    keep <- !is.na(d[[yv]]) & !is.na(d[["I_cons"]]) & !is.na(d[["cod.1970"]]) & !is.na(d[["AC_year"]])
    d2 <- d[keep, , drop = FALSE]
    tibble(
      outcome_mean = mean(d2[[yv]]),
      n_obs        = nrow(d2),
      n_cl         = dplyr::n_distinct(d2$CE_1978)
    )
  })) |>
  unnest(stats)

# Fit models (using meta) ------------------------------------------------------
CACE_models <- setNames(map2(meta$df, meta$y, run_cace), meta$col)
ITT_models  <- setNames(map2(meta$df, meta$y, run_itt),  meta$col)

# Panels as data frames (robust to modelsummary version diffs) -----------------

df_A <- modelsummary(
  CACE_models,
  coef_map = c("cons" = "JPII Bishop"),
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

# Bottom rows (from meta) ------------------------------------------------------
rows <- list(
  "Outcome Measure"         = meta$outcome_measure,
  "Sample (Nr of Parishes)" = meta$sample_parishes,
  "Sample (Priest type)"    = meta$priest_type,
  "Sample (States)"         = meta$states,
  "Outcome Mean"            = fmt_mean(meta$outcome_mean),
  "Municipality FE"         = rep("Yes", nrow(meta)),
  "Yearbook FE"             = rep("Yes", nrow(meta)),
  "Num. Obs."               = fmt_int(meta$n_obs),
  "Num. of Clusters"        = fmt_int(meta$n_cl)
)

make_row <- function(label, values_vec, cols = meta$col) {
  tibble(term = label) %>%
    bind_cols(as_tibble(setNames(as.list(values_vec), cols)))
}

bottom_rows <- purrr::imap_dfr(rows, ~ make_row(.y, .x))

# Stitch, render, save ---------------------------------------------------------
sep_A  <- tibble(term = "\\textbf{Panel A: 2SLS}")
sep_B  <- tibble(term = "\\textbf{Panel B: Reduced Form (ITT)}")
full_df <- bind_rows(sep_A, df_A, sep_B, df_B, bottom_rows)

# Preview to console
modelsummary::datasummary_df(
  full_df,
  #output = "latex",
  title  = "Table 2: The Effect of JPII Bishops on Priest Turnover",
  escape = FALSE,
  add_header_above = c(" " = 1, "Outcome: Priest Turnover" = nrow(meta))
) 

# Save to file
modelsummary::datasummary_df(
  full_df,
  output = file.path(main_dir, "Table2.tex"),
  title  = "Table 2: The Effect of JPII Bishops on Priest Turnover",
  escape = FALSE,
  add_header_above = c(" " = 1, "Outcome: Priest Turnover" = nrow(meta))
)

