# =============================================================================
# main_file_helpers.R
# Source from repo root: source("analysis_code/main_file_helpers.R")
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, ggplot2, estimatr, broom,
               here, rlang, DIDmultiplegtDYN, polars,
               sf, geobr, rmapshaper, ggpubr, scales,
               modelsummary, purrr, tibble, gt)

# =============================================================================
# Layer 1: Data Loading & Preparation
# =============================================================================

load_data <- function() {
  list(
    munidata  = readr::read_csv(here::here("replication_files/data/munidata.csv"),
                                show_col_types = FALSE),
    diodata   = readr::read_csv(here::here("replication_files/data/diodata.csv"),
                                show_col_types = FALSE),
    electoral = readr::read_csv(here::here("replication_files/data/IPEA_electoral_data.csv"),
                                show_col_types = FALSE)
  )
}

prepare_diodata <- function(diodata) {
  diodata %>%
    dplyr::mutate(
      M_YEAR_ND_REP    = 1978 + pmax(0, round(75 - round(AGEOCT78, digits = 0))),
      death_dum_REP    = dplyr::if_else(out_method_0 == "DEATH",    1L, 0L),
      transfer_dum_REP = dplyr::if_else(out_method_0 == "TRANSFER", 1L, 0L)
    )
}

# munidata joined with diodata REP vars only (no electoral); needed for balance tests
prepare_munidata <- function(munidata, diodata_REP) {
  munidata %>%
    dplyr::mutate(parish_1980 = dplyr::if_else(is.na(cod.1980), NA_real_, parish_1980)) %>%
    dplyr::left_join(
      diodata_REP %>% dplyr::select(CE_code, M_YEAR_ND_REP, death_dum_REP, transfer_dum_REP),
      by = "CE_code"
    )
}

prepare_muni_panel <- function(munidata, diodata_REP, electoral) {
  prepare_munidata(munidata, diodata_REP) %>%
    dplyr::left_join(electoral, by = "cod.2010") %>%
    dplyr::mutate(
      Retirement_Years     = dplyr::if_else(I_YEAR        < election, I_YEAR        - 1978, election - 1978),
      Exposure             = dplyr::if_else(JPIIAPT_YEAR  < election, JPIIAPT_YEAR  - 1978, election - 1978),
      Retirement_Years_REP = dplyr::if_else(M_YEAR_ND_REP < election, M_YEAR_ND_REP - 1978, election - 1978),
      TFR                  = dplyr::if_else(JPIIAPT_YEAR  < election, election - JPIIAPT_YEAR, 0L),
      bish_age             = AGEOCT78 + election - 1978,
      experience           = as.numeric(as.Date("1978-10-16") - as.Date(first_in_date_0)) / 365 + election - 1978,
      experience_JPII      = dplyr::if_else(JPIIELEV_YEAR > election, 0, as.numeric(election - JPIIELEV_YEAR))
    )
}

prepare_dio_panel <- function(muni_panel, diodata_REP) {
  muni_panel %>%
    dplyr::group_by(CE_code, cargo, election, partido) %>%
    dplyr::summarise(
      votos       = sum(votos,     na.rm = TRUE),
      votos_total = sum(votos.sum, na.rm = TRUE),
      vote.sh     = 100 * votos / votos_total,
      .groups     = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(diodata_REP, by = "CE_code") %>%
    dplyr::mutate(
      Retirement_Years     = dplyr::if_else(I_YEAR        < election, I_YEAR        - 1978, election - 1978),
      Exposure             = dplyr::if_else(JPIIAPT_YEAR  < election, JPIIAPT_YEAR  - 1978, election - 1978),
      Retirement_Years_REP = dplyr::if_else(M_YEAR_ND_REP < election, M_YEAR_ND_REP - 1978, election - 1978),
      TFR                  = dplyr::if_else(JPIIAPT_YEAR  < election, election - JPIIAPT_YEAR, 0L)
    )
}

# Prepares the DiD-ready dataset; called internally by run_did_dynamic
prepare_did_data <- function(muni_panel, year_min = 1989, year_max = 2002) {
  muni_panel %>%
    dplyr::filter(election >= year_min, election <= year_max, partido == "PT") %>%
    dplyr::select(cod.2010, CE_code, I_YEAR, M_YEAR_ND_REP,
                  JPIIAPT_YEAR, JPIIELEV_YEAR, vote.sh, election, UF) %>%
    dplyr::mutate(
      time                 = (election - 1989L) %/% 4L + 1L,
      UF_int               = as.integer(as.factor(UF)),
      mandatory_retirement = as.integer(election > I_YEAR),
      actual_retirement    = as.integer(election > JPIIAPT_YEAR)
    )
}

# =============================================================================
# Layer 2: Estimation Functions
# =============================================================================

# Single unified estimator: OLS when instrument = NULL, IV otherwise.
# state_fe toggles fixed_effects = ~UF; cluster_var = NULL for no clustering.
run_regression <- function(df, outcome, treatment, instrument = NULL,
                           controls = NULL,
                           state_fe = TRUE, cluster_var = "CE_1978") {
  ctrl_str <- if (!is.null(controls) && length(controls) > 0) {
    paste0(" + ", paste(controls, collapse = " + "))
  } else {
    ""
  }
  fml <- if (is.null(instrument)) {
    as.formula(paste0(outcome, " ~ ", treatment, ctrl_str))
  } else {
    as.formula(paste0(outcome, " ~ ", treatment, ctrl_str, " | ", instrument, ctrl_str))
  }
  base_args <- list(formula = fml, data = df)
  if (state_fe) base_args$fixed_effects <- ~UF
  estimator <- if (is.null(instrument)) estimatr::lm_robust else estimatr::iv_robust

  if (!is.null(cluster_var)) {
    cl_sym <- rlang::sym(cluster_var)
    rlang::inject(estimator(!!!base_args, clusters = !!cl_sym))
  } else {
    do.call(estimator, base_args)
  }
}

# Runs run_regression for each election year. extra_filter accepts a bare
# expression evaluated in the dataframe context, e.g. extra_filter = I_YEAR < election.
# Returns list(models = <named model list>, meta = <tibble with per-year stats>).
run_election_panel <- function(panel_df,
                               outcome,
                               treatment,
                               instrument     = NULL,
                               controls       = NULL,
                               years          = c(1989L, 1994L, 1998L, 2002L),
                               partido_filter = "PT",
                               cargo_filter   = "Presidente",
                               include_arch   = FALSE,
                               extra_filter   = NULL,
                               state_fe       = TRUE,
                               cluster_var    = "CE_1978") {
  extra_q   <- rlang::enquo(extra_filter)
  col_names <- paste0("(", years, ")")
  models    <- setNames(vector("list", length(years)), col_names)
  meta_rows <- vector("list", length(years))

  for (i in seq_along(years)) {
    yr <- years[i]
    df <- panel_df %>%
      dplyr::filter(election == yr, partido == partido_filter, cargo == cargo_filter)

    if (!include_arch)                df <- df %>% dplyr::filter(CE_TYPE != "A")
    if (!rlang::quo_is_null(extra_q)) df <- df %>% dplyr::filter(!!extra_q)

    models[[col_names[i]]] <- run_regression(
      df, outcome, treatment, instrument, controls, state_fe, cluster_var
    )
    meta_rows[[i]] <- tibble::tibble(
      col          = col_names[i],
      outcome_mean = mean(df[[outcome]], na.rm = TRUE),
      n_obs        = nrow(df),
      n_cl         = if (!is.null(cluster_var)) dplyr::n_distinct(df[[cluster_var]]) else NA_integer_
    )
  }

  list(models = models, meta = dplyr::bind_rows(meta_rows))
}

# Assembles a one or two panel regression specification table. 
#header/note follow the same convention as build_balance_table().
build_panel_table <- function(cace_result,
                              itt_result        = NULL,
                              cace_coef_map,
                              itt_coef_map      = NULL,
                              first_panel_title  = "Panel A: 2SLS (LATE)",
                              second_panel_title = "Panel B: Reduced Form (ITT)",
                              header = NULL,
                              note   = "Standard errors clustered at the diocese level in parentheses.") {
  meta       <- cace_result$meta
  col_names  <- meta$col
  yr_names   <- gsub("[()]", "", col_names)
  rename_map <- setNames(col_names, yr_names)

  ms_df <- function(models, coef_map) {
    modelsummary::modelsummary(
      models,
      coef_map  = coef_map,
      gof_omit  = ".*",
      estimate  = "{estimate}{stars}",
      statistic = "({std.error})",
      stars     = c("+" = 0.1, "*" = 0.05, "**" = 0.01),
      output    = "data.frame"
    ) %>%
      # Blank label on SE rows before dropping the statistic column
      dplyr::mutate(term = dplyr::if_else(statistic == "estimate", term, "")) %>%
      dplyr::select(-dplyr::any_of(c("part", "statistic"))) %>%
      dplyr::rename(!!!rename_map)
  }

  df_A <- ms_df(cace_result$models, cace_coef_map)

  blank_row <- function(label) {
    dplyr::bind_cols(
      tibble::tibble(term = label),
      tibble::as_tibble(setNames(as.list(rep("", length(yr_names))), yr_names))
    )
  }
  stat_row <- function(label, values) {
    dplyr::bind_cols(
      tibble::tibble(term = label),
      tibble::as_tibble(setNames(as.list(as.character(values)), yr_names))
    )
  }

  n_cl_fmt   <- if (anyNA(meta$n_cl)) rep("—", nrow(meta)) else format(meta$n_cl, big.mark = ",")
  stats_rows <- dplyr::bind_rows(
    stat_row("Outcome Mean",     sprintf("%.2f", meta$outcome_mean)),
    stat_row("Num. Obs.",        format(meta$n_obs, big.mark = ",")),
    stat_row("Num. of Clusters", n_cl_fmt)
  )

  if (is.null(itt_result)) {
    full_df   <- dplyr::bind_rows(blank_row(first_panel_title), df_A, stats_rows)
    idx_A     <- 1L
    idx_stats <- 1L + nrow(df_A) + 1L
    hdr_rows  <- idx_A
  } else {
    df_B      <- ms_df(itt_result$models, itt_coef_map)
    full_df   <- dplyr::bind_rows(
      blank_row(first_panel_title), df_A,
      blank_row(second_panel_title), df_B,
      stats_rows
    )
    idx_A     <- 1L
    idx_B     <- 1L + nrow(df_A) + 1L
    idx_stats <- idx_B + nrow(df_B) + 1L
    hdr_rows  <- c(idx_A, idx_B)
  }

  pval_legend   <- "+ p < 0.1, * p < 0.05, ** p < 0.01."
  full_footnote <- if (!is.null(note) && nzchar(note)) paste(pval_legend, note) else pval_legend

  tbl <- gt::gt(full_df) %>%
    gt::cols_label(term = "") %>%
    gt::tab_style(
      style     = list(
        gt::cell_text(weight = "bold"),
        gt::cell_borders(sides = "top", color = "#000000", weight = gt::px(1))
      ),
      locations = gt::cells_body(rows = hdr_rows)
    ) %>%
    gt::tab_style(
      style     = gt::cell_borders(sides = "top", color = "#000000", weight = gt::px(1)),
      locations = gt::cells_body(rows = idx_stats)
    ) %>%
    gt::tab_style(
      style     = gt::cell_text(align = "center"),
      locations = list(
        gt::cells_column_labels(columns = -term),
        gt::cells_body(columns = -term)
      )
    ) %>%
    gt::tab_style(
      style     = gt::cell_text(align = "left"),
      locations = list(
        gt::cells_column_labels(columns = term),
        gt::cells_body(columns = term)
      )
    ) %>%
    gt::tab_options(
      table.border.top.style            = "solid",
      table.border.top.width            = gt::px(2),
      table.border.top.color            = "#000000",
      table.border.bottom.style         = "solid",
      table.border.bottom.width         = gt::px(2),
      table.border.bottom.color         = "#000000",
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "#000000",
      table_body.hlines.style           = "none",
      table_body.border.bottom.style    = "none"
    ) %>%
    gt::tab_footnote(footnote = full_footnote)

  if (!is.null(header)) tbl <- gt::tab_header(tbl, title = header)

  tbl
}

# Formats the output of run_balance_test() as a three-panel gt table matching
# the structure of Table B1 in Tuñón (2026). panel_A/B/C_vars are named
# character vectors mapping term codes (as returned by run_balance_test) to
# display labels, e.g. c("IR" = "Resident Priests"). Panel name strings and
# the header/note arguments follow the same convention as build_panel_table().
build_balance_table <- function(balance_df,
                                panel_A_vars,
                                panel_B_vars,
                                panel_C_vars,
                                panel_A_name = "Panel A: Diocese Characteristics",
                                panel_B_name = "Panel B: Social Variables",
                                panel_C_name = "Panel C: Electoral Variables",
                                header = NULL,
                                note   = "Standard errors clustered at the diocese level for municipality-level rows. All specifications estimated by OLS with state fixed effects.") {

  fmt_panel <- function(df, vars) {
    rows <- df[match(names(vars), df$term), ]
    tibble::tibble(
      no        = as.character(seq_len(nrow(rows))),
      label     = unname(vars),
      estimate  = rows$estimate,
      std.error = rows$std.error,
      p.value   = rows$p.value,
      N         = rows$N
    )
  }

  hdr_row <- function(name) {
    tibble::tibble(no = "", label = name,
                   estimate = NA_real_, std.error = NA_real_,
                   p.value  = NA_real_, N = NA_integer_)
  }

  df_A <- fmt_panel(balance_df, panel_A_vars)
  df_B <- fmt_panel(balance_df, panel_B_vars)
  df_C <- fmt_panel(balance_df, panel_C_vars)

  full_df <- dplyr::bind_rows(
    hdr_row(panel_A_name), df_A,
    hdr_row(panel_B_name), df_B,
    hdr_row(panel_C_name), df_C
  )

  idx_A <- 1L
  idx_B <- 1L + nrow(df_A) + 1L
  idx_C <- idx_B + nrow(df_B) + 1L

  pval_legend   <- "+ p < 0.1, * p < 0.05, ** p < 0.01."
  full_footnote <- if (!is.null(note) && nzchar(note)) paste(pval_legend, note) else pval_legend

  tbl <- gt::gt(full_df) %>%
    gt::cols_label(
      no        = "",
      label     = "",
      estimate  = "Estimate",
      std.error = "Std. Error",
      p.value   = "p-value",
      N         = "N"
    ) %>%
    # Bold panel header rows + hairline above each
    gt::tab_style(
      style     = list(
        gt::cell_text(weight = "bold"),
        gt::cell_borders(sides = "top", color = "#000000", weight = gt::px(1))
      ),
      locations = gt::cells_body(rows = c(idx_A, idx_B, idx_C))
    ) %>%
    gt::fmt_number(columns  = c(estimate, std.error, p.value), decimals = 3) %>%
    gt::fmt_integer(columns = N) %>%
    gt::sub_missing(columns = dplyr::everything(), missing_text = "") %>%
    gt::cols_align(align = "right", columns = c(estimate, std.error, p.value, N)) %>%
    gt::cols_align(align = "left",  columns = c(no, label)) %>%
    gt::cols_width(no ~ gt::px(25)) %>%
    gt::tab_options(
      table.border.top.style            = "solid",
      table.border.top.width            = gt::px(2),
      table.border.top.color            = "#000000",
      table.border.bottom.style         = "solid",
      table.border.bottom.width         = gt::px(2),
      table.border.bottom.color         = "#000000",
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "#000000",
      table_body.hlines.style           = "none",
      table_body.border.bottom.style    = "none"
    ) %>%
    gt::tab_footnote(footnote = full_footnote)

  if (!is.null(header)) tbl <- gt::tab_header(tbl, title = header)

  tbl
}

# Runs balance regressions of `response` on diocese/municipality characteristics.
# Panels A (diocese-level), B (social, muni-level), C (electoral, muni-level).
# state_fe = FALSE removes the ~UF fixed effect from muni-level regressions.
# exclusion filters out dioceses by out_method_0 from Panel A only.
run_balance_test <- function(diodata_REP, munidata_REP,
                             response  = "I_YEAR",
                             exclusion = "",
                             state_fe  = TRUE) {
  if (nchar(exclusion) > 0) {
    diodata2 <- dplyr::filter(diodata_REP, !(out_method_0 %in% exclusion))
  } else {
    diodata2 <- diodata_REP
  }

  fe_formula   <- if (state_fe) ~UF else NULL
  response_lhs <- paste0(response, " ~ ")
  dio_sub      <- subset(diodata2,     CE_TYPE != "A")
  muni_sub     <- subset(munidata_REP, CE_TYPE != "A")

  fit_dio <- function(v) {
    tidy <- broom::tidy(
      estimatr::lm_robust(as.formula(paste0(response_lhs, v)), data = dio_sub)
    )
    tidy[tidy$term == v, c("term", "estimate", "std.error", "p.value")]
  }

  fit_muni <- function(v) {
    base_args <- list(
      formula = as.formula(paste0(response_lhs, v)),
      data    = muni_sub
    )
    if (state_fe) base_args$fixed_effects <- ~UF
    cl_sym <- rlang::sym("CE_1978")
    tidy <- broom::tidy(rlang::inject(estimatr::lm_robust(!!!base_args, clusters = !!cl_sym)))
    tidy[tidy$term == v, c("term", "estimate", "std.error", "p.value")]
  }

  # Panel A: Diocese characteristics
  dio_vars <- c("IR", "NIR", "INR", "DIAC", "REL",
                "CASASREL_MASC", "CASASREL_FEM", "NR_PAR", "NR_MUNI",
                "recursos_nro", "recursos_total_activities")

  rel_diolevel      <- do.call(rbind, lapply(dio_vars, fit_dio))
  rel_diolevel$N    <- 189L
  row.names(rel_diolevel) <- NULL

  rel_munilevel     <- fit_muni("parish_1980")
  rel_munilevel$N   <- sum(!is.na(munidata_REP$parish_1980))
  row.names(rel_munilevel) <- NULL

  panel_A <- rbind(rel_diolevel, rel_munilevel)

  # Panel B: Social characteristics
  social_vars <- c("pop.tot.1970", "pop.urb.1970", "pop.rur.1970",
                   "share_catolico_1970", "share_evangelico_1970",
                   "share_catolico_1978", "share_evangelico_1978", "growth_evangelica")

  panel_B      <- do.call(rbind, lapply(social_vars, fit_muni))
  panel_B$N    <- vapply(social_vars, function(v) sum(!is.na(munidata_REP[[v]])), integer(1))
  row.names(panel_B) <- NULL

  # Panel C: Electoral characteristics
  elect_vars <- c("ELEITORADO_1976", "MDB1976_SH", "ARENA1976_SH",
                  "ELEITORADO_1972", "MDB1972_SH", "ARENA1972_SH")

  panel_C      <- do.call(rbind, lapply(elect_vars, fit_muni))
  panel_C$N    <- vapply(elect_vars, function(v) sum(!is.na(munidata_REP[[v]])), integer(1))
  row.names(panel_C) <- NULL

  rbind(panel_A, panel_B, panel_C)
}

# Thin wrapper around did_multiplegt_dyn. Prepares the DiD data internally
# from muni_panel; year_min/year_max default to the presidential election window.
run_did_dynamic <- function(muni_panel,
                            treatment       = "mandatory_retirement",
                            outcome         = "vote.sh",
                            group           = "cod.2010",
                            time            = "time",
                            cluster         = "CE_code",
                            trends_nonparam = NULL,
                            effects         = 3,
                            placebo         = 1,
                            year_min        = 1989L,
                            year_max        = 2002L) {
  did_data <- prepare_did_data(muni_panel, year_min = year_min, year_max = year_max)

  DIDmultiplegtDYN::did_multiplegt_dyn(
    did_data,
    group           = group,
    time            = time,
    cluster         = cluster,
    treatment       = treatment,
    outcome         = outcome,
    trends_nonparam = trends_nonparam,
    effects         = effects,
    placebo         = placebo,
    graph_off = TRUE
  )
}

# Permutation test for whether mandated retirement year (I_YEAR) is randomly
# assigned across states. Under the null, any permutation of I_YEAR across
# dioceses is equally likely; the test statistic is the one-way ANOVA F-statistic
# for I_YEAR ~ UF_dio. Returns a ggplot of the permutation distribution with a
# red line at the observed F; the one-sided p-value appears in the subtitle.
state_instrument_ra_test <- function(diodata_REP, B = 1000) {
  base_f <- summary(aov(I_YEAR ~ UF_dio, data = diodata_REP))[[1]][["F value"]][1]

  f_dist <- numeric(B)
  for (j in seq_len(B)) {
    perm      <- dplyr::mutate(diodata_REP, I_YEAR = sample(I_YEAR))
    f_dist[j] <- summary(aov(I_YEAR ~ UF_dio, data = perm))[[1]][["F value"]][1]
  }

  p_val <- mean(f_dist >= base_f)

  ggplot2::ggplot(data.frame(f = f_dist), ggplot2::aes(x = f)) +
    ggplot2::geom_density(fill = "grey85", color = "grey40") +
    ggplot2::geom_vline(xintercept = base_f, color = "red", linewidth = 0.8) +
    ggplot2::labs(
      x        = "F-statistic (permutation distribution)",
      y        = "Density",
      subtitle = sprintf("Observed F = %.3f  |  p = %.3f  (B = %d)", base_f, p_val, B)
    ) +
    ggplot2::theme_minimal()
}

# Permutation test for whether an outcome (default: PT vote share) varies across
# states more than expected by chance, using municipality-level data. Mirrors
# state_ra_test but operates on muni_panel. When multiple election years are
# pooled, election is added as a covariate and the F-statistic for UF is
# extracted from the two-way ANOVA table; for a single year a one-way ANOVA is
# used. Returns a ggplot with the permutation distribution and a red line at the
# observed F; the one-sided p-value appears in the subtitle.
state_vote_ra_test <- function(muni_panel,
                                outcome        = "vote.sh",
                                years          = c(1989L, 1994L, 1998L, 2002L),
                                partido_filter = "PT",
                                cargo_filter   = "Presidente",
                                include_arch   = FALSE,
                                B              = 1000) {
  df <- muni_panel %>%
    dplyr::filter(
      election %in% years,
      partido  == partido_filter,
      cargo    == cargo_filter,
      !is.na(.data[[outcome]])
    )
  if (!include_arch) df <- dplyr::filter(df, CE_TYPE != "A")

  multi_year <- length(years) > 1

  get_f <- function(data) {
    if (multi_year) {
      fml <- as.formula(paste0(outcome, " ~ UF + factor(election)"))
      summary(aov(fml, data = data))[[1]]["UF", "F value"]
    } else {
      fml <- as.formula(paste0(outcome, " ~ UF"))
      summary(aov(fml, data = data))[[1]][["F value"]][1]
    }
  }

  base_f <- get_f(df)

  out_sym <- rlang::sym(outcome)
  f_dist  <- numeric(B)
  for (j in seq_len(B)) {
    perm      <- dplyr::mutate(df, !!out_sym := sample(.data[[outcome]]))
    f_dist[j] <- get_f(perm)
  }

  p_val <- mean(f_dist >= base_f)

  ggplot2::ggplot(data.frame(f = f_dist), ggplot2::aes(x = f)) +
    ggplot2::geom_density(fill = "grey85", color = "grey40") +
    ggplot2::geom_vline(xintercept = base_f, color = "red", linewidth = 0.8) +
    ggplot2::labs(
      x        = "F-statistic (permutation distribution)",
      y        = "Density",
      subtitle = sprintf("Observed F = %.3f  |  p = %.3f  (B = %d)", base_f, p_val, B)
    ) +
    ggplot2::theme_minimal()
}

# =============================================================================
# Layer 3: First-Stage Visualization
# =============================================================================

# Computes first-stage estimates and F-statistics across years.
# instrument_var = "I_YEAR" replicates the original; "M_YEAR_ND_REP" gives the
# age-only instrument version.
compute_first_stage <- function(diodata_REP,
                                instrument_var = "I_YEAR",
                                years_main     = c(1989L, 1994L, 1998L, 2002L)) {
  years_all <- seq(1979L, max(diodata_REP[[instrument_var]], na.rm = TRUE))
  idx       <- diodata_REP$CE_TYPE != "A"

  lapply(years_all, function(i) {
    I_years <- dplyr::if_else(diodata_REP[[instrument_var]][idx] < i,
                              diodata_REP[[instrument_var]][idx] - 1978L,
                              i - 1978L)
    Expos   <- dplyr::if_else(diodata_REP$JPIIAPT_YEAR[idx] < i,
                              diodata_REP$JPIIAPT_YEAR[idx] - 1978L,
                              i - 1978L)
    fit     <- estimatr::lm_robust(Expos ~ I_years)
    out     <- broom::tidy(fit)[2L, c("estimate", "conf.low", "conf.high")]
    fstat   <- unname(fit$fstatistic[1L])
    cbind(out, fstat = fstat, year = i)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(outcome_years = !(year %in% years_main))
}

plot_first_stage_scatter <- function(diodata_REP, instrument_var = "I_YEAR") {
  diodata_REP %>%
    dplyr::filter(CE_TYPE != "A") %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[instrument_var]], y = JPIIAPT_YEAR)) +
    ggplot2::geom_jitter(alpha = .4, width = .5, height = .5, size = 2, color = "grey60") +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "black",
                         linetype = "dashed", linewidth = .2) +
    ggplot2::geom_smooth(method = lm, color = "black", se = FALSE, linewidth = .8) +
    ggplot2::labs(x = "Mandated Retirement", y = "First JPII Appointment") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = .5))
}

plot_first_stage_coefs <- function(first_stage_df) {
  ggplot2::ggplot(first_stage_df,
                  ggplot2::aes(x = year, y = estimate, color = outcome_years)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0) +
    ggplot2::scale_color_grey() +
    ggplot2::labs(x = "Year", y = "Estimate") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = .5))
}

plot_first_stage_fstats <- function(first_stage_df) {
  ggplot2::ggplot(first_stage_df,
                  ggplot2::aes(x = year, y = fstat)) +
    ggplot2::geom_line(color = "grey40") +
    ggplot2::geom_point(ggplot2::aes(color = outcome_years)) +
    ggplot2::scale_color_grey() +
    ggplot2::labs(x = "Year", y = "F-Statistic") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = .5))
}
