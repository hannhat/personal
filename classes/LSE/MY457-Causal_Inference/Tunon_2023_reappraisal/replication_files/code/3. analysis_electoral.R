################################################################################
# When the Church Votes Left
# Guadalupe Tuñón | Aug 2025
# This script produces the main analysis in Table 1 and all the associated 
# robustness checks
# Produces:
## Main paper:
#### Table 1  - Effects of the Length of Exposure to Progressive Bishops on the PT's Presidential Vote Share
## Supplementary appendix: 
#### Table C1 - Effects of the Length of Exposure to Progressive Bishops on the PT's Presidential Vote Share - Diocese-level Analysis
#### Table C2 - Effects of the Length of Exposure to Progressive Bishops on the PT's Presidential Vote Share - Results Including Archdioceses
#### Table C3 - Effects of the Length of Exposure to Progressive Bishops on the PT's Congressional Vote Share
#### Table C4 - Effects of Bishop Age and Experience on the PT's Presidential Vote Share
#### Table C5 - Effects of the Length of Exposure to Progressive Bishops on the PT's Presidential Vote Share - Only Dioceses With Mandated Retirement Prior to the Election
#### Table C6 - Effects of the Length of Exposure to Progressive Bishops on the PT's Presidential Vote Share - Flexible Treatment Effects
#### Table C7 - Robustness to the Exclusion of Non-Progressive pre-JPII Bishops
#### Table C8 - Only Bishops Elevated by JPII Classified as conservative bishops
#### Table D1 - Effect of Exposure to Progressive Bishops for All Left Wing Parties
################################################################################

# Basics
rm(list = ls())
set.seed(1234)
options(scipen = 999)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, ggplot2, estimatr, modelsummary, broom,
               interflex, grid, gridExtra, here, purrr, tinytable)

# (Optional) Anchor project root once per machine:
# here::i_am("code/3. analysis_electoral.R")
options(modelsummary_warning_latex = FALSE)

# Output directories
main_dir <- here::here("output/main_paper")
app_dir  <- here::here("output/appendix")
if (!dir.exists(main_dir)) dir.create(main_dir, recursive = TRUE)
if (!dir.exists(app_dir))  dir.create(app_dir,  recursive = TRUE)

# Data
munidata <- readr::read_csv(here::here("data/munidata.csv"), show_col_types = FALSE)
diodata  <- readr::read_csv(here::here("data/diodata.csv"),  show_col_types = FALSE)
electoral <- readr::read_csv(here::here("data/IPEA_electoral_data.csv"), show_col_types = FALSE)

# Municipality-level panel -----------------------------------------------------
munidata_panel_election <- dplyr::left_join(munidata, electoral, by = "cod.2010")
rm(electoral)

# Diocese-level panel with vote shares ----------------------------------------
diodata_panel_election <- munidata_panel_election %>%
  dplyr::group_by(CE_code, cargo, election, partido) %>%
  dplyr::summarise(votos = sum(votos, na.rm = TRUE), 
                   votos_total = sum(votos.sum, na.rm = TRUE),
                   vote.sh     = 100 * votos / votos_total,
                   .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::left_join(diodata, by = "CE_code")


# Derived variables (muni & dio panels) ---------------------------------------
munidata_panel_election <- munidata_panel_election %>%
  mutate(
    Retirement_Years = dplyr::if_else(I_YEAR < election, I_YEAR - 1978, election - 1978
    ),
    Exposure = dplyr::if_else(JPIIAPT_YEAR < election, JPIIAPT_YEAR - 1978, election - 1978
    )
  )

diodata_panel_election <- diodata_panel_election %>%
  mutate(
    Retirement_Years = dplyr::if_else(I_YEAR < election, I_YEAR - 1978, election - 1978
    ),
    Exposure = dplyr::if_else(JPIIAPT_YEAR < election, JPIIAPT_YEAR - 1978, election - 1978
    )
  )

# Helpers ----------------------------------------------------------------------
years_pres <- c(1989, 1994, 1998, 2002)

run_itt <- function(df, y) {
  estimatr::lm_robust(
    as.formula(paste0(y, " ~ Retirement_Years")),
    data = df,
    clusters = CE_1978,    # Cluster SEs at the 1978 diocese level
    fixed_effects = ~ UF  # State FEs
  )
}

run_cace <- function(df, y) {
  estimatr::iv_robust(
    as.formula(paste0(y, " ~ Exposure | Retirement_Years")),
    data = df,
    clusters = CE_1978,    # Cluster SEs at the 1978 diocese level
    fixed_effects = ~ UF  # State FEs
  )
}

fmt_mean <- function(x) sprintf("%.3f", x)
fmt_int  <- function(x) format(as.integer(x), big.mark = " ", trim = TRUE, scientific = FALSE)

keep_cols <- function(df, k) {
  wanted <- c("term", sprintf("(%d)", seq_len(k)))
  dplyr::select(df, dplyr::any_of(wanted))
}

#-------------------------------------------------------------------------------
# Table 1: Main results (PT presidential vote, muni level)
#-------------------------------------------------------------------------------

# Columns = election years (paper layout)
cols_named <- setNames(as.list(years_pres), paste0("(", years_pres, ")"))

# One df per column (filter once here)
dfs <- lapply(cols_named, \(yr)
              munidata_panel_election %>%
                filter(partido == "PT", cargo == "Presidente", CE_TYPE != "A", election == yr)
)

# Metadata (computed once)
meta <- tibble(
  col = names(dfs),
  df  = unname(dfs),
  y   = rep("vote.sh", length(dfs))
) |>
  mutate(stats = map(df, \(d) tibble(
    outcome_mean = mean(d[[y[1]]], na.rm = TRUE),
    n_obs        = nrow(d),
    n_cl         = dplyr::n_distinct(d$CE_1978)
  ))) %>%
  unnest(stats)

# Fit models using existing helpers run_cace/run_itt ---------------------------
CACE_models <- setNames(map2(meta$df, meta$y, run_cace), meta$col)
ITT_models  <- setNames(map2(meta$df, meta$y, run_itt),  meta$col)

# Panels as data frames (robust to modelsummary version diffs) -----------------
df_A <- modelsummary(
  CACE_models,
  coef_map = c("Exposure" = "Exposure"),
  gof_omit = ".*",
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = TRUE,
  output = "data.frame"
) %>% dplyr::select(-dplyr::any_of(c("part", "statistic")))

df_B <- modelsummary(
  ITT_models,
  coef_map = c("Retirement_Years" = "Mandated Exposure"),
  gof_omit = ".*",
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = TRUE,
  output = "data.frame"
) %>% dplyr::select(-dplyr::any_of(c("part", "statistic")))

# Bottom rows ------------------------------------------------------------------
fmt2 <- function(x) sprintf("%.2f", x)
rows <- list(
  "Outcome Mean"     = fmt2(meta$outcome_mean),
  "Num. Obs."        = format(meta$n_obs, big.mark = ","),
  "Num. of Clusters" = format(meta$n_cl,  big.mark = ",")
)

make_row <- function(label, values_vec, cols = meta$col) {
  tibble(term = label) %>% bind_cols(as_tibble(setNames(as.list(values_vec), cols)))
}
bottom_rows <- purrr::imap_dfr(rows, ~ make_row(.y, .x))

# Stitch with panel separators -------------------------------------------------
sep_A  <- tibble(term = "\\textbf{Panel A: 2SLS}")
sep_B  <- tibble(term = "\\textbf{Panel B: Reduced Form (ITT)}")
full_df <- bind_rows(sep_A, df_A, sep_B, df_B, bottom_rows)

# Render & save LaTeX matching the screenshot style ---------------------------
tab_title <- "\\textbf{Outcome: PT Presidential Vote Share}"
hdr <- c(" " = 1, setNames(rep(1, nrow(meta)), gsub("[()]", "", meta$col)))

latex_out <- modelsummary::datasummary_df(
  full_df,
  title  = "\\textbf{Outcome: PT Presidential Vote Share}",
  escape = FALSE,
  add_header_above = c(" " = 1, setNames(rep(1, nrow(meta)), gsub("[()]", "", meta$col))),
  fmt = 3
)

latex_out

tinytable::save_tt(latex_out, output = file.path(main_dir, "Table1.tex"))


#-------------------------------------------------------------------------------
# Appendix Table C1: Diocese-level analysis (PT presidential vote)
#-------------------------------------------------------------------------------

#### ITT
ITT_dio <- list()
for (i in years_pres){
  ITT_dio[[paste0('PT vote ', i)]] <- lm_robust(vote.sh ~ Retirement_Years, 
                                                fixed_effects = ~ UF_dio,
                                                data = filter(diodata_panel_election, 
                                                              election == i & 
                                                                partido == "PT" & 
                                                                cargo == "Presidente" &
                                                                CE_TYPE != "A"))
}

#### CACE
CACE_dio <- list()
for (i in years_pres){
  CACE_dio[[paste0('PT vote ', i)]] <- iv_robust(vote.sh ~ Exposure | Retirement_Years, 
                                                 fixed_effects = ~ UF_dio,
                                                 data = filter(diodata_panel_election, 
                                                               election == i & 
                                                                 partido == "PT" & 
                                                                 cargo == "Presidente" &
                                                                 CE_TYPE != "A"))
}

# Table
modelsummary(ITT_dio, stars = T)
modelsummary(CACE_dio, stars = T)

# Save to LaTeX
modelsummary(
  ITT_dio,
  stars = TRUE,
  output = file.path(app_dir, "Table_C1_ITT.tex")
)

modelsummary(
  CACE_dio,
  stars = TRUE,
  output = file.path(app_dir, "Table_C1_CACE.tex")
)


#-------------------------------------------------------------------------------
# Appendix Table C2: With archdioceses 
#-------------------------------------------------------------------------------

#### ITT
ITT_muni_arch <- list()
for (i in years_pres){
  ITT_muni_arch[[paste0('PT vote ', i)]] <- lm_robust(vote.sh ~ Retirement_Years, 
                                                      fixed_effects = ~ UF,
                                                      clusters = CE_1978,
                                                      data = filter(munidata_panel_election, 
                                                                    election == i & 
                                                                      partido == "PT" & 
                                                                      cargo == "Presidente"))
}

#### CACE
CACE_muni_arch <- list()
for (i in years_pres){
  CACE_muni_arch[[paste0('PT vote ', i)]] <- iv_robust(vote.sh ~ Exposure | Retirement_Years, 
                                                       fixed_effects = ~ UF,
                                                       clusters = CE_1978,
                                                       data = filter(munidata_panel_election, 
                                                                     election == i & 
                                                                       partido == "PT" & 
                                                                       cargo == "Presidente"))
}

# Table
modelsummary(ITT_muni_arch, stars = T)
modelsummary(CACE_muni_arch, stars = T)

# Save to LaTeX
modelsummary(
  ITT_muni_arch,
  stars = TRUE,
  output = file.path(app_dir, "Table_C2_ITT.tex")
)

modelsummary(
  CACE_muni_arch,
  stars = TRUE,
  output = file.path(app_dir, "Table_C2_CACE.tex")
)



#-------------------------------------------------------------------------------
# Appendix Table C3: Congressional elections (PT, muni level)
#-------------------------------------------------------------------------------

years_cong <- c(1994, 1998, 2002)

#### ITT
ITT_muni_cong <- list()
for (i in years_cong){
  ITT_muni_cong[[paste0('PT vote ', i)]] <- lm_robust(vote.sh ~ Retirement_Years, 
                                                      fixed_effects = ~ UF,
                                                      clusters = CE_1978,
                                                      data = filter(munidata_panel_election, 
                                                                    election == i & 
                                                                      partido == "PT" & 
                                                                      cargo == "Deputado Federal" &
                                                                      CE_TYPE != "A"))
}

#### CACE
CACE_muni_cong <- list()
for (i in years_cong){
  CACE_muni_cong[[paste0('PT vote ', i)]] <- iv_robust(vote.sh ~ Exposure | Retirement_Years, 
                                                       fixed_effects = ~ UF,
                                                       clusters = CE_1978,
                                                       data = filter(munidata_panel_election, 
                                                                     election == i & 
                                                                       partido == "PT" & 
                                                                       cargo == "Deputado Federal" &
                                                                       CE_TYPE != "A"))
}

# Table
modelsummary(ITT_muni_cong, stars = T)
modelsummary(CACE_muni_cong, stars = T)


# Save to LaTeX
modelsummary(
  ITT_muni_cong,
  stars = TRUE,
  output = file.path(app_dir, "Table_C3_ITT.tex")
)

modelsummary(
  CACE_muni_cong,
  stars = TRUE,
  output = file.path(app_dir, "Table_C3_CACE.tex")
)



#-------------------------------------------------------------------------------
# Appendix Table C4: Bishop age and experience
#-------------------------------------------------------------------------------

# Add variables 
munidata_panel_election <- munidata_panel_election %>%
  dplyr::mutate(bish_age = AGEOCT78 + election - 1978,
                experience = as.numeric(as.Date("1978-10-16") - as.Date(first_in_date_0)) / 365 + election - 1978
  )

#### Age
ITT_muni_age <- list()
for (i in years_pres){
  ITT_muni_age[[paste0('PT vote ', i)]] <-  lm_robust(vote.sh ~ bish_age, 
                                                      fixed_effects = ~ UF,
                                                      clusters = CE_1978,
                                                      data = filter(munidata_panel_election, 
                                                                    election == i & 
                                                                      I_YEAR > i &
                                                                      partido == "PT" & 
                                                                      cargo == "Presidente" &
                                                                      CE_TYPE!="A"))
  
}


#### Experience
ITT_muni_exp <- list()
for (i in years_pres){
  ITT_muni_exp[[paste0('PT vote ', i)]] <-  lm_robust(vote.sh ~ experience, 
                                                      fixed_effects = ~ UF,
                                                      clusters = CE_1978,
                                                      data = filter(munidata_panel_election, 
                                                                    election == i & 
                                                                      I_YEAR > i &
                                                                      partido == "PT" & 
                                                                      cargo == "Presidente" &
                                                                      CE_TYPE!="A"))
  
}


# Table
modelsummary(ITT_muni_age, stars = T)
modelsummary(ITT_muni_exp, stars = T)

# Save to LaTeX
modelsummary(
  ITT_muni_age,
  stars = TRUE,
  output = file.path(app_dir, "Table_C4_age.tex")
)

modelsummary(
  ITT_muni_exp,
  stars = TRUE,
  output = file.path(app_dir, "Table_C4_experience.tex")
)



#-------------------------------------------------------------------------------
# Appendix Table C5: Truncated sample (I_YEAR < election)
#-------------------------------------------------------------------------------

#### ITT
ITT_muni_trunc <- list()
for (i in years_pres){
  ITT_muni_trunc[[paste0('PT vote ', i)]] <- lm_robust(vote.sh ~ Retirement_Years, 
                                                       fixed_effects = ~ UF,
                                                       clusters = CE_1978,
                                                       data = filter(munidata_panel_election, 
                                                                     I_YEAR < i &
                                                                       election == i & 
                                                                       partido == "PT" & 
                                                                       cargo == "Presidente" &
                                                                       CE_TYPE!="A"))
}

#### CACE
CACE_muni_trunc <- list()
for (i in years_pres){
  CACE_muni_trunc[[paste0('PT vote ', i)]] <- iv_robust(vote.sh ~ Exposure | Retirement_Years, 
                                                        fixed_effects = ~ UF,
                                                        clusters = CE_1978,
                                                        data = filter(munidata_panel_election, 
                                                                      I_YEAR < i &
                                                                        election == i & 
                                                                        partido == "PT" & 
                                                                        cargo == "Presidente" &
                                                                        CE_TYPE!="A"))
}

# Table
modelsummary(ITT_muni_trunc, stars = T)
modelsummary(CACE_muni_trunc, stars = T)

# Save to LaTeX
modelsummary(
  ITT_muni_trunc,
  stars = TRUE,
  output = file.path(app_dir, "Table_C5_ITT.tex")
)

modelsummary(
  CACE_muni_trunc,
  stars = TRUE,
  output = file.path(app_dir, "Table_C5_CACE.tex")
)


#-------------------------------------------------------------------------------
# Appendix Table C6: Flexible treatment effects (categorical timing)
#-------------------------------------------------------------------------------

munidata_panel_election <- munidata_panel_election %>%
  dplyr::mutate(
    Retirement_factor = dplyr::case_when(
      I_YEAR >= election ~ "Post-Election",
      I_YEAR < 1985      ~ "Before 1985",
      I_YEAR < 1989      ~ "1985-1989",
      I_YEAR < 1994      ~ "1989-1994",
      I_YEAR < 1998      ~ "1994-1998",
      I_YEAR < 2002      ~ "1998-2002",
      I_YEAR < 2006      ~ "2002-2006",
      I_YEAR < 2010      ~ "2006-2010",
      I_YEAR < 2014      ~ "2010-2014",
      I_YEAR < 2018      ~ "2014-2018",
      TRUE               ~ "Post-Election"
    ),
    Retirement_factor = forcats::fct_relevel(as.factor(Retirement_factor), "Before 1985")
  )


ITT_muni_factor <- list()
for (i in years_pres){
  ITT_muni_factor[[paste0('PT vote ', i)]] <- lm_robust(vote.sh ~ Retirement_factor, 
                                                        fixed_effects = ~ UF,
                                                        clusters = CE_1978,
                                                        data = filter(munidata_panel_election, 
                                                                      election == i & 
                                                                        partido == "PT" & 
                                                                        cargo == "Presidente" & 
                                                                        CE_TYPE!="A"))
  
}


# Table
modelsummary(ITT_muni_factor, coef_map = names(ITT_muni_factor$`PT vote 2002`$coefficients))
modelsummary(ITT_muni_factor, coef_map = names(ITT_muni_factor$`PT vote 2002`$coefficients), 
             file.path(app_dir, "Table_C6.tex"))


#-------------------------------------------------------------------------------
# Appendix Table C7: Dropping conservative bishops 
#-------------------------------------------------------------------------------

# Only progressive pope appointees
table(munidata_panel_election$apt_pope_0)
CACE_muni_no_cons1 <- list()
for (i in years_pres){
  CACE_muni_no_cons1[[paste0('PT vote ', i)]] <- iv_robust(vote.sh ~ Exposure | Retirement_Years, 
                                                           fixed_effects = ~ UF,
                                                           clusters = CE_1978,
                                                           data = filter(munidata_panel_election, 
                                                                         apt_pope_0 %in% c("JOHN XXIII", "PAUL VI") & 
                                                                           election == i & 
                                                                           partido == "PT" & 
                                                                           cargo == "Presidente" & 
                                                                           CE_TYPE!="A"))
}

# Drop conservatives (1976 SNI report)
table(munidata_panel_election$type_AAA_76)
CACE_muni_no_cons2 <- list()
for (i in years_pres){
  CACE_muni_no_cons2[[paste0('PT vote ', i)]] <- iv_robust(vote.sh ~ Exposure | Retirement_Years, 
                                                           fixed_effects = ~ UF,
                                                           clusters = CE_1978,
                                                           data = filter(munidata_panel_election, 
                                                                         type_AAA_76 != "CONSERVADOR" & 
                                                                           election == i & 
                                                                           partido == "PT" & 
                                                                           cargo == "Presidente" & 
                                                                           CE_TYPE!="A"))
}

# Drop conservatives (1981 SNI report)
table(munidata_panel_election$type_AAA_81)
CACE_muni_no_cons3 <- list()
for (i in years_pres){
  CACE_muni_no_cons3[[paste0('PT vote ', i)]] <- iv_robust(vote.sh ~ Exposure | Retirement_Years, 
                                                           fixed_effects = ~ UF,
                                                           clusters = CE_1978,
                                                           data = filter(munidata_panel_election, 
                                                                         type_AAA_81 != "CONSERVADOR" & 
                                                                           election == i & 
                                                                           partido == "PT" & 
                                                                           cargo == "Presidente" & 
                                                                           CE_TYPE!="A"))
}



# Table
modelsummary(CACE_muni_no_cons1, stars = T)
modelsummary(CACE_muni_no_cons2, stars = T)
modelsummary(CACE_muni_no_cons3, stars = T)


# Save to LaTeX
modelsummary(
  CACE_muni_no_cons1,
  stars = TRUE,
  output = file.path(app_dir, "Table_C7_A.tex")
)

modelsummary(
  CACE_muni_no_cons2,
  stars = TRUE,
  output = file.path(app_dir, "Table_C7_B.tex")
)

modelsummary(
  CACE_muni_no_cons3,
  stars = TRUE,
  output = file.path(app_dir, "Table_C7_C.tex")
)




#-------------------------------------------------------------------------------
# Appendix Table C8: Heterogeneity among JPII bishops
#-------------------------------------------------------------------------------

munidata_panel_election <- munidata_panel_election %>%
  mutate(
    Exposure_alt = dplyr::if_else(JPIIELEV_YEAR < election, JPIIELEV_YEAR - 1978, election - 1978
    )
  )

#### ITT
ITT_muni_expalt <- list()
for (i in years_pres){
  ITT_muni_expalt[[paste0('PT vote ', i)]] <- lm_robust(vote.sh ~ Retirement_Years, 
                                                        fixed_effects = ~ UF,
                                                        clusters = CE_1978,
                                                        data = filter(munidata_panel_election, 
                                                                      election == i & 
                                                                        partido == "PT" & 
                                                                        cargo == "Presidente"&
                                                                        CE_TYPE!="A"))
}

#### CACE
CACE_muni_expalt <- list()
for (i in years_pres){
  CACE_muni_expalt[[paste0('PT vote ', i)]] <- iv_robust(vote.sh ~ Exposure_alt | Retirement_Years, 
                                                         fixed_effects = ~ UF,
                                                         clusters = CE_1978,
                                                         data = filter(munidata_panel_election, 
                                                                       election == i & 
                                                                         partido == "PT" & 
                                                                         cargo == "Presidente" &
                                                                         CE_TYPE!="A"))
}

# Table
modelsummary(ITT_muni_expalt, stars = T)
modelsummary(CACE_muni_expalt, stars = T)


# Save to LaTeX
modelsummary(
  ITT_muni_expalt,
  stars = TRUE,
  output = file.path(app_dir, "Table_C8_ITT.tex")
)

modelsummary(
  CACE_muni_expalt,
  stars = TRUE,
  output = file.path(app_dir, "Table_C8_CACE.tex")
)



#-------------------------------------------------------------------------------
# Appendix Table D1: Left-wing parties — CACE by party
#-------------------------------------------------------------------------------

left_parties <- c("PT", "PDT", "PPS", "PSB", "PCDOB")

# Select data
munidata_panel_election_prePT <- munidata_panel_election[munidata_panel_election$partido %in% c("PT","PDT","PPS","PSB","PCDOB") & 
                                                           munidata_panel_election$cargo=="Presidente" &
                                                           munidata_panel_election$CE_TYPE!="A",]

#### CACE
CACE_muni_left <- list()

for (j in c("PT","PPS","PDT","PSB","PCDOB")){ 
  
  for (i in years_pres){
    
    temp <- munidata_panel_election %>% filter(election == i & 
                                                 partido==j &
                                                 cargo == "Presidente" &
                                                 CE_TYPE!="A")
    
    if(nrow(temp)==0){next}
    
    CACE_muni_left[[paste0(j,' vote ', i)]] <- iv_robust(vote.sh ~ Exposure | Retirement_Years, 
                                                         fixed_effects = ~ UF,
                                                         clusters = CE_1978,
                                                         data = temp)
    
    
    
  }}

# Table
modelsummary(CACE_muni_left, stars = T)
modelsummary(CACE_muni_left, file.path(app_dir, "Table_D1.tex"))


