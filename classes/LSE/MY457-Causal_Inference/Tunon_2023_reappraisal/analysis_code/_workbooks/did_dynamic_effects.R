
# Basics
rm(list = ls())
set.seed(1234)
options(scipen = 999)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, ggplot2, estimatr, 
               broom, xtable, modelsummary, here,
               DIDmultiplegtDYN, polars)

# Data
munidata <- readr::read_csv(here::here("replication_files/data/munidata.csv"), show_col_types = FALSE)
diodata  <- readr::read_csv(here::here("replication_files/data/diodata.csv"),  show_col_types = FALSE)
electoral <- readr::read_csv(here::here("replication_files/data/IPEA_electoral_data.csv"), show_col_types = FALSE)


# Creating additional variables in "diodata.csv": 
#   - M_YEAR_ND_jh: Mandated year of retirement, not accounting for death
#   - death_dum_jh: Categorical variable for whether the bishop's reason for replacement was death
#   - transfer_dum_jh: Categorical variable for whether the bishop's reason for replacement was a transfer

diodata_jh <- diodata %>%
  mutate(M_YEAR_ND_jh = 1978 + pmax(0, round(75 - round(AGEOCT78, digits = 0))),
         death_dum_jh = ifelse(out_method_0 == "DEATH", 1, 0),
         transfer_dum_jh = ifelse(out_method_0 == "TRANSFER", 1, 0))

# Merging onto municipal data
munidata_jh <- munidata %>% 
  left_join(diodata_jh %>% 
              select("CE_code", "M_YEAR_ND_jh", 
                     "death_dum_jh", "transfer_dum_jh"), 
            by = "CE_code")

# Quick check of summary statistics on replacement reason
diodata_jh %>% 
  group_by(out_method_0) %>%
  summarize(count = n(),
            mandated_retire_yr = mean(M_YEAR_ND_jh),
            replacement_yr = mean(JPIIAPT_YEAR))

# Additional helper functions and data preparation

# Municipality-level panel -----------------------------------------------------
munidata_panel_election_jh <- dplyr::left_join(munidata_jh, electoral, by = "cod.2010")

# Diocese-level panel with vote shares ----------------------------------------
diodata_panel_election_jh <- munidata_panel_election_jh %>%
  dplyr::group_by(CE_code, cargo, election, partido) %>%
  dplyr::summarise(votos = sum(votos, na.rm = TRUE), 
                   votos_total = sum(votos.sum, na.rm = TRUE),
                   vote.sh     = 100 * votos / votos_total,
                   .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::left_join(diodata_jh, by = "CE_code")

# Derived variables (muni & dio panels) ---------------------------------------
munidata_panel_election_jh <- munidata_panel_election_jh %>%
  mutate(
    Retirement_Years = dplyr::if_else(I_YEAR < election, I_YEAR - 1978, election - 1978
    ),
    Exposure = dplyr::if_else(JPIIAPT_YEAR < election, JPIIAPT_YEAR - 1978, election - 1978
    ),
    # EDIT: Adding custom Exposure variable using modified instrument (does not account for death)
    Retirement_Years_jh = dplyr::if_else(M_YEAR_ND_jh < election, M_YEAR_ND_jh - 1978, election - 1978
    ),
    # EDIT: Adding custom time from replacement (TFR) that is the "opposite" of Exposure.
    TFR = dplyr::if_else(JPIIAPT_YEAR < election, election - JPIIAPT_YEAR, 0
    ),
    # EDIT: Adding logs
    log_TFR = log(1 + TFR),
    log_Exposure = log(Exposure),
    log_Retirement_Years = dplyr::if_else(I_YEAR < election, I_YEAR - 1978, election - 1978
    )
  )

diodata_panel_election_jh <- diodata_panel_election_jh %>%
  mutate(
    Retirement_Years = dplyr::if_else(I_YEAR < election, I_YEAR - 1978, election - 1978
    ),
    Exposure = dplyr::if_else(JPIIAPT_YEAR < election, JPIIAPT_YEAR - 1978, election - 1978
    ),
    Retirement_Years_jh = dplyr::if_else(M_YEAR_ND_jh < election, M_YEAR_ND_jh - 1978, election - 1978
    ),
    TFR = dplyr::if_else(JPIIAPT_YEAR < election, election - JPIIAPT_YEAR, 0
    ),
    log_TFR = log(1 + TFR),
    log_Exposure = log(Exposure),
    log_Retirement_Years = dplyr::if_else(M_YEAR_ND_jh < election, M_YEAR_ND_jh - 1978, election - 1978
    )
  )

did_data <- munidata_panel_election_jh %>% filter(election >= 1989 & election <= 2002,
                                                  partido == "PT") %>%
  select(cod.2010, CE_code, I_YEAR, M_YEAR_ND_jh, JPIIAPT_YEAR, JPIIELEV_YEAR, 
         vote.sh, election, UF) %>%
  mutate(time = (election - 1989) %/% 4 + 1,
         UF_int = as.numeric(as.factor(UF)),
         treated = ifelse(election > I_YEAR, 1, 0),
         treated2 = ifelse(election > JPIIAPT_YEAR, 1, 0))

# Instrument DiD
DIDmultiplegtDYN::did_multiplegt_dyn(did_data,
                                     group = "cod.2010",
                                     time = "time",
                                     cluster = "CE_code",
                                     treatment = "treated",
                                     outcome = "vote.sh",
                                     effects = 5,
                                     placebo = 4)

# Treatment DiD
DIDmultiplegtDYN::did_multiplegt_dyn(did_data,
                                     group = "cod.2010",
                                     time = "time",
                                     cluster = "CE_code",
                                     treatment = "treated2",
                                     outcome = "vote.sh",
                                     effects = 5,
                                     placebo = 4)

# Instrument DiD, no clustering (shouldn't be done)
DIDmultiplegtDYN::did_multiplegt_dyn(did_data,
                                     group = "cod.2010",
                                     time = "time",
                                     treatment = "treated",
                                     outcome = "vote.sh",
                                     effects = 5,
                                     placebo = 4)


# Treatment DiD, no clustering (shouldn't be done)
DIDmultiplegtDYN::did_multiplegt_dyn(did_data,
                                     group = "cod.2010",
                                     time = "time",
                                     treatment = "treated2",
                                     outcome = "vote.sh",
                                     effects = 5,
                                     placebo = 4)

# Instrument DiD, state effects
DIDmultiplegtDYN::did_multiplegt_dyn(did_data,
                                     group = "cod.2010",
                                     time = "time",
                                     cluster = "CE_code",
                                     treatment = "treated",
                                     outcome = "vote.sh",
                                     trends_nonparam = "UF_int",
                                     effects = 5,
                                     placebo = 4)

# Treatment DiD, state effects
DIDmultiplegtDYN::did_multiplegt_dyn(did_data,
                                     group = "cod.2010",
                                     time = "time",
                                     cluster = "CE_code",
                                     treatment = "treated2",
                                     outcome = "vote.sh",
                                     trends_nonparam = "UF_int",
                                     effects = 5,
                                     placebo = 4)


