################################################################################
# When the Church Votes Left
# Guadalupe Tuñón | Aug 2025
# This script produces the maps in the main paper as well as the summary 
# statistics table in the appendix
# Generates: 
## Main paper: 
#### Figure 1: PT vote share by municipality (1989–2002)
#### Figure 3: Mandated exposure to pre-JPII bishops (1989–2002)
## Supplementary appendix: 
#### Table A1 - Summary statistics table
################################################################################

# Basics
rm(list = ls())
set.seed(1234)
options(stringsAsFactors = FALSE, scipen = 999)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, ggplot2, dplyr, tidyr, readr, scales, RColorBrewer, ggpubr,
  geobr, rmapshaper, here, xtable
)

# (Optional) Anchor project root once per machine:
setwd("/Users/johannhatzius/Desktop/GitHub/MY457_summative/replication_files")
here::i_am("code/1. descriptives.R")

# Output directories
main_dir <- here::here("output", "main_paper")
app_dir <- here::here("output", "appendix")
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

# Data
munidata <- readr::read_csv(here::here("data/munidata.csv"), show_col_types = FALSE)
diodata  <- readr::read_csv(here::here("data/diodata.csv"),  show_col_types = FALSE)
electoral <- readr::read_csv(here::here("data", "IPEA_electoral_data.csv"), show_col_types = FALSE)

# Municipality-level panel
munidata_panel_election <- dplyr::left_join(munidata, electoral, by = "cod.2010")
rm(electoral)

# ------------------------------------------------------------------------------
# Figure 1: Territorial Expansion of the PT, 1989–2002
# ------------------------------------------------------------------------------

# Prepare electoral data
munidata_panel_election_map <- munidata_panel_election %>%
  filter(
    election %in% c(1989, 1994, 1998, 2002),
    cargo == "Presidente",
    partido == "PT"
  ) %>%
  # aggregate at 1980 muni borders
  group_by(CE_1978, I_YEAR, CE_TYPE, election, cod.1980_comp, partido, UF) %>%
  summarise(
    votos       = sum(votos, na.rm = TRUE),
    votos.uteis = sum(votos.uteis, na.rm = TRUE),
    votos.sum   = sum(votos.sum, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(
    votos_sh = votos / votos.uteis
  ) %>%
  rename(cod.1980 = cod.1980_comp) 


# Shapefiles
munis_sf <- geobr::read_municipality(year = 1980) %>%
  filter(!is.na(abbrev_state)) %>%
  rename(cod.1980 = code_muni) %>%
  select(-abbrev_state, -code_state, -name_muni) %>%
  st_as_sf() %>%        
  ms_simplify(keep = 0.1)

ufs <- geobr::read_state(year = 1980) %>%
  ms_simplify(keep = 0.5)

df <- munis_sf %>%
  left_join(munidata_panel_election_map, by = "cod.1980") 


# Small helper to draw one year
plot_pt_map <- function(dat, year) {
  ggplot() +
    geom_sf(
      data = dplyr::filter(dat, election == year),
      aes(fill = votos_sh),    # <- no geometry mapping
      colour = NA
    ) +
    scale_fill_distiller(
      name = "Vote share PT", palette = "OrRd",
      breaks = scales::pretty_breaks(), direction = 1, na.value = "grey20"
    ) +
    geom_sf(data = ufs, fill = NA, color = "grey20", linewidth = 0.5) +
    theme_minimal() +
    coord_sf(datum = NA)
}


years <- c(1989, 1994, 1998, 2002)
maps1 <- lapply(years, \(y) plot_pt_map(df, y))

map_1 <- ggarrange(
  plotlist = maps1,
  labels = as.character(years),
  common.legend = TRUE, legend = "bottom",
  ncol = 4, nrow = 1
)

map_1
ggsave(filename = file.path(main_dir, "Fig1.pdf"),
       plot = map_1, height = 4, width = 10, dpi = 1000)

# ------------------------------------------------------------------------------
# Figure 3: Mandated Exposure to Pre-JPII Bishops, 1989–2002
# ------------------------------------------------------------------------------

# Helper: exposure at year y = max(0, min(I_YEAR, y) - 1978)
exposure_at <- function(I_YEAR, y) pmax(0, pmin(I_YEAR, y) - 1978)

# Helper: exposure at year y = max(0, min(I_YEAR, y) - 1978), NA if CE_TYPE == "A"
exposure_at <- function(I_YEAR, y, CE_TYPE) {
  dplyr::if_else(
    CE_TYPE == "A",
    NA_real_,
    pmax(0, pmin(I_YEAR, y) - 1978)
  )
}

# if not already loaded
# library(scales)

plot_exposure_map <- function(shp, y) {
  shp %>%
    mutate(MD_Exposure = exposure_at(I_YEAR, y, CE_TYPE)) %>%  # <— add CE_TYPE
    ggplot() +
    geom_sf(aes(fill = MD_Exposure), color = NA) +
    scale_fill_distiller(
      name = "Mandated\nExposure", palette = "YlGn",
      breaks = scales::pretty_breaks(), direction = 1, na.value = "grey20",
      limits = c(0, 25)
    ) +
    geom_sf(data = ufs, fill = NA, color = "grey20", linewidth = 0.35) +
    coord_sf(datum = NA) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

maps3 <- lapply(years, \(y) plot_exposure_map(df, y))

map_3 <- ggarrange(
  plotlist = maps3,
  labels = as.character(years),
  common.legend = TRUE, legend = "bottom",
  ncol = 4, nrow = 1
)
map_3

ggsave(filename = file.path(main_dir, "Fig3.pdf"),
       plot = map_3, height = 4, width = 10, dpi = 1000)



# -------------------------------------------------------------------
# Table A1 - Summary Statistics table
# -------------------------------------------------------------------

# 1) Year of Mandated Retirement & Year of JPII Appointment
t1 <- diodata %>%
  dplyr::select(
    `Year of Mandated Retirement` = I_YEAR,
    `Year of JPII Appointment`    = JPIIAPT_YEAR
  ) %>%
  rstatix::get_summary_stats(type = "common")

# 2) Retirement Years & Exposure (relative to key elections)
elections <- c(1989, 1994, 1998, 2002)

t2_input <- diodata %>%
  mutate(.id = dplyr::row_number()) %>%
  tidyr::crossing(election = elections) %>%
  mutate(
    Retirement_Years = ifelse(I_YEAR < election, I_YEAR - 1978, election - 1978),
    Exposure         = ifelse(JPIIAPT_YEAR < election, JPIIAPT_YEAR - 1978, election - 1978)
  ) %>%
  select(.id, election, Retirement_Years, Exposure) %>%
  tidyr::pivot_wider(
    id_cols = .id,
    names_from  = election,
    values_from = c(Retirement_Years, Exposure)
  ) %>%
  select(-.id)

# Clean column names to "Retirement Years (YYYY)" and "Exposure (YYYY)"
names(t2_input) <- gsub("_", " ", names(t2_input))
names(t2_input) <- sub("(\\d{4})$", "(\\1)", names(t2_input))

t2 <- rstatix::get_summary_stats(t2_input, type = "common")

# 3) PT vote share (President) by election

# Municipality-level election panel 
#munidata_panel_election <- munidata %>%
#  dplyr::left_join(electoral, by = "cod.2010")

t3 <- munidata_panel_election %>%
  filter(cargo == "Presidente", partido == "PT", election %in% elections) %>%
  dplyr::select(starts_with("cod."), election, vote.sh) %>%
  arrange(election) %>%
  tidyr::pivot_wider(
    names_from  = election,
    values_from = vote.sh,
    names_glue  = "PT vote share {election}"
  ) %>%
  dplyr::select(-starts_with("cod.")) %>%
  rstatix::get_summary_stats(type = "common")

t <- bind_rows(t1, t2, t3) %>%
  dplyr::select(variable, mean, sd, min, max, n) %>%
  mutate(
    n    = as.character(round(n)),
    mean = as.character(round(mean, 2))
  )

print(xtable(t), include.rownames = FALSE)

# Save LaTeX tables
capture.output(
  print(xtable::xtable(t, include.rownames = FALSE, digits = 3), type = "latex"),
  file = file.path(app_dir, "TableA1.tex")
)

