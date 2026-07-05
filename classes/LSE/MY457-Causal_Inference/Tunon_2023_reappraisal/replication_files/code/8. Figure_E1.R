################################################################################
# When the Church Votes Left
# Guadalupe Tuñón | Aug 2025
# This script plots the evolution of Catholic progressivis worldwide.
# Generates: 
## Supplementary appendix: 
#### Table E1 - Catholic Progressivism in Comparative Perspective
################################################################################

# Basics
rm(list = ls())
set.seed(1234)
options(scipen = 999)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, ggplot2, here)

# (Optional) Anchor project root once per machine:
# here::i_am("code/1. descriptives.R")

# Output directories
main_dir <- here::here("output", "main_paper")
app_dir  <- here::here("output", "appendix")
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(app_dir,  recursive = TRUE, showWarnings = FALSE)


# Data -------------------------------------------------------------------------
bishops <- readr::read_csv(here::here("data/bishops_all_countries.csv"),  show_col_types = FALSE)

# Expand to bishop-year panel
bishops_panel <- bishops %>%
  group_by(region, country, name, CE_name, url, CE_bishnr) %>%
  mutate(year = list(seq(in_year, out_year, by = 1))) %>%
  tidyr::unnest(year) %>%
  ungroup()

# Summarise by region-year
bishops_region <- bishops_panel  %>%
  group_by(region, year)  %>%
  summarise(
    nr_bishops = n(),
    jesuits    = mean(jesuit,  na.rm = TRUE),
    mendicant  = mean(mendicant, na.rm = TRUE),
    .groups = "drop"
  )  %>%
  mutate(jes_mend = jesuits + mendicant)


# plot ---

# Define label positions 
labels <- bishops_region  %>%
  filter(year == 2017, !is.na(region), region != "Oceania")  %>%
  mutate(
    jes_mend = dplyr::case_when(
      region == "Latin America and\nthe Caribbean" ~ 0.145,
      region == "North America"                   ~ 0.030,
      region == "Europe"                          ~ 0.048,
      region == "Asia"                            ~ 0.070,
      region == "Africa"                          ~ 0.090,
      TRUE                                        ~ jes_mend
    ),
    year = dplyr::case_when(
      region == "Latin America and\nthe Caribbean" ~ 2015,
      region == "Asia"                             ~ 2019,
      TRUE                                         ~ year
    )
  )


f_E1 <- ggplot(
  bishops_region |>
    filter(!is.na(region), region != "Oceania", dplyr::between(year, 1971, 2020)),
  aes(x = year, y = jes_mend, colour = region, group = region)
) +
  geom_smooth(size = 0.6, se = FALSE) +
  geom_point(aes(size = nr_bishops), alpha = 0.10) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Year",
    y = "Proportion of Bishops from Progressive\nOrders and Societies",
    colour = NULL
  ) +
  geom_text(
    data = labels,
    aes(x = year, y = jes_mend, label = region),
    size = 4, inherit.aes = FALSE
  ) +
  theme_minimal() +
  theme(legend.position = "none")

f_E1

# Save -------------------------------------------------------------------------
ggsave(filename = file.path(app_dir, "FigE1.pdf"),
       plot = f_E1, height = 4, width = 4, dpi = 1000)

