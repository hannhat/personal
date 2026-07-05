################################################################################
# When the Church Votes Left
# Guadalupe Tuñón | Aug 2025
# This script compares surveillance reports of bishop activism.
# Generates: 
## Supplementary appendix: 
#### Table D1 - Bishop Activism, 1987-1990
################################################################################

# Basics
rm(list = ls())
set.seed(1234)
options(scipen = 999)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, ggplot2, estimatr, here)

# (Optional) Anchor project root once per machine:
# here::i_am("code/1. descriptives.R")

# Output directories
main_dir <- here::here("output", "main_paper")
app_dir  <- here::here("output", "appendix")
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(app_dir,  recursive = TRUE, showWarnings = FALSE)

# Data -------------------------------------------------------------------------
diodata   <- readr::read_csv(here::here("data/diodata.csv"),  show_col_types = FALSE)
bishclass <- readr::read_csv(here::here("data/NIS_Bishop-class.csv"), show_col_types = FALSE) %>%
  mutate(progressive = as.integer(type %in% c("PROGRESSISTA RADICAL", "PROGRESSISTA")))

# Merge: diocese–reports panel with classification -----------------------------
data <- merge(diodata, unique(bishclass[, c("source", "year_report", "year_class")]))

data <- data %>%
  left_join(
    bishclass %>% select(CE_1978, source, progressive, type),
    by = c("source", "CE_1978")
  ) %>%
  mutate(progressive = if_else(is.na(progressive), 0L, progressive))

# Figure D1: Bishop Activism, 1987–1990 ---------------------------------------
data <- data %>%
  mutate(JPII_bish = as.integer(year_report >= JPIIAPT_YEAR)) %>%
  filter(CE_TYPE != "A")

# Difference-in-means with source FE
fit <- lm_robust(progressive ~ JPII_bish, fixed_effects = ~ source, data = data)
b   <- unname(fit$coefficients["JPII_bish"])
se  <- unname(fit$std.error["JPII_bish"])
diff_lbl <- sprintf("Diff = %.2f\n(%.2f)***", b, se)


## summarizing data
sum_df <- data |>
  filter(CE_TYPE != "A") |>
  mutate(JPII_bish = as.integer(year_report >= JPIIAPT_YEAR)) |>
  group_by(JPII_bish) |>
  summarise(
    n = n(),
    p = mean(progressive),
    se = sqrt(p * (1 - p) / n),
    .groups = "drop"
  ) |>
  mutate(
    y    = 100 * p,
    ymin = 100 * (p - 1.96 * se),
    ymax = 100 * (p + 1.96 * se)
  )

f_D1 <- ggplot(sum_df, aes(x = factor(JPII_bish), y = y, fill = factor(JPII_bish))) +
  geom_col(width = 0.45, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, ymin), ymax = pmin(100, ymax)),
                width = 0.12, linewidth = 0.6, colour = "black") +
  geom_text(aes(label = sprintf("%.1f", y)), size = 4, vjust = -0.5, nudge_x = 0.05) +
  scale_x_discrete(breaks = c("0", "1"), labels = c("Non-JPII bishops", "JPII bishops")) + 
  scale_fill_grey() +
  coord_cartesian(ylim = c(0, 38), clip = "off") +
  labs(x = NULL, y = "Percent (%)", title = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(colour = "gray30", size = 11),
    axis.text.y = element_text(colour = "gray30", size = 11),
    panel.spacing = unit(3, "lines"),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  annotate("segment", x = 1,   y = 29, xend = 1.2, yend = 31, colour = "darkgray") +
  annotate("segment", x = 1.2, y = 31, xend = 1.8, yend = 31, colour = "darkgray") +
  annotate("segment", x = 1.8, y = 31, xend = 2,   yend = 29, colour = "darkgray") +
  annotate("text", x = 1.5, y = 36, label = diff_lbl, size = 4.5)

f_D1

# Save -------------------------------------------------------------------------
ggsave(filename = file.path(app_dir, "FigD1.pdf"),
       plot = f_D1, height = 4, width = 4, dpi = 1000)

