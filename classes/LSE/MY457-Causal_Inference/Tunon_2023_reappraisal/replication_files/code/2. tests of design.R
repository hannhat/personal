################################################################################
# When the Church Votes Left
# Guadalupe Tuñón | Aug 2025
# This script generates all figures and tables related to tests of the design---
# first stage, balance, and placebo tests. 
# Produces:
## Main paper:
#### Figure 2 - First Stage
## Supplementary appendix: 
#### Figure B2 - Year of Retirement Age and JPII Bishop Replacements, By Type of Exit
#### Figure B3 -  Age distribution of bishops and archbishops in office in October, 1978 
#### Table B1 - Balance Tests
#### Table B2 - Effects of the Length of Exposure to Progressive Bishops on the MDB's 1976 Vote Share
################################################################################

# Basics
rm(list = ls())
set.seed(1234)
options(scipen = 999)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, ggplot2, estimatr, broom, xtable, modelsummary, here)

# (Optional) Anchor project root once per machine:
# here::i_am("code/2. tests of design.R")

# Output directories
main_dir <- here::here("output", "main_paper")
app_dir <- here::here("output", "appendix")
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

# Data
munidata <- readr::read_csv(here::here("data/munidata.csv"), show_col_types = FALSE)
diodata  <- readr::read_csv(here::here("data/diodata.csv"),  show_col_types = FALSE)
electoral <- readr::read_csv(here::here("data/IPEA_electoral_data.csv"), show_col_types = FALSE)

#==============================#
# Figure 2                     #
#==============================#

## Panel A

f_2A <- diodata %>%
  filter(CE_TYPE != "A") %>%
  ggplot(aes(x = I_YEAR, y = JPIIAPT_YEAR)) +
  geom_jitter(alpha = .4, width = .5, height = .5, size = 2, color = "grey60") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", linewidth = .2) +
  geom_smooth(method = lm, color = "black", se = FALSE, linewidth = .8) +
  labs(x = "Mandated Retirement", y = "First JPII Appointment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))
f_2A
ggsave(file.path(main_dir, "Fig2A.pdf"), f_2A, height = 2.25, width = 2.5, dpi = 1000)


## Panel B

years_all  <- 1979:max(diodata$I_YEAR, na.rm = TRUE)
years_main <- c(1989, 1994, 1998, 2002)

first_stage <- lapply(years_all, function(i) {
  idx <- diodata$CE_TYPE != "A"
  I_years <- ifelse(diodata$I_YEAR[idx] < i, diodata$I_YEAR[idx] - 1978, i - 1978)
  Expos   <- ifelse(diodata$JPIIAPT_YEAR[idx] < i, diodata$JPIIAPT_YEAR[idx] - 1978, i - 1978)
  fit     <- estimatr::lm_robust(Expos ~ I_years)
  out     <- broom::tidy(fit)[2, c("estimate", "conf.low", "conf.high")]
  fstat   <- unname(fit$fstatistic[1])
  cbind(out, fstat = fstat, year = i)
}) %>% bind_rows() %>%
  mutate(outcome_years = !(year %in% years_main))

f_2B <- ggplot(first_stage, aes(x = year, y = estimate, color = outcome_years)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  scale_color_grey() +
  labs(x = "Year", y = "Estimate") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = .5))
f_2B
ggsave(file.path(main_dir, "Fig2B.pdf"), f_2B, height = 2.25, width = 2.5, dpi = 1000)


## Panel C

f_2C <- ggplot(first_stage, aes(x = year, y = fstat)) +
  geom_line(color = "grey40") +
  geom_point(aes(color = outcome_years)) +
  scale_color_grey() +
  labs(x = "Year", y = "F-Statistic") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = .5))
f_2C
ggsave(file.path(main_dir, "Fig2C.pdf"), f_2C, height = 2.25, width = 2.5, dpi = 1000)


#==============================#
# Appendix Figure B2          #
#==============================#
diodata <- diodata %>%
  mutate(out_method_0 = recode(out_method_0,
                               "RETIREMENT" = "RETIRED",
                               "DEATH"      = "DIED",
                               "RESIGNATION"= "RESIGNED",
                               "TRANSFER"   = "TRANSFERRED"))

f_B2 <- diodata %>%
  filter(CE_TYPE != "A") %>%
  ggplot(aes(x = I_YEAR, y = JPIIAPT_YEAR)) +
  geom_jitter(alpha = .4, width = .5, height = .5, size = 2, color = "grey60") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", linewidth = .2) +
  geom_smooth(method = lm, color = "black", se = FALSE, linewidth = .8) +
  facet_wrap(~ out_method_0, ncol = 4) +
  labs(x = "Year of Bishop's 75th Birthday/Death", y = "Year of Bishop's Replacement") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))
f_B2
ggsave(file.path(app_dir, "FigB2.pdf"), f_B2, height = 2.5, width = 7, dpi = 1000)

#==============================#
# Appendix Figure B3          #
#==============================#
diodata <- diodata %>% mutate(CE_TYPEA = ifelse(CE_TYPE == "A", "A", "D"))

mu <- diodata %>%
  group_by(CE_TYPEA) %>%
  summarise(grp.mean = mean(AGEOCT78, na.rm = TRUE), .groups = "drop")

f_B3 <- ggplot(diodata, aes(x = AGEOCT78, color = CE_TYPEA, fill = CE_TYPEA, group=CE_TYPEA)) +
  geom_density(alpha = 0.1, linewidth = .7, bw = 4, n = 2048, trim = FALSE, na.rm = TRUE) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = CE_TYPEA),
             linetype = "dashed", linewidth = .7) +
  scale_fill_grey(name = "EC Type") +
  scale_color_grey(name = "EC Type") +
  labs(x = "Prelate Age on Oct 16, 1978", y = "Density") +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.06, 0.85),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", color = NA))
f_B3
ggsave(file.path(app_dir, "FigB3.pdf"), f_B3, height = 3.5, width = 5, dpi = 1000)

#==============================#
# Appendix Table B1           #
#==============================#

# Panel A: Diocese characteristics
rel_diolevel <- do.call(rbind, lapply(c("IR","NIR","INR","DIAC","REL",
                                        "CASASREL_MASC","CASASREL_FEM","NR_PAR","NR_MUNI",
                                        "recursos_nro","recursos_total_activities"), function(v) {
                                          broom::tidy(
                                            estimatr::lm_robust(
                                              formula       = as.formula(paste0("I_YEAR ~ ", v)),
                                              data          = subset(diodata, CE_TYPE != "A")
                                            )
                                          )[2, ]
                                        }))

munidata$parish_1980[is.na(munidata$cod.1980)] <- NA
rel_munilevel <- do.call(rbind, lapply(c("parish_1980"), function(v) {
  broom::tidy(
    estimatr::lm_robust(
      formula       = as.formula(paste0("I_YEAR ~ ", v)),
      data          = subset(munidata, CE_TYPE != "A"),
      clusters      = CE_1978,
      fixed_effects = UF
    )
  )
}))

rel <- rbind(rel_diolevel, rel_munilevel); rm(rel_diolevel, rel_munilevel)

rel <- rel[, c("term", "estimate", "std.error", "p.value")]
rel$N <- 189L
rel$N[nrow(rel)] <- sum(!is.na(munidata$parish_1980))
row.names(rel) <- NULL


# Panel B: Social variables

social <- do.call(rbind, lapply(c("pop.tot.1970", "pop.urb.1970", 
                                            "pop.rur.1970", "share_catolico_1970", 
                                            "share_evangelico_1970", "share_catolico_1978", 
                                            "share_evangelico_1978", "growth_evangelica"), function(v) {
  broom::tidy(
    estimatr::lm_robust(
      formula       = as.formula(paste0("I_YEAR ~ ", v)),
      data          = subset(munidata, CE_TYPE != "A"),
      clusters      = CE_1978,
      fixed_effects = UF
    )
  )
}))

social <- social[, c("term", "estimate", "std.error", "p.value")]
social$N <- c(
  sum(!is.na(munidata$pop.tot.1970)),
  sum(!is.na(munidata$pop.urb.1970)),
  sum(!is.na(munidata$pop.rur.1970)),
  sum(!is.na(munidata$share_catolico_1970)),
  sum(!is.na(munidata$share_evangelico_1970)),
  sum(!is.na(munidata$share_catolico_1978)),
  sum(!is.na(munidata$share_evangelico_1978)),
  sum(!is.na(munidata$growth_evangelica))
)
row.names(social) <- NULL

# Panel C: Electoral variables

elect <- do.call(rbind, lapply(c("ELEITORADO_1976", "MDB1976_SH", "ARENA1976_SH",
                                 "ELEITORADO_1972", "MDB1972_SH", "ARENA1972_SH"), function(v) {
                                    broom::tidy(
                                      estimatr::lm_robust(
                                        formula       = as.formula(paste0("I_YEAR ~ ", v)),
                                        data          = subset(munidata, CE_TYPE != "A"),
                                        clusters      = CE_1978,
                                        fixed_effects = UF
                                      )
                                    )
                                  }))

elect <- elect[, c("term", "estimate", "std.error", "p.value")]
elect$N <- c(
  sum(!is.na(munidata$ELEITORADO_1976)),
  sum(!is.na(munidata$MDB1976_SH)),
  sum(!is.na(munidata$ARENA1976_SH)),
  sum(!is.na(munidata$ELEITORADO_1972)),
  sum(!is.na(munidata$MDB1972_SH)),
  sum(!is.na(munidata$ARENA1972_SH))
)
row.names(elect) <- NULL


balance <- rbind(rel, social, elect); rm(rel, social, elect)
balance

xtable(balance, digits = 3)

# Save LaTeX table
capture.output(
  print(xtable::xtable(balance, include.rownames = FALSE, digits = 3), type = "latex"),
  file = file.path(app_dir, "TableB1.tex")
)


#==============================#
# Appendix Table B2            #
#==============================#

# Municipality-level election panel
munidata_panel_election <- munidata %>%
  dplyr::left_join(electoral, by = "cod.2010")

# Add treatment & IV variables
munidata_panel_election <- munidata_panel_election %>%
  mutate(
    Retirement_Years = pmin(I_YEAR, election) - 1978,
    Exposure         = pmin(JPIIAPT_YEAR, election) - 1978
  )

# Prep data
df76 <- munidata_panel_election %>%
  filter(partido == "PT",
         cargo == "Presidente",
         CE_TYPE != "A",
         !is.na(cod.1970))

years <- c(1989, 1994, 1998, 2002)
names_vec <- paste0("MDB vote ", years)

# ITT
ITT_muni <- setNames(lapply(years, function(y)
  estimatr::lm_robust(
    MDB1976_SH ~ Retirement_Years,
    clusters = CE_1978, fixed_effects = UF,
    data = filter(df76, election == y)
  )
), names_vec)

# CACE
CACE_muni <- setNames(lapply(years, function(y)
  estimatr::iv_robust(
    MDB1976_SH ~ Exposure | Retirement_Years,
    clusters = CE_1978, fixed_effects = UF,
    data = filter(df76, election == y)
  )
), names_vec)

# Output (console + LaTeX files)
modelsummary(ITT_muni,  stars = TRUE)
modelsummary(CACE_muni, stars = TRUE)

modelsummary(ITT_muni,  stars = TRUE, output = file.path(app_dir, "TableB2_placebo_ITT.tex"))
modelsummary(CACE_muni, stars = TRUE, output = file.path(app_dir, "TableB2_placebo_CACE.tex"))

