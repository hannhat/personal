################################################################################
# When the Church Votes Left
# Guadalupe Tuñón | Aug 2025
# This script produces the hte analysis in Figure 4 and all the associated 
# appendix tables.
# Produces:
## Main paper:
#### Figure 4: Marginal Effects of the Length of Exposure to a Progressive Bishop on the PT's Vote Share
## Supplementary appendix: 
#### Table D2	Binning Estimates: Heterogeneous Treatment Effects (Reduced Form - ITT)
#### Table D3	Binning Estimates: Heterogeneous Treatment Effects with Control by Population (Reduced Form - ITT)
#### Table D4	Binning Estimates: Heterogeneous Treatment Effects (Reduced Form - ITT) - 2002 Election
################################################################################

# Basics
rm(list = ls())
set.seed(1234)
options(scipen = 999)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, ggplot2, estimatr, modelsummary, broom,
               interflex, grid, gridExtra, here, purrr)

# (Optional) Anchor project root once per machine:
# here::i_am("code/3. analysis_electoral.R")

# Output directories
main_dir <- here::here("output/main_paper")
app_dir  <- here::here("output/appendix")
if (!dir.exists(main_dir)) dir.create(main_dir, recursive = TRUE)
if (!dir.exists(app_dir))  dir.create(app_dir,  recursive = TRUE)

# Data
munidata <- readr::read_csv(here::here("data/munidata.csv"), show_col_types = FALSE)
#diodata  <- readr::read_csv(here::here("data/diodata.csv"),  show_col_types = FALSE)
electoral <- readr::read_csv(here::here("data/IPEA_electoral_data.csv"), show_col_types = FALSE)

# Municipality-level panel -----------------------------------------------------
munidata_panel_election <- dplyr::left_join(munidata, electoral, by = "cod.2010")
rm(electoral, munidata)

# Derived variables (muni & dio panels) ---------------------------------------
munidata_panel_election <- munidata_panel_election %>%
  mutate(
    Retirement_Years = dplyr::if_else(I_YEAR < election, I_YEAR - 1978, election - 1978
    )
  )


#-------------------------------------------------------------------------------
# Figure 4: Marginal Effects of the Length of Exposure to a Progressive Bishop on the PT's Vote Share
#-------------------------------------------------------------------------------

df <- dplyr::filter(
    munidata_panel_election,
    election == 2002, cargo == "Presidente", partido == "PT", CE_TYPE != "A"
  ) %>% as.data.frame()  # ensure base data.frame



  fit_manufacturing <- interflex(
    data = df,
    estimator = "binning",
    Y  = "vote.sh",         
    D  = "Retirement_Years",
    X  = 'shr_pob_Manufacturing_and_Construction',  
    vcov.type = "cluster", 
    cl = "CE_1978",
    FE = "UF",
    theme.bw = TRUE,
    na.rm = TRUE
  )
  
  fig_manufacturing <- plot(
    fit_manufacturing, theme.bw = TRUE,
    cex.lab = 0, cex.axis = .7, cex.main = .65,
    bin.labs = FALSE, ylim = c(-.4, .75),
    ylab = "", xlab = ""
  )
  fig_manufacturing
  
  fit_agriculture <- interflex(
    data = df,
    estimator = "binning",
    Y  = "vote.sh",         
    D  = "Retirement_Years",
    X  = 'shr_pob_Agriculture_fishing_and_forestry',  
    vcov.type = "cluster", 
    cl = "CE_1978",
    FE = "UF",
    theme.bw = TRUE,
    na.rm = TRUE
  )
  

  fig_agriculture <- plot(
  fit_agriculture, theme.bw = TRUE,
  cex.lab = 0, cex.axis = .7, cex.main = .65,
  bin.labs = FALSE, ylim = c(-.4, .75),
  ylab = "", xlab = ""
) 
  fig_agriculture

# Put figures together
text1 <- textGrob("Share of workers in manufacturing and construction", gp = gpar(fontsize = 12), hjust= 0.5, vjust=.5)
text2 <- textGrob("Share of workers in agriculture, fishing, and forestry", gp = gpar(fontsize = 12), hjust= 0.5, vjust=.5)
text3 <- textGrob("Marginal effect of years of mandated\nexposure on 2002 PT vote share", gp = gpar(fontsize = 12), rot = 90, hjust= 0.5, vjust=.5)
text4 <- textGrob("", gp = gpar(fontsize = 12))

# build the grob (no draw)
figure_4 <- gridExtra::arrangeGrob(
  text3, fig_manufacturing, fig_agriculture,
  text4, text1, text2,
  ncol = 3, nrow = 2, widths = c(1,10,10), heights = c(8,1)
)

# open device, draw, close
grDevices::pdf(file.path(main_dir, "Fig4.pdf"), colormodel = "gray", width = 9, height = 4)
grid::grid.draw(figure_4)   # <- this actually renders it into the PDF
dev.off()


#-------------------------------------------------------------------------------
# Table D2	Binning Estimates: Heterogeneous Treatment Effects (Reduced Form - ITT)
#-------------------------------------------------------------------------------

fit_urbanpop <- interflex(
  data = df,
  estimator = "binning",
  Y  = "vote.sh",         
  D  = "Retirement_Years",
  X  = 'pop.urb.1980_sh',  
  vcov.type = "cluster", 
  cl = "CE_1978",
  FE = "UF",
  theme.bw = TRUE,
  na.rm = TRUE
)


man_and_const_02 <- as.data.frame(fit_manufacturing$est.bin$`Retirement_Years=15 (50%)`)
round(man_and_const_02, digits=3)

agric_02 <- as.data.frame(fit_agriculture$est.bin$`Retirement_Years=15 (50%)`)
round(agric_02, digits=3)

urb_02 <- as.data.frame(fit_urbanpop$est.bin$`Retirement_Years=15 (50%)`)
round(urb_02, digits=3)


#-------------------------------------------------------------------------------
# Table D3	Binning Estimates: Heterogeneous Treatment Effects with Control by Population (Reduced Form - ITT)
#-------------------------------------------------------------------------------

fit_manufacturing_pop <- interflex(
  data = df,
  estimator = "binning",
  Y  = "vote.sh",         
  D  = "Retirement_Years",
  X  = 'shr_pob_Manufacturing_and_Construction',  
  Z = "pop.tot.1980",
  vcov.type = "cluster", 
  cl = "CE_1978",
  FE = "UF",
  theme.bw = TRUE,
  na.rm = TRUE
)

fit_agriculture_pop <- interflex(
  data = df,
  estimator = "binning",
  Y  = "vote.sh",         
  D  = "Retirement_Years",
  X  = 'shr_pob_Agriculture_fishing_and_forestry',  
  Z = "pop.tot.1980",
  vcov.type = "cluster", 
  cl = "CE_1978",
  FE = "UF",
  theme.bw = TRUE,
  na.rm = TRUE
)

fit_urbanpop_pop <- interflex(
  data = df,
  estimator = "binning",
  Y  = "vote.sh",         
  D  = "Retirement_Years",
  X  = 'pop.urb.1980_sh',  
  Z = "pop.tot.1980",
  vcov.type = "cluster", 
  cl = "CE_1978",
  FE = "UF",
  theme.bw = TRUE,
  na.rm = TRUE
)


man_and_const_02_pop <- as.data.frame(fit_manufacturing_pop$est.bin$`Retirement_Years=15 (50%)`)
round(man_and_const_02_pop, digits=3)

agric_02_pop <- as.data.frame(fit_agriculture_pop$est.bin$`Retirement_Years=15 (50%)`)
round(agric_02_pop, digits=3)

urb_02_pop <- as.data.frame(fit_urbanpop_pop$est.bin$`Retirement_Years=15 (50%)`)
round(urb_02_pop, digits=3)


#-------------------------------------------------------------------------------
# Table D4	Binning Estimates: Heterogeneous Treatment Effects (Reduced Form - ITT) - 2002 Election
#-------------------------------------------------------------------------------

fit_evang_gowth <- interflex(
  data = df,
  estimator = "binning",
  Y  = "vote.sh",         
  D  = "Retirement_Years",
  X  = 'growth_evangelica_log', 
  vcov.type = "cluster", 
  cl = "CE_1978",
  FE = "UF",
  theme.bw = TRUE,
  na.rm = TRUE
)

evang_gowth_02_pop <- as.data.frame(fit_evang_gowth$est.bin$`Retirement_Years=15 (50%)`)
round(evang_gowth_02_pop, digits=3)
