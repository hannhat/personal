# ST416 (Multilevel Modelling), 2025/26
#
# Computer class 1
#
# See also the separate instructions for the class. 
# The numbers below match the numbering in those instructions
# 
# Before you start, please download the files for this class 
# (Computer class 1, under Week 2) from Moodle to your computer. 

######################################################################
# 1. 
# Install (if not yet done) and load some add-on packages. 
# The package lme4 (and especially its lme function) is the key one for random effects modelling.

# install.packages("lme4") # uncommment and run this if package not yet installed
# install.packages("metafor") # uncommment and run this if package not yet installed
# install.packages("readstata13") # uncommment and run this if package not yet installed

library(lme4)
library(metafor)
#library(readstata13)

######################################################################
# 2. 
# Load the data
#
# This assumes that the file scidat.rds is saved in the current working directory.
# Change working directory or add a file path to the command if needed.

scidat <- readRDS("Seminar 1/scidat.rds")

######################################################################
# 3. 
# Some initial summary statistics of the data 

table(scidat$centre)
summary(scidat)
hist(scidat$written)

# This calculates sample means (and their standard errors) of the   
# response variable (written) separately for each cluster (centre)
g.means <- coef(summary(lmList(written ~ 1|centre, data=scidat)))[,,"(Intercept)"]

forest(g.means[,"Estimate"],sei=g.means[,"Std. Error"],
       order="obs",psize=2,xlab="Mean written score by centre",
       cex.lab=1,cex.axis=1,annotate=F,slab=NA)

######################################################################
# 4. 
# Fitting a variance components model, i.e. a random effects model 
#   with no explanatory variables.

# A convenience function for calculating the estimated intraclass correlation
# for any linear random intercepts model fitted with the lmer function
icc <- function(model){
  m <- as.data.frame(VarCorr(model))
  icc <- m[1,"vcov"]/(m[1,"vcov"]+m[2,"vcov"])
  icc
}

varcomp.mod <- lmer(written ~ 1+(1|centre), data=scidat,REML=F)
summary(varcomp.mod)
# Intraclass correlation:
icc(varcomp.mod)

######################################################################
# 5. 
# Fitting models which include the explanatory variables:
# Linear model estimated using OLS (i.e. ignoring the clustering), 
# and a random intercepts model estimated with maximum likelihood (ML) and 
# restricted maximum likelihood (REML) estimation. 

summary(ols.mod <- lm(written ~ girl+coursework, data=scidat))

summary(ols_exp.mod <- lm(written ~ girl+coursework+as.factor(centre), data=scidat)) 
# Added to look at estimate with centre treated as a normal level 1 variable

summary(rint.mod <- lmer(written ~ girl+coursework+(1|centre), data=scidat,REML=F))
summary(rint.mod.REML <- lmer(written ~ girl+coursework+(1|centre), data=scidat,REML=T))

######################################################################
# 6. 
# A digression to Stata. This will be demonstrated in the class.
# The Stata commands that we will use are in the file ST416_class1.do

save.dta13(scidat,"scidat.dta")
######################################################################

