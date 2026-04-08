# ST416 (Multilevel Modelling), 2025/26
#
# Computer class 5
#
# See also the separate instructions for the class. 
# 
# Before you start, please download the files for this class 
# (Computer class 5, under Week 11) from Moodle to your computer. 

######################################################################
# Install (if not yet done) and load some add-on packages. 

# install.packages("lme4") # uncommment and run this if package not yet installed
# install.packages("jtools") # uncommment and run this if package not yet installed
# install.packages("lmtest") # uncommment and run this if package not yet installed
# install.packages("geepack") # uncommment and run this if package not yet installed
# install.packages("survival") # uncommment and run this if package not yet installed

library(lme4)
library(jtools)
library(lmtest)
library(sandwich)
library(geepack)
library(survival)

######################################################################
# Read in data

xeropdat <- readRDS(paste0(getwd(), 
                           "/personal/classes/LSE/ST416-Multilevel_Modeling/data/hw5/xeropdat.rds"))

######################################################################
# Standard binary logistic model, without allowing for the clustering
summary(mod.glm1 <- glm(resp~sex+stunted+baseage+xerop,
                        data=xeropdat,family=binomial))

# Adjusting the standard errors for clustering
print(mod.se1 <- coeftest(mod.glm1,vcovCL(mod.glm1, cluster = ~id)))

# Random intercepts model 
## Using a Laplace approximation to evaluate the likelihood contributions
summ(mod.re0 <- glmer(resp~sex+stunted+baseage+xerop+(1|id),
           data=xeropdat,family=binomial,nAGQ=1),
     digits=4,pvals=T)

## Using numerical integration (with 25 support points)
##  to evaluate the likelihood contributions
summ(mod.re1 <- glmer(resp~sex+stunted+baseage+xerop+(1|id),
                      data=xeropdat,family=binomial,nAGQ=25),
     digits=4,pvals=T)

# Marginal model, with Generalised Estimating Equation (GEE) estimation
## With exchangeable (compound symmetry) working correlation 
summary(mod.gee1 <- geeglm(resp~sex+stunted+baseage+xerop,
                        data=xeropdat,family=binomial,
                        id=id,corstr="exchangeable"))

# Fixed effects model
summary(mod.fe1 <- clogit(resp~sex+stunted+baseage+xerop+strata(id),
                           data=xeropdat))
## Note: explanatory variables  which do not vary within person 
## do not get coefficients. Refit without them:
summary(mod.fe1 <- clogit(resp~stunted+xerop+strata(id),
                          data=xeropdat))

## Why is this so different?
## Recall that fixed effects estimation here omits all clusters (children)
## which have only one observation or where the value of Y is the same for 
## all level-1 units. Here the latter is common, because many children 
## never had respiratory infection

t.tmp <- as.vector(by(xeropdat$resp,INDICES=xeropdat$id,FUN=var,na.rm=T))
table(t.tmp==0 | is.na(t.tmp)) 
## This shows that only 77 children were actually used for the estimation
## of the fixed effects model

######################################################################
