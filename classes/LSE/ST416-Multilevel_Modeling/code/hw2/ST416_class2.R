# ST416 (Multilevel Modelling), 2025/26
#
# Computer class 2
#
# See also the separate instructions for the class. 
# The numbers below match the numbering in those instructions
# 
# Before you start, please download the files for this class 
# (Computer class 2, under Week 4) from Moodle to your computer. 

######################################################################
# 1. 
# Install (if not yet done) and load some add-on packages. 
# The package lme4 (and especially its lme function) is the key one for random effects modelling.

# install.packages("lme4") # uncommment and run this if package not yet installed
# install.packages("ggplot2") # uncommment and run this if package not yet installed
# install.packages("GGally") # uncommment and run this if package not yet installed
# install.packages("jtools") # uncommment and run this if package not yet installed
# install.packages("lattice") # uncommment and run this if package not yet installed
# install.packages("metafor") # uncommment and run this if package not yet installed
# install.packages("mitml") # uncommment and run this if package not yet installed
# install.packages("multcomp") # uncommment and run this if package not yet installed

library(lme4)

library(ggplot2)
library(GGally)
library(jtools)
library(lattice)
library(metafor)
library(mitml)
library(multcomp)

######################################################################
# 2. 
# Load the data
#
# This assumes that the file army.rds is saved in the current working directory.
# Change working directory or add a file path to the command if needed.

army <- readRDS("Seminar 2/army.rds")

######################################################################
# 3. 
# Initial summary statistics of the data 

tail(army)

table(army$grp)
summary(as.numeric(table(army$grp)))

summary(army[,-1])

ggpairs(army,columns = 2:5, 
        title = "",
        axisLabels = "show") 

# A function to add smoothed (lowess) fit to a scatterplot
lowess.plot <- function (x, y, ...)
{
  plot(x, y, ...)
  lines(lowess(x,y),lwd=4,col="red")
}

lowess.plot(army$cohes,army$wbeing)
lowess.plot(army$lead,army$wbeing)
lowess.plot(army$hrs,army$wbeing)

######################################################################
# 4. 
# Fitting first random intercepts model 

mod1 <- lmer(wbeing~hrs+cohes+lead+(1|grp),data=army,REML=F)
summary(mod1)
confint(mod1,method="Wald")

# Neater summary presentation, using a function from the jtools package
#   the t.df=10000 adds p-values for the z-tests, 
#   derived from standard normal distribution 

summ(mod1,digits=3,pvals=T,t.df=10000)
summ(mod1,confint=T)

######################################################################
# 5. Extracting and plotting predicted random effects and their "standard errors"

utilde <- ranef(mod1,condVar=T)
utilde.df <- as.data.frame(utilde)
head(utilde.df)

# function from the lattice package:
dotplot(utilde)

# same, with function from the metafor package:
forest(utilde.df[,"condval"],sei=utilde.df[,"condsd"],
       order="obs",psize=2,xlab="Predicted random effects for companies",
       cex.lab=1,cex.axis=1,annotate=F,slab=NA)

######################################################################
# 6. Adding cluster means of the three level-1 explanatory variables

army$hrs.m <- clusterMeans(army$hrs,cluster=army$grp)
army$cohes.m <- clusterMeans(army$cohes,cluster=army$grp)
army$lead.m <- clusterMeans(army$lead,cluster=army$grp)

## within-cluster deviations from the means
army$hrs.dev <- army$hrs-army$hrs.m
army$cohes.dev <- army$cohes-army$cohes.m
army$lead.dev <- army$lead-army$lead.m

mod2 <- lmer(wbeing~hrs+cohes+lead+hrs.m+cohes.m+lead.m
              +(1|grp),data=army,REML=F)
summ(mod2,digits=3,pvals=T,t.df=10000)

## Note: The z-test statistic of the coefficient of cohes.m
##  is not significant at any standard significance levels.
##
## For comparison, carry out the likelihood ratio test of the 
##  same hypothesis

mod3 <- lmer(wbeing~hrs+cohes+lead+hrs.m+lead.m
             +(1|grp),data=army,REML=F)
anova(mod2,mod3)

## Remove cohes.m from the model and retain model mod3

#############################################################################
# 7. More exploration of this model

summ(mod3,digits=3,pvals=T,t.df=10000)

## Random-effect variance (and ICC) does not seem very large.
## Carry out a likelihood ratio test of whether it is significant?

## Model without a random intercept, i.e. standard linear model 
## estimated with ML estimation
mod0 <- glm(wbeing~cohes+hrs+lead+hrs.m+lead.m
             ,family=gaussian,data=army)
anova(mod3,mod0)
## Recall that this p-value is actually too large. But here that makes 
##  no difference, because it is still very small.

## Fitting effectively the same model but with within-cluster 
##  deviations of the level-1 variables instead of the variables 
##  themselves (for those variables which also have the cluster 
##  mean still in the model)

mod3b <- lmer(wbeing~cohes+hrs.dev+lead.dev+hrs.m+lead.m
             +(1|grp),data=army,REML=F)
summ(mod3b,digits=3,pvals=T,t.df=10000)

## Estimating Between effects and testing whether they are 
##  necessary (i.e. different from Within effects)
##  See slide 28 of week 4 lectures. 

## mod3: coefficients of the cluster means are the estimated 
##  between effects (see estimates in the output above)

## mod3b: coefficients of the cluster means are the estimated 
##  differences of between and within effects. 
##  So a test of whether these are 0 is what we need. 

anova(mod3b,mod1)

#############################################################################
# 8. Adding random coefficients (random slopes)

## First for lead.dev
mod4 <- lmer(wbeing~cohes+hrs.dev+lead.dev+hrs.m+lead.m
              +(lead.dev|grp),data=army,REML=F)
summ(mod4,digits=3,pvals=T,t.df=10000)
## This does not include the covariance (or correlation) of the 
## two random effects. We can get it in the standard lmer output
summary(mod4)

## Likelihood ratio test of whether this was needed
##  (again the p-value will be too large)
anova(mod3,mod4)

## Then for hrs.dev
mod5 <- lmer(wbeing~cohes+hrs.dev+lead.dev+hrs.m+lead.m
             +(lead.dev+hrs.dev|grp),data=army,REML=F)
summary(mod5)

## Likelihood ratio test of whether this was needed
##  (again the p-value will be too large, but that would not change
##  the conclusion here)
anova(mod4,mod5)

###########################################################################
# 9. Example of cluster-specific fitted values from the selected model.

round(colMeans(army[,-1]),2)
summary(army[,"lead.dev"])

xvals.tmp <- c(-1,1)
data.tmp <- cbind(with(army, expand.grid(lead.dev=xvals.tmp,grp=unique(grp))),
                  cohes=3,hrs.dev=0,cohes.m=3,hrs.m=11,lead.m=3)
yhat.tmp <- cbind(yhat=predict(mod4,newdata=data.tmp),
                  data.tmp)
yhat0.tmp <- predict(mod4,newdata=data.tmp,re.form=NA)[seq(length(xvals.tmp))]

## For clarity of the plot, show only a sample of 20 lines
set.seed(1234)
nsample.tmp <- 20
yhats.tmp <- yhat.tmp[yhat.tmp$grp %in% 
                       sample(unique(yhat.tmp$grp),size=nsample.tmp,replace=F),]
head(yhats.tmp)

matplot(xvals.tmp,
        matrix(yhats.tmp[,"yhat"],nrow=length(xvals.tmp),byrow=F),
        type='l',lty="solid",xlab="Within-cluster deviation from mean leadership score",
        ylab="Cluster-specific fitted value for well-being score")
lines(xvals.tmp,yhat0.tmp,lwd=5,col="red") ## Average line

###########################################################################
