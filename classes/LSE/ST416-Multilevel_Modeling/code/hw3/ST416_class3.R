# ST416 (Multilevel Modelling), 2025/26
#
# Computer class 3
#
# See also the separate instructions for the class. 
# The numbers below match the numbering in those instructions
# 
# Before you start, please download the files for this class 
# (Computer class 3, under Week 5) from Moodle to your computer. 

######################################################################
# 1. 
# Install (if not yet done) and load some add-on packages. 
# The package lme4 (and especially its lmer function) is the key one for random effects modelling.

# install.packages("lme4") # uncommment and run this if package not yet installed
# install.packages("jtools") # uncommment and run this if package not yet installed
# install.packages("ggplot2") # uncommment and run this if package not yet installed
# install.packages("GGally") # uncommment and run this if package not yet installed

library(lme4)
library(jtools)
library(ggplot2)
library(GGally)

######################################################################
# 2.
# Load the data
#
# This assumes that the file cd4dat.rds is saved in the current working directory.
# Change working directory or add a file path to the command if needed.

cd4dat <- readRDS("Seminar 3/cd4dat.rds")

# Here we also create a new variable, which is the square root of the CD4+ count.
# It will be preferable to use this as the response variable, because it has a more normal distribution
# than the count itself.

cd4dat$sqrtcd4<-sqrt(cd4dat$cd4)

######################################################################
# 3. 
# Initial summary statistics of the data 

# A function to add smoothed (lowess) fit to a scatterplot
# (already defined also last week)
lowess.plot <- function (x, y, ...)
{
  plot(x, y, ...)
  lines(lowess(x,y),lwd=4,col="red")
}


tail(cd4dat)

table(table(cd4dat$subject))

# How many time points per subject?
summary(as.numeric(table(cd4dat$subject)))

summary(cd4dat[,c("cd4","sqrtcd4","time","age","smoke","drugs","partners","cesd")])

hist(cd4dat$cd4)
hist(cd4dat$sqrtcd4)

boxplot(sqrtcd4~drugs,data=cd4dat)
boxplot(sqrtcd4~smoke,data=cd4dat)

lowess.plot(cd4dat$age, cd4dat$sqrtcd4)
lowess.plot(cd4dat$partners, cd4dat$sqrtcd4)
lowess.plot(cd4dat$cesd, cd4dat$sqrtcd4)

# With longitudinal data, plots of the response variable against the time variable are particular
# interest. Here we draw a scatterplot and a smoothed curve for it for sqrtcd4 against time, 
# ignoring the grouping of observations within subjects.

lowess.plot(cd4dat$time, cd4dat$sqrtcd4, xlab="Time", ylab="sqrt(CD4)")
axis(1, at=0, tck=1, lty=2)

# It is not easy to draw any definite conclusions from this plot. However, the following
# observations can tentatively be made:
#  * There is a clear change around zero. This is to be expected, since the cell count
#     should be approximately constant before seroconversion (when the subjects are essentially
#     healthy), and only decline after that.
#   * The decline after seroconversion seems to be fastest early on and then slow down.
#     This suggests the possible need for a nonlinear time effect.
#
######################################################################
# 4. 
# Random effects models for these data
# 
# Here we first examine whether association with time could indeed 
# be regarded as zero before time=0. This can be done by including 
# in a model the interaction between time and a dummy variable 
# for whether time is >0:

cd4dat$timepos <- ifelse(cd4dat$time>0, 1, 0)
summ(mod0<-lmer(sqrtcd4~time*timepos+(1|subject),data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)

# Here the main-effect coefficient of time (0.115) is the time 
# association when time<0. It is indeed not significantly different
# from 0. 
# Note also that the main effect of timepos describes the abrupt change (drop)
# in the (square root of) the cell count at time=0. It is significant, 
# suggesting that we should include it in subsequent models.

# Create a new variable timeafter, which is zero when time is 
# negative and equal to time otherwise.
# Any model with this variable in it implies that the expected cell count
# is constant before time 0 and follows the model specification thereafter

cd4dat$timeafter<-ifelse(cd4dat$time>0, cd4dat$time, 0)

summ(mod1 <-lmer(sqrtcd4~timepos+timeafter+(1|subject),data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)

# Different models:
## Quadratic term in timeafter:
summ(mod2 <-lmer(sqrtcd4~timepos+timeafter+I(timeafter^2)+(1|subject),
                data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)

## Add the other explanatory variables, and remove those which are not significant
## at the 10% level
summ(mod3 <-lmer(sqrtcd4~timepos+timeafter+I(timeafter^2)
                 +age+smoke+partners+cesd+drugs
                 +(1|subject),
                data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)
summ(mod4 <-lmer(sqrtcd4~timepos+timeafter+I(timeafter^2)
                 +smoke+partners+cesd+drugs
                 +(1|subject),
                 data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)
summ(mod5 <-lmer(sqrtcd4~timepos+timeafter+I(timeafter^2)
                 +smoke+cesd+drugs
                 +(1|subject),
                 data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)

## Trying interactions between these explanatory variables and the time 
## variables, and removing some of them

summ(mod6 <-lmer(sqrtcd4~timepos*(smoke+cesd+drugs)
                 +timeafter*(smoke+cesd+drugs)
                 +I(timeafter^2)*(smoke+cesd+drugs)
                 +(1|subject),
                 data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)

## ... here fitted a sequence of models, ending up with this one
## which includes only the interaction between smole and timepos

summ(mod7 <-lmer(sqrtcd4~timepos*smoke
                 +timeafter+I(timeafter^2)
                 +(1|subject),
                 data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)

## Considering random slopes for the time variables 

summ(mod8 <-lmer(sqrtcd4~timepos*smoke
                 +timeafter+I(timeafter^2)
                 +(timepos+timeafter+I(timeafter^2)|subject),
                 data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)

summ(mod9 <-lmer(sqrtcd4~timepos*smoke
                 +timeafter+I(timeafter^2)
                 +(timepos+timeafter|subject),
                 data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)
## Here the test is somewhat questionable, because the log-likelihood for model mod8
## may not be quite correct. But we run the test anyway, and decide to 
## drop the randomn effect for the quadratic term (although it is borderline
##  signifivant, especially if we divide the p-value by 2)
anova(mod8,mod9)

summ(mod10 <-lmer(sqrtcd4~timepos*smoke
                 +timeafter+I(timeafter^2)
                 +(timepos|subject),
                 data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)
anova(mod9,mod10)

summ(mod11 <-lmer(sqrtcd4~timepos*smoke
                  +timeafter+I(timeafter^2)
                  +(timeafter|subject),
                  data=cd4dat,REML=F),digits=3,pvals=T,t.df=10000)
anova(mod9,mod11)

## Random effects for both timepos and timeafter are clearly significant
## The selected model is then mod9

mod <- mod9
summ(mod,digits=3,pvals=T,t.df=10000)
summary(mod) # This shows the correlations of the random effects

######################################################################
# 5. 
# Illustrative fitted values from the selected model 

## Display of predicted random effects
utilde <- ranef(mod)$subject
head(utilde)
ggpairs(utilde, 
        title = "",
        axisLabels = "show") 

summary(cd4dat$time)

times.tmp <- c(-3,0,.01,seq(0.5,5,by=.5))
data.tmp <- cbind(with(cd4dat, expand.grid(time=times.tmp,subject=unique(subject))),smoke=1)
data.tmp$timepos <- ifelse(data.tmp$time>0, 1, 0)
data.tmp$timeafter <- ifelse(data.tmp$time>0, data.tmp$time, 0)

yhat.tmp <- cbind(yhat=predict(mod,newdata=data.tmp),
                  data.tmp)
yhat0.tmp <- predict(mod,newdata=data.tmp,re.form=NA)[seq(length(times.tmp))]

## For clarity of the plot, show only a sample of 20 lines
set.seed(1234)
nsample.tmp <- 20
yhats.tmp <- yhat.tmp[yhat.tmp$subject %in% 
                        sample(unique(yhat.tmp$subject),size=nsample.tmp,replace=F),]
head(yhats.tmp)

matplot(times.tmp,
        matrix(yhats.tmp[,"yhat"],nrow=length(times.tmp),byrow=F),
        type='l',lty="solid",xlab="Time",
        ylab="Subject-specific fitted value for sqrt(CD4+ cell count)")
lines(times.tmp,yhat0.tmp,lwd=5,col="red") ## Average line

#######################################################################################


