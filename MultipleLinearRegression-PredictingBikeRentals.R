#Checks if packages are installed and installs them if they are not already installed
list.of.packages <- c("tidyverse", "car", "corrplot", "leaps", "caret", "multcomp", "FrF2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(car)
library(corrplot)
library(leaps)
library(caret)
library(multcomp)


#User chooses respective files for each variable
dcbiketrain <- read.csv(file.choose(), header=T)
dcbiketest <- read.csv(file.choose(), header=T)
head(dcbiketrain)


#Selects variables to be used in the initial model and shows a correlation plot
numdc <- dplyr::select(dcbiketrain, cnt, rawtemp, rawfeeltemp, rawhumidity, rawwindspeed)
cormat <- cor(numdc)
corrplot(cormat, type = "upper", order = "original", tl.srt = 45)

#Summary statistics
#summary(lm(cnt ~ season + temp, dcbiketrain))

#Creates an initial multiple linear regression model and shows the summary statistics
#model <- lm(cnt ~ season + workingday + weathersit + temp + hum + windspeed, data = dcbiketrain)
#summary(model)

#Creates an initial multiple linear regression model and shows the summary statistics
modelraw <- lm(cnt ~ season + workingday + weathersit + rawfeeltemp + rawhumidity + rawwindspeed, data = dcbiketrain)
summary(modelraw)
anova(modelraw)
vif(modelraw)

#Updated multiple linear regression model with workingday removed
submodel.1 <- lm(cnt ~ season + weathersit + rawfeeltemp + rawhumidity + rawwindspeed, data = dcbiketrain)
summary(submodel.1)
anova(submodel.1)

#Plots the histogram, QQ plot, and acf (autocovariance and autocorrelation function) of standardized residuals.
#Also plots QQline and shows the Shapiro test, durbinWatson test, and ncvTest
residual.analysis <- function(model){
  res.model = rstandard(model)
  par(mfrow=c(2,2))
  plot(model, which = c(1,1))
  hist(res.model, main = "Histogram of standardized residuals")
  qqnorm(res.model, main = "QQ plot of standardized residuals")
  qqline(res.model, col = 2, lty = 2)
  acf(res.model, main = "ACF of standarised residuals")
  print(shapiro.test(res.model))
  print(durbinWatsonTest(model))
  print(ncvTest(model))
}
#Forces plot to show in new window, must be using Windows OS
windows()
residual.analysis(submodel.1)

#Testing the model
dcbiketestsub <- select(dcbiketest, cnt, season, weathersit, rawfeeltemp, rawhumidity, rawwindspeed)
pred <- predict(submodel.1, dcbiketestsub)
output <- cbind(dcbiketestsub, pred)
RMSE(pred, dcbiketestsub$cnt)
er <- RMSE(pred, dcbiketestsub$cnt)/mean(dcbiketestsub$cnt)
mean(dcbiketestsub$cnt)
R2(pred, dcbiketestsub$cnt)
print(pred)
summary(pred)





### END of Assignment ###
### Following code was additional testing
# submodel.2 <- lm(cnt ~  rawfeeltemp, data = dcbiketrain)
# summary(submodel.2)
# anova(submodel.2)
# 
# drop1(update(modelraw, ~ . -workingday), test="F")
# 
# r <- regsubsets(cnt ~ season + workingday + weathersit + rawfeeltemp + rawhumidity + rawwindspeed, data = dcbiketrain)
# plot(r, scale="Cp")
# plot(r, scale="adjr2")
# 
# modelraw2 <- lm(cnt ~ season + workingday + weathersit + rawtemp + rawhumidity + rawwindspeed, data = dcbiketrain)
# summary(modelraw2)


