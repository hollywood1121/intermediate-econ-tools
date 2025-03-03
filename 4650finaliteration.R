# make sure you have all libraries installed and activated
install.packages('gtsummary')
install.packages('lemon')
install.packages("qwraps2")
library(ggplot2)
library(arm)
library(ipumsr)
library(Rcpp)
library(dplyr)
library(readxl)
library(Hmisc)
library(psych)
library(lmtest)
library(tidyverse)
library(skedastic)
library(jtools)
library(dynlm)
library(lmtest)
library(plm)
library(AER)
library(margins)
library(orcutt)
library(spdep)
library(lemon)
library(kableExtra)
library(qwraps2)
setwd("C:\\Users\\x\\Desktop\\econ4650\\")

#this is to remove scientific notation numbers.
options(scipen=999)

# import and view data
#imported mortgage rates, a huge file of alll stuff available for 2005- 2021q1.


#edit this to be where your xls file is, use the one i sent as it has all new updated sources
prjectdata <- read_excel("C:\\Users\\x\\Desktop\\econ4650\\paper-sources\\Project_data_7.26.21.xls")
prjectdata.tst <- prjectdata

#make it easy to read variables for the summary. 
# RUN ALL OF THESE VARIABLES AS THEY ARE USED
medianhouseprice <- prjectdata$MSPUS
GDP <- prjectdata$GDP
compensation <- prjectdata$A576RC1
unemployment <- prjectdata$UNRATE
psaving <-prjectdata$PMSAVE
M2_supply <- prjectdata$M2SL
M2_velocity <- prjectdata$M2V
p_expend <- prjectdata$PCEC96
inflation<- prjectdata$CPILFESL_NBD20050101
labor_market <- prjectdata$FRBKCLMCIM
labor_share <- prjectdata$LaborShare
fed_funds <- prjectdata$FEDFUNDS
mortgaterate<- prjectdata$MORTGAGE30US
disp_income <- prjectdata$DSPIC96
emp_popratio <- prjectdata$EMRATIO
housepriceindex <- prjectdata$USSTHPI
employeecost <- prjectdata$ECIWAG
comm_loans <- prjectdata$TOTCI
minwage<- prjectdata$FEDMINNFRWG
workingpop <- prjectdata$LFWA64TTUSM647S
fulltimeweekly <- prjectdata$LES1252881600Q
black <- prjectdata$BOAAAHORUSQ156N
prjectdata$empcsquared <- (prjectdata$ECIWAG)^2  
prjectdata$minwagesquared <- (prjectdata$FEDMINNFRWG)^2
empcostsquared <- prjectdata$empcsquared
minwages2 <- prjectdata$minwagesquared
prjectdata$minwageslog <- log(prjectdata$FEDMINNFRWG)
minwagelog <- prjectdata$minwageslog

prettyprintsumm <- data.frame( log(medianhouseprice), employeecost, empcostsquared, minwage, minwages2, labor_market)

summary(prettyprintsumm) %>%
  kbl() %>%
  kable_material(c("striped", "hover"))


# plot to see whats up, see the correlations which are causing issues.
plot(lm(prjectdata$MSPUS ~ GDP + compensation + unemployment + psaving + M2_supply + M2_velocity + inflation + p_expend + labor_market )) 



# running into multi-collinearity, throw values into df and run a cor matrix


# set up different data frames to run correlation matrices
# correlations need to be as close to 0 as possible.
dftest <- data.frame (
  GDP,
  minwage,
  inflation,
  
 
  employeecost,
  compensation, 
  unemployment,
  psaving,
  M2_supply,
  M2_velocity, 
  p_expend, 
 
  labor_market, 
  labor_share,
  fed_funds, 
  mortgaterate,
  disp_income,
  emp_popratio,
  housepriceindex,
 
  comm_loans,
  workingpop,
  fulltimeweekly
)
#print out easier to read
mixedCor(dftest)

# final variables found after finding the correlations of betas
dftest6 <- data.frame (
 
  labor_market,
  employeecost,
  minwage
  
  
)
mixedCor(dftest6)





#####################################################################################
#####################################################################################
############################### FINAL ANSWER ########################################
#####################################################################################
#####################################################################################



# note the following: after reading more about autocorrelation (problem often arising 
# from using time-series data like we are) we need to log the dependent variable.
# when logging the dependent variable, it will decrease the variance.


# dynamic autoregressive model for time series
# final answer timeseries <- dynlm(log( medianhouseprice ) ~ labor_market + employeecost + empcostsquared + minwage + minwages2 )
timeseries <- dynlm(log( medianhouseprice ) ~  employeecost + empcostsquared + minwage + minwages2 + labor_market )

# failed model timeseries <- dynlm(log( medianhouseprice ) ~ labor_market + employeecost + empcostsquared + minwage + minwages2 + L(log( medianhouseprice )) )
summary(timeseries)

plot(timeseries, pch = 16, col = "red")
timeseries$residuals


bptest(timeseries)
# p value is greater than 0.05 we fail to reject the null hypothesis, meaning this test is good.

# we confirmed the following:
# - standard errors are reasonable
# - model's p value is in check 
# - beta's p value is in check 
# - beta's t value is in check 
# - Homoscedasticity is present
# - increased adjusted R squared by increasing explanatory variable power ( empcostsquared, minwages2 )


# this is using log-level interpretation: 
# a 1 unit change in (insert beta here) means a 100 * (insert estimate here) percent 
# change in the dependent variable log(medianhouseprice)

timeseries %>% tbl_summary()












