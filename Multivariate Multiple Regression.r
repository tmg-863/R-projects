#I need to revisit this to make updates, but thought it was worth putting it in my repo, for viewing.
#This code was used for my Master's Option Paper, pre-pandemic. 
#The goal for this script was to build a MMR Multivariate Multiple Regression to model
#multiple dependent variables, with a set of predictor variables.
#I wanted to see if transit access was a function of gender, race, and, workplace area characteristics (WAC);
#aka to see if gender, race, and WAC were predictors of transit access/equity (Number of Trips per Hour)
#in the Atlanta MARTA bus network. I tested these variables on 4 different times of day when traffic is at peak levels:
#MORNING RUSH, MIDDAY RUSH, AFTERNOON RUSH, EVENING RUSH
#The results are discussed in my paper.
#This project combined GIS, GTFS, and US Census Data; this script is the short but efficient combination of it all.


install.packages("dplyr")
library(dplyr)
install.packages("faraway")
library(faraway)
install.packages("GGally")
library(GGally)
ggpairs(JAN14_MIDDAY)
install.packages("car")
library(car)
#import .csv table
setwd("D:/OP FOR R/")
JAN14_MIDDAY<-read.csv(file="JAN14_MIDDAY.csv", header = TRUE, sep=",")
summary(JAN14_MIDDAY)
attach(JAN14_MIDDAY)
colnames(JAN14_MIDDAY)

#naming columns
names(JAN14_MIDDAY)<- c("NumTripsPerHr", "F_1", #Number of Trips Per Hour, Per Capita Income in USD
                             "B_2","B_3","B_4","B_5","B_6","B_7","B_8","B_9","B_10", #Race
                             "C_2","C_26", #Sex/Gender
                             "C000") #Workplace Area Characteristics
model_JAN14_MIDDAY = lm(NumTripsPerHr ~ 
             F_1+
             B_2+B_4+B_5+B_6+B_7+B_8+B_9+B_10+	
             C_2+C_26, data= JAN14_MIDDAY[complete.cases(JAN14_MIDDAY),])

#check for aliased variables to detect collinearity:
alias(model_JAN14_MIDDAY)

#run Variance Inflation Factor test
Rsq = summary(model_JAN14_MIDDAY)$r.squared
vif = 1/(1 - Rsq)
vif(model_JAN14_MIDDAY)

model2_JAN14_MIDDAY = lm(C000 ~ 
             F_1+
             B_2+B_4+B_5+B_6+B_7+B_8+B_9+B_10+	
             C_2+C_26, data= JAN14_MIDDAY[complete.cases(JAN14_MIDDAY),])

#check for aliased variables to detect collinearity:
alias(model2_JAN14_MIDDAY)

#run Variance Inflation Factor test
Rsq = summary(model2_JAN14_MIDDAY)$r.squared
vif = 1/(1 - Rsq)
vif(model2_JAN14_MIDDAY)

#it was determined that B_3 and B_5 need to be removed

#running the final multivariate multiple regresssion model
NumTripsPerHr_model_JAN14_MIDDAY<- lm(NumTripsPerHr ~ 
                          F_1+
                          B_2+B_4+B_6+B_7+B_8+B_9+B_10+	
                          C_2+C_26, data= JAN14_MIDDAY[complete.cases(JAN14_MIDDAY),])
summary(NumTripsPerHr_model_JAN14_MIDDAY)

