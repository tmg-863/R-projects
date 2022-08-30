#set working directory
setwd("XXXXX")

#load census data
STQ_CENSUS<-read.csv("STQ_CENSUS.csv", header=TRUE,sep = ",",stringsAsFactors = FALSE)
print(STQ_CENSUS)
summary(STQ_CENSUS)

ARC_CENSUS<-read.csv("ARC_CENSUS.csv", header=TRUE,sep = ",",stringsAsFactors = FALSE) 
print(ARC_CENSUS)
summary(ARC_CENSUS)

#merge the above files to create land use dataset, print new land use dataset
LAND_USE<-merge(STQ_CENSUS,ARC_CENSUS,by.x = "TRACT_num",by.y = "ct00")
print(LAND_USE)
summary(LAND_USE)

#linking land use attributes to survey respondents
PERSON<-read.csv("STQ_PERSON.csv", header=TRUE,sep = ",", stringsAsFactors = FALSE)
print(PERSON)
summary(PERSON)

#use "tract" field as a key variable to join the df to the land use df
PERSON_LAND_USE<-merge(LAND_USE,PERSON,by.x = "TRACT_num",by.y = "tract") 
print(PERSON_LAND_USE)
summary(PERSON_LAND_USE)

#load the STQ_TRIP file, assign it the name "TRIPS"
TRIPS<-read.csv("STQ_TRIP.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(TRIPS)
summary(TRIPS)
#subset trips file by selecting day 1 from the data and create new dataset, assigned below
TRIP_1day<-TRIPS[ which(TRIPS$dayno==1), c("HHID","PERID","NUMTRIPS","DISTANCE","MINUTES")] 
print(TRIP_1day)
summary(TRIP_1day)
#aggregate the trip 1 dataset by both households and person
TRIP_AGG<-aggregate(TRIP_1day[,c("NUMTRIPS","DISTANCE","MINUTES")], by=TRIP_1day[,c("HHID","PERID")],FUN=sum, na.rm=TRUE)#household and persons within 
#every household are aggregated in this dataset
print(TRIP_AGG)
summary(TRIP_AGG)

#Create an Integrated Land Use and Travel Behavior Dataset

#merge HHID and PERID files 
PERSON_LAND_USE_TRIPS<-merge(PERSON_LAND_USE,TRIP_AGG, c("HHID","PERID"))
print(PERSON_LAND_USE_TRIPS)
summary(PERSON_LAND_USE_TRIPS)

#Fit a Linear Model

#must first recode survey responses to better fit the regression equation

#driver's license, needs to be a binary variable and function as a dummy variable
print(PERSON_LAND_USE_TRIPS$lic)
summary(PERSON_LAND_USE_TRIPS$lic)

#create new variable lic2 since we are recoding things

PERSON_LAND_USE_TRIPS$lic2[PERSON_LAND_USE_TRIPS$lic==2] <- 0 #no
PERSON_LAND_USE_TRIPS$lic2[PERSON_LAND_USE_TRIPS$lic==1] <- 1 #yes
PERSON_LAND_USE_TRIPS$lic2[PERSON_LAND_USE_TRIPS$lic== is.na] <- NA #other response

print(PERSON_LAND_USE_TRIPS$lic2)

#recode the remaining dummy variable (female/gender)in the same fashion
#gender
print(PERSON_LAND_USE_TRIPS$gender)
summary(PERSON_LAND_USE_TRIPS$gender)

PERSON_LAND_USE_TRIPS$female[PERSON_LAND_USE_TRIPS$gender==1]<- 0 #no
PERSON_LAND_USE_TRIPS$female[PERSON_LAND_USE_TRIPS$gender==2]<- 1 #yes
PERSON_LAND_USE_TRIPS$female[PERSON_LAND_USE_TRIPS$gender==9]<- NA #other response

print(PERSON_LAND_USE_TRIPS$female)
summary(PERSON_LAND_USE_TRIPS$female)
#recode continuous variable
#age
PERSON_LAND_USE_TRIPS$age[PERSON_LAND_USE_TRIPS$age==999]<- NA
print(PERSON_LAND_USE_TRIPS$age)
summary(PERSON_LAND_USE_TRIPS$age)
#now that age is recoded, must also recode HH Size
PERSON_LAND_USE_TRIPS$HH_hhsize[PERSON_LAND_USE_TRIPS$HH_hhsize==999]<- NA
print(PERSON_LAND_USE_TRIPS$HH_hhsize)
summary(PERSON_LAND_USE_TRIPS$HH_hhsize)
#time to recode my selected variable: 
#so, keeping the variable name, subset 999 (no response), then assign that subset as NA
#education
PERSON_LAND_USE_TRIPS$educa[PERSON_LAND_USE_TRIPS$educa==999]<- NA
print(PERSON_LAND_USE_TRIPS$educa)
summary(PERSON_LAND_USE_TRIPS$educa)
#Regression Equation
ddm<-lm(NUMTRIPS~lic2+age+female+HH_hhsize+educa, PERSON_LAND_USE_TRIPS)
print(ddm)
summary(ddm)

#install necessary packages
install.packages("dplyr")
install.packages("rgdal")
install.packages("ggplot2")
install.packages("stats")

library(dplyr)
library(rgdal)
library(ggplot2)
library(stats)
#Summarize Model and test regression
ddm.resids = resid(ddm) #Test for trends in the residual
plot(ddm.resids) #Plots ddm.resids, thereby enabling visualization of residuals
hist(ddm.resids) #Histogram showing skew direction/if it is a normal distribution
qqnorm(ddm.resids) #Q-Q plot to denote any curves

#Summary Output of the model
install.packages("stargazer")
library(stargazer)

stargazer(ddm, type = "text")


detach() 
quit()
