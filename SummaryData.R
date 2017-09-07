library(MASS)
library(psych)
cardata=read.table("truegasfile.txt",header=T)
carbox=read.table("gasbox.txt",header=T) ## data file with only variables with more than one entry 
card=read.table("gasboxfile.txt",header=T)
head(card)
head(cardata)

factorA=as.factor(cardata[,5]) ## Travel Destination
factorB=as.factor(cardata[,6]) ## CityHigh Way
factorC=as.factor(cardata[,7]) ## Where was I going
factorD=as.factor(cardata[,8]) ## GasType

factorAa=as.factor(carbox[,5]) ## Travel Destination
factorBb=as.factor(carbox[,6]) ## CityHigh Way
factorCc=as.factor(carbox[,7]) ## Where was I going
factorDd=as.factor(carbox[,8]) ## GasType


factora=as.factor(card[,5]) ## Travel Destination
factorb=as.factor(card[,6]) ## CityHigh Way
factorc=as.factor(card[,7]) ## Where was I going
factord=as.factor(card[,8]) ## GasType
factore=as.factor(card[,9]) ## Season

## By Season Box Plots
par(mfrow=c(2,2))
boxplot(Gallons~Season,data=card,main="Boxplot Seasons (Gallons)",
        xlab="Seasons",ylab="# of Gallons")
boxplot(PerGallon~Season,data=card,main="Boxplot Seasons (Per Gallons)",
        xlab="Seasons",ylab="$ Per Gallons")
boxplot(numofMiles~Season,data=card,main="Boxplot Seasons (Miles)",
        xlab="Seasons",ylab="# of Miles")
boxplot(numofdays~Season,data=card,main="Boxplot Seasons (Days)",
        xlab="Seasons",ylab="# of Days")



## By travel Destination Box Plots
par(mfrow=c(2,2))
boxplot(Gallons~TravelDes,data=carbox,main="Boxplot Travel Destination (Gallons)",
        xlab="Destination",ylab="# of Gallons")
boxplot(PerGallon~TravelDes,data=carbox,main="Boxplot Travel Destination (Per Gallon)",
        xlab="Destination",ylab="$ per Gallon")
boxplot(numofMiles~TravelDes,data=carbox,main="Boxplot Travel Destination (Miles)",
        xlab="Destination",ylab="# of Miles")
boxplot(numofdays~TravelDes,data=carbox,main="Boxplot Travel Destination (Days)",
        xlab="Destination",ylab="# of Days")




head(cardata)
## By City Highway Box Plots
par(mfrow=c(2,2))
boxplot(Gallons~CitHig,data=carbox,main="Boxplot City Highway(Gallons)",
        xlab="City Highway",ylab="# of Gallons")
boxplot(PerGallon~CitHig,data=carbox,main="Boxplot City Highway (Per Gallon)",
        xlab="City Highway",ylab="$ per Gallon")
boxplot(numofMiles~CitHig,data=carbox,main="Boxplot City Highway (Miles)",
        xlab="City Highway",ylab="# of Miles")
boxplot(numofdays~CitHig,data=carbox,main="Boxplot City Highway (Days)",
        xlab="City Highway",ylab="# of Days")






head(cardata)
## By Where I was going Box Plots
#boxplot(Gallons~Where,data=carbox,main="Boxplot Where (Gallons)",
#        xlab="MD NJ",ylab="# of Gallons")
#boxplot(PerGallon~Where,data=carbox,main="Boxplot Where (Per Gallon)",
#        xlab="MD NJ",ylab="$ per Gallon")
#boxplot(numofMiles~Where,data=carbox,main="Boxplot Where (Miles)",
#        xlab="MD NJ",ylab="# of Miles")
#boxplot(numofdays~Where,data=carbox,main="Boxplot Where (Days)",
#        xlab="MD NJ",ylab="# of Days")




head(cardata)
## By GasType Box Plots
par(mfrow=c(2,2))
boxplot(Gallons~GasType,data=carbox,main="Boxplot Gas Type (Gallons)",
        xlab="Gas",ylab="# of Gallons")
boxplot(PerGallon~GasType,data=carbox,main="Boxplot Gas Type (Per Gallon)",
        xlab="Gas",ylab="$ per Gallon")
boxplot(numofMiles~GasType,data=carbox,main="Boxplot Gas Type (Miles)",
        xlab="Gas",ylab="# of Miles")
boxplot(numofdays~GasType,data=carbox,main="Boxplot Gas Type (Days)",
        xlab="Gas",ylab="# of Days")




## Descriptive Statistics
head(cardata) ## Travel Destination
TraGA=as.matrix(describeBy(cardata[,1],factorA)) ## Gallons
TraPG=as.matrix(describeBy(cardata[,2],factorA)) ## PerGallons
TraNM=as.matrix(describeBy(cardata[,3],factorA)) ## numofmiles
TraND=as.matrix(describeBy(cardata[,4],factorA)) ## numofdays
write.table(TraGA,"TravelDes_Gallon.txt")
write.table(TraPG,"TravelDes_pergallon.txt")
write.table(TraNM,"TravelDes_numofmiles.txt")
write.table(TraND,"TravelDes_numofdays.txt")


head(cardata) ## CityHighway
CHGA=as.matrix(describeBy(cardata[,1],factorB)) ## Gallons
CHPG=as.matrix(describeBy(cardata[,2],factorB)) ## PerGallons
CHNM=as.matrix(describeBy(cardata[,3],factorB)) ## numofmiles
CHND=as.matrix(describeBy(cardata[,4],factorB)) ## numofdays

write.table(CHGA,"CityHighway_Gallon.txt")
write.table(CHPG,"CityHighway_pergallon.txt")
write.table(CHNM,"CityHighway_numofmiles.txt")
write.table(CHND,"CityHighway_numofdays.txt")


head(cardata) ## Where
WGA=as.matrix(describeBy(cardata[,1],factorC)) ## Gallons
WPG=as.matrix(describeBy(cardata[,2],factorC)) ## PerGallons
WNM=as.matrix(describeBy(cardata[,3],factorC)) ## numofmiles
WND=as.matrix(describeBy(cardata[,4],factorC)) ## numofdays
write.table(WGA,"Where_Gallon.txt")
write.table(WPG,"Where_pergallon.txt")
write.table(WNM,"Where_numofmiles.txt")
write.table(WND,"Where_numofdays.txt")

head(cardata) ## GasType
GTGA=as.matrix(describeBy(cardata[,1],factorD)) ## Gallons
GTPG=as.matrix(describeBy(cardata[,2],factorD)) ## PerGallons
GTNM=as.matrix(describeBy(cardata[,3],factorD)) ## numofmiles
GTND=as.matrix(describeBy(cardata[,4],factorD)) ## numofdays
write.table(GTGA,"GasType_Gallon.txt")
write.table(GTPG,"GasType_pergallon.txt")
write.table(GTNM,"GasType_numofmiles.txt")
write.table(GTND,"GasTypeWhere_numofdays.txt")


############## Analysis of DATA


## A1) Is the difference between gas stations significant with respect 
## to number of days, number of miles and cost per gallon (Controlling for CityHighway)?
## Method One: MANOVA
# Y1: Gallons, PerGallon, numofMiles, numofdays
Y1=cbind(cardata[,1],cardata[,2],cardata[,3],cardata[,4])
fit1=manova(Y1~factorD)
summary.manova(fit1)
### INT: This summary shows that there is a significant difference between 
### gas stations with respect to number of gallons number of days, 
### number of miles and cost per gallon 

## Method Two: Four seperate ANOVAs
fit1aa=aov(cardata[,1]~factorD) ## The # of Gallon
summary(fit1aa)
### INT: There is a difference between gas stations with respect to # of gallons

fit1a=aov(cardata[,2]~factorD) ## Cost Per Gallon
summary(fit1a)
### INT: No difference between gas stations with respect to cost per gallon

fit1b=aov(cardata[,3]~factorD) ## Number of Miles
summary(fit1b)
### INT: No difference between gas stations with respect to number of miles

fit1c=aov(cardata[,4]~factorD) ## Number of days
summary(fit1c)




## Method Two: Four seperate ANOVAs by season
fit11=aov(card[,1]~factore) ## The # of Gallon
summary(fit11)
### INT: There is no difference between seasons with respect to # of gallons

fit22=aov(card[,2]~factore) ## Cost Per Gallon
summary(fit22)
### INT: No difference between gas stations with respect to cost per gallon

fit33=aov(card[,3]~factore) ## Number of Miles
summary(fit33)
### INT: No difference between gas stations with respect to number of miles

fit44=aov(card[,4]~factore) ## Number of days
summary(fit44)

fitsg=lm(card[,3]~factore:factord) ## Number of Miles
summary(fitsg)








### INT: Slight difference between gas stations with respect to number of days

## A2) Is the difference between gas stations significant with respect 
## to number of days, number of miles and cost per gallon (Not Controlling for CityHighway)?
## Method One: MANOVA with Interaction
fit2=manova(Y1~factorD+factorB+factorD*factorB)
summary(fit2)
### INT: This summary shows that there is a difference between 
### gas stations wrt number of days, number of miles and cost per gallon 
### (Not Controlling for CityHighway). This also shows that there 
### is a significant difference between city and highway driving, however, the 
### interaction between gas station and city highway is not significant. 

## Method Two: Three seperate ANOVAs
fit2a=aov(cardata[,2]~factorD+factorB+factorD*factorB) ## Cost Per Gallon
summary(fit2a)
### INT: No difference between gas stations, city highway and interaction
### with respect to cost per gallon

fit2b=aov(cardata[,3]~factorD+factorB+factorD*factorB) ## Number of Miles
summary(fit2b)
### INT: There is a difference between city highway and interaction
### with respect to cost per gallon, however, the gas stations factor is not significant

fit2c=aov(cardata[,4]~factorD+factorB+factorD*factorB) ## Number of days
summary(fit2c)
### INT: There is a difference between city highway and gas station
### with respect to cost per gallon, however, the interaction is not significant






## B) Which gas station is the best? (regression)
##   1) By the  amount of miles:
##       a) In general (controling for type of driving)

model1=lm(carbox[,3]~factorDd)
summary(model1)

##       b) on the highway vs in the city?
model2=lm(carbox[,3]~factorBb*factorDd)
summary(model2)


##   2) Which gas station is cheaper?
##       a) In general (controling for region)
model3=lm(carbox[,2]~factorDd)
summary(model3)

head(carbox)
##  b) New Jersey vs Maryland
model4=lm(carbox[,2]~factorCc*factorDd)
summary(model4)

## C) Can we verify 27 mpg city and 33 mpg highway? Does this hold for NJ and Maryland?
## Miles per gallon implies # of miles / # gallons, the number of 
## gallons the nissan versa hold is 13.2 gallons
cardatav=cbind(cardata,cardata[,3]/13.2)
head(cardatav)
## D) Is there a difference in Maryland driving versus New Jersey driving?

###### Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
f1 <- function(vec) {
  m <- mean(vec, na.rm = TRUE)
  vec[is.na(vec)] <- m
  return(vec)
}

modcar = apply(cardata[,1:4],2,f1)


fit <- princomp(modcar, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components





