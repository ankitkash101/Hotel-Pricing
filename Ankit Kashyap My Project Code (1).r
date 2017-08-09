# Analysis of Hotel Room Pricing in Indian Market
# NAME: Ankit Kashyap
# EMAIL: ankitkashyap.te15@rvce.edu.in
# COLLEGE : R.V.COllege Of Engineering, Bengaluru.

##setting the directory and assigning a variabel to the data frame
setwd("C:/Users/Ankit/Desktop/harvard")

#Reading the dataset and creating a data frame
hotel.df<-read.csv(paste("Cities42.csv",sep = ""))

#Viewing the data
View(hotel.df)

#Removing the repeated date by gsub command

hotel.df$Date<-gsub("18-Dec-16", "Dec 18 2016", hotel.df$Date)
hotel.df$Date<-gsub("21-Dec-16", "Dec 21 2016", hotel.df$Date)
hotel.df$Date<-gsub("24-Dec-16", "Dec 24 2016", hotel.df$Date)
hotel.df$Date<-gsub("25-Dec-16", "Dec 25 2016", hotel.df$Date)
hotel.df$Date<-gsub("28-Dec-16", "Dec 28 2016", hotel.df$Date)
hotel.df$Date<-gsub("31-Dec-16", "Dec 31 2016", hotel.df$Date)
hotel.df$Date<-gsub("4-Jan-17", "Jan 04 2017", hotel.df$Date)
hotel.df$Date<-gsub("4-Jan-16", "Jan 04 2017", hotel.df$Date)
hotel.df$Date<-gsub("8-Jan-16", "Jan 08 2017", hotel.df$Date)
hotel.df$Date<-gsub("8-Jan-17", "Jan 08 2017", hotel.df$Date)
hotel.df$Date<-gsub("Jan 4 2017", "Jan 04 2017", hotel.df$Date)
hotel.df$Date<-gsub("Jan 8 2017", "Jan 08 2017", hotel.df$Date)

#Checking the dates

table(hotel.df$Date)

#Changing dates to factors for labelling 

hotel.df$Date<-factor(hotel.df$Date)

##Arrange the factors in order
is.factor(hotel.df$Date)

#Checking the labelling
levels(hotel.df$Date)

#Analyzing the summary of the data and describing the variables

library(psych)
describe(hotel.df)

summary(hotel.df)

#Corrgram

library(corrgram)

corrgram(hotel.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Hotel  data")
##through corrgram HasSwimming, StarRating, HotelCapital are very well correlated to RoomRent
##so we can take them as predictors

##Visualizing data for Y as Room rent and X1,X2,X3 as HasSwimmingPool, StarRating and HotelCapacity respectively

#Table for HasSwimmingPool
table(hotel.df$HasSwimmingPool)
hasSwim<-table(hotel.df$HasSwimmingPool)
barplot(hasSwim,main="Barrplot of Hotel Swimming Pool")

#Table for StarRating
hasstarRating<-table(hotel.df$StarRating)
barplot(hasstarRating,main = "Barrplot for Star Rating")

#BoxPlot for HotelCapacity
boxplot(hotel.df$HotelCapacity, main="Boxplot for Hotel Capacity",horizontal = TRUE)

#Scatterplot pair wise for predictor variable
library(car)
scatterplot(hotel.df$RoomRent,hotel.df$HotelCapacity,main="Hotel Capacity",ylab = "No. of Rooms", xlab="Rent in INR",cex=1.1)
scatterplot(hotel.df$StarRating,hotel.df$RoomRent,main="Star Rating",ylab = "Rent in INR", xlab="Rating in scale of 5",cex=1.1)
plot(jitter(hotel.df$HasSwimmingPool),jitter(hotel.df$RoomRent),main="Hotels having swimming pool",ylab = "Room Rent in INR", xlab="No. of Swimming Pool",cex=1.1)
library(lattice)
bwplot(HasSwimmingPool~RoomRent,data=hotel.df)


#Scatter plot matrix for RoomRent,HasswimmingPool,StarRating and Hotel Capacity
library(car)
scatterplotMatrix(
  hotel.df[
    ,c("RoomRent","HasSwimmingPool","HotelCapacity","StarRating")],
  spread=FALSE, smoother.args=list(lty=2),
  main="Scatter Plot Matrix", diagonal = "histogram")

#Corrgram for RoomRent,HasSwimmingPool,StarRating,HotelCapacity
library(corrgram)
hotelnew<-data.frame(hotel.df$RoomRent,hotel.df$StarRating,hotel.df$HotelCapacity,hotel.df$HasSwimmingPool)
corrgram(hotelnew, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram for Room Rent in Hotels")

#Variance Matrix, Correlation Matrix and Covariance Matrix
x<-hotel.df[,c("HasSwimmingPool","StarRating","HotelCapacity")]
y<-hotel.df[,c("RoomRent")]
cor(x,y)
cov(x,y)
var(x,y)

roomrent.df<-hotel.df[which(hotel.df$RoomRent<100000),]

#Comparing IsWeekend with RoomRent
#Visualization
weekend1<-table(hotel.df$IsWeekend)
library(lattice)
barplot(weekend1,main="Barrplot of Isweekend")
 
#Effect of Isweekend on RoomRent
pd = aggregate(RoomRent ~ IsWeekend, data = hotel.df, mean)
pd
boxplot(RoomRent~IsWeekend,data=hotel.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="Isweekend",main="Boxplot Presentation of Date Comparision with Roomrent")
#Effect of Isweekend on RoomRent without extreme outliers
boxplot(RoomRent~IsWeekend,data=roomrent.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="Isweekend",main="Boxplot Presentation of Date Comparision with Roomrent")

#Comparing RoomRent with Date
library(lattice)
histogram(~Date, data = hotel.df,main = "Distribution of IsWeekend", xlab= "Weekend=1 Not Weekend=0",col='orange')

#Effect of Date on RoomRent
dd = aggregate(RoomRent ~ Date, data = hotel.df, mean)
dd
library(car)
scatterplot(dd$Date,dd$RoomRent,main="Effect of Date on RoomRent",xlab="Date",ylab="RoomRent in INR")
#Effect of Date on RoomRent eliminating extreme outliers
boxplot(RoomRent~Date,data=roomrent.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="Date",main="Boxplot Presentation of Date Comparision with Roomrent")


#Comparing RoomRent with IsMetroCity
metro1<-table(hotel.df$IsMetroCity)
barplot(weekend1,main="Barrplot of IsMetroCity",xlab="Not Metr City=0 Metro City=1",ylab="No.of hotels")

#Effect of IsMetroCity on RoomRent
im = aggregate(RoomRent ~ IsMetroCity, data = hotel.df, mean)
im
boxplot(RoomRent~IsMetroCity,data=hotel.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="0=not metro city 1=metro city",main="Boxplot Presentation of IsMetroCity Comparision with Roomrent")
 
#Effect of IsMetroCity on RoomRent eliminating extreme outliers
boxplot(RoomRent~IsMetroCity,data=roomrent.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="0=not metro city 1=metro city",main="Boxplot Presentation of IsMetroCity Comparision with Roomrent")


#Comparing RoomRent with IsTouristDestinatio
tourist1<-table(hotel.df$IsTouristDestination)
barplot(tourist1,main="Barrplot of IsMetroCity",xlab="Not tourist destination=0 Tourist Destination=1",ylab="No.of hotels")

#Effect of IsTouristDestination on RoomRent
id = aggregate(RoomRent ~ IsTouristDestination, data = hotel.df, mean)
id
boxplot(RoomRent~IsTouristDestination,data=hotel.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="0=not tourist destination 1=tourist destination",main="Boxplot Presentation of IsMetroCity Comparision with Roomrent")

#Effect of IsTouristDestination on RoomRent eliminating extreme outliers
boxplot(RoomRent~IsTouristDestination,data=roomrent.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="0=not tourist destination 1=tourist destination",main="Boxplot Presentation of IsMetroCity Comparision with Roomrent")

#Effect of Airport and Ismetrocity on RoomRent
summary(hotel.df$Airport)
boxplot(hotel.df$Airport,horizontal = TRUE,xlab="Distance of airport from hotel in km.",main="Boxplot Presentation of Airport Distance from hotel")

#Effect of Airport and Ismerocity on roomrent eliminating extreme outliers
boxplot(roomrent.df$Airport,horizontal = TRUE,xlab="Distance of airport from hotel in km.",main="Boxplot Presentation of Airport Distance from hotel")

#Comparing FreeWifi and RoomRent
free1<-table(hotel.df$FreeWifi)
library(lattice)
barplot(free1,main="Barrplot of FreeWifi")

#Effect of FreeWifi on RoomRent
fw = aggregate(RoomRent ~ FreeWifi, data = hotel.df, mean)
fw
boxplot(RoomRent~FreeWifi,data=hotel.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="FreeWifi",main="Boxplot Presentation of FreeWifi with Roomrent")

#Effect of FreeWifi on RoomRent eliminating extreme outliers
boxplot(RoomRent~FreeWifi,data=roomrent.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="FreeWifi",main="Boxplot Presentation of FreeWifi with Roomrent")

#Comparing FreeBreakfast and RoomRent
free2<-table(hotel.df$FreeBreakfast)
library(lattice)
barplot(free2,main="Barrplot of FreeBreakfast")

#Effect of FreeWifi on RoomRent
fb = aggregate(RoomRent ~ FreeWifi, data = hotel.df, mean)
fb
boxplot(RoomRent~FreeBreakfast,data=hotel.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="FreeBreakfast",main="Boxplot Presentation of FreeBreakfast with Roomrent")

#Effect of FreeWifi on RoomRent eliminating extreme outlier
boxplot(RoomRent~FreeBreakfast,data=roomrent.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="FreeBreakfast",main="Boxplot Presentation of FreeBreakfast with Roomrent")
boxplot(RoomRent~IsTouristDestination,data=roomrent.df,horizontal = TRUE,xlab="RoomRent in INR",ylab="0=not tourist destination 1=tourist destination",main="Boxplot Presentation of IsMetroCity Comparision with Roomrent")

#Hpothesis
#1.Average RoomRent in hotels having swimmingpool is more than that which don't have
t.test(RoomRent~HasSwimmingPool,data=hotel.df,alternative="less")

#2.Average RoomRent in hotels with high star rating is high as compared to one which has low star rating.
t.test(hotel.df$RoomRent,hotel.df$StarRating)

#3.Average RoomRent in hotels providing Free Breakfast is more than that which don't provide free breakfast
t.test(RoomRent~FreeBreakfast,data=hotel.df,alternative="less")

#4.Average RoomRent in metro cities hotels is more than that of non metro cities.
t.test(RoomRent~IsMetroCity,data=hotel.df,alternative="less")

#5.Average RoomRent in hotels having higher capacity is more than than hotels having less capacity
t.test(hotel.df$RoomRent,hotel.df$HotelCapacity,alternative="less")

#Regression Model
#1. ##Generating A Multi Variable Linear Regressional Model for Hotel RoomRent

linear1.mod<- lm(RoomRent~ HasSwimmingPool + StarRating + HotelCapacity -1, data = hotel.df)
summary(linear1.mod)
#Coefficients of the model
coefficients(linear1.mod)
##   Model:    salary = b0 + b1*HasSwimmingPool + b2*StarRating + b3*HotelCapacity
##  b0 = -1(assumption),  b1 = 3719.694300,b2= 1396.874562,b3=-7.659814
## Model:    salary = -1 + 3719.694300*HasSwimmingPool+1396.874562*StarRating+ -7.659814*HotelCapacity


#2.

linear2.mod<- lm(RoomRent~ IsWeekend + IsTouristDestination -1, data = hotel.df)
summary(linear2.mod)
#Coefficients of the model
coefficients(linear2.mod)
##   Model:    salary = b0 + b1*IsWeekend + b2*IsTouristDestination
##  b0 = -1(assumption),  b1 = 2286.297,b2= 4656.327
## Model:    salary = -1 + 2286.297*IsWeekend + 4656.327*IsTouristDestination


#3.
linear3.mod<- lm(RoomRent~ HasSwimmingPool + StarRating + HotelCapacity + Airport  -1, data = hotel.df)
summary(linear3.mod)
#Coefficients of the model
coefficients(linear3.mod)
 
##   Model:    salary = b0 + b1*HasSwimmingPool + b2*StarRating + b3*HotelCapacity+b4*Airport
##  b0 = -1(assumption),  b1 = 3903.736921     b2=1248.426988       b3=-6.743354     b4=  18.869726 
## Model:    salary = -1 + 3903.736921 *HasSwimmingPool +1248.426988*StarRating+ -6.743354*HotelCapacity +18.869726 *AirPort
