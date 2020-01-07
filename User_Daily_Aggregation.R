#load libraries
library(data.table)
library(readr)
library(readxl)
library(lubridate)
library(readxl)
library(stringr)
library(tidyr)
library(haven)
library(dplyr)
install.packages("haven")
library(haven)
library(ggplot2)
install.packages("shiny")
library(shiny)
library(hms)
if (!require("DT")) install.packages('DT')
library(datasets)

#Reading in AnalyticDataInternetGambling 
ActualInternetSportsGamblingActivity<-read_sas("C:/Users/alopezzeron/Desktop/Fall_2019/Business_Analytics_Tools_OS/Group_Assignment/Group-Assignment/AnalyticDataInternetGambling.sas7bdat")

#Reading in RawDataIDemographics 
Demographics<-read_sas("C:/Users/alopezzeron/Desktop/Fall_2019/Business_Analytics_Tools_OS/Group_Assignment/Group-Assignment/RawDataIDemographics.sas7bdat")


#Reading in RawDataIIIPokerChipConversions 
PokerChipConversions<-read_sas("C:/Users/alopezzeron/Desktop/Fall_2019/Business_Analytics_Tools_OS/Group_Assignment/Group-Assignment/RawDataIIIPokerChipConversions.sas7bdat")


#Reading in RawDataIIUserDailyAggregation
UserDailyAggregation<-read_sas("C:/Users/alopezzeron/Desktop/Fall_2019/Business_Analytics_Tools_OS/Group_Assignment/Group-Assignment/RawDataIIUserDailyAggregation.sas7bdat")

#Treating null values of ActualInternetSportsGamblingActivity with zero presuming that Null denotes 0 as rest of the data is available

replace_na(ActualInternetSportsGamblingActivity, list(FOFirstActiveDate = 0))
replace_na(ActualInternetSportsGamblingActivity, list(as.Date.numeric(FOLastActiveDate = 0, origin = "2005-01-01")))

ActualInternetSportsGamblingActivity[is.na(ActualInternetSportsGamblingActivity)] <- 0


#Treating date values of demographics

Demographics$FirstPay<-ymd(Demographics$FirstPay)
Demographics$FirstAct<-ymd(Demographics$FirstAct)
Demographics$FirstSp<-ymd(Demographics$FirstSp)
Demographics$FirstCa<-ymd(Demographics$FirstCa)
Demographics$FirstGa<-ymd(Demographics$FirstGa)
Demographics$FirstPo<-ymd(Demographics$FirstPo)

Demographics$FirstCa<-as.numeric(as.character(Demographics$FirstCa)) 

#Merging of the data merge(x, y, by = "k1")
datam_v1<-merge(Demographics,UserDailyAggregation, by="UserID")
datam_v2<-merge(datam_v1,PokerChipConversions, by="UserID")
datamart_final<-merge(datam_v2,ActualInternetSportsGamblingActivity, by.x="UserID", by.y="USERID")


#UserDailyAggregation
UserDailyAggregation$Date <- as_date(UserDailyAggregation$Date) 
as.numeric(difftime(max(UserDailyAggregation$Date),min(UserDailyAggregation$Date)))

#UserDailyAggregation by UserID with Stats of Players - New Table
UserDailyAggregationID <- UserDailyAggregation %>% group_by(UserID) %>%
  summarise(GainsPerDay=round(mean(Winnings-Stakes),digits=2),WinningsPerDay=round(mean(Winnings),digits=2),
            StakePerDay=round(mean(Stakes),digits=2),OverallGains=round(sum(Winnings)-sum(Stakes),digits=2),
            BetsPerDay=round(mean(Bets),digits=2),
            StakesPerBet=round(mean(Stakes/Bets,na.rm = TRUE),digits=2),
            TotalNumberofBets=round(sum(Bets),digits=0))

#New Table - Bets per player per product
ProductUserPref <- UserDailyAggregation %>% group_by(UserID,ProductID)  %>% 
  summarise(BetsOnProducts=sum(Bets))

ProductUserPref <- spread(ProductUserPref,ProductID,BetsOnProducts,fill=0)
ProductUserPref["3"] = 0
ProductUserPref <- ProductUserPref[,c(1,2,3,9,4,5,6,7,8)]
ProductUserPref

#RatiosProdcuts for UserDailyAggregationID 
for (x in 1:8) {
  Name <- paste("Product_Ratio", as.character(x),sep="")
  print(Name)
  UserDailyAggregationID[Name] = round(ProductUserPref[,x+1]/UserDailyAggregationID$TotalNumberofBets,digits=2)
}  

#Products Types
UserDailyAggregationProduct <- UserDailyAggregation %>% group_by(ProductID) %>% 
  summarise(BetsPerDay=round(sum(Bets)/as.numeric(difftime(max(Date),min(Date))),digits=1),StakesPerBet=round(mean(Stakes/Bets,na.rm=TRUE),digits=3),
                             StakesPerDay=round(mean(Stakes),digits=2),
                             WinningsPerDay=round(mean(Winnings),digits=2),
                             WinningsStakesRatio=round(WinningsPerDay/StakesPerDay,digits=3),
                             TotalNumberofBets=round(sum(Bets),digits=0))
UserDailyAggregationProduct  

