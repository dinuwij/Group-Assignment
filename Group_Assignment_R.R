######Data Preparation#####

#load libraries
library(data.table)
library(readr)
library(readxl)
library(lubridate)
library(readxl)
library(stringr)
library(tidyr)
library(haven)

#Reading in AnalyticDataInternetGambling 
ActualSportsGambling<-read_sas("C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/AnalyticDataInternetGambling.sas7bdat")

#Reading in RawDataIDemographics 
Demographics<-read_sas("C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/RawDataIDemographics.sas7bdat")


#Reading in RawDataIIIPokerChipConversions 
PokerChipConversions<-read_sas("C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/RawDataIIIPokerChipConversions.sas7bdat")


#Reading in RawDataIIUserDailyAggregation
UserDailyAggregation<-read_sas("C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/RawDataIIUserDailyAggregation.sas7bdat")

#Treating null values of ActualInternetSportsGamblingActivity with zero presuming that Null denotes 0 


#Treating NA values for date columns

for (i in colnames(ActualSportsGambling)) {
  if (class(ActualSportsGambling[[i]]) == 'Date') {
    ActualSportsGambling[[i]] = as.character(ActualSportsGambling[[i]])
    ActualSportsGambling[[i]] = replace_na(ActualSportsGambling[[i]],'2011-01-01')
    ActualSportsGambling[[i]] = ymd(ActualSportsGambling[[i]])
  }
}

#Treating NA values for all the other columns
ActualSportsGambling[is.na(ActualSportsGambling)] <- 0

#ActualSportsGambling$FOTotalStakes[is.na(ActualSportsGambling$FOTotalStakes)] <- 0
#ActualSportsGambling$FOTotalWinnings[is.na(ActualSportsGambling$FOTotalWinnings)] <- 0



#Gender category
ActualSportsGambling$GENDERCAT <- ifelse(ActualSportsGambling$GENDER=="0","Female","Male")

#Marking active customers
ActualSportsGambling$ACTIVECUSTOMER<- ifelse(ActualSportsGambling$FOTotalDaysActive>1,1,0)

#Marking idle customers
ActualSportsGambling$IDLECUSTOMER<- ifelse(is.na(ActualSportsGambling$LAFirstActiveDate),1,0)

#Marking loyal customers if they are active for or more than 60 days
ActualSportsGambling$LOYALCUSTOMER<-ifelse(ActualSportsGambling$FOTotalDaysActive>=60,1,0)

#Returning visitor for live action if they are active for or more than 15 days
ActualSportsGambling$LAReturningVisitor<-ifelse(ActualSportsGambling$LATotalDaysActive>=15,1,0)

#High performing customers - Total wins per number of bets


#Total bets made FO and LA


#Reading languages and merging with language ID
languages<-read_excel("C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/Language.xlsx")

ActualSportsGambling<-merge(ActualSportsGambling,languages, by.x="LANGUAGE", by.y="Language")

#Reading in country and merging with country ID

Countries<-read_excel("C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/Country.xlsx")

ActualSportsGambling<-merge(ActualSportsGambling, Countries, by.x="COUNTRY", by.y="Country")





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



