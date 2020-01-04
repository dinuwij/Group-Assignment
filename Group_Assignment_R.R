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
ActualInternetSportsGamblingActivity<-read_sas("C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/AnalyticDataInternetGambling.sas7bdat")

#Reading in RawDataIDemographics 
Demographics<-read_sas("C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/RawDataIDemographics.sas7bdat")


#Reading in RawDataIIIPokerChipConversions 
PokerChipConversions<-read_sas("C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/RawDataIIIPokerChipConversions.sas7bdat")


#Reading in RawDataIIUserDailyAggregation
UserDailyAggregation<-read_sas("C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/RawDataIIUserDailyAggregation.sas7bdat")

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



