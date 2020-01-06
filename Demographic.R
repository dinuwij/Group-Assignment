#load libraries
library(data.table)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(readxl)
library(stringr)
library(tidyr)
library(haven)

#Cleaning Environment to make sure no errors came other objects in the environment:
rm(list = ls())
#Reading in AnalyticDataInternetGambling 
ActualInternetSportsGamblingActivity<-read_sas("C:/Users/spavot/Documents/GitHub/Group-Assignment/AnalyticDataInternetGambling.sas7bdat")

#Reading in RawDataIIUserDailyAggregation
UserDailyAggregation<-read_sas("C:/Users/spavot/Documents/GitHub/Group-Assignment/RawDataIIUserDailyAggregation.sas7bdat")

# #ActualInternetSportsGamblingActivity -- Code for Dinuu, delete if she already used it.
# for (i in colnames(ActualInternetSportsGamblingActivity)) {
#   if (class(ActualInternetSportsGamblingActivity[[i]]) == 'Date') {
#     ActualInternetSportsGamblingActivity[[i]] = as.character(ActualInternetSportsGamblingActivity[[i]])
#     ActualInternetSportsGamblingActivity[[i]] = replace_na(ActualInternetSportsGamblingActivity[[i]],'2011-01-01')
#     ActualInternetSportsGamblingActivity[[i]] = ymd(ActualInternetSportsGamblingActivity[[i]])
#   }
# }
# class(ActualInternetSportsGamblingActivity$FirstSportsActiveDate)




#Reading in RawDataIDemographics 
Demographics<-read_sas("C:/Users/spavot/Documents/GitHub/Group-Assignment/RawDataIDemographics.sas7bdat")

#Demographic: Cleaning data

#We first replace missing value with the value of 2 for gender
Demographics$Gender = replace_na(Demographics$Gender, 2)
#Now we replace 0 with female
Demographics$Gender[Demographics$Gender == 0] = "Female"
#1 with male
Demographics$Gender[Demographics$Gender == 1] = "Male"
#And 2 (missing values) with unknown
Demographics$Gender[Demographics$Gender == 2] = "Unknown"

#Converting character columns into date format:

#We replace missing values that are categorized with 'NULL' by a date easy to identify '20991212' in order to be able
#to flag them later
for (i in colnames(Demographics)) {
  Demographics[Demographics[i] == 'NULL',i] = "20991212"
}

#Now that there's no more missing value, we can convert the date column into date format
Demographics$RegDate = ymd(Demographics$RegDate)
Demographics$FirstPay = ymd(Demographics$FirstPay)
Demographics$FirstAct = ymd(Demographics$FirstAct)
Demographics$FirstSp = ymd(Demographics$FirstSp)
Demographics$FirstCa = ymd(Demographics$FirstCa)
Demographics$FirstGa = ymd(Demographics$FirstGa)
Demographics$FirstPo = ymd(Demographics$FirstPo)

#Now that we've fixed missings values, we still have the 2099-12-12 date in our data
#We're going to create a new column for FirstAct / FirstSP / FirstCA / FirstGA / First PO
#to identify if they already done this activity or not regarding the date we defined
#as it's corresponding to missing value.

Demographics$AlreadyPlaySportsBook = "Yes"
Demographics$AlreadyPlaySportsBook[Demographics$FirstSp == "2099-12-12"] = "No"
Demographics$AlreadyPlayCasino = "Yes"
Demographics$AlreadyPlayCasino[Demographics$FirstSp == "2099-12-12"] = "No"
Demographics$AlreadyPlayGames = "Yes"
Demographics$AlreadyPlayGames[Demographics$FirstSp == "2099-12-12"] = "No"
Demographics$AlreadyPlayPoker = "Yes"
Demographics$AlreadyPlayPoker[Demographics$FirstSp == "2099-12-12"] = "No"

#To fix country, language & applicationID, we import 3 excels files corresponding
#to the 3 appendix to transform these numeric values into character with meaning

Countries<-read_excel("C:/Users/spavot/Documents/GitHub/Group-Assignment/Country.xlsx")
Languages<-read_excel("C:/Users/spavot/Documents/GitHub/Group-Assignment/Language.xlsx")
ApplicationID<-read_excel("C:/Users/spavot/Documents/GitHub/Group-Assignment/ApplicationID.xlsx")

#Now we will merge these dataset with the demographic based on their respective keys (country for country, language for languages, applicationID for ApplicationID)

Demographics<-merge(Demographics, Countries, by.x="Country", by.y="Country")
Demographics<-merge(Demographics, Languages, by.x="Language", by.y="Language")
Demographics<-merge(Demographics, ApplicationID, by.x = "ApplicationID", by.y = "ApplicationID")

#We now drop the column we used to merge as we don't need them anymore.

Demographics$Country = Demographics$ApplicationID = Demographics$Language = NULL


#Reading in RawDataIIIPokerChipConversions 
PokerChipConversions<-read_sas("C:/Users/spavot/Documents/GitHub/Group-Assignment/RawDataIIIPokerChipConversions.sas7bdat")

#PokerChipConversions: Cleaning data

#First, we check for missing values:
for (i in colnames(PokerChipConversions)) {
  print(sum(is.na(PokerChipConversions[i])))
}
#No missing values apparently

#We will replace now the transaction type with buy for 124 and sell for 24:
PokerChipConversions[PokerChipConversions$TransType == 124,"TransType"] = "Buy"
PokerChipConversions[PokerChipConversions$TransType == 24,"TransType"] = "Sell"

#Now that we've done this, we're going to create two new columns "Buy" & "Sell" to have the amount of sell & buy
#First, we initialize these columns with 0
PokerChipConversions$Sell = 0
PokerChipConversions$Buy = 0

#Now we replace the 0 if it's sell with the amount for the column sell and same for buy column.
PokerChipConversions$Sell <- ifelse(PokerChipConversions$TransType == "Sell", PokerChipConversions$TransAmount, 0)
PokerChipConversions$Buy <- ifelse(PokerChipConversions$TransType == "Buy", PokerChipConversions$TransAmount, 0)

#Now, we will change TransDateTime format into date:
PokerChipConversions$TransDateTime = ymd_hms(PokerChipConversions$TransDateTime)

#Now, we will initiate DateDay, we will use this column to compute number of days 
#a player played and average money he spent and won. We will do the same for month.

PokerChipConversions$DateDay = substr(PokerChipConversions$TransDateTime,1,10)
PokerChipConversions$DateDay = ymd(PokerChipConversions$DateDay)
PokerChipConversions$Month = month(PokerChipConversions$DateDay)
#We iniate a column transac = 1 to count the number of transactions when we 
#will merge on UserID
PokerChipConversions$Transac = 1

#Dplyr to create special columns
#We use Dplyr to do groupby on differents values to compute new variables.
#We put them in new dataset that we're going to merge together at the end.

#Here we create a dataset containing sum and mean for Buy & Sell, the date of last transaction and
#the number of transactions per user
Poker_user = PokerChipConversions %>% 
  group_by(UserID) %>%
  summarise(Total_Sell = sum(Sell),Total_Buy = sum(Buy),Last_Trans = max(DateDay), Mean_Sell = mean(Sell), Mean_Buy = mean(Buy), Nb_Trans = sum(Transac))

#Here, we compute the mean sell & buy and the number of transactions per month.
Poker_Month = PokerChipConversions %>%
  group_by(UserID,Month) %>%
  summarise(mean(Sell),mean(Buy),sum(Transac))

#But, we only have these into one column where we want one column per month. So we use spread to make the data long.
Sell_Month =  spread(Poker_Month,"Month", 'mean(Sell)',fill = 0)
Sell_Month['mean(Buy)'] = Sell_Month['sum(Transac)'] = NULL
names(Sell_Month) = c('UserID','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct')

#Now that we have our column, we still have to merge on customer ID.
Sell_Month = Sell_Month %>%
  group_by(UserID) %>%
  summarise(February_Sell = sum(Feb), March_Sell = sum(Mar), April_Sell = sum(Apr),
            May_Sell = sum(May), June_Sell = sum(Jun), July_Sell = sum(Jul),
            August_Sell = sum(Aug), Sept_Sell = sum(Sep), Oct_Sell = sum(Oct))


#As we could only do the spread for one variable at the time, now we do the same but for Buy Values.
Buy_Month =  spread(Poker_Month,"Month", 'mean(Buy)',fill = 0)
Buy_Month['mean(Sell)'] = Buy_Month['sum(Transac)'] = NULL
names(Buy_Month) = c('UserID','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct')
Buy_Month = Buy_Month %>%
  group_by(UserID) %>%
  summarise(February_Buy = sum(Feb), March_Buy = sum(Mar), April_Buy = sum(Apr),
            May_Buy = sum(May), June_Buy = sum(Jun), July_Buy = sum(Jul),
            August_Buy = sum(Aug), Sept_Buy = sum(Sep), Oct_Buy = sum(Oct))


trans_Month =  spread(Poker_Month,"Month", 'sum(Transac)',fill = 0)
trans_Month['mean(Sell)'] = trans_Month['mean(Buy)'] = NULL
names(trans_Month) = c('UserID','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct')
trans_Month = trans_Month %>%
  group_by(UserID) %>%
  summarise(February_NbTrans = sum(Feb), March_NbTrans = sum(Mar), April_NbTrans = sum(Apr),
            May_NbTrans = sum(May), June_NbTrans = sum(Jun), July_NbTrans = sum(Jul),
            August_NbTrans = sum(Aug), Sept_NbTrans = sum(Sep), Oct_NbTrans = sum(Oct))

#Finally, we use the DateDay column we created before to create the sum of Buy & Sell per Active day and number
#of active days per user.

Poker_Day = PokerChipConversions %>%
  group_by(UserID,DateDay) %>%
  summarise(Sell = sum(Sell),Buy = sum(Buy),Transac = sum(Transac))
Poker_Day$NbDayPlayed = 1

#Now we only groupby userID to obtain one line per user with information about sell, buy, nb of days active etc..
Poker_Day = Poker_Day %>%
  group_by(UserID) %>%
  summarise(Mean_Day_Sell = mean(Sell),Mean_Day_Buy = mean(Buy),Mean_Day_NbTransac = mean(Transac), NbDaysPlayed = sum(NbDayPlayed))

#Finally we merge the different dataset computed to obtain one dataset containing more than 20 variables
#per user.
Poker_Final = merge(Poker_user, Sell_Month,'UserID')
Poker_Final = merge(Poker_Final, Buy_Month, 'UserID')
Poker_Final = merge(Poker_Final, trans_Month,'UserID')
Poker_Final = merge(Poker_Final, Poker_Day, 'UserID')

#We check if we have the same number of rows as we had of user ID at in the original dataset.
uniqueN(Poker_Final$UserID) == uniqueN(PokerChipConversions$UserID)