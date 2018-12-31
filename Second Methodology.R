

library(partykit)
library(CHAID)
library(lubridate)
library(dplyr)
# Reading the input File

chaidInput= read.csv(file='D:/Delhivery/Codes/Sunday Prediction/20181221/CompleteData.csv', na.strings = "NA", stringsAsFactors = F)
chaidInput$weekday <- as.POSIXlt(chaidInput$cs_sd)$wday

# Getting the day of the Week
chaidInput$Day <- ifelse(chaidInput$weekday==0,'Sunday',(ifelse(chaidInput$weekday==1,'Monday',(ifelse(chaidInput$weekday==2,'Tuesday',(ifelse(chaidInput$weekday==3,'Wednesday',(ifelse(chaidInput$weekday==4,'Thursday',(ifelse(chaidInput$weekday==5,'Friday', (ifelse(chaidInput$weekday==6,'Saturday','Null')))))))))))))



DaywiseDelivery <- as.data.frame(table(chaidInput$Day))


# Filtering for ucidhaving count of wbn >=5


greaterThanFive <- aggregate(wbn~ucid_uci,data=chaidInput, FUN = function(x){
  length(unique(x))
})


#--------------- Filtering for those ucid uci having count of wbn greater than equal to 5


CountGreaterThanFive <- subset.data.frame(greaterThanFive,wbn>=5)

filteredUcid <- chaidInput[chaidInput$ucid_uci %in% greaterThanFive$ucid_uci,]







