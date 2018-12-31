

library(partykit)
library(CHAID)
library(lubridate)
library(dplyr)
library(reshape2)
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
filteredUcid <- chaidInput[(chaidInput$ucid_uci %in% CountGreaterThanFive$ucid_uci) & (chaidInput$ucid_uci!=''),]

# Filtering for delivery statuses
delvieredProducts <- subset.data.frame(filteredUcid,cs_ss=='Delivered')
aggreggatedDelivery <- aggregate(cs_ss~ucid_uci+Day,data=delvieredProducts,FUN = function(x){
  return(length(x))
})

# Converting into a wide format 

wideDataFrame <- dcast(aggreggatedDelivery,ucid_uci~Day, mean, value = 'cs_ss')


wideDataFrame <- wideDataFrame[,c("ucid_uci","Sunday",'Monday',"Tuesday","Wednesday","Thursday","Friday","Saturday")]
wideDataFrame$na_count <- apply(wideDataFrame, 1, function(x) sum(is.na(x)))

# Filtering for those for which delivery is done in 5 different days

filterdWideDataFrame <- subset.data.frame(wideDataFrame,na_count<=2)



#Addresses for which order is never delivered on SUnday

sundayAvoid <- subset.data.frame(filterdWideDataFrame,is.na(Sunday))
sundayAvoid$na_count <-NULL

#Gettign the count of wbn alos
sundayAvoid[is.na(sundayAvoid )]<-0
sundayAvoid$Total <- sundayAvoid$Sunday+ sundayAvoid$Monday+ sundayAvoid$Tuesday +sundayAvoid$Wednesday +sundayAvoid$Thursday+sundayAvoid$Friday +sundayAvoid$Saturday
#------------For checking
check1 <- subset.data.frame(filteredUcid, ucid_uci=='00087b74-b88f-f2b5-643a-80c064bc440c')


# Getting the count when there are Pending statuses on Sunday










