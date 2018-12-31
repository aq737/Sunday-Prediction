

library(partykit)
library(CHAID)
library(lubridate)
library(dplyr)
# Reading the input File

chaidInput= read.csv(file='D:/Delhivery/Codes/Sunday Prediction/20181221/CompleteData.csv', na.strings = "NA", stringsAsFactors = F)
chaidInput$weekday <- as.POSIXlt(chaidInput$cs_sd)$wday
