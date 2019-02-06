#-----------------------------------------Sunday Delivery Prediction-------------------------------------#


#--------------------------------------------------------------------------------------------------------#


#-------**************************************************************************************************#



#*-----------------------------------Reading Input File----------------------------------------------------#


chaidInput= read.csv(file='D:/Delhivery/Projects/Sunday Prediction/20181221/CompleteData.csv', na.strings = "NA", stringsAsFactors = F)

#----------------------------Filtering for Null Ucids---------------------------------------------------#

chaidInput <- subset.data.frame(chaidInput, ucid_uci!='')
#-----------------------------Getting the Week day from the weekday --------------------#
chaidInput$weekday <- as.POSIXlt(chaidInput$cs_sd)$wday

#------------------------------Creating the Sunday Flag------------------------------------------#
chaidInput$Sunday_Flag  <- ifelse(chaidInput$weekday %in% c(1,2,3,4,5,6),"ROD","Sunday")

#----------------------Filtering the dataset for the required columns----------------------------------#
filteredAddress <- chaidInput[,c("ucid_uci","wbn","cs_sd","cs_ss","Sunday_Flag")]

filteredAddress <- arrange(filteredAddress,ucid_uci,wbn,cs_sd)


#-------------------Filtering for wbns which have more than one statuses---------------------------------#

morethanOne <- aggregate(cs_ss~ucid_uci+wbn, data=filteredAddress, FUN =length)
# Getting the list of Phone number and addresses having more than one statuses
multipleStatus <- morethanOne[which(morethanOne$cs_ss >1),c("ucid_uci","wbn")]


#--------------------------------------------------------------------------------------------------#

#------------Joining wbn and statuses for wbns for which there are more than one entries----------------#

finalDataset <- merge(x=filteredAddress, y=multipleStatus, by=c("ucid_uci","wbn"),all=F)


#----------------------------Geting those shipments which were once delivered-------------------------# 

deliveredShipments <- unique(finalDataset[which(finalDataset$cs_ss=='Delivered'),"wbn"])
deliveredShipments_Data <- finalDataset[finalDataset$wbn %in% deliveredShipments,]

#------------------------------ Creating the Sunday Success Flag---------------------------------------#

deliveredShipments_Data$Sunday_Success  <- ifelse(deliveredShipments_Data$cs_ss=='Delivered' &  deliveredShipments_Data$Sunday_Flag=='Sunday',1,0)


#------------------------Creating the dataframe for getting the Sunday or ROD probability-------------------------------#

deliveredShipments_new<- subset.data.frame(deliveredShipments_Data,Sunday_Flag=="Sunday")

deliveredShipments_final <- deliveredShipments_Data[deliveredShipments_Data$wbn %in% deliveredShipments_new$wbn,]

deliveredShipments_final <- arrange(deliveredShipments_final, ucid_uci,wbn,cs_sd)

phone_Delivered <- aggregate(wbn~ucid_uci, data=deliveredShipments_final, FUN = function(x){
  return(length(unique(x)))
})

#--------------------------------------Computing the Probabilities-----------------------------------#

#**************Sunday Probability -Number of Sunday Successes/Total Number of Wbns
#**************ROD Probability    - 1- Sunday Probability

phone_Sunday_delivered <- aggregate(Sunday_Success~ucid_uci, data=deliveredShipments_final, FUN =sum, na.rm=T)
phone_Delivered_final <- merge(x=phone_Delivered, y=phone_Sunday_delivered, by="ucid_uci",all.x=T)
phone_Delivered_final$Sunday_probability <- phone_Delivered_final$Sunday_Success/phone_Delivered_final$wbn
phone_Delivered_final$ROD_prob <- 1- phone_Delivered_final$Sunday_probability



#-------------------------------------------------------------------------------------------------------#


#**********************Preparing data for computing the Sunday Preferred Customers***********************


#--------------------------------------------------------------------------------------------------------#


#---------Methodology 1  - customers having WBN count greater than 2 and Sunday Probability as zero

phoneDeliveredFinal <- as.data.frame(phoneDeliveredFinal)
sundayAvoidCustomers<- subset.data.frame(phoneDeliveredFinal, wbn>2 &  ROD_prob==1)
sundayAvoidCount <- length(unique(finalDeliverdFinal$ucid_uci))
print(sundayAvoidCount)

#---------Methodology 2  - customers having WBN count greater than 2 and Sunday Probability as 1 

sundayPreferredCustomers<- subset.data.frame(phoneDeliveredFinal, wbn>2 &  ROD_prob==0)
sundayPreferredCount <- length(length(unique(finalDeliverdFinal$ucid_uci)))
print(sundayPreferredCount)


#***********************------Preparing data for the Methodology Number 3 ****************************#

#---------------------------------------Getting the day of the Week-----------------------------------#
chaidInput$Day <- ifelse(chaidInput$weekday==0,'Sunday',(ifelse(chaidInput$weekday==1,'Monday',(ifelse(chaidInput$weekday==2,'Tuesday',(ifelse(chaidInput$weekday==3,'Wednesday',(ifelse(chaidInput$weekday==4,'Thursday',(ifelse(chaidInput$weekday==5,'Friday', (ifelse(chaidInput$weekday==6,'Saturday','Null')))))))))))))

#-------------------------------Filtering for ucidhaving count of wbn >=5-----------------------------#
greaterThanFive <- aggregate(wbn~ucid_uci,data=chaidInput, FUN = function(x){
  length(unique(x))
})

CountGreaterThanFive <- subset.data.frame(greaterThanFive,wbn>=5)
filteredUcid <- chaidInput[(chaidInput$ucid_uci %in% CountGreaterThanFive$ucid_uci) & (chaidInput$ucid_uci!=''),]

#----------------------------------Filtering for delivered Statuses----------------------------------#

delvieredProducts <- subset.data.frame(filteredUcid,cs_ss=='Delivered')
aggreggatedDelivery <- aggregate(cs_ss~ucid_uci+Day,data=delvieredProducts,FUN = function(x){
  return(length(x))
})

#------------------------------------Converting data into wide format--------------------------------#

wideDataFrame <- dcast(aggreggatedDelivery,ucid_uci~Day, mean, value = 'cs_ss')
wideDataFrame <- wideDataFrame[,c("ucid_uci","Sunday",'Monday',"Tuesday","Wednesday","Thursday","Friday","Saturday")]
wideDataFrame$na_count <- apply(wideDataFrame, 1, function(x) sum(is.na(x)))

#----------------Filtering UCIDs for those for which delivery is done in 5 different days--------------#

filterdWideDataFrame <- subset.data.frame(wideDataFrame,na_count<=2)

#--------------------------Addresses for which order is never delivered on SUnday-----------------------#
sundayAvoid <- subset.data.frame(filterdWideDataFrame,is.na(Sunday))
sundayAvoid$na_count <-NULL


#----------------------------------Gettign the count of wbns for each UCID-------------------------------#
sundayAvoid[is.na(sundayAvoid )]<-0
sundayAvoid$Total <- sundayAvoid$Sunday+ sundayAvoid$Monday+ sundayAvoid$Tuesday +sundayAvoid$Wednesday +sundayAvoid$Thursday+sundayAvoid$Friday +sundayAvoid$Saturday

# Getting the count when there are Pending statuses on Sunday
sundayPending <- subset.data.frame(chaidInput,Day=='Sunday' & cs_ss=='Pending')

#------------Getting the unique list of UCID's for which at least one of the day is Sunday--------------#
sundayPendingUcid <- as.data.frame(unique(sundayPending$ucid_uci))
colnames(sundayPendingUcid)[1] <- "ucid_uci"

#-------------Getting the final list of UCID for whom Delivery on Sunday should not be done--------------# 
SundayAvoidDataset <- merge(x= sundayAvoid,y=sundayPendingUcid
                            , by='ucid_uci',all=F)

