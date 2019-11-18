
#SQL Query (Sales Report Deets)
#select acct_id, acct_msisdn, acct_idn, dt_created
#from EDWDM.TEDW_SINGCASH_ACCOUNT
#where acct_stat='A'
#and (ACCT_TRST_LVL LIKE '%Startrek%'or acct_trst_lvl like '%Remit%')
rm(list=ls())

library("openxlsx")
library(readxl)
library(xlsx)
library(dplyr)
#date<- (Sys.Date()-1)
date<- "17-11-2019"
date<-as.Date(date,format = "%d-%m-%Y")


#Read Huipings signup email
signup <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Raw/Sign_up_Report.csv")
#Read SQL (sales Report Deets)
acct <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Raw/acct.csv")
acct$ACCT_MSISDN <- as.character(acct$ACCT_MSISDN)
#Read Madhu's file
cognos <- read_xlsx("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Raw/1065 - Dash Pay Registration Daily Report.xlsx", "Page1",skip = 5)
cognos<-cognos[-nrow(cognos),]
cognos<-cognos[!is.na(cognos$`Customer ID number`),]
#Read Spears file
spear<-read_xls("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Raw/RGN-0017a Dash Registration Daily Report.xls", "Sheet1",skip = 2)
spear$`MOBILE NUMBER` <- as.character(paste0(65, spear$`MOBILE NUMBER`))
colnames(spear)[4]<-"MSISDN"
spear<-spear[!is.na(spear$CUSTOMERID),]
#Read NFC
##nfc<-read_xls("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Raw/RMT-0092 Daily NFC sim card takeups.xls", "Sheet1",skip = 1)
##nfc<-nfc[!is.na(nfc$SERVICE_NO),]

#Read from Reporting Master List(List of all Dealers)
dealerMappingspear <- read_xlsx("X:/Group Digital Life/mCommerce/New Singtel Dash Reporting/sales ga dealers.xlsx", "Spear")
dealerMappingcognos <- read_xlsx("X:/Group Digital Life/mCommerce/New Singtel Dash Reporting/sales ga dealers.xlsx", "Cognos")

#signup------------------------------------------------------------------------
mappedsignup <- left_join(signup, acct, by = c("Account.ID" = "ACCT_ID"))
mappedsignup<-data.frame(mappedsignup$ACCT_MSISDN)
colnames(mappedsignup)<-"MSISDN"
mappedsignup$Channel<-NA
mappedsignup$Channel<-"Organic-One app"
mappedsignup$Date<-NA
mappedsignup$Date<-date
mappedsignup<-mappedsignup[!is.na(mappedsignup$MSISDN),]
mappedsignupcount<-data.frame(nrow(mappedsignup))
colnames(mappedsignupcount)<-"Count"
mappedsignupcount$Channel<- NA
mappedsignupcount$Channel<-"Sign ups through App"
mappedsignupcount<-mappedsignupcount[c(2,1)]

#cognos-------------------------------------------------------------------------
cognos<-data.frame(cognos[,4:6])
colnames(cognos)[1]<-"MSISDN"
colnames(cognos)[2]<-"Vendor_code"
colnames(cognos)[3]<-"Dealer_code"
#Extract SERS, there are when vendor code is V
sers<- cognos[grep("V", cognos$Vendor_code) ,]
colnames(sers)[2]<-"Channel"
sers$Channel<-"SERS"
sers$Date<-NA
sers$Date<-date
#Extract STS, there are when vendor code is T
sts<- cognos[grep("T", cognos$Vendor_code) ,]
colnames(sts)[2]<-"Channel"
sts$Channel<-"STS"
sts$Date<-NA
sts$Date<-date
cognosdealer<-data.frame(cognos$Dealer_code)
colnames(cognosdealer)<-"Dealer"
cognosdealer$Dealer <- as.character(cognosdealer$Dealer)

#Spears---------------------------------------------------------------------------

mappedspears <- left_join(spear, acct, by = c("MSISDN" = "ACCT_MSISDN"))
mappedspears<-mappedspears[!is.na(mappedspears$DT_CREATED),]
mappedspears2<-select(mappedspears,MSISDN,VENDORCODE)
#Spear sts
stsspear<- mappedspears2[grep("T", mappedspears2$VENDORCODE) ,]
stsspear$Channel<- NA
colnames(stsspear)[3]<-"Channel"
stsspear$Channel<-"STS"
stsspear$Date<-NA
stsspear$Date<-date
stsspearcount<-data.frame(nrow(stsspear))
colnames(stsspearcount)<-"Count"
stsspearcount$Channel<- NA
stsspearcount$Channel<-"Singtel Shops"
stsspearcount<-stsspearcount[c(2,1)]
speardealer<- data.frame(mappedspears$DEALERCODE)
colnames(speardealer)<-"Dealer"
#Spear sers
#sersspear<- mappedspears2[grep("V", mappedspears2$VENDORCODE) ,]
#sersspear$Channel<- NA
#colnames(sersspear)[3]<-"Channel"
#sersspear$Channel<-"STS"
#sersspear$Date<-NA
#sersspear$Date<-date

#Mapping Dealers
mappeddealerspear <- left_join(speardealer, dealerMappingspear, by = c("Dealer" = "Dealer code"))
mappeddealerspear <- summarize(group_by(mappeddealerspear,Location), Count = n())

mappeddealercognos_SERS <-left_join(cognosdealer, dealerMappingcognos, by = c("Dealer" = "Dealer Code"))
mappeddealercognos_SERS<- summarize(group_by(mappeddealercognos_SERS,Dealer,Location), Count = n())

mappeddealercognos_Spear <-left_join(cognosdealer, dealerMappingspear, by = c("Dealer" = "Dealer code"))
mappeddealercognos_Spear<- summarize(group_by(mappeddealercognos_Spear,Dealer,Location), Count = n())
#
acct$DT_CREATED<-as.Date(acct$DT_CREATED,format = "%d/%m/%y")
othersources<-filter(acct, DT_CREATED == date)
othersources<-data.frame(nrow(othersources))
colnames(othersources)<-"Count"
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#Combining tables----------------------------------------------------------------
#if serscognos,stscognos(sts2) and stsspears data available 
#sers2<-select(sers, MSISDN, Channel, Date)
sts2<-select(sts, MSISDN, Channel, Date)
#stsspear2<-select(stsspear,MSISDN,Channel,Date)
#Channelwisebreakdown<-rbind(mappedsignup,stsspear2)
Channelwisebreakdown<-rbind(mappedsignup,sts2)
Channelwisebreakdown$Date <- as.Date(Channelwisebreakdown$Date, format = "%Y-%m-%d")
#colnames(Channelwisebreakdown)[3] <-"Date"


acct$DT_CREATED<-as.Date(acct$DT_CREATED,format = "%d/%m/%y")
othersources<-filter(acct, DT_CREATED == date)
othersources<-data.frame(nrow(othersources))
colnames(othersources)<-"Count"

#-------------------------------------------------------------------------------
Channelwisebreakdown_Base <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Raw/ChannelwiseBreakdown.csv")
Channelwisebreakdown_Base$Date <- as.Date(Channelwisebreakdown_Base$Date, format = "%Y-%m-%d")
#Channelwisebreakdown_Base$Date <- as.Date(Channelwisebreakdown_Base$Date, format = "%d/%m/%Y")
Channelwisebreakdown_Base$X <- NULL
FinalChannelwiseBreakdown <- rbind(Channelwisebreakdown_Base,Channelwisebreakdown)

#write.csv(FinalChannelwiseBreakdown,"C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Raw/ChannelwiseBreakdown.csv")

#-------------------------------------------------------------------------------
# Sales GA Daily Tracking Numbers 

#readfiles
signups <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Daily Sales data/Signup.csv")
signups$X <-NULL
singtelshop <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Daily Sales data/Singtelshop.csv")
singtelshop$X <-NULL
SERS <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Daily Sales data/SERS.csv")
SERS$X <-NULL
Othersources <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Daily Sales data/Othersources.csv")
Othersources$X <-NULL
NFC <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Daily Sales data/NFC.csv")
NFC$X <-NULL

#signups
mappedsignupcount_PBI <-mappedsignupcount
mappedsignupcount_PBI$Channel <- date
colnames(mappedsignupcount_PBI)[1] <- "Date"
colnames(mappedsignupcount_PBI)[2] <- "Signups"
signups$Date <- as.Date(signups$Date, format = "%d/%m/%Y" )
#signups$Date <- as.Date(signups$Date, format = "%Y-%m-%d" )
signups<- rbind(signups,mappedsignupcount_PBI)
signups<- signups[!is.na(signups$Date),]

#singtelshop
cognosstsmsisdn <- data.frame(sts[,c(1)])
colnames(cognosstsmsisdn)[1] <- "MSISDN"
spearstsmsisdn <- data.frame(spear[,c(4)])
colnames(spearstsmsisdn)[1] <- "MSISDN"
spear_cognos_sts <- rbind(cognosstsmsisdn,spearstsmsisdn)
STS_count<-data.frame(nrow(spear_cognos_sts))
colnames(STS_count)[1]<-"Signups"
STS_count$Date <- NULL
STS_count$Date <- date
STS_count <-STS_count[,c(2,1)]
STS_count$Date <- as.Date(STS_count$Date)
singtelshop$Date <- as.Date(singtelshop$Date, format = "%d/%m/%Y")
#singtelshop$Date <- as.Date(singtelshop$Date, format = "%Y-%m-%d")
singtelshop<- rbind(singtelshop,STS_count)
singtelshop<- singtelshop[!is.na(singtelshop$Date),]

#SERS
sers_count<-data.frame(nrow(sers))
colnames(sers_count)[1]<-"Signups"
sers_count$Date <- NULL
sers_count$Date <- date
sers_count <-sers_count[,c(2,1)]
sers_count$Date <- as.Date(sers_count$Date)
SERS$Date <- as.Date(SERS$Date, format = "%d/%m/%Y")
#SERS$Date <- as.Date(SERS$Date, format = "%Y-%m-%d")
SERS<- rbind(SERS,sers_count)
SERS<- SERS[!is.na(SERS$Date),]


#OthersSources
ChannelSignups <- rbind(mappedsignupcount_PBI,STS_count,sers_count)
TotalChannelSignup <- as.data.frame(sum(ChannelSignups$Signups))
colnames(TotalChannelSignup)[1] <- "Signups"
OthersourcesCount <-data.frame(othersources$Count - TotalChannelSignup$Signups)
colnames(OthersourcesCount)[1] <- "Signups"
OthersourcesCount$Date <- NULL
OthersourcesCount$Date <- date
Othersources$Date <- as.Date(Othersources$Date,format = "%d/%m/%Y")
#Othersources$Date <- as.Date(Othersources$Date,format = "%Y-%m-%d")
OthersourcesCount$Date <- as.Date(OthersourcesCount$Date,format = "%d/%m/%Y")
Othersources$Date <- as.Date(Othersources$Date)
OthersourcesCount <-OthersourcesCount[,c(2,1)]
Othersources<- rbind(Othersources,OthersourcesCount)
Othersources<- Othersources[!is.na(Othersources$Date),]

#Remove uncessary tables
#rm("Channelwisebreakdown_Base", "Channelwisebreakdown", "acct","cognos","cognosdealer","dealerMappingcognos","dealerMappingspear","mappedsignup","mappedspears","mappedspears2","sers","sers2","signup","spear","speardealer","sts","stsspear","stsspear2")
rm("acct","ChannelSignups","cognos","Channelwisebreakdown_Base","mappedsignup","mappedsignupcount","NFC","othersources","cognosdealer","cognosstsmsisdn","mappedspears","mappedspears2","sers","spear","spear_cognos_sts","speardealer","spearstsmsisdn","stsspear","stsspearcount","signup","Channelwisebreakdown","sts","dealerMappingcognos","dealerMappingspear")


write.csv(signups,"C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Daily Sales data/Signup.csv")
write.csv(singtelshop,"C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Daily Sales data/Singtelshop.csv")
write.csv(SERS,"C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Daily Sales data/SERS.csv")
write.csv(Othersources,"C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Daily Sales data/Othersources.csv")
#write.csv(FinalChannelwiseBreakdown,"C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Raw/ChannelwiseBreakdown.csv")


#write.csv(NFC,"C:/Users/CP666383/Desktop/Power BI Reports/Sales GA/Daily Sales data/NFC.csv")
