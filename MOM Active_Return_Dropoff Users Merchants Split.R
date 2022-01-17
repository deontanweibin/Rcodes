rm(list=ls())
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(RJDBC)
library(plyr)
options(java.parameters = "-Xmx8048m")

Dropoff_Month <- "Sep-2019"

# set up Oracle driver for live connection with Oracle SQL developer (please adjust filepath below to your desktop's)
#Deon Driver
jdbcDriver <- JDBC("oracle.jdbc.OracleDriver", classPath = "C:\\Users\\CP666383\\Downloads\\ojdbc6.jar")

# pull out all tables available as sense-check
conn <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//dw6prd-scan.app.vic:1831/EDWPRD", "dshdm", "ds1dm_99")
dbListTables(conn)

# Read All Transactions Files-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p2m <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/p2m/dailyP2Mvisualisation.csv", stringsAsFactors = FALSE)
vpconline <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/vpc/vpconlinevisualisation.csv", stringsAsFactors = FALSE)
vpcpaywave <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/vpc/vpcpaywavevisualisation.csv", stringsAsFactors = FALSE)

# Combine Transactions Files for Channel Names-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p2m <- select(p2m,TXN_ID,Channel)
vpconline <- select(vpconline,TXN_ID,Merchant)
colnames(vpconline)[2] <- "Channel"
vpcpaywave <- select(vpcpaywave,TXN_ID,Merchant)
colnames(vpcpaywave)[2] <- "Channel"
alltxn <- rbind(p2m,vpconline,vpcpaywave)
alltxn$Channel <-as.character(alltxn$Channel)

test<-ddply(alltxn,~Channel,summarise,distinctcount=length(unique(TXN_ID)))




# Reading Previous & Current Month Active Users ***(MUST EDIT FILE PATH FOR NEW MONTH)***------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
CurMth <-read.csv("C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/MTDUsers History/Oct19MTDUsers.csv")
CurMth <- CurMth[complete.cases(CurMth),]
PreMth <-read.csv("C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/MTDUsers History/Sep19MTDUsers.csv")
PreMth <- PreMth[complete.cases(PreMth),]
PreMth$X <- NULL
CurMth$X <- NULL
#CurMth <- CurMth[c(2,1,3)]


# Remove Comment for below 2 lines if you need to compare Transact MAU ***(DOING SO WILL EXCLUDE LOGIN USERS AS AN ACTIVE USER)***
#PreMth <- PreMth[PreMth[, "Type"] == "Transact",]
#CurMth <- CurMth[CurMth[, "Type"] == "Transact",]

PreMth <- select(PreMth,ACCT_ID,Date)
PreMth$NewUsers <- 'N'
#MTDUsersCurrentMth <-read.csv("C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/MTDUsers.csv")
CurMth <- select(CurMth,ACCT_ID,Date)
CurMth$Dropoff <- 'N'
#CurMth<- CurMth[-c(1), ]
CurMth$ACCT_ID <- as.character(CurMth$ACCT_ID)
PreMth$ACCT_ID <- as.character(PreMth$ACCT_ID)
colnames(CurMth)[2] <- "Current_Dt"
colnames(PreMth)[2] <- "Previous_Dt"
#Cleaning of duplicates--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#CurMth <- data.frame(CurMth[!duplicated(CurMth$ACCT_ID),])
#PreMth <- data.frame(PreMth[!duplicated(PreMth$ACCT_ID),])

#Dropoff Users---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Dropoffs <- left_join(PreMth,CurMth, by = "ACCT_ID")
Dropoffs$Dropoff[is.na(Dropoffs$Dropoff)] <- 'Y'
Dropoffs[3] <- NULL
colnames(Dropoffs)[2] <- 'Previous Date'
colnames(Dropoffs)[3] <- 'Current Date'
Dropofflist <- Dropoffs[Dropoffs[, "Dropoff"] == 'Y',]
Dropofflist_v2 <- select(Dropofflist,ACCT_ID)

dropoff_txn_id <- dbGetQuery(conn, paste("SELECT a.scr_acct_id as ACCT_ID, Month, svc_class_name, a.txn_id
                                          from(
                                          select scr_acct_id, to_char(txn_dt, 'Mon-YYYY') as Month ,max(txn_id) as txn_id
                                          FROM EDWDM.TEDW_SINGCASH_SALESJOURNAL_DOM
                                          Where (SVC_CLASS_NAME IN('P2M','P2M Pinless', 'P2P', 'SingTel Prepaid Top Up','Cashout2CASA',
                                          'CASA Cashout DBS','ISO8583 Visa Paywave','ISO8583 Visa Online') 
                                          OR brnd_grp = 'International Money Remittance'
                                          OR brnd_id in ('311','312','313', '314','315','316','112','111'))
                                          AND ACCT_MSISDN != '6583381170' 
                                          AND ACCT_MSISDN != '6597727199'
                                          AND ACCT_MSISDN != '6596510899'
                                          AND ACCT_MSISDN > '6580000000'
                                          AND TXN_DT between to_date('01-Sep-2019','DD-Mon-YY') AND to_date('01-Oct-2019','DD-Mon-YY')
                                          AND ORIG_AMT > 0.09
                                          AND acct_msisdn > '6580000000'
                                          group by scr_acct_id, to_char(txn_dt, 'Mon-YYYY')
                                          ) a
                                          inner join EDWDM.TEDW_SINGCASH_SALESJOURNAL_DOM b
                                          on a.txn_id = b. txn_id
                                          "))

dropoff_txn_id$ACCT_ID <- as.character(dropoff_txn_id$ACCT_ID)

Dropofflist_v3 <- left_join(Dropofflist_v2,dropoff_txn_id, by =c("ACCT_ID"))

#Dropofflist_v4 <- Dropofflist_v3[complete.cases(Dropofflist_v3),]

# Left Join to get Merchant names
Dropofflist_v5 <- left_join(Dropofflist_v3,alltxn, by = c("TXN_ID" = "TXN_ID"))

#Renaming last_txn Channels-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Dropofflist_v5$Channel[Dropofflist_v5$SVC_CLASS_NAME == "P2P"] <- "P2P"
Dropofflist_v5$Channel[Dropofflist_v5$SVC_CLASS_NAME %like% "Tranglo"] <- "Remittance"
Dropofflist_v5$Channel[Dropofflist_v5$SVC_CLASS_NAME %like% "TransferTo"] <- "Remittance"
Dropofflist_v5$Channel[Dropofflist_v5$SVC_CLASS_NAME %like% "GCash"] <- "Remittance"
Dropofflist_v5$Channel[Dropofflist_v5$SVC_CLASS_NAME %like% "Telkomsel"] <- "Remittance"
Dropofflist_v5$Channel[Dropofflist_v5$SVC_CLASS_NAME == "SingTel Prepaid Top Up"] <- "Prepaid Top Up"
Dropofflist_v5$Channel[Dropofflist_v5$SVC_CLASS_NAME %like% "Cashout"] <- "Cashout"


Dropofflist_v5$Channel[is.na(Dropofflist_v5$Channel)] <- "Login"
Dropofflist_v5$MONTH[is.na(Dropofflist_v5$MONTH)] <- Dropoff_Month
Dropofflist_v5$SVC_CLASS_NAME[is.na(Dropofflist_v5$SVC_CLASS_NAME)] <- "Login"
Dropofflist_v5$TXN_ID[is.na(Dropofflist_v5$TXN_ID)] <- 0



# Clean list, including 16th Sep System double charge issue
Dropofflist_v6 <- Dropofflist_v5[complete.cases(Dropofflist_v5),]
Dropofflist_v6$Type <- "Dropoff"
Dropoff_User <- Dropofflist_v6
#Dropofflist_v6$MONTH <- "Aug-2019"

#rbind


#dropoffbase <-read.csv("X:/Group Digital Life/mCommerce/New Singtel Dash Reporting/Deon/MAU Analysis/MAU_MOM_Transact_Comparison/Category test/dropoff.csv")
#dropoffbase$X <- NULL
#dropoffbase_final <-rbind(dropoffbase,Dropofflist_v6)  

#Write testing
#write.csv(dropoffbase_final,file= "X:/Group Digital Life/mCommerce/New Singtel Dash Reporting/Deon/MAU Analysis/MAU_MOM_Transact_Comparison/Category test/dropoff.csv")


#

#NewUsers Cleaning------------------------------------------------------------------------------------------------------------------------------------------------------------------
NewUsers <- left_join(CurMth,PreMth, by = "ACCT_ID")
NewUsers[3] <- NULL
colnames(NewUsers)[2] <- 'Current Date'
colnames(NewUsers)[3] <- 'Previous Date'
NewUsers$NewUsers[is.na(NewUsers$NewUsers)] <- 'Y'
#NewUsers$`Current Date`[ NewUsers$`Current Date` == "Y" ] <- NA
#NewUsers$`Current Date`[is.na(NewUsers$`Current Date`)] <- ""
#NewUsers$`Current Date`<-as.Date(NewUsers$`Current Date`,format = "%Y-%m-%d")

#Create Separate table in order to allow counts of recurring users by substraction
NewUsers_v2<-NewUsers[NewUsers[,"NewUsers"]=="Y",]


#Acct of users
acct <- dbGetQuery(conn, paste("select acct_id, dt_created 
                               from EDWDM.TEDW_SINGCASH_ACCOUNT 
                               where acct_trst_lvl is not NULL
                               AND DT_CREATED between to_date('01-Oct-2019','DD-Mon-YY') AND to_date('01-Nov-2019','DD-Mon-YY')
                               AND layr_name LIKE '%Customers%'
                               "))

acct$ACCT_ID <- as.character(acct$ACCT_ID)


#Gather New Users, Return New & New New
NewUsers_v2 <- left_join(NewUsers_v2,acct, by = c("ACCT_ID"))
NewUsers_v2$`Previous Date` <- NULL

#NewReturnUsers <- as.data.frame(is.na(NewUsers_v2))


first_txn_id <- dbGetQuery(conn, paste("SELECT a.scr_acct_id as ACCT_ID, Month, orig_amt, svc_class_name, a.txn_id
                                        from(
                                        select scr_acct_id, to_char(txn_dt, 'Mon-YYYY') as Month ,min(txn_id) as txn_id
                                        FROM EDWDM.TEDW_SINGCASH_SALESJOURNAL_DOM
                                        Where (SVC_CLASS_NAME IN('P2M','P2M Pinless', 'P2P', 'SingTel Prepaid Top Up','Cashout2CASA',
                                        'CASA Cashout DBS','ISO8583 Visa Paywave','ISO8583 Visa Online') 
                                        OR brnd_grp = 'International Money Remittance'
                                        OR brnd_id in ('311','312','313', '314','315','316','112','111'))
                                        AND ACCT_MSISDN != '6583381170' 
                                        AND ACCT_MSISDN != '6597727199'
                                        AND ACCT_MSISDN != '6596510899'
                                        AND ACCT_MSISDN > '6580000000'
                                        AND TXN_DT between to_date('01-Oct-2019','DD-Mon-YY') AND to_date('01-Nov-2019','DD-Mon-YY')
                                        AND ORIG_AMT > 0.09
                                        AND acct_msisdn > '6580000000'
                                        group by scr_acct_id, to_char(txn_dt, 'Mon-YYYY')
                                        ) a
                                        inner join EDWDM.TEDW_SINGCASH_SALESJOURNAL_DOM b
                                        on a.txn_id = b. txn_id
                                        "))

first_txn_id$ACCT_ID <- as.character(first_txn_id$ACCT_ID)

NewUsers_v3 <- left_join(NewUsers_v2,first_txn_id, by =c("ACCT_ID"))
NewUsers_v3_txnonly <- NewUsers_v3[!is.na(NewUsers_v3$TXN_ID), ]

# Left Join to get Merchant names
NewUsers_v4 <- left_join(NewUsers_v3_txnonly,alltxn, by = c("TXN_ID" = "TXN_ID"))

#Renaming last_txn Channels-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
NewUsers_v4$Channel[NewUsers_v4$SVC_CLASS_NAME == "P2P"] <- "P2P"
NewUsers_v4$Channel[NewUsers_v4$SVC_CLASS_NAME %like% "Tranglo"] <- "Remittance"
NewUsers_v4$Channel[NewUsers_v4$SVC_CLASS_NAME %like% "TransferTo"] <- "Remittance"
NewUsers_v4$Channel[NewUsers_v4$SVC_CLASS_NAME %like% "GCash"] <- "Remittance"
NewUsers_v4$Channel[NewUsers_v4$SVC_CLASS_NAME %like% "Telkomsel"] <- "Remittance"
NewUsers_v4$Channel[NewUsers_v4$SVC_CLASS_NAME == "SingTel Prepaid Top Up"] <- "Prepaid Top Up"
NewUsers_v4$Channel[NewUsers_v4$SVC_CLASS_NAME %like% "Cashout"] <- "Cashout"

# Clean list, including 16th Sep System double charge issue
#NewUsers_v6 <- NewUsers_v5[complete.cases(NewUsers_v5),]


NewCreatedUsers <- NewUsers_v4[complete.cases(NewUsers_v4),] 
ReturnNewUsers <- anti_join(NewUsers_v4,acct, by = c("ACCT_ID"))
NewCreatedUsers$Type <- "New_Created"
ReturnNewUsers$Type <- "Return_New"

NewCreatedUsers <- select(NewCreatedUsers,ACCT_ID,MONTH,SVC_CLASS_NAME,TXN_ID,Channel,Type)
ReturnNewUsers <- select(ReturnNewUsers,ACCT_ID,MONTH,SVC_CLASS_NAME,TXN_ID,Channel,Type)


base <- rbind(Dropoff_User,NewCreatedUsers,ReturnNewUsers)

Users_Breakdown <- read.csv("X:/Group Digital Life/mCommerce/New Singtel Dash Reporting/Deon/MAU Analysis/MAU_MOM_Transact_Comparison/Users_Breakdown.csv", stringsAsFactors = FALSE)
Users_Breakdown$X <- NULL
Users_Breakdown_final <-rbind(Users_Breakdown,base)


write.csv(Users_Breakdown_final,file= "X:/Group Digital Life/mCommerce/New Singtel Dash Reporting/Deon/MAU Analysis/MAU_MOM_Transact_Comparison/Users_Breakdown.csv")
#write.csv(base,file= "X:/Group Digital Life/mCommerce/New Singtel Dash Reporting/Deon/MAU Analysis/MAU_MOM_Transact_Comparison/Users_Breakdown.csv")

#ggplot------------------------------------------------------------------------------------------------------------------------------------------------------------
#UserCountTrackingReport <- UserCountTrackingReport %>% 
#  mutate(Date2=dmy(Date))
#UserCountTrackingReport %>%
#  gather('Category','Value',-c(Date,MAU,Date2)) %>%
#  ggplot(aes(x=Date2, y=Value,colour = Category)) + 
#  geom_line()


