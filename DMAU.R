library(readxl)
library(dplyr)
library(stringr)

rm(list=ls())
Date<- factor("17-11-2019")
#Date <- (Sys.Date()-1)
Date<- as.Date(Date,format = "%d-%m-%Y")


# Read csv file from folder (Email from huiping-dash logins)
Login <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/Dash Daily Login.csv", skip = 6)
# Read csv file for SQL query-(SalesReportDeets)
#Dashacct <- read.csv ("C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/dashacct.csv")
#Read Transacted Users
Transactedusers <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/Transact.csv")

# Read MTD user list
MTDusers <- read.csv(file="C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/MTDusers.csv")
MTDusers<- MTDusers[,-c(1)]

# Mapping of Login Id to MSISDN
#Login users
#Login<- left_join(Dashlogins, Dashacct, by = c("MCASH_ACCOUNT_ID" = "ACCT_ID"))
#Filter out MSISDN which are NA
#Login <- filter(Login, !is.na(ACCT_MSISDN))
#Login<- select(Login,ACCT_MSISDN)
#colnames(Login)[1]<-"MSISDN"

#Transact Users
#---------------------------------------------------------------------------------
#Only on the first Day of the month
#transactonly<-Transactedusers

#---------------------------------------------------------------------------------

#Merge mtd with transact
Transactedusers$Type<-NULL
Transactedusers$Type<-"Transact"
MTDusers2 <- left_join(MTDusers, Transactedusers,by="ACCT_ID")
colnames(MTDusers2)[3] <- "MTD Type"
colnames(MTDusers2)[4] <- "Transact Type"

#Date Formatting 
#MTDusers2$Date <-as.Date(MTDusers2$Date,format = "%d/%m/%Y")
MTDusers2$Date <-as.Date(MTDusers2$Date,format = "%Y-%m-%d")


MTDusers2$Date[MTDusers2$`MTD Type`== "Login" & MTDusers2$ `Transact Type`== "Transact"] <- Date
MTDusers2$`MTD Type`[MTDusers2$`MTD Type`== "Login" & MTDusers2$ `Transact Type`== "Transact"] <- "Transact"
MTDusers2$`Transact Type` = NULL
colnames(MTDusers2)[3] <- "Type"

#Binding new new transact to mtd
transactonly<-data.frame(setdiff(Transactedusers$ACCT_ID,MTDusers$ACCT_ID))
colnames(transactonly)[1] <- "ACCT_ID"
transactonly$Date<- Date
transactonly$Type<- NULL
transactonly$Type<- "Transact"
MTDusers2 <- rbind(MTDusers2, transactonly)


Loginonly<-data.frame(setdiff(Login$MCASH_ACCOUNT_ID,MTDusers2$ACCT_ID))
colnames(Loginonly)[1] <- "ACCT_ID"
Loginonly$Date<- NA
Loginonly$Date<- Date
Loginonly$Type<- NULL
Loginonly$Type<- "Login"
MTDusers2 <- rbind(MTDusers2, Loginonly)

#------------------------------------------------------------------
MTDusers2 <-MTDusers2[complete.cases(MTDusers2), ]

#------------------------------------------------------------------

#MTDusersPivot <- summarize(group_by(MTDusers2, Type), Count = n())

rm(Login,Loginonly,MTDusers,Transactedusers,transactonly)

#Write to csv 
write.csv(MTDusers2, file="C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/MTDusers.csv")

