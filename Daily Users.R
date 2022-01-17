#Input date(Have to do day by day)
rm(list=ls())
Date<- "17/11/2019"
Date<- data.frame(Date)
#Date$Date<-as.Date(Date$Date,format = "%d/%m/%Y")
options(stringsAsFactors = FALSE)

# Dependencies
library(readxl)
library(dplyr)
library(stringr)

#Slide 2

# Read email csv file from folder( From Huiping-FW: [OneApp Server PRD-BATCH01] Singtel DashApp User Login Status Daily Report for XXXX)
Login <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/Dash Daily Login.csv", skip = 6)
Login<- select(Login,MCASH_ACCOUNT_ID)
colnames(Login)[1] <- "ACCT_ID"
Login$ACCT_ID <- as.character(Login$ACCT_ID)
# Read csv file for SQL query(SalesReportDeets)
#Dashacct <- read.csv ("C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode/dashacct.csv")
#Read Transacted Users SQL query (All usage extraction (even cashin cashout))
Transactedusers <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/Transact.csv")
#Read Daily Users base data
DailyUsersBase <- read.csv("C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/Daily Users.csv")
DailyUsersBase$X<- NULL

#To find login users who have not come from previous days of the month
#Joining the 2 tables together(Dash Login for the day & Dash user pool)
#Login<- left_join(Dashlogins, Dashacct, by = c("MCASH_ACCOUNT_ID" = "ACCT_ID"))
#Filter out MSISDN which are NA
#Login <- filter(Login, !is.na(ACCT_MSISDN))
#Login<- select(Login,ACCT_MSISDN)
#colnames(Login)[1]<-"MSISDN"


#Read transacted users
#Transactedusers2 <- select(Transactedusers, ACCT_ID)
#Transactedusers2<- sapply(Transactedusers,as.character)
#Transactedusers3<-data.frame(Transactedusers2)
#colnames(Transactedusers3)[1]<-"MSISDN"

Transactedusers$ACCT_ID <- as.character(Transactedusers$ACCT_ID)
loginonly <- anti_join(Login, Transactedusers, by = c("ACCT_ID"))


#Finding Login Only (Excluding duplicates of login in transacted)
#loginonly<-setdiff(Login,Transactedusers3)
length(Login$ACCT_ID[!(which(Login$ACCT_ID %in% Transactedusers$ACCT_ID))])
length(Login$ACCT_ID)
length(Transactedusers$ACCT_ID[which(Transactedusers$ACCT_ID %in% Login$ACCT_ID)])

#loginonly<- data.frame(Login$MSISDN[!(Login$MSISDN %in% Transactedusers3$MSISDN)])
loginonlycount<- data.frame(nrow(loginonly))

#Combining login and transacted users
Currentdayusers<-rbind(loginonly,Transactedusers)
colnames(Currentdayusers)[1]<- "Date"
Currentdayusers <- unique(Currentdayusers)


TotalDayUsers<- nrow(Currentdayusers)

Transactedonlyusers<- nrow(Transactedusers)
Transactedonlyusers<-data.frame(Transactedonlyusers)
Transactedonlyusers$Date<-NA
Transactedonlyusers<-Transactedonlyusers[c(2,1)]
colnames(Transactedonlyusers)[2]<-"Users"
Transactedonlyusers$Type<-"Transact"
Transactedonlyusers<-Transactedonlyusers[c(1,3,2)]
Transactedonlyusers$Date<- Date$Date
Transactedonlyusers$Type<- NULL
colnames(Transactedonlyusers)[2] <- "Transact"

Transactusers<-nrow(Transactedusers)
Loginonlyusers<- loginonlycount
Loginonlyusers<-data.frame(Loginonlyusers)
Loginonlyusers$Date<-NA
Loginonlyusers<-Loginonlyusers[c(2,1)]
colnames(Loginonlyusers)[2]<-"Users"
Loginonlyusers$Type<-"Login only"
Loginonlyusers<-Loginonlyusers[c(1,3,2)]
colnames(Loginonlyusers)[3]<-"Users"
Loginonlyusers$Date<- Date$Date
Loginonlyusers$Type<- NULL
colnames(Loginonlyusers)[2] <- "Login"

#Joing Transact to Login & sum total
Dailyusers2<- left_join(Loginonlyusers, Transactedonlyusers, by = c("Date" = "Date"))
Dailyusers2$Total <- NULL
Dailyusers2$Total <- Dailyusers2$Login + Dailyusers2$Transact

#Binding Daily Users
DailyUsersBase<- data.frame(rbind(DailyUsersBase,Dailyusers2))

rm("loginonlycount","loginonly","Currentdayusers","Date","Login","Transactedusers","Loginonlyusers","Transactedonlyusers")

#Writing Dailyusers
write.csv(DailyUsersBase,file="C:/Users/CP666383/Desktop/Power BI Reports/DMAU/rcode(acct_id)/Daily Users.csv")

