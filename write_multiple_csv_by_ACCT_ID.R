#set working directory
library(dplyr)
directory <- "X:\\Group Digital Life\\mCommerce\\New Singtel Dash Reporting\\Fictitious_Accounts_Management_&_Regulatory_Data_Report\\Transaction Dump\\Transaction_Dump_Feb20\\"
setwd(directory)

df <- read.csv( file = "X:/Group Digital Life/mCommerce/New Singtel Dash Reporting/Fictitious_Accounts_Management_&_Regulatory_Data_Report/Transaction Dump/Transaction_Dump_Feb20.csv")

split_csv <- function (df,y) {
  # splits dataframe into multiple csvs for each unique ID in the ID column
  # df is a dataframe
  # y is the index of the ID column, starting from 1
  # the dplyr package is required for this function
  unique_accounts <- unique(df[y])
  
  for (i in 1:nrow(unique(df[y]))) {
    print(i)
    id = unique_accounts[i,]
    print(id)
    x_filename <- paste0(id,"_transactions.csv", sep='')
    filtered_df <- df %>% filter(MAIN_ACCOUNT==unique_accounts[i,1]) 
    write.csv(filtered_df, file = x_filename, row.names=FALSE)
    
  }
}

split_csv(df,18)
