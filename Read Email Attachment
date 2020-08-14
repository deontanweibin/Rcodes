library(RDCOMClient) #load package


outlook_app <- COMCreate("Outlook.Application") #find email
search <- outlook_app$AdvancedSearch(
  "Inbox",
  "urn:schemas:httpmail:sendername = 'Dash Batch'"
)



results <- search$Results()
Sys.sleep(30)

for(i in 1:results$Count()){
  if(as.Date("1899-12-30") + floor(results$Item(i)$ReceivedTime()) == Sys.Date()){
    email <- results$Item(i)
  }
}

#save email attachment

attachment_file <- "C:\\Users\\P1318124\\Desktop\\Remittance\\Insurance\\insurance_report.csv"
email$Attachments(1)$SaveAsFile(attachment_file)
data <- read.csv(attachment_file)
