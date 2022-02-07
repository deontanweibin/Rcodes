output <- split(Discrepancies_table, Discrepancies_table$po)

wb <- createWorkbook()

for (i in 1:length(output)) {
  addWorksheet(wb, sheetName=names(output[i]))
  writeData(wb, sheet=names(output[i]), x=output[[i]]) # Note [[]]
}

saveWorkbook(wb, "C:/Users/Dtan1/OneDrive - Williams-Sonoma Inc/Desktop/mytasks/Manual Shipment Uploading/4.Europe/Practice/For P12/Discrepancies.xlsx", overwrite = TRUE)
