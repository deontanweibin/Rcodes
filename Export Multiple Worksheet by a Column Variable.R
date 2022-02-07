output <- split(Discrepancies_table, Discrepancies_table$po)

wb <- createWorkbook()

for (i in 1:length(output)) {
  addWorksheet(wb, sheetName=names(output[i]))
  writeData(wb, sheet=names(output[i]), x=output[[i]]) # Note [[]]
}

saveWorkbook(wb, ".xlsx", overwrite = TRUE)
