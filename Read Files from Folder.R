setwd("C:/Users/CP666383/Desktop/test")

myMergedData <- 
  do.call(rbind,
          lapply(list.files(path = "C:/Users/CP666383/Desktop/test"), read.csv))
