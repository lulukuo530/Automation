library(stringr)
library(openxlsx)
library(magrittr)
rm(list=ls())
coding = read.xlsx("/Users/macintosh/Desktop/R/Coding practice/190027_Coding_0321(149).xlsx", sheet = "Gen_Code", colNames = TRUE)
codelist = read.xlsx("/Users/macintosh/Desktop/R/coding practice/190027_C.xlsx", colNames = FALSE)

# If coding Verbatim matches codelist brand, add codelist brand name to Key Word
x=NULL
for(i in 1:length(codelist[,3])){
  pattern = gsub("／", "\\|", codelist[,3])[i]
  x[[i]] = grep(pattern, coding[,4], ignore.case = TRUE)
  coding[x[[i]],8] = 
    ifelse(is.na(coding[x[[i]],8]),
           strsplit(codelist[i,3], "／")[[1]][1],
           paste(coding[x[[i]],8], strsplit(codelist[i,3], "／")[[1]][1], sep=";"))
}
coding[,c("Verbatim","Key.Word")]

colnames(coding) = gsub(".", " ", colnames(coding), fixed = TRUE)
write.xlsx(coding, "/Users/macintosh/Desktop/R/Coding practice/Coding Matched.xlsx", colNames = TRUE)
head(coding)
