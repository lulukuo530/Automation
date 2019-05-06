rm(list=ls())
library(readxl)
library(openxlsx)
library(magrittr)
b18 = readxl::read_xls("C:/Users/kulu8001/Desktop/R/小工具練習/Datamap比對/180257/180195.xls") %>% as.data.frame()
b19= readxl::read_xls("C:/Users/kulu8001/Desktop/R/小工具練習/Datamap比對/180257/180257.xls") %>% as.data.frame()

### Paste all rows, check if file A's rows are in file B's rows
x = paste(b18[,1],b18[,2],b18[,3],b18[,4],b18[,5],b18[,6],b18[,7],b18[,8],b18[,9],sep= "_;,")
y = paste(b19[,1],b19[,2],b19[,3],b19[,4],b19[,5],b19[,6],b19[,7],b19[,8],b19[,9],sep= "_;,")
diff = y[!(y %in% x)]  #different rows will come up
diffdata = matrix(0, nrow=length(diff), ncol=ncol(b18)) %>% as.data.frame
colnames(diffdata) = colnames(b18)
for(i in 1:length(diff)){
  diffdata[i,] = t(strsplit(diff[i],"_;,")[[1]])
}

### Whether the code is newly add or not
diffdata$`New Add` = "Y"
for(j in 1:length(unique(diffdata$`Variable ID`))){
  oldAnsCode = b18[grep(unique(diffdata$`Variable ID`)[j],b18$`Variable ID`),5]
  for(i in 1:nrow(diffdata)){
    if (diffdata$`Variable ID`[i]==unique(diffdata$`Variable ID`)[j]){
      if (diffdata[i,5] %in% oldAnsCode) {
        diffdata[i,"New Add"] = "N"
      } else {
        diffdata[i,"New Add"] = "Y"
      }
    }
  }
}

bb = rbind(b18,b19) %>% unique()
z = paste(bb[,1],bb[,2],bb[,3],bb[,4],bb[,5],bb[,6],bb[,7],bb[,8],bb[,9],sep= "_;,")
w = paste(diffdata[,-10][,1],diffdata[,-10][,2],diffdata[,-10][,3],diffdata[,-10][,4],diffdata[,-10][,5],diffdata[,-10][,6],diffdata[,-10][,7],diffdata[,-10][,8],diffdata[,-10][,9],sep= "_;,")

### If 合併的dataset有在diff的dataset中, 標註"Y"
for(i in 1:length(z)){
  if(z[i] %in% w){
    z[i] = paste(z[i], "Y", sep="_;,")
  } else {
    z[i] = paste(z[i], " ", sep="_;,")
  }
}
z1 = strsplit(z, "_;,", fixed=TRUE)

newdata = matrix(0, nrow=length(z1), ncol=ncol(diffdata)) %>% data.frame()
colnames(newdata) = colnames(diffdata)
colnames(newdata)[10] = "Difference"
for(i in 1:length(z1)){
  newdata[i, ] = t(z1[[i]])
}

#只秀出兩年datamap不一致的row
newdata[which(newdata$Difference=="Y"),"New Add"] = diffdata[,"New Add"]
newT = newdata[with(newdata, order(`Question ID`,`Variable ID`,`Answer Code`)),]
newT[grep("N", newT$`New Add`)-1, "Difference"] = "Y"
for(i in 1:nrow(newT)){
  newT[i,"New Add"] = ifelse(is.na(newT[i,"New Add"]), 
                             "R", newT[i,"New Add"])
}
newT = newT[which(newT$Difference=="Y" & newT$`New Add`!="Y"),-10]
if(nrow(newT)>0){
  newT[which(newT$`New Add`=="R"),"New Add"] = " "
}

### replace NA with blank
for(i in 1:nrow(newT)){
  for(j in 1:ncol(newT)){
    newT[i,j] = gsub("NA", " ", newT[i,j])
  }
}

write.xlsx(newT, "C:/Users/kulu8001/Desktop/R/小工具練習/Datamap比對/180257/R_compare.xlsx", colNames=TRUE)


