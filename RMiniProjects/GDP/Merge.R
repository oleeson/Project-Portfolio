#merge data by country code
both <- merge(gdpdata,edudata, by="C_Code")
#order data by GDP
both <- both[order(both$GDP_Mills),] 
#delete observations with no ID
both <- subset(both, both$ID != "")

#output cleaned data as csv
wd <- getwd()
filePath <- paste(wd,"/MergedData.csv",sep='')
write.csv(both, file = filePath, row.names = FALSE)
