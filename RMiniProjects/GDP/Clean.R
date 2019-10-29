#attach data frame gdpdata
attach(gdpdata)

#view data before analysis
summary(gdpdata)

#rename variables
gdpdata <- subset(gdpdata, select= -c(X.2, X.5, X.6, X.7, X.8, X.9))
gdpdata <- rename(gdpdata, c("X" = "C_Code", "X.1" = "ID", "X.3" = "Country", "X.4" = "GDP_Millions"))

#check GDP type and change to integer/double
typeof('GDP_Millions')
gdpdata$GDP_Mills <- as.numeric(gsub("[^[:digit:]]","", gdpdata$GDP_Millions))
gdpdata <- subset(gdpdata, select= - c(GDP_Millions))

#drop observations with no GDP
count(is.na(gdpdata$GDP_Mills))
gdpdata <- gdpdata %>% drop_na(GDP_Mills)
count(is.na(gdpdata$GDP_Mills))
detach(gdpdata)

#attach dataframe and load libraries required for cleaning
attach(edudata)
#rename two columns
edudata <- rename(edudata, c("CountryCode" ="C_Code", "Long.Name" = "Country"))
#drop unneeded columns
edudata <- subset(edudata, select= c(C_Code, Country, Income.Group, Region))
detach(edudata)