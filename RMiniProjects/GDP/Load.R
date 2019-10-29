#Read in gdp data 
gdpdata <- read.csv(file="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", skip=4, header = TRUE, sep=",")

#Read in edu data
edudata <- read.csv(file="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", header = TRUE, sep=",")

#Activate libraries
library(tidyr)
library(plyr)

