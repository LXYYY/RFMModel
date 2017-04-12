library(magrittr)
library(dplyr)
library(readxl)
library(fpc)
salesRFM <- read_excel("consumption_data.xls")

names(salesRFM)<-c("CustomerId", "Monetization", "Frequency","Recency")

salesRFM$Monetization=scale(salesRFM$Monetization,F)
salesRFM$Frequency=scale(salesRFM$Frequency,F)
salesRFM$Recency=scale(salesRFM$Recency,F)

salesRFM$Value=salesRFM$Monetization*0.221+salesRFM$Frequency*0.341+salesRFM$Recency*0.439

salesRFM$Category = kmeans(salesRFM$Value,8)$cluster

CategoryInfo = aggregate(salesRFM$Value,list(salesRFM$Category),mean)
names(CategoryInfo)<-c("CategoryID", "Mean")

ValueMean=mean(CategoryInfo$Mean)

CategoryInfo$Trend <- CategoryInfo$Mean>ValueMean

