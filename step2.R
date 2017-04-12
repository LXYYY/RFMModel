library(magrittr)
library(dplyr)
library(readxl)
library(fpc)
library(lattice)
library(MASS)
library(nnet)
library(outliers)

Matrix=c(1,0.71,0.46,1.41,1,0.85,2.18,1.18,1);
dim(Matrix)=c(3,3)
ev=eigen(Matrix)
EValue=max(as.numeric(ev$values))
n=dim(Matrix)[1]
CI=(EValue-n)/(n-1)

salesRFM <- read_excel("consumption_data.xls")
names(salesRFM)<-c("CustomerId", "Monetization", "Frequency","Recency")

#data processing
sum(is.na(salesRFM))
sum(complete.cases(salesRFM))

#boxplot analysis
bplot=boxplot(salesRFM$Monetization,boxwex=0.7)
if(length(bplot$out)>0)
  salesRFM <- salesRFM[-which(salesRFM$Monetization %in% bplot$out),]
bplot=boxplot(salesRFM$Frequency,boxwex=0.7)
if(length(bplot$out)>0)
  salesRFM <- salesRFM[-which(salesRFM$Frequency %in% bplot$out),]
bplot=boxplot(salesRFM$Recency,boxwex=0.7)
if(length(bplot$out)>0)
  salesRFM <- salesRFM[-which(salesRFM$Recency %in% bplot$out),]

#k-means Abnormal detection
salesRFM$Monetization=scale(salesRFM$Monetization,F)
salesRFM$Frequency=scale(salesRFM$Frequency,F)
salesRFM$Recency=scale(salesRFM$Recency,F)
kmeansAbnDetecion<-kmeans(salesRFM[,2:4],centers=8)
centers<-kmeansAbnDetecion$centers[kmeansAbnDetecion$cluster,]
centers
distances<-sqrt(rowSums((salesRFM-centers)^2))
outliers<-order(distances,decreasing = T)[1:10]
plot(salesRFM[,2:3],pch="o",col=kmeansAbnDetecion$cluster,cex=0.3)
outliers
points(salesRFM[outliers,2:3],pch="+",col=4,cex=1.5)
salesRFM <- salesRFM[-which(salesRFM$CustomerId %in% outliers),]

#scale
salesRFM$Monetization=scale(salesRFM$Monetization,F)
salesRFM$Frequency=scale(salesRFM$Frequency,F)
salesRFM$Recency=scale(salesRFM$Recency,F)

salesRFM$Value=salesRFM$Monetization*0.221+salesRFM$Frequency*0.341+salesRFM$Recency*0.439

salesRFM$Category = kmeans(salesRFM$Value,8)$cluster

CategoryInfo <- aggregate(salesRFM[,2:5],list(salesRFM$Category),mean)

names(CategoryInfo)<-c("CategoryInfoID", "MonetizationMean", "FrequencyMean","RecencyMean","ValueMean")

MonetizationMean=mean(CategoryInfo$MonetizationMean)
FrequencyMean=mean(CategoryInfo$FrequencyMean)
RecencyMean=mean(CategoryInfo$RecencyMean)
ValueMean=mean(CategoryInfo$ValueMean)

CategoryInfo$MonetizationComp=CategoryInfo$MonetizationMean>MonetizationMean
CategoryInfo$FrequencyComp=CategoryInfo$FrequencyMean>FrequencyMean
CategoryInfo$RecencyComp=CategoryInfo$RecencyMean>RecencyMean
CategoryInfo$ValueComp=CategoryInfo$ValueMean>ValueMean

CategoryInfo=CategoryInfo[order(CategoryInfo$ValueMean,decreasing=T),]

