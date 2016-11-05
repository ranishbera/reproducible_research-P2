url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url,destfile = "FStormData.csv")
data<-read.csv("FStormData.csv")
str(data)
sub_dat<-data[,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
library(plyr)
library(ggplot2)
library(gridExtra)
library(grid)

