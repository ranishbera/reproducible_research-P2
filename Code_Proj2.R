url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url,destfile = "FStormData.csv")
data<-read.csv("FStormData.csv")
str(data)
sub_dat<-data[,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

#Loading libraries required
library(plyr)
library(ggplot2)
library(gridExtra)
library(grid)

#Crating table for analysis
health<-ddply(sub_dat, .(EVTYPE), summarize,fatalities = sum(FATALITIES),injuries = sum(INJURIES))
fatal<-health[order(health$fatalities, decreasing = T), ]#Ordered data for fatalities
injuries<-health[order(health$injuries, decreasing = T), ]#Ordered data for injuries

#Function for converting denotations into power of 10
getExp <- function(e) {
    if (e %in% c("h", "H"))
        return(2)
    else if (e %in% c("k", "K"))
        return(3)
    else if (e %in% c("m", "M"))
        return(6)
    else if (e %in% c("b", "B"))
        return(9)
    else if (!is.na(as.numeric(e))) 
        return(as.numeric(e))
    else if (e %in% c("", "-", "?", "+"))
        return(0)
    else {
        stop("Invalid value.")
    }
}


#Getting data for economic damage
propExp <- sapply(sub_dat$PROPDMGEXP, FUN=getExp)
sub_dat$propDamage <- sub_dat$PROPDMG * (10 ** propExp)
cropExp <- sapply(sub_dat$CROPDMGEXP, FUN=getExp)
sub_dat$cropDamage <- sub_dat$CROPDMG * (10 ** cropExp)

econDamage <- ddply(sub_dat, .(EVTYPE), summarize,propDamage = sum(propDamage), cropDamage = sum(cropDamage))
econDamage <- econDamage[(econDamage$propDamage > 0 || econDamage$cropDamage > 0), ]%Those that do not cause financial loss will be ommitted

propDmgSorted <- econDamage[order(econDamage$propDamage, decreasing = T), ]%Ordered data for property damage
cropDmgSorted <- econDamage[order(econDamage$cropDamage, decreasing = T), ]%Ordered data for crop damage

#Results
#Most Injury Event
head(fatal[, c("EVTYPE", "fatalities")],5)

#Most Injury Event
head(injuries[, c("EVTYPE", "injuries")],5)

#Graphic reprsentation

p1 <- ggplot(data=head(injury,10), aes(x=reorder(EVTYPE, injuries), y=injuries)) +
   geom_bar(fill="olivedrab",stat="identity")  + coord_flip() + 
    ylab("Total number of injuries") + xlab("Event type") +
    ggtitle("Health impact of weather events in the US - Top 10") +
    theme(legend.position="none")

p2 <- ggplot(data=head(fatal,10), aes(x=reorder(EVTYPE, fatalities), y=fatalities)) +
    geom_bar(fill="red4",stat="identity") + coord_flip() +
    ylab("Total number of fatalities") + xlab("Event type") +
    theme(legend.position="none")

grid.arrange(p1, p2, nrow =2)

#Economic Damage results

#Property damage
head(propDmgSorted[, c("EVTYPE", "propDamage")], 5)

#Crop Damage

head(cropDmgSorted[, c("EVTYPE", "cropDamage")], 5)

#Plot
p1 <- ggplot(data=head(propDmgSorted,10), aes(x=reorder(EVTYPE, propDamage), y=log10(propDamage), fill=propDamage )) +
    geom_bar(fill="darkred", stat="identity") + coord_flip() +
    xlab("Event type") + ylab("Property damage in dollars (log10)") +
    ggtitle("Economic impact of weather events in the US - Top 10") +
    theme(plot.title = element_text(hjust = 0))

p2 <- ggplot(data=head(cropDmgSorted,10), aes(x=reorder(EVTYPE, cropDamage), y=cropDamage, fill=cropDamage)) +
    geom_bar(fill="goldenrod", stat="identity") + coord_flip() + 
    xlab("Event type") + ylab("Crop damage in dollars") + 
    theme(legend.position="none")

grid.arrange(p1, p2, ncol=1, nrow =2)
