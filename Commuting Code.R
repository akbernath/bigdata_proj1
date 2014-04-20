##  ACS Data Project
##  Commute Times and Sprawl:
##  Transportation Profiles for Los Angeles, California
##  and Portland, Oregon

##  Code contributed by Andrew Bernath

##  Setup working directory, libraries, global options
setwd("D:/School/Spring 14/ST - Big Data/Proj1")
library(dplyr)
library(plyr)
library(ggplot2)
library(tables)
options(stringsAsFactors = FALSE)

#########################
##  Data Manipulation  ##
#########################


##  Create Raw Data Frames  ##

##  For California data
acs_pca <- read.csv("Data/csv_pca/ss12pca.csv")
acs.ca.data <- as.data.frame(acs_pca)

##  For Oregon data
acs_por <- read.csv("Data/csv_por/ss12por.csv")
acs.or.data <- as.data.frame(acs_por)



##  Reduce Data  ##

#### Following county region expanded in third commit ####
##  Collect rows for persons reporting LA County as home
home_LA <- as.data.frame(filter(acs.ca.data, 
            (PUMA00 == "5401")|(PUMA00 == "5402")|
            (PUMA00 == "5403")|(PUMA00 == "5404")|
            (PUMA00 == "5405")|(PUMA00 == "5406")|
            (PUMA00 == "5407")|(PUMA00 == "5408")|
            (PUMA00 == "5409")|(PUMA00 == "5410")|
            (PUMA00 == "5411")|(PUMA00 == "5412")|
            (PUMA00 == "5413")|(PUMA00 == "5414")|
            (PUMA00 == "5415")|(PUMA00 == "5416")|
            (PUMA00 == "5417")|(PUMA00 == "5418")|
            (PUMA00 == "5419")|(PUMA00 == "5420")|
            (PUMA00 == "5421")|(PUMA00 == "5422")|
            (PUMA00 == "5423")|(PUMA00 == "5424")|
            (PUMA00 == "6101")|(PUMA00 == "6102")|
            (PUMA00 == "6103")|(PUMA00 == "6104")|
            (PUMA00 == "6105")|(PUMA00 == "6106")|
            (PUMA00 == "6107")|(PUMA00 == "6108")|
            (PUMA00 == "6109")|(PUMA00 == "6110")|
            (PUMA00 == "6111")|(PUMA00 == "6112")|
            (PUMA00 == "6113")|(PUMA00 == "6114")|
            (PUMA00 == "6115")|(PUMA00 == "6116")|
            (PUMA00 == "6117")|(PUMA00 == "6118")|
            (PUMA00 == "6119")|(PUMA00 == "6410")|
            (PUMA00 == "5701")|(PUMA00 == "5702")|
            (PUMA00 == "5703")|(PUMA00 == "6120")|
            (PUMA00 == "6121")|(PUMA00 == "6122")|
            (PUMA00 == "6123")|(PUMA00 == "6124")|
            (PUMA00 == "6125")|(PUMA00 == "6126")|
            (PUMA00 == "4500")|(PUMA00 == "4600")|
            (PUMA00 == "4700")|(PUMA00 == "4800")|
            (PUMA00 == "4900")|(PUMA00 == "5000")|
            (PUMA00 == "5100")|(PUMA00 == "5200")|
            (PUMA00 == "5300")|(PUMA00 == "5500")|
            (PUMA00 == "5600")|(PUMA00 == "5800")|
            (PUMA00 == "5900")|(PUMA00 == "6000")
        ))
##  LA County PUMA codes found at:
##  http://www2.census.gov/census_2000/datasets/PUMS/FivePercent/California/PUMEQ5-CA.TXT


#### Following county region expanded in third commit ####
##  Collect rows for persons reporting Portland city or 
##  Mulnomah County (and some adjacent cities) as home
home_PD <- as.data.frame(filter(acs.or.data,
            (PUMA00 == "1301")|(PUMA00 == "1302")|
            (PUMA00 == "1303")|(PUMA00 == "1304")|
            (PUMA00 == "1305")|(PUMA00 == "1306")|
            (PUMA00 == "1307")|(PUMA00 == "1308")|
            (PUMA00 == "1309")|(PUMA00 == "1310")|
            (PUMA00 == "1311")|(PUMA00 == "1312")|
            (PUMA00 == "1313")
        ))
##  Portland City PUMA codes found at:
##  http://www2.census.gov/census_2000/datasets/PUMS/FivePercent/Oregon/PUMEQ5-OR.TXT
    


######################
##  Data Summaries  ##
######################

##  Sample sizes from each city
n.la <- length(home_LA$PUMA00)
n.pd <- length(home_PD$PUMA00)


##  Travel Time Summaries  ##

##  Number of missing entries for travel time
la.time.na <- count(is.na(home_LA$JWMNP))
pd.time.na <- count(is.na(home_PD$JWMNP))

##  Mean reported travel times
la.avgtime <- mean(home_LA$JWMNP, na.rm=TRUE)
pd.avgtime <- mean(home_PD$JWMNP, na.rm=TRUE)
tot.avgtime <- mean(c(home_LA$JWMNP,home_PD$JWMNP),na.rm=TRUE)

#### Unused code commented out in 4th commit ####
####  Occupancy Summaries  ##
####  Mean number of vehicle occupants
##la.avgocc <- mean(home_LA$JWRIP, na.rm=TRUE)
##pd.avgocc <- mean(home_PD$JWRIP, na.rm=TRUE)


##  Vehicle Type Summaries  ##

##  Number of missing entries for transportation type
la.type.na <- count(is.na(home_LA$JWTR))
pd.type.na <- count(is.na(home_PD$JWTR))

##  Proportion of types of transportation
la.counts <- count(home_LA$JWTR)
la.counts[,2] <- round_any(100*la.counts[,2]/la.type.na[1,2],
                           0.01,f=round)
pd.counts <- count(home_PD$JWTR)
pd.counts[,2] <- round_any(100*pd.counts[,2]/pd.type.na[1,2],
                           0.01,f=round)
##  So to rebut the band Missing Persons, 2.8% of
##  commuters walk in LA.


#### Following section added in 2nd commit ####
#### Following section expanded in 4th commit ####
##  Mean Travel Times by Vehicle Type  ##

##  Counts of travel type
LA.cnt <- count(home_LA, .(JWTR))
PD.cnt <- count(home_PD, .(JWTR))
# note only 1 person traveled by ferry in Portland, so no SD

## Los Angeles
LA.data <- ddply(home_LA, .(JWTR), summarise,
                 mean=mean(JWMNP,na.rm=TRUE),
                 sd=sd(JWMNP,na.rm=TRUE))
LA.data[,4] <- rep(0,13)
LA.data[,5] <- rep("Los Angeles",13)
colnames(LA.data) <- c("type","mean","sd","se","City")

##  Loop to create standard errors
for(i in 1:13)(
  LA.data[i,4] <- LA.data$sd[i]/LA.cnt$freq[i]
  )

##  Portland
PD.data <- ddply(home_PD, .(JWTR), summarise,
                 mean=mean(JWMNP,na.rm=TRUE),
                 sd=sd(JWMNP,na.rm=TRUE))
PD.data[,4] <- rep(0,13)
PD.data[,5] <- rep("Portland",13)
colnames(PD.data) <- c("type","mean","sd","se","City")

##  Loop to create standard errors
for(i in 1:13)(
  PD.data[i,4] <- PD.data$sd[i]/PD.cnt$freq[i]
)

##  Clean up entries
LA.data <- LA.data[c(-11,-13),]
PD.data <- PD.data[c(-11,-13),]
la.counts <- la.counts[c(-11,-13),]
pd.counts <- pd.counts[c(-11,-13),]

##  Re-label types
LA.data$type <- PD.data$type <- c("Car/Truck/Van","Bus",
                                  "Streetcar","Subway","Railroad","Ferry",
                                  "Taxi","Motorcycle","Bicycle","Walked","Other")

#### Added proportion table in 6th commit ####
city.table <- as.data.frame(cbind(LA.data$type,la.counts[,2],pd.counts[,2]))
colnames(city.table) <- c("Type","LA","Portland")


#### Following section added in 4th commit ####
####################
## Visualization  ##
####################


## Organize data and prep for graph
1 <- rbind(LA.data, PD.data)
limits <- aes(ymax=mean+se, ymin=mean-se)
dodge <- position_dodge(width=0.8)

##  Graph for commute times
ggplot(data=city.data, aes(x=factor(type),y=mean,fill=City))+
  geom_bar(stat="identity",position=dodge,color="black",width=0.8)+
  ggtitle("Mean Travel Time by Type")+
  theme(plot.title=element_text(size=24,face="bold"))+
  xlab("Transportation Type")+
  ylab("Average Travel Time (minutes)")+
  scale_fill_manual(values=c("#456AB4","#33CC33"))+
  geom_abline(intercept=la.avgtime,slope=0,color="#456AB4",size=1)+
  geom_abline(intercept=pd.avgtime,slope=0,color="#33CC33",size=1)+
  geom_text(aes(10.5,31,label="LA Average"),size=4)+
  geom_text(aes(10.5,23.5,label="Portland Average"),size=4)+
  geom_errorbar(limits,position=dodge,width=0.25)


#### 6th commit added table for proportion of use
tabular(data=city.table,Heading("Transportation Type")*factor(Type)~Heading("Percent Use by Type")*(Heading()*identity*(LA+Portland)))

