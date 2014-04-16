##  ACS Data Project
##  Commute Times and Sprawl:
##  Transportation Profiles for Los Angeles, California
##  and Portland, Oregon

##  Code contributed by Andrew Bernath

##  Setup working directory, libraries, global options
setwd("D:/School/Spring 14/ST - Big Data/Proj1")
library(dplyr)
library(plyr)
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

##  Collect rows for persons reporting greater LA area as home
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
    (PUMA00 == "5423")|(PUMA00 == "5424")
    ))
##  LA City PUMA codes found at:
##  http://www2.census.gov/census_2000/datasets/PUMS/FivePercent/California/PUMEQ5-CA.TXT


##  Collect rows for persons reporting greater Portland area as home
home_PD <- filter(acs.or.data,
    (PUMA00 == "1301")|(PUMA00 == "1302")|
    (PUMA00 == "1303")|(PUMA00 == "1304")|
    (PUMA00 == "1305")|(PUMA00 == "1306")|
    (PUMA00 == "1307")|(PUMA00 == "1308")|
    (PUMA00 == "1309")|(PUMA00 == "1310")|
    (PUMA00 == "1311")|(PUMA00 == "1312")|
    (PUMA00 == "1313")
    )
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



##  Occupancy Summaries  ##

##  Mean number of vehicle occupants
la.avgocc <- mean(home_LA$JWRIP, na.rm=TRUE)
pd.avgocc <- mean(home_PD$JWRIP, na.rm=TRUE)



##  Vehicle Type Summaries  ##

##  Number of missing entries for transportation type
la.type.na <- count(is.na(home_LA$JWTR))
pd.type.na <- count(is.na(home_PD$JWTR))

##  Proportion of types of transportation
la.counts <- count(home_LA$JWTR)
la.counts[,2] <- la.counts[,2]/la.type.na[1,2]
pd.counts <- count(home_PD$JWTR)
pd.counts[,2] <- pd.counts[,2]/pd.type.na[1,2]



#### Following section added in 2nd commit ####
##  Mean Travel Times by Vehicle Type  ##

## Los Angeles
ddply(home_LA, .(JWTR), summarise, mean=mean(JWMNP,na.rm=TRUE))

##  Portland
ddply(home_PD, .(JWTR), summarise, mean=mean(JWMNP,na.rm=TRUE))