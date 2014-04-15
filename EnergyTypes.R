#Heather Kitada 
#Looking at energy type used in home 
#housing level data 

#looking at data for Oregon first 
#then generalize code for all states

download.file("http://www2.census.gov/acs2012_5yr/pums/csv_hor.zip", 
              destfile = "/Users/heatherhisako1/csv_hor.zip")

#use getwd() to check working directory


or_h <- read.csv(unz("/Users/heatherhisako1/csv_hor.zip", "ss12hor.csv"), nrows = 10,  # notice just 10 lines! why?
                  stringsAsFactors = FALSE)
str(or_h)

#tells which column numbers
which(names(or_h)%in% c("ELEP", "FULP", "GASP", "HFL"))

types2 <- ifelse(names(or_h) %in% c( "ELEP", "FULP", "GASP", "HFL"), "integer", "NULL")

system.time(or3 <- read.csv(unz("/Users/heatherhisako1/csv_hor.zip", "ss12hor.csv"),stringsAsFactors = FALSE, colClasses = types2))
#modified charlottes code to use unz() this worked!
dim(or3)

head(or3)

library(dplyr)
library(ggplot2)
or_df <- tbl_df(or3)

HFL_codes <- c("1" = "Utility Gas",
                "2" = "Bottled, tank, or LP gas",
                "3" = "Electricity",
                "4" = "Fuel oil, kerosene, etc",
                "5" = "Coal or coke",
                "6" = "Wood",
                "7" = "Solar engery",
                "8" = "Other fuel",
                "9" = "No fuel used")

or_df <- mutate(or_df, energy_type = HFL_codes[as.character(HFL)])

nrg_type <- group_by(or_df, HFL)

nrg_summary <- summarise(nrg_type, 
                          energy_type = first(energy_type),
                          n = n(),
                         n_missing = sum(is.na(HFL)))

nrg_summary

mutate(nrg_summary, prop = n/sum(n))

energy_type = HFL_codes[as.character(or_df$HFL)]

nrg_summary2 <- mutate(nrg_summary, prop = round(n/sum(n),4))
qplot(energy_type, prop, data =nrg_summary2) 
qplot(reorder(energy_type, prop), prop, data =nrg_summary2) 
qplot( prop, reorder(energy_type, prop), data =nrg_summary2) 


