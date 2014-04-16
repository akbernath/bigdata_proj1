# Code to repeat for all states
library(dplyr)

# 1. download all state 5year files
# 2. unzip all state 5year files
# 3. cut out columns of interest
# 4. read in a file, produce summary, and concatenate

state_names <- tolower(state.abb)

which(state_names=="nv")
# 1. download zip files
base_url <- "http://www2.census.gov/acs2012_5yr/pums/csv_h"

get_acs_pfile <- function(state_name){
  url <- paste(base_url, state_name, ".zip", sep = "")
  dest <- paste("/Users/heatherhisako1/csv_h", state_name, ".zip", sep = "")
  download.file(url, destfile = dest)
}

status <- lapply(state_names, 
                 failwith("failed to download", get_acs_pfile))

any(status == "failed to download")

# 2. unzip all state 5year files 
#example for oregon 
unzip("/Users/heatherhisako1/csv_hor.zip", exdir = "/Users/heatherhisako1/")
#generalize for all states

get_unzips<- function(state_name){
  dest <- paste("/Users/heatherhisako1/csv_h", state_name, ".zip", sep = "")
  unzip(dest, exdir = "/Users/heatherhisako1/")
}

status <- lapply(state_names, 
                 failwith("failed to unzip", get_unzips))

any(status == "failed to unzip")

# 3. cut out columns of interest 
#example for oregon 
system("cut -d, -f13,35,37,75 /Users/heatherhisako1/ss12hor.csv > /Users/heatherhisako1/ss12hor-cut.csv")

get_nrg_cols<-function(state_name){
  state.csv<-paste("/Users/heatherhisako1/ss12h", state_name,".csv",sep="")
  cut.csv<-paste("/Users/heatherhisako1/ss12h",state_name,"-cut.csv",sep="")
  system(paste("cut","-d,","-f20,22,23,24", state.csv,">",cut.csv,sep=" "))
}

status <- lapply(state_names, 
                 failwith("failed to cut", get_nrg_cols))

any(status == "failed to cut")

# 4. read in a file, produce a summary, concatenate

