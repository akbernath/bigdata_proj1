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

51*11 #elements of mat

nrg_types<-matrix(rep(0,561),nrow=11)
nrg_types[1,]<-c("State",state_names)
nrg_types[,1]<-c("State","Utility Gas","Bottled, tank, or LP gas","Electricity","Fuel oil, kerosene, etc","Coal or coke","Wood","Solar engery","Other fuel","No fuel used",NA)
nrg_types[,1]

for(i in 1:50){
  h=i+1
  state<-read.csv(paste("/Users/heatherhisako1/ss12h",tolower(state.abb)[i],"-cut.csv",sep=""),header=TRUE)
  state_df <- tbl_df(state)
  state_df <- mutate(state_df, energy_type = HFL_codes[as.character(HFL)])
  
  state_nrg_type <- group_by(state_df, HFL)
  
  nrg_summary <- summarise(state_nrg_type, 
                           energy_type = first(energy_type),
                           n = n(),
                           n_missing = sum(is.na(HFL)))
  
  nrg_summary2 <- mutate(nrg_summary, prop = n/sum(n))
  for(j in 1:(length(nrg_summary2$energy_type)-1)){
    for(k in 2:10){
      if(nrg_summary2$energy_type[j]==nrg_types[k,1]){
        nrg_types[k,h]=nrg_summary2$prop[j]
      }
    } 
  }
  nrg_types[11,h]=nrg_summary2$prop[length(nrg_summary2$energy_type)]
}

write.csv(nrg_types, file = "/Users/heatherhisako1/Documents/bigdata_proj1/nrg_types.csv")
