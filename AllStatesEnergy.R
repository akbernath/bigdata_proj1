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
  
  nrg_summary2 <- mutate(nrg_summary, prop = round(n/sum(n),6))
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

library(ggplot2)

solar.prop<-c(as.numeric(nrg_types[8,2:51]))
solar.mat<-cbind(solar.prop,state_names[1:50])
new.solar<-solar.mat[order(solar.prop),]
qplot(as.numeric(new.solar[,1]),reorder(new.solar[,2],order(as.numeric(new.solar[,1]))),main="Proportion of Households Predominantly Using Solar Energy by State",xlab="Proportion of Households Predominantly Using Solar Energy",ylab="State")
#awesome plot!!! Hawaii wins solar energy

elec.prop<-c(as.numeric(nrg_types[4,2:51]))
elec.mat<-cbind(elec.prop,state_names[1:50])
new.elec<-elec.mat[order(elec.prop),]
qplot(as.numeric(new.elec[,1]),reorder(new.elec[,2],order(as.numeric(new.elec[,1]))),main="Proportion of Households Predominantly Using Electricity by State",xlab="Proportion of Households Predominantly Using Solar Energy",ylab="State")
# Florida wins electricity

##### electricity bill 

e.bill<-matrix(rep(0,153),nrow=3)
e.bill[1,]<-c("State",state_names)
e.bill[,1]<-c("State","Mean Elec Bill","SD Elec Bill")
e.bill[,1]
for(i in 1:50){
  h=i+1
  state<-read.csv(paste("/Users/heatherhisako1/ss12h",tolower(state.abb)[i],"-cut.csv",sep=""),header=TRUE)
  state_df <- tbl_df(state)
 
  e.bill[2,h]=mean(state_df$ELEP,na.rm=TRUE)
  e.bill[3,h]=sd(state_df$ELEP,na.rm=TRUE)
 
}

head(e.bill)

mean<-e.bill[2,2:51]
se<-e.bill[3,2:51]

e.mat<-cbind(mean,se,state_names)
new.emat<-e.mat[order(as.numeric(mean)),]
head(new.emat)

ord_mean_bill<-as.numeric(new.emat[,1])
ord_sd_bill<-as.numeric(new.emat[,2])
ord_state_e<-new.emat[,3]

new.emat<-cbind(ord_mean_bill,ord_sd_bill,ord_state_e)


qplot(ord_mean_bill, reorder(ord_state_e,ord_mean_bill))

install.packages("Hmisc", dependencies=T)
library("Hmisc")

d = data.frame(
  x  = reorder(ord_state_e,ord_mean_bill)
  , y  = ord_mean_bill
  , sd = ord_sd_bill
)

#plot of mean electricty bill and standard error bars
plot(d$x, d$y, ylim=c(min(d$y-d$sd),max(d$y+d$sd)),type="n",main="Average Electricity Bill by State with Standard Error Bars",xlab="Average Electricity Bill",ylab="State")
with (
  data = d
  , expr = errbar(x, y, y+sd, y-sd, add=T, pch=1, cap=.1)
)

