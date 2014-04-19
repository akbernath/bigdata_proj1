# Code to repeat for all states
library(dplyr)

# 1. download all state 5year files
# 2. unzip all state 5year files
# 3. cut out columns of interest
# 4. read in a file, produce summary, and concatenate

state_names <- c(tolower(state.abb),"dc")

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

nrg_types<-matrix(rep(0,624),nrow=52)
nrg_types[,1]<-c("State",tolower(state.name[1:50]),tolower("District of Columbia"))
nrg_types[,2]<-c("Abb",state_names)
nrg_types[1,]<-c("State","Abb","Utility Gas","Bottled, tank, or LP gas","Electricity","Fuel oil, kerosene, etc","Coal or coke","Wood","Solar engery","Other fuel","No fuel used",NA)
nrg_types[1,]

for(i in 1:51){
  h=i+1
  state<-read.csv(paste("/Users/heatherhisako1/ss12h",state_names[i],"-cut.csv",sep=""),header=TRUE)
  state_df <- tbl_df(state)
  state_df <- mutate(state_df, energy_type = HFL_codes[as.character(HFL)])
  
  state_nrg_type <- group_by(state_df, HFL)
  
  nrg_summary <- summarise(state_nrg_type, 
                           energy_type = first(energy_type),
                           n = n(),
                           n_missing = sum(is.na(HFL)))
  
  nrg_summary2 <- mutate(nrg_summary, prop = round(n/sum(n),6))
  for(j in 1:(length(nrg_summary2$energy_type)-1)){
    for(k in 3:11){
      if(nrg_summary2$energy_type[j]==nrg_types[1,k]){
        nrg_types[h,k]=nrg_summary2$prop[j]
      }
    } 
  }
  nrg_types[h,12]=nrg_summary2$prop[length(nrg_summary2$energy_type)]
}

write.csv(nrg_types, file = "/Users/heatherhisako1/Documents/bigdata_proj1/nrg_types.csv")

sum_nrg<-read.csv("/Users/heatherhisako1/Documents/bigdata_proj1/nrg_types.csv",header=TRUE)
names(sum_nrg)
library(ggplot2)

solar.prop<-c(as.numeric(nrg_types[2:52,9]))
solar.mat<-cbind(solar.prop,state_names[1:51])
new.solar<-solar.mat[order(solar.prop),]
qplot(as.numeric(new.solar[,1]),reorder(new.solar[,2],order(as.numeric(new.solar[,1]))),main="Proportion of Households Predominantly Using Solar Energy by State",xlab="Proportion of Households Predominantly Using Solar Energy",ylab="State")
#awesome plot!!! Hawaii wins solar energy

elec.prop<-c(as.numeric(nrg_types[2:52,5]))
elec.mat<-cbind(elec.prop,state_names[1:51])
new.elec<-elec.mat[order(elec.prop),]
qplot(as.numeric(new.elec[,1]),reorder(new.elec[,2],order(as.numeric(new.elec[,1]))),main="Proportion of Households Predominantly Using Electricity by State",xlab="Proportion of Households Predominantly Using Solar Energy",ylab="State")
# Florida wins electricity

##### electricity bill 

e.bill<-matrix(rep(0,156),nrow=3)
e.bill[1,]<-c("State",state_names)
e.bill[,1]<-c("State","Mean Elec Bill","SD Elec Bill")
e.bill[,1]
for(i in 1:51){
  h=i+1
  state<-read.csv(paste("/Users/heatherhisako1/ss12h",state_names[i],"-cut.csv",sep=""),header=TRUE)
  state_df <- tbl_df(state)
 
  e.bill[2,h]=mean(state_df$ELEP,na.rm=TRUE)
  e.bill[3,h]=sd(state_df$ELEP,na.rm=TRUE)
 
}

head(e.bill)

mean<-e.bill[2,2:52]
se<-e.bill[3,2:52]

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

#use ggplot to show point estimates and standard error bars
df <- data.frame(
  state = factor(reorder(ord_state_e,ord_mean_bill)),
  mean = ord_mean_bill,
  se = ord_sd_bill
)

limits <- aes(ymax = mean + se, ymin=mean - se)

p <- ggplot(df, aes(fill=mean,y=mean, x=state,main="Average Electricity Bill by State"))
p

p + geom_bar(position="dodge", stat="identity")
dodge <- position_dodge(width=0.9)

p + geom_bar(position=dodge) + 
  geom_errorbar(limits, position=dodge, width=0.25)+
  ggtitle("Average Electricity Bill by State")+xlab("State")+ylab("Average Energy Bill Cost")+
  scale_fill_gradient(low = "white", high = "blue")

###it looks like using means is a bad idea to look at center
##looks like the data is skewed because bills cant be negative
##lets look at medians instead and the 25% and 75% quartiles 
e.billmed<-matrix(rep(0,260),nrow=52)
e.billmed[,1]<-c("State",state_names)
e.billmed[1,]<-c("State","MeanElec","25Elec","MedElec","75Elec")
e.billmed[1,]
for(i in 1:51){
  h=i+1
  state<-read.csv(paste("/Users/heatherhisako1/ss12h",state_names[i],"-cut.csv",sep=""),header=TRUE)
  state_df <- tbl_df(state)
  
  e.billmed[h,2]=mean(state_df$ELEP[state_df$ELEP %in% c(3:999)])
  e.billmed[h,3]=quantile(state_df$ELEP[state_df$ELEP %in% c(3:999)])[2]
  e.billmed[h,4]=quantile(state_df$ELEP[state_df$ELEP %in% c(3:999)])[3]
  e.billmed[h,5]=quantile(state_df$ELEP[state_df$ELEP %in% c(3:999)])[4]
  
}
head(e.billmed)

mean<-e.billmed[2:52,2]
q25<-e.billmed[2:52,3]
med<-e.billmed[2:52,4]
q75<-e.billmed[2:52,5]

e.matmed<-cbind(med,mean,q25,q75,state_names)
new.ematmed<-e.matmed[order(as.numeric(med)),]
head(new.ematmed)

ord_med_bill<-as.numeric(new.ematmed[,1])
ord_mean_bill<-as.numeric(new.ematmed[,2])
ord_q25_bill<-as.numeric(new.ematmed[,3])
ord_q75_bill<-as.numeric(new.ematmed[,4])
ord_state_e<-new.ematmed[,5]

new.ematmed<-cbind(ord_med_bill,ord_mean_bill,ord_q25_bill,ord_q75_bill,ord_state_e)

head(new.ematmed)

install.packages("Hmisc", dependencies=T)
library("Hmisc")

d = data.frame(
  x  = 1:51
  , y  = ord_med_bill
  , z=ord_mean_bill
  , q25=ord_q25_bill
  ,q75=ord_q75_bill
)

plot(d$x, d$y, ylim=c(min(d$q25),max(d$q75)),type="n",main="Electricity Bill by State",xlab="State",ylab="Electricity Bill",xaxt="n")
mtext("Average and Median Electricty Bill with Interquartile Range Bars")
with (
  data = d
  , expr = errbar(x, y, q25, q75, add=T, pch=1, cap=.1)
)
points(d$x, d$z, col="red")
axis(1, at=d$x,labels=c(ord_state_e), col.axis="red", las=1,cex.axis=0.5)
legend(locator(1), c("State Median Bill","State Mean Bill"),pch = c(1,1), col=c("black","red"),bty="n")


###fuel cost

f.bill<-matrix(rep(0,156),nrow=3)
f.bill[1,]<-c("State",state_names)
f.bill[,1]<-c("State","Mean Fuel Bill","SD Fuel Bill")
f.bill[,1]
for(i in 1:51){
  h=i+1
  state<-read.csv(paste("/Users/heatherhisako1/ss12h",state_names[i],"-cut.csv",sep=""),header=TRUE)
  state_df <- tbl_df(state)
  
  f.bill[2,h]=mean(state_df$FULP,na.rm=TRUE)
  f.bill[3,h]=sd(state_df$FULP,na.rm=TRUE)
  
}

mean<-f.bill[2,2:52]
se<-f.bill[3,2:52]

f.mat<-cbind(mean,se,state_names)
new.fmat<-f.mat[order(as.numeric(mean)),]

ord_mean_fbill<-as.numeric(new.fmat[,1])
ord_sd_fbill<-as.numeric(new.fmat[,2])
ord_state_f<-new.fmat[,3]

new.fmat<-cbind(ord_mean_fbill,ord_sd_fbill,ord_state_f)


#use ggplot to show point estimates and standard error bars
df2 <- data.frame(
  state = factor(reorder(ord_state_f,ord_mean_fbill)),
  mean = ord_mean_fbill,
  se = ord_sd_fbill
)

limits <- aes(ymax = mean + se, ymin=mean - se)

p <- ggplot(df2, aes(fill=mean,y=mean, x=state,main="Average Fuel Bill by State"))
p

p + geom_bar(position="dodge", stat="identity")
dodge <- position_dodge(width=0.9)

p + geom_bar(position=dodge) + 
  geom_errorbar(limits, position=dodge, width=0.25)+
  ggtitle("Average Fuel Bill by State")+xlab("State")+ylab("Average Fuel Bill Cost")+
  scale_fill_gradient(low = "white", high = "green")
### again lets do medians instead

f.billmed<-matrix(rep(0,260),nrow=52)
f.billmed[,1]<-c("State",state_names)
f.billmed[1,]<-c("State","MeanFuel","25Fuel","MedFuel","75Fuel")
f.billmed[1,]
for(i in 1:51){
  h=i+1
  state<-read.csv(paste("/Users/heatherhisako1/ss12h",state_names[i],"-cut.csv",sep=""),header=TRUE)
  state_df <- tbl_df(state)
  
  f.billmed[h,2]=mean(state_df$FULP[state_df$FULP %in% c(3:9999)])
  f.billmed[h,3]=quantile(state_df$FULP[state_df$FULP %in% c(3:9999)])[2]
  f.billmed[h,4]=quantile(state_df$FULP[state_df$FULP %in% c(3:9999)])[3]
  f.billmed[h,5]=quantile(state_df$FULP[state_df$FULP %in% c(3:9999)])[4]
  
}
head(f.billmed)

mean<-f.billmed[2:52,2]
q25<-f.billmed[2:52,3]
med<-f.billmed[2:52,4]
q75<-f.billmed[2:52,5]

f.matmed<-cbind(med,mean,q25,q75,state_names)
new.fmatmed<-f.matmed[order(as.numeric(med)),]
head(new.fmatmed)

ord_med_bill<-as.numeric(new.fmatmed[,1])
ord_mean_bill<-as.numeric(new.fmatmed[,2])
ord_q25_bill<-as.numeric(new.fmatmed[,3])
ord_q75_bill<-as.numeric(new.fmatmed[,4])
ord_state_e<-new.fmatmed[,5]

new.fmatmed<-cbind(ord_med_bill,ord_mean_bill,ord_q25_bill,ord_q75_bill,ord_state_e)

head(new.fmatmed)

d = data.frame(
  x  = 1:51
  , y  = ord_med_bill
  , z=ord_mean_bill
  , q25=ord_q25_bill
  ,q75=ord_q75_bill
)

plot(d$x, d$y, ylim=c(min(d$q25),max(d$q75)),type="n",main="Fuel Bill by State",xlab="State",ylab="Fuel Bill",xaxt="n")
mtext("Average and Median Fuel Bill with Interquartile Range Bars")
with (
  data = d
  , expr = errbar(x, y, q25, q75, add=T, pch=1, cap=.1)
)
points(d$x, d$z, col="red")
axis(1, at=d$x,labels=c(ord_state_e), col.axis="red", las=1,cex.axis=0.5)
legend(locator(1), c("State Median Bill","State Mean Bill"),pch = c(1,1), col=c("black","red"),bty="n")



###gas cost

g.bill<-matrix(rep(0,156),nrow=3)
g.bill[1,]<-c("State",state_names)
g.bill[,1]<-c("State","Mean Gas Bill","SD Gas Bill")
g.bill[,1]
for(i in 1:51){
  h=i+1
  state<-read.csv(paste("/Users/heatherhisako1/ss12h",state_names[i],"-cut.csv",sep=""),header=TRUE)
  state_df <- tbl_df(state)
  
  g.bill[2,h]=mean(state_df$GASP,na.rm=TRUE)
  g.bill[3,h]=sd(state_df$GASP,na.rm=TRUE)
  
}

mean.g<-g.bill[2,2:52]
se.g<-g.bill[3,2:52]

g.mat<-cbind(mean.g,se.g,state_names)
new.gmat<-g.mat[order(as.numeric(mean.g)),]

ord_mean_gbill<-as.numeric(new.gmat[,1])
ord_sd_gbill<-as.numeric(new.gmat[,2])
ord_state_g<-new.fmat[,3]

new.gmat<-cbind(ord_mean_gbill,ord_sd_gbill,ord_state_g)


#use ggplot to show point estimates and standard error bars
df3 <- data.frame(
  state = factor(reorder(ord_state_g,ord_mean_gbill)),
  mean = ord_mean_gbill,
  se = ord_sd_gbill
)

limits <- aes(ymax = mean + se, ymin=mean - se)

p <- ggplot(df3, aes(fill=mean,y=mean, x=state,main="Average Gas Bill by State"))
p

p + geom_bar(position="dodge", stat="identity")
dodge <- position_dodge(width=0.9)

p + geom_bar(position=dodge) + 
  geom_errorbar(limits, position=dodge, width=0.25)+
  ggtitle("Average Gas Bill by State")+xlab("State")+ylab("Average Gas Bill Cost")+
  scale_fill_gradient(low = "white", high = "red")

### again lets do medians instead

g.billmed<-matrix(rep(0,260),nrow=52)
g.billmed[,1]<-c("State",state_names)
g.billmed[1,]<-c("State","MeanGas","25Gas","MedGas","75Gas")
g.billmed[1,]
for(i in 1:51){
  h=i+1
  state<-read.csv(paste("/Users/heatherhisako1/ss12h",state_names[i],"-cut.csv",sep=""),header=TRUE)
  state_df <- tbl_df(state)
  
  g.billmed[h,2]=mean(state_df$GASP[state_df$GASP %in% c(4:999)])
  g.billmed[h,3]=quantile(state_df$GASP[state_df$GASP %in% c(4:999)])[2]
  g.billmed[h,4]=quantile(state_df$GASP[state_df$GASP %in% c(4:999)])[3]
  g.billmed[h,5]=quantile(state_df$GASP[state_df$GASP %in% c(4:999)])[4]
  
}
head(g.billmed)

mean<-g.billmed[2:52,2]
q25<-g.billmed[2:52,3]
med<-g.billmed[2:52,4]
q75<-g.billmed[2:52,5]

g.matmed<-cbind(med,mean,q25,q75,state_names)
new.gmatmed<-g.matmed[order(as.numeric(med)),]
head(new.gmatmed)

ord_med_bill<-as.numeric(new.gmatmed[,1])
ord_mean_bill<-as.numeric(new.gmatmed[,2])
ord_q25_bill<-as.numeric(new.gmatmed[,3])
ord_q75_bill<-as.numeric(new.gmatmed[,4])
ord_state_e<-new.gmatmed[,5]

new.gmatmed<-cbind(ord_med_bill,ord_mean_bill,ord_q25_bill,ord_q75_bill,ord_state_e)

head(new.gmatmed)

d = data.frame(
  x  = 1:51
  , y  = ord_med_bill
  , z=ord_mean_bill
  , q25=ord_q25_bill
  ,q75=ord_q75_bill
)

plot(d$x, d$y, ylim=c(min(d$q25),max(d$q75)),type="n",main="Gas Bill by State",xlab="State",ylab="Gas Bill",xaxt="n")
mtext("Average and Median Gas Bill with Interquartile Range Bars")
with (
  data = d
  , expr = errbar(x, y, q25, q75, add=T, pch=1, cap=.1)
)
points(d$x, d$z, col="red")
axis(1, at=d$x,labels=c(ord_state_e), col.axis="red", las=1,cex.axis=0.5)
legend(locator(1), c("State Median Bill","State Mean Bill"),pch = c(1,1), col=c("black","red"),bty="n")




####map plots
sum_nrg<-read.csv("/Users/heatherhisako1/Documents/bigdata_proj1/nrg_types.csv",header=TRUE)
names(sum_nrg)

head(sum_nrg)
install.packages("maps")
require(maps)
require(ggplot2)
state=map_data("state")

# Not faceted
states_map = map_data("state")
ggplot(sum_nrg, aes(map_id = State, fill = Solar.engery)) +
  geom_map(map = states_map, color = "black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  scale_fill_gradient(low = "white", high = "blue")

# Faceted

sum_nrg2=sum_nrg
sum_nrg2

install.packages("reshape2")
library(reshape2) # for melt
sum_nrgm = melt(sum_nrg[,-2], id = 1)
states_map = map_data("state")
ggplot(sum_nrgm, aes(map_id = State, fill = value)) +
  geom_map(map = states_map, color = "black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  facet_wrap( ~ variable,scales="free") +
  scale_fill_gradient(low = "white", high = "blue")

###what about alaska and hawaii?
#use charlottes code
library(mapproj)
require(maps)
library(maptools)
library(dplyr)
library(ggplot2)


usa <- readShapeSpatial("/Users/heatherhisako1/Downloads/gz_2010_us_040_00_20m/gz_2010_us_040_00_20m.shp")
usa_df <- fortify(usa)
usa_df$state <-  as.character(usa$NAME[as.numeric(usa_df$id) + 1])

usa_df <- subset(usa_df, state != "Puerto Rico")
# remove Puerto Rico 

# wrap alutiean?
usa_df$long[usa_df$long  > 0] <- usa_df$long[usa_df$long  > 0] - 360

# put alaska and hawaii in different facets
usa_df$ak_hi <- as.character(usa_df$state)
usa_df$ak_hi[!(usa_df$ak_hi %in% c("Alaska", "Hawaii"))] <- "lower48"

# add abbreviations
abbs <- tolower(c(state.abb, "dc"))
names(abbs) <- c(state.name, "District of Columbia")

usa_df$state.abb <- abbs[usa_df$state]
usa_df <- plyr::rename(usa_df, 
                       c("state" = "state_name", "state.abb" = "state"))

# long, lat version with alaska and hawaii in correct locations
saveRDS(usa_df, "usa-state-map.rds")

# == project and move ak and hi == #
# all 
lower48 <- subset(usa_df, 
                  !(state_name %in% c("Alaska", "Hawaii")))

ak <- subset(usa_df, state_name == "Alaska")
hi <- subset(usa_df, state_name == "Hawaii")
# scale and move closer


ak_proj <- as.data.frame(mapproject(ak$long,  ak$lat,
                                    projection = "albers", 
                                    parameters = list(lat0 = 55, lat1 = 65))[c("x","y")])
ak_proj <- cbind(ak, ak_proj)

hi_proj <- as.data.frame(mapproject(hi$long,  hi$lat,
                                    projection = "albers", 
                                    parameters = list(lat0 = 8, lat1 = 18))[c("x","y")])
hi_proj <- cbind(hi, hi_proj)

lower48_proj <- as.data.frame(
  mapproject(lower48$long, lower48$lat,
             projection = "albers", 
             parameters = list(lat0 = 29.5, lat1 = 45.5))[c("x","y")])
lower48_proj <- cbind(lower48, lower48_proj)

ak_proj_trans <- mutate(ak_proj,
                        x = 0.35*x - 0.4, y = 0.35*y - 1.25)
hi_proj_trans <- mutate(hi_proj,
                        x = x - 0.2, y = y + 2.7)

usa_all <- rbind(lower48_proj, hi_proj_trans, ak_proj_trans)
# x, y version with alaska and hawaii near lower 48
saveRDS(usa_all, "usa-state-map_all.rds")

head(usa_all)

#plot


colnames(sum_nrg)[1]="state_name"
colnames(sum_nrg)[2]="state"
colnames(sum_nrg)[9]="Solar_Energy"
colnames(sum_nrg)[3]="Utility_Gas"
colnames(sum_nrg)[4]="Bottled_tank_or_LPGas"
colnames(sum_nrg)[6]="Fuel_oil_kerosene_etc"
colnames(sum_nrg)[7]="Coal_or_Coke"
colnames(sum_nrg)[10]="Other_Fuel"
colnames(sum_nrg)[11]="No_Fuel_Used"
sum_nrg$state=as.character(sum_nrg$state)
foo=inner_join(sum_nrg,usa_all,by="state")
class(sum_nrg$state)
class(usa_all$state)
head(foo)
head(sum_nrg)
head(usa_df)

ggplot(foo, aes(x,y, fill = Solar_energy,group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue")+
  coord_fixed()

qplot(x, y, data = foo, 
      geom = "polygon", group =group, fill = Solar_energy) +
  coord_fixed()

dim(foo)
dim(sum_nrg)
dim(usa_all)

names(sum_nrg)

plots=list()
plots[[1]]=ggplot(foo, aes(x,y, fill = Utility_Gas,group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue")+
  coord_fixed()
plots[[2]]=ggplot(foo, aes(x,y, fill = Bottled_tank_or_LPGas,group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue")+
  coord_fixed()
plots[[3]]=ggplot(foo, aes(x,y, fill = Electricity,group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue")+
  coord_fixed()
plots[[4]]=ggplot(foo, aes(x,y, fill = Fuel_oil_kerosene_etc,group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue")+
  coord_fixed()
plots[[5]]=ggplot(foo, aes(x,y, fill = Coal_or_Coke,group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue")+
  coord_fixed()
plots[[6]]=ggplot(foo, aes(x,y, fill = Wood,group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue")+
  coord_fixed()
plots[[7]]=ggplot(foo, aes(x,y, fill = Solar_Energy,group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue")+
  coord_fixed()
plots[[8]]=ggplot(foo, aes(x,y, fill = Other_Fuel,group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue")+
  coord_fixed()
plots[[9]]=ggplot(foo, aes(x,y, fill = No_Fuel_Used,group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue")+
  coord_fixed()

#yay! plots with map
install.packages("gridExtra")
library(gridExtra)

do.call(grid.arrange, plots)


