#Nandhita Narendra Babu

#Project 1 - ST99 Big Data Analysis
#My Question of Interest - Immigration Analysis - Immigrant Pattern, Immigrants by race, Immigrant Earnings

#downloaded the files and extracted

download.file("http://www2.census.gov/acs2012_5yr/pums/csv_por.zip", destfile = "data/csv_por.zip")
unzip("data/csv_por.zip", list = TRUE)

or1 <- read.csv("data/ss12por.csv", stringsAsFactors = FALSE)

install.packages(c('dplyr','ggplot2'))
install.packages('RColorBrewer') 
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(grid)

native=group_by(wa_foreign, NATIVITY)
u=summarize(native, v=n(),na.rm=TRUE)
ggplot(u, aes(x=NATIVITY, y=v), fill=cond)+geom_bar(stat='identity')

#plot showing total population for each state
ggplot(data=nativitycheck, aes(x=State , y=ForBorn))+geom_bar(stat='identity')

#plot comparing native born and foreign born
q=qplot(State, Total, data=nativitycheck, geom='bar',fill='category',theme_set(theme_bw()))
q+geom_text(aes(label=State),)

#shows two plots side by side comparing foreign born and native born population
library(grid)
g.mid<-ggplot(nativitycheck,aes(x=1,y=State))+geom_text(aes(label=State))+
  ggtitle("")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"))

g1 <- ggplot(data = nativitycheck, aes(x = State, y=USBorn), fill=color) + 
  geom_bar(stat = "identity") + ggtitle("US Born Population") +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,-1,1,0), "mm")) +
  scale_y_reverse() + coord_flip()

g2 <- ggplot(data = nativitycheck, aes(x = State, y = ForBorn),fill=color) +xlab(NULL)+
  geom_bar(stat = "identity") + ggtitle("Foreign Born Population") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()
install.packages('gridExtra')
library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,0.5/9,4/9))

g1 <- ggplot(data = nativitycheck, aes(x = State, y=USBorn),fill=color)
g1+geom_bar(position='stacked') 

################################################################################################

ca_data <- read.csv("C:/Users/student/Documents/St599Project/ss12pca-data.csv",stringsAsFactors = FALSE)
ny_data <- read.csv("C:/Users/student/Documents/St599Project/ss12pny-data.csv",stringsAsFactors = FALSE)
fl_data <- read.csv("C:/Users/student/Documents/St599Project/ss12pfl-data.csv",stringsAsFactors = FALSE)
tx_data <- read.csv("C:/Users/student/Documents/St599Project/ss12ptx-data.csv",stringsAsFactors = FALSE)

full = rbind (ca_data, fl_data, ny_data, tx_data)
race_USBorn=subset(full, NATIVITY==1)
race_ForBorn=subset(full, NATIVITY==2)

race_ForBorn=subset(full, NATIVITY==2, select=c(RAC1P, ST, RAC2P05))
race_group=group_by(race_ForBorn, RAC1P, ST)
yu=summarize(race_group, p=n(), na.rm=TRUE)
yu$ST[yu$ST==6]='CA'
yu$ST[yu$ST==12]='FL'
yu$ST[yu$ST==36]='NY'
yu$ST[yu$ST==48]='TX'
yu$RAC1P[yu$RAC1P==1]='White'
yu$RAC1P[yu$RAC1P==2]='Black or African American'
yu$RAC1P[yu$RAC1P==3]='American Indian'
yu$RAC1P[yu$RAC1P==4]='Alaska Native'
yu$RAC1P[yu$RAC1P==5]='American Indian and Alaska Native tribes'
yu$RAC1P[yu$RAC1P==6]='Asian alone'
yu$RAC1P[yu$RAC1P==7]='Native Hawaiian'
yu$RAC1P[yu$RAC1P==8]='Some Other Race'
yu$RAC1P[yu$RAC1P==9]='Two or more races'

rac1p_forborn_full <- read.csv("C:/Users/student/Documents/St599Project/rac1p_ForBorn.csv",stringsAsFactors = FALSE)
rac1p_forborn <- ggplot(rac1p_forborn_full, aes(x=STATE,y=POPULATION, fill=RACE))
rac1p_forborn + geom_bar(stat='identity', position='dodge')

full1=subset(full, RAC2P05!=-9)
race_ForBorn2=subset(full1, NATIVITY==2, select=c(RAC1P, ST, RAC2P05))
race_group2=group_by(race_ForBorn2, RAC2P05, ST)
yup=summarize(race_group2, p=n(), na.rm=TRUE)
yup$ST[yup$ST==6]='CA'
yup$ST[yup$ST==12]='FL'
yup$ST[yup$ST==36]='NY'
yup$ST[yup$ST==48]='TX'

#####PLOT FOR RAC2P05 FOREIGN BORN#######
rac2p05_forborn_full <- read.csv("C:/Users/student/Documents/St599Project/rac2p05_ForBorn.csv",stringsAsFactors = FALSE)
rac2p05_forborn <- ggplot(rac2p05_forborn_full, aes(x=STATE,y=POPULATION, fill=RACE))
rac2p05_forborn + geom_bar(stat='identity')

rac2p05_ca=subset(rac2p05_forborn_full, STATE=='CA')
ggplot(rac2p05_ca, aes(x=RACE,y=POPULATION)) + geom_bar(stat='identity')

rac2p05_fl=subset(rac2p05_forborn_full, STATE=='FL')
ggplot(rac2p05_fl, aes(x=RACE,y=POPULATION)) + geom_bar(stat='identity')

rac2p05_ny=subset(rac2p05_forborn_full, STATE=='NY')
ggplot(rac2p05_ny, aes(x=RACE,y=POPULATION)) + geom_bar(stat='identity')

rac2p05_tx=subset(rac2p05_forborn_full, STATE=='TX')
ggplot(rac2p05_tx, aes(x=RACE,y=POPULATION)) + geom_bar(stat='identity')

order(rac2p05_tx$POPULATION, decreasing=TRUE)

pop=rbind(rac2p05_ca,rac2p05_fl,rac2p05_ny,rac2p05_tx)
pop1=subset(pop, POPULATION>1000)
pop1$RACE[pop1$RACE==1]='White alone'
pop1$RACE[pop1$RACE==2]='Black or African American alone'
pop1$RACE[pop1$RACE==40]='Asian Indian alone'
pop1$RACE[pop1$RACE==42]='Cambodian alone'
pop1$RACE[pop1$RACE==43]='Chinese alone'
pop1$RACE[pop1$RACE==44]='Filipino alone'
pop1$RACE[pop1$RACE==45]='Hmong alone'
pop1$RACE[pop1$RACE==47]='Japanese alone'
pop1$RACE[pop1$RACE==48]='Korean alone'
pop1$RACE[pop1$RACE==49]='Laotian alone'
pop1$RACE[pop1$RACE==53]='Thai alone'
pop1$RACE[pop1$RACE==54]='Vietnamese alone'
pop1$RACE[pop1$RACE==56]='Asian, not specified'
pop1$RACE[pop1$RACE==57]='Combinations of Asian groups only'
pop1$RACE[pop1$RACE==66]='Some other race alone'
pop1$RACE[pop1$RACE==67]='Two or more races'
pop1$RACE[pop1$RACE==41]='Bangladeshi alone'
pop1$RACE[pop1$RACE==51]='Pakistani alone'

rac<- ggplot(pop1, aes(x=STATE,y=POPULATION, fill=RACE))
rac + geom_bar(stat='identity', position='stack')+ coord_flip()

#################################################################################################

cat1_full = subset(full, PINCP>=50000)
cat2_full = subset(full, PINCP>=35000 & PINCP<50000)
cat3_full = subset(full, PINCP>=25000 & PINCP<35000)
cat4_full = subset(full, PINCP>=15000 & PINCP<25000)
cat5_full = subset(full, PINCP<15000)

cat1=cbind(cat1_full, Category='More than $50,000')
cat2=cbind(cat2_full, Category='Between $35,000 and $50,000')
cat3=cbind(cat3_full, Category='Between $25,000 and $35,000')
cat4=cbind(cat4_full, Category='Between $15,000 and $25,000')
cat5=cbind(cat5_full, Category='Less than $15,000')

Categories=rbind(cat1,cat2,cat3,cat4)
summary(Categories)
Categories_FOR=subset(Categories, NATIVITY==2)
Categories_US=subset(Categories, NATIVITY==1)

cat11=group_by(Categories_US, Category, ST)
cat111=summarize(cat11, p=n(), na.rm=TRUE)
cat111$ST[cat111$ST==6]='CA'
cat111$ST[cat111$ST==12]='FL'
cat111$ST[cat111$ST==36]='NY'
cat111$ST[cat111$ST==48]='TX'
cat111$NATIVITY[cat111$NATIVITY==1]='US Born'


cat22=group_by(Categories_FOR, Category, ST)
cat222=summarize(cat22, p=n(), na.rm=TRUE)
cat222$ST[cat222$ST==6]='CA'
cat222$ST[cat222$ST==12]='FL'
cat222$ST[cat222$ST==36]='NY'
cat222$ST[cat222$ST==48]='TX'
cat222$NATIVITY[cat222$NATIVITY==2]='Foreign Born'

income_US <- read.csv("C:/Users/student/Documents/St599Project/cat111.csv",stringsAsFactors = FALSE)
inc_US <- ggplot(income_US, aes(x=STATE,y=POPULATION, fill=Category))
inus=inc_US + geom_bar(stat='identity', position='dodge', alpha=0.5)

income_for <- read.csv("C:/Users/student/Documents/St599Project/cat222.csv",stringsAsFactors = FALSE)
inc_for <- ggplot(income_for, aes(x=STATE,y=POPULATION, fill=Category))
infor=inc_for + geom_bar(stat='identity', position='dodge', alpha=0.5)


#Grapgh overlappting the income of native and foreign boen individuals
library(reshape)
dat=data.frame(cat111,cat222)
inplot1=ggplot(dat, aes(x=ST,y=p,color=Category, fill=Category, alpha=0.5))+ geom_bar(position='dodge') + geom_bar(aes(x=ST.1,y=p.1, fill=Category, alpha=0.6),stat='identity', position='dodge') 
#inplot2=inplot1 + geom_bar(stat='identity')
inplot1+ opts(panel.background = theme_rect(fill='white'))+theme_bw()


#Plot for mean and median income comparison between states
plot(incc$STATE, incc$Q50, ylim=c(min(incc$Q25),70000),type="n",main="Income of US Born population by State",xlab="State",ylab="Income",xaxt="n")
mtext("Average and Median Income of US born with Interquartile Range Bars")
with (data = incc, expr = errbar(STATE, Q50, Q25, Q75, add=T, pch=1, cap=.1))
points(incc$STATE, incc$MEAN, col="red")
axis(1, at=incc$STATE,labels=c(incc$stat), col.axis="red", las=1,cex.axis=0.5)

