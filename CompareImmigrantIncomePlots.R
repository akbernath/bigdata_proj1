#Helping Nandhita with plots 

native<-read.csv("/Users/heatherhisako1/Downloads/INCOME.csv",header=TRUE)
foreign<-read.csv("/Users/heatherhisako1/Downloads/INCOME2.csv",header=TRUE)
head(native)
head(foreign)
dim(native)
dim(foreign)
tail(foreign)
native.med=native$Q50
native.mean=native$MEAN
native.25=native$Q25
native.75=native$Q75
for.med=foreign$Q50
for.mean=foreign$MEAN
for.25=foreign$Q25
for.75=foreign$Q75
state=as.character(native$stat)

ndata<-cbind(native.med,native.mean,native.25,native.75,for.med,for.mean,for.25,for.75,state)
head(ndata)

new.ndata<-ndata[order(as.numeric(native.med)),]
head(new.ndata)
new.ndata

ord.nat.med<-as.numeric(new.ndata[,1])
ord.nat.mean<-as.numeric(new.ndata[,2])
ord.nat.q25<-as.numeric(new.ndata[,3])
ord.nat.q75<-as.numeric(new.ndata[,4])
ord.for.med<-as.numeric(new.ndata[,5])
ord.for.mean<-as.numeric(new.ndata[,6])
ord.for.q25<-as.numeric(new.ndata[,7])
ord.for.q75<-as.numeric(new.ndata[,8])
ord.states<-new.ndata[,9]


install.packages("Hmisc", dependencies=T)
library("Hmisc")
x=1:52
d = data.frame(
  x  = 1:52
  , native_med  = ord.nat.med
  , native_mean=ord.nat.mean
  , native_q25=ord.nat.q25
  ,native_q75=ord.nat.q75
  ,w=x+0.3
  ,foreign_med  = ord.for.med
  ,foreign_mean=ord.for.mean
  ,foreign_q25=ord.for.q25
  ,foreign_q75=ord.for.q75
)

plot(d$x, d$native_med, ylim=c(min(d$native_q25,d$foreign_q25),max(d$native_q75,d$foreign_q75)),type="n",main="Native vs Foreign Median Income",xlab="State",ylab="Median Income",xaxt="n")
mtext("Median Income with Interquartile Range Bars")
with (
  data = d
  , expr = errbar(x, native_med, native_q25, native_q75, add=T, pch=1, cap=.02,errbar.col="orange",col="orange")
)
with (
  data = d
  , expr = errbar(w, foreign_med, foreign_q25, foreign_q75, add=T, pch=1, cap=.02,errbar.col="blue",col="blue")
)
axis(1, at=d$x,labels=c(ord.states), col.axis="red", las=1,cex.axis=0.5)
legend(locator(1), c("Native Income","Foreign Income"),pch = c(1,1), col=c("orange","blue"),bty="n")

