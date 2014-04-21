# install.packages("dplyr")
library(dplyr)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("maps")
library(maps)

options(stringsAsFactors = FALSE)

# 1) DOWNLOAD DATA
#
# This code (inspired by Heather's, and used with her permission) 
# allows for automated downloading of all 50 states' worth of data.
# Additionally, it will only download any missing data.

st.name <- c(tolower(state.abb), "dc")

dl.state <- function(state){
  dest <- paste("./data/data_", state, ".zip", sep="")
  if (file.exists(dest) == FALSE) {
    url <- paste("http://www2.census.gov/acs2012_5yr/pums/csv_p", state, ".zip", sep="")
    download.file(url, dest)    
  }
}

lapply(st.name, dl.state)

# We will need the following columns preserved:
#     6: ST
#         (State; for identifying which data tables)
#     7: ADJINC
#         (Adjusted income; for comparing on same scale)
#    14: DDRS
#         (Self-care difficulty)
#    17: DOUT
#         (Independent-living difficulty)
#    19: DRAT
#         (Veteran service connected disability rating)
#    75: WAGP
#         (Wages or salary income, past 12 months) [ADJUST]
#   112: PINCIP
#         (Total person's income) [ADJUST]
#   141: VPS
#         (For identifying veteran status)
#   
# 2) GET DATA INTO R
#
# With this established, we may do command-line work on cutting out unnecessary sections.
# We begin by unzipping:

unzip.state <- function(state){
  dest <- paste("./data/ss12p", state, ".csv", sep="")
  if(file.exists(dest) == FALSE) {
    found <- paste("./data/data_", state, ".zip", sep="")
    unzip(found, exdir="./Data")
  }
}

lapply(st.name, unzip.state)

# A NOTE: while running it does no harm on any machine, the following
# function will only work on an UNIX-based OS; after much searching,
# I gave up on trying to find a way to make it work in Windows.
# (I instead manually cut all data using cygwin.)

# (The closest I got was by calling:
#       system(paste("c:/utilities/cygwin64/bin/cut.exe", " -d, -f6,14,19,141",
#        sQuote(csv.src), ">", sQuote(csv.cut), sep=" ")),
# but that started running into issues thanks to (as far as I can tell) 
# certain things endemic to cygwin. Running R through cygwin seems to also
# be an option, but that runs into its own problems.)

cut.state <- function(state){
  ddir <- paste(getwd(), "/data", sep="")
  csv.cut <- paste(ddir, "/ss12p", state, "-cut.csv", sep="")
  if(file.exists(csv.cut) == FALSE) {
    csv.src <- paste(ddir, "/ss12p", state, ".csv", sep="")
    system(paste("cut -d, -f14,19,141", sQuote(csv.src), ">", sQuote(csv.cut), sep=" "))
  }
}

lapply(st.name, cut.state)

# This loop reads all the cut datasets into a table; I attempted to
# do this through a function, but assigning variables in a function
# is a bit more complicated to get through than it was worth.

i=0;
while (i < 51) {
  i=i+1;
  fsrc <- paste("./data/ss12p", st.name[i], "-cut2.csv", sep="")
  assign(paste("table.", st.name[i], sep=""), read.csv(fsrc))
}

# And filtering out the information we are interested in:

filter.state <- function(state){
  obj.temp <- filter(get(paste("table.", state, sep="")), (VPS >= 1 & VPS <= 15))
  obj <- select(obj.temp, -VPS)
  return(obj)
}

i=0;
while (i < 51) {
  i=i+1;
  assign(paste("table.", st.name[i], sep=""), filter.state(st.name[i]))
}

# At this point, we are now working with 51 tables of veteran-only data.
# Additionally, we may now focus entirely on DDRS and DRAT :
# Difficulty of self-care and VA disability rating.

# Next, we change the "1 if trouble/2 if not" model to a "0 if not/1 if yes":

sort.state <- function(state){
  obj.temp <- mutate(get(paste("table.", state, sep="")),
                DDRS_A = -1*(DDRS - 2))
  obj <- select(obj.temp, -DDRS)
  return(obj)
}

i=0;
while (i < 51) {
  i=i+1;
  assign(paste("table.", st.name[i], sep=""), sort.state(st.name[i]))
}

# Now, we aggregate:

table.agg <- data.frame(st.name, rep(0,51), rep(0,51))
colnames(table.agg) <- c("State", "NotSelfCare", "SdCare")

sum.state <- function(state){
  sd.work <- get(paste("table.", state, sep=""))[,2]
  sdcare <- sqrt(mean(sd.work)*(1-mean(sd.work))/length(sd.work))
  obj <- summarize(get(paste("table.", state, sep="")),
                   mean(DDRS_A), sdcare)  
  return(obj)
}

i=0;
while (i < 51) {
  i=i+1;
  table.agg[i,2:3] <- sum.state(st.name[i])
}

# By proportions incapable of self-care:

care.mean <- cbind(table.agg$NotSelfCare[order(table.agg$NotSelfCare)],
                    st.name[order(table.agg$NotSelfCare)])

care.df <- data.frame(state = reorder(care.mean[,2],order(as.numeric(care.mean[,1]))),
                       mean = as.numeric(care.mean[,1]),
                       se = table.agg$SdCare[order(table.agg$NotSelfCare)])

# And we begin visualization.
# Time to make a graph with error bars!
# (It didn't make the cut, but it is reproduced here for posterity.)

limits <- aes(ymax= mean + qnorm(0.975)*se, ymin= mean-qnorm(0.975)*se)

qplot(y=mean, x=state, data=care.df,
      main = "Proportion of veterans experiencing difficulty with self-care",
      xlab="State", ylab="Proportion of surveyed veterans") +
  geom_bar(fill="#FF9999", colour="#222222") +
  geom_errorbar(limits, width=0.25)

# At this point a visualization or two more should suffice. I'll incorporate
# a state-based one (like Heather suggested and used), and additionally a brief one
# zooming in (ideally) to show veteran disability ratings in MS vs. AK/ND.

# State:
 
map.st <- map_data("state")
table.agg2 <- table.agg
table.agg2[,1] <- tolower(c(state.name, "District of Columbia"))
ggplot(table.agg2, aes(map_id = State),
       main="Proportion of Veterans Capable of Self-Care (by state)",
       xlab="x (Longitude)", ylab="y (Latitude)") +
  geom_map(aes(fill = NotSelfCare), map = map.st, color="black") +
  expand_limits(x = map.st$long, y = map.st$lat) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  ggtitle("Proportion of veterans experiencing difficulty with self care") +
  theme(plot.title = element_text(size = rel(1.5))) + theme_minimal()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

# At this point, it's missing Alaska and Hawaii. I'm going to get the
# veteran disability chart done first, make sure everything's prepared
# to send, and then spend a few hours checking out how to get them 
# incorporated (if possible).

drat.ms <- select(table.ms, -2)
drat.nd <- select(table.nd, -2)
drat.msrm <- na.omit(drat.ms)
drat.ndrm <- na.omit(drat.nd)

num <- numeric(36)
drat <- matrix(num, ncol=3)
drat <- as.data.frame(drat)
colnames(drat) <- c("Proportion", "Response", "State")

drat[1,2] <- "10 Percent"
drat[7,2] <- "10 Percent"
drat[2,2] <- "20 Percent"
drat[8,2] <- "20 Percent"
drat[3,2] <- "30-40 Percent"
drat[9,2] <- "30-40 Percent"
drat[4,2] <- "50-60 Percent"
drat[10,2] <- "50-60 Percent"
drat[5,2] <- "70-100 Percent"
drat[11,2] <- "70-100 Percent"
drat[6,2] <- "Elected not to answer"
drat[12,2] <- "Elected not to answer"

drat[,3] <- c(rep("North Dakota",6), rep("Mississippi",6))
i = 0; while (i < 6) {
  drat[i+1,1] <- length(which(drat.ndrm[,1]==i+1)) / length(drat.ndrm[,1])
  i = i+1;
}
i = 0; while (i < 6) {
  drat[i+7,1] <- length(which(drat.msrm[,1]==i+1)) / length(drat.msrm[,1])
  i = i+1;
}

ggplot(data=drat,
       aes(x=factor(1), y=Proportion, fill = factor(Response))) +
  geom_bar(width = 1) + facet_grid(facets=. ~ State) +
  coord_polar(theta="y") + xlab("") + ylab("") +
  labs(fill="Proportion", title="Veteran Disability Rating")

# This pie chart, while imperfect, does at least give us some concise
# info. I'm attaching it just in case, with the acknowledgment that we
# probably won't want to use it (especially with four members presenting!)

# Now to convert it to simultaneous bar plots...

drat.msrm.2 <- cbind(drat.msrm, rep("Mississippi", length(drat.msrm[,1])))
colnames(drat.msrm.2) <- c("DR", "State")
drat.ndrm.2 <- cbind(drat.ndrm, rep("North Dakota", length(drat.ndrm[,1])))
colnames(drat.ndrm.2) <- c("DR", "State")
drat.rm <- rbind(drat.msrm.2,drat.ndrm.2)

drat.rm[drat.rm == 1] <- "10 percent"
drat.rm[drat.rm == 2] <- "20 percent"
drat.rm[drat.rm == 3] <- "30-40 percent"
drat.rm[drat.rm == 4] <- "50-60 percent"
drat.rm[drat.rm == 5] <- "70-100 percent"
drat.rm[drat.rm == 6] <- "Elected not to answer"

drat.g <- group_by(drat.rm, DR, State)
drats.1 <- summarize(drat.g, count= n())
drats.1[,4] <- rep(c(length(drat.msrm[,1]), length(drat.ndrm[,1])),6)
drat.sum <- summarise(group_by(drats.1,DR, State), Proportion = count/V4)

ggplot(drat.sum, aes(x=DR, y=Proportion, fill=State)) +
  geom_bar(position="dodge") +
  ggtitle("VA Disability Ratings within MS and ND (by proportion)") +
  xlab("VA Disability Rating") +
  theme(plot.title = element_text(size = rel(1.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())