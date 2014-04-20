# minor modification; testing a commit
# hi

# install.packages("dplyr")
library(dplyr)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("maps")
library(maps)
# install.packages("mapproj")
library(mapproj)

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

cut.state <- function(state){
  ddir <- paste(getwd(), "/data", sep="")
  csv.cut <- paste(ddir, "/ss12p", state, "-cut.csv", sep="")
  if(file.exists(csv.cut) == FALSE) {
    csv.src <- paste(ddir, "/ss12p", state, ".csv", sep="")
    system(paste("cut -d, -f6,7,14,17,19,75,112,141", sQuote(csv.src), ">", sQuote(csv.cut), sep=" "))
  }
}

lapply(st.name, cut.state)

i=0;
while (i < 51) {
  i=i+1;
  fsrc <- paste("./data/ss12p", st.name[i], "-cut.csv", sep="")
  assign(paste("table.", st.name[i], sep=""), read.csv(fsrc))
}

# And filtering out the information we are interested in:

filter.state <- function(state){
  obj <- filter(get(paste("table.", state, sep="")), (VPS >= 1 & VPS <= 15))
  return(obj)
}

i=0;
while (i < 51) {
  i=i+1;
  assign(paste("table.", st.name[i], sep=""), filter.state(st.name[i]))
}

# At this point, we are now working with 51 tables of veteran-only data.
# We adjust the incomes for yearly inflation and remove extraneous data:

sort.state <- function(state){
  obj.temp <- mutate(get(paste("table.", state, sep="")),
                WAGP_A = ADJINC * (WAGP / 1000000),
                PINCP_A = ADJINC * (PINCP / 1000000),
                DOUT_A = -1*(DOUT - 2),
                DDRS_A = -1*(DDRS - 2))
  obj <- select(obj.temp, -ADJINC, -ST, -WAGP, -PINCP, -DOUT, -DDRS)
  return(obj)
}

i=0;
while (i < 51) {
  i=i+1;
  assign(paste("table.", st.name[i], sep=""), sort.state(st.name[i]))
}

# Now, we begin Visualization/Inference!

table.agg <- data.frame(st.name, rep(0,51), rep(0,51), rep(0,51), rep(0,51),
                        rep(0,51), rep(0,51), rep(0,51), rep(0,51))
colnames(table.agg) <- c("State",  "AvgWage", "SdWage",
                         "AvgInc", "SdInc", 
                         "NotIndep", "SdIndep",
                         "NotSelfCare", "SdCare")

sum.state <- function(state){
  sd.work <- get(paste("table.", state, sep=""))[,6]
  sdcare <- sqrt(mean(sd.work)*(1-mean(sd.work))/length(sd.work))
  obj <- summarize(get(paste("table.", state, sep="")),
                   mean(WAGP_A), sd(WAGP_A), 
                   mean(PINCP_A),sd(PINCP_A),
                   mean(DOUT_A), sd(DOUT_A),
                   mean(DDRS_A), sdcare)  
  return(obj)
}

i=0;
while (i < 51) {
  i=i+1;
  table.agg[i,2:9] <- sum.state(st.name[i])
}

# By proportions incapable of self-care:

order(table.agg$NotSelfCare)
care.mean <- cbind(table.agg$NotSelfCare[order(table.agg$NotSelfCare)],
                    st.name[order(table.agg$NotSelfCare)])

care.df <- data.frame(state = reorder(care.mean[,2],order(as.numeric(care.mean[,1]))),
                       mean = as.numeric(care.mean[,1]),
                       se = table.agg$SdCare[order(table.agg$NotSelfCare)])

limits <- aes(ymax= mean + qnorm(0.975)*se, ymin= mean-qnorm(0.975)*se)

# Time to make a graph with error bars!

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
  scale_fill_gradient(low = "white", high = "red")

# At this point, it's missing Alaska and Hawaii. I'm going to get the
# veteran disability chart done first, make sure everything's prepared
# to send, and then spend a few hours checking out how to get them 
# incorporated (if possible).

drat.ms <- select(table.ms, -2,-3,-4,-5,-6)
drat.nd <- select(table.nd, -2,-3,-4,-5,-6)
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
drats.2[,4] <- rep(c(length(drat.msrm[,1]), length(drat.ndrm[,1])),6)

ggplot(drat.sum, aes(x=DR, fill=State)) +
  geom_bar(position="dodge")