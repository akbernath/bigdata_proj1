# minor modification; testing a commit
# hi

library(dplyr)
library(ggplot2)

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

# In this code, the error bars are fixed. This is basically what we want!
# (All it took was working out binomial proportion sd instead of sd().)
# ((I am not a clever man.))

# At this point a visualization or two more should suffice. I'll incorporate
# a state-based one (like Heather suggested), and additionally a brief one
# zooming in (ideally) to show veteran disability ratings in MS vs. AK.

# State:

