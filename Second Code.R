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

# Normally the commented function would work beautifully; however, running Windows and
# having spaces in my working directory means I cannot run this function, and thus must
# cut everything manually. Awesome.
#
# cut.state <- function(state){
#   ddir <- paste(getwd(), "/data", sep="")
#   csv.cut <- paste(ddir, "/ss12p", state, "-cut.csv", sep="")
#   if(file.exists(csv.cut) == FALSE) {
#     csv.src <- paste(ddir, "/ss12p", state, ".csv", sep="")
#     shell(paste("cut -d, -f6,7,14,17,19,75,112,141", shQuote(csv.src), ">", shQuote(csv.cut), sep=" "))
#   }
# }
#
# Now it is just a matter of importing the data into data frames.
# (A loop is used here rather than the much more pleasant lapply method;
#  I coudn't quite figure out how to modify the workspace data from within
#  a function.)

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
                WAGP_SD =
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
  obj <- summarize(get(paste("table.", state, sep="")),
                   mean(WAGP_A), sd(WAGP_A), 
                   mean(PINCP_A),sd(PINCP_A),
                   mean(DOUT_A), sd(DOUT_A),
                   mean(DDRS_A), sd(DDRS_A))  
  return(obj)
}

i=0;
while (i < 51) {
  i=i+1;
  table.agg[i,2:9] <- sum.state(st.name[i])
}

# First, by proportions incapable of independent living:

order(table.agg$NotIndep)
indep.mean <- cbind(table.agg$NotIndep[order(table.agg$NotIndep)],
                    st.name[order(table.agg$NotIndep)])

indep.df <- data.frame(state = reorder(indep.mean[,2],order(as.numeric(indep.mean[,1]))),
                       mean = as.numeric(indep.mean[,1]),
                       se = table.agg$SdIndep[order(table.agg$NotIndep)])

limits <- aes(ymax= mean + se, ymin= mean-se)

# Time to make a graph!

qplot(y=mean, x=state, data=indep.df,
      main = "Proportion of veterans incapable of independent living",
      xlab="State", ylab="Proportion of surveyed veterans") +
      geom_bar(fill="white", colour="grey")

# Oh, hey, that looks pretty nice...

qplot(y=mean, x=state, data=indep.df,
      main = "Proportion of veterans incapable of independent living",
      xlab="State", ylab="Proportion of surveyed veterans") +
      geom_bar(fill="white", colour="grey") +
      geom_errorbar(limits, width=0.25)

# My entire life is pain.
#
# At this point, just repeating the process for each other variable and 
# asking the group tomorrow in class:

# Second, by proportions incapable of self-care:

order(table.agg$NotSelfCare)
care.mean <- cbind(table.agg$NotSelfCare[order(table.agg$NotSelfCare)],
                    st.name[order(table.agg$NotSelfCare)])

care.df <- data.frame(state = reorder(care.mean[,2],order(as.numeric(care.mean[,1]))),
                       mean = as.numeric(care.mean[,1]),
                       se = table.agg$SdCare[order(table.agg$NotSelfCare)])

limits <- aes(ymax= mean + se, ymin= mean-se)

# Time to make a graph!

qplot(y=mean, x=state, data=care.df,
      main = "Proportion of veterans experiencing difficulty with self-care",
      xlab="State", ylab="Proportion of surveyed veterans") +
  geom_bar(fill="white", colour="grey")

# And once again, with the error bars...

qplot(y=mean, x=state, data=care.df,
      main = "Proportion of veterans experiencing difficulty with self-care",
      xlab="State", ylab="Proportion of surveyed veterans") +
  geom_bar(fill="white", colour="grey") +
  geom_errorbar(limits, width=0.25)

# Life is pain.
#
# Third, average wages for veterans:

order(table.agg$AvgWage)
wage.mean <- cbind(table.agg$AvgWage[order(table.agg$AvgWage)],
                   st.name[order(table.agg$AvgWage)])

wage.df <- data.frame(state = reorder(wage.mean[,2],order(as.numeric(wage.mean[,1]))),
                      mean = as.numeric(wage.mean[,1]),
                      se = table.agg$SdWage[order(table.agg$AvgWage)])

limits <- aes(ymax= mean + se, ymin= mean-se)

# Time to make a graph!

qplot(y=mean, x=state, data=wage.df,
      main = "Average wages (adjusted) for veterans by state",
      xlab="State", ylab="Average wage (adjusted) in dollars") +
  geom_bar(fill="white", colour="grey")

# And once again, with the error bars...

qplot(y=mean, x=state, data=wage.df,
      main = "Average wages (adjusted) for veterans by state",
      xlab="State", ylab="Average wage (adjusted) in dollars") +
  geom_bar(fill="white", colour="grey") +
  geom_errorbar(limits, width=0.25)

# Yep.
#
# Finally, average incomes:

order(table.agg$AvgInc)
inc.mean <- cbind(table.agg$AvgInc[order(table.agg$AvgInc)],
                   st.name[order(table.agg$AvgInc)])

inc.df <- data.frame(state = reorder(inc.mean[,2],order(as.numeric(inc.mean[,1]))),
                      mean = as.numeric(inc.mean[,1]),
                      se = table.agg$SdInc[order(table.agg$AvgInc)])

limits <- aes(ymax= mean + se, ymin= mean-se)

# Time to make a graph!

qplot(y=mean, x=state, data=inc.df,
      main = "Average income (adjusted) for veterans by state",
      xlab="State", ylab="Average income (adjusted) in dollars") +
  geom_bar(fill="white", colour="grey")

# And once again, with the error bars...

qplot(y=mean, x=state, data=inc.df,
      main = "Average income (adjusted) for veterans by state",
      xlab="State", ylab="Average income (adjusted) in dollars") +
  geom_bar(fill="white", colour="grey") +
  geom_errorbar(limits, width=0.25)

# In some cases, we even can see that veterans are probably making more than 0 dollars yearly!
#
# Having done what I can, I'm pushing this to git.
# Hope to have this fixed within a few hours.