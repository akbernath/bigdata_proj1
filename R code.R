library(dplyr)

options(stringsAsFactors = FALSE)

# PREPARATION FOR DATA MANIPULATION
# 0a) Download OR/TX 2012_5yr
#
# UNCOMMENT:
# download.file("http://www2.census.gov/acs2012_5yr/pums/csv_por.zip", 
#              destfile = "data/csv_por.zip")
# download.file("http://www2.census.gov/acs2012_5yr/pums/csv_ptx.zip", 
#              destfile = "data/csv_ptx.zip")

# 0b) Extract and read names (command prompt overhead)
#
# UNCOMMENT: 
# or_sm <- read.csv(unz("data/csv_por.zip", "ss12por.csv"), nrows = 10)
# names(or_sm)

# 1) Load cut data into R
or.time <- proc.time()
data.or <- read.csv("data/ss12por-cut.csv")
or.time <- proc.time() - or.time
or.time

tx.time <- proc.time()
data.tx <- read.csv("data/ss12ptx-cut.csv")
tx.time <- proc.time() - tx.time
tx.time

head(data.or)
head(data.tx)