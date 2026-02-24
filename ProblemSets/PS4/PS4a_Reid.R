# retrieve data from url
system('wget -O ~/events.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

# print data
system('cat ~/events.json)

# load libraries
library(tidyverse)
library(jsonlite)

# convert JSON to list
mylist <- fromJSON("~/events.json", flatten = TRUE)

# convert list to data frame
mydf <- bind_rows(mylist$result[-1])

# get class of date column
class(mydf$date)

# print out first few rows
head(mydf)
