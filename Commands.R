# OSCER ####

# login
# ssh ouecon010@schooner.oscer.ou.edu
# Sys.getenv("OSCER_SSH")

# move files to my fork w/ bash
# scp /c/Users/caleb/Downloads/PS4_Reid.tex /c/Users/caleb/Downloads/PS4_Reid.pdf ouecon010@schooner.oscer.ou.edu:~/DScourseS26/ProblemSets/PS4
# scp "C:/Users/caleb/Downloads/PS4_Reid.tex" "C:/Users/caleb/Downloads/PS4_Reid.pdf" ouecon010@schooner.oscer.ou.edu:~/DScourseS26/ProblemSets/PS4/
# scp "C:/Users/caleb/Downloads/PS5_Reid.R" "C:/Users/caleb/Downloads/PS5_Reid.tex" "C:/Users/caleb/Downloads/PS5_Reid.pdf" ouecon010@schooner.oscer.ou.edu:~/DScourseS26/ProblemSets/PS4/
# color files green if executable, blue if folder/directory
# chmod +x *batch

# scp "C:/Users/caleb/Downloads/PS6_Reid.R" "C:/Users/caleb/Downloads/PS6_Reid.tex" "C:/Users/caleb/Downloads/PS6_Reid.pdf" "C:/Users/caleb/Downloads/PS6a_Reid.png" "C:/Users/caleb/Downloads/PS6b_Reid.png" "C:/Users/caleb/Downloads/PS6c_Reid.png"  ouecon010@schooner.oscer.ou.edu:~/DScourseS26/ProblemSets/PS6/


# SQLite ####
suppressMessages(library(tidyverse))
suppressMessages(library(sqldf))

df <- iris %>% as_tibble()

sqldf('SELECT count(*) FROM df WHERE Species = "virginica"')

# store the output:
counted <- sqldf('SELECT count(*) FROM df WHERE Species = "virginica"')

# dplyr
counted.dplyr <- df %>% filter(Species=="virginica") %>% count %>% print()

# check if results are same
identical(counted[[1]],counted.dplyr[[1]])

florida <- read_csv("/ProblemSets/PS3/FL_insurance_sample.csv")

# CREATE TABLE florida(
# "county" CHAR,
# "tiv_2011" INTEGER,
# "tiv_2012" INTEGER,
# "construction" CHAR,
# )

# Purrr ####
suppressMessages(library(purrr))

mtcars  %>%  
  split(mtcars$cyl) %>%  # from base R
  map(\(df) lm(mpg ~ wt, data = df)) %>% 
  map(summary) %>%
  map_dbl("r.squared")

mtcars %>%
  group_by(cyl) %>%
  summarize(r_squared=summary(lm(mpg~wt,data=cur_data()))$r.squared,
    .groups = "drop")

# Rvest ####
suppressMessages(library(rvest))
suppressMessages(library(dplyr))

url <- "http://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression"

webpage <- read_html(url)

olympicTable1976 <- webpage %>%
  html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(17)") %>%
  html_table(fill=T)

olympicTable1977 <- webpage %>% 
  html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(23)") %>% 
  html_table(fill=T)

olympicTable <- bind_rows(olympicTablePre,olympicTable1976,olympicTable1977)

names(olympicTable)

# Polite ####
suppressMessages(library(polite))
suppressMessages(library(janitor))


session <- bow(url,force=T)

result <- scrape(session,query=list(t="semi-soft",per_page=100)) %>% 
  html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(17)") %>%
  html_table(fill=T)


# Fredr ####
## Load and install the packages that we'll be using today

pacman::p_load(tidyverse, httr, lubridate, janitor, jsonlite, fredr, 
               listviewer, usethis)

endpoint <- "https://data.cityofnewyork.us/resource/nwxe-4ae8.json?$limit=68000"

nyc_trees <- fromJSON(endpoint) %>%
  as_tibble()

nyc_trees %>% 
  select(longitude, latitude, stump_diam, spc_common, spc_latin, tree_id) %>% 
  mutate_at(vars(longitude:stump_diam), as.numeric) %>% 
  ggplot(aes(x=longitude, y=latitude, size=stump_diam)) + 
  geom_point(alpha=0.5) +
  scale_size_continuous(name = "Stump diameter") +
  labs(
    x = "Longitude", y = "Latitude",
    title = "Sample of New York City trees",
    caption = "Source: NYC Open Data"
  )

names(nyc_trees)

head(nyc_trees)

# head(nyc_trees) %>% select(tree_id:health)

# head(nyc_trees) %>% select(spc_latin:user_type)

params = list(
  api_key= Sys.getenv("FRED_API_KEY"), ## Get API directly and safely from the stored environment variable
  file_type="json", 
  series_id="GNPCA"
)

df <- fredr(
  series_id="GNPCA",
  observation_start=as.Date("1929-01-01"),
  observation_end=as.Date("2026-01-01")
)

df <- fredr(
  series_id="GNPCA",
  observation_start=as.Date("1929-01-01"),
  observation_end=as.Date("2026-01-01"),
  frequency="a",
  units="chg"
)

df %>%
  ggplot(aes(date, value)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x="Date", y="2012 USD (Billions)",
    title="US Real Gross National Product", caption="Source: FRED"
  )

fred = 
  httr::GET(
    url = "https://api.stlouisfed.org/", ## Base URL
    path = paste0("fred/", endpoint),    ## The API endpoint
    query = params                       ## Our parameter list
  )



# PS4 ####
# ~/bin/Rbatch PS4a_Reid.R events_output.log 1:00 redacted@ou.edu
# PS5 ####
# SelectorGadget 
# Libraries
suppressMessages(library(rvest))
suppressMessages(library(dplyr))

# Read url from ESPN.com
url <- "https://www.espn.com/mens-college-basketball/stats/player"

# Convert htm
webpage <- read_html(url)

# Create tables
tables <- webpage %>% 
  html_elements(".ResponsiveTable table") %>% 
  html_table(fill = TRUE)

# Extract data
left  <- tables[[1]]
right <- tables[[2]]

# Combine data
ppgTable <- bind_cols(left, right)

# Print data frame
ppgTable

# API 
# Libraries
library(httr)
library(jsonlite)

# Read endpoint from ESPN's API
url <- "https://site.api.espn.com/apis/v2/sports/basketball/nba/standings"

# Read url
response <- GET(url)

# Convert data
data <- fromJSON(content(response, "text"), simplifyVector = FALSE)

# Eastern Conference teams
east <- data$children[[1]]$standings$entries

# Western Conference teams
west <- data$children[[2]]$standings$entries

# Create data frames
eastTeams <- sapply(east, function(x) x$team$displayName)
westTeams <- sapply(west, function(x) x$team$displayName)

# Transform data
parse_entries <- function(entries, conf){
  do.call(rbind, lapply(entries, function(x){
    data.frame(
      team = x$team$displayName,
      abbrev = x$team$abbreviation,
      conference = conf
    )
  }))
}

# New tables
east_df <- parse_entries(east, "East")
west_df <- parse_entries(west, "West")

# Combined data frame
standings <- rbind(east_df, west_df)

# Print data frame
standings

# Nloptr ####
library(nloptr)
alpha <- 0.003

iter <- 500

gradient <- function(x) return((4*x^3) - (9*x^2))

set.seed(100)

x <- floor(runif(1)*10)

x.All <- vector("numeric",iter)

for(i in 1:iter) {
  x <- x - alpha*gradient(x)
  x.All[i] <- x
  print(x)
}

print(paste("The minimun of f(x) is ", gradient(x), sep=""))

# Five inputs needed for nloptr
# 1. Objective function
# 2. Gradient vector of the objective function
# 3. Algorithm
# 4. Initial value
# 5. Tolerance parameters
