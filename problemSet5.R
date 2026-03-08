# SelectorGadget ####
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

# API ####
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