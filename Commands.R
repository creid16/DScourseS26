# OSCER ####

# login
# ssh ouecon010@schooner.oscer.ou.edu
# Sys.getenv("OSCER_SSH")

# move files to my fork w/ bash
# scp /c/Users/caleb/Downloads/PS3_Reid.tex /c/Users/caleb/Downloads/PS3_Reid.pdf ouecon010@schooner.oscer.ou.edu:~/DScourseS26/ProblemSets/PS3

# color files green if executable, blue if folder/directory
# chmod +x *batch

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

olympicTablePre <- webpage %>%
  html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(11)") %>%
  html_table(fill=T)

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
