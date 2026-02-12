# OSCER ####

# login
# ssh ouecon010@schooner.oscer.ou.edu

# move files to my fork w/ bash
# scp /c/Users/caleb/Downloads/PS2_Reid.tex /c/Users/caleb/Downloads/PS2_Reid.pdf ouecon010@schooner.oscer.ou.edu:~/DScourseS26/

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

