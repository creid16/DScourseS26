# Read in libraries
suppressMessages(library(dplyr))
suppressMessages(library(readr))
suppressMessages(library(hms))
suppressMessages(library(ggplot2))
suppressMessages(library(scales))
suppressMessages(library(stringr))
suppressMessages(library(gtExtras))





# Read in data
siegeData <- read_csv("ProblemSets/PS6/siegeData.csv")

# Column names
names(siegeData)

# Platforms used to play R6 Siege
unique(siegeData$platform)

# Playable maps in R6 Siege
unique(siegeData$mapname)

# Playable gamemodes in R6 Siege
unique(siegeData$gamemode)

# Average Duration of Rounds by Console
siegeData %>% 
  group_by(platform) %>% 
  summarize(n=n(),
            avgDuration=mean(roundduration,na.rm=T),
            duration=hms(seconds=avgDuration*69),
            .groups = "drop")

# Number of rounds played per map
plotData <- siegeData %>% 
  group_by(mapname,gamemode) %>% 
  summarize(n=n())

# Map Distirubtion by Gamemode Plot
plot <- plotData %>% 
  ggplot(aes(x=n,y=mapname))+
  geom_bar(aes(fill=gamemode),stat = "identity")+
  scale_x_continuous(labels=label_number())+
  labs(title="Map Count Distribution by Gamemode",
       x="Count",
       y="Map",
       fill="Gamemode")
ggsave("Map Distribution by Gamemode.png",plot)

# Avg kills per operator
plotData <- siegeData %>% 
  mutate(operator = str_extract(operator, "(?<=-).*")) %>% 
  group_by(operator) %>% 
  summarize(avgKills=mean(nbkills,na.rm=T),
            role=first(role)) %>%
  arrange(-avgKills) %>% 
  head(10)

# Avg KPR by Operator Plot
plot <- plotData %>% 
  ggplot(aes(x=reorder(operator,avgKills),y=avgKills))+
  geom_col(aes(fill=role))+
  labs(title="Avg Kills per Round (KPR) by Operator",
       x="Operator",
       y="Avg KPR",
       fill="Role")
ggsave("Avg KPR by Operator.png",plot)

# Avg Clearance Level by Rank
plotData <- siegeData %>% 
  group_by(skillrank) %>% 
  summarize(rounds=n_distinct(matchid),
            avgLevel=round(mean(clearancelevel,na.rm=T),0),
            .groups="drop") %>% 
  arrange(-avgLevel)

# Table for Avg Clearance Level by Rank
table <- plotData %>% 
  gt(rowname_col = "skillrank") %>% 
  cols_label(rounds = "Rounds Played",
             avgLevel = "Average Clearance Level") %>% 
  cols_align("center")

gtsave(table,"Avg Level by Operator.png")
