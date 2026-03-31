# Libraries
library(mice)
library(modelsummary)
library(readr)
library(dplyr)

# Read in csv
wages <- read_csv("ProblemSets/PS7/wages.csv")

# Calculate missing data points for each feature
naCount <- wages %>% 
  summarize(across(everything(), ~sum(is.na(.))))

# Remove rows where hgc or tenure is missing and update the df
wages <- wages %>% 
  filter(!is.na(hgc) & !is.na(tenure))

# Count missing data points after removal
naCount2 <- wages %>% 
  summarize(across(everything(), ~sum(is.na(.))))

# Summary table outputted to LaTeX
table <- datasummary_skim(wages,"latex")

# Listwise deletion of missing logwage
modelData1 <- wages %>% filter(!is.na(logwage))

# Listwise deletion model
lm1 <- lm(logwage ~ hgc+college+tenure+tenure^2+age+married,modelData1)

# Compute mean wages 
meanLogwage <- mean(wages$logwage,na.rm=T)

# Mean imputation of logwage
modelData2 <- wages %>% 
  mutate(logwage=if_else(is.na(logwage),meanLogwage,logwage))

# Mean imputation model
lm2 <- lm(logwage ~ hgc+college+tenure+tenure^2+age+married,modelData2)

# Impute missing logwage with predicted logwage from listwise deletion model (lm1)
modelData3 <- wages %>% 
  mutate(predLogwage=predict(lm1,.),
         logwage=if_else(is.na(logwage),predLogwage,logwage)) %>% 
  select(-predLogwage)

# Predicted values model
lm3 <- lm(logwage ~ hgc+college+tenure+tenure^2+age+married,modelData3)

# Data for mice model
modelData4 <- wages

# Impute with `mice`
wagesMice <- mice(modelData4, m = 5, printFlag = FALSE)

# Mice model
lm4 <- with(wagesMice, lm(logwage ~ hgc+college+tenure+tenure^2+age+married)) 

# Create empty list to store models
models <- list()

# Put models inside list
models[['Listwise Deletion']] <- lm1
models[['Mean Imputation']] <- lm2
models[['Predicted Values']] <- lm3
models[['Mice']] <- mice::pool(lm4)

# Create summary table of the four models
modelsummary(models)