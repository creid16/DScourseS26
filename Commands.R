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
# scp "C:/Users/caleb/Downloads/PS7_Reid.R" "C:/Users/caleb/Downloads/PS7_Reid.tex" "C:/Users/caleb/Downloads/PS7_Reid.pdf" ouecon010@schooner.oscer.ou.edu:~/DScourseS26/ProblemSets/PS7/
# scp "C:/Users/caleb/Downloads/PS8_Reid.R" "C:/Users/caleb/Downloads/PS8_Reid.tex" "C:/Users/caleb/Downloads/PS8_Reid.pdf" ouecon010@schooner.oscer.ou.edu:~/DScourseS26/ProblemSets/PS8/
# scp "C:/Users/caleb/Downloads/PS9_Reid.R" "C:/Users/caleb/Downloads/PS9_Reid.tex" "C:/Users/caleb/Downloads/PS9_Reid.pdf" ouecon010@schooner.oscer.ou.edu:~/DScourseS26/ProblemSets/PS9/


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

# PS7 ####
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

# PS8 ####
# Load in libraries
{
library(nloptr)
library(tidyverse)
library(modelsummary)}

# Create data set for generating matrices
{
# Select random seed
set.seed(100)

# Fixed values
N <- 100000
K <- 10
sigma <- 0.5

# Create X matrix using normaly distributed random numbers
X <- matrix(rnorm(N*K),N,K)
# First column of X should be all ones
X[,1] <- 1 

# Set epsilon with a mean of 0 and standard deviation equal to sigma (normal distribution)
eps <- rnorm(N,mean=0,sd=sigma)

# True value of beta
beta <- c(1.5,-1,-0.25,0.75,3.5,-2,0.5,1,1.25,2)

# Calculate Y as a vector
Y <- X %*% beta + eps
}

# 5. OLS closed-form solution
{
# Calculate estimated beta using OLS
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y

# Compare estimated beta to actual beta
comparison <- cbind(beta, beta_hat)
colnames(comparison) <- c("True Beta", "Estimated Beta")

# Print comparison
comparison
}

# 6. OLS batch gradient descent
{
# set up a stepsize
alpha <- 0.0000003

# set up a number of iterations
maxiter <- 500000

# Define objective function
objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}

# Define the gradient of the objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

# Read in the data from above
y <- Y
X <- X

# Initial values
beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# set to same seed as before
set.seed(100)

# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),maxiter)

# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))
}

# 7a. OLS L-BFGS algorithm
{
# Our objective function
eval_f <- function(x) {
  return( sum((Y-X%*%x)^2) )
}

# Gradient of our objective function
eval_grad_f <- function(x) {
  return( as.vector(-2*t(X)%*%(Y-X%*%x)) )
}

# initial values
set.seed(100)
x0 <- runif(ncol(X))

# Algorithm parameters
opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6)

# Find the optimum!
res <- nloptr( x0=x0,eval_f=eval_f,eval_grad_f=eval_grad_f,opts=opts)

# Pull estimated betas
betahat  <- res$solution[1:(length(res$solution) - 1)]

# Print betahat
betahat
}

# 7b. OLS Nelder-Mead algorithm
{
# initial values
xstart <- x0

# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)

# Find the optimum!
res <- nloptr( x0=xstart,eval_f=eval_f,opts=options)
# Pull beta values
betahat  <- res$solution[1:(length(res$solution) - 1)]

# Print betahat
betahat
}

# 8. MLE estimation with L-BFGS
{
## Our objective function
objfun <- function(theta, y, X) {
  # slice parameter vector into beta and sigma
  beta <- theta[1:(length(theta) - 1)]
  sig  <- theta[length(theta)]
  
  # keep sigma positive
  if (sig <= 0) return(1e12)
  
  # negative log-likelihood
  loglike <- -sum(-0.5 * (log(2 * pi * (sig^2)) + ((y - X %*% beta) / sig)^2))
  return(loglike)
}

## Gradient of the objective function
gradient <- function(theta, y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta) - 1)]
  sig  <- theta[length(theta)]
  
  # keep sigma positive
  if (sig <= 0) {
    grad[] <- 1e12
    return(grad)
  }
  
  grad[1:(length(theta) - 1)] <- as.vector(-t(X) %*% (y - X %*% beta) / (sig^2))
  grad[length(theta)] <- length(y) / sig - as.numeric(crossprod(y - X %*% beta)) / (sig^3)
  
  return(grad)
}

## read in the data
y <- Y
X <- X

## initial values
# start from OLS for beta and sample sd of residuals for sigma
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% y
sigma_ols <- sqrt(mean((y - X %*% beta_ols)^2))

theta0 <- c(as.vector(beta_ols), sigma_ols)

## Algorithm parameters
options <- list(
  "algorithm" = "NLOPT_LD_LBFGS",
  "xtol_rel" = 1.0e-6,
  "maxeval" = 1e4
)

## Optimize!
result <- nloptr(
  x0 = theta0,
  eval_f = objfun,
  eval_grad_f = gradient,
  opts = options,
  y = y,
  X = X
)

# Pull estimated betas
betahat  <- result$solution[1:(length(result$solution) - 1)]

# Print betahat
betahat
}

# 9. OLS using linear model
{
# Fit linear model to data
lm_fit <- lm(Y ~ X - 1)

# Print summary table to latex file
modelsummary(
  lm_fit,
  output = "ols_results.tex"
)

# Print betas
summary(lm_fit)
}
# PS9 ####
library(tidyverse)
library(tidymodels)
library(glmnet)

set.seed(123456)

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names)
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#    13. LSTAT    lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's

housing_recipe <- recipe(medv ~ ., data = housing) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
                  ptratio:b:lstat:dis:nox) %>%
  # create square terms of some continuous variables
  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b,
            lstat,dis,nox, degree=6)

# Run the recipe
housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)

housing_train_prepped <- housing_prep %>% juice
housing_test_prepped  <- housing_prep %>% bake(new_data = housing_test)

# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select( medv)
housing_test_y  <- housing_test_prepped %>% select( medv)

#::::::::::::::::::::::::::::::::
# cross-validate the lambda with lasso
#::::::::::::::::::::::::::::::::
tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# Now train with tuned lambda
final_lasso <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print

top_rmse %>% 
  slice(1) %>% 
  select(penalty, mean)

# lambda was 0.00356
# in-sample RMSE was 0.0687
# out-of-sample RMSE is 0.173


#::::::::::::::::::::::::::::::::
# cross-validate the lambda with ridge
#::::::::::::::::::::::::::::::::
tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 0       #0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) #%>%
#add_recipe(housing_recipe)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# Now train with tuned lambda
final_ridge <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
last_fit(final_ridge, split = housing_split) %>%
  collect_metrics() %>% print

top_rmse %>% 
  slice(1) %>% 
  select(penalty, mean)

# lambda was 0.0233
# in-sample RMSE was 0.0687
# out-of-sample RMSE is 0.173
