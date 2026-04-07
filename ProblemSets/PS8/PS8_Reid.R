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