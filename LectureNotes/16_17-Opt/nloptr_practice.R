library(nloptr)
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
print(res)





# initial values
xstart <- x0

# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)

# Find the optimum!
res <- nloptr( x0=xstart,eval_f=eval_f,opts=options)
print(res)
