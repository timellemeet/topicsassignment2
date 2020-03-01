# -----------------------------------------
# Author:
# Tim Ellemeet (425288)
# Erasmus Universiteit Rotterdam
# -----------------------------------------

## Use this code skeleton to implement the procedures for obtaining point
## estimates and valid standard errors via multiple imputation or the 
## bootstrap.  Please submit your implementation together with your report.

## IMPORTANT: Please do not change any function names and make sure that you
##            use the correct input and output as specified for each function.
##            This will simplify grading because each student's code will be
##            similarly structured.  However, feel free to add other arguments
##            to the function definitions as needed.



library("VIM")
library("cellWise")  
library("robustbase")
library("doParallel")  
no_cores <- detectCores() - 1  

## Functions for multiple imputation via iterative model-based imputation


# Multiple imputation
# 
# Input:
# xy    data set with missing values
# m     number of imputations
# DDC   a logical indicating whether to run the DetectDeviatingCells algorithm
#       before performing multiple imputation (such that the flagged outlying 
#       cells are also imputed)
# ...   additional arguments to be passed to function irmi() from package VIM
#       (for example, whether to use robust models)
# 
# Output:
# A list with the following components:
# imputed   this should again be a list in which each list element is one 
#           imputed data set
# m         the number of imputations
# any other information you want to keep



multimp <- function(xy, m = 20, DDC = FALSE, ...) {
  # You should set a sensible default for the number of imputations m.
  #
  # You can use function DDC() from package cellWise, as well as function 
  # irmi() from package VIM for imputations.

  if(DDC){
    #find possible outliers
    outliers = DDC(xy, DDCpars = list(silent=TRUE))$indall
    
    #set outliers NA
    xy[outliers] = NA
  }
  
  #filter out full nan rows
  n_col = ncol(xy)
  xy = xy[apply(xy, 1, function(x) !all(is.na(x)))]
  xy = matrix(xy, ncol = n_col)

  #generate m imputed datasets
  imputed <- foreach(i = 1:m, .packages='VIM') %do% {
    #irmi options are set in simulation study for flexiblity!
    return(irmi(xy,...))
  }

  return(list(
    imputed = imputed,
    m=m,
    irmi=list(...)))
}




# Fit regression models
# 
# Input:
# xyList   list of imputed data sets as returned by function multimp()
# ...      additional arguments to be passed to modeling function (for example,
#          control parameters for the MM-algorithm)
# 
# Output:
# A list with the following components:
# models   this should again be a list in which each list element is a 
#          regression model (fitted to the corresponding imputed data set)
# m        the number of imputations
# any other information you want to keep

fit <- function(xyList, ...) {
  # You can use function lmrob() from package robustbase for the MM-estimator.
  
  m = length(xyList)

  #estimate all models
  models <- foreach(i = 1:m, .packages='robustbase') %do% {
    xy = xyList[[i]]
    X = as.matrix(xy[,-ncol(xy)])
    y = xy[,ncol(xy)]
    return(lmrob(y ~ X, ...))
  }

  return(list(
    models = models,
    m=m,
    lmrob=list(...)))
  
  
}


# Pool point estimates and standard errors
#
# Input:
# fitList  a list as returned by function fit() containting the regression 
#          models fitted to the imputed data sets
# ...      additional arguments you may need to pass down to other functions
#
# Output:
# A matrix that contains the pooled coefficients in the first column, the
# pooled standard errors in the second column, the t-statistic in the third
# column, the estimated degrees of freedom in the fourth column, and the
# p-value in the fifth column (see slide 50 of Lecture 5 for an example)

pool <- function(fitList, ...) {

}



## Function for the bootstrap with kNN imputation and linear regression


# Input:
#
# x     data set with missing values
# R     number of bootstrap replications
# k     number of neighbors for kNN imputation
# DDC   a logical indicating whether to run the DetectDeviatingCells algorithm
#       before performing imputation (such that the flagged outlying cells are 
#       also imputed)
# ...   additional arguments to be passed to modeling function (for example,
#       control parameters for the MM-algorithm)
#
# Output:
# A list with the following components:
# replicates   a matrix with all coefficient estimates from all replications
# summary      a matrix that contains the point estimates of the coefficients 
#              in the first column, the standard errors in the second column, 
#              the z-statistic in the third column, and the p-value in the 
#              fourth column (see slide 29 of Lecture 5 for an example)

bootstrap <- function(x, R, k, DDC = FALSE, ...) {
  # You should set a sensible default for the number of bootstrap replicates R 
  # and the number of neighbors k.
  #
  # You can use function DDC() from package cellWise, function kNN() from 
  # package VIM for imputations, and function lmrob() from package robustbase 
  # for the MM-estimator.
}


source("Simulation.R")
set.seed(123)
m = 50
x_cov = gen_x_cov(n_x=3, max_cov=0.5)
xy = gen_data(n_obs = 500, x_cov, mcar = 0.1, mar=0, mnar=0, outliers=0, n_sets=m)
xy = xy[[1]]

start_time1 <- Sys.time()
mi = multimp(xy,m=50, imp_var = FALSE, DDC=FALSE)
end_time1 <- Sys.time()

start_time2 <- Sys.time()
fit = fit(mi$imputed)
end_time2 <- Sys.time()

print(fit)
print(end_time1 - start_time1)
print(end_time2 - start_time2)

# Time difference of 3.909026 secs
# Time difference of 2.586185 secs
