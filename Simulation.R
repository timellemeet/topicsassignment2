# -----------------------------------------
# Author:
# Tim Ellemeet (425288)
# Erasmus Universiteit Rotterdam
# -----------------------------------------

## Use this empty file to implement your simulation study.  Please submit your 
## code together with your report.
library("mvtnorm")
library("clusterGeneration")

# Load R script with your implementation of the methods
#source("Imputation.R")


# Put the code for your simulations below here

#Generate missing completely at random
gen_mcar <- function(x, n_missing){
  x
}

#Generate missing at random
gen_mar <- function(x, n_missing){
  x
}

#Generate missing not at random
gen_mnar <- function(x, n_missing){
  x
}

#Transform cells to outliers
gen_outliers <- function(x, n_outliers){
  x
}

#generate the dataset
gen_data <- function(n_obs, n_x, x_cov, mcar = 0, mar = 0, mnar = 0, outliers = 0, n_sets=1, y_cov = 1) {
  #generate all data points
  y = rmvnorm(n_obs * n_sets, sigma = diag(y_cov))
  x = rmvnorm(n_obs * n_sets, sigma = x_cov)
  xy = cbind(x,y)
  
  
  #Alter data
  xy = gen_mcar(xy, round(n_obs*mcar))
  xy = gen_mar(xy, round(n_obs*mar))
  xy = gen_mnar(xy, round(n_obs*mnar))
  xy = gen_outliers(xy, round(n_obs*outliers))
  
  #split return list of datasets 
  split(xy, rep(1:n_sets, each = n_obs))
}

set.seed(123)
x_cov = genPositiveDefMat("unifcorrmat",dim=p, rangeVar =c(0,0.5))$Sigma
diag(x_cov)= 1 
data = gen_data(n_obs = 10, n_x = 1, x_cov, n_sets=1)
print(data)
