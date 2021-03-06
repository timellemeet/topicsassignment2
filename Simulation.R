# -----------------------------------------
# Author:
# Tim Ellemeet (425288)
# Erasmus Universiteit Rotterdam
# -----------------------------------------

## Use this empty file to implement your simulation study.  Please submit your 
## code together with your report.
library("mvtnorm")
library("clusterGeneration")
library("sigmoid")
library("doParallel") #for parallel
no_cores <- detectCores() - 1  

# Load R script with your implementation of the methods
source("Imputation.R")


# Put the code for your simulations below here

#generate the covariance matrix of the x variables
gen_x_cov <- function(n_x=3, max_cov=0.5){
  x_cov = genPositiveDefMat("unifcorrmat",dim=n_x, rangeVar =c(0,0.5))$Sigma
  diag(x_cov)= 1 
  
  return(x_cov)
}


#Select missing completely at random
gen_mcar <- function(x, n_missing){
  #draw a random sample and set NA
  x[sample(nrow(x)*ncol(x), n_missing)] = NA
  
  return(x)
}

#Select missing at random
gen_mar <- function(x, n_missing){
  #shift columns by one to make missing depend on size of right neighbour
  shifted = x[,c(2:ncol(x),1)]
  
  #construct a probabilty for NA matrix based on the scaled size (larger is higher prob)
  probs = sigmoid(shifted)
  probs[is.na(probs)] = 0
  
  #draw a random sample based on probabilites and set NA
  selection = sample(nrow(x)*ncol(x), n_missing, prob=probs)
  x[selection] = NA
  
  return(x)
}

#Select missing not at random
gen_mnar <- function(x, n_missing){
  #construct a probabilty for NA matrix based on the scaled size (larger is higher prob)
  probs = sigmoid(x)
  probs[is.na(probs)] = 0
  
  #draw a random sample based on probabilites and set NA
  selection = sample(nrow(x)*ncol(x), n_missing, prob=probs)
  x[selection] = NA
  
  return(x)
}

#Transform cells to outliers
gen_outliers <- function(x, n_outliers){
  #generate the outlier numbers
  mu = 2*sqrt(qchisq(.99, df=1))
  outliers = rnorm(n_outliers, mean = mu, sd = 0.1)
  
  #select cells to make outliers
  selection = sample(nrow(x)*ncol(x), n_outliers, prob=!is.na(x))
  x[selection] = outliers
  
  return(x)
}

#generate the dataset
gen_data <- function(n_obs, x_cov, mcar = 0, mar = 0, mnar = 0, outliers = 0, n_sets=1, y_cov = 1) {
  #check if parameters valid
  if((mcar + mar + mnar + outliers)>1) warning('Cannot contaminate more than complete data')
  
  n_col = 1+ncol(x_cov)
  
  #generate all data points
  y = rmvnorm(n_obs * n_sets, sigma = diag(y_cov))
  x = rmvnorm(n_obs * n_sets, sigma = x_cov)
  xy = cbind(x,y)
  
  #Alter data
  total_cells = n_obs * n_col
  xy = gen_mcar(xy, round(total_cells*mcar))
  xy = gen_mar(xy, round(total_cells*mar))
  xy = gen_mnar(xy, round(total_cells*mnar))
  xy = gen_outliers(xy, round(total_cells*outliers))
  
  #split return list of datasets 
  xy = split(xy, rep(1:n_sets, each = n_obs))
  xy = lapply(xy, matrix, ncol=n_col)
}

######

simulate <- function(x_cov, n_obs=100, n_simulations=100, mcar=0, mar=0, mnar=0, outliers=0, DDC=TRUE, m = 10, R=1000, k=5){
  #generate data
  data = gen_data(n_obs = n_obs, x_cov, mcar = mcar, mar=mar, mnar=mnar, outliers=outliers, n_sets=n_simulations)
  
  
  #define cluster for parallel imputation	
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  
  start_time <- Sys.time()
  
  #perform simulations
  simulations <- foreach(i = 1:n_simulations, .packages=c("VIM", "robustbase")) %dopar% {
    #load packages in each process
    source("Imputation.R")
    
    #select the data
    xy = data[[i]]
    
    #perform methods
    mi = multimp(xy, m = m, DDC = FALSE, imp_var = FALSE, robust = TRUE)
    mi_fit = fit(mi$imputed,  setting = "KS2014", robust=TRUE)
    mi_pool = pool(mi_fit$models)
    boot = bootstrap(xy, R=R, k=k, DDC = FALSE,  setting = "KS2014", robust=TRUE)$summary
    
    #also do DDC is asked
    if(DDC){
      ddc_mi = multimp(xy, m = m, DDC = FALSE, imp_var = FALSE, robust = TRUE)
      ddc_mi_fit = fit(ddc_mi$imputed,  setting = "KS2014", robust=TRUE)
      ddc_mi_pool = pool(ddc_mi_fit$models)
      ddc_boot = bootstrap(xy, R=R, k=k, DDC = FALSE,  setting = "KS2014", robust=TRUE)$summary
      
    } else {
      ddc_mi_pool = NA
      ddc_boot = NA
    }
    
    #return list of results
    return(list(
      mi = mi_pool,
      mi_ddc = ddc_mi_pool,
      boot = boot,
      boot_ddc = ddc_boot
    ))
  }
  
  print(Sys.time()-start_time)
  
  # free up processes	
  stopCluster(cl)
  
  
  #return
  return(simulations)
}


# set.seed(123)
# x_cov = gen_x_cov(n_x=3, max_cov=0.5)
# xy = gen_data(n_obs = 200, x_cov, mcar = 0.1, mar=0, mnar=0, outliers=0, n_sets=1)[[1]]
# 
# mi = multimp(xy,m=10, imp_var = FALSE, DDC=FALSE)
# fit = fit(mi$imputed)
# pool = pool(fit$models)
# print(pool)

######experimental design
set.seed(123)
x_cov = gen_x_cov(n_x=3, max_cov=0.5)

#config1 10% for each, each iter
c1 = diag(3) *0.1

#config2 40% for each, each iter
c2 = diag(3) *0.4

#combine configs
config = rbind(c1, c2)
colnames(config) = c("MCAR", "MAR", "MNAR")

results = list()

for(i in 1:nrow(config)) {
  R = 1000
  n_obs=200
  n_simulations=100
  outliers=0
  k=5
  DDC = FALSE
  
  ci = i # - 6 #correcetion for config files append
  

  #determine m
  m = config[ci,1] + config[ci,2] + config[ci,3]
  m = round(m*100)

  #run simulation
  simulation = simulate(x_cov, 
                        n_obs=n_obs,
                        n_simulations=n_simulations,
                        mcar=config[ci,1],
                        mar=config[ci,2],
                        mnar=config[ci,3],
                        outliers=outliers,
                        DDC=DDC,
                        m = m,
                        R=R,
                        k=k)
  
  #save with all information
  results[[i]] = list(
    n_obs=n_obs,
    n_simulations=n_simulations,
    mcar=config[ci,1],
    mar=config[ci,2],
    mnar=config[ci,3],
    outliers=outliers,
    DDC=DDC,
    m = m,
    R=R,
    k=k,
    x_cov=x_cov,
    simulation = simulation
  )
  
  #store to disk after each iteration
  save(results, file = paste(c("results no outliers after ", i, ".RData"), collapse = ""))
}


####### Determining baseline results

n_obs=200
n_simulations=100

clean_data = gen_data(n_obs = 200, x_cov, mcar = 0, mar=0, mnar=0, outliers=0, n_sets=n_simulations)
base = fit(clean_data,  setting = "KS2014", robust=TRUE)

results[[13]] = list(
  n_obs=n_obs,
  n_simulations=n_simulations,
  mcar=0,
  mar=0,
  mnar=0,
  outliers=0,
  DDC=FALSE,
  m = 0,
  R=0,
  k=0,
  x_cov=x_cov,
  simulation = base$models
)

#store to disk after each iteration
save(results, file = "full results.RData")
