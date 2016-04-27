library(parallel)
library(doParallel)

# initialise inner vars
trm.1.1 <- NULL
trm.2.1 <- NULL
trm.3.1 <- NULL
trm.4.1 <- NULL
p1 <- NULL
p2 <- NULL
p3 <- NULL
p4 <- NULL
adjusted.2 <- numeric(0)
adjusted.3 <- numeric(0)
adjusted.4 <- numeric(0)

# number of predictions to return
preds <- 3

# tuning parameters
lambdas <- c(0.6, 0.29, 0.99995, 0.00005)
rare <- 1
# rare <- sum(txType[[1]] < v + 1)

trials <- 1
seed <- 12021

runParTest <- function(f) {
  
  # set up parallel processing
  p_clus <- makeCluster(detectCores() - 1)
  registerDoParallel(p_clus)
  
  out <- f
  
  # close parallel processing
  stopCluster(p_clus)
  
  return(out)
}


source("shinapp\\app_startup_interpolation.R")

pred_interp <- testPredText(trials, seed)
pred_interp <- runParTest(testPredText(trials, seed))
pred_interp$lambdas <- lambdas
pred_interp$v <- v
pred_interp$rare <- rare
pred_interp$accuracy_1
pred_interp$accuracy_any

source("shinapp\\app_startup_backoff_naive.R")

pred_backoffn <- testPredText(trials, seed)
pred_backoffn$accuracy_1
pred_backoffn$accuracy_any

source("shinapp\\app_startup_gtd.R")

pred_gtd <- testPredText(trials, seed)
pred_gtd$accuracy_1
pred_gtd$accuracy_any