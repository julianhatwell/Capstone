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

# tuning parameters
lambdas <- c(0.6, 0.29, 0.99995, 0.00005)
rare <- 1
rare <- sum(txType[[1]] < v + 1)

trials <- 1
seed <- 12021

source("shinapp\\app_startup_interpolation.R")

pred_interp <- testPredText(trials, seed)
pred_interp$accuracy_1
pred_interp$accuracy_any

source("shinapp\\app_startup_backoff_naive.R")

pred_backoffn <- testPredText(trials, seed)
pred_backoffn$accuracy

source("shinapp\\app_startup_gtd.R")

pred_gtd <- testPredText(trials, seed)
pred_gtd$accuracy