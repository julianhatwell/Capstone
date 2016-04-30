library(profr)
library(tm)
library(dplyr)
source("utility_code2.R")
# load up the validation catalogues
en_US.val <- dataLoad(valDir)
val <- myPreProc(en_US.val, stem = FALSE, rmStops = FALSE)

# creating nGrams from val objects
val.tdm.1 <- createTDM(val, oneGramTK)
val.freqs.1 <- as.matrix(val.tdm.1)

val.tdm.2 <- createTDM(val, twoGramTK)
val.freqs.2 <- as.matrix(val.tdm.2)

val.tdm.3 <- createTDM(val, threeGramTK)
val.freqs.3 <- as.matrix(val.tdm.3)

val.tdm.4 <- createTDM(val, fourGramTK)
val.freqs.4 <- as.matrix(val.tdm.4)


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

trials <- 100
seed <- 12021
load("shinapp\\tdm_v4.Rdata")

source("shinapp\\app_startup_interpolation.R")

pred_interp <- testPredText(trials, seed)
profr(testPredText(trials, seed))
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