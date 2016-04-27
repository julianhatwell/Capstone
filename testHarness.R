library(tm)
library(dplyr)
source("env_consts.R")
source("common_functions.R")

# load up the app objects
load("shinapp\\tdm.Rdata")

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

# set the algo of choice, manually

testPredText <- function(trials = 10, sd = 12021) {
  res <- matrix(nrow = 0, ncol = 19)
  set.seed(sd)
  for (k in 2:4) {
    frq <- get(paste0("val.freqs.", k))
    assign(paste0("b.", k), sample(frq[frq[,1] > 0,1], 1000))
    assign(paste0("n.", k), sample(frq[frq[,2] > 0,2], 1000))
    assign(paste0("t.", k), sample(frq[frq[,3] > 0,3], 1000))
  
    for (j in c("b", "n", "t")) {
      valSet <- get(paste0(j, ".", k))
      
      for (i in 1:trials) {
        tx <- trimws(names(valSet[i]))
        spaces <- rev(unlist(gregexpr(" ", tx)))
        probe <- trimws(substring(tx, 1, spaces[1]))
        if (k == 2) probe <- paste0(probe, " ")
        nextWord <- trimws(substring(tx, spaces[1]))
        
        pred_a = predText(probe, all)
        pred_x = predText(probe, txt)
        pred_t = predText(probe, twit)
                
        res <- rbind(res, c(type = j
                            , ngram = k
                            , probe = probe
                            , nextWord = nextWord
                            , pred_a_1 = pred_a[1]
                            , pred_a_2 = pred_a[2]
                            , pred_a_3 = pred_a[3]
                            , pred_x_1 = pred_x[1]
                            , pred_x_2 = pred_x[2]
                            , pred_x_3 = pred_x[3]
                            , pred_t_1 = pred_t[1]
                            , pred_t_2 = pred_t[2]
                            , pred_t_3 = pred_t[3]
                            , correct_a_1 = nextWord == pred_a[1]
                            , correct_x_1 = nextWord == pred_x[1]
                            , correct_t_1 = nextWord == pred_t[1]
                            , correct_a_any = any(nextWord == c(pred_a[1], pred_a[2], pred_a[3]))
                            , correct_x_any = any(nextWord == c(pred_x[1], pred_x[2], pred_x[3]))
                            , correct_t_any = any(nextWord == c(pred_t[1], pred_t[2], pred_t[3]))
                            ))
      }
    }
  }
  res <- data.frame(res)
  out <- list(accuracy_1 = list(a = tapply(as.logical(res$correct_a_1), list(res$ngram, res$type), sum)
                              , x = tapply(as.logical(res$correct_x_1), list(res$ngram, res$type), sum)
                              , t = tapply(as.logical(res$correct_t_1), list(res$ngram, res$type), sum))
              , accuracy_any = list(a = tapply(as.logical(res$correct_a_any), list(res$ngram, res$type), sum)
                                , x = tapply(as.logical(res$correct_x_any), list(res$ngram, res$type), sum)
                                , t = tapply(as.logical(res$correct_t_any), list(res$ngram, res$type), sum))
              , matches = res)
  out
}

txs <- c(" how do i make money on "
     , "how do i make money on "
     , "how do i make money on"
     , " do i make money on "
     , "do i make money on "
     , "do i make money on"
     , " i make money on "
     , "i make money on "
     , "i make money on"
     , " make money on "
     , "make money on "
     , "make money on"
     , " money on "
     , "money on "
     , "money on"
     , " on "
     , "on "
     , "on")
txType <- all
for(i in seq_along(txs)) {
tx <- txs[i]
    
# if (grepl(" ", trimws(tx, which = "left"))) {
#   return(names(which.max(txType[[1]][grep(paste0("^", trimws(tx)), names(txType[[1]]))])))
# }

tx <- trimws(tx)
spaces <- rev(unlist(gregexpr(" ", tx)))
if (spaces[1] < 0) { 
  currentWords <- paste0(tx, " ")
} else {
  if (length(spaces) < 5) { spaces <- c(spaces,1) }
  currentWords <- rev(ifelse(is.na(substring(tx, spaces)), NA
                             , paste0(trimws(substring(tx, spaces)), " "))[1:min(c(3, (length(spaces))))])
}

print(currentWords)
}