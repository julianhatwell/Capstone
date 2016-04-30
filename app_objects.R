source("utility_code2.R")
en_US.unstemmed <- myPreProc(en_US, stem = FALSE, rmStops = FALSE)

# creating R objects to use in the app
en_US.tdm.1.unstemmed <- createTDM(en_US.unstemmed, oneGramTK)
en_US.freqs.1.unstemmed <- as.matrix(en_US.tdm.1.unstemmed)

en_US.tdm.2.unstemmed <- createTDM(en_US.unstemmed, twoGramTK)
en_US.freqs.2.unstemmed <- as.matrix(en_US.tdm.2.unstemmed)

en_US.tdm.3.unstemmed <- createTDM(en_US.unstemmed, threeGramTK)
en_US.freqs.3.unstemmed <- as.matrix(en_US.tdm.3.unstemmed)

en_US.tdm.4.unstemmed <- createTDM(en_US.unstemmed, fourGramTK)
en_US.freqs.4.unstemmed <- as.matrix(en_US.tdm.4.unstemmed)

save(en_US.freqs.1.unstemmed
     , en_US.freqs.2.unstemmed
     , en_US.freqs.3.unstemmed
     , en_US.freqs.4.unstemmed
     , file = "intermediateObjs.R")

v <- 0

for (i in 1:4) {
  tdm <- as.matrix(get(paste0("en_US.tdm.", i, ".unstemmed")))
  all <- apply(tdm, 1, sum)
  twit <- tdm[,3]
  txt <- all - twit
  assign(paste0("freqs.all.", i), all[all > v])
  assign(paste0("freqs.twitter.", i), twit[twit > v])
  assign(paste0("freqs.text.", i), txt[txt > v])
}
rm(all, twit, txt)

setType <- function(txType) {
  tType <- list(freqs.1 = get(paste0("freqs.", txType, ".1"))
                , freqs.2 = get(paste0("freqs.", txType, ".2"))
                , freqs.3 = get(paste0("freqs.", txType, ".3"))
                , freqs.4 = get(paste0("freqs.", txType, ".4"))
  )
  for (i in 1:4) {
    probs <- tType[[paste0("freqs.", i)]]/sum(tType[[paste0("freqs.", i)]])
    tType[[paste0("probs.", i)]] <- probs
  }
  for (i in 2:4) {
    space <- unlist(gregexpr(" ", names(tType[[paste0("freqs.", i)]])))
    space <- space[seq_along(space) %% (i-1) == 0]
    firstWord <- substring(names(tType[[paste0("freqs.", i)]]), 1, space - 1)
    condFreq <- tType[[paste0("freqs.", i-1)]][firstWord]
    tType[[paste0("condProbs.", i)]] <- tType[[paste0("freqs.", i)]]/condFreq
  }
  for (i in 1:4) {
    gtCounts <- sapply(1:500, function(x) { length(all[[paste0("freqs.", i)]][all[[paste0("freqs.", i)]]==x]) })
    firstZero <- which(gtcounts == 0)[1]
    goodTuringAdjustor <- function(r) {
      if (r > firstZero) { 
        return(r+1) 
      } else {
        return((r+1)*gtCounts[r+1]/gtCounts[r])
      }
    }
    tType[[paste0("adjCounts.", i)]] <- sapply(tType[[paste0("freqs.", i)]], goodTuringAdjustor)
  }
  tType[["adjProbs.1"]] <- tType[["adjCounts.1"]]/(sum(tType[["freqs.1"]]) + 1)
  return(tType)
}

all <- setType("all")
twit <- setType("twitter")
txt <- setType("text")

save(all, twit, txt, v, file = "shinapp\\tdm_cond.Rdata")