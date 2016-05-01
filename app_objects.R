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
     , file = "intermediateObjs.RData")

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
  for (i in 1:4) {
    gtCounts <- sapply(1:500, function(x) { length(tType[[paste0("freqs.", i)]][tType[[paste0("freqs.", i)]]==x]) })
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

all_full <- setType("all")
twit_full <- setType("twitter")
txt_full <- setType("text")

modelReduce <- function(tType, rare) {
  for (i in 1:4) {
    typs <- c("freqs.", "probs.", "adjCounts.")
    nonRare <- tType[[paste0("probs.", i)]] > rare
    for (typ in typs) {
      tType[[paste0(typ, i)]] <- tType[[paste0(typ, i)]][nonRare]
    }
    tType[["adjProbs.1"]] <- tType[["adjProbs.1"]][nonRare]  
  }
  return(tType)
}

rare <- 5e-6
all <- modelReduce(all_full, rare)
twit <- modelReduce(twit_full, rare)
txt <- modelReduce(txt_full, rare)



save(all, twit, txt, file = "shinapp\\tdm_cond.Rdata")