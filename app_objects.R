source("utility_code2.R")
en_US.unstemmed <- myPreProc(en_US, stem = FALSE, rmStops = FALSE)

# creating R objects to use in the app
en_US.tdm.1.unstemmed <- createTDM(en_US.unstemmed, oneGramTK)
en_US.freqs.1.unstemmed <- as.matrix(en_US.tdm.1.unstemmed)
# minor correction
en_US.freqs.1.unstemmed["the",] <- 
  en_US.freqs.1.unstemmed["the",] + en_US.freqs.1.unstemmed["-the",]
en_US.freqs.1.unstemmed <- en_US.freqs.1.unstemmed[-1,] # reduced by 3.25MB

en_US.tdm.2.unstemmed <- createTDM(en_US.unstemmed, twoGramTK)
en_US.freqs.2.unstemmed <- as.matrix(en_US.tdm.2.unstemmed)

en_US.tdm.3.unstemmed <- createTDM(en_US.unstemmed, threeGramTK)
en_US.freqs.3.unstemmed <- as.matrix(en_US.tdm.3.unstemmed)

en_US.tdm.4.unstemmed <- createTDM(en_US.unstemmed, fourGramTK)
en_US.freqs.4.unstemmed <- as.matrix(en_US.tdm.4.unstemmed)

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
rm(twit, txt)

setType <- function(txType) {
  tType <- list(freqs.1 = get(paste0("freqs.", txType, ".1"))
                , freqs.2 = get(paste0("freqs.", txType, ".2"))
                , freqs.3 = get(paste0("freqs.", txType, ".3"))
                , freqs.4 = get(paste0("freqs.", txType, ".4"))
  )
  return(tType)
}

all <- setType("all")
twit <- setType("twitter")
txt <- setType("text")

save(all, twit, txt, v, file = "shinapp\\tdm.Rdata")
