
predText <- function(tx, txType) {
  if (trimws(tx) == "") { return(list(words = c("", "", ""), probs = rep(0, 3), firstWord = TRUE)) }
  if (!(grepl(" ", trimws(tx, which = "left")))) {
    words <- names(sort(txType[[1]][grep(paste0("^", trimws(tx)), names(txType[[1]]))],decreasing = TRUE)[1:preds])
    probs <- txType[["adjProbs.1"]][words]
    return(list(words = words
                , probs = probs
                , firstWord = TRUE))
  }
  
  tx <- trimws(tx)
  spaces <- rev(unlist(gregexpr(" ", tx)))
  if (spaces[1] < 0) { 
    currentWords <- paste0(tx, " ")
  } else {
    if (length(spaces) < 5) { spaces <- c(spaces,1) }
    currentWords <- ifelse(is.na(substring(tx, spaces)), NA
                           , paste0(trimws(substring(tx, spaces)), " "))[1:min(c(3, (length(spaces))))]
  }
    
  trm.2 <- txType[["adjCounts.2"]][grep(paste0("^",currentWords[1])
                            ,names(txType[["freqs.2"]]))]
  if (length(trm.2) == 0) { 
    words <- sample(names(sort(txType[[1]], decreasing = TRUE)[1:100]), preds)
    probs <- txType[["adjProbs.1"]][words]
    return(list(words = words
                , probs = probs
                , firstWord = FALSE))
  } else {
    names(trm.2) <- substring(names(trm.2), nchar(currentWords[1]) + 1)
    trm.2a <- txType[["freqs.1"]][trimws(currentWords[1])]
    condProbs.2 <- trm.2/trm.2a
  }
  if (!(is.na(currentWords[2]))) {
    trm.3 <- txType[["adjCounts.3"]][grep(paste0("^",currentWords[2])
                              ,names(txType[["freqs.3"]]))]
    if (length(trm.3) > 0) {
      names(trm.3) <- substring(names(trm.3), nchar(currentWords[2]) + 1)
      trm.3a <- txType[["freqs.2"]][trimws(currentWords[2])]
      condProbs.3 <- trm.3/trm.3a
    }
  }
  if (!(is.na(currentWords[3]))) {
    trm.4 <- txType[["adjCounts.4"]][grep(paste0("^",currentWords[3])
                                       ,names(txType[["freqs.4"]]))]
    if (length(trm.4) > 0) {
      names(trm.4) <- substring(names(trm.4), nchar(currentWords[3]) + 1)
      trm.4a <- txType[["freqs.3"]][trimws(currentWords[3])]
      condProbs.4 <- trm.4/trm.4a
    }
  }
  allCandidates <- unique(names(c(condProbs.4, condProbs.3, condProbs.2)))
  condProbs.4 <- condProbs.4[allCandidates]
  condProbs.4[is.na(condProbs.4)] <- 0
  condProbs.3 <- condProbs.3[allCandidates]
  condProbs.3[is.na(condProbs.3)] <- 0
  condProbs.2 <- condProbs.2[allCandidates]
  condProbs.2[is.na(condProbs.2)] <- 0
  condProbs.2[condProbs.3 != 0] <- 0
  condProbs.3[condProbs.4 != 0] <- 0
  
  condProbs <- condProbs.4 + (1 - sum(condProbs.4)) * condProbs.3 + (1 - sum(condProbs.3)) * condProbs.2
  names(condProbs) <- allCandidates
  condProbs <- condProbs[condProbs > 0]
  condProbs <- condProbs[order(condProbs, decreasing = TRUE)]
  words <- names(condProbs)
  return(list(words = words[1:preds], probs = condProbs[1:preds], firstWord = FALSE))
}