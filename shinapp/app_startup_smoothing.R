library(tm)
library(dplyr)
load("tdm.Rdata")

predText <- function(tx, txType) {
  if (tx == "") { return("") }
  # utilities
  raggedNames <- function(rn) unlist(lapply(strsplit(names(rn), " "), function(x) {first(rev(x))}))
  
  # tuning parameters
  lambdas <- c(0.6, 0.39, 0.0995, 0.00005)
  
  n <- numeric(0)
  for (i in 1:4) {
    n[i] <- sum(txType[[i]]) + v
  }

  spaces <- c(rev(unlist(gregexpr(" ", tx)))[1:2], 1)
  if (spaces[1] < 0) { 
    return(names(which.max(txType[[1]][grep(paste0("^", tx), names(txType[[1]]))])))
  }
  if (is.na(spaces[2])) { spaces <- c(spaces[1], 1) }
  currentWords <- ifelse(is.na(substring(tx, spaces)), NA, paste0(trimws(substring(tx, spaces)), " "))
  if (currentWords[1] == " ") { currentWords <- currentWords[-1] }
  
  p4 <- if (is.na(spaces[3])) { 0
  } else { txType[[4]][grep(paste0("^", currentWords[3]), names(txType[[4]]))] / n[4] }
  if (length(p4) == 0) {
    p4 <- txType[[4]][grep(paste0(" ", currentWords[2]), names(txType[[4]]))]
    p4[seq_along(p4)] <- v/n[4]
    names(p4) <- raggedNames(p4)
  } else {
    names(p4) <- substring(names(p4), nchar(currentWords[3]) + 1)
  }
  
  p3 <- if (is.na(spaces[2])) { 0
  } else { txType[[3]][grep(paste0("^", currentWords[2]), names(txType[[3]]))] / n[3] }
  if (length(p3) == 0) {
    p3 <- txType[[3]][grep(paste0(" ", currentWords[1]), names(txType[[3]]))]
    p3[seq_along(p3)] <- v/n[3]
    names(p3) <- raggedNames(p3)
  } else {
    names(p3) <- substring(names(p3), nchar(currentWords[2]) + 1)
  }
  
  p2 <- txType[[2]][grep(paste0("^", currentWords[1]), names(txType[[2]]))] / n[2]
  names(p2) <- substring(names(p2), nchar(currentWords[1]) + 1)
  
  allCandidates <- unique(names(c(p4, p3, p2)))
  if (sum(p4, p3, p2) == 0) { return(sample(names(sort(txType[[1]], decreasing = TRUE)[1:100]),1))}
  p1 <- txType[[1]][allCandidates] / n[1]
  
  p4 <- p4[allCandidates]
  p4[is.na(p4)] <- 0
  p3 <- p3[allCandidates]
  p3[is.na(p3)] <- 0
  p2 <- p2[allCandidates]
  p2[is.na(p2)] <- 0
  p1 <- p1[allCandidates]
  p1[is.na(p1)] <- 0
  
  p <- lambdas[1] * p4 + lambdas[2] * p3 + lambdas[3] * p2 + lambdas[4] * p1
  names(p) <- allCandidates
  names(which.max(p))
}
