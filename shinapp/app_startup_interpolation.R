
predText <- function(tx, txType) {
  if (trimws(tx) == "") { return(list(words = c("", "", ""), probs = rep(0, 3), firstWord = TRUE)) }
  if (!(grepl(" ", trimws(tx, which = "left")))) {
    words <- names(sort(txType[[1]][grep(paste0("^", trimws(tx)), names(txType[[1]]))],decreasing = TRUE)[1:preds])
    probs <- txType[["probs.1"]][words]
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
  
  # utilities
  raggedNames <- function(rn) unlist(lapply(strsplit(names(rn), " "), function(x) {dplyr::first(rev(x))}))
  
  p4 <- if (is.na(currentWords[3])) { 0
  } else { txType[["probs.4"]][grep(paste0("^", currentWords[3]), names(txType[["probs.4"]]))] }
  if (length(p4) == 0) {
    p4 <- txType[["probs.4"]][grep(paste0(" ", currentWords[2]), names(txType[["probs.4"]]))]
    names(p4) <- raggedNames(p4)
  } else {
    names(p4) <- substring(names(p4), nchar(currentWords[3]) + 1)
  }
  
  p3 <- if (is.na(currentWords[2])) { 0
  } else { txType[["probs.3"]][grep(paste0("^", currentWords[2]), names(txType[["probs.3"]]))] }
  if (length(p3) == 0) {
    p3 <- txType[["probs.3"]][grep(paste0(" ", currentWords[1]), names(txType[["probs.3"]]))]
    names(p3) <- raggedNames(p3)
  } else {
    names(p3) <- substring(names(p3), nchar(currentWords[2]) + 1)
  }
  
  p2 <- txType[["probs.2"]][grep(paste0("^", currentWords[1]), names(txType[["probs.2"]]))]
  names(p2) <- substring(names(p2), nchar(currentWords[1]) + 1)
  
  allCandidates <- unique(names(c(p4, p3, p2)))
  if (sum(p4, p3, p2) == 0) { 
    words <- sample(names(sort(txType[[1]], decreasing = TRUE)[1:100]), preds)
    probs <- txType[["probs.1"]][words]
    return(list(words = words
                , probs = probs
                , firstWord = FALSE)) }
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
  p <- p[p > 0]
  p <- p[order(p, decreasing = TRUE)]
  pWords <- names(p)
  if (length(pWords) < preds) {
    pWords <- c(pWords, sample(names(sort(txType[[1]], decreasing = TRUE)[1:100]), preds - length(pWords))) 
  }
  return(list(words = pWords[1:preds], probs = p[1:preds], firstWord = FALSE))
}
