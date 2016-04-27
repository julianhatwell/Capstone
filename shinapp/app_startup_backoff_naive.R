
predText <- function(tx, txType) {
  if (trimws(tx) == "") { return("") }
  if (!(grepl(" ", trimws(tx, which = "left")))) {
    return(names(sort(txType[[1]][grep(paste0("^", trimws(tx)), names(txType[[1]]))],decreasing = TRUE)[1:preds]))
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
  remainingPreds <- preds
  foundWords <- character(0)
  if (!(is.na(currentWords[3]))) {
    trm.4.1 <- sort(txType[[4]][grep(paste0("^",currentWords[3])
                                       ,names(txType[[4]]))], decreasing = TRUE)[1:remainingPreds]
    remainingPreds <- remainingPreds - length(which(!(is.na(trm.4.1))))
    foundWords <- substring(names(trm.4.1), nchar(currentWords[3]) + 1)
    if (remainingPreds == 0) {
      return(foundWords)
    }
  }
  if (!(is.na(currentWords[2]))) {
    trm.3.1 <- sort(txType[[3]][grep(paste0("^",currentWords[2])
                                ,names(txType[[3]]))][1:remainingPreds], decreasing = TRUE)[1:remainingPreds]
    remainingPreds <- remainingPreds - length(which(!(is.na(trm.3.1))))
    foundWords <- c(foundWords, substring(names(trm.3.1), nchar(currentWords[2]) + 1))
    if (remainingPreds == 0) {
      return(foundWords[!(is.na(foundWords))])
    }
  }
  trm.2.1 <- sort(txType[[2]][grep(paste0("^",currentWords[1])
                                ,names(txType[[2]]))][1:remainingPreds], decreasing = TRUE)[1:remainingPreds]
  remainingPreds <- remainingPreds - length(which(!(is.na(trm.2.1))))
  foundWords <- c(foundWords, substring(names(trm.2.1), nchar(currentWords[1]) + 1))
    
  if (remainingPreds == 0) {
    return(foundWords[!(is.na(foundWords))])
  }
  
  return(sample(names(sort(txType[[1]], decreasing = TRUE)[1:100]),preds)) 
}