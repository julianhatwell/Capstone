
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
    
  trm.2 <- txType[[2]][grep(paste0("^",currentWords[1])
                            ,names(txType[[2]]))]
  if (length(trm.2) == 0) { 
    return(sample(names(sort(txType[[1]], decreasing = TRUE)[1:100]),1)) 
  } else {
    trm.2a <- txType[[1]]
    unseen.2 <- (1 + sum(trm.2 < rare))/(sum(trm.2a) + rare)
    adjusted.2 <- (trm.2 + 1) * unseen.2
    names(adjusted.2) <- substring(names(trm.2), nchar(currentWords[1]) + 1)
  }
  if (!(is.na(currentWords[2]))) {
    trm.3 <- txType[[3]][grep(paste0("^",currentWords[2])
                              ,names(txType[[3]]))]
    if (length(trm.3) > 0) {
      trm.3a <- txType[[2]][grep(paste0("^",trimws(currentWords[2]))
                                 ,names(txType[[2]]))]
      unseen.3 <- (1 + sum(trm.3 < rare))/(sum(trm.3a) + rare)
      adjusted.3 <- (trm.3 + 1) * unseen.3
      names(adjusted.3) <- substring(names(trm.3), nchar(currentWords[2]) + 1)
    }
  }
  if (!(is.na(currentWords[3]))) {
    trm.4 <- txType[[4]][grep(paste0("^",currentWords[3])
                                       ,names(txType[[4]]))]
    if (length(trm.4) > 0) {
      trm.4a <- txType[[3]][grep(paste0("^",trimws(currentWords[3]))
                       ,names(txType[[3]]))]
      unseen.4 <- (1 + sum(trm.4 < rare))/(sum(trm.4a) + rare)
      adjusted.4 <- (trm.4 + 1) * unseen.4
      names(adjusted.4) <- substring(names(trm.4), nchar(currentWords[3]) + 1)
    }
  }
  allCandidates <- unique(names(c(adjusted.4, adjusted.3, adjusted.2)))
  adjusted.4 <- adjusted.4[allCandidates]
  adjusted.4[is.na(adjusted.4)] <- 0
  adjusted.3 <- adjusted.3[allCandidates]
  adjusted.3[is.na(adjusted.3)] <- 0
  adjusted.2 <- adjusted.2[allCandidates]
  adjusted.2[is.na(adjusted.2)] <- 0
  adjusted.2[adjusted.3 != 0] <- 0
  adjusted.3[adjusted.4 != 0] <- 0
  
  adjusted <- adjusted.4 + adjusted.3 + adjusted.2
  names(adjusted) <- allCandidates
  names(p[order(p, decreasing = TRUE)][1:preds])
}