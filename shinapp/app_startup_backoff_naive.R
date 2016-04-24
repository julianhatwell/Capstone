
predText <- function(tx, txType) {
  if (trimws(tx) == "") { return("") }
  if (!(grepl(" ", trimws(tx, which = "left")))) {
    return(names(which.max(txType[[1]][grep(paste0("^", trimws(tx)), names(txType[[1]]))])))
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
  
  if (!(is.na(currentWords[3]))) {
    trm.4.1 <- txType[[4]][grep(paste0("^",currentWords[3])
                                       ,names(txType[[4]]))]
    if (length(trm.4.1) > 0) {
      return(substring(names(which.max(trm.4.1)), nchar(currentWords[3]) + 1))
    }
  }
  if (!(is.na(currentWords[2]))) {
    trm.3.1 <- txType[[3]][grep(paste0("^",currentWords[2])
                                ,names(txType[[3]]))]
    
    if (length(trm.3.1) > 0) {
      return(substring(names(which.max(trm.3.1)), nchar(currentWords[2]) + 1))
    }
  }
  trm.2.1 <- txType[[2]][grep(paste0("^",currentWords[1])
                                ,names(txType[[2]]))]
    
  if (length(trm.2.1) > 0) {
    return(substring(names(which.max(trm.2.1)), nchar(currentWords[1]) + 1))
  }
  
  return(sample(names(sort(txType[[1]], decreasing = TRUE)[1:100]),1)) 
}