library(tm)
load("tdm.Rdata")

predText <- function(tx, txType) {
  if (tx == "" | tx == " ") { return("") }
  trm.1.1 <- character(0)
  trm.2.1 <- character(0)
  trm.3.1 <- character(0)
  trm.4.1 <- character(0)

  spaces <- c(rev(unlist(gregexpr(" ", tx)))[1:2], 1)
  if (spaces[1] < 0) { 
    return(names(which.max(txType[[1]][grep(paste0("^", tx), names(txType[[1]]))])))
  }
  if (is.na(spaces[2])) { spaces <- c(spaces[1], 1) }
  currentWords <- ifelse(is.na(substring(tx, spaces)), NA, paste0(trimws(substring(tx, spaces)), " "))
  if (currentWords[1] == " ") { currentWords <- currentWords[-1] }

  if (!(is.na(spaces[3]))) {
    trm.4.1 <- txType[[4]][grep(paste0("^",currentWords[3])
                                       ,names(txType[[4]]))]
    if (length(trm.4.1) > 0) {
      return(substring(names(which.max(trm.4.1)), nchar(currentWords[3]) + 1))
    }
  }
  if (!(is.na(spaces[2]))) {
    trm.3.1 <- txType[[3]][grep(paste0("^",currentWords[2])
                                ,names(txType[[3]]))]
    
    if (length(trm.3.1) > 0) {
      return(substring(names(which.max(trm.3.1)), nchar(currentWords[2]) + 1))
    }
  }
  if (!(is.na(spaces[1]))) {
    trm.2.1 <- txType[[2]][grep(paste0("^",currentWords[1])
                                ,names(txType[[2]]))]
    
    if (length(trm.2.1) > 0) {
      return(substring(names(which.max(trm.2.1)), nchar(currentWords[1]) + 1))
    }
  }
  return(sample(names(sort(txType[[1]], decreasing = TRUE)[1:100]),1)) 
}

    
      
      
 