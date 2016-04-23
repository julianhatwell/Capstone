library(tm)
load("tdm.Rdata")

predText <- function(tx, txType) {
  if (tx == "") { return("") }
  trm.1.1 <- character(0)
  trm.2.1 <- character(0)
  trm.3.1 <- character(0)
  return.1 <- character(0)
  return.2 <- character(0)
  
  spaces <- rev(unlist(gregexpr(" ", trimws(tx))))[1:2]
  if (spaces[1] >= 0 & is.na(spaces[2])) { spaces[2] <- 1 }
  currentWords <- trimws(substring(tx, spaces))
  
  if (spaces[1] < 0) {
    if (nchar(currentWords[1]) > 0) {
      trm.1.1 <- names(txType[[1]])[grep(paste0("^",trimws(tx))
                                                      ,names(txType[[1]]))]
      
      return.1 <- names(which.max(txType[[1]][trm.1.1]))
      
      trm.1.2 <- names(txType[[2]])[grep(paste0("^",return.1)
                                                      ,names(txType[[2]]))]
      
      return.2 <- substring(names(which.max(txType[[2]][trm.1.2]))
                            , nchar(paste(return.1, " ")))  
    } else { return(which.max(txType[[1]])) }
  } else {
    if (!(is.na(spaces[2]))) {
      trm.3.1 <- names(txType[[3]])[grep(paste0("^",currentWords[2])
                                                      ,names(txType[[3]]))]
      if (length(trm.3.1) > 0) {
        return.1 <- trimws(substring(names(which.max(txType[[3]][trm.3.1]))
                                     , nchar(paste0(currentWords[2], " "))))
        
        trm.3.2 <- names(txType[[2]])[grep(paste0("^",return.1, " ")
                                                        ,names(txType[[2]]))]
        
        return.2 <- substring(names(which.max(txType[[2]][trm.3.2]))
                              , nchar(paste(return.1, " ")))
        
      } else {
        trm.2.1 <- names(txType[[2]])[grep(paste0("^",currentWords[1], " ")
                                                        ,names(txType[[2]]))]
        if (length(trm.2.1) == 0) { return(which.max(txType[[1]])) 
        } else {
          return.1 <- substring(names(which.max(txType[[2]][trm.2.1]))
                                , nchar(paste(currentWords[1], " ")))

          trm.2.2 <- names(txType[[2]])[grep(paste0("^",return.1, " ")
                                                          ,names(txType[[2]]))]
          
          return.2 <- substring(names(which.max(txType[[2]][trm.2.2]))
                                , nchar(paste(return.1, " ")))
        }
      }
    }
  }
  pred <- trimws(paste(trimws(return.1), trimws(return.2)))
  return(if (nchar(pred) > 0) { pred } else { which.max(txType[[1]]) })
}