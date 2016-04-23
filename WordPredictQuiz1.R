source("env_consts.R")
source("common_functions.R")

#en_US <- dataLoad(mainDir)
#en_US.unstemmed <- myPreProc(en_US, stem = FALSE, rmStops = FALSE)
#save(en_US.unstemmed, file = "fullCorpus.RData")
load("fullCorpus.RData")

phras <- tolower("settle the account")

g1 <- grep(phras, en_US.unstemmed[[1]]$content, value = TRUE)
g2 <- grep(phras, en_US.unstemmed[[2]]$content, value = TRUE)
g3 <- grep(phras, en_US.unstemmed[[3]]$content, value = TRUE)
cat(phras, length(c(g1, g2, g3)))