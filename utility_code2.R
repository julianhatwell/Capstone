source("env_consts.R")
source("common_functions.R")
## ---- dataLoadSample ----
en_US <- dataLoad(sampDir)

## ---- edaPreProc ----
en_US.clean <- myPreProc(en_US
                         , stem = TRUE
                         , rmStops = TRUE
                         )
