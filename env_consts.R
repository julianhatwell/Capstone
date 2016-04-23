mainDir <- "en_US"
sampDir <- "sample"
valDir <- "validation"
locale <- "en_US"
sources <- c("blogs", "news", "twitter")
makeDir <- function(dir, loc, f) {paste0(dir, "\\", loc, ".", f, ".txt")}

sk.files <- makeDir(mainDir, locale, sources)
sk.dumps <- makeDir(sampDir, locale, sources)
sk.vals <- makeDir(valDir, locale, sources)

smp.size <- 25000
val.size <- 0.2

fq <- c(100, 10, 3)