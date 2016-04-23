# Plots
## ---- basicNsPlot1 ----
tempTheme <- MyLatticeTheme
tempTheme$plot.symbol$alpha <- 1
tempTheme$plot.symbol$cex <- 3
tempTheme$plot.line$lwd <- 3

dotplot(sources~aveChars, type = c("p", "h")

        , par.settings = tempTheme
        , strip = MyLatticeStrip
        , xlim = c(0, max(aveChars) + 10)
        , main = "Dotplot to compare average line lengths"
        , xlab = "Average number of characters per line")

## ---- uniqueTermsPlots ----
uniqueTerms <- data.frame(sources)
for (j in 1:4) {
  frequencyTable <- get(paste0("en_US.freqs.", j))
  uniqueTerms <- cbind(uniqueTerms # add a weighting for size of nGram reduces search space
                       , (j-1) * smp.size * lengthRatio[i] + apply(frequencyTable > 0, 2, sum))
}
names(uniqueTerms) <- c("sources", paste0("nGramSize", 1:4))
rownames(uniqueTerms) <- NULL
#uniqueTerms <- melt(uniqueTerms)
d <- dotplot(sources~nGramSize1+nGramSize2+nGramSize3+nGramSize4
    , cex = 1.5, type = c("p", "a")
    , data = uniqueTerms
    , par.settings = MyLatticeTheme
    , strip = MyLatticeStrip
    , main = "Dotplot of distinct terms per source"
    , xlab = "Number of distinct terms"
    , auto.key = list(points = TRUE
                      #, lines = TRUE
                      , columns = 4))
print(d)

## ---- wordCountPlots ----
MyLatticeTheme$axis.text$cex <- 0.5
wordCountChart <- function(wc, gsz, f) {
  barchart(wc
  , horizontal = FALSE
  , log = TRUE
  , scales = list(rot = 60)
  , par.settings = MyLatticeTheme
  , strip = MyLatticeStrip
  , main = list(paste("n- Gram Size", gsz, "-", f)
               , cex = 0.8)
  , xlab = "Number of times n-Gram appears"
  , ylab = "log(count)")
}

for (j in 1:4) {
  
  frequencyTable <- get(paste0("en_US.freqs.", j))
  
  for (i in seq_along(sources)) {
    filename <- en_US[[i]]$meta[["id"]]
    wordCount <- log(table(frequencyTable[frequencyTable[,i]>0,i]))
    b <- wordCountChart(wc = wordCount, gsz = j, f = filename)
    print(b)
  } 
}

## ---- wordClouds ----
for (i in 1:3) {
  tdm.i <- paste0("en_US.freqs.", i)
  tdm <- get(tdm.i)
  tdm <- cbind(tdm, total = apply(tdm, 1, sum))
  tdm <- tdm[order(tdm[,"total"], decreasing = TRUE),]
  wc <- wordcloud(rownames(tdm), tdm[,"total"], min.freq=fq[i], max.words = 100
                 , rot.per = 0.2, colors = MyLatticeTheme$superpose.symbol$col)
  print(wc)
}
