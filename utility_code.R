## ---- dataLoadMain ----
en_US <- dataLoad(mainDir)

## ---- basicNs ----
basicNumbers <- list()
for (i in seq_along(sources)) {
  basicNumbers <- cbind(basicNumbers,
    c(en_US[[i]]$meta[["id"]]
      , round(file.info(files[i])[["size"]] / 1024^2,1)
      , length(en_US[[i]]$content)
      , round(mean(sapply(en_US[[i]]$content, nchar)),0)
      , max(sapply(en_US[[i]]$content, nchar))
    )
  )
}
basicNumbers <- data.frame(t(basicNumbers))
names(basicNumbers) <- c("filename", "filesize (MB)"
                         , "number of lines"
                         , "ave chars per line"
                         , "max chars per line")
kable(basicNumbers, format = "markdown")
aveChars <- as.numeric(unlist(basicNumbers$`ave chars per line`))
lengthRatio <- max(prop.table(aveChars)) / prop.table(aveChars)

## ---- createSamples ----
set.seed(12021)
for (i in seq_along(sk.files)) {
  # open conns
  read.conn <- file(sk.files[i])
  write.conn <- file(sk.dumps[i])
  write.conn2 <- file(sk.vals[i])
  
  extract <- readLines(read.conn, encoding = "UTF-8")
  samp.lines <- sample(length(extract), round(smp.size * lengthRatio[i]))
  train <- as.logical(rbinom(length(samp.lines), 1, 1-val.size))
  
  
  # getting an even sized sample by average number of chars per line
  train.extract <- extract[samp.lines[train]]
  writeLines(train.extract, write.conn, useBytes = TRUE)
  val.extract <- extract[samp.lines[!train]]
  writeLines(val.extract, write.conn2, useBytes = TRUE)
  
  # clean up
  close(read.conn)
  close(write.conn)
  close(write.conn2)
}
rm(list = c("extract", "train.extract", "val.extract"))

## ---- countBasedEvaluation ----
en_US.tdm.1 <- createTDM(en_US.clean, oneGramTK)
en_US.freqs.1 <- as.matrix(en_US.tdm.1)

en_US.tdm.2 <- createTDM(en_US.clean, twoGramTK)
en_US.freqs.2 <- as.matrix(en_US.tdm.2)

en_US.tdm.3 <- createTDM(en_US.clean, threeGramTK)
en_US.freqs.3 <- as.matrix(en_US.tdm.3)

en_US.tdm.4 <- createTDM(en_US.clean, fourGramTK)
en_US.freqs.4 <- as.matrix(en_US.tdm.4)
