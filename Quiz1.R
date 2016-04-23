
tw.conn <- "en_US\\en_US.twitter.txt"
twitter <- readLines(tw.conn, -1)
length(twitter)
max(sapply(twitter, nchar))
rm(twitter)

bl.conn <- "en_US\\en_US.blogs.txt"
blogs <- readLines(bl.conn, -1)
length(blogs)
max(sapply(blogs, nchar))
rm(blogs)

nw.conn <- "en_US\\en_US.news.txt"
news <- readLines(nw.conn, -1)
length(news)
max(sapply(news, nchar))
rm(news)

sum(grepl("love", twitter))/sum(grepl("hate", twitter))
twitter[grep("biostats", twitter)]

sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twitter))
