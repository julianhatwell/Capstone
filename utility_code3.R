library(car)

testCreateTDM <- function(corp, tk) {
  TDM <-  TermDocumentMatrix(
    corp, control = list(
      tokenize = tk
      , removePunctuation = FALSE
      , stopwords = FALSE
      , stemming = FALSE
      , tolower = FALSE))
  TDM
}

en_US <- dataLoad(sampDir)
trans <- en_US

trans <- tm_map(trans, content_transformer(
  function(x) { iconv(x
                      , from = "UTF-8"
                      , to = "latin1", sub="") })) # remove weird codepage characters
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("(\u0090|\u0091|\u0092|\u0093|\u0094|\u0095|\u0096|\u0097|\u0098|\u0099)", "", x) })) # remove unicode characters
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("#[0-9a-zA-Z]+", "", x) })) # remove hashtags
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("(http[s]?|www)[.:/]*[a-zA-Z0-9]*[.:/]?[a-zA-Z0-9]*[.:/]?[a-zA-Z0-9]*[.:/]?[a-zA-Z0-9]*[?]?.*", "", x) })) # remove links
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("(-{2,}|-_+-*| -[a-zA-Z]+|^-|[0-9]+-+|-+[0-9]+| - )", " ", x) })) # remove anything with non-grammatical punc
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("-$", "", x) })) # remove anything with non-grammatical punc
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("[<>]3*", " ", x) })) # remove anything with non-grammatical punc
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("[~@%()_=:;{}`/&!#|]", " ", x) })) # remove anything with non-grammatical punc
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("[\\^$+\"]", " ", x) })) # remove anything with non-grammatical punc
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("(^'| '|' |''| ' )", " ", x) })) # remove anything with non-grammatical punc
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("[\\[*]", " ", x) })) # remove anything with non-grammatical punc
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("[\\]*]", " ", x) })) # remove anything with non-grammatical punc
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("[\\]*]", " ", x) })) # remove anything with non-grammatical punc
trans <- tm_map(trans, content_transformer(
  function(x) { gsub("[.+,+\\?+]", " ", x) })) # remove ramaining punc except apostrophes
trans <- tm_map(trans, removeNumbers)
trans <- tm_map(trans, content_transformer(tolower))
#if (rmStops) { trans <- tm_map(trans, removeWords, stops) }
#if (rmPrfn) { trans <- tm_map(trans, removeWords, prfn) }
#if (rmPunc) { trans <- tm_map(trans, removePunctuation) }  
trans <- tm_map(trans, stripWhitespace)
trans <- tm_map(trans, content_transformer(
  function(x) { trimws(x, which = "both") })) # belt and braces
# if (stem) { 
#   dict <- trans
#   trans <- tm_map(trans, stemDocument)
#   trans <- tm_map(trans, stemCompletion, dictionary=dict, type = "prevalent")
# }
# trans

trans <- tm_map(trans, content_transformer(
  function(x) { gsub("[\\?+]", " ", x) })) # remove anything with non-grammatical punc

grep("\\?", trans[[1]]$content) 
grep("''", trans[[2]]$content, value = TRUE)
gsub(" - ", " ", trans[[2]]$content[11383]) 

trans <- testCreateTDM(trans, threeGramTK)
trans <- as.matrix(trans)
head(trans);tail(trans);some(trans)
head(trans[trans[,1]>0,1]);tail(trans[trans[,1]>0,1]);some(trans[trans[,1]>0,1])
head(trans[trans[,2]>0,2]);tail(trans[trans[,2]>0,2]);some(trans[trans[,2]>0,2])
head(trans[trans[,3]>0,3]);tail(trans[trans[,3]>0,3]);some(trans[trans[,3]>0,3])

rownames(trans)[grep("[^a-zA-Z '-]+", rownames(trans))]



sum(en_US.freqs.1[en_US.freqs.1[,1] > 0,1])/length(en_US.freqs.1[en_US.freqs.1[,1] > 0,1])
quantile(sort(en_US.freqs.1[en_US.freqs.1[,1] > 0,1], decreasing = TRUE), 0.5)
z <- cumsum(en_US.freqs.1[en_US.freqs.1[,1] > 0,1])



en_US.tdm.1.unstemmed <- removeSparseTerms(createTDM(en_US.unstemmed
                                                     , oneGramTK),0.99)
en_US.freqs.1.unstemmed <- as.matrix(en_US.tdm.1.unstemmed)

en_US.tdm.1.unstemmed <- createTDM(en_US.unstemmed
                                                     , oneGramTK)
en_US.freqs.1.unstemmed <- as.matrix(en_US.tdm.1.unstemmed)



en_US.tdm.2.unstemmed <- removeSparseTerms(createTDM(en_US.unstemmed
                                                     , twoGramTK),0.99)
en_US.freqs.2.unstemmed <- as.matrix(en_US.tdm.2.unstemmed)

en_US.tdm.3.unstemmed <- removeSparseTerms(createTDM(en_US.unstemmed
                                                     , threeGramTK),0.99)
en_US.freqs.3.unstemmed <- as.matrix(en_US.tdm.3.unstemmed)
