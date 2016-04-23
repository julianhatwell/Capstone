# common functions
require(tm)
require(RWeka)

dataLoad <- function(dir) {
  corp <- Corpus(DirSource(dir, encoding = "UTF-8"),
                 readerControl = list(reader = readPlain,
                                      language = "en",
                                      load = TRUE))
}

myPreProc <- function(corp
                      , rmStops = FALSE
                      , stops = c(stopwords("en"), "ass")
                      , rmPrfn = FALSE
                      , prfn = ""
                      , rmPunc = FALSE
                      , stem = FALSE) {
  trans <- corp
  
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
    function(x) { gsub("( '|' )", " ", x) })) # remove anything with non-grammatical punc
  trans <- tm_map(trans, content_transformer(
    function(x) { gsub("[\\[*]", " ", x) })) # remove anything with non-grammatical punc
  trans <- tm_map(trans, content_transformer(
    function(x) { gsub("[\\]*]", " ", x) })) # remove anything with non-grammatical punc
  trans <- tm_map(trans, removeNumbers)
  trans <- tm_map(trans, content_transformer(tolower))
  if (rmStops) { trans <- tm_map(trans, removeWords, stops) }
  if (rmPrfn) { trans <- tm_map(trans, removeWords, prfn) }
  if (rmPunc) { trans <- tm_map(trans, removePunctuation) }  
  trans <- tm_map(trans, stripWhitespace)
  trans <- tm_map(trans, content_transformer(
    function(x) { trimws(x, which = "both") })) # belt and braces
  if (stem) { 
    # dict <- trans
    trans <- tm_map(trans, stemDocument)
    # trans <- tm_map(trans, stemCompletion, dictionary=dict, type = "first")
  }
  trans
}

custom.Tokenizer <- function(x, n) { 
  NGramTokenizer(x, Weka_control(min = n, max= n))
}

oneGramTK <- function(x) { custom.Tokenizer(x, 1) }
twoGramTK <- function(x) { custom.Tokenizer(x, 2) }
threeGramTK <- function(x) { custom.Tokenizer(x, 3) }
fourGramTK <- function(x) { custom.Tokenizer(x, 4) }

createTDM <- function(corp, tk) {
  TDM <-  TermDocumentMatrix(corp
                             , control = list(
                               tokenize = tk
                               , removePunctuation = FALSE
                               , stopwords = FALSE
                               , stemming = FALSE
                               , tolower = FALSE
                               , wordLengths = c(2, Inf)))
  TDM
}
