vectorize_query = function(abstract,community){


library(tm)
library(SnowballC)

abs = data.frame(text = abstract,
                 doc_id = "q1")

docs = VCorpus(DataframeSource(abs))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

query.dtm <- DocumentTermMatrix(docs)

query.terms = Terms(query.dtm)

n = length(query.terms)

m = length(community)

query.vector = rep(0,m)

for (j in 1:m) {
  
  ind = (query.terms %in% unlist(community[j])) 
  
  query.vector[j] = any(ind) * 1
}
 
return(query.vector)

}