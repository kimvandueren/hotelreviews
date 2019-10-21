#naïve bayes
review_db <- dbConnect(MySQL(), dbname = "hotelreviews", user = "root", password = "newrootpassword", host = "localhost")
dbListTables(review_db)
nb_data1 <- dbGetQuery(review_db, "CALL GetAllReviews();")
dbDisconnect(review_db)

#https://rpubs.com/cen0te/naivebayes-sentimentpolarity
set.seed(1)
nb_data2 <- nb_data1[sample(nrow(nb_data1)), ]

nb_data2$label <- as.factor(nb_data2$label)

corpus <- Corpus(VectorSource(nb_data2$review))
inspect(corpus[1:3])

corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

dtm <- DocumentTermMatrix(corpus.clean)
dtm <- removeSparseTerms(dtm, .95)
dtm_matrix <- as.matrix(dtm)

inspect(dtm[40:50, 10:15])

nb_trainingData <- subset(nb_data2, split == TRUE)
nb_testData <- subset(nb_data2, split == FALSE)
nb_trainingDtm <- subset(dtm_matrix, split == TRUE)
nb_testDtm <- subset(dtm_matrix, split == FALSE)
nb_trainingCorpus <- subset(corpus.clean, split == TRUE)
nb_testCorpus <- subset(corpus.clean, split == TRUE)
#feature selection