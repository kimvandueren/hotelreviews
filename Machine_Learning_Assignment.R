#db connection
review_db <- dbConnect(MySQL(), dbname = "hotelreviews", user = "root", password = "newrootpassword", host = "localhost")
dbListTables(review_db)
data1 <- dbGetQuery(review_db, "CALL GetAllReviews();")
dbDisconnect(review_db)

data1$review <- as.factor(data1$review)
data1$label <- as.factor(data1$label)

set.seed(123)

#creating a corpus
corpus <- Corpus(VectorSource(data1$review))
inspect(corpus[1])

corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stemDocument)

dtm <- DocumentTermMatrix(corpus_clean)
dtm <- removeSparseTerms(dtm, 0.99)

data2 <- as.data.frame(as.matrix(dtm))
colnames(data2) <- make.names(colnames(data2))
data2$label <- data1$label

split = sample.split(data2$label, SplitRatio = 0.75)
training_data <- subset(data2, split == TRUE)
test_data <- subset(data2, split == FALSE)

#naive bayes
nb_classifier <- naiveBayes(training_data$label ~ ., data = training_data)
nb_predict <- predict(nb_classifier, test_data)
nb_cm <- confusionMatrix(nb_predict, test_data$label)

#support vector machine
svm_classifier <- svm(training_data$label ~ ., data = training_data, type = 'C-classification')
svm_predict <- predict(svm_classifier, test_data)
svm_cm <- confusionMatrix(svm_predict, test_data$label)

#random forest
rf_classifier <- randomForest(training_data$label ~ ., data = training_data)
rf_predict <- predict(rf_classifier, test_data)
rf_cm <- confusionMatrix(rf_predict, test_data$label)

#confusion matrixes
nb_cm
svm_cm
rf_cm



#code for assessment
test_text <- "I would love to stay at this place again, the room was wonderful and the service fantastic."
test_corpus <- Corpus(VectorSource(test_text))
test_dtm <- DocumentTermMatrix(test_corpus)
test_text2 <- as.data.frame(as.matrix(test_dtm))
test_text2 <- cbind(test_text, test_text2)
test_text2 <- cbind(test_text2, data2[15, -146])[1,]
test_text2$test_text <- NULL

test_nb_predict <- predict(nb_classifier, test_text2)
test_nb_predict

test_svm_predict <- predict(svm_classifier, test_text2)
test_svm_predict

test_rf_predict <- predict(rf_classifier, test_text2)
test_rf_predict

