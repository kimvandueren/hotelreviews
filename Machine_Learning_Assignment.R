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

#plot for comparing the different results
nb_accuracy <- nb_cm$overall['Accuracy']
nb_precision <- nb_cm$byClass['Neg Pred Value']
nb_recall <- nb_cm$byClass['Specificity']

svm_accuracy <- svm_cm$overall['Accuracy']
svm_precision <- svm_cm$byClass['Neg Pred Value']
svm_recall <- svm_cm$byClass['Specificity']

rf_accuracy <- rf_cm$overall['Accuracy']
rf_precision <- rf_cm$byClass['Neg Pred Value']
rf_recall <- rf_cm$byClass['Specificity']

nb_stats <- rbind(nb_accuracy, nb_precision, nb_recall)
nb_stats <- as.data.frame(nb_stats)
nb_stats$classifier <- "Naïve Bayes"

svm_stats <- rbind(svm_accuracy, svm_precision, svm_recall)
svm_stats <- as.data.frame(svm_stats)
svm_stats$classifier <- "Support Vector Machine"

rf_stats <- rbind(rf_accuracy, rf_precision, rf_recall)
rf_stats <- as.data.frame(rf_stats)
rf_stats$classifier <- "Random Forest"

stats_type <- c("Accuracy", "Precision", "Recall")

classifier_stats <- rbind(nb_stats, svm_stats, rf_stats)
classifier_stats$type <- stats_type
colnames(classifier_stats)[1] <- "fraction"
rownames(classifier_stats) <- 1:nrow(classifier_stats)
classifier_stats <- as.data.frame(classifier_stats)

stats_plot <- ggplot(classifier_stats, aes(classifier, fraction)) +
  geom_col(aes(fill = classifier, color = classifier), alpha = 0.5) +
  facet_grid(~type) +
  labs(title = "Classifier Statistics") +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
stats_plot

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

