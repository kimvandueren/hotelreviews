#notes from consulting:
# - cleaning only as much as is necessary for each classifier, don't want overfitting
# - Only use half of the original dataset (but make sure to shuffle it before you do) so it isnt as big

library(dplyr)
library(data.table)
library(RMySQL)
library(DBI)
library(rvest)
library(caTools)
library(e1071) 
library(tm)
#library(RTextTools)
#library(doMC)
library(caret)

#read in hotel reviews
reviewData <- read.csv("Hotel_Reviews.csv")

#read in handwritten data
reviewHW <- read.csv("hand-written-data.csv")

#positive and negative dataframes
reviewPos <- reviewData["Positive_Review"]
reviewNeg <- reviewData["Negative_Review"]
names(reviewPos)[1] <- "review"
names(reviewNeg)[1] <- "review"

#labeling the reviews
reviewPos$label = "Positive"
reviewNeg$label = "Negative"

#merging the reviews
labeledData <- rbind(reviewPos, reviewNeg, reviewHW)

#cleaning data
labeledData$review <- as.character(labeledData$review)
labeledData$review <- gsub(",", "", labeledData$review)
labeledData$review <- gsub("\\.", "", labeledData$review)
labeledData$review <- tolower(labeledData$review)
labeledData$label <- tolower(labeledData$label)
tail(labeledData)

#database connection
reviewDb <- dbConnect(MySQL(), dbname = "hotelreviews", user = "root", password = "newrootpassword", host = "localhost")
dbListTables(reviewDb)
dbWriteTable(reviewDb, value = labeledData, name = "labeleddata", row.names = FALSE, overwrite = TRUE)
dbDisconnect(reviewDb)

#webscraping
url <- "https://uk.hotels.com/ho377829-tr/?q-check-in=2019-10-17&q-check-out=2019-10-18&q-rooms=1&q-room-0-adults=2&SYE=3&ZSX=0&MGT=1&YGF=2&WOD=4&WOE=5&JHR=1&FPQ=2&applyEmbargo=false&reviewTab=brand-reviews"
webpage <- read_html(url)

text_data_html <- html_nodes(webpage, ".description")
text_data <- html_text(text_data_html)
head(text_data)
text_data <- as.data.frame(text_data)

score_data_html <- html_nodes(webpage, ".rating-score")
score_data <- html_text(score_data_html)
score_data <- as.data.frame(score_data)
head(score_data)

url2 <- "https://uk.hotels.com/ho248446-tr/?q-check-in=2019-10-17&q-check-out=2019-10-18&q-rooms=1&q-room-0-adults=2&SYE=3&ZSX=0&MGT=1&YGF=1&WOD=4&WOE=5&JHR=1&FPQ=2&applyEmbargo=false&reviewTab=brand-reviews"
webpage2 <- read_html(url2)

text_data_html2 <- html_nodes(webpage2, ".description")
text_data2 <- html_text(text_data_html2)
head(text_data2)
text_data2 <- as.data.frame(text_data2)

score_data_html2 <- html_nodes(webpage2, ".rating-score")
score_data2 <- html_text(score_data_html2)
score_data2 <- as.data.frame(score_data2)
head(score_data2)

#labelling webscraped data
score_data <- score_data[-c(1),]
text_data$score <- score_data
ws_data1 <- text_data

score_data2 <- score_data2[-c(1),]
text_data2$score <- score_data2
ws_data2 <- text_data2
ws_data2 <- ws_data2[-c(42:50),]
names(ws_data2)[1] <- "text_data"

ws_data3 <- rbind(ws_data1, ws_data2)

#support vector machine
#https://www.geeksforgeeks.org/classifying-data-using-support-vector-machinessvms-in-r/
set.seed(123) 
split = sample.split(labeledData$label, SplitRatio = 0.75) 

training_set = subset(labeledData, split == TRUE) 
test_set = subset(labeledData, split == FALSE)

classifier = svm(formula = training_set$label ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear')
y_pred = predict(classifier, newdata = test_set$review)

#naïve bayes
#https://rpubs.com/cen0te/naivebayes-sentimentpolarity
set.seed(1)
nbLabeledData <- labeledData[sample(nrow(labeledData)), ]
nbLabeledData <- nbLabeledData[sample(nrow(nbLabeledData)), ]

nbLabeledData$label <- as.factor(nbLabeledData$label)

corpus <- Corpus(VectorSource(nbLabeledData$review))
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

nb_trainingData <- subset(nbLabeledData, split == TRUE)
nb_testData <- subset(nbLabeledData, split == FALSE)
nb_trainingDtm <- subset(dtm_matrix, split == TRUE)
nb_testDtm <- subset(dtm_matrix, split == FALSE)
nb_trainingCorpus <- subset(corpus.clean, split == TRUE)
nb_testCorpus <- subset(corpus.clean, split == TRUE)
#feature selection
