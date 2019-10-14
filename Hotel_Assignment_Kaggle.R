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

#removing No Positive/Negative
labeledData$review <- as.character(labeledData$review)
labeledData$review <- gsub(",", "", labeledData$review)
labeledData$review <- gsub("\\.", "", labeledData$review)
labeledData$review <- tolower(labeledData$review)
labeledData$label <- tolower(labeledData$label)
tail(labeledData)

#database connection
reviewDb <- dbConnect(MySQL(), dbname = "hotelreviews", user = "root", password = "newrootpassword", host = "localhost")
dbListTables(reviewDb)
dbWriteTable(reviewDb, value = labeledData, name = "labeledData", row.names = FALSE, overwrite = TRUE)
dbDisconnect(reviewDb)

#webscraping
url <- "https://www.tripadvisor.co.uk/Hotel_Review-g312659-d1605020-Reviews-Pepperclub_Hotel-Cape_Town_Central_Western_Cape.html"
webpage <- read_html(url)

text_data_html <- html_nodes(webpage, ".common-text-ReadMore__content--2X4LR")
text_data <- html_text(score_data_html)
#score_data <- as.numeric(score_data)
head(text_data)
text_data <- as.data.frame(text_data)

#support vector machine
#https://www.geeksforgeeks.org/classifying-data-using-support-vector-machinessvms-in-r/
set.seed(123) 
split = sample.split(labeledData$label, SplitRatio = 0.75) 

training_set = subset(labeledData, split == TRUE) 
test_set = subset(labeledData, split == FALSE)

classifier = svm(formula = label ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear')
y_pred = predict(classifier, newdata = test_set)

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

dtm <- as.data.frame(DocumentTermMatrix(corpus.clean))
inspect(dtm[40:50, 10:15])

nb_trainingData <- subset(nbLabeledData, split == TRUE)
nb_testData <- subset(nbLabeledData, split == FALSE)
nb_trainingDtm <- subset(dtm$dimnames, split == TRUE)
nb_testDtm <- subset(dtm, split == FALSE)
nb_trainingCorpus <- subset(corpus.clean, split == TRUE)
nb_testCorpus <- subset(corpus.clean, split == TRUE)
#feature selection
