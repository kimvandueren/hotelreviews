library(dplyr)
library(data.table)
library(RMySQL)
library(DBI)
library(rvest)
library(caTools)
library(e1071) 
library(tm)
library(caret)
library(utf8)
library(SnowballC)
library(randomForest)

#read in hotel reviews
k_data1 <- read.csv("Hotel_Reviews.csv")

#positive and negative dataframes
review_pos <- k_data1["Positive_Review"]
review_neg <- k_data1["Negative_Review"]
names(review_pos)[1] <- "review"
names(review_neg)[1] <- "review"

#labeling the reviews
review_pos$label = "Positive"
review_neg$label = "Negative"

k_data2 <- rbind(review_pos, review_neg)
k_data2 <- k_data2[sample(nrow(k_data2)), ]

#cleaning and removing data
k_data3 <- subset(k_data2, review != "No Positive") %>%
  subset(review != "No Negative") %>%
  subset(review != "Nothing") %>%
  subset(review != "Na")

split = sample.split(k_data3$review, SplitRatio = 0.05)
k_data4 <- subset(k_data3, split == TRUE)

#read in handwritten data
hw_data1 <- read.csv("hand-written-data.csv")

#webscraping
url1 <- "https://uk.hotels.com/ho188843-tr/?q-check-in=2020-02-11&q-check-out=2020-02-12&q-rooms=1&q-room-0-adults=2&SYE=3&ZSX=0&MGT=1&YGF=14&WOD=2&WOE=3&JHR=1&FPQ=2&applyEmbargo=false&reviewTab=brand-reviews"
webpage1 <- read_html(url1)

text_data_html1 <- html_nodes(webpage1, ".description")
text_data1 <- html_text(text_data_html1)
text_data1 <- as.data.frame(text_data1)
head(text_data1)

score_data_html1 <- html_nodes(webpage1, ".rating-score")
score_data1 <- html_text(score_data_html1)
score_data1 <- as.numeric(score_data1)
score_data1 <- as.data.frame(score_data1)
head(score_data1)

url2 <- "https://uk.hotels.com/ho180856-tr/?q-check-in=2020-03-25&q-check-out=2020-03-26&q-rooms=1&q-room-0-adults=2&SYE=3&ZSX=0&MGT=1&YGF=14&WOD=3&WOE=4&JHR=1&FPQ=2&applyEmbargo=false&reviewTab=brand-reviews"
webpage2 <- read_html(url2)

text_data_html2 <- html_nodes(webpage2, ".description")
text_data2 <- html_text(text_data_html2)
text_data2 <- as.data.frame(text_data2)
head(text_data2)

score_data_html2 <- html_nodes(webpage2, ".rating-score")
score_data2 <- html_text(score_data_html2)
score_data2 <- as.numeric(score_data2)
score_data2 <- as.data.frame(score_data2)
head(score_data2)

#labelling webscraped data
score_data1 <- score_data1[-c(1),]
text_data1$score <- score_data1
ws_data1 <- text_data1
names(ws_data1)[1] <- "review"

score_data2 <- score_data2[-c(1),]
text_data2$score <- score_data2
ws_data2 <- text_data2
names(ws_data2)[1] <- "review"

ws_data3 <- rbind(ws_data1, ws_data2)
ws_data3$score <- lapply(ws_data3$score, as.numeric)

ws_data3 <- ws_data3 %>%
  mutate(label = case_when(ws_data3$score > 5 ~ "Positive",
                           ws_data3$score < 5 ~ "Negative",))

ws_data3$score <- NULL
ws_data3$review <- gsub("[^[:alnum:][:space:]]","", ws_data3$review)

#combining all three data sets
data1 <- rbind(k_data4, hw_data1, ws_data3)
data1 <- data1[sample(nrow(data1)), ]
 
#database connection
review_db <- dbConnect(MySQL(), dbname = "hotelreviews", user = "root", password = "newrootpassword", host = "localhost")
dbListTables(review_db)
dbWriteTable(review_db, value = data1, name = "labeleddata", row.names = FALSE, overwrite = TRUE)
dbDisconnect(review_db)
