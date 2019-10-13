library(dplyr)
library(data.table)
library(RMySQL)
library(DBI)
library(rvest)

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
labeledData <- labeledData[labeledData$review != "No Positive", ]
labeledData <- labeledData[labeledData$review != "No Negative", ]

#database connection
reviewDb <- dbConnect(MySQL(), dbname = "hotelreviews", user = "root", password = "newrootpassword", host = "localhost")
dbListTables(reviewDb)
dbWriteTable(reviewDb, value = labeledData, name = "labeledData", row.names = FALSE, overwrite = TRUE)
dbDisconnect(reviewDb)

#webscraping
url <- "https://www.tripadvisor.co.uk/Hotel_Review-g312659-d1605020-Reviews-Pepperclub_Hotel-Cape_Town_Central_Western_Cape.html"
webpage <- read_html(url)

score_data_html <- html_nodes(webpage, ".common-text-ReadMore__content--2X4LR")
score_data <- html_text(score_data_html)
#score_data <- as.numeric(score_data)
head(score_data)
score_data <- as.data.frame(score_data)
