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
url <- "https://www.hotelspecials.nl/apollo-hotel-almere-city-centre?utm_source=tripadvisor&utm_medium=meta&utm_campaign=1738&pc=CAQsFzQBBC0XNAEBAQEBAQIAAAA&refid=XaInbwoQL4oAAe8HE-IAAAAU&sc=EAAAAAAAAAAAAAAAAAAAAAA"
webpage <- read_html(url)

score_data_html <- html_nodes(webpage, ".rating-value span")
score_data <- html_text(score_data_html)
score_data <- as.numeric(score_data)
head(score_data)
