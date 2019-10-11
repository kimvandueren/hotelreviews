library(dplyr)
library(data.table)

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


##removing No Positive/Negative
#labeledData <- labeledData[review != "no positive", ]
#labeledData <- labeledData[review != "no negative", ]
#labeledData <- labeledData[review != "nothing", ]