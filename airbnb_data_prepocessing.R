# Read the csv file.
sf <- read.csv("sf_airbnb.csv")

# Basically feature selection.
sf <- sf[ ,(colnames(sf) %in% c("property_type","room_type","accommodates","bathrooms",
                                "beds","bed_type","guests_included","review_scores_rating","number_of_reviews",
                                "review_scores_checkin","review_scores_communication",
                                "review_scores_location","review_scores_value","price"))]

#Check null values.
table(is.na(sf)) 
#install.packages("mice")
library(mice)
md.pattern(sf)

sf2 <- na.omit(sf) #Delete missing value

# Data preprocessing
library("nnet")
dummypt <- class.ind(sf2$property_type)
dummypt[sample(nrow(dummypt),5),]
dummyrt <- class.ind(sf2$room_type)
dummyrt[sample(nrow(dummyrt),5),]
dummybt <- class.ind(sf2$bed_type)
dummybt[sample(nrow(dummybt),5),]

sf3 <- cbind(dummypt,dummybt,dummyrt,sf2)
head(sf3)

sf3 <- subset(sf3,select=-c(Other,property_type,Private_room,room_type,Airbed,bed_type,Hut,Timeshare,Treehouse))

# Delete outliers.
boxplot(sf3$price)
library(tidyverse)
library(readxl)
sf4 <-
  sf3 %>%
  filter((price > median(price) - 1.5 * IQR(price)) &
           (price < median(price) + 1.5 * IQR(price))) %>%
  ungroup()
boxplot(sf4$price)
hist(sf4$price,breaks = c(0,50,100,150,200,250,300,350,400)) 

# Feature selection.
# Method 1
train <- sample(1:nrow(sf4), nrow(sf4)/2)
sf.train <- sf4[train, ]
sf.test <- sf4[-train, ]
library(randomForest)
set.seed(1)
bag.sf <- randomForest(price ~ ., data = sf4, subset = train, mtry = 10,
                        importance = TRUE)
bag.sf
predict.bag <- predict(bag.sf, newdata = sf.test)
mean((predict.bag - sf.test$price)^2)

importance(bag.sf)
varImpPlot(bag.sf)

# Method 2
set.seed(7)
# load the library
#install.packages("mlbench")
library(mlbench)
library(caret)
#P calculate correlation matrix
correlationMatrix <- cor(sf4[,1:41])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

sf5 <- subset(sf4,select=c(Entire_home,review_scores_rating,accommodates,bathrooms,
                           number_of_reviews,guests_included,Shared_room,review_scores_location,
                           Boutique_hotel,House,beds,Apartment,Futon,review_scores_communication,
                           review_scores_value,price))
