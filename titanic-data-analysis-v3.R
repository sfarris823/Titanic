########################################################################
##
## Dataset:    Titanic: Machine Learning from Disaster
## URL:        https://www.kaggle.com/c/titanic
## Submitted:  TBD  
##
## Description:  
##             This script splits the combined test/train dataset 
##             into 2 separate models (m1 and m2) to determine whether  
##             the individual results of the models will be more 
##             accurate than predicting on the entire dataset.
##
########################################################################

# reset environment
rm(list=ls())


########################################################################
##
## Install/Load needed libraries
##
########################################################################

require("stringr")              # string manipulation
require("dplyr")                # data manipulation
require("ggplot2")              # simple plots 
require("randomForest")         # exploratory modeling
require("caret")                # cross validation
require("rpart")                # recursive partitioning
require("rpart.plot")           # plotting decision trees
require("lubridate")            # simplify working with dates in filenames



########################################################################
##
## Custom functions
##
########################################################################

# factorit() will take a data frame column and replace NAs, empty 
# strings with another value and returns the column as a factor
factorit <- function (dframe, na="Unk") 
{
  dframe[which(is.na(dframe))] <- na
  dframe[which(dframe=="")] <- na
  return (as.factor(dframe))
}

# showhist() builds a histogram using a facetwrap
showhist <- function(data, xaxis, fillgrp, facetwrap, title) 
{
  ggplot(data, aes_string(x = xaxis, fill = fillgrp)) +
    stat_count(width = 0.5) + 
    facet_wrap(facetwrap) +
    ggtitle(title) +
    xlab(xaxis) +
    ylab("Total Count") +
    labs(fill = fillgrp)
}

# rpart.cv() trains a predictive model 
rpart.cv <- function(seed, training, labels, ctrl)
{
  set.seed(seed)
  rpart.cv <- train(x=training, y=labels, method="rpart", tuneLength=30, trControl=ctrl)
  return (rpart.cv)
}

########################################################################
##
## Get data
##
########################################################################

# Read the train and test csv files into R
all.train <- read.csv("train.csv", stringsAsFactors = F)
all.test <- read.csv("test.csv", stringsAsFactors = F)

# I need to combine the test and train data sets, but need to add Survived to test first.
temp.test <- all.test
temp.test$Survived <- "Unk"
all.test <- select(temp.test, PassengerId, Survived, Pclass:Embarked)
rm (temp.test)
combined <- rbind(all.train, all.test)


########################################################################
##
## Understand the Data
##
########################################################################

# PassengerId
class(combined$PassengerId)                     # Class: integer
summary(combined$PassengerId)                   # Ranges from 1 - 1309
count(combined, vars=PassengerId)               # 1309 unique values
table(is.na(combined$PassengerId))              # No data is unknown

# Survived
class(all.train$Survived)                       # Class: integer (originally)
class(combined$Survived)                        # Class: character (after "Unk" was inserted)
summary(as.factor(combined$Survived))           # Ranges from 0-1
count(combined, vars=Survived)                  # 3 unique values (0, 1, Unk)
table(is.na(combined$Survived))                 # No data is unknown
table(combined$Survived=="Unk")                 # 418 values were empty, but replaced with "Unk"

# Pclass
class(combined$Pclass)                          # Class: integer
summary(combined$Pclass)                        # Ranges from 1-3
count(combined, vars=Pclass)                    # 3 unique values (1, 2, 3)
table(is.na(combined$Pclass))                   # No data is unknown

# Name
class(combined$Name)                            # Class: character
count(combined, vars=Name)                      # 1307 unique values (2 duplicates)
table(is.na(combined$Name))                     # No data is unknown
table(combined$Name=="")                        # No values are empty

# Sex
class(combined$Sex)                             # Class: character
count(combined, vars=Sex)                       # 2 unique values (male, female)
table(is.na(combined$Sex))                      # No data is unknown
table(combined$Sex=="")                         # No values are empty

# Age
class(combined$Age)                             # Class: numeric
count(combined, vars=Age)                       # 99 unique values
table(is.na(combined$Age))                      # 263 unknown values

# SibSp - The number of siblings and spouses traveling with an individual
class(combined$SibSp)                           # Class: integer
count(combined, vars=SibSp)                     # 7 unique values
table(is.na(combined$SibSp))                    # No data is unknown

# Parch - The number of parents and children traveling with an individual
class(combined$Parch)                           # Class: integer
count(combined, vars=Parch)                     # 8 unique values
table(is.na(combined$Parch))                    # No data is missing

# Ticket
class(combined$Ticket)                          # Class: character
count(combined, vars=Ticket)                    # 929 unique values
table(is.na(combined$Ticket))                   # No data is unknown
table(combined$Ticket=="")                      # No values are empty

# Fare
class(combined$Fare)                            # Class: numeric
count(combined, vars=Fare)                      # 282 unique values
table(is.na(combined$Fare))                     # 1 value is unknown

# Cabin
class(combined$Cabin)                           # Class: character
count(combined, vars=Cabin)                     # 187 unique values
table(is.na(combined$Cabin))                    # No data is unknown
table(combined$Cabin=="")                       # 1014 values are empty

# Embarked
class(combined$Embarked)                        # Class: character
count(combined, vars=Embarked)                  # 3 unique values
table(is.na(combined$Embarked))                 # No data is unknown
table(combined$Embarked=="")                    # 2 values are empty
combined$Embarked <- factorit(combined$Embarked)


########################################################################
##
## Add new features
##
########################################################################

# Simplify the Titles into Master, Mr, Miss, and Mrs
combined$Title <- gsub('(.*, )|(\\..*)', '', combined$Name)
combined$SimpleTitle <- combined$Title
# Simplify Mr titles
combined$SimpleTitle[which(combined$SimpleTitle %in% c("Don", "Jonkheer", "Sir", "Capt", "Col", "Major", "Rev"))] <- "Mr"
# Simplify Miss titles
combined$SimpleTitle[which(combined$SimpleTitle %in% c("Mlle", "Ms"))] <- "Miss"
# Simplify Mrs titles
combined$SimpleTitle[which(combined$SimpleTitle %in% c("Dona", "Lady", "Mme", "the Countess"))] <- "Mrs"
combined$SimpleTitle[which(combined$SimpleTitle=="Dr" & combined$Sex=="female")] <- "Mrs"
combined$SimpleTitle[which(combined$SimpleTitle=="Dr" & combined$Sex=="male")] <- "Mr"
combined$SimpleTitle <- as.factor(combined$SimpleTitle)

# Calculate the family unit size
combined$FamilySize <- as.numeric(paste(combined$SibSp)) + as.numeric(paste(combined$Parch)) + 1
combined$FamilySize <- as.factor(combined$FamilySize)

# Adjust Fare based on number of passengers per ticket
ticket.party.size <- rep(0, nrow(combined))
avg.fare <- rep(0.0, nrow(combined))
tickets <- unique(combined$Ticket)
# Loop through dataset to calculate PartySize and Average Fare
for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(combined$Ticket == current.ticket)
  current.avg.fare <- combined[party.indexes[1], "Fare"] / length(party.indexes)
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}
# Add these new features to the combined data frame
combined$PartySize <- ticket.party.size
combined$AverageFare <- avg.fare
# Cleanup
rm (avg.fare, current.avg.fare, current.ticket, i, k, party.indexes, ticket.party.size, tickets)

# Calculate a value for the missing Fare (passenger #1044)
# based on mean of other average fares in 3rd class
combined[1044, "Fare"] <- mean(combined[combined$Pclass==3,"AverageFare"], na.rm=T)

# New Age-related Features

# Dataset excluding NA values for Age  
hasagevalue <- filter(combined, !is.na(Age))

# Dataset with NA values for Age  
hasnoagevalue <- filter(combined, is.na(Age))

# Datasets to hold individuals belonging to different categories
MaleSingle <- filter(hasagevalue, Sex=="male", SibSp==0, Parch==0, SimpleTitle=="Mr")
FemaleSingle <- filter(hasagevalue, Sex=="female", SibSp==0, Parch==0)

MaleKid <- filter(hasagevalue, Sex=="male", SibSp>=0, SimpleTitle=="Master")
FemaleKid <- filter(hasagevalue, Sex=="female", SibSp>0, Parch>0, SimpleTitle=="Miss")
FemaleAdult <- filter(hasagevalue, Sex=="female", SibSp>0, Parch>0, SimpleTitle=="Miss")

HasSibSpParchMaleAdults <- filter(hasagevalue, Sex=="male", SibSp!=0, Parch!=0, SimpleTitle=="Mr")
HasSibSpParchFemaleMarried <- filter(hasagevalue, Sex=="female", SibSp!=0, Parch!=0, SimpleTitle=="Mrs")

# TBD: Likely that some individuals may not be categorized ... to review after populating age based on these 7 categories

# Average values for each of these categories
AvgAgeMaleSingle <- mean(MaleSingle$Age)
AvgAgeFemaleSingle <- mean(FemaleSingle$Age)

AvgAgeMaleKid <- mean(MaleKid$Age)
AvgAgeFemaleKid <- mean(FemaleKid$Age)
AvgAgeFemaleAdult <- mean(FemaleAdult$Age)

AvgAgeMaleAdults <- mean(HasSibSpParchMaleAdults$Age)
AvgAgeFemaleMarried <- mean(HasSibSpParchFemaleMarried$Age)

# Set age value in the dataset that holds rows where AGE is NA
hasnoagevalue$Age <- ifelse((hasnoagevalue$Sex=='male' & hasnoagevalue$SibSp==0 & hasnoagevalue$Parch==0 & hasnoagevalue$SimpleTitle=="Mr"), AvgAgeMaleSingle, "NA")
hasnoagevalue$Age <- ifelse((hasnoagevalue$Sex=='female' & hasnoagevalue$SibSp==0 & hasnoagevalue$Parch==0), AvgAgeFemaleSingle, hasnoagevalue$Age)
hasnoagevalue$Age <- ifelse((hasnoagevalue$Sex=='male' & hasnoagevalue$SibSp>=0 & hasnoagevalue$SimpleTitle=="Master"), AvgAgeMaleKid, hasnoagevalue$Age)
hasnoagevalue$Age <- ifelse((hasnoagevalue$Sex=='female' & hasnoagevalue$SibSp>0 & hasnoagevalue$Parch > 0 & hasnoagevalue$SimpleTitle=="Miss"), AvgAgeFemaleKid, hasnoagevalue$Age)
hasnoagevalue$Age <- ifelse((hasnoagevalue$Sex=='male' & hasnoagevalue$SibSp!=0 & hasnoagevalue$Parch!=0 & hasnoagevalue$SimpleTitle=="Mr"), AvgAgeMaleAdults, hasnoagevalue$Age)
hasnoagevalue$Age <- ifelse((hasnoagevalue$Sex=='female' & hasnoagevalue$SibSp!=0 & hasnoagevalue$Parch!=0 & hasnoagevalue$SimpleTitle=="Mrs"), AvgAgeFemaleMarried, hasnoagevalue$Age)

#temporarily set age for other males and females (around 32 cases) - to be updated after further analysis
hasnoagevalue$Age <- ifelse((hasnoagevalue$Sex=='female' & hasnoagevalue$SimpleTitle=="Mrs"), AvgAgeFemaleMarried, hasnoagevalue$Age)
hasnoagevalue$Age <- ifelse((hasnoagevalue$Sex=='female' & hasnoagevalue$SimpleTitle=="Miss"), AvgAgeFemaleSingle, hasnoagevalue$Age)
hasnoagevalue$Age <- ifelse((hasnoagevalue$Sex=='male' & hasnoagevalue$SimpleTitle=="Mr"), AvgAgeMaleAdults, hasnoagevalue$Age)

# End of function that estimates the age
# 9 cases are still having Age as NA - to be addressed after further analysis

# combine datasets with age value and the one where age value was populated above using rbind
# combined dataset is the new dataset with age values populated for most cases 
# Use this to build models instead of the dataset "combined" to see if accuracy improves
combined_new = rbind(hasagevalue, hasnoagevalue)

# Temp Fix to remove NAs in Age
combined_new[1201,6] <- AvgAgeMaleAdults
combined_new[1071,6] <- AvgAgeFemaleAdult
combined_new[1093,6] <- AvgAgeFemaleAdult 
combined_new[1094,6] <- AvgAgeFemaleAdult
combined_new[1111,6] <- AvgAgeFemaleAdult
combined_new[1170,6] <- AvgAgeFemaleAdult
combined_new[1176,6] <- AvgAgeFemaleAdult
combined_new[1249,6] <- AvgAgeFemaleAdult
combined_new[1282,6] <- AvgAgeFemaleAdult



########################################################################
##
## Model 1:  1st class: male children, all females
##           2nd class: everyone
##           3rd class: none
##
########################################################################
m1.combined_new <- rbind(  
            combined_new[combined_new$Pclass==1 & (combined_new$SimpleTitle=="Master" | combined_new$SimpleTitle=="Mrs" | combined_new$SimpleTitle=="Miss"),], 
            combined_new[combined_new$Pclass==2,]
           )
m1.train <- m1.combined_new[m1.combined_new$Survived!="Unk",]
m1.test  <- m1.combined_new[m1.combined_new$Survived=="Unk",]


# Exploratory Modeling

# Play with plots
showhist(m1.train, "SimpleTitle", "Survived", ~Pclass, "Survival Rates by Passenger Class and Simplified Title")

# Build an ensemble of decision trees (random forest)
set.seed(10000)
#m1.rf.features <- c("SimpleTitle", "AverageFare", "PartySize", "Pclass")
m1.rf.features <- c("SimpleTitle", "AverageFare", "PartySize", "Pclass","Age")
m1.rf <- randomForest(x = m1.train[, m1.rf.features], y = as.factor(m1.train$Survived), importance = T, ntree = 500)
# OLD RESULTS-View the results of the random forest, OOB estimate of error rate is 6.76%.  93.24% accuracy.
# NEW RESULTS-View the results of the random forest, OOB estimate of error rate is 6.41%.  93.59% accuracy.
m1.rf
# Draw a Variable Importance Plot.
varImpPlot(m1.rf)

# Cross Validation
set.seed(11000)
m1.rf.label <- as.factor(m1.train$Survived)
m1.cv.10.folds <- createMultiFolds(m1.rf.label, k = 10, times = 10)
m1.ctrl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = m1.cv.10.folds)

set.seed(12000)
#m1.rf.cv <- train(x=m1.train[, c("SimpleTitle", "AverageFare", "PartySize", "Pclass")], 
#                  y=m1.rf.label, method="rf", tuneLength=3, ntree=64, trControl=m1.ctrl1)
m1.rf.cv <- train(x=m1.train[, c("SimpleTitle", "AverageFare", "PartySize", "Pclass", "Age")], 
                   y=m1.rf.label, method="rf", tuneLength=3, ntree=64, trControl=m1.ctrl1)
# Show the results of the cross validation.  Accuracy is 93.39% (compared to 93.24% accuracy of random forest)
m1.rf.cv

# More Exploratory Modeling
#m1.features <- c("SimpleTitle", "AverageFare", "PartySize", "Pclass")
m1.features <- c("SimpleTitle", "AverageFare", "PartySize", "Pclass", "Age")
m1.rpart.train.1 <- m1.train[, m1.features]
m1.rpart.1.cv.1 <- rpart.cv(94622, m1.rpart.train.1, m1.rf.label, m1.ctrl1)
m1.rpart.1.cv.1
prp(m1.rpart.1.cv.1$finalModel, type=0, extra=1, under=T)
m1.rpart.1.cv.1$finalModel
# The results:
#      1 - Adult males (2nd class adult males) in this dataset are  
#          predicted to perish at an overall accuracy rate of 91.21%.
#      2-  All others in this dataset are predicted to survive at an
#          overall accuracy rate of 94.80%.


########################################################################
##
##  Predict results of m1
##
########################################################################

# Collect the test data with all the new features I've built.
#m1.submission <- m1.test[, c("SimpleTitle", "AverageFare", "PartySize", "Pclass")]
m1.submission <- m1.test[, c("SimpleTitle", "AverageFare", "PartySize", "Pclass", "Age")]

# Predict whether the test data individuals survived or perished
m1.predictions <- predict(m1.rpart.1.cv.1$finalModel, m1.submission, type="class")
# Should be 145 in total
#  - 61 perished
#  - 84 survived
length(m1.predictions)
table(m1.predictions)

m1.predictedresults <- cbind(m1.test$PassengerId, as.numeric(as.character(m1.predictions)))
colnames(m1.predictedresults) <- c("PassengerId", "Survived")




########################################################################
##
## Model 2:  1st class: adult males
##           2nd class: none
##           3rd class: everyone
##
########################################################################
m2.combined_new <- rbind(  
            combined_new[combined_new$Pclass==1 & !(combined_new$SimpleTitle=="Master" | combined_new$SimpleTitle=="Mrs" | combined_new$SimpleTitle=="Miss"),], 
            combined_new[combined_new$Pclass==3,]
          )
m2.train <- m2.combined_new[m2.combined_new$Survived!="Unk",]
m2.test  <- m2.combined_new[m2.combined_new$Survived=="Unk",]

# Exploratory Modeling

# Play with plots
showhist(m2.train, "SimpleTitle", "Survived", ~Pclass, "Survival Rates by Passenger Class and Simplified Title")

# Build an ensemble of decision trees (random forest)
set.seed(20000)
#m2.rf.features <- c("SimpleTitle", "AverageFare", "PartySize", "Pclass")
m2.rf.features <- c("SimpleTitle", "AverageFare", "PartySize", "Pclass", "Age")
m2.rf <- randomForest(x = m2.train[, m2.rf.features], y = as.factor(m2.train$Survived), importance = T, ntree = 500)
# OLD RESULTS: View the results of the random forest, OOB estimate of error rate is 19.34%.  80.66% accuracy.
# NEW RESULTS: View the results of the random forest, OOB estimate of error rate is 19.02%.  80.98% accuracy.
m2.rf
# Draw a Variable Importance Plot.
varImpPlot(m2.rf)

# Cross Validation
set.seed(11000)
m2.rf.label <- as.factor(m2.train$Survived)
m2.cv.10.folds <- createMultiFolds(m2.rf.label, k = 10, times = 10)
m2.ctrl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = m2.cv.10.folds)

set.seed(12000)
#m2.rf.cv <- train(x=m2.train[, c("SimpleTitle", "AverageFare", "PartySize", "Pclass")], 
#                  y=m2.rf.label, method="rf", tuneLength=3, ntree=64, trControl=m2.ctrl1)
m2.rf.cv <- train(x=m2.train[, c("SimpleTitle", "AverageFare", "PartySize", "Pclass", "Age")], 
                  y=m2.rf.label, method="rf", tuneLength=3, ntree=64, trControl=m2.ctrl1)
# Show the results of the cross validation.  Accuracy is 80.26% (compared to 80.66% accuracy of random forest)
m2.rf.cv

# More Exploratory Modeling
#m2.features <- c("SimpleTitle", "AverageFare", "PartySize", "Pclass")
m2.features <- c("SimpleTitle", "AverageFare", "PartySize", "Pclass", "Age")
m2.rpart.train.1 <- m2.train[, m2.features]
m2.rpart.1.cv.1 <- rpart.cv(94622, m2.rpart.train.1, m2.rf.label, m2.ctrl1)
m2.rpart.1.cv.1
prp(m2.rpart.1.cv.1$finalModel, type=0, extra=1, under=T)
m2.rpart.1.cv.1$finalModel
# The results:
#      1 - Adult males in this dataset are  
#          predicted to perish at an overall accuracy rate of 78.33%.
#      2-  All others in this dataset are predicted to survive at an
#          overall accuracy rate of 94.80%.


########################################################################
##
##  Predict results of m2
##
########################################################################

# Collect the test data with all the new features I've built.
#m2.submission <- m2.test[, c("SimpleTitle", "AverageFare", "PartySize", "Pclass")]
m2.submission <- m2.test[, c("SimpleTitle", "AverageFare", "PartySize", "Pclass", "Age")]

# Predict whether the test data individuals survived or perished
m2.predictions <- predict(m2.rpart.1.cv.1$finalModel, m2.submission, type="class")
# Should be 273 in total
#  - 205 perished
#  - 68 survived
length(m2.predictions)
table(m2.predictions)

m2.predictedresults <- cbind(m2.test$PassengerId, as.numeric(as.character(m2.predictions)))
colnames(m2.predictedresults) <- c("PassengerId", "Survived")



########################################################################
##
##  Combine the predicted results from m1 and m2
##
########################################################################
all.results <- rbind(m1.predictedresults, m2.predictedresults)
all.pids <- order(all.results[,1])
all.finalresults <- all.results[all.pids,]

# Write the submission out to CSV with no row names.
filename <- paste(c("TITANIC-", format(now(), "%Y%m%d_%H%M%S"), ".csv"), sep="", collapse="")
write.csv(all.finalresults, file=filename, row.names=FALSE)
