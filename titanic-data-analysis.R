########################################################################
##
## Dataset:    Titanic: Machine Learning from Disaster
## URL:        https://www.kaggle.com/c/titanic
## Submitted:  December 11, 2016 - 0.80383
##
########################################################################

# reset environment
rm(list=ls())


########################################################################
##
## Install/Load needed libraries
##
########################################################################

# List of packages for session
.packages = c("dplyr",          # data manipulation
              "ggplot2",        # simple plots 
              "randomForest",   # exploratory modeling
              "caret",          # cross validation
              "doSNOW",         # clustering for performance
              "rpart",          # recursive partitioning
              "rpart.plot",     # plotting decision trees
              "lubridate"
             )

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, library, character.only=TRUE)
rm(.packages, .inst)


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
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

# I need to combine the test and train data sets, but need to add Survived to test first.
temp.test <- test
temp.test$Survived <- "Unk"
test <- select(temp.test, PassengerId, Survived, Pclass:Embarked)
rm (temp.test)
combined <- rbind(train, test)


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
class(train$Survived)                           # Class: integer (originally)
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

# SibSp
class(combined$SibSp)                           # Class: integer
count(combined, vars=SibSp)                     # 7 unique values
table(is.na(combined$SibSp))                    # No data is unknown

# Parch
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


########################################################################
##
## Exploratory Data Analysis
##
########################################################################

# Convert a few columns to factors so they can be analyzed using ggplot
combined$Survived <- as.factor(combined$Survived)
combined$Pclass <- as.factor(combined$Pclass)
combined$Sex <- as.factor(combined$Sex)
combined$Embarked <- factorit(combined$Embarked)
combined$SibSp <- factorit(combined$SibSp)
combined$Parch <- factorit(combined$Parch)

# Play with plots
showhist(combined[1:891,], "SimpleTitle", "Survived", ~Pclass, "Survival Rates by Passenger Class and Simplified Title")
#showhist(combined[1:891,], "Sex", "Survived", ~Pclass, "Survival Rates by Passenger Class and Sex")
#showhist(combined[1:891,], "Embarked", "Survived", ~Pclass, "Survival Rates by Passenger Class and Embarked")
#showhist(combined[1:891,], "Embarked", "Survived", ~Pclass+Sex, "Survival Rates by Passenger Class/Sex and Embarked")
#showhist(combined[1:891,], "SibSp", "Survived", ~Pclass, "Survival Rates by Passenger Class and SibSp")
#showhist(combined[1:891,], "SibSp", "Survived", ~Pclass+Sex, "Survival Rates by Passenger Class/Sex and SibSp")
#showhist(combined[1:891,], "Parch", "Survived", ~Pclass, "Survival Rates by Passenger Class and Parch")
#showhist(combined[1:891,], "Parch", "Survived", ~Pclass+Sex, "Survival Rates by Passenger Class/Sex and Parch")
#showhist(combined[1:891,], "Sex", "Survived", ~Pclass+Embarked, "Survival Rates by Passenger Class/Embarked and Sex")

# Explore adult males
train.adj <- combined[1:891,]
train.adj$Survived <- as.factor(train.adj$Survived)
train.adj$Pclass <- as.factor(train.adj$Pclass)
train.adj$FamilySize <- as.factor(train.adj$FamilySize)
train.adj$SimpleTitle <- as.factor(train.adj$SimpleTitle)

train.adj.mr <- train.adj[train.adj$SimpleTitle=="Mr",]
train.adj.mr$Survived <- as.factor(train.adj.mr$Survived)
train.adj.mr$Pclass <- as.factor(train.adj.mr$Pclass)
train.adj.mr$FamilySize <- as.factor(train.adj.mr$FamilySize)
showhist(train.adj.mr, "Age", "Survived", ~Pclass, "Survival Rates by Passenger Class and Age")
showhist(train.adj.mr, "FamilySize", "Survived", ~Pclass, "Survival Rates by Passenger Class and Age")
# Note: if your family size was >4 regardless of pclass, you likely perished.

# Explore family size   
showhist(train.adj, "FamilySize", "Survived", ~Pclass+SimpleTitle, "Survival Rates by Passenger Class and Age")
# Note: if your family size was >4 and you were in 3rd class, you likely perished.

# Explore cabin
table(filter(select(combined, Pclass, Cabin), Pclass=="1"))
table(filter(select(combined, Pclass, Cabin), Pclass=="2"))
table(filter(select(combined, Pclass, Cabin), Pclass=="3"))


# Exploratory Modeling

# Build an ensemble of decision trees (random forest)
set.seed(1234)
rf.1 <- randomForest(x = combined[1:891, c("Pclass", "SimpleTitle", "FamilySize")], 
                     y = as.factor(train$Survived), importance = T, ntree = 500)
# View the results of the random forest, OOB estimate of error rate is 17.06%.  82.94% accuracy.
rf.1
# Draw a Variable Importance Plot.  SimpleTitle seems to be the most important
varImpPlot(rf.1)

# Modify the random forest to try some new variables
rf.2 <- randomForest(x = combined[1:891, c("Pclass", "SimpleTitle", "PartySize", "AverageFare")], 
                     y = as.factor(train$Survived), importance = T, ntree = 500)
# View the results of the random forest, OOB estimate of error rate is 16.05%!  83.95% accuracy rate.
rf.2
# Draw a Variable Importance Plot.  PartySize and AverageFare seem to be more important than Pclass.
varImpPlot(rf.2)

# Cross Validation
set.seed(37596)
rf.label <- as.factor(train$Survived)
cv.10.folds <- createMultiFolds(rf.label, k = 3, times = 10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.10.folds)

set.seed(94622)
rf.1.cv.1 <- train(x=combined[1:891, c("Pclass", "SimpleTitle", "FamilySize")], 
                   y=rf.label, method="rf", tuneLength=3, ntree=64, trControl=ctrl.1)
# Show the results of the cross validation.  Accuracy is 81.92% (compared to 82.94% accuracy of random forest)
rf.1.cv.1

set.seed(94622)
rf.2.cv.1 <- train(x=combined[1:891, c("Pclass", "SimpleTitle", "PartySize", "AverageFare")], 
                   y=rf.label, method="rf", tuneLength=3, ntree=64, trControl=ctrl.1)
# Show the results of the cross validation.  Accuracy is 83.34% (compared to 83.95% accuracy of random forest)
rf.2.cv.1

# More Exploratory Modeling

features <- c("Pclass", "SimpleTitle", "FamilySize")
rpart.train.1 <- combined[1:891, features]
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.1)
rpart.1.cv.1
prp(rpart.1.cv.1$finalModel, type=0, extra=1, under=T)
rpart.1.cv.1$finalModel
# The plot brings out some interesting lines of investigation. Namely:
#      1 - SimpleTitles of "Mr" are predicted to perish at an
#          overall accuracy rate of 80.93%
#      2 - SimpleTitles of "Master", "Miss", and "Mrs" in 1st and 2nd class
#          are predicted to survive at an overall accuracy rate of 94.8%
#      3 - SimpleTitles of "Master", "Miss", and "Mrs" in 3rd class with
#          family sizes equal to 5, 6, 8, and 11 are predicted to perish
#          with 100% accuracy. [this may be overfitting due to specificity]
#      4 - SimpleTitles of "Master", "Miss", and "Mrs" in 3rd class with
#          family sizes not equal to 5, 6, 8, and 11 are predicted to survive
#          with 66.27% accuracy.

rpart.train.2 <- combined[1:891, c("Pclass", "SimpleTitle", "PartySize", "AverageFare")]
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.1)
rpart.2.cv.1
prp(rpart.2.cv.1$finalModel, type=0, extra=1, under=T)
rpart.2.cv.1$finalModel
# The plot brings out some interesting lines of investigation. Namely:
#      1 - SimpleTitles of "Mr" are predicted to perish at an
#          overall accuracy rate of 80.93%
#      2 - SimpleTitles of "Master", "Miss", and "Mrs" in 1st and 2nd class
#          are predicted to survive at an overall accuracy rate of 94.8%
#      3 - SimpleTitles of "Master", "Miss", and "Mrs" in 3rd class with
#          a party size greater than 4.5 are predicted to perish
#          with 92.68% accuracy.
#      4 - SimpleTitles of "Master", "Miss", and "Mrs" in 3rd class with
#          a party size greater than 4.5 and an average fare greater than or 
#          equal to 8 are predicted to survive with 58.9% accuracy.


########################################################################
##
##  Generate Submission
##
########################################################################

# Collect the test data with all the new features I've built.
test.submission <- combined[892:1309, c("Pclass", "SimpleTitle", "PartySize", "AverageFare")]

# Predict whether the test data individuals survived or perished
rpart.2.predictions <- predict(rpart.2.cv.1$finalModel, test.submission, type="class")
# Should be 418 in total
#  - 266 perished
#  - 152 survived
length(rpart.2.predictions)
table(rpart.2.predictions)

# Build and format the actual submission data according to requirements
# +-------------+----------+
# | PassengerId | Survived |
# +-------------+----------+
# | xxxx        | 0 or 1   |
# +-------------+----------+
submission <- data.frame(PassengerId=rep(892:1309), Survived=rpart.2.predictions)

# Write the submission out to CSV with no row names.
filename <- paste(c("TITANIC-", format(now(), "%Y%m%d_%H%M%S"), ".csv"), sep="", collapse="")
write.csv(submission, file=filename, row.names=FALSE)

# Compare results for improvements
results1 <- read.csv("TITANIC-20161211_110400.csv")
results2 <- read.csv("TITANIC-20161211_130551.csv")
# If there are any FALSE values then there was a change in the results.
ifelse(identical(results1, results2), "There were no changes to the results", "There were some changes to the results")
# Cleanup
rm(results1, results2)
