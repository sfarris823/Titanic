# reset environmental
rm(list=ls())

# load needed libraries
library(dplyr)           #data manipulation
library(ggplot2)         #simple plots
library(randomForest)    #exploratory modeling
library(caret)           #cross validation
library(doSNOW)          #clustering for performance
library(rpart)
library(rpart.plot)

##
## Custom functions
##

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

rpart.cv <- function(seed, training, labels, ctrl)
{
  set.seed(seed)
  rpart.cv <- train(x=training, y=labels, method="rpart", tuneLength=30, trControl=ctrl)
  return (rpart.cv)
}


##
## Get data
##

# Read the train and test csv files into R
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

# I need to combine the test and train data sets, but need to add Survived to test first.
temp.test <- test
temp.test$Survived <- "Unk"
test <- select(temp.test, PassengerId, Survived, Pclass:Embarked)
rm (temp.test)
combined <- rbind(train, test)

# Convert a few columns to factors so they can be analyzed using ggplot
combined$Survived <- as.factor(combined$Survived)
combined$Pclass <- as.factor(combined$Pclass)
combined$Sex <- as.factor(combined$Sex)
combined$Embarked <- factorit(combined$Embarked)
combined$SibSp <- factorit(combined$SibSp)
combined$Parch <- factorit(combined$Parch)

##
## Add new features
##
 
# Simplify the Titles into Master, Mr, Miss, and Mrs
combined$Title <- gsub('(.*, )|(\\..*)', '', combined$Name)
combined$SimpleTitle <- combined$Title
# Simplify Mr titles
combined$SimpleTitle[which(combined$SimpleTitle=="Don")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Capt")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Col")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Major")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Jonkheer")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Sir")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Rev")] <- "Mr"
# Simplify Miss titles
combined$SimpleTitle[which(combined$SimpleTitle=="Mlle")] <- "Miss"
combined$SimpleTitle[which(combined$SimpleTitle=="Ms")] <- "Miss"
# Simplify Mrs titles
combined$SimpleTitle[which(combined$SimpleTitle=="Dona")] <- "Mrs"
combined$SimpleTitle[which(combined$SimpleTitle=="Lady")] <- "Mrs"
combined$SimpleTitle[which(combined$SimpleTitle=="Mme")] <- "Mrs"
combined$SimpleTitle[which(combined$SimpleTitle=="the Countess")] <- "Mrs"
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

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(combined$Ticket == current.ticket)
  current.avg.fare <- combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

combined$PartySize <- ticket.party.size
combined$AverageFare <- avg.fare

##
##  Explore the data with plots
##

# Play with plots
showhist(combined[1:891,], "SimpleTitle", "Survived", ~Pclass, "Survival Rates by Passenger Class and Simplified Title")
showhist(combined[1:891,], "Sex", "Survived", ~Pclass, "Survival Rates by Passenger Class and Sex")
showhist(combined[1:891,], "Embarked", "Survived", ~Pclass, "Survival Rates by Passenger Class and Embarked")
showhist(combined[1:891,], "Embarked", "Survived", ~Pclass+Sex, "Survival Rates by Passenger Class/Sex and Embarked")
showhist(combined[1:891,], "SibSp", "Survived", ~Pclass, "Survival Rates by Passenger Class and SibSp")
showhist(combined[1:891,], "SibSp", "Survived", ~Pclass+Sex, "Survival Rates by Passenger Class/Sex and SibSp")
showhist(combined[1:891,], "Parch", "Survived", ~Pclass, "Survival Rates by Passenger Class and Parch")
showhist(combined[1:891,], "Parch", "Survived", ~Pclass+Sex, "Survival Rates by Passenger Class/Sex and Parch")
showhist(combined[1:891,], "Sex", "Survived", ~Pclass+Embarked, "Survival Rates by Passenger Class/Embarked and Sex")

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
set.seed(1234)

rf.1 <- randomForest(x = combined[1:891, c("Pclass", "SimpleTitle", "FamilySize")], 
                     y = as.factor(train$Survived), 
                     importance = T, 
                     ntree = 500)
rf.1
varImpPlot(rf.1)

rf.2 <- randomForest(x = combined[1:891, c("Pclass", "SimpleTitle", "PartySize", "AverageFare")], 
                     y = as.factor(train$Survived), 
                     importance = T, 
                     ntree = 500)
rf.2
varImpPlot(rf.2)

# Cross Validation
set.seed(37596)
rf.label <- as.factor(train$Survived)
cv.10.folds <- createMultiFolds(rf.label, k = 3, times = 10)


ctrl.1 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.10.folds)

# not clustering due to PC power.
#mycluster <- makeCluster(2, type="SOCK")
#registerDoSNOW(mycluster)

set.seed(94622)
rf.1.cv.1 <- train(x=combined[1:891, c("Pclass", "SimpleTitle", "FamilySize")], 
                   y=rf.label, 
                   method="rf",
                   tuneLength=3, 
                   ntree=64, 
                   trControl=ctrl.1)

# not clustering due to PC power.
#stopCluster(mycluster)
rf.1.cv.1

set.seed(94622)
rf.2.cv.1 <- train(x=combined[1:891, c("Pclass", "SimpleTitle", "PartySize", "AverageFare")], 
                   y=rf.label, 
                   method="rf",
                   tuneLength=3, 
                   ntree=64, 
                   trControl=ctrl.1)

# not clustering due to PC power.
#stopCluster(mycluster)
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
