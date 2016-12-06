# reset environmental
rm(list=ls())

# load needed libraries
library(dplyr)
library(ggplot2)

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
