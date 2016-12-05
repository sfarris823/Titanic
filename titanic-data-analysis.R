# reset environmental
rm(list=ls())

# load needed libraries
library(dplyr)
library(ggplot2)

# my custom functions

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
  ggplot(data, aes(x = data[,xaxis], fill = data[,fillgrp])) +
    stat_count(width = 0.5) + 
    facet_wrap(facetwrap) +
    ggtitle(title) +
    xlab(xaxis) +
    ylab("Total Count") +
    labs(fill = fillgrp)
}

# read the train and test csv files into R
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

# I need to combine the test and train data sets, but need to add Survived to test first.
temp.test <- test
temp.test$Survived <- "Unk"
test <- select(temp.test, PassengerId, Survived, Pclass:Embarked)
rm (temp.test)
combined <- rbind(train, test)

# convert a few columns to factors so they can be analyzed using ggplot
combined$Survived <- as.factor(combined$Survived)
combined$Pclass <- as.factor(combined$Pclass)
combined$Sex <- as.factor(combined$Sex)
combined$Embarked <- factorit(combined$Embarked)
combined$SibSp <- factorit(combined$SibSp)
combined$Parch <- factorit(combined$Parch)

# add new features
combined$Title <- gsub('(.*, )|(\\..*)', '', combined$Name)
combined$SimpleTitle <- combined$Title
combined$SimpleTitle[which(combined$SimpleTitle=="Don")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Dona")] <- "Mrs"
combined$SimpleTitle[which(combined$SimpleTitle=="Capt")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Col")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Major")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Jonkheer")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Sir")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Lady")] <- "Mrs"
combined$SimpleTitle[which(combined$SimpleTitle=="Mlle")] <- "Miss"
combined$SimpleTitle[which(combined$SimpleTitle=="Mme")] <- "Mrs"
combined$SimpleTitle[which(combined$SimpleTitle=="the Countess")] <- "Mrs"
combined$SimpleTitle[which(combined$SimpleTitle=="Rev")] <- "Mr"
combined$SimpleTitle[which(combined$SimpleTitle=="Ms")] <- "Miss"
combined$SimpleTitle[which(combined$SimpleTitle=="Dr" & combined$Sex=="female")] <- "Mrs"
combined$SimpleTitle[which(combined$SimpleTitle=="Dr" & combined$Sex=="male")] <- "Mr"

# play with plots

train.adj <- combined[1:891,]
showhist(train.adj, "SimpleTitle", "Survived", ~Pclass, "Survival Rates by Passenger Class and Simplified Title")

ggplot(train.adj, aes(x = train.adj[,"SimpleTitle"], fill = train.adj[,"Survived"])) +
  stat_count(width = 0.75) + 
  facet_wrap(~Pclass) 

#showhist(combined[1:891,], "Sex", "Survived", ~Pclass, "Survival Rates by Passenger Class and Sex")
#showhist(combined[1:891,], "Embarked", "Survived", ~Pclass, "Survival Rates by Passenger Class and Embarked")
#showhist(combined[1:891,], "Embarked", "Survived", ~Pclass+Sex, "Survival Rates by Passenger Class/Sex and Embarked")
#showhist(combined[1:891,], "SibSp", "Survived", ~Pclass, "Survival Rates by Passenger Class and SibSp")
#showhist(combined[1:891,], "SibSp", "Survived", ~Pclass+Sex, "Survival Rates by Passenger Class/Sex and SibSp")
#showhist(combined[1:891,], "Parch", "Survived", ~Pclass, "Survival Rates by Passenger Class and Parch")
#showhist(combined[1:891,], "Parch", "Survived", ~Pclass+Sex, "Survival Rates by Passenger Class/Sex and Parch")
#showhist(combined[1:891,], "Sex", "Survived", ~Pclass+Embarked, "Survival Rates by Passenger Class/Embarked and Sex")
