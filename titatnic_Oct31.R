#Created By: Nalina Varanasi 
#Created: Oct 31, 2015
#Titanic Kaggle Competition
#Reference: http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r

#Set path and import data files
path <- "C:\\Users\\nvarana\\Desktop\\Training\\datasciencedojo\\"
train <- read.csv(paste(path, "data\\train.csv", sep=""), stringsAsFactors=F) 
train <- train[!is.na(train$PassengerId),]
test <- read.csv(paste(path, "data\\test.csv", sep=""), stringsAsFactors=F) 

#Load packages required for decision tree and random forest
library(rpart)
library(randomForest)
library(party)

#Combine train and test data for variable pre-processing
test$Survived <- NA
d <- rbind(train, test)
colnames(d) <- tolower(colnames(d))

#Create variable Title - folks with royal title are likely to have survived due to preference in getting off the ship to rescue boats
d$title <- sapply(d$name, FUN=function (x) {strsplit(x, split='[,.]')[[1]][2]})
d$title <- gsub(' ', '', d$title)
d$title[d$title %in% c('Mme', 'Mlle', 'Ms')] <- 'Mlle'
d$title[d$title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
d$title[d$title %in% c('Dona', 'Lady', 'theCountess', 'Jonkheer')] <- 'Lady'

#Create variable family size
d$fsize <- d$sibsp + d$parch + 1

#Create family id
d$lname <- sapply(d$name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
d$famid <- paste(as.character(d$fsize), d$lname, sep="")
d$famid[d$fsize<=2] <- 'small'
f <- data.frame(table(d$famid))
f <- f[f$Freq<=2,]
d$famid[d$famid %in% f$Var1] <- 'small'

#Convert family id to factor
d$famid <- as.factor(d$famid)

#Impute missing Age 
summary(d$age)
agefit <- rpart(age ~ pclass + sex + sibsp + parch + fare + embarked + title + fsize, data=d[!is.na(d$age),], method="anova")
d$age[is.na(d$age)] <- predict(agefit, d[is.na(d$age),])

#Check what else is missing in d and impute them
summary(d)
d$fare[is.na(d$fare)] <- median(d$fare, na.rm=T)
d$embarked[d$embarked==""] <- 'S'

#Reduce levels for family ID
d$famid2 <- d$famid
d$famid <- as.character(d$famid)
d$famid2[d$fsize<=3] <- 'small'
d$famid2 <- as.factor(d$famid2)

#Add if the passengers loaded the lifeboats from port side or starboard side
#The instructions for male passengers were different in the starboard and port side
#In the port side male passengers were allowed if there was space in the lifeboats once women and children were loaded
#In the starboard side even if there was space - they were not allowed to board the life boat. As a result several life boats
#had several seats unoccupied


#Did it matter if the male on the ship were crew members - were most of them male? 
#Then it probably would not have 


#Split back to test and train sets
d$survived <- as.factor(d$survived)
d$pclass <- as.factor(d$pclass)
d$sex <- as.factor(d$sex)
d$embarked <- as.factor(d$embarked)
d$title <- as.factor(d$title)
d$famid <- as.factor(d$famid)
str(d)
train <- d[!is.na(d$survived),]
test <- d[is.na(d$survived),]

#Build a random forest ensemble
set.seed(415)

fit <- randomForest(as.factor(survived) ~ pclass + sex + age + sibsp + parch + fare + embarked + title + fsize, data=train, importance=T, ntree=2000) 
  
#Look at the variable importance
varImpPlot(fit)
#Now let's make a prediction and write a submission file
prediction <- predict(fit, test)
submit <- data.frame(PassengerId=test$passengerid, survived=prediction)
write.csv(submit, file=paste(path, "titanic_forest.csv", row.names=F))

#build a conditional inference tree random forest
set.seed(415)
fit <- cforest(as.factor(survived)~pclass+sex+age+sibsp+parch+fare+embarked+title+fsize+famid, data=train, controls=cforest_unbiased(ntree=2000, mtry=3))
prediction <- predict(fit, test, OOB=T, type="response")
submit <- data.frame(PassengerId=test$passengerid, Survived=prediction)
write.csv(submit, file=paste(path, "results\\titanic_ciforest.csv", sep=""), row.names=F)



















































