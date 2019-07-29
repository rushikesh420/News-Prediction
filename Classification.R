
library(caret)
options(warn=-1)
news<-read.csv("E:/UT Dallas/JSOM/Fall 18/MIS 6356 BA with R/Project/Dataset/OnlineNewsPopularity_Original.csv")
#summary(news)


#DATA PREPROCESSING
#Remove missing data i.e remove rows with n_token_content as 0. 
#1181 rows are removed
modifiedData<-news[news[,4]!=0,] 
dim(modifiedData)
#Removing outlier - delete rows with value >1 for n_non_stop_words. 1 row is removed
modifiedData<-modifiedData[modifiedData[,6]<=1,]
#Plot the target variable
par(mfrow=c(1,1))	  #Use this to display only one plot in the page
hist(modifiedData$shares,main=" Histogram on number of shares ", xlab=" Number of shares ", ylab=" Frequency " ,sub="Before transformation")
boxplot(modifiedData$shares,names = "Shares",main="Box plot on number of shares",
        ylab="Number of shares",sub="Before transformation")

#Log transformation for the target variable and plot again
modifiedData$normalizedshares<-log(modifiedData$shares)
boxplot(modifiedData$normalizedshares,names = "Shares",main="Box plot on log values of number of shares",
        ylab="Number of shares",sub="After transformation")
hist(modifiedData$normalizedshare,main="Histogram on number of shares",
     xlab="Normalized number of shares",ylab="Frequency",sub="After transformation")

#Remove outliers in target variable
boxplotstats<-boxplot(modifiedData$normalizedshare)$stats
print(boxplotstats)
#stats returns a vector of length 5, containing the extreme of the lower whisker, the lower hinge, the median, the upper hinge and the extreme of the upper whisker. Each of these is assigned to a separate variable

minimum<-boxplotstats[1,1]
lowerhinge<-boxplotstats[2,1]
median<-boxplotstats[3,1]
upperhinge<-boxplotstats[4,1]
maximum<-boxplotstats[5,1]

#Calculation for fence to separate mild and extreme outliers 

hspread<-upperhinge-lowerhinge
lowerouterfence<-lowerhinge-3*hspread
upperouterfence<-upperhinge+3*hspread
print(hspread)
print(lowerouterfence)
print(upperouterfence)

#Removing values beyond the fences, i.e. extreme outliers. 133 rows are removed

modifiedData<-modifiedData[!(modifiedData$normalizedshare>=upperouterfence | modifiedData$normalizedshare<=lowerouterfence),]
boxplot(modifiedData$normalizedshare,xlab="Normalized Shares",main="Shares after extreme oulier removal")

#Categorizing the target variable.

#Two categories (Flop: 0, Hit: 1)
modifiedData$popularity<-cut(modifiedData $normalizedshare,c(lowerouterfence,median,upperouterfence),labels=c(0,1))
table(modifiedData $popularity)
# treat Popularity as categorical (R will create dummy variables)
modifiedData$popularity <- as.factor(modifiedData$popularity)


#Remove separate days columns (just keeping weekdays or not columns)
#Remove the newly created column news_day
modifiedData <- modifiedData[,-c(1,2,32:38,61,62)]

# partition data
set.seed(1234)
numberOfRows <- nrow(modifiedData)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- modifiedData[train.index,]
valid.df <- modifiedData[-train.index,]


#DECISION TREE
library(rpart)
fit <- rpart(popularity~., data = train.df, method = 'class')
predict_unseen <-predict(fit, valid.df, type = 'class')
confusionMatrix(predict_unseen, valid.df$popularity)


#Random Forest
require(randomForest)
rf = randomForest(popularity~ . , train.df, type = 'classification', importance = TRUE, ntree = 51)
predict_unseen <-predict(rf, valid.df, type = 'class')
confusionMatrix(predict_unseen, valid.df$popularity)


#Scaling data for LogRegression
modifiedScaledData <- modifiedData
modifiedScaledData[c(1:11,18:51)] <- lapply(modifiedScaledData[c(1:11,18:51)], function(x) c(scale(x)))
#modifiedScaledData <- scale(modifiedData)
#modifiedScaledData <- as.data.frame(modifiedScaledData)
#partition data after scaling
set.seed(1234)
numberOfRows <- nrow(modifiedScaledData)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- modifiedScaledData[train.index,]
valid.df <- modifiedScaledData[-train.index,]


#LOGISTIC REGRESSION
# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logitI.reg <- glm(popularity~., data = train.df, family = "binomial") 
#summary(logitI.reg)
confusionMatrix(table(predict(logitI.reg, newdata = valid.df, type="response") > 0.5, valid.df$popularity == 1))
#summary(logitI.reg)

#KNN
#model_knn <- train(train.df[, 1:51], train.df[, 52], method='knn')
#predict_unseen <- predict(object=model_knn, valid.df[,1:51])
# Confusion matrix 
#confusionMatrix(predict_unseen,valid.df[,52])

