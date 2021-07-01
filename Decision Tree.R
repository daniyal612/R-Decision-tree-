
#import dataset
movie <- read.csv("E:/Machine Learning With R/Data Files/Data Files/Decision Tree Dataset/Movie_regression.csv")
View(movie)

#Data Preprocessing
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE)

# Test-Train Split
library(caTools)
set.seed(0)
split =sample.split(movie,SplitRatio = 0.8)
train = subset(movie,split == TRUE)
test = subset(movie,split == FALSE)

#install required packages
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

# Run regression tree model on train set
regtree <- rpart(formula = Collection~., data = train, control = rpart.control(maxdepth = 3))
# press F1 on rpart for help on this function

# plot the Decision Tree
rpart.plot(regtree, box.palette ="RdBu", digits = -3)

# Predict value at any point
test$pred <- predict(regtree, test, type = "vector")

MSE2 <- mean((test$pred - test$Collection)^2)

# Tree Pruning
fulltree <- rpart(formula = Collection~., data = train, control = rpart.control( cp = 0))
rpart.plot(fulltree, box.palette ="RdBu", digits = -3)
printcp(fulltree)
plotcp(regtree)

mincp <- regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]

prunedtree <- prune(regtree, cp = mincp)
rpart.plot(prunedtree, box.palette = "RdBu", digits = -3)

test$fulltree <- predict(fulltree, test, type = "vector")
MSE2full <- mean((test$fulltree - test$Collection)^2)

test$pruned <- predict(prunedtree, test, type = "vector")
MSE2pruned <- mean((test$pruned - test$Collection)^2)



accuracy_postprun <- mean(test$pred == test$left)



# Classification

df <- read.csv("E:/Machine Learning With R/Data Files/Data Files/Decision Tree Dataset/Movie_classification.csv")
View(df)

#Data Preprocessing
summary(df)
df$Time_taken

#Test-Train split
library(caTools)
set.seed(0)
split =sample.split(movie,SplitRatio = 0.8)
trainc = subset(df,split == TRUE)
testc = subset(df,split == FALSE)

#install required packages
library(rpart)
library(rpart.plot)

#Run classification tree model on train set
classtree <- rpart(formula = Start_Tech_Oscar~., data = trainc, method = 'class', control = rpart.control(maxdepth = 3))
#press F1 on rpart for help on this function

#Plot the Decision Tree
rpart.plot(classtree, box.palette = "RdBu", digits = -3)

#Predict value at any point
testc$pred <- predict(classtree, testc, type = "class")

table(testc$Start_Tech_Oscar,testc$pred)
63/112


# Bagging #################
###########################

install.packages('randomForest')
library(randomForest)
set.seed(0)

bagging =randomForest(Collection~., data = train ,mtry = 17)
test$bagging <- predict(bagging, test)
MSE2bagging <- mean((test$bagging - test$Collection)^2)


# Random Forest
library(randomForest)

randomfor <- randomForest(Collection~., data = train,ntree=500)
# Predict Output
test$random <- predict(randomfor, test)
MSE2random <- mean((test$random - test$Collection)^2)


#Boosting
install.packages('gbm')
library(gbm)
set.seed(0)
boosting = gbm(Collection~., data = train, distribution = "gaussian",n.trees = 5000 , interaction.depth = 4, shrinkage = 0.2,verbose = F)
#distribution = 'Gaussian' for regression and 'Bernoulli' for classification
test$boost = predict (boosting, test, n.trees =5000)
MSE2boost <- mean((test$boost - test$Collection)^2)

#Adaboost

install.packages('adabag')
library(adabag)

trainc$Start_Tech_Oscar1 <- as.factor(trainc$Start_Tech_Oscar)

adaboost <- boosting(Start_Tech_Oscar1~.-Start_Tech_Oscar, data=trainc, boos = TRUE,mfinal = 1000)

predada <- predict(adaboost,testc)
table(predada$class,testc$Start_Tech_Oscar)
70/113
77/113


t1<-adaboost$trees[[1]]
plot(t1)
text(t1,pretty=100)

# XGBOOST

install.packages("xgboost")
library(xgboost)

trainY = trainc$Start_Tech_Oscar == "1"

trainX <- model.matrix(Start_Tech_Oscar ~ .-1, data = trainc)
trainX <- trainX[,-12]

testY = testc$Start_Tech_Oscar == "1"

testX <- model.matrix(Start_Tech_Oscar ~ .-1, data = testc)
testX <- testX[,-12]
#delete additional variables

Xmatrix <- xgb.DMatrix(data = trainX, label = trainY)
Xmatrix_t <- xgb.DMatrix(data = testX, label = testY)

Xgboosting <- xgboost(data = Xmatrix, # the data
                      nround = 50, # max number of boosting iterations
                      objective = "multi;softmax",eta = 0.3, num_class = 2, max_depth = 100)

xpred <- predict(Xgboosting, Xmatrix_t)
table(testY,xgpred)

74/113

































