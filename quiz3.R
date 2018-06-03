### Quiz 3

  #install.packages("AppliedPredictiveModeling", Dependencies = TRUE)
  #install.packages("caret", Dependencies = TRUE)
  #install.packages("ElemStatLearn", Dependencies = TRUE)
  #install.packages("pgmm", Dependencies = TRUE)
  #install.packages("rpart", Dependencies = TRUE)

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)



inTrain <- createDataPartition(y = segmentationOriginal$Case, p = 0.6, 
                               list = FALSE) # 60% training
training <- segmentationOriginal[inTrain, ]
testing <- segmentationOriginal[-inTrain, ]
# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. (The outcome class is contained in a factor variable called Class with levels "PS" for poorly segmented and "WS" for well segmented.)
set.seed(125)
modFit <- train(Class ~ ., method = "rpart", data = training)

modFit$finalModel

fancyRpartPlot(model$finalModel)


###question 3
library(pgmm)
data(olive)
olive = olive[,-1]
model<-train(Area ~ ., data=olive, method="rpart")

newdata = as.data.frame(t(colMeans(olive)))

predict(model, newdata)


### Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)

#SAheart<-SAheart[c("age", "alcohol", "obesity", "tobacco", "typea", "ldl", "chd")]

trainSA = SAheart[train,]
testSA = SAheart[-train,]


set.seed(1234)
model <- train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family="binomial")
missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predict(model, trainSA))


missClass(testSA$chd, predict(model, testSA))



#### Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)


vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
library(randomForest)


modvowel <- randomForest(y ~ ., data = vowel.train)
order(varImp(modvowel), decreasing = T)

