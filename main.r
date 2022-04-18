###Vowel Recognition
##Trying out several different methods and evaluating their performance
library("OpenML")
library(MASS)
library(gam)
library(tree)
library(randomForest)
library(class)
library(klaR)
library(e1071)
library(neuralnet)
library(tidyverse)

nor = function(x){
  (x-min(x)/max(x)-min(x))
}



setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f")
data = getOMLDataSet(data.id=58L)$data
train = data.frame()
test = data.frame()
for(i in 1:990){
  if (data$Train_or_Test[i]=='Train'){
    train= rbind(train,data[i,])
  }else{
    test= rbind(test,data[i,])
  }
}
train$Speaker_Number=NULL
test$Speaker_Number=NULL
trainNorm = as.data.frame(lapply(train[,c(3,4,5,6,7,8,9,10,11,12)],nor))
testNorm = as.data.frame(lapply(test[,c(3,4,5,6,7,8,9,10,11,12)],nor))
trainNorm$Train_or_Test=train$Train_or_Test
trainNorm$Sex=train$Sex
trainNorm$Class=train$Class
testNorm$Train_or_Test=test$Train_or_Test
testNorm$Sex=test$Sex
testNorm$Class=test$Class
#Multivariate Linear Discriminant Analysis
lda.fit = lda(Class~.-Train_or_Test,data=train)
ldaTestPred = predict(lda.fit,test)$class
ldaTestErr = 1-mean(ldaTestPred == test$Class)
table(ldaTestPred,test$Class)
#We encounter a first test error of 55.4% here

#Multivariate Linear Discriminant Analysis normalized Data
ldaNorm.fit = lda(Class~.-Train_or_Test,data=trainNorm)
ldaNormTestPred = predict(ldaNorm.fit,testNorm)$class
ldaNormTestErr = 1-mean(ldaNormTestPred == testNorm$Class)
table(ldaNormTestPred,testNorm$Class)
#We encounter a test error of 89% here

#Quadratic Discriminant Analysis
qda.fit = qda(Class~.-Train_or_Test,data=train)
qdaTestPred = predict(qda.fit,test)$class
qdaTestErr = 1-mean(qdaTestPred == test$Class)
table(qdaTestPred,test$Class)
#We encounter an even worse test error of 58.9%
#From these two methods we can conclude that these methods aren't of use at all

#Regularized Discriminant Analysis
rda.fit = rda(Class~.-Train_or_Test,data=train,crossval=TRUE,fold=10)
rdaTestPred = predict(rda.fit,test)$class
rdaTestErr = 1-mean(rdaTestPred == test$Class)

#Trees
train$Speaker_Number = NULL
test$Speaker_Number = NULL
tree=tree(Class~.-Train_or_Test,train)
plot(tree)
text(tree)
tree.pred = predict(tree,test,type="class")
table(tree.pred,test$Class)
treeTestErr = 1-mean(tree.pred == test$Class)
#Test Error of nearly 62%
#try tree pruning
set.seed(1337)
cv.tree = cv.tree(tree,FUN=prune.misclass)
par(mfrow=c(1,2))
plot(cv.tree$size,cv.tree$dev,type="b")
plot(cv.tree$k,cv.tree$dev,type="b")
prune.tree = prune.misclass(tree,best=23)
pruneTree.pred = predict(prune.tree,test,type="class")
pruneTreeTestErr = 1-mean(pruneTree.pred == test$Class)
#Test error reduced to ~60%

#Random Forest
randFor = randomForest(Class~.-Train_or_Test,data=train,importance=TRUE)
randFor.pred = predict(randFor,test,type="class")
randFor.TestErr = 1-mean(randFor.pred == test$Class)
#Here we achieve our lowest test error yet 40,3% !

#Bagging
bag = randomForest(Class~.-Train_or_Test,data=train,mtry=11,importance=TRUE)
bag.pred = predict(bag,test, type="class")
bagTestErr= 1-mean(bag.pred == test$Class)
#When we consider every predictor at every split we get a test error of 50,6 %


#Multiclass SVM
svm1 = svm(Class~.,data=train,gamma=0.083,cost=100)
svm1.pred = predict(svm1,test, type="class")
svm1TestErr= 1-mean(svm1.pred == test$Class)
#we achieve a test error of 30%!

#Neural Network Approaches
#Naive
data = getOMLDataSet(data.id=58L)$data
data = data %>% mutate(Class=as_factor(Class))
train$Sex = as.numeric(train$Sex=="Female")
test$Sex = as.numeric(test$Sex=="Female")
nn1 = neuralnet(Class~.-Train_or_Test,data=train,hidden=c(2,2),stepmax = 1e+06,threshold=0.01,linear.output = FALSE)


nn1.pred = predict(nn1,test)
table(test$Class, apply(nn1.pred,1,which.max))
#absolutely horrendous
#it's time to look at papers on this topic
