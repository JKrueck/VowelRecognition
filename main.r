###Vowel Recognition
##Trying out several different methods and evaluating their performance
library("OpenML")
library(MASS)
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
#Multivariate Linear Discriminant Analysis
lda.fit = lda(Class~Sex+Feature_0+Feature_1+Feature_2+Feature_3+Feature_4+Feature_5+Feature_6+Feature_7+Feature_8+Feature_9,data=train)
ldaTestPred = predict(lda.fit,test)$class
ldaTestErr = 1-mean(ldaTestPred == test$Class)
table(ldaTestPred,test$Class)
#We encounter a first test error of 55.4% here

#Quadratic Discriminant Analysis
qda.fit = qda(Class~Sex+Feature_0+Feature_1+Feature_2+Feature_3+Feature_4+Feature_5+Feature_6+Feature_7+Feature_8+Feature_9,data=train)
qdaTestPred = predict(qda.fit,test)$class
qdaTestErr = 1-mean(qdaTestPred == test$Class)
table(qdaTestPred,test$Class)
#We encounter an even worse test error of 58.9%
#From these two methods we can conclude that easier methods aren't of use at all
