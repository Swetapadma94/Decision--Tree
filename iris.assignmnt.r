data("iris")
View(iris)
install.packages("party")
library(party)
class(iris$Species)
library(caret)
install.packages("plyr")
library(plyr)
sum(is.na(iris ))
### partitioning the data######
train<-createDataPartition(iris$Species,p = .75,list = F)
train
training<-iris[train, ]
testing<-iris[-train, ]
testing
#overall descriptives of the dataset 
summary(iris)
#### Building tree##
tree1<-ctree(iris$Species~.,data = training)
plot(tree1)
summary(tree1)
##tree2<-ctree(iris$Petal.Length~., data = training)
### prediction##

predict<-predict(tree1,testing[ ,-5])
summary(predict)
table(predict)
a<-table(testing$Species,predict)
a
sum(diag(a)/sum(a))
###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(iris$Species,p=.85,list=F)
  training1<-iris[inTraininglocal,]
  testing<-iris[-inTraininglocal,]
  
  fittree<-ctree(training1$Species~.,data=training1)
  pred<-predict(fittree,testing[,-5])
  
  x<-table(testing$Species,pred)
  
  acc<-c(acc,sum(diag(x))/sum(x))
  
}

acc
summary(acc)
confusionMatrix(pred,testing$Species)
