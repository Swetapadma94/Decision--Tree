fraud<-read.csv("E:\\Assignment\\Decession Tree\\Fraud_check.csv")
View(fraud)
library(caret)
library(C50)
library(party)
## Converting taxable_income <= 30000 as "Risky" and others are "Good"

fraud$Taxable.Income<-ifelse(fraud$Taxable.Income >30000,"Good", "Risky") 
View(fraud)
class(fraud$Taxable.Income)
fraud$Taxable.Income<-as.factor(fraud$Taxable.Income)
class(fraud$Taxable.Income)
dim(fraud) 
sum(is.na(fraud))
### Data Pationin###
train<- createDataPartition(fraud$Taxable.Income,p = .70,list = F)
train
training<-fraud[train,]
View(training) 
dim(train)
testing<-fraud[-train, ]
View(testing)
dim(testing)

## model Building###
model<-C5.0(training$Taxable.Income~.,data = training)
model
plot(model)
summary(model)
##model2<-ctree(training$Taxable.Income~.,data = training)
#summary(model2)
#plot(model2)
##model1<-C5.0(training$Taxable.Income~.,data = training,trails = 50)
#plot(model1)
## prediction##
predict<-predict.C5.0(model,newdata = testing[ ,-3])
predict
table(predict)
a<-table(testing$Taxable.Income,predict)
a
sum(diag(a)/sum(a))
plot(model)

 ######### Bagging########
acc<-c()
for(i in 1:100)
{
   print(i)
inTraininglocal<-createDataPartition(fraud,p=.75,list=F)
   training1<-iris[inTraininglocal,]
   testing<-iris[-inTraininglocal,]
   
   fittree<-C5.0(training1$Species~.,data=training1)
   pred<-predict.C5.0(fittree,testing[,-5])
   ##pred1<-predict.C5.0(fittree,testing)
   a<-table(testing$Species,pred)
   
   acc<-c(acc,sum(diag(a))/sum(a))
   
}

acc
summary(acc)


