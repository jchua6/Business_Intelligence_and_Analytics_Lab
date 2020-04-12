#Data Exploration + Processing
train_dirty <- read.csv(file = "~/Desktop/trainset.csv")
View(train_dirty)
str(train_dirty)#used str() to determine which columns contain "unknown" variable"

#varibale housing and loan will be used to determine the mode of both columns, because the columns values are non numeric we cannot use mean or median

View(table(train_dirty$job))
View(table(train_dirty$marital))
View(table(train_dirty$education))
housing_dirty<-table(train_dirty$housing)
View(housing_dirty)

loan_dirty<-table(train_dirty$loan)
View(loan_dirty)

#removing unknowns
library(dplyr)

#new dataframe which does not include "unknowns"
train<-train_dirty%>%mutate(job=recode(job,"unknown" = "admin"), 
                            marital=recode(marital,"unknown" = "married"),
                            education=recode(education,"unknown" = "university.degree"),
                            housing=recode(housing,"unknown"= "yes"),
                            loan=recode(loan,"unknown" = "no"))

View(train)
loan2<-table(train$loan)
housing2<- table(train$housing)
View(loan2)
View(housing2)
View(table(train$marital))

#testdata
testing_dirty<-read.csv(file='~/Desktop/testset.csv')

test<-testing_dirty%>%mutate(job=recode(job,"unknown" = "admin"), 
                     marital=recode(marital,"unknown" = "married"),
                     education=recode(education,"unknown" = "university.degree"),
                     housing=recode(housing,"unknown"= "yes"),
                     loan=recode(loan,"unknown" = "no"))

#Decision Tree 1: CTree
library(party)
library(partykit)

formula1<-Subscribed~.
tree1<-ctree(formula1,
             data = train,
             control =  ctree_control(mincriterion = 0.95, minsplit = 1000))
plot(tree1)
predict(tree1,test, type = 'prob')
test_ctree<- predict(tree1,newdata = test)

ctree_confmatrix<-table(predict(tree1),train$Subscribed)
print(ctree_confmatrix)
ctree_accuracy<- sum(diag(ctree_confmatrix))/sum(ctree_confmatrix)
print(ctree_accuracy) #96%

#Decision Tree 2: R Part

library(rpart)
library(rpart.plot)

RPFit <- rpart(Subscribed ~ duration +  marital + education,
               method = "class",
               parms =  list(split = "information"),
               data = train,
               control = rpart.control(cp=0.00093867, 
                                       minsplit = 2, 
                                       minbucket = 5, 
                                       maxdepth = 10,
                                       xval = 10))
               
rpart.plot(RPFit,
           cex = .5,
           extra = 4)

#cp is complexity parameter, gives quality of split. any variable which will reduce the error of .2 will give the split
#cross validation exercise
printcp(RPFit)
plotcp(RPFit)

#tuning
tree.fit<-prune(RPFit,
                cp= 0.00344180)

rpart.plot(tree.fit,
           cex = .5,
           extra = 4)
#prediction
RPPredict<-predict(RPFit,
                   test,
                   type = "class")

table(test$Subscribed,RPPredict)
sum(diag(table(test$Subscribed,RPPredict)))/sum(table(test$Subscribed,RPPredict))

#KNN education & duration
str(train)
library(class)
a<-as.numeric(train$education)
b<-as.numeric(train$duration)
c<-as.numeric(test$education)
d<-as.numeric(test$duration)
knn_train<- cbind(a,b)
knn_test<- cbind(c,d)
target<- train$Subscribed
test_target<-test$Subscribed
set.seed(123)
dim(train)
dim(test)

pred = knn(knn_train,knn_test,target,k=250)

table(pred,test_target) 
mean(pred==test_target)
#87% accuracy

#knn2 nr employed and duration
knn2_train<- cbind(train$nr.employed,train$duration)
knn2_test<- cbind(test$nr.employed,test$duration)
set.seed(123)
pred2<- knn(knn2_train,knn2_test,target,k=250)
table(pred2,test_target)
mean(pred2==test_target) #20% accuracy

#knn3 marital & duration
as.numeric(train$marital)
as.numeric(test$marital)

knn3_train <- cbind(train$marital,train$duration)
knn3_test<- cbind(test$marital,test$duration)
set.seed(123)
pred3<- knn(knn3_train,knn3_test,target,k=300)
table(pred3,test_target)
mean(pred3==test_target) 

#knn4 housing & duration
as.numeric(train$housing)
as.numeric(test$housing)
knn4_train<- cbind(train$housing,train$duration)
knn4_test<- cbind(test$housing,test$duration)
set.seed(1)
pred4<-knn(knn4_train,knn4_test,target,k=250)
table(pred4,test_target)
mean(pred4==test_target) 


#random forest
set.seed(123)
library(randomForest)
rf <- randomForest(Subscribed~ duration +  marital + education,
                   data = train,
                   ntree = 10,
                   mtry= 3,
                   importance = TRUE,
                   proximity = TRUE)
print(rf)
attributes(rf)
rf$confusion
# Prediction and confusion matrix - training data
library(caret)
p1<-predict(rf,train)
head(p1)
head(train$Subscribed)
confusionMatrix(p1,train$Subscribed) 
#prediction and confusion matrix - testing data
p2<- predict(rf,test)
confusionMatrix(p2,test$Subscribed)
plot(rf)

#tune
tuning <-tuneRF(train[,-15],train[,15],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 10,
       trace = TRUE,
       improve = 0.05)
#no of nodes for the trees
hist(treesize(rf),
     main= "No. of Nodes for the Trees",
     col = "blue")
#variable importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main="top 10 variables")
varUsed(rf)

partialPlot(rf,train,nr.employed,"yes")