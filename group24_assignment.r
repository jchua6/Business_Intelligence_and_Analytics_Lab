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
tree2<-rpart(formula1, train)
rpart.plot(tree2,extra = 4)
RPartPred<- predict(tree2, test,type = 'class')
RPArtConfMatrix<-table(RPartPred,test$Subscribed) 
rpart_accuracy <- sum(diag(RPArtConfMatrix))/sum(RPArtConfMatrix)
print(rpart_accuracy) #14% really really bad so we will disregard this model

#KNN
str(train)
library(class)
as.numeric(train$education)
as.numeric(train$duration)
as.numeric(test$education)
as.numeric(test$duration)
knn_train<- cbind(train$education,train$duration)
knn_test<- cbind(test$education,test$duration)
target<- train$Subscribed
test_target<-test$Subscribed
set.seed(1)
dim(train)
dim(test)

pred = knn(knn_train,knn_test,target,k=250)
table(pred,test_target) 
mean(pred==test_target)
#87% accuracy
