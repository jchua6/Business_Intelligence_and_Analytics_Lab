#Task 1 - Dicide data into two datasets 75% training 25% test
creditdata <- read.csv(file = "~/Desktop/CreditData.csv")
View(creditdata)

smpsize <- floor(0.75 * nrow(creditdata))
set.seed(123)
training_ind <- sample(seq_len(nrow(creditdata)), size = smpsize)

train_data <- creditdata[training_ind, ]
View(train_data)

test_data <- creditdata[-training_ind, ]
View(test_data)

#Task 2 - Build a classification model based on the training data to predict if a new customer is approved or not.

#Decision Tree

str(train_data)
formula1 <- Approved ~ .

#CTree
library(party)
tree <-
  ctree(formula1,
        data = train_data,
        controls =  ctree_control(mincriterion = 0.95, minsplit = 100))
plot(tree)
predict(tree, test_data, type = "prob")
test_ctree <- predict(tree, newdata = test_data)

#RPart
library(rpart)
library(rpart.plot)
tree2 <- rpart(formula1, train_data)
rpart.plot(tree2, extra = 4)
RPartPred <- predict(tree2, test_data)

#Task 3

#training misclassification
ConfMatrix <- table(predict(tree), train_data$Approved)
print(ConfMatrix)
1 - sum(diag(ConfMatrix)) / sum(ConfMatrix)
#misclassification error for CTree is about 23%

#testing misclassification error
testpred <- predict(tree, newdata <- test_data)
ConfMatrix2 <- table(testpred, test_data$Approved)
print(ConfMatrix2)
1 - sum(diag(ConfMatrix2) / sum(ConfMatrix2))
#test misclassification error is about 25% 
