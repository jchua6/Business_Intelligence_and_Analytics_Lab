#The purpose of this lab is to build decision trees using 
#The dataset is about a set of cars with different features (e.g., number of doors, ...). 
#Our goal is to predictthe target class which shows the condition of the car.
#We will use different features to build differentdecision trees
#A training dataset is provided and is used to build the model. 
#A test dataset is also provided to evaluatethe accuracy of different models.

trainData <- read.csv("trainData.csv")
View(trainData)

testData <- read.csv("testData.csv")
View(testData)

install.packages("party")
library(party)

#Tree 1: First Determine your formula consists of target value and informative attributes 

formula_1 <- target_class ~ buying
tree_1 <- ctree(formula_1, data = trainData)
plot(tree_1)
print(tree_1)

testPredict_1 <- predict(tree_1, newdata = testData)
#below code shows you what the target class really was and what model predicted
table(testPredict_1, testData$target_class)

#Tree 2
formula_2 <- target_class ~ persons + lug_boot + safety
tree_2 <- ctree(formula_2, data = trainData)
plot(tree_2)

testPredict_2 <- predict(tree_2, newdata = testData)
table(testPredict_2, testData$target_class)

#tree 3: considering all attributes
formula_3 <- target_class ~ buying + maint + doors + persons + lug_boot + safety
tree_3 <- ctree(formula_3 , data= trainData)
plot(tree_3)

testPredict_3 <- predict(tree_3, newdata = testData)
table(testPredict_3, testData$target_class)

#final comparison
table(testPredict_1,testData$target_class)
table(testPredict_2,testData$target_class)
table(testPredict_3,testData$target_class)




