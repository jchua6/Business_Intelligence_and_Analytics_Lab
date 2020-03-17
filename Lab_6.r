#The purpose of this lab is to build decision trees using two different methods. 
#We will also see that R isable to build a decision tree using both categorical and numeric attributes. 
#We will also calculate theinformation gain of different attributes regarding the target class.
#We will randomly divide the dataset into two sets: train data and test data. ]
#We build the model on traindata and test its performance on test data.
#The dataset is about 200 customers that applied for a loan. 
#Each customer was either approved or notapproved. So, the value of the target class is either Yes or No.
#We have the following attributes:Risk (categorical)Length (numerical) (Length of membership in months)Credit (categorical)Balance (numerical)Approved which is the target class and has two values: Yes//o

cdata <- read.csv(file="~/Desktop/CreditData.csv")
View(cdata)

set.seed(123)

myindex <- sample(1:nrow(cdata), 0.8*nrow(cdata))

trainData <- cdata [myindex,]
View(trainData)

testData <-cdata[-myindex, ]
View(testData)

install.packages("RWeka")
library(RWeka)

install.packages("partykit")
library(partykit)

#use all attributes to predict value of the target class
myFormula <- Approved ~ .

#Calculate Information Gain
weights <- InfoGainAttributeEval(myFormula, data = trainData)
barplot(weights, las = 0)

#CTREE
Ctree <- ctree(myFormula, data = trainData)
plot(Ctree)
testCTree <- predict(Ctree, newdata = testData)
table (testCTree, testData$Approved)

#J48TREE
J48Tree <- J48(myFormula, data=trainData)
plot(J48Tree)
testJ48Tree <- predict(J48Tree, newdata= testData)
table(testJ48Tree,testData$Approved)


