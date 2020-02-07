#importcsv
credit_data <- read.csv("dataset3.csv")
View(credit_data)

mean(credit_data$Score1)
mean(credit_data$Score2)
mean(credit_data$Score3)
mean(credit_data$Score4)

#this returns na
mean(credit_data$CreditType)

#standard deviation
sd(credit_data$Score1)
sd(credit_data$Score3)

#varience
var(credit_data$Score1)
var(credit_data$Score3)

#summary of statistics
summary(credit_data$Score1)

#same package as last week
install.packages("dplyr")
library(dplyr)

#Filtering the table
type1 <- filter(credit_data, CreditType == 'T1')
View(type1)

type2 <- filter(credit_data, CreditType == 'T2')
View(type2)

#the mean of Score1 in entire dataset is different than the mean for type 1
mean(credit_data$Score1)
mean(type1$Score2)

summary(credit_data$Score1)
summary(type1$Score1)

#plots
plot(credit_data$Score1)
plot(type1$Score1)

plot(credit_data$CreditType)

#comparing the outcome of boxplot for different scores
boxplot(credit_data$Score1, ylab = "Score1")
boxplot(credit_data$Score2, ylab = "Score2")

#Histograms
hist(credit_data$Score1)
hist(type1$Score1)

#only for type 1

hist(type1$Score1)

#using colors
boxplot(credit_data$Score2, ylab= "Score2", col = "red")
hist(credit_data$Score1, col = "pink")
