#The purpose of this lab is to build a simple regression model
#The data set is about 150 Companies.
#For each company, we know how much they spent on TV Ads,Radio Ads, and WWW Ads.
#We also know the revenue of each company. 
#We want to build a regressionmodel to predict the revenue of the company based on the amount of money they spend on different types of Ads.


mdata <- read.csv(file= "~/Desktop/marketing.csv")
str(mdata)
pairs(mdata)
plot(mdata$WWW_ads,mdata$revenue, ylab = "Revenue", xlab = "WWW_ads", main = "Revenue and Marketing")

model1 <- lm(revenue ~ WWW_ads, data = mdata)
abline(model1)
model1

model2 <- lm(revenue ~ WWW_ads + TV_ads, data=mdata)
model2

model3 <- lm(revenue ~ WWW_ads + TV_ads + Radio_ads, data = mdata)
model3