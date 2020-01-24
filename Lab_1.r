#Integer Vector
x <- 1:10
print(x)

(z <- c("abc","d","ef","g"))
print(z)
#Character Vector
m <- matrix(1:20,nrow = 4, byrow = T)

#Matrix
m <- matrix(1:20,nrow = 4, byrow = F)

#Data Frame
age<- c(45,22,61,14,37)
gender <- c("female","Male","Male","Female","Male")
height <- c(1.68,1.85,1.8,1.66,1.72)
married <- c(T,F,T,F,F)
df <- data.frame(age,gender,height,married)
str(df)

tmp <- lapply(x, log)
print(tmp)

a <- 1:10
save(a,file="dumData.rData")

rm(a)
load("dumData.rData")

#Import from and export to .csv files

var1 <- 1:5
var2 <- (1:5)/10
var3 <- c("R","and","Data Mining","Examples","Case Studies")
df1 <- data.frame(var1,var2,var3)
names(df1) <-c("VarInt","VarReal","VarChar")

write.csv(df1,"dummyData.csv",row.names=FALSE)


df2 <- read.csv("dummyData.csv")
print(df2)


#import from and export to excel files

install.packages("xlsx")
library(xlsx)
xlsx.file <- "dummyData.xlsx"
write.xlsx(df2,xlsx.file, sheetName = "dummyData", row.names = F)
df3 <- read.xlsx(xlsx.file,sheetName = "dummyData")
print(df3)
