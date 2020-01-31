#get you the project's folder. this is the folder you must copy bike_sharing_data.csv
getwd()
#Read data set. Ensure that csv is within the same folder
bike<- read.csv("bike_sharing_data.csv")
#View(bike)
#str(bike)

#another way of reading data in a data set

bike <- read.table("bike_sharing_data.csv", sep = ",", header = TRUE)
View(bike)

#install dplyr package that supports basic OLAP operations
#you can also install a package from the menu, instead of typing in the command

install.packages("dplyr")

#load the package, note that for loading the package we not use quotation
library(dplyr)

#perform dice operation on data. We only select rows in which registered is equal to 0, and season is equal to 1 or 2
extracted_rows <- filter(bike, registered == 0, season == 1 | season == 2)
View(extracted_rows)

#return the dimension of data. IN this case, we have an array, with 10 rows and 12 columns
dim(extracted_rows)

#perform the same Dice operation in a different way

using_membership <- filter(bike, registered == 0, season %in% c(1,2))
#View(using_membership)

#this function compares the two arrays, and return True if they are identical
identical(extracted_rows, using_membership)
#output = true

#select only two columns (season and casual)
extracted_columns <- select(extracted_rows, season, casual)
View(extracted_columns)

#mutate function adds a new variable (columns) to an existing table and preserves existing ones
#in this example , the new variable is called revenue and its value is 5 times the value of casual variable
add_revenue <- mutate(extracted_columns, revenue = casual * 5)
View(add_revenue)

#group add_revenue table by season
grouped <- group_by(add_revenue, season)
#this is similar to Roll-up operation. We show the sum of two variables casual and revenue
report1 <- summarise(grouped, sum(casual), sum(revenue))
View(report1)

#now perform summarise on original add_revenue and see the difference
report2 <- summarise(add_revenue, sum(casual), sum(revenue))
View(report2)

#save data in csv file, the file is created in your project's folder
write.csv(report1,"revenue_report.csv", row.names = FALSE)

#save data in txt file, the file is created in your project's folder
write.table(report1, "revenue_report.txt", row.names =  FALSE, sep = "\t")
