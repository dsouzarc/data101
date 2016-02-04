#Source: http://archive.ics.uci.edu/ml/datasets/Adult
adult.data <- read.csv("adult.data.csv")

age = adult.data$Age

#First 5 columns
adult.part <- adult.data[1:5]

#Specific columns
adult.part <- adult.data[c(1,3,5,7)]

#Get 5 rows
adult.row.part.first <- adult.data[1:5,]

#Get specific rows
adult.row.part.spec <- adult.data[c(1,3,5,7),]

#Specific rows and columns
adult.both.part <- adult.data[1:5,1:5]

#Person >= 30
adult.age.part <- subset(adult.data, Age>=30)

#All females
adult.women <- subset(adult.data, Sex =='Female')

#All bachelors degree
adult.bachelors <- subset(adult.data, Education == 'Bachelors')

#Vector containing row ids from 1 to N
ids <- c(1:nrow(adult.data))

#Add the IDs to the table
adult.data.ids <- ids

s <- summary(adult.data)

#Histogram for distribution of ONE specific item --> Frequency of x
hist(adult.data$Age,main=paste("Histogram of age distribution"), xlab = 'age', ylab = 'frequency',ylim = c(0,5000))

#Barplot --> plots the value of x
barplot(c(1,2,3,4,5), col = rev(heat.colors(5)))

#Relation between gender and salary
mosaicplot(table(adult.data$Sex, adult.data$Class), color=3:4, xlab="Gender", main="Gender v. Class")

#Boxplot of age and salary
boxplot(adult.data$Age~adult.data$Class, ylab="Age")

#transform values
education <- rep("post-HS",nrow(adult.data))
education[adult.data$Education %in% c(" 1st-4th", " 5th-6th", " 7th-8th", " 9th", " 10th", " 11th", " 12th", " HS-grad")] <- "HS"
mosaicplot(table(education,adult.data$Class),color=3:4,xlab="Hours per week",main="education v. Salary")