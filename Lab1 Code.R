#Dataset Example
library(plyr)
Item1 <- c(2,4,5,6)
Item2 <- c(1,3,3,5)
Total <- Item1 + Item2
data <- data.frame(Item1,Item2,Total)
data

#command to view dataset
View(data)


#Helpful syntax for clearing out R workspace
rm(list=ls()) #Cleans (empties) global environment
dev.off()     #Cleans plots/graphics
cat("\014")   #Cleans out the console


#Find the file you want to read
data.file <- file.choose()
#Change the working directory to match file so you don't have to specify path to file
setwd(dirname(data.file))

#need to specify correct read function
#read a csv file
data <- read.csv(data.file, header=TRUE)

#read a txt file
data <- read.table(data.file, header=TRUE)
data

#read an xlsx file, need xlsx package installed and loaded
data <- read.xlsx(data.file, sheetName="Sheet1", header=FALSE)
data


#Variable with missing value
gender <-c("M","M","F","M","F",NA)

#table function produces frqeuencies without telling us number of missing values
table(gender)

#count function from plyer package tells us frequencies and missing values
count(gender)


#Read in .csv file, can also use Import Dataset option
example1 <- read.csv("C:/Users/seanpotter/Desktop/example1.csv")

#example1 <- data
example1
str(example1)
#Basic Barplot for Categorical Variable
plot(example1$sex)
example1$sex <-as.factor(example1$sex)
#Barplot with added options
plot(example1$sex, main="Males & Females",
     xlab="Sex", ylab="Frequency", ylim=c(0,140))


#Basic Histogram for Numeric Variable
hist(example1$Perf)


#Histogram with added options
hist(example1$Perf, main="Performance Distribution", 
     xlab="Performance", xlim=c(10,35))


#Basic Scatterplot of Performance by Conscientiousness
plot(x = example1$Conscale, y =example1$Perf)


# Scatterplot with added options
plot(y = example1$Perf, x = example1$Conscale,
     main="Performance and Conscientiousness", 
     xlab="Conscientiousness", ylab="Performance")
#Create fit line for scatterplot
line <- lm(example1$Perf~example1$Conscale)
#Plot fit line
abline(line, col="red")




#Create male fit line
male.line <- lm(data=subset(example1,sex==1),Perf~Conscale)
#Create female fit line
female.line <- lm( data=subset(example1,sex==0),Perf~Conscale)
#Scatterplot by Sex
plot(x = example1$Conscale, y = example1$Perf, 
     col=c("Blue","Red")[example1$sex],
     main="Performance and Conscientiousness"
)


#Adds legend position top left, with legends values named Female and Male, with blue and red boxes next to values
#Legend values should match up with order of factor levels, check using levels() on factor variable,
legend("topleft", legend=c("Female","Male"), fill=c("Blue","Red"))
#Plot male line
abline(male.line, col="red")
#Plot female line
abline(female.line, col="blue")
