#Helpful syntax for clearing out R workspace
rm(list=ls()) #Cleans (empties) global environment
dev.off()     #Cleans plots/graphics
cat("\014")   #Cleans out the console

#Find current working directory
getwd()

#Set working directory, need to use forward slashes instead of backwards
setwd("C:/Users/Sean Potter/Desktop/R Project")
#Need to only write out file name, otherwise would have to specify path name for file
write.table(x,file="table.txt", quote=FALSE)

#Merge two dataframe by ID
schoolAB <- merge(schoolA,schoolB, by="ID")
schoolAB

#Merge observations from two datasets
#If dataB doesn't have variables in dataA, 
#delete those variables or create new variables and set values to NA
schoolAC <- rbind(schoolA, schoolC)

#Create variable
schoolAB$avgGPA <- (schoolAB$HSGPA+schoolAB$CGPA)/2
schoolAB

#Delete variable
schoolAB$avgGPA <- NULL
schoolAB

#Keep all variables except college
final.school <- subset(schoolAB, select=c(ID,Gender,Race,Age,HSGPA,ACT,CGPA,Credit))
final.school


#Bootstrap package installation
install.packages("boot")
library(boot)


#Function that returns mean for each resampled dataset
mean.function <- function(data, indices){
  d <- data[indices] # allows boot to select sample, no comma required if dataset is single variable 
  average <- mean(d)
 return(average) 
}
#Bootstrap using mean function resampling 1000 times
results <- boot(final.school$CGPA,mean.function, R=1000)
#See results for each resampling
print(results$t)
#Histogram of bootstrap results
plot(results)
#Get 95% confidence interval
boot.ci(results, conf=.95, type="bca")


# Bootstrap 95% CI for regression coefficients 
# Function to obtain regression weights 
regression.function <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample, set comma after indices if dataset has multiple variables 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=schoolAB, regression.function, R=1000, formula=CGPA~ACT+HSGPA)
#Intercept statistic presented first, followed by regression variables in order they're entered
results

plot(results, index=1) # intercept 
plot(results, index=2) # ACT 
plot(results, index=3) # HSGPA 

# Get 95% confidence intervals
# May get warning about bca results being unstable, typically resolved by increasing
# number of resamplings with boot function 
boot.ci(results, type="bca", index=1) # intercept 
boot.ci(results, type="bca", index=2) # ACT 
boot.ci(results, type="bca", index=3) # HSGPA