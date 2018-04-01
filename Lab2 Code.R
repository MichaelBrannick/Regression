#Helpful syntax for clearing out R workspace
rm(list=ls()) #Cleans (empties) global environment
dev.off()     #Cleans plots/graphics
cat("\014")   #Cleans out the console
data.file <- file.choose()
#Change the working directory to match file so you don't have to specify path to file
setwd(dirname(data.file))
example2 <-read.csv(data.file)
str(example2)
View(example2)
par("mar")
#Sort dataset by Employee ID
example2[order(example2$Employee),]

#Sort dataset by Item1 and Item2 
example2[order(example2$Item1,example2$Item2),]

#Sort dataset by Item1 (ascending) and Item2 (descending)
example2[order(example2$Item1, -example2$Item2),]


#Select cases only where with Perf >= 20 and Conscale >10, Select Employee, Perf, Conscale, and Sex for new dataset
newdata1 <- subset(example2, example2$Perf>= 20 & example2$Conscale>10, select=c(Employee, Perf, Conscale, sex))
newdata1

#Select cases only where with sex is 1, select all variables from Employee to Item3
newdata2 <- subset(example2, example2$sex=="1" & example2$Conscale>10, select=c(Employee:Item3))
newdata2

#Label sex values where 0=Male and 1=Female
example2$sex.labeled <- factor(example2$sex, levels = c(0,1), labels = c("Male","Female"))
example2$sex
example2$sex.labeled

#Check differences in summary output with sex treated as numeric vs sex treated as factor with labels
summary(example2$sex)
summary(example2$sex.labeled)
 
#Reverse code variables with recode function in car package
install.packages("car")
library(car)
example2$Item1.R <- recode(example2$Item1, "1=5; 2=4; 4=2; 5=1")
example2$Item1
example2$Item1.R

#Install and Load Psych package for describe function
install.packages("psych")
library(psych)
#Describe function available with psych package
describe(example2)

#Allows you to split up describe function by group, doesn't export well though unless you set mat=TRUE
describeBy(example2, group=example2$sex.labeled, mat=TRUE)
#Install and Load Xtable package to export table to html webpage (easy to transfer to word documents)
install.packages('xtable')
library(xtable)
table1 <- describe(example2)
table1 <- xtable(table1)
#print.xtable will produce html file of table in current working directory, use setwd() or
#set working directory option under session in top toolbar to define working directory
print.xtable(table1,type="html", file="table1.html")

#Produce histogram of Conscale, set freq to false to plot density on y axis
hist(example2$Conscale, freq = FALSE, xlim=c(5,18), main="Histogram of Conscientiousness", xlab = "Conscientiousness")
#Produce density line to evaluate variable's normality
lines(density(example2$Conscale))


#Produce qq-plot with fitted line
qqnorm(example2$Conscale)
qqline(example2$Conscale)

#Secondary way to get normalitiy plots, uses fitdist from fitdistrplus package
install.packages('fitdistrplus')
library(fitdistrplus)
fit <- fitdist(example2$Conscale,"norm")
plot(fit)


#Produce boxplots by Sex
#dev.off() reset plots and margins to default settings
dev.off()
#par("mar") shows you current margin settings 
par("mar")
boxplot(example2$Conscale ~ example2$sex.labeled, 
        main="Boxplot of Conscale by Sex",
        ylab="Conscientiousness",
        xlab="Sex")

#par adjusts plotting parameters
#mar adjusts margin sizes in following order: bottom, left, top, right
#setting xpd=TRUE allows us to plot things outside the plot region
par(mar=c(5.1,4.1,4.1,8.1), xpd=TRUE)
#Produce scatterplot that makes male observations red, blue observations female
plot(example2$Conscale, example2$Perf, col=c("red","blue")[example2$sex.labeled])
#Produce legend in top right corner, use inset option so legend appears outside plot region
legend(x="topright", inset=c(-.5,0), legend = levels(example2$sex.labeled), col=c("red","blue"), pch=1)

#Shapiro-Wilks Test of Normality, p-value tests null that data came from normal distribution
#Is sample size dependent, should be combined with other diagnositcs like qq-plot (which help identify where issue may be)
shapiro.test(example2$Conscale)

#SAMPLING DISTRIBUTION EXERCISE
#CREDIT GOES TO NICOLE RADZIWILL FOR ORIGINAL R CODE
sdm.sim <- function(N,resampling,src.dist=NULL,pop.mean=NULL,pop.sd=NULL) {
  # This produces a matrix of observations with  
  # n columns and r rows. Each row is one sample:
  my.samples <- switch(src.dist,
                       "N" = matrix(rnorm(N*resampling,pop.mean,pop.sd),resampling),
                       "G" = matrix(rgamma(N*resampling,pop.mean,pop.sd),resampling))
  all.sample.means <- apply(my.samples,1,mean)   
  par(mfrow=c(1,2))
  hist(my.samples,col="gray",main="Distribution of Population", xlim =c(min(my.samples),max(my.samples)))
  hist(all.sample.means,col="gray",main="Sampling Distribution of the Mean", xlim =c(min(my.samples),max(my.samples)))
  Sampling.Distribution.Mean <- mean(all.sample.means)
  Sampling.Distribution.SD <- sd(all.sample.means)
  Pop.Distribution.Mean <- mean(my.samples)
  Pop.Distribution.SD <- sd(my.samples)
  output <- data.frame(Pop.Distribution.Mean, Pop.Distribution.SD,
                       Sampling.Distribution.Mean,Sampling.Distribution.SD)
  return(output)
}

#Adjust plot margins to better see plots
par(mar=c(2.1,2.1,2.1,2.1)) 
#Example simulation of using a normal distribution with a mean of 10, SD of 1
sdm.sim(N=50,resampling=10000,src.dist="N",pop.mean=10,pop.sd=1)


