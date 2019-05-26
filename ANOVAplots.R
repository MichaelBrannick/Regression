############ ggplot2 for ANOVA results #####################

##### Bar chart and Boxplots

######################################################################################
# preparation - also install any needed supporting programs 
rm(list=ls()) # Cleans (empties) global environment
dev.off()     # Cleans plots/graphics
cat("\014")   # Cleans out the console
######################################################################################
library(psych)      # for descriptive stats
library(ggplot2)    # for plots
######################################################################################

# Import the data from file SurgDat.xlsx

# Data are surgical performance measured as steps in a checklist completed
#  correctly in a simulated surgery.  Training was randomized into two groups:
#  live tissue training and simulation training.  Both groups were tested on
#  psychomotor performance (steps in the checklist) before and after training.
#  Design is mixed (split-plot) ANOVA; one between (training type), 
#  one within (trial - pre/post). Approximatley 45 people in each training type.
# Random error was added to the performance scores so that these data are not
#  the actual scores of the participants in the study, but the pattern of results 
#  is the same.

str(SurgDat)  # check to see what it read -- table w/ 2 alpha, one numeric
SurgDat
# find the descriptive stats for each cell of the design
cell1 <- subset(SurgDat, Train == 'Live Tissue' & Time == '1-Pre')  # subset for each cell
cell2 <- subset(SurgDat, Train == 'Live Tissue' & Time == '2-Post')
cell3 <- subset(SurgDat, Train == 'Simulation' & Time == '1-Pre') 
cell4 <- subset(SurgDat, Train == 'Simulation' & Time == '2-Post')
#
a <- describe(cell1$Perf) # compute descriptives for each cell
b <- describe(cell2$Perf)
c <- describe(cell3$Perf)
d <- describe(cell4$Perf)
a                         # check first cell
a$mean                    # check mean value for first cell
# data for plotting the barchart

plotdat <- data.frame(Train = c("Live Tissue", "Live Tissue", "Simulation", "Simulation"),
                      Time = c("1-Pre", "2-Post", "1-Pre", "2-Post"),
                      Means = c(a$mean, b$mean, c$mean, d$mean),
                      Serrs = c(a$se, b$se, c$se, d$se))
plotdat  # note the means and standard errors were computed by the describe function
# 
#  bar chart for a presentation

motorbar <- ggplot(data=plotdat, aes(x = Train, y=Means, fill = Time))
motorbar <- motorbar + geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.2,
                position=position_dodge(.9)) +
  labs(x="Training Group", y = "Steps Correctly Completed")+
  scale_fill_brewer(palette="Paired") + theme_minimal()
motorbar

# bar chart for a publication

motorbar <- ggplot(data=plotdat, aes(x = Train, y=Means, fill = Time))
motorbar <- motorbar + geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.2,
                position=position_dodge(.9)) +
  labs(x="Training Group", y = "Steps Correctly Completed")+
  scale_fill_manual(values=c('darkgray','lightgray'))+
  theme_classic()
#
motorbar <- motorbar + theme(
  axis.title.x = element_text(size=14, face="bold"),
  axis.title.y = element_text(size=14, face="bold"),
  legend.title = element_text(size=14, face="bold"))

motorbar + theme(text = element_text(size = 15))
# exported as .tiff 500w x 600h to desktop as 'barplot' then import to Word
###################################################################################
# using ggplot for boxplots
SurgDat
motorbox <- ggplot(data=SurgDat, aes(x = Train, y=Perf, fill = Time))
motorbox + geom_boxplot(notch=TRUE, position = position_dodge(1)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, 
               position = position_dodge(1)) +
  labs(x="Training Group", y = "Steps Correctly Completed")+
  #
  scale_fill_manual(values=c('darkgray','lightgray'))+
  theme_classic()
#
scale_fill_manual(values=c('#006747','#CFC493'))+    
  # the hex values for green and gold are custom for USF (you can Google these)
  theme_classic()                              
##############################################################
