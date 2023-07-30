#Set the working directory

setwd("C:/Users/Admin/OneDrive - University of Essex/MA335/Final project-20230509 2023-05-09 09_39_38")

#Load the Data Set
data<-read.csv("C:/Users/Admin/OneDrive - University of Essex/MA335/Final project-20230509 2023-05-09 09_39_38/project data.csv",header = TRUE, sep=",")

#Remove the missing values
data <- na.omit(data)

#Converting M/F column into Numeric Values

data$M.F <- factor(data$M.F, levels = c("M", "F"))

#Installing package dplyr so that we can use the re code function
library(dplyr)
data$M.F <-unclass(data$M.F)


#Removing the values of the column Group who is having the values as Converted

data <- data[!grepl('Converted', data$Group),]
head(data)

#Q.1

# Graphical Representation
#In this we are going to show the relation between the columns

#Box Plot

#we are going to plot the graph between Group and Age
#Instaling libraries which are ggplot2 viridis
library(ggplot2)
library(viridis)
library(ggthemes)
library(hrbrthemes)
library(ggpubr)

#Relationship has been made between Group and Age using boxplot
ggsummarystats(
  data, x = "Group", y = "Age",
  ggfunc = ggboxplot, add = "jitter", color = "M.F"
)

#Graph has been plotted between the columns CDR and ASF because both of them are measuring the score which helps us to understand.
ggplot(data, aes(ASF)) +
  geom_histogram() +
  geom_text(x = mean(data$ASF), y = 0, label = "Mean:", hjust = 0) +
  geom_text(x = median(data$ASF), y = 0, label = "Median:", hjust = 0) +
  geom_text(x = max(data$ASF), y = 0, label = "Max:", hjust = 1) +
  geom_text(x = min(data$ASF), y = 0, label = "Min:", hjust = 0) +
  geom_text(x = quantile(data$ASF, 0.25), y = 0, label = "1st Qu:", hjust = 0) +
  geom_text(x = quantile(data$ASF, 0.75), y = 0, label = "3rd Qu:", hjust = 0)+
  scale_fill_discrete(name = "M.F") +
  theme(axis.title.x = element_blank())


#Barplot of SES and MMSE
ggsummarystats(
  data, x = "SES", y = "MMSE",
  ggfunc = ggbarplot, add = c("jitter", "median_iqr"), position = position_dodge(),
  color = "Group"
)


#Numerical Representation

#Descriptive Statistics of the above dataset
#1. Summary

summary(data)

#sapply() function to calculate specific descriptive statistics for each variable.
suppressWarnings(sapply(data, sd, na.rm=TRUE))


#Q.2
#Clustering Algorithm

#Clustering Algorithm is of 3 types which are k means, hierarchy and gaussian algorithm .
#And in this we are going to use the k means clustering algorithm


#First we have to convert the group into numerical because it is categorical and we cannot use mix features to do k means

#Converting Group
data$Group <- factor(data$Group, levels = c("Nondemented", "Demented"))

#Recoding
data$Group <- recode(data$Group, Nondemented = 0, Demented = 1)

#Installing hte factoextra package
install.packages("factoextra")

#Load the library
library(factoextra)
library(cluster)

set.seed(123)
#when k=4
kms <- kmeans(data, 4, nstart = 25)

#When k=2
kms1<- kmeans(data,2,nstart=25)

fviz_cluster(kms, data = data)
fviz_cluster(kms1, data = data)
plot(data, col = kms1$cluster)



#Q.3 Logistic Regression
#In this we have selected some important features to predict the model and we use
#multiple logistic regression
install.packages("ISLR")

library("ISLR")
model1<- glm(Group ~ .,data = data,family = binomial(link = "logit"))
suppressWarnings(summary(model1))
modellr <- glm(Group ~Age+SES+CDR+eTIV+nWBV,data = data, family=binomial(link="logit"))
suppressWarnings(summary(modellr))

plot(modellr)
#From the above Summary we can say that the model is not getting fitted because the p value is not less than 0.05. And here we fail to
#reject the null hypothesis

#Q.4
#Feature Selection
install.packages("MASS")
library(MASS)
fectselect <- stepAIC(modellr, direction="both")
suppressWarnings(summary(fectselect))