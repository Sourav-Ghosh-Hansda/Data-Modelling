# Load the necessary libraries
library(dplyr)
library(MASS)
# Set the working directory to the location of the data file
setwd("C:/Users/Admin/OneDrive - University of Essex/MA335/Final project-20230509 2023-05-09 09_39_38")

#Load the Data Set

data <- read.csv("project data.csv")



#Remove the missing values

data <- na.omit(data)



#Converting M/F column into Numeric Values





data$M.F <- factor(data$M.F, levels = c("M", "F"))



#Installing package dplyr so that we can use the re code function

library(dplyr)



data$M.F <- recode(data$M.F, M = 1, F = 2)



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
  
  data, x = "EDUC", y = "MMSE",
  
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



#Installing hte factoextra package

install.packages("factoextra")



#Load the library

library(factoextra)



set.seed(123)

km.res <- kmeans(data, 4, nstart = 25)