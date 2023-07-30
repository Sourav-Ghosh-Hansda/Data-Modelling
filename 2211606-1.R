# Load the necessary libraries
library(dplyr)
library(MASS)
library(corrplot)
library(Boruta)
library(class)
library(factoextra)
library(gridExtra)
library(ggplot2)
library(viridis)
library(ggplot2)
library(viridis)
library(ggthemes)
library(hrbrthemes)
library(ggpubr)
library(psych)
myproject<-read.csv("C:/Users/Admin/OneDrive - University of Essex/MA335/Final project-20230509 2023-05-09 09_39_38/project data.csv",header = TRUE, sep=",")
myproject$M.F <- ifelse( myproject$M.F == "F", 0, 1) #Converted Male/Female into numerical value
num_na_values <- sum(is.na(myproject))
data_clean<- na.omit(myproject)  # Remove missing date
new_data_clean <- subset(data_clean, !(Group == "Converted"))  #Remove rows wit Group="Converted"

#Q1
summary(new_data_clean)

p<-ggplot(new_data_clean, aes(x=Age, color=M.F))+
  geom_histogram(fill="white")+
  facet_grid(Group ~ .)
p

# Add mean lines
p+geom_vline(data=mu, aes(xintercept=grp.mean),linetype="dashed")


g<-ggplot(new_data_clean, aes(x=MMSE))+
  geom_histogram(color="green", fill="white")+
  facet_grid(Group ~ .)
g

# Add mean lines
g+geom_vline(data=wu, aes(xintercept=grp.mean),linetype="dashed")

r<-ggplot(new_data_clean, aes(x=nWBV))+
  geom_histogram(color="yellowgreen", fill="white")+
  facet_grid(Group ~ .)
r

r+geom_vline(data=pu, aes(xintercept=grp.mean),linetype="dashed")

#Q3
new_data_clean$Group <- as.factor(new_data_clean$Group)
# Fit the logistic regression model
logistic_expression <-suppressWarnings(glm(Group ~ ., data = new_data_clean, family = binomial(link = "logit")))
# Print the model summary
summary(logistic_expression)
#Q4
# Perform stepwise selection
step1 <- step(logistic_expression, scope = ~ M.F + Age + EDUC + SES + MMSE + CDR + eTIV + nWBV + ASF, method = "forward")
# Print the summary of the selected model
summary(step1)
#Q2
new_data_clean$Group<-as.numeric(new_data_clean$Group)
numeric_data <- new_data_clean[, c("M.F", "Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")]
set.seed(123)
data1 <- scale(numeric_data)
#when k=2
kms2<- kmeans(data1, 2, nstart = 25)
str(kms2)
#When k=4
kms4<- kmeans(data1,4,nstart=25)
fviz_cluster(kms2, data = data1)

