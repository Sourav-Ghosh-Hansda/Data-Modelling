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
myproject <- subset(myproject, !(Group == "Converted"))  #Remove rows wit Group="Converted"
new_data_clean<- na.omit(myproject)  # Remove missing date

#Q1
ggplot(new_data_clean, aes(x = factor(Group), y = Age, fill = Group)) +
  geom_boxplot() +
  geom_jitter(aes(color = factor(M.F)), width = 0.2, alpha = 0.5) +
  labs(x = "Group", y = "Age", fill = "Group", color = "Gender") +
  scale_color_manual(values = c("blue", "red"), labels = c("Female", "Male")) +
  ggtitle("Box Plot: Age by Group and Gender")


str(new_data_clean)
ggplot(new_data_clean, aes(ASF)) +
  geom_histogram() +
  geom_text(x = mean(new_data_clean$ASF), y = 0, label = "Mean:", hjust = 0) +
  geom_text(x = median(new_data_clean$ASF), y = 0, label = "Median:", hjust = 0) +
  geom_text(x = max(new_data_clean$ASF), y = 0, label = "Max:", hjust = 1) +
  geom_text(x = min(new_data_clean$ASF), y = 0, label = "Min:", hjust = 0) +
  geom_text(x = quantile(new_data_clean$ASF, 0.25), y = 0, label = "1st Qu:", hjust = 0) +
  geom_text(x = quantile(new_data_clean$ASF, 0.75), y = 0, label = "3rd Qu:", hjust = 0)+
  scale_fill_discrete(name = "Group") +
  theme(axis.title.x = element_blank())
new_data_clean$Group <- ifelse(new_data_clean$Group == "Demented", 1, 0)
pairs(new_data_clean)
describe(new_data_clean)
str(new_data_clean)
#Q2
new_data_clean$Group <- ifelse(new_data_clean$Group == "Demented", 1, 0)
numeric_data <- new_data_clean[, c("M.F", "Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")]
set.seed(123)
data1 <- scale(numeric_data)
#when k=2
kms2<- kmeans(data1, 2, nstart = 25)
fviz_cluster(kms2, data = data1)
plot(new_data_clean, col = kms2$cluster,main="K-means Clustering")
#Q3
new_data_clean$Group <- as.factor(new_data_clean$Group)
# Fit the logistic regression model
logistic_expression <-suppressWarnings(glm(Group ~ ., data = new_data_clean, family = binomial(link = "logit")))
# Print the model summary
summary(logistic_expression)
#Q4
# Perform stepwise selection
step1 <- suppressWarnings(step(logistic_expression, scope = ~ M.F + Age + EDUC + SES + MMSE + CDR + eTIV + nWBV + ASF, method = "forward"))
# Print the summary of the selected model
summary(step1)
step2<- suppressWarnings(step(logistic_expression, scope = ~ M.F + Age + EDUC + SES + MMSE + CDR + eTIV + nWBV + ASF, method = "backward"))
summary(step2)



# Generate boxplots for numeric variables
numeric_vars <- c("Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")
boxplot_data <- new_data_clean[, numeric_vars]
boxplot(boxplot_data, main = "Boxplot of Numeric Variables")

# Generate histograms for numeric variables hist_data <- na.omit(data[, numeric_vars]) ggplot(data = stack(hist_data)) +
aes(x = values, fill = ind) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) + facet_wrap(~ ind, scales = "free") +
  labs(title = "Histograms of Numeric Variables")



# Scatterplot of eTIV vs. nWBV with color
ggplot(data = scatterplot_data, aes(x = eTIV, y = nWBV, color = color_variable)) +
  geom_point() +
  labs(title = "Scatterplot of eTIV vs. nWBV")



