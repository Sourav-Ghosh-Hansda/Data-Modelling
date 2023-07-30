# Load the dataset
data<-read.csv("C:/Users/Admin/OneDrive - University of Essex/MA335/Final project-20230509 2023-05-09 09_39_38/project data.csv",header = TRUE, sep=",")

# Convert M/F to numeric values (1 for Male, 2 for Female)
data$M.F <- ifelse(data$M.F == "M", 1, 2)

# Remove rows with Group = "Converted"
data <- data[data$Group != "Converted", , drop = FALSE]

# Remove rows with missing values
data <- na.omit(data)

##Question#1: Descriptive Statistics
# Load required packages
# Install and load ggplot2 package
install.packages("ggplot2")	# Install if not already installed
library(ggplot2)

# Generate summary statistics
summary_table <- summary(data)
count <- apply(data, 2, function(x) sum(!is.na(x))) sd_values <- apply(data, 2, sd)
correlation_matrix <- cor(data)

# Print summary statistics table
print(summary_table)

# Generate boxplots for numeric variables
numeric_vars <- c("Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")
boxplot_data <- data[, numeric_vars]
boxplot(boxplot_data, main = "Boxplot of Numeric Variables")

# Generate histograms for numeric variables hist_data <- na.omit(data[, numeric_vars]) ggplot(data = stack(hist_data)) +
aes(x = values, fill = ind) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) + facet_wrap(~ ind, scales = "free") +
  labs(title = "Histograms of Numeric Variables")

# Generate scatterplots for numeric variables
scatterplot_data <- data[, c("Age", "MMSE", "eTIV", "nWBV")]
color_variable <- data$Group	# Assuming "Group" is a categorical variable in your dataset

# Scatterplot of eTIV vs. nWBV with color
ggplot(data = scatterplot_data, aes(x = eTIV, y = nWBV, color = color_variable)) +
  geom_point() +
  
  labs(title = "Scatterplot of eTIV vs. nWBV with Color")


# Scatterplot of eTIV vs. nWBV with color
ggplot(data = scatterplot_data, aes(x = eTIV, y = nWBV, color = color_variable)) +
  geom_point() +
  labs(title = "Scatterplot of eTIV vs. nWBV")


#Question#2: Clustering Algorithms
# Load the required libraries
library(cluster)

# Prepare the data
numeric_vars <- c("Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")
data <- data[, numeric_vars]

# Handle missing values
data <- na.omit(data)	# Remove rows with missing values

# Handle infinite values
data[!is.finite(data)] <- NA	# Replace infinite values with NA

# Scale the data
scaled_data <- scale(data)

# Apply K-means clustering
k <- 3	# Number of clusters
kmeans_model <- kmeans(scaled_data, centers = k, nstart = 25)	# Adjust nstart for multiple initializations

# Obtain cluster assignments
cluster_labels <- kmeans_model$cluster

# Visualize the clusters
plot(data, col = cluster_labels, pch = 16, main = "K-means Clustering")

# Comment on the results cat("Cluster Centers:\n") print(kmeans_model$centers)

cat("\nCluster Sizes:\n") table(cluster_labels)


# Comment on the results
# Analyze the clusters, their separation, and any patterns or insights obtained

##Question#3: Logistic Regression Model
# Load the required library

library(stats)

# Load the dataset
data <- read.csv("C:/Users/Downloads/project_data.csv")

# Convert M/F to numeric values (1 for Male, 2 for Female)
data$M_F <- ifelse(data$M_F == "M", 1, 2)

# Remove rows with missing values
data <- na.omit(data)

# Fit logistic regression model
data <- glm(Group ~ ., data = data, family = binomial())

# Describe the produced model
summary(data)


##Question#4: Implement a feature selection method
install.packages("caret") install.packages("glmnet") install.packages("randomForest")


# Load required libraries library(caret) library(glmnet) library(randomForest)

# Load the dataset
data <- read.csv("C:/Users/Downloads/project_data.csv")

# Remove any missing values data <- na.omit(data) names(data)

# Convert categorical variables to factors if needed
data$Group <- as.factor(data$Group) data$M_F <- as.factor(data$M.F)


# Perform feature selection using RFE
control <- rfeControl(functions = rfFuncs, method = "cv", number = 5) result <- rfe(data[, -1], data$Group, sizes = 1:8, rfeControl = control)


# Print the results
print(result)

# Plot the results
plot(result, type = c("g", "o"))

# Get the most important features selected_features <- names(result$optVariables) print(selected_features)
