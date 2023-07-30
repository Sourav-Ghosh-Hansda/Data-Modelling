# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(corrplot)
library(Boruta)
library(class)
library(factoextra)
# Read the data from the CSV file
project_data <- read.csv("C:/Users/Admin/OneDrive - University of Essex/MA335/Final project-20230509 2023-05-09 09_39_38/project data.csv",header = TRUE, sep=",")

# Convert M/F into numeric values
project_data$M.F <- as.numeric(factor(project_data$M.F, levels = c("M", "F"), labels = c(0, 1)))

# Remove rows with Group = "Converted" and missing values
project_data <- project_data[project_data$Group != "Converted", ]
project_data <- na.omit(project_data)

# Task 1: Descriptive statistics
# Summary table
summary_table <- summary(project_data)

# Boxplot
boxplot <- ggplot(project_data, aes(x = Group, y = Age)) +
  geom_boxplot() +
  labs(x = "Group", y = "Age") +
  ggtitle("Boxplot of Age by Group")

# Histogram
histogram <- ggplot(project_data, aes(x = Age, fill = Group)) +
  geom_histogram(binwidth = 5, alpha = 0.7) +
  labs(x = "Age", y = "Count", fill = "Group") +
  ggtitle("Histogram of Age by Group")

# Scatterplot
scatterplot <- ggplot(project_data, aes(x = MMSE, y = CDR, color = Group)) +
  geom_point() +
  labs(x = "MMSE", y = "CDR", color = "Group") +
  ggtitle("Scatterplot of MMSE and CDR by Group")

# Task 2: Clustering algorithms
# Perform k-means clustering
project_data$Group<-as.numeric(project_data$Group)
data1 <- scale(project_data)
set.seed(123)
kmeans2 <- kmeans(data1, centers = 2, nstart = 30)
kmeans3 <- kmeans(data1, centers = 3, nstart = 30)
kmeans4 <- kmeans(data1, centers = 4, nstart = 30)
str(kmeans2)
# Visualize cluster results
cluster_plot_2 <- fviz_cluster(kmeans2, data = data1) + ggtitle("k = 2")
cluster_plot_3 <- fviz_cluster(kmeans3, data = data1) + ggtitle("k = 3")
cluster_plot_4 <- fviz_cluster(kmeans4, data = data1) + ggtitle("k = 4")
cluster_plot_2
cluster_plot_3
cluster_plot_4
# Task 3: Logistic regression model
# Fit the logistic regression model
logistic_expression <- glm(Group ~ ., data = project_data, family = binomial(link = "logit"))

# Task 4: Feature selection method
# Perform feature selection using Boruta algorithm
boruta_obj <- Boruta(Group ~ ., data = project_data)
boruta_result <- TentativeRoughFix(boruta_obj)

# Print the Boruta result
print(boruta_result)

# Save the plots and summary table
ggsave("boxplot.png", boxplot)
ggsave("histogram.png", histogram)
ggsave("scatterplot.png", scatterplot)
ggsave("cluster_plot_2.png", cluster_plot_2)
ggsave("cluster_plot_3.png", cluster_plot_3)
ggsave("cluster_plot_4.png", cluster_plot_4)
write.table(summary_table, "summary_table.txt")

# Print the summary of the logistic regression model
summary(logistic_expression)

# Save the R code with explanations in a text file
code <- capture.output({
  cat("# Task 1: Descriptive statistics\n")
  cat("summary_table <- summary(project_data)\n")
  cat("\n")
  cat("# Boxplot\n")
  cat("boxplot <- ggplot(project_data, aes(x = Group, y = Age)) +\n")
  cat("  geom_boxplot() +\n")
  cat("  labs(x = \"Group\", y = \"Age\") +\n")
  cat("  ggtitle(\"Boxplot of Age by Group\")\n")
  cat("\n")
  cat("# Histogram\n")
  cat("histogram <- ggplot(project_data, aes(x = Age, fill = Group)) +\n")
  cat("  geom_histogram(binwidth = 5, alpha = 0.7) +\n")
  cat("  labs(x = \"Age\", y = \"Count\", fill = \"Group\") +\n")
  cat("  ggtitle(\"Histogram of Age by Group\")\n")
  cat("\n")
  cat("# Scatterplot\n")
  cat("scatterplot <- ggplot(project_data, aes(x = MMSE, y = CDR, color = Group)) +\n")
  cat("  geom_point() +\n")
  cat("  labs(x = \"MMSE\", y = \"CDR\", color = \"Group\") +\n")
  cat("  ggtitle(\"Scatterplot of MMSE and CDR by Group\")\n")
  cat("\n")
  cat("# Task 2: Clustering algorithms\n")
  cat("# Perform k-means clustering\n")
  cat("data1 <- scale(project_data)\n")
  cat("set.seed(123)\n")
  cat("kmeans2 <- kmeans(data1, centers = 2, nstart = 30)\n")
  cat("kmeans3 <- kmeans(data1, centers = 3, nstart = 30)\n")
  cat("kmeans4 <- kmeans(data1, centers = 4, nstart = 30)\n")
  cat("\n")
  cat("# Visualize cluster results\n")
  cat("cluster_plot_2 <- fviz_cluster(kmeans2, data = data1) + ggtitle(\"k = 2\")\n")
  cat("cluster_plot_3 <- fviz_cluster(kmeans3, data = data1) + ggtitle(\"k = 3\")\n")
  cat("cluster_plot_4 <- fviz_cluster(kmeans4, data = data1) + ggtitle(\"k = 4\")\n")
  cat("\n")
  cat("# Task 3: Logistic regression model\n")
  cat("# Fit the logistic regression model\n")
  cat("logistic_expression <- glm(Group ~ ., data = project_data, family = binomial(link = \"logit\"))\n")
  cat("\n")
  cat("# Task 4: Feature selection method\n")
  cat("# Perform feature selection using Boruta algorithm\n")
  cat("boruta_obj <- Boruta(Group ~ ., data = project_data)\n")
  cat("boruta_result <- TentativeRoughFix(boruta_obj)\n")
  cat("\n")
  cat("# Print the Boruta result\n")
  cat("print(boruta_result)\n")
  cat("\n")
  cat("# Print the summary of the logistic regression model\n")
  cat("summary(logistic_expression)\n")
  cat("\n")
  cat("# Save the plots and summary table\n")
  cat("ggsave(\"boxplot.png\", boxplot)\n")
  cat("ggsave(\"histogram.png\", histogram)\n")
  cat("ggsave(\"scatterplot.png\", scatterplot)\n")
  cat("ggsave(\"cluster_plot_2.png\", cluster_plot_2)\n")
  cat("ggsave(\"cluster_plot_3.png\", cluster_plot_3)\n")
  cat("ggsave(\"cluster_plot_4.png\", cluster_plot_4)\n")
  cat("write.table(summary_table, \"summary_table.txt\")\n")
})

# Save the R code with explanations in a text file
writeLines(code, "R_code_with_explanations.txt")
