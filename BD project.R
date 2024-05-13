#Getting our Dataset
df <- read.csv("C:/Users/Mark/Desktop/Big data data/breast-cancer.csv",header=TRUE)
#Statistics summary of the dataset
summary(df)

#installing necessary packages 
#install.packages("glmnet")
#install.packages("ggplot2")
#install.packages("caTools")
#install.packages("corrplot")
#install.packages("dplyr")
#install.packages("kernlab")
#install.packages("ellipse")
library("ggplot2")
library("caTools")
library("corrplot")
library(dplyr)
library(tidyr)
library(caret)

#Viewing the first 5 samples of the data
View(head(df))

#Viewing the Dimensions of our dataset
dim(df)

#Checking for null values
null_count <- sum(is.na(df))

# Print the count of null values
print(null_count)


#Number of Women affected in Benign or malignant state
#pipe operator to create multiple operations sequentially
df %>% count(diagnosis)

# calculate percentages of enign or malignant state (data is Biased)
#getting groups of different values in doagnosis column then applies the summerize on each value in total ni of rows

percentage_diagnosis <- df %>%
  group_by(diagnosis) %>%
  summarize(percentage = n() / nrow(df) * 100)

# Print the result
print(percentage_diagnosis)


#Data Visualizations and EDA

# Creating a table of frequencies
diagnosis.table <- table(df$diagnosis)

# Create a pie chart.
# Calculates proportions, and then multiplying by 100 converts them to percentages.
diagnosis.prop.table <- prop.table(diagnosis.table) * 100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)

# This line creates labels for the pie chart
pielabels <- sprintf("%s - %3.1f%%", rownames(diagnosis.prop.df), diagnosis.prop.table)

# Define colors vector (assuming you have defined it elsewhere)
colors <- rainbow(length(diagnosis.prop.table))

# Draw the pie chart
pie(diagnosis.prop.table, labels = pielabels, clockwise = TRUE, col = colors, border = "gainsboro", radius = 0.8, cex = 0.8, main = "Cancer diagnosis")



#replace 'M' with 1 and 'B' with 0 in the 'diagnosis' 
df$diagnosis <- ifelse(df$diagnosis == "M", 1, ifelse(df$diagnosis == "B", 0, as.numeric(df$diagnosis)))



#Correlation plot 
c<-cor(df[,3:32])
corrplot(c,tl.cex=0.7)




#Comparing radius ,area & Concavity of benign and malignant

# Plotting radius comparison
ggplot(df, aes(x = factor(diagnosis), y = radius_mean)) +
  geom_boxplot(fill="yellow") +
  labs(title = "Comparison of Radius Mean  in Benign Vs Malignant",
       x = "Diagnosis",
       y = "Radius") +
  facet_wrap(~ diagnosis)

# Plotting Area comparison
ggplot(df, aes(x = factor(diagnosis), y = area_mean)) +
  geom_boxplot(fill="yellow") +
  labs(title = "Comparison of Area Mean in Benign Vs Malignant",
       x = "Diagnosis",
       y = "Radius") +
  facet_wrap(~ diagnosis)


# Plotting Concavity comparison
ggplot(df, aes(x = factor(diagnosis), y = concavity_mean)) +
  geom_boxplot(fill="yellow") +
  labs(title = "Comparison of Concavity mean in Benign Vs Malignant",
       x = "Diagnosis",
       y = "Radius") +
  facet_wrap(~ diagnosis)


colnames(df)

#-----------------------------------------------------------------------------------------
#Printing Histograms of se,Mean & worst

# Selecting relevant columns to separate dataframe

df3 <- df[, c('diagnosis', 'radius_mean', 'texture_mean', 'perimeter_mean',
               'area_mean', 'smoothness_mean', 'compactness_mean', 'concavity_mean',
               'concave.points_mean', 'symmetry_mean', 'fractal_dimension_mean')]

df4 <- df[, c('diagnosis', 'radius_se', 'texture_se', 'perimeter_se', 'area_se', 'smoothness_se',
               'compactness_se', 'concavity_se', 'concave.points_se', 'symmetry_se',
               'fractal_dimension_se')]
df5 <- df[, c('diagnosis', 'radius_worst', 'texture_worst',
               'perimeter_worst', 'area_worst', 'smoothness_worst',
               'compactness_worst', 'concavity_worst', 'concave.points_worst',
               'symmetry_worst', 'fractal_dimension_worst')]

# Create subplots for histograms
par(mfrow = c(8, 4), mar = c(2, 2, 2, 2))  # Set layout

# Loop through columns and create histograms for df3, df4, and df5
for (i in 2:ncol(df3)) {  # Start from the second column (excluding 'diagnosis')
  hist(df3[[i]], main = paste("Histogram of", colnames(df3)[i]),
       xlab = colnames(df3)[i], col = 'skyblue', breaks = 10, freq = TRUE,
       xlim = c(0, max(df3[[i]])))
  hist(df4[[i]], main = paste("Histogram of", colnames(df4)[i]),
       xlab = colnames(df4)[i], col = 'orange', breaks = 10, freq = TRUE,
       xlim = c(0, max(df4[[i]])))
  hist(df5[[i]], main = paste("Histogram of", colnames(df5)[i]),
       xlab = colnames(df5)[i], col = 'red', breaks = 10, freq = TRUE,
       xlim = c(0, max(df5[[i]])))
}

# Adjust layout
par(mfrow = c(1, 1))  # Reset layout

#----------------------------------------------------------------------------------------------------------------------------------------
# Create subplots for boxplots
par(mfrow = c(4, 2), mar = c(4, 4, 2, 2))  # Set layout with larger margins

# Loop through columns and create boxplots for df3
for (i in 2:ncol(df3)) {  # Start from the second column (excluding 'diagnosis')
  boxplot(df3[[i]], main = paste("Boxplot of", colnames(df3)[i]),
          col = 'skyblue', ylim = c(0, max(df3[[i]]) * 1.2))  # Increase ylim for more values
}

# Reset layout
par(mfrow = c(1, 1))

# Create subplots for boxplots
par(mfrow = c(4, 2), mar = c(4, 4, 2, 2))  # Set layout with larger margins

# Loop through columns and create boxplots for df4
for (i in 2:ncol(df4)) {  # Start from the second column (excluding 'diagnosis')
  boxplot(df4[[i]], main = paste("Boxplot of", colnames(df4)[i]),
          col = 'orange', ylim = c(0, max(df4[[i]]) * 1.2))  # Increase ylim for more values
}

# Reset layout
par(mfrow = c(1, 1))

# Create subplots for boxplots
par(mfrow = c(4, 2), mar = c(4, 4, 2, 2))  # Set layout with larger margins

# Loop through columns and create boxplots for df5
for (i in 2:ncol(df5)) {  # Start from the second column (excluding 'diagnosis')
  boxplot(df5[[i]], main = paste("Boxplot of", colnames(df5)[i]),
          col = 'red', ylim = c(0, max(df5[[i]]) * 1.2))  # Increase ylim for more values
}

# Reset layout
par(mfrow = c(1, 1))

#-------------------------------------------------------------------------------------------------------------------
# Convert diagnosis to a factor in the original dataframe
original_diagnosis <- df$diagnosis
df$diagnosis <- factor(df$diagnosis)

# ANOVA test for mean columns with diagnosis
mean_anova <- df %>%
  select(ends_with("_mean"), diagnosis) %>%
#reshapes the data from wide to long format using the pivot_longer function from the tidyr package.
#It takes all columns except the "diagnosis" column, and pivots them into two columns: "Feature" (which contains the original column names) and "Value" (which contains the corresponding values).
  pivot_longer(cols = -diagnosis, names_to = "Feature", values_to = "Value") %>%
#tests whether there are statistically significant differences in the mean values (represented by "Value") across different levels of the "diagnosis" variable.
  aov(Value ~ diagnosis, data = .)

summary(mean_anova)

# ANOVA test for SE columns with diagnosis
se_anova <- df %>%
  select(ends_with("_se"), diagnosis) %>%
  pivot_longer(cols = -diagnosis, names_to = "Feature", values_to = "Value") %>%
  aov(Value ~ diagnosis, data = .)

summary(se_anova)

# ANOVA test for worst columns with diagnosis
worst_anova <- df %>%
  select(ends_with("_worst"), diagnosis) %>%
  pivot_longer(cols = -diagnosis, names_to = "Feature", values_to = "Value") %>%
  aov(Value ~ diagnosis, data = .)

summary(worst_anova)
#------------------------------------------------------------------------------------------------------------------------------------
# Tukey's test for mean_anova
mean_tukey <- TukeyHSD(mean_anova)
print(mean_tukey)

# Tukey's test for se_anova
se_tukey <- TukeyHSD(se_anova)
print(se_tukey)

# Tukey's test for worst_anova
worst_tukey <- TukeyHSD(worst_anova)
print(worst_tukey)

# Revert diagnosis column back to its original format
df$diagnosis <- original_diagnosis


#------------------------------------------------------------------------------------------------------------------------------------
# Function to remove outliers using interquartile range (IQR)
#remove_outliers_iqr <- function(df, threshold = 1.5) {
  # Extract diagnosis column
 # diagnosis <- df$diagnosis
  
  # Remove diagnosis column before calculating IQR
 # df_no_diag <- df[, !names(df) %in% "diagnosis"]
  
  # Calculate Q1 and Q3 for each column
#  Q1 <- apply(df_no_diag, 2, quantile, probs = 0.25)
#  Q3 <- apply(df_no_diag, 2, quantile, probs = 0.75)
  
  # Calculate IQR for each column
#  IQR <- Q3 - Q1
  
  # Identify outliers based on IQR
#  outlier_indices <- apply(df_no_diag, 2, function(x) (x < (Q1 - threshold * IQR)) | (x > (Q3 + threshold * IQR)))
  
  # Combine outlier indices across columns
 # outlier_indices <- apply(outlier_indices, 1, any)
  
  # Remove outliers from the dataframe
#  df_cleaned <- df[!outlier_indices, ]
  
#  return(df_cleaned)
#}

# Apply function to remove outliers using IQR from your dataset
#cleaned_df <- remove_outliers_iqr(df)
#dim(cleaned_df)




#-------------------------------------------------------------------------------------------------------------------------------------
# Load required libraries
library(tidyr)

# Function to remove outliers using z-scores
remove_outliers <- function(df, threshold = 3) {
  # Extract diagnosis column
  diagnosis <- df$diagnosis
  
  # Remove diagnosis column before calculating z-scores
  df_no_diag <- df[, !names(df) %in% "diagnosis"]
  
  # Calculate z-scores for each column
  z_scores <- scale(df_no_diag)
  
  # Identify outliers based on z-scores
  outlier_indices <- apply(abs(z_scores) > threshold, 1, any)
  
  # Remove outliers from the dataframe
  df_cleaned <- df[!outlier_indices, ]
  
  return(df_cleaned)
}

# Apply function to remove outliers from your dataset
cleaned_df <- remove_outliers(df)
dim(cleaned_df)

#---------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)

# Melt the dataframe for easier plotting
cleaned_df_melted <- reshape2::melt(cleaned_df, id.vars = "diagnosis")

# Plot boxplots
ggplot(cleaned_df_melted, aes(x = diagnosis, y = value, fill = factor(diagnosis))) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Boxplots of Cleaned Data by Diagnosis",
       x = "Diagnosis",
       y = "Value") +
  theme_minimal()


#--------------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------

# Shuffle the data
set.seed(123)  # For reproducibility
shuffled_index <- sample(nrow(cleaned_df))
shuffled_df <- cleaned_df[shuffled_index, ]

# Split the shuffled data into train and test sets
split <- sample.split(seq(nrow(shuffled_df)), SplitRatio = 0.7) 

# Split the data using the logical vector
train_data <- shuffled_df[split, ]
test_data <- shuffled_df[!split, ]

# Check the dimensions of train and test sets
dim(train_data)
dim(test_data)

#------------------------------------------------------------------------------------------------------------
# Fit logistic regression model
model <- glm(diagnosis ~ ., data = train_data, family = binomial)

# Make predictions on test data
predictions <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the accuracy
accuracy <- mean(predicted_classes == test_data$diagnosis)
print(paste("Accuracy:", accuracy))
#-------------------------------------------------------------------------------------------------------------------------

# Load necessary libraries
library(caret)
library(glmnet)

# Define hyperparameter grid for grid search
hyper_grid <- expand.grid(alpha = 0:1, lambda = seq(0.01, 1, by = 0.01))

# Set up train control
train_control <- trainControl(method = "cv", number = 5)

# Convert the outcome variable to a factor (assuming binary classification)
train_data$diagnosis <- as.factor(train_data$diagnosis)

# Train the model
model <- train(
  diagnosis ~ ., 
  data = train_data, 
  method = "glmnet", 
  trControl = train_control,
  tuneGrid = hyper_grid,
  family = "binomial"
)

# Print the best tuning parameters
print(model$bestTune)

# Make predictions on test data (get predicted probabilities)
predictions <- predict(model, newdata = test_data, type = "prob")

# Extract probabilities for the positive class
probabilities <- predictions[, 2]  # Assuming class 1 is the positive class

# Convert probabilities to binary predictions (0 or 1)
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)

# Evaluate the accuracy
accuracy <- mean(predicted_classes == test_data$diagnosis)
print(paste("Accuracy:", accuracy))

#----------------------------------------------------------------------------------------------------
# Plot the model
plot(model)

# Add a legend
legend("topright", legend = levels(train_data$diagnosis), fill = 1:2)

#--------------------------------------------------------------------------------------------------------------
# Load the necessary library
library(caret)
library(ellipse)
library(ggplot2)

# Create a confusion matrix
cm <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$diagnosis))

# Print the confusion matrix
print(cm)

# Convert the confusion matrix to a table
cm_table <- as.table(cm$table)

# Plot the confusion matrix
ggplot(as.data.frame(cm_table), aes(x=Reference, y=Prediction)) +
  geom_tile(aes(fill = log(Freq)), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(x=Reference, y=Prediction, label=Freq)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  labs(fill="Log(Frequency)")
#--------------------------------------------------------------------------------------------------------
