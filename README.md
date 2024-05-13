# Breast Cancer Detection Analysis

This repository contains R code for analyzing breast cancer data. The dataset used in this analysis is `breast-cancer.csv`.
## Getting Started

To run the analysis, follow these steps:

1. Ensure R is installed on your system.
2. Open the R script containing the code in this repository.
3. Set your working directory to the location of the dataset.
4. Run each code block sequentially.

## Dataset

The dataset contains information about various attributes related to breast cancer. To load the dataset and view a summary of its statistics, the following code is used:

```R
df <- read.csv("datasetpath", header = TRUE)
```
# Exploratory Data Analysis (EDA)
The code conducts various EDA tasks, including:

Checking for null values.
Visualizing the distribution of the diagnosis variable.
Plotting histograms and boxplots.
Conducting ANOVA tests.
Data Preprocessing
# Before building predictive models, the data is preprocessed, which includes:

Converting diagnosis labels to numerical format.
Removing outliers using the interquartile range (IQR) method.
Splitting the dataset into training and testing sets.
Model Building
Two types of models are built for predicting breast cancer diagnosis:

Logistic Regression
Regularized Logistic Regression (using glmnet)
Model Evaluation
The performance of each model is evaluated using accuracy metrics and visualizations such as confusion matrices.

Conclusion
The analysis provides insights into breast cancer detection and demonstrates the effectiveness of predictive modeling techniques in healthcare analytics.
