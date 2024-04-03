cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(tidyverse)
library(dplyr)
library(corrplot)
library(ISLR)
library(tidyverse)
library(ggplot2)
library(glmnet)
library(Metrics)
library(caret)


# Load the dataset
insurance_data <- read_csv('healthinsurance.csv')
print(insurance_data)


# Summary of the dataset
summary(insurance_data)



# Head of the dataset
head(insurance_data)


# Structure of the dataset
str(insurance_data)



# Check for missing values
sum(is.na(insurance_data))


# Check for missing values in each column
missing_values_column <- colSums(is.na(insurance_data))
print(missing_values_column)



# Rename the 'reg_ex' column to 'regular_exercise'
insurance_data <- insurance_data %>%
  rename(regular_exercise = regular_ex)


# Impute missing values for 'age' with mean
insurance_data$age <- ifelse(is.na(insurance_data$age), mean(insurance_data$age, na.rm = TRUE), insurance_data$age)

# Impute missing values for 'bmi' with mean
insurance_data$bmi <- ifelse(is.na(insurance_data$bmi), mean(insurance_data$bmi, na.rm = TRUE), insurance_data$bmi)


# Check for missing values
sum(is.na(insurance_data))


# Correlation matrix for numeric variables
correlation_matrix <- cor(insurance_data %>% select_if(is.numeric))
print(correlation_matrix)


# Plot the correlation matrix
corrplot(correlation_matrix, method = "color")




# Assuming 'insurance_data' is your dataframe
ggplot(insurance_data, aes(x = claim)) +
  geom_histogram(binwidth = 1000, fill = "darkviolet", color = "black") +
  ggtitle("Distribution of Insurance Claim Amounts")+
  theme_minimal() + 
  theme(panel.grid = element_blank())



# Create a scatter plot without gridlines
ggplot(insurance_data, aes(x = age, y = claim)) +
  geom_point(col = 'orangered') +
  ggtitle("Insurance Claim Amount vs Age") +
  theme_minimal() + 
  theme(panel.grid = element_blank())



# Create a scatter plot without gridlines and color-coded by smoking status
ggplot(insurance_data, aes(x = bmi, y = claim, color = smoker)) +
  geom_point() +
  ggtitle("Insurance Claim Amount vs BMI (colored by smoking status)") +
  theme_minimal() +
  theme(panel.grid = element_blank())


#Lasso3
# Bar plot for smoker and claim
ggplot(insurance_data, aes(x = factor(smoker), fill = factor(smoker))) +
  geom_bar() +
  labs(title = "Distribution of Claims by Smoking Status") +
  scale_x_discrete(labels = c("Non-Smoker", "Smoker")) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Boxplot for age by diabetes status 
ggplot(insurance_data, aes(x = factor(diabetes), y = age, fill = factor(diabetes))) +
  geom_boxplot() +
  labs(title = "Distribution of Age by Diabetes Status") +
  scale_x_discrete(labels = c("No Diabetes", "Diabetes")) +
  scale_fill_manual(values = c("orangered", "lightgreen")) +  # Define custom colors
  theme_minimal() +
  theme(panel.grid = element_blank())

  
  # Boxplot for BMI
  ggplot(insurance_data, aes(x = factor(diabetes), y = bmi, fill = factor(diabetes))) +
    geom_boxplot() +
    labs(title = "Distribution of BMI by Diabetes Status") +
    scale_x_discrete(labels = c("No Diabetes", "Diabetes")) +
    scale_fill_manual(values = c("orangered", "lightgreen")) +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  
  
  # Scatter plot for age vs. claim with color encoding for regular exercise
  ggplot(insurance_data, aes(x = age, y = claim, color = factor(regular_exercise))) +
    geom_point(alpha = 0.7) +
    labs(title = "Scatter Plot: Age vs. Health Insurance Claim Amounts with Regular Exercise Encoding") +
    scale_color_discrete(labels = c("No Exercise", "Exercise")) +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  
  
  # Bar plot for average health insurance claims by regular exercise
  ggplot(insurance_data, aes(x = factor(regular_exercise), y = claim, fill = factor(regular_exercise))) +
    stat_summary(fun = "mean", geom = "bar") +
    labs(title = "Impact of Regular Exercise on Average Health Insurance Claims", ylab = "Regular Exercise") +
    scale_x_discrete(labels = c("No Exercise", "Exercise")) +
    scale_fill_manual(values = c("palevioletred1", "plum1")) +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  # Select the columns of interest
  columns_outliers <- c('age', 'bmi', 'weight', 'bloodpressure')
  # Reshape the data for ggplot2
  outlier_df <- tidyr::gather(insurance_data, key = "Variable", value = "Value", columns_outliers)

  # Create a boxplot using ggplot2 for the data without outliers
  ggplot(outlier_df, aes(x = Variable, y = Value)) +
    geom_boxplot(fill = "tan1", color = "black", width = 0.4) +
    labs(title = "Boxplot of Age, BMI, Weight, Blood Pressure (Outliers Removed)") +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  # Remove outliers based on IQR for 'bmi' and 'bloodpressure'
  insurance_data_no_outliers <- insurance_data %>%
    filter(
      between(bmi, quantile(bmi, 0.25) - 1.5*IQR(bmi), quantile(bmi, 0.75) + 1.5*IQR(bmi)) &
        between(bloodpressure, quantile(bloodpressure, 0.25) - 1.5*IQR(bloodpressure), quantile(bloodpressure, 0.75) + 1.5*IQR(bloodpressure))
    )
  
  # Reshape the data for ggplot2
  outlier_df <- tidyr::gather(insurance_data_no_outliers, key = "Variable", value = "Value", columns_outliers)
  
  # Create a boxplot using ggplot2 for the data without outliers
  ggplot(outlier_df, aes(x = Variable, y = Value)) +
    geom_boxplot(fill = "tan1", color = "black", width = 0.4) +
    labs(title = "Boxplot of Age, BMI, Weight, Blood Pressure (Outliers Removed)") +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  
  # Final dataset without the outliers
  final_dataset <- insurance_data_no_outliers
  
  # Convert 'sex' to binary (1 for 'male', 0 for 'female')
  final_dataset$sex <- as.numeric(final_dataset$sex  == "male")

  # Mapping values for 'hereditary_diseases'
  hereditary_diseases_mapping <- c('NoDisease' = 0, 'Epilepsy' = 1, 'EyeDisease' = 2, 
                                   'Alzheimer' = 3, 'Arthritis' = 4, 'HeartDisease' = 5, 
                                   'Diabetes' = 6, 'Cancer' = 7, 'High BP' = 8, 'Obesity' = 9)
  
  # Apply mapping to 'hereditary_diseases' column in 'final_dataset'
  final_dataset <- final_dataset %>%
    mutate(hereditary_diseases = recode(hereditary_diseases, !!!hereditary_diseases_mapping))
  
  
#1.	How does age compare to other factors like weight or exercise in predicting health insurance claim amounts? 
# Fit a linear regression model
Linear_regression_model <- lm(claim ~ age + weight + regular_exercise, data = final_dataset)
  
# Summarize the model
summary(Linear_regression_model)

# Create separate plots for each predictor
#par(mfrow = c(1, 3))  # Set up the layout for multiple plots
dev.off()

# Plot for Age vs. Claim
plot(final_dataset$age, final_dataset$claim, main = "Age vs. Claim", xlab = "Age", ylab = "Claim Amount", col = "darkolivegreen2", pch = 16)
abline(Linear_regression_model$coefficients[1], Linear_regression_model$coefficients[2], col = "darkmagenta", lwd = 2)

# Plot for Weight vs. Claim
plot(final_dataset$weight, final_dataset$claim, main = "Weight vs. Claim", xlab = "Weight", ylab = "Claim Amount", col = "plum3", pch = 16)
abline(Linear_regression_model$coefficients[1], Linear_regression_model$coefficients[3], col = "orangered4", lwd = 2)




# 2.	Can we predict the likelihood of an individual having diabetes based on age and BMI? 
  
# Fit a logistic regression model
logistic_model <- glm(diabetes ~ age + bmi, data = final_dataset, family = "binomial")
  
# Summarize the model
summary(logistic_model)

# Predict probabilities
final_dataset$predicted_prob <- predict(logistic_model, type = "response")

#  ROC Curve
library(pROC)
roc_obj <- roc(final_dataset$diabetes, final_dataset$predicted_prob)
plot(roc_obj, main = "ROC Curve")
abline(a = 0, b = 1, lty = 2)


# Area under the curve
Auc_curve <- auc(roc_obj)
# Display the Auc curve
Auc_curve



#3.	How do the variables relate to smoking impact health insurance claims?  

# Split the data into training and testing sets (e.g., 80-20 split)
set.seed(123)  # for reproducibility
train_index <- sample(seq_len(nrow(final_dataset)), 0.7 * nrow(final_dataset))
train_data <- final_dataset[train_index, ]
test_data <- final_dataset[-train_index, ]


# Prepare the training data without intercept
X_train <- model.matrix(claim ~ smoker + age + weight + diabetes - 1, data = train_data)
y_train <- train_data$claim

# Fit Lasso regression model on training data
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)
print(lasso_model)

# Plot the coefficients
plot(lasso_model)

# Prepare the testing data without intercept
X_test <- model.matrix(claim ~ smoker + age + weight + regular_exercise - 1, data = test_data)
y_test <- test_data$claim

# Predict on the testing data
predictions <- predict(lasso_model, newx = X_test, s = lasso_model$lambda.min)

# Calculate RMSE
rmse_lasso <- sqrt(mean((predictions - y_test)^2))
print(paste("RMSE:", rmse_lasso))


#4.How does the inclusion of 'regular_ex' (regular exercise) and bloodpressure impact health insurance claims?

# Split the data into training and testing sets
set.seed(123)
index <- createDataPartition(final_dataset$claim, p = 0.7, list = FALSE)
train_data <- final_dataset[index, ]
test_data <- final_dataset[-index, ]


# Prepare the training data without intercept
X_train_ridge <- model.matrix(claim ~ regular_exercise + bloodpressure - 1, data = train_data)
y_train_ridge <- train_data$claim

# Fit Ridge regression model
ridge_model <- cv.glmnet(X_train_ridge, y_train_ridge, alpha = 0)  # alpha = 0 for Ridge
print(ridge_model)
# Plot the coefficients
plot(ridge_model)

# Display the optimal lambda value chosen by cross-validation
print(ridge_model$lambda.min)

# Prepare the testing data without intercept
X_test_ridge <- model.matrix(claim ~ regular_exercise + bloodpressure - 1, data = test_data)
y_test_ridge <- test_data$claim

# Make predictions
predictions <- predict(ridge_model, s = ridge_model$lambda.min, newx = X_test_ridge)

# Calculate RMSE
rmse_ridge <- sqrt(mean((predictions - y_test_ridge)^2))
print(paste("RMSE:", rmse_ridge))

