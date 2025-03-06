# PROJECT: ALY6015-HEART ATTACK RISK ANALYSIS
# Load Required Libraries
library(tidyverse)  # Data manipulation & visualization
library(janitor)    # Data cleaning
library(ggplot2)    # Visualization
library(dplyr)      # Data wrangling
library(corrplot)   # Correlation matrix
library(car)        # ANOVA & regression diagnostics
library(ggpubr)     # Publication-ready plots
library(glmnet)      # LASSO & Ridge Regression
library(MASS)        # Stepwise Regression
library(cluster)     # K-means clustering
library(pROC)        # ROC Curve Analysis
library(forecast)   # Doing time models


# Load Dataset
health_data <- read_csv("heart_2022_with_nans_dataset.csv")

# Standardize Column Names
health_data <- clean_names(health_data)

# Reduce Dataset to 50,000 Rows for Performance
set.seed(123)  # Ensure reproducibility
health_data <- health_data %>% sample_n(50000)

# Handling Missing Values
health_data <- health_data %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Convert Categorical Variables to Factors
health_data$physical_activities <- as.factor(health_data$physical_activities)
health_data$had_heart_attack <- as.character(health_data$had_heart_attack)
health_data$had_heart_attack[health_data$had_heart_attack == ""] <- "No"
health_data$had_heart_attack <- as.factor(health_data$had_heart_attack)
health_data$heart_attack_binary <- ifelse(health_data$had_heart_attack == "Yes", 1, 0)
health_data$sex <- as.factor(health_data$sex)
health_data$race_ethnicity_category <- as.factor(health_data$race_ethnicity_category)
health_data$age_category <- as.factor(health_data$age_category)

# Create Sleep Category Variable
health_data$sleep_category <- cut(
  health_data$sleep_hours,
  breaks = c(0, 5, 7, 9, Inf),
  labels = c("Very Low", "Low", "Moderate", "High"),
  right = FALSE
)

# -------------------------------
# 1. Exploratory Data Analysis (EDA)
# -------------------------------

# Summary Statistics
summary(health_data)

# Histogram of BMI
ggplot(health_data, aes(x = bmi)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of BMI", x = "BMI", y = "Count") +
  theme_minimal()

# Bar Chart - Sleep Categories
ggplot(health_data, aes(x = sleep_category)) +
  geom_bar(fill = "purple", alpha = 0.7) +
  labs(title = "Sleep Category Distribution", x = "Sleep Category", y = "Count") +
  theme_minimal()

# Correlation Heatmap (Numeric Variables)
numeric_vars <- select_if(health_data, is.numeric)
corr_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# BMI Boxplot by Heart Attack Status
ggplot(health_data, aes(x = had_heart_attack, y = bmi, fill = had_heart_attack)) +
  geom_boxplot() +
  labs(title = "BMI Distribution by Heart Attack Status", x = "Heart Attack", y = "BMI") +
  theme_minimal()

# Scatterplot - BMI vs. Sleep Hours
ggplot(health_data, aes(x = sleep_hours, y = bmi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "BMI vs Sleep Hours", x = "Sleep Hours", y = "BMI") +
  theme_minimal()

# -------------------------------
# 2. Statistical Analysis
# -------------------------------

# Chi-Square Test (Physical vs. Mental Health)
chi_data <- na.omit(health_data[, c("physical_health_days", "mental_health_days")])
chi_table <- table(chi_data$physical_health_days, chi_data$mental_health_days)
chi_test <- chisq.test(chi_table)
print(chi_test)

# ANOVA - BMI Differences Across Heart Attack Groups
anova_model <- aov(bmi ~ had_heart_attack, data = health_data)
summary(anova_model)

# Tukey Post-Hoc Test
tukey_results <- TukeyHSD(anova_model)
print(tukey_results)

# -------------------------------
# 3. Logistic Regression Model - Predicting Heart Attacks
# -------------------------------

# Convert Heart Attack Status to Binary (1 = Yes, 0 = No)
health_data$heart_attack_binary <- ifelse(health_data$had_heart_attack == "Yes", 1, 0)

# Logistic Regression Model
logistic_model <- glm(heart_attack_binary ~ bmi + weight_in_kilograms + sleep_hours +
                        sex + race_ethnicity_category + age_category +
                        physical_activities + smoker_status + alcohol_drinkers,
                      data = health_data, family = "binomial")

summary(logistic_model)

# -------------------------------
# 4. Diagnostic Plots (for Model Performance)
# -------------------------------

# Residuals vs Fitted
plot(logistic_model, which = 1)

# Normal Q-Q Plot
plot(logistic_model, which = 2)

# Scale-Location Plot
plot(logistic_model, which = 3)

# Cook's Distance Plot (Identifying Outliers)
plot(logistic_model, which = 4)

# ================================
# PART 2 OF THE PROJECT
# ================================


# -------------------------------
# 1. Regularization - LASSO & Ridge Regression
# -------------------------------

# Define Model Matrix
x <- model.matrix(heart_attack_binary ~ bmi + weight_in_kilograms + sleep_hours + 
                    sex + race_ethnicity_category + age_category + 
                    physical_activities + smoker_status + alcohol_drinkers, 
                  data = health_data)[, -1]
y <- health_data$heart_attack_binary

# LASSO Regression
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")
lasso_best_lambda_min <- lasso_model$lambda.min
lasso_best_lambda_1se <- lasso_model$lambda.1se

# Ridge Regression
ridge_model <- cv.glmnet(x, y, alpha = 0, family = "binomial")
ridge_best_lambda_min <- ridge_model$lambda.min
ridge_best_lambda_1se <- ridge_model$lambda.1se

# Regularization Plots (LASSO & Ridge)
par(mar = c(5, 4, 6, 2)) # Increase top margin
plot(lasso_model)
title("LASSO Regularization Path", line = 4)


par(mar = c(5, 4, 6, 2)) # Increase top margin
plot(ridge_model)
title("Ridge Regularization Path", line = 4)


#compare the ridge and lasso regression models
lasso_best_lambda_1se
ridge_best_lambda_1se

# -------------------------------
# 2. Stepwise Regression Model
# -------------------------------

# Base Logistic Model
full_model <- glm(heart_attack_binary ~ bmi + weight_in_kilograms + sleep_hours + 
                    sex + race_ethnicity_category + age_category + 
                    physical_activities + smoker_status + alcohol_drinkers, 
                  data = health_data, family = "binomial")

# Stepwise Model Selection
stepwise_model <- stepAIC(full_model, direction = "both", trace = FALSE)

# Stepwise AIC Plot
stepwise_aic_values <- stepwise_model$aic
ggplot(data.frame(Step = 1:length(stepwise_aic_values), AIC = stepwise_aic_values), aes(x = Step, y = AIC)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Stepwise Regression AIC Reduction",
       x = "Step",
       y = "AIC Value") +
  theme_minimal()

print(paste("Final AIC Value:", stepwise_model$aic))


# -------------------------------
# 3. Clustering - K-Means
# -------------------------------

# Select Numeric Variables for Clustering
cluster_data <- health_data %>%
  dplyr::select(bmi, weight_in_kilograms, sleep_hours, physical_health_days, mental_health_days)

# Standardize Data
cluster_data_scaled <- scale(cluster_data)

# Determine Optimal Clusters Using Elbow Method
wss <- sapply(1:10, function(k) { kmeans(cluster_data_scaled, centers = k, nstart = 10)$tot.withinss })
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# Apply K-Means Clustering
set.seed(123)
kmeans_model <- kmeans(cluster_data_scaled, centers = 3, nstart = 10)

# Assign Cluster Labels
health_data$cluster_group <- as.factor(kmeans_model$cluster)

# Clustering Visualization - PCA Plot
pca_result <- prcomp(cluster_data_scaled, center = TRUE, scale. = TRUE)
cluster_plot_data <- data.frame(PC1 = pca_result$x[,1], PC2 = pca_result$x[,2], Cluster = factor(kmeans_model$cluster))

ggplot(cluster_plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "K-Means Clustering Results",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# -------------------------------
# 4. ROC Curve - Model Performance
# -------------------------------

# Predict Probabilities
health_data$predicted_probs <- predict(stepwise_model, type = "response")

# Generate ROC Curve
roc_curve <- roc(health_data$heart_attack_binary, health_data$predicted_probs)
auc_value <- auc(roc_curve)

# Plot ROC Curve
ggplot(data.frame(tpr = roc_curve$sensitivities, fpr = 1 - roc_curve$specificities), aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(linetype = "dashed") +
  labs(title = paste("ROC Curve (AUC =", round(auc_value, 3), ")"),
       x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()

# -------------------------------
# 5. Time Series Model (ARIMA)
# -------------------------------

# Generate a Dummy Time Series (Since Dataset Lacks Time Component)
time_series_data <- ts(sample(0:1, 30000, replace = TRUE, prob = c(0.95, 0.05)), frequency = 12)

# Apply Auto ARIMA Model
arima_model <- auto.arima(time_series_data)

# Forecast Next 12 Periods
forecast_result <- forecast(arima_model, h = 12)

# Plot Forecast
autoplot(forecast_result) +
  ggtitle("Forecast of Heart Attack Trends") +
  ylab("Predicted Cases") +
  xlab("Time")

# -------------------------------
# 6. Save Final Processed Dataset
# -------------------------------

write.csv(health_data, "processed_health_data_final.csv", row.names = FALSE)
print("Analysis complete! Results saved.")
