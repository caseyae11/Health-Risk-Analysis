# Ontario Heart Attack Risk Analysis

## 📌 Project Overview
This project analyzes **heart attack risks** using a dataset of health-related factors, including BMI, sleep patterns, and lifestyle choices. The study applies statistical analysis and machine learning to identify key health indicators associated with increased heart attack risk.

- 💉 **Health factors analyzed** → BMI, sleep hours, physical activity, and medical history.
- 📊 **Statistical tests used** → ANOVA, chi-square, and correlation analysis.
- 🤖 **Machine learning models applied** → Logistic regression, LASSO, Ridge regression, and K-means clustering.
- 🔮 **Time series forecasting** → Uses ARIMA models to predict future heart attack trends.

---

## ⚙️ Technologies Used
- **R** → Data analysis and machine learning.
- **tidyverse, dplyr** → Data manipulation.
- **ggplot2, ggpubr** → Data visualization.
- **corrplot, car** → Statistical modeling.
- **glmnet** → LASSO & Ridge Regression.
- **MASS** → Stepwise regression.
- **cluster** → K-Means Clustering.
- **pROC** → ROC Curve Analysis.
- **forecast** → ARIMA time series models.

---

## 🚀 Features
- ✅ **Data Cleaning & Preprocessing** → Handles missing values and converts categorical variables.
- 📊 **Exploratory Data Analysis (EDA)** → Histograms, box plots, and correlation heatmaps.
- 📈 **Statistical Analysis** → ANOVA, chi-square tests, and hypothesis testing.
- 🔮 **Machine Learning Models**:
  - **Logistic Regression** → Predicts likelihood of a heart attack.
  - **LASSO & Ridge Regression** → Feature selection and regularization.
  - **Stepwise Regression** → Selects the most relevant health indicators.
  - **K-Means Clustering** → Groups individuals based on health profiles.
  - **ROC Curve Analysis** → Evaluates model accuracy.
- 📆 **Time Series Forecasting** → ARIMA models predict future heart disease trends.

---

## 🛠 How to Run

```bash
# 1️⃣ Clone the Repository
git clone https://github.com/YOUR-USERNAME/Ontario-Heart-Attack-Risk-Analysis.git
cd Ontario-Heart-Attack-Risk-Analysis
# 2️⃣ Install Required R Packages
install.packages(c("tidyverse", "janitor", "ggplot2", "dplyr", "corrplot", "car", 
                   "ggpubr", "glmnet", "MASS", "cluster", "pROC", "forecast"))
# 3️⃣ Load Dataset
# Ensure 'heart_2022_with_nans.csv' is in the project directory.
# 4️⃣ Run the Analysis
source("Ontario_Heart_Attack_Risk_Analysis.R")
