# Install and load required packages
required_packages <- c("readxl", "tidyverse", "janitor", "ggpubr", "moments", "nortest")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(readxl)
library(tidyverse)
library(janitor)
library(ggpubr)
library(moments)
library(nortest)

# Set working directory (modify if necessary)
setwd("/Users/colm.mulcahy/Documents/R_projects/UOE_Probability and Statistics_Final Assignment")

# Read and clean datasets
covariates <- read_excel("covariates.xlsx") %>% clean_names()
biomarkers <- read_excel("biomarkers.xlsx") %>% clean_names()

# Split 'biomarker' column into 'patient_id' and 'time_point'
biomarkers <- biomarkers %>%
  separate(biomarker, into = c("patient_id", "time_point"), sep = "-") %>%
  mutate(patient_id = as.numeric(patient_id))  # Ensure numeric patient ID

# Merge datasets
merged_data <- biomarkers %>%
  left_join(covariates, by = "patient_id")

# Create High/Low VAS grouping
merged_data <- merged_data %>%
  mutate(vas_group = ifelse(vas_at_inclusion >= 5, "High_VAS", "Low_VAS"))

# Verify merging
print(table(merged_data$vas_group))

# Identify numeric biomarker columns
biomarker_cols <- setdiff(colnames(merged_data), c("patient_id", "time_point", "vas_group", "vas_at_inclusion"))

# **Step 1: Normality Testing**
# Perform Shapiro-Wilk test for normality on all biomarkers
normality_results <- sapply(merged_data[biomarker_cols], function(x) shapiro.test(x)$p.value)

# Identify normally distributed biomarkers
normally_distributed <- names(normality_results[normality_results > 0.05])
non_normal_biomarkers <- names(normality_results[normality_results <= 0.05])

# Print results
print("Normality Test Results (p-values):")
print(normality_results)
print("Normally distributed biomarkers:")
print(normally_distributed)

# **Step 2: Hypothesis Testing**
# Apply t-test for normal biomarkers and Wilcoxon for non-normal ones
test_results <- lapply(biomarker_cols, function(b) {
  if (b %in% normally_distributed) {
    test <- t.test(merged_data[[b]] ~ merged_data$vas_group, data = merged_data)
    return(data.frame(Biomarker = b, p_value = test$p.value, Test = "t-test"))
  } else {
    test <- wilcox.test(merged_data[[b]] ~ merged_data$vas_group, data = merged_data)
    return(data.frame(Biomarker = b, p_value = test$p.value, Test = "Wilcoxon"))
  }
})

# Convert results to dataframe
test_results_df <- do.call(rbind, test_results)

# Print test results before correction
print("Hypothesis Test Results (Before Bonferroni Correction):")
print(test_results_df)

# **Step 3: Multiple Testing Correction (Bonferroni)**
test_results_df$adjusted_p_value <- p.adjust(test_results_df$p_value, method = "bonferroni")

# Print results after correction
print("Hypothesis Test Results (After Bonferroni Correction):")
print(test_results_df)

# **Step 4: Visualizations**
# Histogram of biomarker levels
ggplot(merged_data %>% pivot_longer(cols = all_of(biomarker_cols), names_to = "Biomarker", values_to = "Value"), 
       aes(x = Value)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  facet_wrap(~Biomarker, scales = "free") +
  ggtitle("Histograms of Biomarkers") +
  xlab("Biomarker Levels") +
  ylab("Frequency") +
  theme_minimal()

# Q-Q plots for normality check
ggplot(merged_data %>% pivot_longer(cols = all_of(biomarker_cols), names_to = "Biomarker", values_to = "Value"), 
       aes(sample = Value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Biomarker, scales = "free") +
  ggtitle("Q-Q Plots for Biomarkers") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

# Boxplot of IL-18 Levels by VAS Group (as an example)
ggplot(merged_data, aes(x = vas_group, y = il_18, fill = vas_group)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, color = "black") +
  ggtitle("IL-18 Levels by VAS Group (t-test applied)") +
  xlab("VAS Group") +
  ylab("IL-18 Levels") +
  theme_minimal() +
  scale_fill_manual(values = c("High_VAS" = "red", "Low_VAS" = "blue"))

# Visualize Bonferroni Correction Results
bonferroni_results <- test_results_df %>%
  select(Biomarker, p_value, adjusted_p_value) %>%
  pivot_longer(cols = c(p_value, adjusted_p_value), names_to = "Type", values_to = "Value")

ggplot(bonferroni_results, aes(x = Biomarker, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Raw vs. Bonferroni-Corrected p-Values") +
  ylab("p-Value") +
  theme_minimal() +
  scale_fill_manual(values = c("p_value" = "steelblue", "adjusted_p_value" = "darkred")) +
  coord_flip()

# Install and load required packages
required_packages <- c("readxl", "tidyverse", "janitor", "ggpubr", "moments", "nortest", "caret", "broom", "performance", "car", "GGally", "randomForest", "knitr", "kableExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Install and load required packages
required_packages <- c("readxl", "tidyverse", "janitor", "ggpubr", "moments", "nortest", 
                       "caret", "broom", "performance", "car", "randomForest", "GGally", "ggcorrplot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(readxl)
library(tidyverse)
library(janitor)
library(ggpubr)
library(moments)
library(nortest)
library(caret)
library(broom)
library(performance)
library(car)
library(randomForest)
library(GGally)
library(ggcorrplot)

# Set working directory (update if necessary)
setwd("/Users/colm.mulcahy/Documents/R_projects/UOE_Probability and Statistics_Final Assignment")

# Read and clean datasets
covariates <- read_excel("covariates.xlsx") %>% clean_names()
biomarkers <- read_excel("biomarkers.xlsx") %>% clean_names()

# Merge datasets and clean
merged_data <- biomarkers %>%
  separate(biomarker, into = c("patient_id", "time_point"), sep = "-") %>%
  mutate(patient_id = as.numeric(patient_id)) %>%
  left_join(covariates, by = "patient_id") %>%
  drop_na()

# Create binary pain group variable (High VAS ≥5, Low VAS <5)
merged_data <- merged_data %>%
  mutate(vas_group = ifelse(vas_at_inclusion >= 5, "High_VAS", "Low_VAS"))

# Select relevant variables for regression analysis
regression_data <- merged_data %>%
  select(vas_12months, il_8, vegf_a, opg, tgf_beta_1, il_6, cxcl9, cxcl1, il_18, csf_1, age, sex_1_male_2_female, smoker_1_yes_2_no) %>%
  drop_na()

# Step 1: Exploratory Data Analysis (EDA)
## Check normality of biomarkers
normality_results <- sapply(regression_data %>% select(-vas_12months), function(x) shapiro.test(x)$p.value)
print("Shapiro-Wilk Normality Test (p-values):")
print(round(normality_results, 3))

## Correlation matrix
cor_matrix <- cor(regression_data %>% select(-vas_12months))
ggcorrplot(cor_matrix, method = "circle", title = "Correlation Matrix of Predictors")

## Variance Inflation Factor (VIF) for collinearity check
initial_model <- lm(vas_12months ~ ., data = regression_data)
vif_values <- car::vif(initial_model)
print("Variance Inflation Factor (VIF) Scores:")
print(vif_values)

# Step 2: Model Training & Performance Evaluation
## Split Data: 80% Training, 20% Testing
set.seed(42)
train_indices <- createDataPartition(regression_data$vas_12months, p = 0.8, list = FALSE)
train_data <- regression_data[train_indices, ]
test_data  <- regression_data[-train_indices, ]

# Fit Linear Regression Model (for comparison)
lm_model <- lm(vas_12months ~ ., data = train_data)
summary(lm_model)

# Compute Model Performance for Linear Regression
test_data$lm_pred <- predict(lm_model, newdata = test_data)
lm_performance <- data.frame(
  RMSE = sqrt(mean((test_data$lm_pred - test_data$vas_12months)^2)),
  MAE = mean(abs(test_data$lm_pred - test_data$vas_12months)),
  R2 = cor(test_data$lm_pred, test_data$vas_12months)^2
)
print("Linear Regression Performance:")
print(lm_performance)

# Fit Random Forest Model
set.seed(42)
rf_model <- randomForest(vas_12months ~ ., data = train_data, ntree = 500, importance = TRUE)
print(rf_model)

# Make predictions on test data
test_data$rf_pred <- predict(rf_model, newdata = test_data)

# Compute Model Performance for Random Forest
rf_performance <- data.frame(
  RMSE = sqrt(mean((test_data$rf_pred - test_data$vas_12months)^2)),
  MAE = mean(abs(test_data$rf_pred - test_data$vas_12months)),
  R2 = cor(test_data$rf_pred, test_data$vas_12months)^2
)
print("Random Forest Performance:")
print(rf_performance)

# Step 3: Feature Importance Analysis
importance_scores <- importance(rf_model)
importance_df <- data.frame(
  Biomarker = rownames(importance_scores),
  Importance = importance_scores[, 1]
) %>%
  arrange(desc(Importance))

# Print Feature Importance
print("Feature Importance (Mean Decrease in Accuracy):")
print(importance_df)

# Step 4: Visualization
## Regression Coefficients (for Linear Model)
lm_results <- tidy(lm_model) %>%
  mutate(across(c(estimate, std.error, statistic, p.value), round, 3))

ggplot(lm_results, aes(x = reorder(term, estimate), y = estimate, fill = p.value < 0.05)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey")) +
  ggtitle("Linear Regression Coefficients for 12-Month VAS Prediction") +
  xlab("Predictor Variables") +
  ylab("Estimated Effect on 12-Month VAS") +
  theme(legend.position = "none")

## Predicted vs. Actual (Random Forest)
ggplot(test_data, aes(x = vas_12months, y = rf_pred)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  ggtitle("Random Forest: Predicted vs. Actual 12-Month VAS Scores") +
  xlab("Actual VAS Score") +
  ylab("Predicted VAS Score") +
  theme_minimal()

## Feature Importance Plot
ggplot(importance_df, aes(x = reorder(Biomarker, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  coord_flip() +
  ggtitle("Feature Importance: Random Forest Model") +
  xlab("Biomarkers & Covariates") +
  ylab("Mean Decrease in Accuracy") +
  theme_minimal()

# Final Output
list(
  "Linear Regression Performance" = lm_performance,
  "Random Forest Performance" = rf_performance,
  "Feature Importance" = importance_df
)
