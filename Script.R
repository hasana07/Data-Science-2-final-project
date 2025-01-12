# BTC1877 Team Project

#### Preparation ####
# Install and load packages.
install.packages("readxl")
install.packages("funModeling")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("mice")
install.packages("glmnet")
install.packages("pROC")
install.packages("tree")
install.packages("survival")
install.packages("car")
install.packages("lubridate")
library(readxl)
library(funModeling)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(mice)
library(glmnet)
library(pROC) 
library(tree)
library(survival)
library(car)
library(lubridate)

# Import data.
raw_data <- read_excel("transfusion_data.xlsx")

#' For simplicity, let's consider only highlighted variables and variables of
#' interest.
working <- raw_data[,c(1,3:15,25,26,28:41,46,47,49,50,57,60:65,77:92,116,117)]

# Assess NAs.
which(is.na(working))
# Lots of NAs and blank spaces.
status(working)
#' Massive NA values for Pre_Fibrinogen, RBC timepoints, FFP timepoints, 
#' Plt timepoints and Cryo timepoints. Remove these variables.
#' QUESTION: What's our rationale for removing massively missing values?
#' As opposed to imputing (i.e., what is the threshold for imputation?).
missing_variables <- c(26,42:44,46:48,50:52,54:56)
missing_table <- working %>%
  summarise(across(all_of(missing_variables), list(
    na_count = ~sum(is.na(.x)),
    na_proportion = ~mean(is.na(.x))
    ))) %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)$")
print(missing_table, n=Inf)
working <- working[,-c(26,42:44,46:48,50:52,54:56)]

# Refactor variables.
# Some character variables should be factors.
working <- working %>%
  mutate(across(c(Type, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, 
                  `Massive Transfusion`, COPD), as.factor))
# Some character variables should be logical.
working <- working %>%
  mutate(across(c(`Gender (male)`, COPD, `alpha1-Antitrypsin Deficiency`, 
                  `Cystic Fibrosis`, `Idiopathic Pulmonary Hypertension`, 
                  `Interstitial Lung Disease`, Pulm_Other, 
                  `First Lung Transplant`, `Redo Lung Transplant`, 
                  `ExVIVO Lung Perfusion`, `Preoperative ECLS`, ECLS_ECMO, 
                  ECLS_CPB), ~ as.logical(.)))
# Some character and POSIXCT variables should be Date type.
working$DEATH_DATE <- as.Date(working$DEATH_DATE, format = "%d-%b-%Y")
working$`OR Date` <- as.Date(working$`OR Date`, format = "%d-%b-%Y")
str(working)

#### Variable Selection from Literature Review ####
working <- working[,-c(5,6,14,17,26)]

#### Remove Collinear Variables ####
#'First_Lung_Transplant and Redo_Lung_Transplant are opposite
#' (negative collinearity). Remove First_Lung_Transplant.
working <- working[,-12]

#'Removing intraoperative ECLS since it is collinear with ECLS_ECMO
#' and ECLS_CPB.
working <- working[,-21]

#### EDA ####
# Generate descriptive statistics for numeric variables.
which(sapply(working, is.numeric))
num_variables <- c(1,5,6,14,20,23:27,32:38)
num_table <- working %>%
  summarise(across(all_of(num_variables), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    iqr = ~IQR(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)$")
# Print table.
num_table %>%
  kable(
    format = "markdown",  # Use "markdown" for plain text, "html" for R Markdown rendering
    caption = "Summary Statistics for Numerical Variables",
    digits = 2  # Number of decimal places
  )

# Generate descriptive statistics for factor variables.
which(sapply(working, is.factor))
factor_variables <- c(3,29:31,39)
factor_table <- working %>%
  dplyr::select(all_of(factor_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
# Print table.
factor_table %>%
  arrange(variable, desc(n)) %>%  # Optional: Arrange by variable and count
  kable(
    format = "markdown",  # Use "markdown" for plain text, "html" for R Markdown
    caption = "Summary of Factor Variables",
    digits = 2,  # For proportion formatting
    col.names = c("Variable", "Value", "Count", "Proportion")  # Rename columns
  )

# Generate descriptive statistics for logical variables.
which(sapply(working, is.logical))
logical_variables <- c(4,7:13,21,22)
logical_table <- working %>%
  dplyr::select(all_of(logical_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
# Print table.
logical_table %>%
  arrange(variable, desc(n)) %>%  # Optional: Sort by variable and count
  kable(
    format = "markdown",  # Use "markdown" for plain text; "html" for R Markdown
    caption = "Summary of Logical Variables",
    digits = 2,  # Number of decimal places for proportions
    col.names = c("Variable", "Value", "Count", "Proportion")  # Rename columns for clarity
  )

# Generate descriptive statistics (NAs) for date variables.
which(sapply(working, inherits, "Date"))
date_variables <- c(2,28)
date_table <- working %>%
  dplyr::select(all_of(date_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(value = ifelse(is.na(value), "NA", "Not NA")) %>%  # Group values as "NA" or "Not NA"
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
# Print table.
date_table %>%
  arrange(variable, desc(n)) %>%  # Optional: Sort by variable and count
  kable(
    format = "markdown",  # Use "markdown" for plain text, "html" for R Markdown
    caption = "Summary of Date Variables (NA vs Not NA)",
    digits = 2,  # Number of decimal places for proportions
    col.names = c("Variable", "Value", "Count", "Proportion")  # Rename columns for clarity
  )

#' Create histogram for each numerical variable and bar chart for each
#' categorical variable in data set.
for (var in names(working)) {
  if (is.numeric(working[[var]]) && length(unique(working[[var]])) < 135) {
    histogram <- ggplot(working, aes(x = .data[[var]])) +
      geom_histogram(fill = "skyblue", binwidth = 1, na.rm = TRUE,
                     col = "black") +
      labs(title = paste0("Distribution of ", var), x = var, y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 18))
    print(histogram)
  } else if (is.numeric(working[[var]])) {
    histogram <- ggplot(working, aes(x = .data[[var]])) +
      geom_histogram(fill = "skyblue", binwidth = 5, na.rm = TRUE,
                     col = "black") +
      labs(title = paste0("Distribution of ", var), x = var, y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 18))
    print(histogram)
  } else if (is.factor(working[[var]]) || is.logical(working[[var]])) {
    barchart <- ggplot(working, aes(x = .data[[var]])) +
      geom_bar(fill = "salmon", col = "black") +
      labs(title = paste0("Distribution of ", var), x = var, y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(, hjust = 0.5, size = 18))
    print(barchart)
  }
}

# Create histogram for date variables.
# For OR Date.
date_bins_1 <- floor_date(working$`OR Date`, unit = "month")
ggplot(data = data.frame(working$`OR Date`), aes(x = date_bins_1)) +
  geom_histogram(binwidth = 30, fill = "orange", color = "black") +
  labs(title = "Distribution of OR Date", x = "Date", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# For Death Date.
date_bins_2 <- floor_date(working$DEATH_DATE , unit = "month")
ggplot(data = data.frame(working$`DEATH_DATE`), aes(x = date_bins_2)) +
  geom_histogram(binwidth = 30, fill = "orange", color = "black") +
  labs(title = "Distribution of Death Date", x = "Date", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#### Question 1. Characteristics of Patients that Require Transfusions ####
#'Create data set of patients that required transfusion based on 
#' Total 24hr RBC.
working_char <- working %>%
  mutate(RBC_Transfusion = if_else(`Total 24hr RBC` > 0, 1, 0))
working_char <- working_char[which(working_char$RBC_Transfusion == 1),]

# For numerical variables.
num_table_char <- working_char %>%
  summarise(across(all_of(num_variables), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    iqr = ~IQR(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)$")
# Print table.
num_table_char %>%
  kable(
    format = "markdown",  # Use "markdown" for plain text, "html" for R Markdown rendering
    caption = "Summary Statistics for Numerical Variables",
    digits = 2  # Number of decimal places
  )

# Generate descriptive statistics for factor variables.
factor_table_char <- working_char %>%
  dplyr::select(all_of(factor_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
# Print table.
factor_table_char %>%
  arrange(variable, desc(n)) %>%  # Optional: Arrange by variable and count
  kable(
    format = "markdown",  # Use "markdown" for plain text, "html" for R Markdown
    caption = "Summary of Factor Variables",
    digits = 2,  # For proportion formatting
    col.names = c("Variable", "Value", "Count", "Proportion")  # Rename columns
  )

# Generate descriptive statistics for logical variables.
logical_table_char <- working_char %>%
  dplyr::select(all_of(logical_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
# Print table.
logical_table_char %>%
  arrange(variable, desc(n)) %>%  # Optional: Sort by variable and count
  kable(
    format = "markdown",  # Use "markdown" for plain text; "html" for R Markdown
    caption = "Summary of Logical Variables",
    digits = 2,  # Number of decimal places for proportions
    col.names = c("Variable", "Value", "Count", "Proportion")  # Rename columns for clarity
  )

#### Imputation of Variables with Missingness ####
# Create table for variables with missingness.
missing_variables <- c(14,20,27)
imputation_table <- working %>%
  dplyr::select(all_of(missing_variables)) %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Number_of_NAs") %>%
  mutate(Proportion_of_NAs = Number_of_NAs / nrow(working))
# Print table.
imputation_table %>%
  kable(caption = "Summary of Missing Values by Variable",
        col.names = c("Variable", "Number of NAs", "Proportion of NAs"))

# Create imputation data set with dates removed, as they cannot used for predictions.
impute_data <- working[,-c(2,28)]
# Replace spaces with underscores, not accepted by mice function.
colnames(impute_data) <- gsub(" ", "_", colnames(impute_data))  
# Remove non-alphanumeric characters, not accepted by mice function.
colnames(impute_data) <- gsub("[^[:alnum:]_]", "", colnames(impute_data)) 
# Impute and create mids object.
imp_stoch <- mice(impute_data, method = "pmm", seed = 123, m = 1, print = FALSE)
summary(imp_stoch)
# Plot the newly imputed values.
xyplot(imp_stoch, LAS_score ~ Age)
xyplot(imp_stoch, Pre_PTT ~ Age)
xyplot(imp_stoch, Duration_of_ICU_Stay_days ~ Age)
# Unable to impute this variable due to collinearity.
# Convert from mids object to data frame.
imp_stoch_df <- complete(imp_stoch)
# Move imputed variables back to working copy.
working$`LAS score` <- imp_stoch_df$LAS_score
working$Pre_PTT <- imp_stoch_df$Pre_PTT
status(working)
# Remove observation with un-imputable value.
which(is.na(working$`Duration of ICU Stay (days)`))
working <- working[-85,]

#### Question 1: Data Set Creation and Cleaning ####
# Create a new data set for classification called "working_class".
#'Add a "RBC Transfusion" column to the data frame. If "Total 24hr 
#' RBC" value is greater than 0, indicate it as 1 (for transfusion).
#' If it is 0, then indicate it as 0 (no transfusion).
working_class <- working %>%
  mutate(RBC_Transfusion = if_else(`Total 24hr RBC` > 0, 1, 0))
# Convert new response variable to factor.
working_class$RBC_Transfusion <- as.factor(working_class$RBC_Transfusion)

# Convert logical variables to factors for upcoming analyses.
working_class <- working_class %>%
  mutate_if(is.logical, as.factor)

# Keep RBC transfusion outcome and predictors for RBC transfusion. 
working_class <- working_class %>%
  select(RBC_Transfusion, Type, `Gender (male)`, Age, BMI, COPD,
    `alpha1-Antitrypsin Deficiency`, `Cystic Fibrosis`,
    `Idiopathic Pulmonary Hypertension`, `Interstitial Lung Disease`,
    `Redo Lung Transplant`, `Preoperative ECLS`,
    `LAS score`, Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, 
    Pre_PTT, ECLS_ECMO, ECLS_CPB) %>%
  # Replace spaces in variable names to underscores.
  rename_with(~ gsub(" ", "_", .x))

# Rename potential problematic columns, for ease of further analyses.
working_class <- working_class %>%
  rename(alpha1_Antitrypsin_Deficiency = `alpha1-Antitrypsin_Deficiency`)
# Replace non-alphanumeric characters in variable names with underscores.
colnames(working_class) <- gsub("[^A-Za-z0-9_]", "_", colnames(working_class))

#### Question 1: Compare Lasso Classification and Pruned Tree Models ####
#'Make a loop to compare the performance of lasso classification 
#' and pruned tree models over 5 different seeds (including the seed 
#' above. This will help decide which model to use, by assessing and 
#' comparing the models' performances via AUC. 

# Make a data frame to store AUCs for each model for each iteration.
auc_table <- data.frame(Iteration = integer(), AUC_LASSO = numeric(), AUC_Pruned_Tree = numeric())

#'Loop through 5 creations of lasso classification and pruned tree 
#' models (different seed each time) and generate AUC for each, 
#' saving into "auc_table". 
for (i in 1:5) {
  # Different seed for each iteration.
  set.seed(10 + i)
  # Split the data into training and test sets
  train_data <- sample(nrow(working_class), 
                       round(nrow(working_class) / 2))
  
  # Create training set model matrix with predictors.
  x_train <- model.matrix(
    RBC_Transfusion ~ ., working_class)[train_data, -1]
  # Create vector with training set response values.
  y_train <- working_class$RBC_Transfusion[train_data]
  
  # Create in-loop lasso classification model.
  lasso_model <- glmnet(x_train, y_train, family = "binomial")
  # Cross-validate model for lambda selection, 5 folds selected.
  cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, 
                        family = "binomial", type.measure = "auc", 
                        nfolds = 5)
  optimal_lambda <- cv_lasso$lambda.1se
  # Can also try: 
  # optimal_lambda <- cv_lasso$lambda.min
  
  # Create vector of predicted probabilities using the test set.
  pred_lasso <- predict(
    lasso_model, newx = model.matrix(
      RBC_Transfusion ~ ., working_class)[-train_data, -1], 
    s = optimal_lambda, type = "response")
  # Calculating test set AUC for lasso classification model.
  roc_lasso <- roc(working_class$RBC_Transfusion[-train_data], 
                   as.numeric(pred_lasso))
  # Assign AUC to "auc_lasso".
  auc_lasso <- roc_lasso$auc
  
  # Create in-loop classification tree using training set.
  tree_model <- tree::tree(RBC_Transfusion ~ ., data = working_class, subset = train_data)
  # Cross-validation for pruning of tree model.
  cv_res <- cv.tree(tree_model, FUN = prune.tree)
  # Determine the best size of the tree based on minimum deviance.
  best_size <- cv_res$size[which.min(cv_res$dev)]
  # If best size is 1, then make it 2. We don't want a trunk.
  best_size <- ifelse(
    best_size == 1, 2,
    best_size
  )
  # Prune the tree using the best size.
  pruned_tree <- prune.tree(tree_model, best = best_size)

  # Create vector of predicted probabilities using the test set.
  preds_pruned <- predict(pruned_tree, 
                          newdata = working_class[-train_data, ], 
                          type = "vector")
  # Converts probabilities from matrix form to numerical vector.
  pred_probs_pruned <- as.numeric(preds_pruned[, 2])
  # Calculate AUC for pruned tree.
  roc_pruned_tree <- roc(working_class$RBC_Transfusion[-train_data], pred_probs_pruned)
  # Assign AUC to "auc_pruned_tree".
  auc_pruned_tree <- roc_pruned_tree$auc
  
  # Update the AUC values from each model to "auc_table".
  auc_table <- rbind(
    auc_table,
    data.frame(Iteration = i, AUC_LASSO = auc_lasso, AUC_Pruned_Tree = auc_pruned_tree
    )
  )
}
# Print table with AUC values from each model over five seeds.
print(auc_table)

#'Lasso classification models perform consistently better via measure
#' of AUC. Highest AUC value was for the lasso model at iteration 5 
#' (seed = 65).

#### Question 1: Create Full Lasso Classification Model ####

# Create matrix of predictor features using full data set.
xfull1 <- model.matrix(RBC_Transfusion ~.,working_class)
# Create a vector with the response values for the training set.
yfull1 <- working_class$RBC_Transfusion

# Create lasso classification model, with binomial family.
lasso_model_full1 <- glmnet(xfull1, yfull1, family = "binomial")
# Plot lambda against coefficient weights. 
plot(lasso_model_full1,label = T, xvar = "lambda")

# Use cross validation to tune the model. 5 folds specified.
cv_lasso_full1 <- cv.glmnet(xfull1, yfull1, alpha = 1, family = "binomial", 
                       type.measure = "auc", nfolds = 5)
# Plot AUC for this model for different values of lambda.
plot(cv_lasso_full1)
# View lambda that maximizes the AUC for this model.
cv_lasso_full1$lambda.min
# Extract and view coefficients for minimum lambda and 1se lambda.
coef_min_full1 <- coef(cv_lasso_full1, s = "lambda.min")
coef_min_full1
rownames(coef_min_full1)[coef_min_full1[,1] != 0][-1]
coef_1se_full1 <- coef(cv_lasso_full1, s = "lambda.1se")
coef_1se_full1
rownames(coef_1se_full1)[coef_1se_full1[,1] != 0][-1]

#### Question 1: Data Set Creation and Cleaning ####
# Create a new data set for regression called "working_cont".

# Convert logical variables to factors.
working_cont <- working %>%
  mutate_if(is.logical, as.factor)
#'Response variable already present, and same predictor set as 
#' previous part to be used.
# Include only outcome and desired predictors.
working_cont <- working_cont %>%
  select(
    `Total 24hr RBC`, Type, `Gender (male)`, Age, BMI, COPD,
    `alpha1-Antitrypsin Deficiency`, `Cystic Fibrosis`,
    `Idiopathic Pulmonary Hypertension`, `Interstitial Lung Disease`,
    `Redo Lung Transplant`, `Preoperative ECLS`,
    `LAS score`, Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, ECLS_ECMO, ECLS_CPB
  ) %>%
  # Replace spaces in variable names with underscores.
  rename_with(~ gsub(" ", "_", .x))

# Rename potentially problematic column.
working_cont <- working_cont %>%
  rename(alpha1_Antitrypsin_Deficiency = `alpha1-Antitrypsin_Deficiency`)
# Replace non-alphanumeric characters in variable names with underscores.
colnames(working_cont) <- gsub("[^A-Za-z0-9_]", "_", colnames(working_cont))

#### Question 1: Create Full Lasso Regression Model ####

# Create a matrix with predictor features for full data.
xfull2 <- model.matrix(Total_24hr_RBC ~.,working_cont)[,-1]
# Created a vector with response values for full data.
yfull2 <- working_cont$Total_24hr_RBC

# Create lasso regression model, with gaussian family.
lasso_model_full2 <- glmnet(xfull2, yfull2, family="gaussian")
# Plot lambda values against coefficient weights. 
plot(lasso_model_full2,label = T, xvar = "lambda")

# Cross-validate to tune model, nfolds of 5 specified. 
cv_lasso_full2 <- cv.glmnet(xfull2, yfull2, alpha = 1, family = "gaussian", 
                       type.measure = "mse", nfolds = 5)
# Plot lambda values against corresponding AUC values.
plot(cv_lasso_full2)
# View lambda that maximizes the AUC for this model.
cv_lasso_full2$lambda.min

# Extract coefficients for model with minimum lambda.
coef_min_full2 <- coef(cv_lasso_full2, s = "lambda.min")
coef_min_full2
rownames(coef_min_full2)[coef_min_full2[,1] != 0][-1]
# Extract coefficients for model with 1se lambda.
coef_1se_full2 <- coef(cv_lasso_full2, s = "lambda.1se")
coef_1se_full2
rownames(coef_1se_full2)[coef_1se_full2[,1] != 0][-1]

# Calculate full model MSE.
pred_lasso_full <- as.numeric(predict(
  lasso_model_full2, newx = xfull2, s = cv_lasso_full2$lambda.1se, 
  type = "response"))
test_mse <- mean((yfull2 - pred_lasso_full)^2)
test_mse

# Calculate null model MSE.
null_mse <- mean((yfull2 - mean(yfull2))^2)
null_mse

# Full model is better than null model in terms of MSE.

#### Question 2: Data Set Creation and Cleaning ####
# Create new data set.
#' If "Total 24hr RBC" value was greater than 0, indicate it as 1 
#' (for transfusion), if its 0, then indicate it as 0 (no transfusion).
working_outcomes <- working %>%
  mutate(
    RBC_Transfusion = if_else(`Total 24hr RBC` > 0, 1, 0))
# Convert to factor.
working_outcomes$RBC_Transfusion <- as.factor(working_outcomes$RBC_Transfusion)
# Create time variable, for time to death in days.
working_outcomes$time <- as.numeric(working_outcomes$DEATH_DATE - working_outcomes$`OR Date`)
# Assign time as 365 for those patients that have not died by 1 year.
working_outcomes$time <- ifelse(is.na(working_outcomes$time), 365, working_outcomes$time)
# Create status variable, to indicate death or censored.
working_outcomes$status <- ifelse(!is.na(working_outcomes$DEATH_DATE),1,0)

#### Question 2: Create Survival Curve ####
# Create non-stratified survival fit.
sf <- survfit(Surv(time, status==1) ~ 1, data=working_outcomes)
print(sf)
# Plot survival curve with confidence intervals.
plot(sf, xlab = "Days From Operation", ylab = "Survival",
     main = "Survival Probability After Operation", cex.main = 0.8)
plot(sf, xscale = 365.25, xlab = "Years From Operation", ylab="Survival",
     main = "Survival Probability After Operation", cex.main = 0.8) 

# Create stratified survival fit.
sf2 <- survfit(Surv(time, status == 1) ~ RBC_Transfusion, data = working_outcomes)
print(sf2)

# Plot stratified survival curves.
plot(sf2, xlab = "Days from Operation", ylab="Survival", 
     col= 1:2, main = "Survival Probability After Operation", 
     cex.main = 0.8) 
plot(sf2, xlab = "Days from Operation", ylab="Survival", 
     col=1:2, conf.int = 0.95,
     main = "Survival Probability After Operation", cex.main = 0.8) 
legend("bottomright",legend = c("No Transfusion", "Transfusion"),
       lty = 1, col = 1:2, cex = 0.6)
# Plot with scaled x and y-axes.
plot(sf2, xlab = "Days from Operation", ylab="Survival", 
     col= 1:2, xlim = c(0,365), ylim = c(0.8,1),
     main = "Survival Probability After Operation", cex.main = 0.8) 
plot(sf2, xlab = "Days from Operation", ylab="Survival", 
     col=1:2, conf.int = 0.95, xlim = c(0,365), ylim = c(0.8,1),
     main = "Survival Probability After Operation", cex.main = 0.8) 
legend("bottomright",legend = c("No Transfusion", "Transfusion"),
       lty = 1, col = 1:2, cex = 0.6)
# Extract stratified survival probabilities at 1 year.
summary(sf2,time = 365)

# We can also perform a Log-rank test to compare the two survival curves.
# First, we need to check the PH assumption.
# Plot stratified KM curve for analysis:
plot(survfit(Surv(time, status==1) ~ RBC_Transfusion, 
             data=working_outcomes), fun = "S", col = 1:2,
     main = "Survival Probability After Operation", cex.main = 0.8)
legend("bottomright",legend = c("No Transfusion", "Transfusion"),
       lty = 1, col = 1:2, cex = 0.6)
# There is no obvious deviation from the PH assumption before 1 year.
# Also, plot a cloglog plot against log(t).
plot(survfit(Surv(time, status==1) ~ RBC_Transfusion, 
             data = working_outcomes), fun = "cloglog", 
     xlab = "log(Days from Operation)", ylab = "log(Survival)",
     main = "log(Survival Probability) After Operation", cex.main = 0.8,
     col = 1:2)
legend("bottomright",legend = c("No Transfusion", "Transfusion"),
       lty = 1, col = 1:2, cex = 0.6)
#'Curves are parallel except in post-year period. so PH seems to hold, 
#' the log-rank test is valid.

# Perform the log rank test.
survdiff(Surv(time, status==1) ~ RBC_Transfusion, data = working_outcomes)
# We do not see strong evidence that the two curves are not identical.

#### Question 2: Create Cox PH Model (RBC Transfusion) ####
#'Create Cox PH model with RBC_Transfusion as main predictor. Other 
#' predictors are being assessed as confounders.
coxmod1 <- coxph(Surv(time, status==1) ~ RBC_Transfusion + Type + 
                `Gender (male)` + Age + BMI + COPD +
                `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                `Idiopathic Pulmonary Hypertension` + 
                  `Interstitial Lung Disease` + 
                  `Redo Lung Transplant` + `Preoperative ECLS` +
                `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                  Pre_PT + Pre_INR + Pre_PTT + ECLS_ECMO + ECLS_CPB, 
                data = working_outcomes)
# Some predictors perfectly predict survival or are collinear. Remove.
vif(coxmod1)
coxmod1 <- coxph(Surv(time, status==1) ~ RBC_Transfusion + Type + 
                        `Gender (male)` + Age + BMI + COPD +
                        `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                        `Interstitial Lung Disease` + 
                        `Preoperative ECLS` +
                        `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                        Pre_INR + Pre_PTT + ECLS_ECMO, 
                      data = working_outcomes)
# Observe which predictors are significant.
summary(coxmod1)
# RBC Transfusion is not significant.
cox.zph(coxmod1)
# Proportional hazard assumption met, but let's check graphical first.
plot(cox.zph(coxmod1),var=1) # Potentially a problem.
plot(cox.zph(coxmod1),var=2) # Potentially a problem.
plot(cox.zph(coxmod1),var=3) # Clear problem.

#### Question 2: Create Cox PH Model (Total 24hr RBC) ####
# Try the same thing but with "Total 24hr RBC" instead.
#'Create Cox PH model with Total 24hr RBC as main predictor. Other 
#' predictors are being assessed as confounders.
coxmod2 <- coxph(Surv(time, status==1) ~ `Total 24hr RBC` + Type + 
                        `Gender (male)` + Age + BMI + COPD +
                        `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                        `Idiopathic Pulmonary Hypertension` + 
                        `Interstitial Lung Disease` + 
                        `Redo Lung Transplant` + `Preoperative ECLS` +
                        `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                        Pre_PT + Pre_INR + Pre_PTT + ECLS_ECMO + ECLS_CPB, 
                      data = working_outcomes)
# Some predictors perfectly predict survival or are collinear. Remove.
vif(coxmod2)
coxmod2 <- coxph(Surv(time, status==1) ~ `Total 24hr RBC` + Type + 
                        `Gender (male)` + Age + BMI + COPD +
                        `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                        `Interstitial Lung Disease` + 
                        `Preoperative ECLS` +
                        `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                        Pre_INR + Pre_PTT + ECLS_ECMO, 
                      data = working_outcomes)
# Observe which predictors are significant.
summary(coxmod2)
# Total 24hr RBC is not significant.
cox.zph(coxmod2)
# Proportional hazard assumption met, but let's check graphical first.
plot(cox.zph(coxmod2),var=1) # Potentially a problem.
plot(cox.zph(coxmod2),var=2) # Clear problem.
plot(cox.zph(coxmod2),var=3) # Clear problem.

#### Question 2: Create Linear Regression (ICU LOS) ####
# Full linear regression model with RBC Transfusion as predictor.
icu_model1 <- lm(ICU_LOS ~ RBC_Transfusion + Type + 
                        `Gender (male)` + Age + BMI + COPD +
                        `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                        `Idiopathic Pulmonary Hypertension` + 
                        `Interstitial Lung Disease` + 
                        `Redo Lung Transplant` + `Preoperative ECLS` +
                        `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                        Pre_PT + Pre_INR + Pre_PTT + ECLS_ECMO + 
                        ECLS_CPB, data = working_outcomes)
# Assess perfect multicollinearity.
vif(icu_model1)
# Remove perfectly multicollinear variable.
icu_model1 <- lm(ICU_LOS ~ RBC_Transfusion + Type + 
                   `Gender (male)` + Age + BMI + COPD +
                   `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                   `Idiopathic Pulmonary Hypertension` + 
                   `Interstitial Lung Disease` + 
                   `Redo Lung Transplant` + `Preoperative ECLS` +
                   `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                   Pre_INR + Pre_PTT + ECLS_ECMO + 
                   ECLS_CPB, data = working_outcomes)
# Find significant predictors.
summary(icu_model1)
# RBC Transfusion is not significant.

# Examine residuals for heteroscedasticity.
qqnorm(residuals(icu_model1), 
       main = "Q-Q Plot of Residuals (ICU Model 1)")
qqline(residuals(icu_model1), col = "red")
plot(fitted(icu_model1), residuals(icu_model1), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values (ICU Model 1)")
abline(h = 0, col = "red")

# Full linear regression model with Total 24hr RBC as predictor.
icu_model2 <- lm(ICU_LOS ~ `Total 24hr RBC` + Type + 
                        `Gender (male)` + Age + BMI + COPD +
                        `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                        `Idiopathic Pulmonary Hypertension` + 
                        `Interstitial Lung Disease` + 
                        `Redo Lung Transplant` + `Preoperative ECLS` +
                        `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                        Pre_PT + Pre_INR + Pre_PTT + ECLS_ECMO + 
                        ECLS_CPB, data = working_outcomes)
# Assess perfect multicollinearity.
vif(icu_model2)
# Remove perfectly multicollinear variable.
icu_model2 <- lm(ICU_LOS ~ `Total 24hr RBC` + Type + 
                   `Gender (male)` + Age + BMI + COPD +
                   `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                   `Idiopathic Pulmonary Hypertension` + 
                   `Interstitial Lung Disease` + 
                   `Redo Lung Transplant` + `Preoperative ECLS` +
                   `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                   Pre_INR + Pre_PTT + ECLS_ECMO + 
                   ECLS_CPB, data = working_outcomes)
# Find significant predictors.
summary(icu_model2)
# Total 24hr RBC is significant!

# Examine residuals for heteroscedasticity.
qqnorm(residuals(icu_model2), 
       main = "Q-Q Plot of Residuals (ICU Model 2)")
qqline(residuals(icu_model2), col = "red")
plot(fitted(icu_model2), residuals(icu_model2), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values (ICU Model 2)")
abline(h = 0, col = "red")

#### Question 2: Create Linear Regression (Hospital LOS) ####
# Full linear regression model with RBC Transfusion as predictor.
hospital_model1 <- lm(HOSPITAL_LOS ~ RBC_Transfusion + Type + 
                       `Gender (male)` + Age + BMI + COPD +
                       `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                       `Idiopathic Pulmonary Hypertension` + 
                       `Interstitial Lung Disease` + 
                       `Redo Lung Transplant` + `Preoperative ECLS` +
                       `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                       Pre_PT + Pre_INR + Pre_PTT + ECLS_ECMO + 
                       ECLS_CPB, data = working_outcomes)
# Assess perfect multicollinearity.
vif(hospital_model1)
# Remove perfectly multicollinear variable.
hospital_model1 <- lm(HOSPITAL_LOS ~ RBC_Transfusion + Type + 
                        `Gender (male)` + Age + BMI + COPD +
                        `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                        `Idiopathic Pulmonary Hypertension` + 
                        `Interstitial Lung Disease` + 
                        `Redo Lung Transplant` + `Preoperative ECLS` +
                        `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                        Pre_INR + Pre_PTT + ECLS_ECMO + 
                        ECLS_CPB, data = working_outcomes)
# Find significant predictors.
summary(hospital_model1)
# RBC Transfusion is not significant.

# Examine residuals for heteroscedasticity.
qqnorm(residuals(hospital_model1), 
       main = "Q-Q Plot of Residuals (Hospital Model 1)")
qqline(residuals(hospital_model1), col = "red")
plot(fitted(hospital_model1), residuals(hospital_model1), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values (Hospital Model 1)")
abline(h = 0, col = "red")

# Full linear regression model with Total 24hr RBC as predictor.
hospital_model2 <- lm(HOSPITAL_LOS ~ `Total 24hr RBC` + Type + 
                        `Gender (male)` + Age + BMI + COPD +
                        `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                        `Idiopathic Pulmonary Hypertension` + 
                        `Interstitial Lung Disease` + 
                        `Redo Lung Transplant` + `Preoperative ECLS` +
                        `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                        Pre_PT + Pre_INR + Pre_PTT + ECLS_ECMO + 
                        ECLS_CPB, data = working_outcomes)
# Assess perfect multicollinearity.
vif(hospital_model2)
# Remove perfectly multicollinear variable.
hospital_model2 <- lm(HOSPITAL_LOS ~ `Total 24hr RBC` + Type + 
                        `Gender (male)` + Age + BMI + COPD +
                        `alpha1-Antitrypsin Deficiency` + `Cystic Fibrosis` +
                        `Idiopathic Pulmonary Hypertension` + 
                        `Interstitial Lung Disease` + 
                        `Redo Lung Transplant` + `Preoperative ECLS` +
                        `LAS score` + Pre_Hb + Pre_Hct + Pre_Platelets + 
                        Pre_INR + Pre_PTT + ECLS_ECMO + 
                        ECLS_CPB, data = working_outcomes)
# Find significant predictors.
summary(hospital_model2)
# Total 24hr RBC is significant!

# Examine residuals for heteroscedasticity.
qqnorm(residuals(hospital_model2), 
       main = "Q-Q Plot of Residuals (Hospital Model 2)")
qqline(residuals(hospital_model2), col = "red")
plot(fitted(hospital_model2), residuals(hospital_model2), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values (Hospital Model 2)")
abline(h = 0, col = "red")

# Assess rule of thumb for overfitting.
n <- nrow(working_outcomes)
p <- n / 10
p
#' All regression models in Question #2 have less than p degrees of 
#' freedom.