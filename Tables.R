library(flextable)
library(webshot2)
library(htmltools)

#### Massive Missingness ####
data <- data.frame(
  Variable = c("Pre_Fibrinogen", "RBC 0-24hrs", "RBC 24-48hrs", 
               "RBC 48-72hrs", "FFP 0-24hrs", "FFP 24-48hrs", 
               "FFP 48-72hrs", "Plt 0-24hrs", "Plt 24-48hrs", 
               "Plt 48-72hrs", "Cryo 0-24hrs", "Cryo 24-48hrs", 
               "Cryo 48-72hrs"),
  NA_Count = c(187, 132, 140, 144, 154, 159, 159, 154, 156, 158, 155, 159, 159),
  NA_Proportion = c(0.974, 0.688, 0.729, 0.75, 0.802, 0.828, 0.828, 0.802, 
                 0.812, 0.823, 0.807, 0.828, 0.828)
)
# Create a flextable.
ft <- flextable(data) %>%
  add_header_row(values = "Variables with Massive Missingness", 
                 colwidths = ncol(data)) %>%
  set_caption("Regression Summary for Hospital LOS ~ Total 24hr RBC") %>%
  theme_vanilla() %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Save as an image.
save_as_image(ft, path = "massive_missingness_variables.png")

#### Numerical Variables ####
# Note that "STUDY ID #" is removed.
data <- data.frame(
  Variable = c("Age", "BMI", "LAS score", "Pre_PTT", 
               "Intra_Fresh Frozen Plasma", "Intra_Packed Cells", 
               "Intra_Platelets", "Intra_Cryoprecipitate", 
               "Duration of ICU Stay (days)", "ICU_LOS", 
               "HOSPITAL_LOS", "RBC 72hr Total", "FFP 72hr Total", 
               "Plt 72hr Total", "Cryo 72hr Total", "Total 24hr RBC"),
  mean = c(56.26, 24.11, 36.91, 26.07, 
           0.61, 2.27, 0.43, 2.71, 7.29, 9.24, 
           33.60, 0.80, 0.19, 0.14, 0.47, 2.67),
  median = c(61.00, 24.37, 34.00, 24.50, 
             0.00, 1.00, 0.00, 0.00, 4.00, 3.62, 
             22.84, 0.00, 0.00, 0.00, 0.00, 1.00),
  sd = c(14.80, 3.98, 9.15, 7.99, 
         2.54, 4.23, 1.39, 7.38, 11.45, 29.25, 
         42.36, 2.23, 0.98, 0.84, 2.76, 4.98),
  min = c(19.00, 15.54, 24.00, 17.70, 
          0.00, 0.00, 0.00, 0.00, 1.00, 0.63, 
          9.66, 0.00, 0.00, 0.00, 0.00, 0.00),
  max = c(76.00, 31.78, 90.00, 93.50, 
          26.00, 36.00, 11.00, 50.00, 99.00, 380.86, 
          381.52, 23.00, 8.00, 10.00, 30.00, 38.00),
  iqr = c(20.00, 6.80, 5.62, 4.55, 
          0.00, 3.00, 0.00, 0.00, 6.00, 5.66, 
          20.38, 1.00, 0.00, 0.00, 0.00, 4.00)
)
# Create a flextable.
ft <- flextable(data) %>%
  add_header_row(values = "Numerical Variables", 
                 colwidths = ncol(data)) %>%
  set_header_labels(
    Variable = "Variable",
    mean = "Mean",
    median = "Median",
    sd = "Standard Deviation",
    min = "Minimum",
    max = "Maximum",
    iqr = "Interquartile Range"
  ) %>%
  theme_vanilla() %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Save as an image.
save_as_image(ft, path = "numerical_variables.png")

#### Factor Variables ####
data <- data.frame(
  Variable = c("ALIVE_12MTHS_YN", "ALIVE_12MTHS_YN", 
               "ALIVE_30DAYS_YN", "ALIVE_30DAYS_YN", 
               "ALIVE_90DAYS_YN", "ALIVE_90DAYS_YN", 
               "Massive Transfusion", "Massive Transfusion", 
               "Type", "Type", "Type"),
  Value = c("Y", "N", 
            "Y", "N", 
            "Y", "N", 
            "0", "1", 
            "Bilateral", "Single Left Lung", "Single Right Lung"),
  Count = c(169, 23, 
            189, 3, 
            183, 9, 
            183, 9, 
            157, 18, 17),
  Proportion = c(0.88, 0.12, 
                 0.98, 0.02, 
                 0.95, 0.05, 
                 0.95, 0.05, 
                 0.82, 0.09, 0.09)
)
# Create a flextable
ft <- flextable(data) %>%
  add_header_row(values = "Factor Variables", 
                 colwidths = ncol(data)) %>%
  set_header_labels(
    Variable = "Variable",
    Value = "Value",
    Count = "Count",
    Proportion = "Proportion"
  ) %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, path = "factor_variables.png")

#### Logical Variables ####
data <- data.frame(
  Variable = c(
    "COPD", "COPD", 
    "Cystic Fibrosis", "Cystic Fibrosis", 
    "ECLS_CPB", "ECLS_CPB", 
    "ECLS_ECMO", "ECLS_ECMO", 
    "Gender (male)", "Gender (male)", 
    "Idiopathic Pulmonary Hypertension", "Idiopathic Pulmonary Hypertension", 
    "Interstitial Lung Disease", "Interstitial Lung Disease", 
    "Preoperative ECLS", "Preoperative ECLS", 
    "Redo Lung Transplant", "Redo Lung Transplant", 
    "alpha1-Antitrypsin Deficiency", "alpha1-Antitrypsin Deficiency"
  ),
  Value = c(
    "FALSE", "TRUE", 
    "FALSE", "TRUE", 
    "FALSE", "TRUE", 
    "TRUE", "FALSE", 
    "TRUE", "FALSE", 
    "FALSE", "TRUE", 
    "FALSE", "TRUE", 
    "FALSE", "TRUE", 
    "FALSE", "TRUE", 
    "FALSE", "TRUE"
  ),
  Count = c(
    134, 58, 
    162, 30, 
    190, 2, 
    101, 91, 
    104, 88, 
    186, 6, 
    100, 92, 
    185, 7, 
    182, 10, 
    183, 9
  ),
  Proportion = c(
    0.70, 0.30, 
    0.84, 0.16, 
    0.99, 0.01, 
    0.53, 0.47, 
    0.54, 0.46, 
    0.97, 0.03, 
    0.52, 0.48, 
    0.96, 0.04, 
    0.95, 0.05, 
    0.95, 0.05
  )
)
# Create a flextable
ft <- flextable(data) %>%
  add_header_row(values = "Logical Variables", 
                 colwidths = ncol(data)) %>%
  set_header_labels(
    Variable = "Variable",
    Value = "Value",
    Count = "Count",
    Proportion = "Proportion"
  ) %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, path = "logical_variables.png")

#### Missingness for Date Variables ####
date_table <- data.frame(
  Variable = c("DEATH_DATE", "DEATH_DATE", "OR Date"),
  Value = c("NA", "Not NA", "Not NA"),
  Count = c(160, 32, 192),
  Proportion = c(0.83, 0.17, 1.00)
)
# Create a flextable
ft <- flextable(date_table) %>%
  add_header_row(values = "Missingness for Date Variables", 
                 colwidths = ncol(date_table)) %>%
  set_header_labels(
    Variable = "Variable",
    Value = "Value",
    Count = "Count",
    Proportion = "Proportion"
  ) %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, path = "date_variables.png")

#### Missingness for Imputation Variable Selection ####
na_summary <- data.frame(
  Variable = c("LAS score", "Pre_PTT", "Duration of ICU Stay (days)"),
  Number_of_NAs = c(12, 1, 1),
  Proportion_of_NAs = c(0.0625, 0.0052083, 0.0052083)
)
# Create a flextable object
ft <- flextable(na_summary) %>%
  add_header_row(values = "Missingness of Imputed Variables", 
                 colwidths = ncol(na_summary)) %>%
  set_caption("Missingness of Imputed Variables") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "imputation_variables.png")


#### Question 1: Characteristics Tables ####
# Numerical variables, note that "STUDY ID #" is removed.
data <- data.frame(
  Variable = c("Age", "BMI", "LAS score", "Pre_PTT",
               "Intra_Fresh Frozen Plasma", "Intra_Packed Cells", "Intra_Platelets",
               "Intra_Cryoprecipitate", "Duration of ICU Stay (days)", "ICU_LOS",
               "HOSPITAL_LOS", "RBC 72hr Total", "FFP 72hr Total", "Plt 72hr Total",
               "Cryo 72hr Total", "Total 24hr RBC"),
  mean = c(53.73, 23.50, 38.39, 26.97, 1.04, 3.82, 0.70, 4.39, 8.22, 11.47, 37.50, 
           1.21, 0.32, 0.19, 0.70, 4.50),
  median = c(58.00, 23.85, 35.00, 24.80, 0.00, 3.00, 0.00, 0.00, 4.00, 4.56, 
             25.54, 0.00, 0.00, 0.00, 0.00, 3.00),
  sd = c(14.90, 4.16, 9.84, 9.56, 3.23, 4.93, 1.75, 9.12, 11.35, 36.68, 45.74, 
         2.70, 1.26, 1.02, 3.45, 5.79),
  min = c(21.00, 15.54, 24.00, 17.70, 0.00, 0.00, 0.00, 0.00, 1.00, 0.63, 11.08, 
          0.00, 0.00, 0.00, 0.00, 1.00),
  max = c(76.00, 31.78, 90.00, 93.50, 26.00, 36.00, 11.00, 50.00, 99.00, 380.86, 
          381.52, 23.00, 8.00, 10.00, 30.00, 38.00),
  iqr = c(24.00, 7.51, 8.00, 5.25, 0.00, 3.00, 0.00, 10.00, 6.00, 6.86, 19.77, 
          1.00, 0.00, 0.00, 0.00, 3.00)
)
# Create a flextable.
ft <- flextable(data) %>%
  add_header_row(values = "Numerical Variables (Transfusion)", 
                 colwidths = ncol(data)) %>%
  set_header_labels(
    Variable = "Variable",
    mean = "Mean",
    median = "Median",
    sd = "Standard Deviation",
    min = "Minimum",
    max = "Maximum",
    iqr = "Interquartile Range"
  ) %>%
  theme_vanilla() %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Save as an image.
save_as_image(ft, path = "numerical_variables_char.png")

# Factor variables.
data <- data.frame(
  Variable = c(
    "ALIVE_12MTHS_YN", "ALIVE_12MTHS_YN",
    "ALIVE_30DAYS_YN", "ALIVE_30DAYS_YN",
    "ALIVE_90DAYS_YN", "ALIVE_90DAYS_YN",
    "Massive Transfusion", "Massive Transfusion",
    "Type", "Type", "Type"
  ),
  Value = c(
    "Y", "N",
    "Y", "N",
    "Y", "N",
    "0", "1",
    "Bilateral", "Single Right Lung", "Single Left Lung"
  ),
  Count = c(
    99, 15,
    111, 3,
    107, 7,
    105, 9,
    105, 5, 4
  ),
  Proportion = c(
    0.87, 0.13,
    0.97, 0.03,
    0.94, 0.06,
    0.92, 0.08,
    0.92, 0.04, 0.04
  )
)
# Create a flextable
ft <- flextable(data) %>%
  add_header_row(values = "Factor Variables (Transfusion)", 
                 colwidths = ncol(data)) %>%
  set_header_labels(
    Variable = "Variable",
    Value = "Value",
    Count = "Count",
    Proportion = "Proportion"
  ) %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, path = "factor_variables_char.png")

# Logical variables.
data <- data.frame(
  Variable = c(
    "COPD", "COPD",
    "Cystic Fibrosis", "Cystic Fibrosis",
    "ECLS_CPB", "ECLS_CPB",
    "ECLS_ECMO", "ECLS_ECMO",
    "Gender (male)", "Gender (male)",
    "Idiopathic Pulmonary Hypertension", "Idiopathic Pulmonary Hypertension",
    "Interstitial Lung Disease", "Interstitial Lung Disease",
    "Preoperative ECLS", "Preoperative ECLS",
    "Redo Lung Transplant", "Redo Lung Transplant",
    "alpha1-Antitrypsin Deficiency", "alpha1-Antitrypsin Deficiency"
  ),
  Value = c(
    "FALSE", "TRUE",
    "FALSE", "TRUE",
    "FALSE", "TRUE",
    "TRUE", "FALSE",
    "FALSE", "TRUE",
    "FALSE", "TRUE",
    "FALSE", "TRUE",
    "FALSE", "TRUE",
    "FALSE", "TRUE",
    "FALSE", "TRUE"
  ),
  Count = c(
    91, 23,
    89, 25,
    112, 2,
    77, 37,
    64, 50,
    110, 4,
    57, 57,
    107, 7,
    104, 10,
    109, 5
  ),
  Proportion = c(
    0.80, 0.20,
    0.78, 0.22,
    0.98, 0.02,
    0.68, 0.32,
    0.56, 0.44,
    0.96, 0.04,
    0.50, 0.50,
    0.94, 0.06,
    0.91, 0.09,
    0.96, 0.04
  )
)
# Create a flextable
ft <- flextable(data) %>%
  add_header_row(values = "Logical Variables (Transfusion)", 
                 colwidths = ncol(data)) %>%
  set_header_labels(
    Variable = "Variable",
    Value = "Value",
    Count = "Count",
    Proportion = "Proportion"
  ) %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, path = "logical_variables_char.png")

#### Question 1: AUC Table ####
data <- data.frame(
  Iteration = c(1, 2, 3, 4, 5),
  AUC_LASSO = c(0.7600733, 0.7183333, 0.8820971, 0.7393813, 0.7236070),
  AUC_Pruned_Tree = c(0.7138278, 0.6797619, 0.8237179, 0.6622807, 0.6219453)
)
# Create a flextable object
ft <- flextable(data) %>%
  add_header_row(values = "Model Comparison via AUC", 
                 colwidths = ncol(data)) %>%
  set_caption("Model Comparison via AUC") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "AUC_table.png")

#### Question 1: Coefficients for Full Lasso Classification Model####
data <- data.frame(
  Variable = c("(Intercept)", "(Intercept)", 
               "TypeSingle Left Lung", "TypeSingle Right Lung", 
               "Gender__male_TRUE", "Age", "BMI", 
               "COPDTRUE", "alpha1_Antitrypsin_DeficiencyTRUE", 
               "Cystic_FibrosisTRUE", "Idiopathic_Pulmonary_HypertensionTRUE", 
               "Interstitial_Lung_DiseaseTRUE", "Redo_Lung_TransplantTRUE", 
               "Preoperative_ECLSTRUE", "LAS_score", "Pre_Hb", 
               "Pre_Hct", "Pre_Platelets", "Pre_PT", "Pre_INR", 
               "Pre_PTT", "ECLS_ECMOTRUE", "ECLS_CPBTRUE"),
  Coefficient = c(3.10014438, NA, 
                  NA, NA, 
                  NA, NA, NA, 
                  NA, NA, 
                  NA, NA, 
                  NA, NA, 
                  NA, NA, -0.02307781, 
                  NA, NA, NA, NA, 
                  NA, 0.47488129, NA)
)
ft <- flextable(data) %>%
  add_header_row(values = "Lasso Classification Model Weights", 
                 colwidths = ncol(data)) %>%
  set_caption("Lasso Classification Model Weights") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "lasso_classification_model_weights.png")

#### Question 1: Coefficients for Full Lasso Regression Model####
data <- data.frame(
  Variable = c(
    "(Intercept)", "TypeSingle Left Lung", "TypeSingle Right Lung", 
    "Gender__male_TRUE", "Age", "BMI", 
    "COPDTRUE", "alpha1_Antitrypsin_DeficiencyTRUE", 
    "Cystic_FibrosisTRUE", "Idiopathic_Pulmonary_HypertensionTRUE", 
    "Interstitial_Lung_DiseaseTRUE", "Redo_Lung_TransplantTRUE", 
    "Preoperative_ECLSTRUE", "LAS_score", 
    "Pre_Hb", "Pre_Hct", "Pre_Platelets", 
    "Pre_PT", "Pre_INR", "Pre_PTT", 
    "ECLS_ECMOTRUE", "ECLS_CPBTRUE"
  ),
  Coefficient = c(
    7.69718445, NA, NA, 
    NA, NA, NA, 
    NA, NA, 
    NA, NA, 
    NA, 4.96511536, 
    2.39094343, NA, 
    NA, -13.76478524, NA, 
    NA, NA, NA, 
    0.09694887, 0.50889569
  )
)
ft <- flextable(data) %>%
  add_header_row(values = "Lasso Regression Model Weights", 
                 colwidths = ncol(data)) %>%
  set_caption("Lasso Regression Model Weights") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "lasso_regression_model_weights.png")

#### Question 1: MSEs for Lasso Regression Models ####
data <- data.frame(
  Model_Type = c("Null", "Lasso Regression"),
  MSE = c(24.76188, 16.86478)
)
ft <- flextable(data) %>%
  add_header_row(values = "MSEs", 
                 colwidths = ncol(data)) %>%
  set_caption("Lasso Regression Model Weights") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "lasso_regression_and_null_models_MSE.png")

#### Question 2: KM Survival Difference by RBC Transfusion Status ####
data <- data.frame(
  `RBC Transfusion` = c("No (0)", "Yes (1)"),
  N = c(78, 113),
  `Median Time to Death` = c(484,403),
  Observed = c(13, 18),
  Expected = c(14.3, 16.7),
  Pearson = c(0.119, 0.102),
  Wald = c(0.24, 0.24)
)
# Create flextable. 
ft <- flextable(data) %>%
  add_header_row(values = "Survival Difference by RBC Transfusion Status", 
                 colwidths = ncol(data)) %>%
  set_caption("Survival Difference by RBC Transfusion Status") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "survival_difference_table.png")

#### Question 2: Cox Model Summary for RBC Transfusion ####
data <- data.frame(
  Variable = c("RBC_Transfusion1", "TypeSingle Left Lung", "TypeSingle Right Lung",
               "Gender (male)TRUE", "Age", "BMI", "COPDTRUE", 
               "alpha1-Antitrypsin DeficiencyTRUE", "Cystic FibrosisTRUE", 
               "Interstitial Lung DiseaseTRUE", "Preoperative ECLSTRUE", 
               "LAS score", "Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_INR", 
               "Pre_PTT", "ECLS_ECMOTRUE"),
  Coef = c(4.375e-01, -9.751e-01, -1.170e-01, -1.994e-01, 
           8.795e-03, 8.007e-02, -4.925e-01, 7.953e-01, 
           7.052e-02, 4.031e-01, 2.602e+00, -1.119e-02, 
           4.846e-02, -1.268e+01, 2.678e-03, 1.363e+00, 
           -1.307e-01, 1.262e-02),
  Exp_Coef = c(1.549e+00, 3.772e-01, 8.896e-01, 8.192e-01, 
               1.009e+00, 1.083e+00, 6.111e-01, 2.215e+00, 
               1.073e+00, 1.496e+00, 1.349e+01, 9.889e-01, 
               1.050e+00, 3.112e-06, 1.003e+00, 3.908e+00, 
               8.775e-01, 1.013e+00),
  SE_Coef = c(5.397e-01, 8.804e-01, 7.380e-01, 4.562e-01, 
              2.163e-02, 6.061e-02, 6.739e-01, 8.778e-01, 
              1.025e+00, 6.709e-01, 1.454e+00, 2.581e-02, 
              4.088e-02, 1.421e+01, 2.124e-03, 5.590e-01, 
              7.096e-02, 4.699e-01),
  Z = c(0.811, -1.108, -0.159, -0.437, 
        0.407, 1.321, -0.731, 0.906, 
        0.069, 0.601, 1.790, -0.433, 
        1.185, -0.892, 1.261, 2.438, 
        -1.842, 0.027),
  P_Value = c(0.4176, 0.2680, 0.8740, 0.6620, 
              0.6842, 0.1864, 0.4649, 0.3649, 
              0.9451, 0.5480, 0.0734, 0.6648, 
              0.2359, 0.3723, 0.2073, 0.0148, 
              0.0654, 0.9786)
)
# Create flextable. 
ft <- flextable(data) %>%
  add_header_row(values = "Cox Model Summary for RBC Transfusion", 
                 colwidths = ncol(data)) %>%
  set_caption("Cox Model Summary for RBC Transfusion") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "cox_summary_rbc_transfusion.png")
#### Question 2: Cox Model Summary for Total 24hr RBC ####
data <- data.frame(
  Variable = c("Total 24hr RBC", "TypeSingle Left Lung", "TypeSingle Right Lung",
               "Gender (male)TRUE", "Age", "BMI", "COPDTRUE", 
               "alpha1-Antitrypsin DeficiencyTRUE", "Cystic FibrosisTRUE", 
               "Interstitial Lung DiseaseTRUE", "Preoperative ECLSTRUE", 
               "LAS score", "Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_INR", 
               "Pre_PTT", "ECLS_ECMOTRUE"),
  Coef = c(-5.294e-02, -1.153e+00, -2.454e-01, -2.991e-01, 
           9.195e-03, 6.868e-02, -5.114e-01, 8.589e-01, 
           2.010e-01, 4.187e-01, 2.953e+00, -8.468e-03, 
           4.510e-02, -1.355e+01, 2.885e-03, 1.385e+00, 
           -1.335e-01, 1.684e-01),
  Exp_Coef = c(9.484e-01, 3.158e-01, 7.824e-01, 7.415e-01, 
               1.009e+00, 1.071e+00, 5.996e-01, 2.361e+00, 
               1.223e+00, 1.520e+00, 1.916e+01, 9.916e-01, 
               1.046e+00, 1.303e-06, 1.003e+00, 3.995e+00, 
               8.751e-01, 1.183e+00),
  SE_Coef = c(8.135e-02, 8.470e-01, 7.044e-01, 4.538e-01, 
              2.141e-02, 6.056e-02, 7.292e-01, 8.826e-01, 
              1.061e+00, 7.415e-01, 1.551e+00, 2.452e-02, 
              4.199e-02, 1.474e+01, 2.124e-03, 5.671e-01, 
              7.261e-02, 4.773e-01),
  Z = c(-0.651, -1.361, -0.348, -0.659, 
        0.430, 1.134, -0.701, 0.973, 
        0.189, 0.565, 1.904, -0.345, 
        1.074, -0.919, 1.358, 2.442, 
        -1.838, 0.353),
  P_Value = c(0.5152, 0.1736, 0.7275, 0.5098, 
              0.6675, 0.2568, 0.4831, 0.3305, 
              0.8497, 0.5723, 0.0569, 0.7298, 
              0.2828, 0.3580, 0.1745, 0.0146, 
              0.0661, 0.7242)
)
# Create flextable. 
ft <- flextable(data) %>%
  add_header_row(values = "Cox Model Summary for Total 24hr RBC", 
                 colwidths = ncol(data)) %>%
  set_caption("Cox Model Summary for Total 24hr RBC") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "cox_summary_total_24hr_RBC.png")

#### Question 2: Regression Summary for ICU LOS ~ RBC Transfusion ####
data <- data.frame(
  Variable = c("(Intercept)", "RBC_Transfusion1", "TypeSingle Left Lung", "TypeSingle Right Lung",
               "Gender (male)TRUE", "Age", "BMI", "COPDTRUE", 
               "alpha1-Antitrypsin DeficiencyTRUE", "Cystic FibrosisTRUE", 
               "Idiopathic Pulmonary HypertensionTRUE", "Interstitial Lung DiseaseTRUE", 
               "Redo Lung TransplantTRUE", "Preoperative ECLSTRUE", "LAS score", 
               "Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_INR", "Pre_PTT", 
               "ECLS_ECMOTRUE", "ECLS_CPBTRUE"),
  Estimate = c(2.387e+01, -1.018e+00, -1.500e+00, -2.713e+00,
               -2.277e+00, -5.522e-02, 4.921e-01, -3.206e+00, 
               -7.528e-01, -2.859e+00, -5.066e+00, -1.470e+00, 
               -9.769e-01, 1.642e+01, -3.025e-01, 2.554e-01, 
               -1.160e+02, -4.618e-03, -5.061e-01, 9.413e-02, 
               4.042e+00, 4.280e+00),
  Std_Error = c(1.195e+01, 2.131e+00, 3.341e+00, 3.105e+00,
                1.759e+00, 8.797e-02, 2.369e-01, 2.832e+00, 
                4.073e+00, 3.700e+00, 5.228e+00, 2.884e+00, 
                3.906e+00, 5.646e+00, 1.039e-01, 1.512e-01, 
                5.451e+01, 8.968e-03, 1.960e+00, 1.245e-01, 
                2.003e+00, 8.252e+00),
  T_Value = c(1.999, -0.477, -0.449, -0.874, 
              -1.294, -0.628, 2.077, -1.132, 
              -0.185, -0.773, -0.969, -0.510, 
              -0.250, 2.907, -2.913, 1.689, 
              -2.127, -0.515, -0.258, 0.756, 
              2.018, 0.519),
  P_Value = c(0.04725, 0.63363, 0.65404, 0.38353, 
              0.19734, 0.53102, 0.03929, 0.25918, 
              0.85358, 0.44084, 0.33393, 0.61092, 
              0.80283, 0.00414, 0.00407, 0.09299, 
              0.03485, 0.60726, 0.79652, 0.45057, 
              0.04513, 0.60464)
)
# Create flextable. 
ft <- flextable(data) %>%
  add_header_row(values = "Regression Summary for ICU LOS ~ RBC Transfusion", 
                 colwidths = ncol(data)) %>%
  set_caption("Regression Summary for ICU LOS ~ RBC Transfusion") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "regression_summary_ICU_RBC_Transfusion.png")
#### Question 2: Regression Summary for ICU LOS ~ Total 24hr RBC ####
data <- data.frame(
  Variable = c("(Intercept)", "Total 24hr RBC", "TypeSingle Left Lung", "TypeSingle Right Lung",
               "Gender (male)TRUE", "Age", "BMI", "COPDTRUE", 
               "alpha1-Antitrypsin DeficiencyTRUE", "Cystic FibrosisTRUE", 
               "Idiopathic Pulmonary HypertensionTRUE", "Interstitial Lung DiseaseTRUE", 
               "Redo Lung TransplantTRUE", "Preoperative ECLSTRUE", "LAS score", 
               "Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_INR", "Pre_PTT", 
               "ECLS_ECMOTRUE", "ECLS_CPBTRUE"),
  Estimate = c(13.448313, 0.878554, -0.419602, -1.903445,
               -0.985899, -0.060021, 0.491801, -2.992137, 
               -0.451075, -3.463490, -4.392878, -1.895812, 
               -9.192264, 10.760533, -0.275189, 0.241042, 
               -90.813473, -0.003595, 0.175755, 0.043040, 
               2.271849, -7.714199),
  Std_Error = c(11.325507, 0.224650, 3.090403, 2.909013,
                1.711304, 0.083098, 0.224342, 2.694266, 
                3.899991, 3.548144, 4.992714, 2.765429, 
                4.240659, 5.603175, 0.099695, 0.144350, 
                52.584882, 0.008478, 1.885148, 0.120005, 
                1.873883, 8.314856),
  T_Value = c(1.187, 3.911, -0.136, -0.654, 
              -0.576, -0.722, 2.192, -1.111, 
              -0.116, -0.976, -0.880, -0.686, 
              -2.168, 1.920, -2.760, 1.670, 
              -1.727, -0.424, 0.093, 0.359, 
              1.212, -0.928),
  P_Value = c(0.236722, 0.000133, 0.892160, 0.513791, 
              0.565307, 0.471114, 0.029733, 0.268336, 
              0.908059, 0.330390, 0.380186, 0.493943, 
              0.031584, 0.056487, 0.006412, 0.096801, 
              0.085997, 0.672050, 0.925830, 0.720303, 
              0.227061, 0.354855)
)
# Create flextable. 
ft <- flextable(data) %>%
  add_header_row(values = "Regression Summary for ICU LOS ~ Total 24hr RBC", 
                 colwidths = ncol(data)) %>%
  set_caption("Regression Summary for ICU LOS ~ Total 24hr RBC") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "regression_summary_ICU_total_24hr_RBC.png")

#### Question 2: Regression Summary for Hospital LOS ~ RBC Transfusion ####
data <- data.frame(
  Variable = c("(Intercept)", "RBC_Transfusion1", "TypeSingle Left Lung", "TypeSingle Right Lung",
               "Gender (male)TRUE", "Age", "BMI", "COPDTRUE", 
               "alpha1-Antitrypsin DeficiencyTRUE", "Cystic FibrosisTRUE", 
               "Idiopathic Pulmonary HypertensionTRUE", "Interstitial Lung DiseaseTRUE", 
               "Redo Lung TransplantTRUE", "Preoperative ECLSTRUE", "LAS score", 
               "Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_INR", "Pre_PTT", 
               "ECLS_ECMOTRUE", "ECLS_CPBTRUE"),
  Estimate = c(4.263e+01, -5.358e+00, -1.063e+01, -1.588e+01,
               2.802e-01, 8.768e-02, 1.031e+00, -1.841e+01, 
               -6.237e+00, -1.122e+01, -2.388e+01, -8.064e+00, 
               5.500e+00, -1.012e+01, -6.994e-01, 4.404e-01, 
               -2.480e+02, -9.979e-03, -1.491e+01, 2.254e+00, 
               8.733e+00, 1.399e+01),
  Std_Error = c(3.358e+01, 5.992e+00, 9.392e+00, 8.730e+00,
                4.946e+00, 2.473e-01, 6.660e-01, 7.962e+00, 
                1.145e+01, 1.040e+01, 1.470e+01, 8.107e+00, 
                1.098e+01, 1.587e+01, 2.920e-01, 4.250e-01, 
                1.532e+02, 2.521e-02, 5.510e+00, 3.500e-01, 
                5.630e+00, 2.320e+01),
  T_Value = c(1.270, -0.894, -1.132, -1.818, 
              0.057, 0.354, 1.548, -2.312, 
              -0.545, -1.079, -1.625, -0.995, 
              0.501, -0.638, -2.395, 1.036, 
              -1.618, -0.396, -2.707, 6.440, 
              1.551, 0.603),
  P_Value = c(0.20599, 0.37246, 0.25945, 0.07076, 
              0.95489, 0.72341, 0.12342, 0.02199, 
              0.58670, 0.28229, 0.10612, 0.32129, 
              0.61717, 0.52454, 0.01769, 0.30157, 
              0.10745, 0.69275, 0.00749, 1.19e-09, 
              0.12278, 0.54735)
)
# Create flextable. 
ft <- flextable(data) %>%
  add_header_row(values = "Regression Summary for Hospital LOS ~ RBC Transfusion", 
                 colwidths = ncol(data)) %>%
  set_caption("Regression Summary for Hospital LOS ~ RBC Transfusion") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "regression_summary_hospital_RBC_transfusion.png")

#### Question 2: Regression Summary for Hospital LOS ~ Total 24hr RBC ####
data <- data.frame(
  Variable = c("(Intercept)", "Total 24hr RBC", "TypeSingle Left Lung", "TypeSingle Right Lung",
               "Gender (male)TRUE", "Age", "BMI", "COPDTRUE", 
               "alpha1-Antitrypsin DeficiencyTRUE", "Cystic FibrosisTRUE", 
               "Idiopathic Pulmonary HypertensionTRUE", "Interstitial Lung DiseaseTRUE", 
               "Redo Lung TransplantTRUE", "Preoperative ECLSTRUE", "LAS score", 
               "Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_INR", "Pre_PTT", 
               "ECLS_ECMOTRUE", "ECLS_CPBTRUE"),
  Estimate = c(17.80589, 1.69430, -7.12737, -13.25286,
               2.96360, 0.05481, 1.08875, -17.45410, 
               -5.95535, -12.47051, -23.36834, -8.89703, 
               -11.09361, -20.59746, -0.65479, 0.43504, 
               -197.11125, -0.01046, -13.74182, 2.15697, 
               4.38230, -11.36113),
  Std_Error = c(32.65163, 0.64767, 8.90969, 8.38673,
                4.93372, 0.23957, 0.64678, 7.76761, 
                11.24374, 10.22936, 14.39408, 7.97278, 
                12.22589, 16.15405, 0.28742, 0.41616, 
                151.60312, 0.02444, 5.43491, 0.34598, 
                5.40244, 23.97187),
  T_Value = c(0.545, 2.616, -0.800, -1.580, 
              0.601, 0.229, 1.683, -2.247, 
              -0.530, -1.219, -1.623, -1.116, 
              -0.907, -1.275, -2.278, 1.045, 
              -1.300, -0.428, -2.528, 6.234, 
              0.811, -0.474),
  P_Value = c(0.5862, 0.0097, 0.4249, 0.1159, 
              0.5489, 0.8193, 0.0942, 0.0259, 
              0.5970, 0.2245, 0.1064, 0.2660, 
              0.3655, 0.2040, 0.0240, 0.2974, 
              0.1953, 0.6692, 0.0124, 3.5e-09, 
              0.4184, 0.6362)
)
# Create flextable. 
ft <- flextable(data) %>%
  add_header_row(values = "Regression Summary for Hospital LOS ~ Total 24hr RBC", 
                 colwidths = ncol(data)) %>%
  set_caption("Regression Summary for Hospital LOS ~ Total 24hr RBC") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "regression_summary_hospital_total_24hr_RBC.png")