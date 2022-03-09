## Inpatient Admission Prediction with no Discrimination
### Capstone Project for Master's Degree in Biostatistics at Univeristy of Washington
### Collaboration between 3 students from our cohort and Community Health Plan of Washington (CHPW)

The primary goal of this project is to develop a prediction model for inpatient admission using available data on insurance claims, demographics, and medical diagnosis using statistical and machine leanring methods.

The secondary goal of this project is to apply the technique of Classification with No Discrimination (CND) to reduce the racial bias in the prediction results

## Overview of the project

-- 1. Data extraction from CHPW's database using SQL
-- 2. Data cleaning and transformation in R using Tidyverse
-- 3. EDA: outcome class distribution; descriptive statistics for both outcome groups; characteristics for each feature; racial bias in data
-- 4. Preprocessing: grouping categories; one-hot encoding; scaling;
-- 5. Modeling: penalized logistic regression (glmnet); gradient boosting trees (xgboost); bayesian additive regression trees (BART)
-- 6. Bias correction: apply CND after first training stage; then use the CND-massaged data for second stage training
-- 7. Evaluation: evaluation on testing data: performance evaluated using threshold-based metrics (f1 score) and prAUC; racial bias evaluated by predicted incidence rate
-- 8. Discussion: feature importance investigation; direction of association for the most important features

## Results

Best model yields f1 score of 0.34, which is not ideal, but it is greatly improved upon CHPW's previous work.
CND technique successfully corrected bias in prediction, with only minimal decrease in prediction power.

## Challenges

Class imbalance is a big challenge: the outcome distribuion is 95.7% no admission vs 4.3% admission, so the imbalance is fairly extreme.
We attempted different strategy: 
-- 1. weight-based modeling: setting parameters like 'scale_pos_weight' in xgboost
-- 2. umdersampling with different class ratio for training
-- 3. advanced oversampling techniques such as SMOTE and ROSE

These strategies either did not provide meaningful improvement on predicion performance, or was not carried out succesfully due to limited computation power (we have a fairly large dataset of around 180,000 rows and 85 columns after data cleaning, and we are running our models on CHPW's remote desktop with 32 GB memory).

## Impact and Future Direction
