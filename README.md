## Inpatient Admission Prediction with no Discrimination
### Capstone Project for Master's Degree in Biostatistics at Univeristy of Washington
### Collaboration between 3 students from our cohort and Community Health Plan of Washington (CHPW)

The primary goal of this project is to develop a prediction model for inpatient admission using available data on insurance claims, demographics, and medical diagnosis using statistical and machine leanring methods.

The secondary goal of this project is to apply the technique of Classification with No Discrimination (CND) to reduce the racial bias in the prediction results

## Overview of the project

- Data extraction from CHPW's database using SQL
- Data cleaning and transformation in R using Tidyverse
- EDA: outcome class distribution; descriptive statistics for both outcome groups; characteristics for each feature; racial bias in data
- Preprocessing: grouping categories; one-hot encoding; scaling;
- Modeling: penalized logistic regression (glmnet); gradient boosting trees (xgboost); bayesian additive regression trees (BART)
- Bias correction: apply CND after first training stage; then use the CND-massaged data for second stage training
- Evaluation: evaluation on testing data: performance evaluated using threshold-based metrics (f1 score) and prAUC; racial bias evaluated by predicted incidence rate
- Discussion: feature importance investigation; direction of association for the most important features

## Results

Best model yields f1 score of 0.34, which is not ideal, but it is greatly improved upon CHPW's previous work.
CND technique successfully corrected bias in prediction, with only minimal decrease in prediction power.

## Challenges

Class imbalance is a big challenge: the outcome distribuion is 95.7% no admission vs 4.3% admission, so the imbalance is fairly extreme.
We attempted different strategy: 
- Weight-based modeling: setting parameters like 'scale_pos_weight' in xgboost
- Umdersampling and oversampling with different class ratio for training
- Advanced oversampling techniques such as SMOTE and ROSE

These strategies either did not provide meaningful improvement on predicion performance, or was not carried out succesfully due to limited computation power (we have a fairly large dataset of around 180,000 rows and 85 columns after data cleaning, and we are running our models on CHPW's remote desktop with 32 GB memory).

## Codes
- BART.rmd has the end-to-end codes for the complete project, with BART modeling implementation
- Util.R has all the utility functions needed in BART.rmd


