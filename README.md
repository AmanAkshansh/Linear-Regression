# Linear-Regression
Predicting the credit card spend of customers using Linear regression

# Dataset
The sample dataset(Linear regression case) contains data of 5000 customers with 132 variables. The excel file also contains the Data dictionary.

# Business Problem:
The objective of this Project is to understand what's driving the total credit card spend(Primary card spend + Secondary card spend) of the customers, Priotize the drivers based on the importance and Predict the credit card spend.

# Techniques used:
1. Explanatory Data analysis
2. Data cleaning/preparation with missing value treatment and outliers treatment.
3. variable reduction techniques:
   (i) Performed ANOVA test to select the significant categorical variables.
   (ii) Performed Stepwise Linear regression on the entire dataset with previously selected categorical variables along with all the               continuous variables to further reduce the variables for final regression model.
4. Splitted the dataset into 'Development' and 'validation' datsets.
5. Built the final model with all the significant variables.
6. Applied Cook's D method to remove the influential observations.
7. Predicted the total spend on Dev and Val datasets.
8. Performed Decile analysis on both datasets.

The Excel file(Case study 2 - Linear regression) contains the descriptive stats of all the variables, Regression result,Decile analysis with graphs between predicted and actual spend for both development and validation datasets.

