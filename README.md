# Bank-data

The dataset is used to predict bank term deposit subscriptions.

From primary analysis, we can see that the majority don't have the subscription (88.3%). On average, a person in this dataset is married, around 40 years old, with a yearly balance of €1362 and no credit or loan. Since the "campaign" and "previous" variables contain almost the same information, only the "previous" variable was used. From the boxplot of the "previous" variable, one residual value was observed, so after a quick check, it was removed. Also, age categories were added: "0-30", "31-40", "41-50", "51-60", "60+".

Since there are no strong correlations between variables, the dataset was divided into training and testing sets. Firstly, a logistic regression model was applied to the entire dataset, but the "age", "default", and "pdays" variables weren't significant, so they were removed from the model. The final model didn't have multicollinearity and had an accuracy of 90.1%, but the AIC value was relatively high.

The same process was repeated with a probit model. After comparing these two models with an ANOVA test, it showed that the probit model appears to provide a better fit to the data compared to the logit model. However, without the p-value, it is difficult to determine the exact significance level. When both models were applied to the test data, they showed similar accuracies: logistic - 90.28% and probit - 90.26%.
