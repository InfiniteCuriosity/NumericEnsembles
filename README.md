
# NumericEnsembles

<!-- badges: start -->
<!-- badges: end -->

The goal of NumericEnsembles is to automatically conduct a thorough analysis of numeric data. The user only needs to provide the data and answer a few questions (such as which column to analyze). NumericEnsembles fits 18 individual models to the training data, and also makes predictions and checks accuracy for each of the individual models. It also builds 14 ensembles from the ensembles of data, fits each ensemble model to the training data then makes predictions and tracks accuracy for each ensemble. The package also automatically returns 26 plots (such as train vs holdout for the best model), 6 tables (such as head of the data), and a grand summary table sorted by accuracy with the best model at the top of the report.

## Installation

You can install the development version of NumericEnsembles like so:

``` r
devtools::install_github("InfiniteCuriosity/NumericEnsembles")
```

## Example

NumericEnsembles will automatically build 32 models to predict the sale price of houses in Boston, from the Boston housing data set.

``` r
library(NumericEnsembles)
Numeric(data = MASS::Boston,
        colnum = 14,
        numresamples = 2,
        remove_VIF_above = 5.00,
        remove_ensemble_correlations_greater_than = 1.00,
        scale_all_predictors_in_data = "N",
        data_reduction_method = 0,
        ensemble_reduction_method = 0,
        how_to_handle_strings = 0,
        predict_on_new_data = "N",
        save_all_trained_models = "N",
        set_seed = "N",
        save_all_plots = "N",
        use_parallel = "Y",
        train_amount = 0.60,
        test_amount = 0.20,
        validation_amount = 0.20)

```

The 32 models which are all built automatically and without error are:

1. Bagging
2. BayesGLM
3. BayesRNN
4. Cubist
5. Earth
6. Elastic (optimized by cross-validation)
7. Ensemble Bagging
8. Ensemble BayesGLM
9. Ensemble BayesRNN
10. Ensemble Cubist
11. Ensemble Earth
12. Ensemble Elastic (optimized by cross-validation)
13. Ensemble Gradient Boosted
14. Ensemble Lasso (optimized by cross-validation)
15. Ensemble Linear (tuned)
16. Ensemble Ridge (optimized by cross-validation)
17. Ensemble RPart
18. EnsembleSVM (tuned)
19. Ensemble Trees
20. Ensemble XGBoost
21. GAM (Generalized Additive Models, with smoothing splines)
22. Gradient Boosted (optimized)
23. Lasso
24. Linear (tuned)
25. Neuralnet
26. PCR (Principal Components Regression)
27. PLS (Partial Least Squares)
28. Ridge (optimized by cross-validation)
29. RPart
30. SVM (Support Vector Machines, tuned)
31. Tree
32. XGBoost

The 30 plots created automatically:

01. Correlation plot of the numeric data (as numbers and colors)
02. Correlation plot of the numeric data (as circles with colors)
03. Cook's D Bar Plot
04. Four plots in one for the most accurate model: Predicted vs actual, Residuals, Histogram of residuals, Q-Q plot
05. Most accurate model: Predicted vs actual
06. Most accurate model: Residuals
07. Most accurate model: Histogram of residuals
08. Most accurate model: Q-Q plot
09. Accuracy by resample and model, fixed scales
10. Accuracy by resample and model, free scales
11. Holdout RMSE/train RMSE, fixed scales
12. Holdout RMSE/train RMSE, free scales
13. Histograms of each numeric column
14. Boxplots of each numeric column
15. Predictor vs target variable
16. Model accuracy bar chart (RMSE)
17. t-test p-value bar chart
18. Train vs holdout by resample and model, free scales
19. Train vs holdout by resampleand model, fixed scales
20. Duration bar chart
21. Holdout RMSE / train RMSE bar chart
22. Mean bias bar chart
23. Mean MSE bar chart
24. Mean MAE bar chart
25. Mean SSE bar chart
26. Kolmogorov-Smirnof test bar chart
27. Bias plot by model and resample
28. MSE plot by model and resample
29. MAE plot by model and resample
30. SSE plot by model and resample

The tables created automatically (which are both searchable and sortable) are:

01. Variance Inflation Factor
02. Correlation of the ensemble
03. Head of the ensemble
04. Data summary
05. Correlation of the data
06. Grand summary table includes:
  1. Mean holdout RMSE
  2. Standard deviation of mean holdout RMSE
  3. t-test value
  4. t-test p-value
  5. t-test p-value standard deviation
  6. Kolmogorov-Smirnov stat mean
  7. Kolmogorov-Smirnov stat p-value
  8. Kolmogorov-Smirnov stat standard deviation
  9. Mean bias
  10. Mean bias standard deviation
  11. Mean MAE
  12. Mean MAE standard deviation
  13. Mean MSE
  14. Mean MSE standard deviation
  15. Mean SSE
  16. Mean SSE standard deviation
  17. Mean data (this is the mean of the target column in the original data set)
  18. Standard deviation of mean data (this is the standard deviation of the data in the target column in the original data set)
  19. Mean train RMSE
  20. Mean test RMSE
  21. Mean validation RMSE
  22. Holdout vs train mean
  23. Holdout vs train standard deviation
  24. Duration
  25. Duration standard deviation

## Example using pre-trained models on totally new data in the NumericEnsembles package

The NumericEnsembles package also has a way to create trained models and test those pre-trained models on totally unseen data *using the same pre-trained models as on the initial analysis.*

The package contains two example data sets to demonstrate this result. Boston_Housing is the Boston Housing data set, but the first five rows have been removed. We will build our models on that data set. NewBoston is totally new data, and actually the first five rows from the original Boston Housing data set.

``` r
library(NumericEnsembles)
Numeric(data = Boston_housing,
        colnum = 14,
        numresamples = 25,
        remove_VIF_above = 5.00,
        remove_ensemble_correlations_greater_than = 1.00,
        scale_all_predictors_in_data = "N",
        data_reduction_method = 0,
        ensemble_reduction_method = 0,
        how_to_handle_strings = 0,
        predict_on_new_data = "Y",
        set_seed = "N",
        save_all_trained_models = "N",
        save_all_plots = "N",
        use_parallel = "Y",
        train_amount = 0.60,
        test_amount = 0.20,
        validation_amount = 0.20)

```

Use the data set New_Boston when asked for "What is the URL of the new data?". The URL for the new data is: https://raw.githubusercontent.com/InfiniteCuriosity/EnsemblesData/refs/heads/main/NewBoston.csv

External data may be used to accomplish the same result.
