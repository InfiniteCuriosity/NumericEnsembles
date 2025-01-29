
# NumericEnsembles

<!-- badges: start -->
<!-- badges: end -->

The goal of NumericEnsembles is to automatically conduct a thorough analysis of numeric data. The user only needs to provide the data and answer a few questions (such as which column to analyze). NumericEnsembles fits 23 individual models to the training data, and also makes predictions and checks accuracy for each of the individual models. It also builds 17 ensembles from the 23 individual data, fits each ensemble model to the training data then makes predictions and tracks accuracy for each ensemble. The package also automatically returns 26 plots (such as train vs holdout for the best model), 6 tables (such as head of the data), and a grand summary table sorted by accuracy with the best model at the top of the report.

## Installation

You can install the development version of NumericEnsembles like so:

``` r
devtools::install_github("InfiniteCuriosity/NumericEnsembles")
```

## Example

NumericEnsembles will automatically build 40 models to predict the sale price of houses in Boston, from the Boston housing data set.

``` r
library(NumericEnsembles)
Numeric(data = MASS::Boston,
        colnum = 14,
        numresamples = 25,
        how_to_handle_strings = 0,
        do_you_have_new_data = "N",
        save_all_trained_models = "N",
        remove_ensemble_correlations_greater_than = 1.00,
        use_parallel = "Y",
        train_amount = 0.60,
        test_amount = 0.20,
        validation_amount = 0.20
)

```

The 40 models which are all built automatically and without error are:

1. Bagged Random Forest (tuned)
2. Bagging
3. BayesGLM
4. BayesRNN
5. BoostRF (tuned)
6. Cubist
7. Earth
8. Elastic (optimized by cross-validation)
9. Ensemble Bagged Random Forest (tuned)
10. Ensemble Bagging
11. Ensemble BayesGLM
12. Ensemble BayesRNN
13. Ensemble BoostRF (tuned)
14. Ensemble Cubist
15. Ensemble Earth
16. Ensemble Elastic (optimized by cross-validation)
17. Ensemble Gradient Boosted
18. Ensemble K-Nearest Neighbors (tuned)
19. Ensemble Lasso (optimized by cross-validation)
20. Ensemble Linear (tuned)
21. EnsembleRF (tuned)
22. Ensemble Ridge (optimized by cross-validation)
23. Ensemble RPart
24. EnsembleSVM (tuned)
25. Ensemble Trees
26. Ensemble XGBoost
27. GAM (Generalized Additive Models)
28. Gradient Boosted
29. KNN (K-Nearest Neighbors) (tuned)
30. Lasso
31. Linear (tuned)
32. Neuralnet
33. PCR (Principal Components Regression)
34. PLS (Partial Least Squares)
35. RF (Random Forest)
36. Ridge (optimized by cross-validation)
37. RPart
38. SVM (Support Vector Machines, tuned)
39. Tree
40. XGBoost

The 26 plots created automatically:

1. SSE by model and resample
2. MAE by model and resample
3. MSE by model and resample
4. Bias by model and resample
5. Mean SSE barchart
6. Mean MAE barchart
7. Mean MSE barchart
8. Mean bias barchart
10. Over or underfitting barchart
11. Duration barchart
12. Train vs holdout by model and resample
13. Model accuracy barchart
14. y (predictor variable) vs target variables
15. Boxplots of the numeric data
16. Histograms of the numeric data
17. Overfitting by model and resample
18. Accuracy by model and resample
19. Best model Q-Q plot
20. Best model histogram of the residuals
21. Best model residuals
22. Best model predicted vs actual
23. Best model four plots at once (Predicted vs actual, residuals, histogram of residuals, Q-Q plot)
24. Correlation plot of the numeric data as circles and colors
25. Correlation of the numeric data as numbers and colors
26. Pairwise scatter plots

The tables created automatically are:

1. Correlation of the ensemble
2. Head of the ensemble
3. Data summary
4. Correlation of the data
5. RMSE, means, fitting, model summaries of the train, test and validation sets
6. Head of the data frame

## Example using pre-trained models on totally new data in the NumericEnsembles package

The NumericEnsembles package also has a way to create trained models and test those pre-trained models on totally unseen data *using the same pre-trained models as on the initial analysis.*

The package contains two example data sets to demonstrate this result. Boston_Housing is the Boston Housing data set, but the first five rows have been removed. We will build our models on that data set. NewBoston is totally new data, and actually the first five rows from the original Boston Housing data set.

``` r
library(NumericEnsembles)
Numeric(data = Boston_housing,
        colnum = 14,
        numresamples = 25,
        how_to_handle_strings = 0,
        do_you_have_new_data = "Y",
        save_all_trained_models = "Y",
        remove_ensemble_correlations_greater_than = 1.00,
        use_parallel = "Y",
        train_amount = 0.60,
        test_amount = 0.20,
        validation_amount = 0.20
)

```

Use the data set New_Boston when asked for "What is the URL of the new data?". The URL for the new data is: https://raw.githubusercontent.com/InfiniteCuriosity/EnsemblesData/refs/heads/main/NewBoston.csv

External data may be used to accomplish the same result.
