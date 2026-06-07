
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NumericEnsembles

The goal of NumericEnsembles is to automatically build ensembles of
complete solutions where the target column is numeric.

## The story of NumericEnsembles

My name is Russ Conte, and I have worked for many years with
multi-million dollar accounts for multi-billion dollar customers, main
as a recruiter. One of the most common results I have seen are companies
who do not use their data to get the best return for their investment to
get the data. I had the insight about how to build ensembles of
solutions on Saturday, October 22, 2022 at 4:58 pm. The original
ensembles solution has been improved many times, and NumericEnsembles is
one of ten ensembles solutions I have built that are currently
available. The total list is:

1.  `NumericEnsembles`
2.  `ClassificationEnsembles`
3.  `LogisticEnsembles`
4.  `ForecastingEnsembles`
5.  `ClusteringEnsembles`
6.  `SurvivalEnsembles`
7.  `TextEnsembles`
8.  `CountingEnsembles`
9.  `SeverityEnsembles`
10. `MultiLabelEnsembles`

## Installation

You can install the development version of NumericEnsembles from
[GitHub](https://github.com/) with:

``` r
# library(pak)
# pak::pkg_install("NumericEnsembles")
```

## Pipelines: The best way to get results from all the ensembles packages

All ten ensembles packages all work best if you start by building a
pipeline first. A pipeline will combine all the results (tables, plots,
etc.) in one file which you can print, plot, predict, export, save, and
much more.

## Your first pipeline in NumericEnsembles

``` r
library(NumericEnsembles)
Concrete_pipeline <- Numeric(
  dataset = Concrete[1:1000, ],
  target_col = 'Strength',
  facet_col = "",
  color_col = "",
  stratify_col = "",
  palette_style = "standard",
  verbose = "FALSE"
)
#> Number of parameters (weights and biases) to estimate: 27 
#> Nguyen-Widrow method
#> Scaling factor= 0.7012786 
#> gamma= 25.5713    alpha= 0.2424   beta= 18.6642
#> Loading required package: earth
#> Loading required package: Formula
#> Loading required package: plotmo
#> Loading required package: plotrix
#> Loading required package: gam
#> Loading required package: splines
#> Loading required package: foreach
#> Loaded gam 1.22-7
```

## Print six summary tables from the pipeline

``` r
print(Concrete_pipeline)
#> 
#> =========================================================================
#>                   NUMERIC PIPELINE PROFILE EXPORTS             
#> =========================================================================
#> 
#> [1. BASELINE DATA SAMPLE HEAD]
#>   Cement Blast_Furnace_Slag Fly_Ash Water Superplasticizer Coarse_Aggregate
#> 1  540.0                0.0       0   162              2.5           1040.0
#> 2  540.0                0.0       0   162              2.5           1055.0
#> 3  332.5              142.5       0   228              0.0            932.0
#> 4  332.5              142.5       0   228              0.0            932.0
#> 5  198.6              132.4       0   192              0.0            978.4
#> 6  266.0              114.0       0   228              0.0            932.0
#>   Fine_Aggregate Age Strength
#> 1          676.0  28    79.99
#> 2          676.0  28    61.89
#> 3          594.0 270    40.27
#> 4          594.0 365    41.05
#> 5          825.5 360    44.30
#> 6          670.0  90    47.03
#> 
#> [2. STRUCTURAL DATA DICTIONARY]
#>   Feature            Type    Missing_Count Missing_Pct Unique_Values
#> 1 Cement             numeric 0             0%          252          
#> 2 Blast_Furnace_Slag numeric 0             0%          171          
#> 3 Fly_Ash            numeric 0             0%          138          
#> 4 Water              numeric 0             0%          174          
#> 5 Superplasticizer   numeric 0             0%          106          
#> 6 Coarse_Aggregate   numeric 0             0%          260          
#> 7 Fine_Aggregate     numeric 0             0%          279          
#> 8 Age                integer 0             0%           14          
#> 9 Strength           numeric 0             0%          833          
#> 
#> [3. STATISTICAL POPULATION DESCRIPTIVE SUMMARY]
#>      Cement      Blast_Furnace_Slag    Fly_Ash           Water      
#>  Min.   :102.0   Min.   :  0.00     Min.   :  0.00   Min.   :121.8  
#>  1st Qu.:194.7   1st Qu.:  0.00     1st Qu.:  0.00   1st Qu.:164.9  
#>  Median :272.8   Median : 20.00     Median :  0.00   Median :185.0  
#>  Mean   :282.1   Mean   : 72.71     Mean   : 53.32   Mean   :181.4  
#>  3rd Qu.:356.8   3rd Qu.:142.50     3rd Qu.:118.30   3rd Qu.:192.0  
#>  Max.   :540.0   Max.   :359.40     Max.   :200.00   Max.   :247.0  
#>  Superplasticizer Coarse_Aggregate Fine_Aggregate       Age        
#>  Min.   : 0.000   Min.   : 801.0   Min.   :594.0   Min.   :  1.00  
#>  1st Qu.: 0.000   1st Qu.: 932.0   1st Qu.:732.0   1st Qu.:  7.00  
#>  Median : 6.100   Median : 968.0   Median :780.0   Median : 28.00  
#>  Mean   : 6.122   Mean   : 975.5   Mean   :773.9   Mean   : 46.19  
#>  3rd Qu.:10.100   3rd Qu.:1034.2   3rd Qu.:825.0   3rd Qu.: 56.00  
#>  Max.   :32.200   Max.   :1145.0   Max.   :992.6   Max.   :365.00  
#>     Strength    
#>  Min.   : 2.33  
#>  1st Qu.:23.52  
#>  Median :33.95  
#>  Mean   :35.70  
#>  3rd Qu.:46.23  
#>  Max.   :82.60  
#> 
#> [4. MULTICOLLINEARITY VIF FILTERS REPORT]
#>             Feature  VIF  Status
#>               Water 7.00 Dropped
#>              Cement 3.65    Kept
#>  Blast_Furnace_Slag 3.91    Kept
#>             Fly_Ash 3.96    Kept
#>    Superplasticizer 2.25    Kept
#>    Coarse_Aggregate 1.69    Kept
#>      Fine_Aggregate 2.45    Kept
#>                 Age 1.19    Kept
#> 
#> =========================================================================
#>                      LEADERBOARD & PREDICTIVE KPIS                       
#> =========================================================================
#> Total Models Run: 33
#> 
#> Top 10 Architectures By Testing RMSE:
#>                       Model Testing_RMSE Testing_MAE Adjusted_R2 Variance
#>         Cubist+RandomForest       5.1777      3.4325      0.9042 243.6943
#>           Cubist+QuantileRF       5.2372      3.4115      0.9020 253.0057
#>                      Cubist       5.2394      3.5121      0.9006 257.1599
#>    QuantileRF+Averaged_NNet       5.2722      3.7872      0.9006 238.3332
#>        Cubist+Averaged_NNet       5.2828      3.8581      0.9002 242.5385
#>  RandomForest+Averaged_NNet       5.3017      3.8453      0.8995 229.9509
#>     RandomForest+SVM_Radial       5.3650      3.6763      0.8971 235.7480
#>       SVM_Radial+QuantileRF       5.3831      3.6333      0.8964 244.6432
#>          Cubist+Bagged_MARS       5.3943      3.9464      0.8960 245.2803
#>             Cubist+BayesRNN       5.4101      3.9702      0.8954 245.4915
#>  KS_p_value Overfitting
#>      0.7332      1.8342
#>      0.9448      2.4795
#>      0.7289      1.5103
#>      0.6628      1.7760
#>      0.4192      1.2801
#>      0.4935      1.4432
#>      0.3614      1.8431
#>      0.6028      2.3399
#>      0.4346      1.1998
#>      0.5386      1.1752
#> 
#> =========================================================================
#>                AUTOMATED RESIDUAL DIAGNOSTIC LEADERBOARD                 
#> =========================================================================
#>                       Model Residual_Normality Variance_Stability
#>         Cubist+RandomForest         Non-Normal    Heteroscedastic
#>           Cubist+QuantileRF         Non-Normal    Heteroscedastic
#>                      Cubist         Non-Normal    Heteroscedastic
#>    QuantileRF+Averaged_NNet         Non-Normal    Heteroscedastic
#>        Cubist+Averaged_NNet         Non-Normal    Heteroscedastic
#>  RandomForest+Averaged_NNet         Non-Normal    Heteroscedastic
#>     RandomForest+SVM_Radial         Non-Normal    Heteroscedastic
#>       SVM_Radial+QuantileRF         Non-Normal    Heteroscedastic
#>          Cubist+Bagged_MARS         Non-Normal    Heteroscedastic
#>             Cubist+BayesRNN         Non-Normal    Heteroscedastic
#>  Error_Independence
#>         Independent
#>         Independent
#>         Independent
#>         Independent
#>         Independent
#>         Independent
#>         Independent
#>         Independent
#>         Independent
#>         Independent
```

The six summary tables:

1.  Baseline sample head (the first six rows of the data)
2.  Data dictionary
3.  Data summary (Min., 1st Qu., Media, Mean, 3rd Qu., Max.)
4.  Multicollineraity VIF Filters Report
5.  Leaderboard: Top 10 Architectures by Testing RMSE
6.  Residuals Diagnostics

## Generate 11 plots from the pipeline

``` r
plot(Concrete_pipeline) # Return one plot at a time
```

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-1.png" alt="" width="100%" /><img src="man/figures/README-Print plots from the pipeline, two ways to do it-2.png" alt="" width="100%" /><img src="man/figures/README-Print plots from the pipeline, two ways to do it-3.png" alt="" width="100%" />

    #> `geom_smooth()` using formula = 'y ~ x'

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-4.png" alt="" width="100%" /><img src="man/figures/README-Print plots from the pipeline, two ways to do it-5.png" alt="" width="100%" /><img src="man/figures/README-Print plots from the pipeline, two ways to do it-6.png" alt="" width="100%" /><img src="man/figures/README-Print plots from the pipeline, two ways to do it-7.png" alt="" width="100%" /><img src="man/figures/README-Print plots from the pipeline, two ways to do it-8.png" alt="" width="100%" /><img src="man/figures/README-Print plots from the pipeline, two ways to do it-9.png" alt="" width="100%" /><img src="man/figures/README-Print plots from the pipeline, two ways to do it-10.png" alt="" width="100%" /><img src="man/figures/README-Print plots from the pipeline, two ways to do it-11.png" alt="" width="100%" />

``` r
Concrete_pipeline$plots # Return all plots at the same time
#> $histograms
```

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-12.png" alt="" width="100%" />

    #> 
    #> $boxplots

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-13.png" alt="" width="100%" />

    #> 
    #> $correlation

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-14.png" alt="" width="100%" />

    #> 
    #> $scatter_matrix
    #> `geom_smooth()` using formula = 'y ~ x'

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-15.png" alt="" width="100%" />

    #> 
    #> $metric_heatmap

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-16.png" alt="" width="100%" />

    #> 
    #> $kpis

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-17.png" alt="" width="100%" />

    #> 
    #> $risks

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-18.png" alt="" width="100%" />

    #> 
    #> $tradeoff

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-19.png" alt="" width="100%" />

    #> 
    #> $ks_test

<img src="man/figures/README-Print plots from the pipeline, two ways to do it-20.png" alt="" width="100%" />

    #> 
    #> $draw_top3
    #> function () 
    #> {
    #>     .draw_top3_panel(top_3_models, pred_test_list, actual_test, 
    #>         models_list, train_data, target_col, theme_colors)
    #> }
    #> <bytecode: 0xadf5c9d90>
    #> <environment: 0xadf5c7230>
    #> 
    #> $draw_diagnostics
    #> function () 
    #> {
    #>     .draw_diagnostics_panel(top_3_models, pred_test_list, pred_train_list, 
    #>         actual_test, actual_train, test_data, target_col, theme_colors, 
    #>         top_pred_names)
    #> }
    #> <bytecode: 0xadf5c6468>
    #> <environment: 0xadf5c7230>

The 11 plots:

Histograms (Feature Distributions)

Box Plots (Feature Range Profiles)

Feature Correlation Heatmap

Scatter Analysis: Target Variable vs Each Feature (with trend line)

Comparative Performance Metric Heat map Matrix

Core Model Performance Metrics (with 95% Confidence Intervals) and KPIs

Overfitting ratio (closer to 1.00 is better)

Directional Model Bias (closer to 0.00 is better)

Bias-Variance Joint Mapping Space (bias is the x-axis, variance is the
y-axis)

Kolomogorov-Smirnov Test p-values (tests how likely the model result is
from the same distribution as the training data)

## Predicting on new data, maybe the most important part

``` r
Pipeline_prediction <- predict(object = Concrete_pipeline, newdata = Concrete[1001:nrow(Concrete), ], model_name = "best")
Pipeline_prediction
#>  [1] 35.86373 47.02990 47.22531 54.92194 52.40724 38.45000 20.61849 27.96067
#>  [9] 32.24154 37.72459 36.56694 41.76276 57.41181 43.78478 32.73451 47.73055
#> [17] 18.40128 37.38667 39.06619 28.54410 41.94186 32.96326 38.28524 33.83889
#> [25] 38.27045 44.97791 36.05123 26.91195 30.69691 38.11566
```

## Professional features of NumericEnsembles

Configuration options:

<figure>
<img src="images/Config_file.jpg"
alt="Configuration options in the NumericEnsemblesConfig() function" />
<figcaption aria-hidden="true">Configuration options in the
NumericEnsemblesConfig() function</figcaption>
</figure>

<figure>
<img src="images/Plot_output.jpg"
alt="All plotting options from the pipeline" />
<figcaption aria-hidden="true">All plotting options from the
pipeline</figcaption>
</figure>

All the full reports from the pipeline:

<figure>
<img src="images/NumericEnsemblespipelinefeatures.jpg"
alt="All the options from the pipeline (top level only)" />
<figcaption aria-hidden="true">All the options from the pipeline (top
level only)</figcaption>
</figure>
