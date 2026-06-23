
# NumericEnsembles

The goal of NumericEnsembles is to automatically build highly optimized
ensembles of complete solutions where the target column is continuous
numeric.

## The story of NumericEnsembles

My name is Russ Conte, and I have worked for many years with
multi-million dollar accounts for multi-billion dollar customers, mainly
as a recruiter. One of the most common results I have seen are companies
who do not use their data to get the best return for their investment to
get the data. I had the insight about how to build ensembles of
solutions on Saturday, October 22, 2022 at 4:58 pm. The original
ensembles solution has been improved many times, and NumericEnsembles is
one of 13 ensembles solutions I have built that are currently available.
The total list is:

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

11. `NetworkEnsembles`

12. `SpatialEnsembles`

13. `SurveyEnsembles`

## Installation

You can install the development version of NumericEnsembles
from [GitHub](https://github.com/) with:

``` r
# library(pak)
# pak::pkg_install("InfiniteCuriosity/NumericEnsembles")
```

## Pipelines: The best way to get results from all the ensembles packages

All 13 ensembles packages work best if you start by building a pipeline
first. A pipeline combines all the results (tables, plots, models, and
metadata) into one structured asset which you can print, plot, predict,
export, save, and much more.

## Track 1: The Express Track (Quick Start)

The Express Track allows you to test your installation instantly using
rapid cross-validation configurations and automated synthetic data
generations:

``` r
library(NumericEnsembles)

# Using internal demo data generator as an express validation run
Concrete_express_pipeline <- NumericEnsemblesDemo()
#> Initializing NumericEnsembles Comprehensive Validation Demo...
#> --- Comprehensive Machine Learning Pipeline ---
#> 
#> [Extracting Baseline Profiles]: Capturing Head, Summary, and Correlation matrices...
#> 
#> [EDA Engine]: Generating data distribution, correlation, and scatter plots...
#> 
#> [VIF Check]: Evaluating attributes for multicollinearity using car::vif...
#> 
#> [Modeling Phase]: Launching 17 competitive base architectures concurrently...
#> Number of parameters (weights and biases) to estimate: 12 
#> Nguyen-Widrow method
#> Scaling factor= 0.7050777 
#> gamma= 5.9419     alpha= 1.1572   beta= 38.7273
#> Loading required package: earth
#> Loading required package: Formula
#> Loading required package: plotmo
#> Loading required package: plotrix
#> note: only 1 unique complexity parameters in default grid. Truncating the grid to 1 .
#> 
#> note: only 1 unique complexity parameters in default grid. Truncating the grid to 1 .
#> 
#> 
#> [Meta-Learner Engine]: Training 6 Advanced Stacking Meta-Learners (GLM, Enet, GAM, PLS, RF, SVM)...
#> Loading required package: gam
#> Loading required package: splines
#> Loading required package: foreach
#> Loaded gam 1.22-7
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
#> 
#> =========================================================================
#>                   NUMERIC PIPELINE PIPELINE PROFILE EXPORTS             
#> =========================================================================
#> 
#> [1. BASELINE DATA SAMPLE HEAD]
#>   GDP_Growth Housing_Index Unemployment
#> 1  12.459262      257.9835   0.04228547
#> 2   9.955127      190.2356   0.04594228
#> 3  10.826804      222.7095   0.07676044
#> 4  11.816892      232.1502   0.10300087
#> 5  12.344511      224.1494   0.04038258
#> 6  11.923865      206.2856   0.11040796
#> 
#> [2. STRUCTURAL DATA DICTIONARY]
#>   Feature       Type    Missing_Count Missing_Pct Unique_Values
#> 1 GDP_Growth    numeric 0             0%          250          
#> 2 Housing_Index numeric 0             0%          250          
#> 3 Unemployment  numeric 0             0%          250          
#> 
#> [3. PIPELINE AUTOMATED EXPLORATORY SUMMARY INSIGHTS]
#>   Feature_Name  Data_Type          Missing_Rate Skewness_Coef Outliers_Found
#> 1 GDP_Growth    Numeric Continuous 0%           -0.12         1             
#> 2 Housing_Index Numeric Continuous 0%           -0.04         3             
#> 3 Unemployment  Numeric Continuous 0%            0.03         0             
#>   Operational_Insight          
#> 1 Structural Signature: Healthy
#> 2 Structural Signature: Healthy
#> 3 Structural Signature: Healthy
#> 
#> [4. STATISTICAL POPULATION DESCRIPTIVE SUMMARY]
#>    GDP_Growth     Housing_Index    Unemployment    
#>  Min.   : 5.898   Min.   :105.2   Min.   :0.03004  
#>  1st Qu.: 9.703   1st Qu.:187.4   1st Qu.:0.05398  
#>  Median :10.839   Median :208.8   Median :0.07491  
#>  Mean   :10.844   Mean   :209.3   Mean   :0.07546  
#>  3rd Qu.:11.901   3rd Qu.:232.2   3rd Qu.:0.09810  
#>  Max.   :14.630   Max.   :304.6   Max.   :0.11964  
#> 
#> [5. MULTICOLLINEARITY VIF FILTERS REPORT]
#>        Feature  VIF Status
#>  Housing_Index 1.04   Kept
#>   Unemployment 1.04   Kept
#> 
#> =========================================================================
#>                      LEADERBOARD & PREDICTIVE KPIS                       
#> =========================================================================
#> Total Models Analyzed: 33
#> Sampling Protocol: Standard Population Split
#> 
#> Top 10 Architectures By Testing RMSE:
#>              Model Testing_RMSE Testing_MAE Adjusted_R2 Variance KS_p_value
#>             Linear       0.5319      0.4242      0.8581   1.7708     0.3777
#>  Linear+ElasticNet       0.5334      0.4260      0.8573   1.7520     0.3777
#>       Linear+Lasso       0.5334      0.4260      0.8573   1.7520     0.3777
#>      Linear+Cubist       0.5335      0.4258      0.8572   1.7642     0.3777
#>         ElasticNet       0.5351      0.4278      0.8564   1.7334     0.3777
#>              Lasso       0.5351      0.4278      0.8564   1.7334     0.3777
#>   ElasticNet+Lasso       0.5351      0.4278      0.8564   1.7334     0.3777
#>             Cubist       0.5352      0.4275      0.8563   1.7577     0.3777
#>  Cubist+ElasticNet       0.5352      0.4277      0.8563   1.7455     0.3777
#>       Cubist+Lasso       0.5352      0.4277      0.8563   1.7455     0.3777
#>  Overfitting    Bias Duration
#>       1.0706 -0.0212    1.462
#>       1.0735 -0.0216    1.549
#>       1.0735 -0.0216    1.529
#>       1.0737 -0.0238    1.595
#>       1.0765 -0.0221    0.087
#>       1.0765 -0.0221    0.067
#>       1.0765 -0.0221    0.154
#>       1.0769 -0.0265    0.133
#>       1.0767 -0.0243    0.220
#>       1.0767 -0.0243    0.200
#> 
#> =========================================================================
#>                AUTOMATED RESIDUAL DIAGNOSTIC LEADERBOARD                 
#> =========================================================================
#>              Model Residual_Normality Variance_Stability Error_Independence
#>             Linear             Normal      Homoscedastic        Independent
#>  Linear+ElasticNet             Normal      Homoscedastic        Independent
#>       Linear+Lasso             Normal      Homoscedastic        Independent
#>      Linear+Cubist             Normal      Homoscedastic        Independent
#>         ElasticNet             Normal      Homoscedastic        Independent
#>              Lasso             Normal      Homoscedastic        Independent
#>   ElasticNet+Lasso             Normal      Homoscedastic        Independent
#>             Cubist             Normal      Homoscedastic        Independent
#>  Cubist+ElasticNet             Normal      Homoscedastic        Independent
#>       Cubist+Lasso             Normal      Homoscedastic        Independent
```

## Track 2: The Fast (but not as fast as Express) track

This example features facet colors, column colors and stratify colors.
You can use these features in many other data sets.

``` r
library(NumericEnsembles)
Insurance <- NumericEnsembles::Insurance
Insurance_pipeline <- Numeric(dataset = Insurance, target_col = 'charges', facet_col = 'sex', color_col = 'smoker', stratify_col = 'region', palette_style = "modern", config = NumericEnsemblesFastConfig(), verbose = TRUE)
#> --- Comprehensive Machine Learning Pipeline ---
#> 
#> [Extracting Baseline Profiles]: Capturing Head, Summary, and Correlation matrices...
#> 
#> [EDA Engine]: Generating data distribution, correlation, and scatter plots...
#> 
#> [VIF Check]: Evaluating attributes for multicollinearity using car::vif...
#> 
#> [Modeling Phase]: Launching 17 competitive base architectures concurrently...
#> Number of parameters (weights and biases) to estimate: 20 
#> Nguyen-Widrow method
#> Scaling factor= 0.7006037 
#> gamma= 19.5235    alpha= 0.2628   beta= 27.0007 
#> 
#> [Meta-Learner Engine]: Training 6 Advanced Stacking Meta-Learners (GLM, Enet, GAM, PLS, RF, SVM)...
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
print(Insurance_pipeline)
#> 
#> =========================================================================
#>                   NUMERIC PIPELINE PIPELINE PROFILE EXPORTS             
#> =========================================================================
#> 
#> [1. BASELINE DATA SAMPLE HEAD]
#>   age    sex    bmi children smoker    region   charges
#> 1  19 female 27.900        0    yes southwest 16884.924
#> 2  18   male 33.770        1     no southeast  1725.552
#> 3  28   male 33.000        3     no southeast  4449.462
#> 4  33   male 22.705        0     no northwest 21984.471
#> 5  32   male 28.880        0     no northwest  3866.855
#> 6  31 female 25.740        0     no southeast  3756.622
#> 
#> [2. STRUCTURAL DATA DICTIONARY]
#>   Feature  Type      Missing_Count Missing_Pct Unique_Values
#> 1 age      integer   0             0%            47         
#> 2 sex      character 0             0%             2         
#> 3 bmi      numeric   0             0%           548         
#> 4 children integer   0             0%             6         
#> 5 smoker   character 0             0%             2         
#> 6 region   character 0             0%             4         
#> 7 charges  numeric   0             0%          1337         
#> 
#> [3. PIPELINE AUTOMATED EXPLORATORY SUMMARY INSIGHTS]
#>   Feature_Name Data_Type          Missing_Rate Skewness_Coef Outliers_Found
#> 1 age          Numeric Continuous 0%           0.06            0           
#> 2 sex          character          0%             NA            0           
#> 3 bmi          Numeric Continuous 0%           0.28            9           
#> 4 children     Numeric Continuous 0%           0.94            0           
#> 5 smoker       character          0%             NA            0           
#> 6 region       character          0%             NA            0           
#> 7 charges      Numeric Continuous 0%           1.51          139           
#>   Operational_Insight                                        
#> 1 Structural Signature: Healthy                              
#> 2 Discrete Feature / Dummy Pipeline Required                 
#> 3 Structural Signature: Healthy                              
#> 4 Structural Signature: Healthy                              
#> 5 Discrete Feature / Dummy Pipeline Required                 
#> 6 Discrete Feature / Dummy Pipeline Required                 
#> 7 High Structural Tail Skewness Risk / Severe Outlier Density
#> 
#> [4. STATISTICAL POPULATION DESCRIPTIVE SUMMARY]
#>       age               sex            bmi           children    
#>  Min.   :18.00   Length   :1338   Min.   :15.96   Min.   :0.000  
#>  1st Qu.:27.00   N.unique :   2   1st Qu.:26.30   1st Qu.:0.000  
#>  Median :39.00   N.blank  :   0   Median :30.40   Median :1.000  
#>  Mean   :39.21   Min.nchar:   4   Mean   :30.66   Mean   :1.095  
#>  3rd Qu.:51.00   Max.nchar:   6   3rd Qu.:34.69   3rd Qu.:2.000  
#>  Max.   :64.00                    Max.   :53.13   Max.   :5.000  
#>        smoker           region        charges     
#>  Length   :1338   Length   :1338   Min.   : 1122  
#>  N.unique :   2   N.unique :   4   1st Qu.: 4740  
#>  N.blank  :   0   N.blank  :   0   Median : 9382  
#>  Min.nchar:   2   Min.nchar:   9   Mean   :13270  
#>  Max.nchar:   3   Max.nchar:   9   3rd Qu.:16640  
#>                                    Max.   :63770  
#> 
#> [5. MULTICOLLINEARITY VIF FILTERS REPORT]
#>          Feature  VIF Status
#>              age 1.01   Kept
#>          sexmale 1.01   Kept
#>              bmi 1.12   Kept
#>         children 1.01   Kept
#>        smokeryes 1.02   Kept
#>  regionnorthwest 1.52   Kept
#>  regionsoutheast 1.69   Kept
#>  regionsouthwest 1.53   Kept
#> 
#> =========================================================================
#>                      LEADERBOARD & PREDICTIVE KPIS                       
#> =========================================================================
#> Total Models Analyzed: 33
#> Sampling Protocol: Stratified Sampling based on column 'region'
#> 
#> Top 10 Architectures By Testing RMSE:
#>                       Model Testing_RMSE Testing_MAE Adjusted_R2  Variance
#>    BayesRNN+Cond_Inf_Forest     4908.037    2522.371      0.8309 126742185
#>             Cond_Inf_Forest     4912.411    2529.773      0.8287 129879265
#>            Bagging+BayesRNN     4935.244    2603.404      0.8290 122689321
#>     Bagging+Cond_Inf_Forest     4941.068    2634.172      0.8286 125840811
#>  QuantileRF+Cond_Inf_Forest     4953.368    2155.342      0.8278 129123324
#>      Cubist+Cond_Inf_Forest     4960.479    2129.591      0.8273 128970974
#>  SVM_Radial+Cond_Inf_Forest     4964.969    2539.397      0.8270 118230632
#>         BayesRNN+QuantileRF     4979.868    2183.544      0.8259 126282087
#>          Bagging+SVM_Radial     4995.343    2628.982      0.8248 114216462
#>          Bagging+QuantileRF     5003.294    2356.372      0.8243 125309146
#>  KS_p_value Overfitting      Bias Duration
#>      0.0095      1.2588 -435.5202    1.992
#>      0.0268      1.3121 -425.0740    1.858
#>      0.0000      1.1986 -426.7560    0.289
#>      0.0000      1.2457 -416.3099    2.013
#>      0.0673      1.3751 -946.4446    3.797
#>      0.0193      1.2775 -979.4971    2.237
#>      0.0076      1.2550 -657.9079    2.582
#>      0.0390      1.3047 -956.8907    2.073
#>      0.0000      1.1954 -649.1437    0.879
#>      0.0000      1.3012 -937.6804    2.094
#> 
#> =========================================================================
#>                AUTOMATED RESIDUAL DIAGNOSTIC LEADERBOARD                 
#> =========================================================================
#>                       Model             Residual_Normality
#>    BayesRNN+Cond_Inf_Forest Non-Normal (Biased Tail Risks)
#>             Cond_Inf_Forest Non-Normal (Biased Tail Risks)
#>            Bagging+BayesRNN Non-Normal (Biased Tail Risks)
#>     Bagging+Cond_Inf_Forest Non-Normal (Biased Tail Risks)
#>  QuantileRF+Cond_Inf_Forest Non-Normal (Biased Tail Risks)
#>      Cubist+Cond_Inf_Forest Non-Normal (Biased Tail Risks)
#>  SVM_Radial+Cond_Inf_Forest Non-Normal (Biased Tail Risks)
#>         BayesRNN+QuantileRF Non-Normal (Biased Tail Risks)
#>          Bagging+SVM_Radial Non-Normal (Biased Tail Risks)
#>          Bagging+QuantileRF Non-Normal (Biased Tail Risks)
#>                   Variance_Stability Error_Independence
#>                        Homoscedastic        Independent
#>  Heteroscedastic (Unstable Variance)        Independent
#>  Heteroscedastic (Unstable Variance)        Independent
#>  Heteroscedastic (Unstable Variance)        Independent
#>  Heteroscedastic (Unstable Variance)        Independent
#>  Heteroscedastic (Unstable Variance)        Independent
#>  Heteroscedastic (Unstable Variance)        Independent
#>  Heteroscedastic (Unstable Variance)        Independent
#>                        Homoscedastic        Independent
#>                        Homoscedastic        Independent
#> 
#> [Audit Alert]: Heteroscedasticity caught in leader zone. Upper intervals could degrade.
#> [Audit Alert]: Non-normal residuals mapped in leader zone. Points possess fat tails.
Insurance_pipeline$plots # plots all in one command
#> $histograms
```

<img src="man/figures/README-Fast but not express track-1.png" alt="" width="100%" />

    #> 
    #> $boxplots

<img src="man/figures/README-Fast but not express track-2.png" alt="" width="100%" />

    #> 
    #> $correlation

<img src="man/figures/README-Fast but not express track-3.png" alt="" width="100%" />

    #> 
    #> $scatter_matrix
    #> `geom_smooth()` using formula = 'y ~ x'

<img src="man/figures/README-Fast but not express track-4.png" alt="" width="100%" />

    #> 
    #> $metric_heatmap

<img src="man/figures/README-Fast but not express track-5.png" alt="" width="100%" />

    #> 
    #> $kpis

<img src="man/figures/README-Fast but not express track-6.png" alt="" width="100%" />

    #> 
    #> $risks

<img src="man/figures/README-Fast but not express track-7.png" alt="" width="100%" />

    #> 
    #> $tradeoff

<img src="man/figures/README-Fast but not express track-8.png" alt="" width="100%" />

    #> 
    #> $ks_test

<img src="man/figures/README-Fast but not express track-9.png" alt="" width="100%" />

    #> 
    #> $cooks_distance

<img src="man/figures/README-Fast but not express track-10.png" alt="" width="100%" />

    #> 
    #> $draw_top3
    #> function () 
    #> {
    #>     .draw_top3_panel(top_3_models, pred_test_list, actual_test, 
    #>         models_list, train_data, target_col, theme_colors)
    #> }
    #> <bytecode: 0x99801a0e8>
    #> <environment: 0x988151428>
    #> 
    #> $draw_diagnostics
    #> function () 
    #> {
    #>     .draw_diagnostics_panel(top_3_models, pred_test_list, pred_train_list, 
    #>         actual_test, actual_train, test_data, target_col, theme_colors, 
    #>         top_pred_names)
    #> }
    #> <bytecode: 0x99801a698>
    #> <environment: 0x988151428>

## Track 3: The Institutional Track (Professional Production, this example will have a lower root mean squared error than an article in Nature for the exact same data set)

For this next example we will be using the `Concrete` data set, and
achieving a lower root mean squared error (RMSE) than this article in
Nature on the same data set:
<https://www.nature.com/articles/s41598-024-69616-9>. The article shows
a lowest RMSE of 5.11, NumericEnsembles will get a best RMSE that is
lower than 5.11, and you will be able to verify the result.

For enterprise-grade model deployments, you can decouple hyperparameter
states from your execution tracks using the
complete `NumericEnsemblesConfig()` matrix. This path showcases advanced
feature transformations (including `YeoJohnson`power-scaling),
high-leverage data outlier filtering via Cook’s Distance, and rigorous
multi-model hyperparameter tuning grids:

``` r
library(NumericEnsembles)

# 1. Establish custom, comprehensive hyperparameter tuning grids
custom_glmnet_grid <- expand.grid(
  alpha  = seq(0, 1, length = 5),
  lambda = seq(0.001, 0.2, length = 10)
)

custom_rf_grid <- expand.grid(
  mtry = c(2, 4, 6, 8)
)

# 2. Build the fine-grained execution configuration matrix
institutional_config <- NumericEnsemblesConfig(
  cv_folds        = 10,       # Rigid 10-fold cross-validation
  train_pct       = 0.80,     # 80/20 train-test population split
  vif_threshold   = 10.0,     # Strict multicollinearity screening
  cooks_threshold = 2.0,      # Prune high-leverage outliers over 2 * (4/n)
  transform_steps = c("nzv", "medianImpute", "center", "scale", "YeoJohnson"),
  glmnet_grid     = custom_glmnet_grid,
  rf_grid         = custom_rf_grid,
  svm_tune_length = 10,
  pcr_tune_length = 10
)

# 3. Execute the concurrent machine learning rival engine
Concrete_pipeline <- Numeric(
  dataset       = Concrete[1:1000, ], 
  target_col    = 'Strength', 
  palette_style = "modern", 
  config        = institutional_config, 
  verbose       = TRUE
)
#> --- Comprehensive Machine Learning Pipeline ---
#> 
#> [Extracting Baseline Profiles]: Capturing Head, Summary, and Correlation matrices...
#> 
#> [EDA Engine]: Generating data distribution, correlation, and scatter plots...
#> 
#> [Leverage Engine]: Pruning 25 structural outliers via Cook's Distance cutoff (0.00998)...
#> 
#> [VIF Check]: Evaluating attributes for multicollinearity using car::vif...
#> 
#> [Modeling Phase]: Launching 17 competitive base architectures concurrently...
#> Number of parameters (weights and biases) to estimate: 30 
#> Nguyen-Widrow method
#> Scaling factor= 0.7009904 
#> gamma= 28.6345    alpha= 0.3958   beta= 21.9296 
#> 
#> [Meta-Learner Engine]: Training 6 Advanced Stacking Meta-Learners (GLM, Enet, GAM, PLS, RF, SVM)...
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%

# 4 Verify five best results
Concrete_pipeline$performance_report[1:5, ]
#>                 Model Testing_RMSE RMSE 95% CI Lower RMSE 95% CI Upper
#> 1              Cubist       4.8121            3.6889            5.7189
#> 2   Cubist+QuantileRF       5.0015            3.6721            6.0453
#> 3 Cubist+RandomForest       5.0550            3.7793            6.0682
#> 4   Cubist+SVM_Radial       5.1411            4.1628            5.9608
#> 5         Cubist+MARS       5.1521            4.3007            5.8815
#>   Testing_MAE MAE 95% CI Lower MAE 95% CI Upper Adjusted_R2
#> 1      3.2407           2.7440           3.7375      0.9118
#> 2      3.1639           2.6230           3.7049      0.9077
#> 3      3.3311           2.8002           3.8621      0.9057
#> 4      3.5358           3.0146           4.0569      0.9024
#> 5      3.8439           3.3648           4.3229      0.9020
#>   Adjusted R2 95% CI Lower Adjusted R2 95% CI Upper Duration Overfitting
#> 1                   0.8761                   0.9484    2.088      1.7693
#> 2                   0.8658                   0.9299   14.480      2.8946
#> 3                   0.8647                   0.9299   20.834      2.1659
#> 4                   0.8695                   0.9299    5.759      1.6825
#> 5                   0.8729                   0.9299    2.478      1.3146
#>      Bias Variance KS_p_value
#> 1  0.1372 250.7353     0.6135
#> 2 -0.0770 236.7411     0.6135
#> 3  0.0233 227.5309     0.4804
#> 4  0.3395 251.0203     0.6407
#> 5  0.0410 233.9051     0.4687
```

## Print the Summary Profiles from any NumericEnsembles Pipeline

When you call `print()` on your pipeline object, it outputs a
multi-profile metadata evaluation series:

1.  **Baseline Sample Head & Population Description:** Immediate
    tracking of your baseline raw data structures.

2.  **Structural Data Dictionary:** Maps column classes, missing value
    counts, and unique value frequencies.

3.  **Automated Exploratory Summary Insights:** Granular tracking of
    feature anomalies, calculating exact skewness coefficients, IQR
    outlier counts, and emitting specific operational text insights.

4.  **Multicollinearity Threshold Audit Report:** A complete breakdown
    of columns evaluated, empirical Variance Inflation Factors (VIF),
    and selection status (“Kept” vs “Dropped”).

5.  **Ensemble Performance Leaderboard Evaluation:** Multi-engine
    rankings sorted by Testing RMSE, complete with MAE, Adjusted R2,
    prediction variance, and run durations.

6.  **Automated Residual Diagnostic Leaderboard:** Runs validation scans
    checking residuals for normality (Shapiro-Wilk), homoscedasticity
    (Spearman), and error independence (Durbin-Watson).

## Generate Diagnostic Plots from any NumericEnsembles Pipeline

You can interact with your visual diagnostics package in two standard
ways:

``` r
plot(Concrete_pipeline)  # Sequentially render plots to your active device window
```

<img src="man/figures/README-Generate diagnostic plots-1.png" alt="" width="100%" /><img src="man/figures/README-Generate diagnostic plots-2.png" alt="" width="100%" /><img src="man/figures/README-Generate diagnostic plots-3.png" alt="" width="100%" />

    #> `geom_smooth()` using formula = 'y ~ x'

<img src="man/figures/README-Generate diagnostic plots-4.png" alt="" width="100%" /><img src="man/figures/README-Generate diagnostic plots-5.png" alt="" width="100%" /><img src="man/figures/README-Generate diagnostic plots-6.png" alt="" width="100%" /><img src="man/figures/README-Generate diagnostic plots-7.png" alt="" width="100%" /><img src="man/figures/README-Generate diagnostic plots-8.png" alt="" width="100%" /><img src="man/figures/README-Generate diagnostic plots-9.png" alt="" width="100%" /><img src="man/figures/README-Generate diagnostic plots-10.png" alt="" width="100%" /><img src="man/figures/README-Generate diagnostic plots-11.png" alt="" width="100%" /><img src="man/figures/README-Generate diagnostic plots-12.png" alt="" width="100%" />

``` r
Concrete_pipeline$plots  # Direct programmatic access to specific ggplot2 objects
#> $histograms
```

<img src="man/figures/README-Generate diagnostic plots-13.png" alt="" width="100%" />

    #> 
    #> $boxplots

<img src="man/figures/README-Generate diagnostic plots-14.png" alt="" width="100%" />

    #> 
    #> $correlation

<img src="man/figures/README-Generate diagnostic plots-15.png" alt="" width="100%" />

    #> 
    #> $scatter_matrix
    #> `geom_smooth()` using formula = 'y ~ x'

<img src="man/figures/README-Generate diagnostic plots-16.png" alt="" width="100%" />

    #> 
    #> $metric_heatmap

<img src="man/figures/README-Generate diagnostic plots-17.png" alt="" width="100%" />

    #> 
    #> $kpis

<img src="man/figures/README-Generate diagnostic plots-18.png" alt="" width="100%" />

    #> 
    #> $risks

<img src="man/figures/README-Generate diagnostic plots-19.png" alt="" width="100%" />

    #> 
    #> $tradeoff

<img src="man/figures/README-Generate diagnostic plots-20.png" alt="" width="100%" />

    #> 
    #> $ks_test

<img src="man/figures/README-Generate diagnostic plots-21.png" alt="" width="100%" />

    #> 
    #> $cooks_distance

<img src="man/figures/README-Generate diagnostic plots-22.png" alt="" width="100%" />

    #> 
    #> $draw_top3
    #> function () 
    #> {
    #>     .draw_top3_panel(top_3_models, pred_test_list, actual_test, 
    #>         models_list, train_data, target_col, theme_colors)
    #> }
    #> <bytecode: 0x99801a0e8>
    #> <environment: 0x986fcce78>
    #> 
    #> $draw_diagnostics
    #> function () 
    #> {
    #>     .draw_diagnostics_panel(top_3_models, pred_test_list, pred_train_list, 
    #>         actual_test, actual_train, test_data, target_col, theme_colors, 
    #>         top_pred_names)
    #> }
    #> <bytecode: 0x99801a698>
    #> <environment: 0x986fcce78>

The professional visual diagnostics portfolio includes:

- **Histograms:** Continuous feature density and population distribution
  panels.

- **Box Plots:** Predictor range distribution quantiles and scale
  profiles.

- **Correlation Heatmap:** Multi-feature linear explanatory predictor
  correlation matrices.

- **Scatter Analysis Matrix:** Individual regression line mappings
  matching the target column against features.

- **Performance Metrics & KPIs:** Horizontal ranking of cross-validated
  architectures showcasing explicit **95% predictive confidence
  intervals** across RMSE, MAE, and Adjusted R2.

- **Generalization Risks & Structural Bias:** Mappings tracking
  overfitting ratios and model directionality bias.

- **Bias-Variance Space:** Joint coordinate mapping of directional model
  bias vs empirical variance relative to an absolute ideal vector
  origin.

- **Kolmogorov-Smirnov Test Mappings:** Charting distribution alignment
  p-values across your candidate algorithms.

- **Cook’s Distance Leverage Timeline:** A standalone segment tracker
  charting raw outlier boundaries.

## Predicting on New Data

Programmatic predictions can be generated instantly using the pipeline’s
optimized S3 method wrapper, utilizing the absolute top-performing
champion model architecture:

``` r
prospective_data <- Concrete[1001:1030, ]
Pipeline_predictions <- predict(object = Concrete_pipeline, newdata = prospective_data, model_name = "best")
```

For industrial workloads, use `predict_production()` to automatically
obtain point projections alongside matching row-level 95% upper and
lower assurance boundaries for the top 3 champion models:

``` r
Production_report <- predict_production(object = Concrete_pipeline, newdata = prospective_data)
Production_report
#>    Row_Index Rank_1_Cubist_Prediction Rank_1_Cubist_95_LowerBound
#> 1          1                    34.62                       25.17
#> 2          2                    43.88                       34.43
#> 3          3                    55.04                       45.59
#> 4          4                    65.06                       55.61
#> 5          5                    48.83                       39.38
#> 6          6                    35.70                       26.25
#> 7          7                    17.56                        8.11
#> 8          8                    31.79                       22.34
#> 9          9                    30.92                       21.47
#> 10        10                    38.16                       28.71
#> 11        11                    34.44                       24.99
#> 12        12                    43.37                       33.92
#> 13        13                    54.33                       44.87
#> 14        14                    41.25                       31.80
#> 15        15                    32.04                       22.59
#> 16        16                    47.40                       37.95
#> 17        17                    17.78                        8.33
#> 18        18                    39.29                       29.84
#> 19        19                    38.36                       28.91
#> 20        20                    25.16                       15.70
#> 21        21                    39.85                       30.40
#> 22        22                    29.19                       19.74
#> 23        23                    37.68                       28.23
#> 24        24                    37.70                       28.25
#> 25        25                    36.47                       27.02
#> 26        26                    43.83                       34.38
#> 27        27                    35.16                       25.71
#> 28        28                    29.12                       19.67
#> 29        29                    31.68                       22.22
#> 30        30                    34.10                       24.65
#>    Rank_1_Cubist_95_UpperBound Rank_2_Cubist_and_QuantileRF_Prediction
#> 1                        44.08                                   36.03
#> 2                        53.33                                   48.70
#> 3                        64.49                                   55.83
#> 4                        74.51                                   65.48
#> 5                        58.28                                   50.51
#> 6                        45.15                                   34.55
#> 7                        27.01                                   17.79
#> 8                        41.24                                   29.33
#> 9                        40.37                                   31.88
#> 10                       47.62                                   40.15
#> 11                       43.89                                   37.25
#> 12                       52.82                                   42.66
#> 13                       63.78                                   57.06
#> 14                       50.71                                   41.06
#> 15                       41.49                                   32.67
#> 16                       56.86                                   49.91
#> 17                       27.24                                   16.44
#> 18                       48.75                                   38.88
#> 19                       47.81                                   37.81
#> 20                       34.61                                   30.19
#> 21                       49.30                                   40.99
#> 22                       38.64                                   30.53
#> 23                       47.13                                   39.31
#> 24                       47.15                                   38.57
#> 25                       45.92                                   37.19
#> 26                       53.28                                   44.06
#> 27                       44.61                                   33.52
#> 28                       38.57                                   26.43
#> 29                       41.13                                   32.26
#> 30                       43.56                                   33.25
#>    Rank_2_Cubist_and_QuantileRF_95_LowerBound
#> 1                                       26.20
#> 2                                       38.87
#> 3                                       46.00
#> 4                                       55.66
#> 5                                       40.69
#> 6                                       24.72
#> 7                                        7.97
#> 8                                       19.50
#> 9                                       22.05
#> 10                                      30.32
#> 11                                      27.42
#> 12                                      32.83
#> 13                                      47.24
#> 14                                      31.24
#> 15                                      22.84
#> 16                                      40.09
#> 17                                       6.61
#> 18                                      29.05
#> 19                                      27.99
#> 20                                      20.37
#> 21                                      31.16
#> 22                                      20.70
#> 23                                      29.48
#> 24                                      28.75
#> 25                                      27.36
#> 26                                      34.23
#> 27                                      23.69
#> 28                                      16.60
#> 29                                      22.43
#> 30                                      23.43
#>    Rank_2_Cubist_and_QuantileRF_95_UpperBound
#> 1                                       45.85
#> 2                                       58.53
#> 3                                       65.66
#> 4                                       75.31
#> 5                                       60.34
#> 6                                       44.38
#> 7                                       27.62
#> 8                                       39.15
#> 9                                       41.71
#> 10                                      49.97
#> 11                                      47.08
#> 12                                      52.48
#> 13                                      66.89
#> 14                                      50.89
#> 15                                      42.50
#> 16                                      59.74
#> 17                                      26.26
#> 18                                      48.70
#> 19                                      47.64
#> 20                                      40.02
#> 21                                      50.82
#> 22                                      40.36
#> 23                                      49.13
#> 24                                      48.40
#> 25                                      47.02
#> 26                                      53.88
#> 27                                      43.34
#> 28                                      36.26
#> 29                                      42.08
#> 30                                      43.08
#>    Rank_3_Cubist_and_RandomForest_Prediction
#> 1                                      36.31
#> 2                                      45.22
#> 3                                      53.77
#> 4                                      62.87
#> 5                                      48.62
#> 6                                      34.78
#> 7                                      19.05
#> 8                                      29.07
#> 9                                      31.57
#> 10                                     38.02
#> 11                                     36.52
#> 12                                     42.29
#> 13                                     55.66
#> 14                                     41.68
#> 15                                     31.86
#> 16                                     48.90
#> 17                                     17.67
#> 18                                     39.40
#> 19                                     38.79
#> 20                                     28.75
#> 21                                     40.59
#> 22                                     30.96
#> 23                                     38.40
#> 24                                     37.73
#> 25                                     36.02
#> 26                                     43.73
#> 27                                     34.91
#> 28                                     27.31
#> 29                                     33.55
#> 30                                     34.61
#>    Rank_3_Cubist_and_RandomForest_95_LowerBound
#> 1                                         26.37
#> 2                                         35.29
#> 3                                         43.84
#> 4                                         52.93
#> 5                                         38.68
#> 6                                         24.85
#> 7                                          9.12
#> 8                                         19.13
#> 9                                         21.63
#> 10                                        28.09
#> 11                                        26.59
#> 12                                        32.36
#> 13                                        45.73
#> 14                                        31.75
#> 15                                        21.93
#> 16                                        38.96
#> 17                                         7.73
#> 18                                        29.47
#> 19                                        28.86
#> 20                                        18.82
#> 21                                        30.66
#> 22                                        21.02
#> 23                                        28.47
#> 24                                        27.79
#> 25                                        26.08
#> 26                                        33.80
#> 27                                        24.98
#> 28                                        17.38
#> 29                                        23.62
#> 30                                        24.68
#>    Rank_3_Cubist_and_RandomForest_95_UpperBound
#> 1                                         46.24
#> 2                                         55.16
#> 3                                         63.70
#> 4                                         72.80
#> 5                                         58.55
#> 6                                         44.72
#> 7                                         28.98
#> 8                                         39.00
#> 9                                         41.50
#> 10                                        47.96
#> 11                                        46.45
#> 12                                        52.23
#> 13                                        65.59
#> 14                                        51.61
#> 15                                        41.79
#> 16                                        58.83
#> 17                                        27.60
#> 18                                        49.34
#> 19                                        48.73
#> 20                                        38.68
#> 21                                        50.53
#> 22                                        40.89
#> 23                                        48.33
#> 24                                        47.66
#> 25                                        45.95
#> 26                                        53.66
#> 27                                        44.85
#> 28                                        37.24
#> 29                                        43.48
#> 30                                        44.54
```

## Render the Automated Executive Report

You can compile and render a standalone, professional corporate report
from your pipeline instantly. This function extracts your metadata
matrix and generates a polished, executive summary document utilizing
high-speed local Quarto compilation:

``` r
# Compiles a polished HTML document matching your chosen palette style
RenderExecutiveReport(pipeline_object = Concrete_pipeline, output_directory = getwd())
#> [1] TRUE
```

## Professional features of NumericEnsembles

- Fine-grained hyperparameters handling
  via `NumericEnsemblesConfig()` and rapid cross-validation
  configurations with `NumericEnsemblesFastConfig()`.

- Error-bound verifications delivering explicit 95% predictive
  confidence intervals across all primary KPIs.

- 7 professional curated test sample datasets available at
  the [EnsemblesData
  Repository](https://github.com/InfiniteCuriosity/EnsemblesData/tree/main/NumericEnsembles).

- Dedicated I/O pipelines for seamless asset transportation
  via `save_pipeline()`, `load_pipeline()`,
  and `ExportNumericResults()`.

- Never calls any large language models (LLMs). This is a completely
  LLM-free, high-performance local algorithmic solution.
