# NumericEnsembles 0.9.0
## Corrected 160 plots (40 models x 4 plots each, pred_vs_actual, pred_vs_residuals, hist_residuals and qq plots. They were not printing the correct plot, that is now fixed.)

## Added set_seed, so NumericEnsembles runs correctly when a specific seed is set.

# NumericEnsembles 0.8.0
## Added Variance Inflation Factor. The user is able to set the VIF value, and models are built with VIF values at or below the user's choice of VIF value

## Added "free" and "fixed" scales to all appropriate plots. Each result has two plots, one with free scales, the other with fixed scales.

## Added Kolomogrov-Smirnovv test to help the user see which models test similar to the actual holdout data

## Added several "Holdout vs train" charts to show how each model performs across multiple resamples, and the range of values of holdout RMSE / train RMSE

# NumericEnsembles 0.7.0

# NumericEnsembles 0.6.0

# NumericEnsembles 0.5.0

# NumericEnsembles 0.4.0

# NumericEnsembles 0.3.0

# NumericEnsembles 0.2.0

# NumericEnsembles 0.1.0

* Initial CRAN submission.

# NumericEnsembles 1.0.0

## Added example of New_Boston data set as a new data set to use in NumericEnsembles ('do_you_have_new_data?')

* To see how this works, use the data as Boston_housing, and the new data as New_Boston.

## Removed neuralnet models since I could not get the RMSE down, added cppls, which creates a much more accurate ensemble compared to the previous version.

## Removed cppls, not reliable on several data sets.

## Re-added neuralnet models (individual and ensemble) since I was able to get those to work without error

## Added best subsets: Forward, backward, exhaustive and seqrep

## Added save_all_plots to automatically save all plots in the user's choice of one of six graphics formats: eps, jpeg, pdf, png, svg or tiff

