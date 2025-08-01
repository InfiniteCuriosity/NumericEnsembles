% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NumericEnsembles.R
\name{Numeric}
\alias{Numeric}
\title{Numeric—function to automatically build 23 individual models and 17 ensembles then return the results to the user}
\usage{
Numeric(
  data,
  colnum,
  numresamples,
  remove_VIF_above = 5,
  remove_ensemble_correlations_greater_than = 0.98,
  scale_all_predictors_in_data = c("Y", "N"),
  data_reduction_method = c(0("none"), 1("BIC exhaustive"), 2("BIC forward"),
    3("BIC backward"), 4("BIC seqrep"), 5("Mallows_cp exhaustive"),
    6("Mallows_cp forward"), 7("Mallows_cp backward"), 8("Mallows_cp, seqrep")),
  ensemble_reduction_method = c(0("none"), 1("BIC exhaustive"), 2("BIC forward"),
    3("BIC backward"), 4("BIC seqrep"), 5("Mallows_cp exhaustive"),
    6("Mallows_cp forward"), 7("Mallows_cp backward"), 8("Mallows_cp, seqrep")),
  how_to_handle_strings = c(0("none"), 1("factor levels"), 2("One-hot encoding"),
    3("One-hot encoding with jitter")),
  predict_on_new_data = c("Y", "N"),
  set_seed = c("Y", "N"),
  save_all_trained_models = c("Y", "N"),
  save_all_plots = c("Y", "N"),
  use_parallel = c("Y", "N"),
  train_amount,
  test_amount,
  validation_amount
)
}
\arguments{
\item{data}{data can be a CSV file or within an R package, such as MASS::Boston}

\item{colnum}{a column number in your data}

\item{numresamples}{the number of resamples}

\item{remove_VIF_above}{remove columns with Variable Inflation Factor above value chosen by the user}

\item{remove_ensemble_correlations_greater_than}{maximum value for correlations of the ensemble}

\item{scale_all_predictors_in_data}{"Y" or "N" to scale numeric data}

\item{data_reduction_method}{0(none), BIC (1, 2, 3, 4) or Mallow's_cp (5, 6, 7, 8) for Forward, Backward, Exhaustive and SeqRep}

\item{ensemble_reduction_method}{0(none), BIC (1, 2, 3, 4) or Mallow's_cp (5, 6, 7, 8) for Forward, Backward, Exhaustive and SeqRep}

\item{how_to_handle_strings}{0: No strings, 1: Factor values, 2: One-hot encoding, 3: One-hot encoding AND jitter}

\item{predict_on_new_data}{"Y" or "N". If "Y", then you will be asked for the new data}

\item{set_seed}{"Y" or "N" to set the seed to make the results fully reproducible}

\item{save_all_trained_models}{"Y" or "N". If "Y", then places all the trained models in the Environment}

\item{save_all_plots}{Saves all plots to the working directory}

\item{use_parallel}{"Y" or "N" for parallel processing}

\item{train_amount}{set the amount for the training data}

\item{test_amount}{set the amount for the testing data}

\item{validation_amount}{Set the amount for the validation data}
}
\value{
a real number
}
\description{
Numeric—function to automatically build 23 individual models and 17 ensembles then return the results to the user
}
