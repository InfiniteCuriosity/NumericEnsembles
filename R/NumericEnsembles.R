#' Numeric—function to automatically build 23 individual models and 17 ensembles then return the results to the user
#'
#' @param data data can be a CSV file or within an R package, such as MASS::Boston
#' @param colnum a column number in your data
#' @param numresamples the number of resamples
#' @param how_to_handle_strings 0: No strings, 1: Factor values, 2: One-hot encoding, 3: One-hot encoding AND jitter
#' @param predict_on_new_data "Y" or "N". If "Y", then you will be asked for the new data
#' @param save_all_trained_models "Y" or "N". If "Y", then places all the trained models in the Environment
#' @param save_all_plots Saves all plots to the working directory
#' @param scale_all_predictors_in_data "Y" or "N" to scale numeric data
#' @param remove_VIF_above remove columns with Variable Inflation Factor above value chosen by the user
#' @param data_reduction_method 0(none), BIC (1, 2, 3, 4) or Mallow's_cp (5, 6, 7, 8) for Forward, Backward, Exhaustive and SeqRep
#' @param ensemble_reduction_method 0(none), BIC (1, 2, 3, 4) or Mallow's_cp (5, 6, 7, 8) for Forward, Backward, Exhaustive and SeqRep
#' @param remove_ensemble_correlations_greater_than maximum value for correlations of the ensemble
#' @param use_parallel "Y" or "N" for parallel processing
#' @param train_amount set the amount for the training data
#' @param test_amount set the amount for the testing data
#' @param validation_amount Set the amount for the validation data
#'
#' @return a real number
#' @export Numeric

#' @importFrom arm bayesglm
#' @importFrom brnn brnn
#' @importFrom broom tidy
#' @importFrom car vif
#' @importFrom caret dummyVars
#' @importFrom corrplot corrplot
#' @importFrom Cubist cubist
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr all_of arrange relocate rename last_col n_distinct filter %>% mutate_if
#' @importFrom e1071 tune.svm tune.gknn tune.randomForest
#' @importFrom earth earth
#' @importFrom gam gam gam.s s
#' @importFrom gbm gbm
#' @importFrom glmnet glmnet
#' @importFrom ggplot2 aes geom_boxplot facet_wrap labs geom_histogram
#' @importFrom graphics mtext par hist rect panel.smooth
#' @importFrom grDevices dev.off
#' @importFrom gridExtra arrangeGrob
#' @importFrom ipred bagging
#' @importFrom leaps regsubsets
#' @importFrom Metrics rmse
#' @importFrom nnet nnet
#' @importFrom parallel makeCluster
#' @importFrom pls pcr
#' @importFrom purrr keep map_dbl
#' @importFrom randomForest randomForest
#' @importFrom reactable reactable
#' @importFrom reactablefmtr add_title
#' @importFrom readr read_lines
#' @importFrom rpart rpart
#' @importFrom stats as.formula BIC cor lm sd predict residuals reorder quantile gaussian var
#' @importFrom tidyr gather pivot_longer
#' @importFrom tree tree cv.tree misclass.tree
#' @importFrom utils tail str head read.csv
#' @importFrom xgboost xgb.DMatrix xgb.train


Numeric <- function(data, colnum, numresamples,
                    remove_VIF_above = 5.00, remove_ensemble_correlations_greater_than = 0.98, scale_all_predictors_in_data = c("Y", "N"),
                    data_reduction_method = c(0("none"), 1("BIC exhaustive"), 2("BIC forward"), 3("BIC backward"), 4("BIC seqrep"),
                                              5("Mallows_cp exhaustive"), 6("Mallows_cp forward"), 7("Mallows_cp backward"), 8("Mallows_cp, seqrep")),
                    ensemble_reduction_method = c(0("none"), 1("BIC exhaustive"), 2("BIC forward"), 3("BIC backward"), 4("BIC seqrep"),
                                                  5("Mallows_cp exhaustive"), 6("Mallows_cp forward"), 7("Mallows_cp backward"), 8("Mallows_cp, seqrep")),
                    how_to_handle_strings = c(0("none"), 1("factor levels"), 2("One-hot encoding"), 3("One-hot encoding with jitter")),
                    predict_on_new_data = c("Y", "N"), save_all_trained_models = c("Y", "N"), save_all_plots = c("Y", "N"),
                    use_parallel = c("Y", "N"),
                    train_amount, test_amount, validation_amount) {

use_parallel <- 0
no_cores <- 0

if (use_parallel == "Y") {
  cl <- parallel::makeCluster(no_cores, type = "FORK")
  doParallel::registerDoParallel(cl)
}

y <- 0
colnames(data)[colnum] <- "y"

df <- data %>% dplyr::relocate(y, .after = last_col()) # Moves the target column to the last column on the right
df <- df[sample(nrow(df)), ]

vif <- car::vif(lm(y ~ ., data = df[, 1:ncol(df)]))
for (i in 1:ncol(df)) {
  if(max(vif) > remove_VIF_above){
    df <- df %>% dplyr::select(-which.max(vif))
    vif <- car::vif(lm(y ~ ., data = df[, 1:ncol(df)]))
  }
}

VIF <- reactable::reactable(as.data.frame(vif),
                            searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                            striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Variance Inflation Factor")


if(data_reduction_method == 1){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "exhaustive")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(BIC == min(BIC)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, y = df$y)
}

if(data_reduction_method == 2){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "forward")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(BIC == min(BIC)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, y = df$y)
}

if(data_reduction_method == 3){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "backward")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(BIC == min(BIC)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, df$y)
}

if(data_reduction_method == 4){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "seqrep")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(BIC == min(BIC)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, df$y)
}

if(data_reduction_method == 5){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "exhaustive")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(mallows_cp == min(mallows_cp)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, df$y)
}

if(data_reduction_method == 6){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "forward")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(mallows_cp == min(mallows_cp)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, df$y)
}

if(data_reduction_method == 7){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "backward")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(mallows_cp == min(mallows_cp)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, y)
}

if(data_reduction_method == 8){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "seqrep")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(mallows_cp == min(mallows_cp)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, y)
}

if (scale_all_predictors_in_data == "Y"){
  df <- as.data.frame(scale(df[, 1:ncol(df) -1]) %>% cbind(y = df$y))
}

if (predict_on_new_data == "Y") {
  new_data <- readline("What is the URL of the new data? ")
  new_data <- read.csv("https://raw.githubusercontent.com/InfiniteCuriosity/EnsemblesData/refs/heads/main/NewBoston.csv")

  y <- 0
  colnames(new_data)[colnum] <- "y"

  new_data <- new_data %>% dplyr::relocate(y, .after = last_col()) # Moves the target column to the last column on the right
}


if (how_to_handle_strings == 1) {
  df <- dplyr::mutate_if(df, is.character, as.factor)
  df <- dplyr::mutate_if(df, is.factor, as.numeric)
}

if (how_to_handle_strings == 1 && predict_on_new_data == "Y") {
  newdata <- dplyr::mutate_if(newdata, is.character, as.factor)
  newdata <- dplyr::mutate_if(newdata, is.factor, as.numeric)
}

if (how_to_handle_strings == 2) {
  dummy <- caret::dummyVars(" ~ .", data=df)
  df <- data.frame(predict(dummy, newdata=df))
}

if (how_to_handle_strings == 2 && predict_on_new_data == "Y") {
  dummy <- caret::dummyVars(" ~ .", data=newdata)
  newdata <- data.frame(predict(dummy, newdata=newdata))
}

if (how_to_handle_strings == 3) {
  dummy <- caret::dummyVars(" ~ .", data=df)
  df <- data.frame(predict(dummy, newdata=df))
  df <- data.frame(lapply(df, jitter))
}

if (how_to_handle_strings == 3 && predict_on_new_data == "Y") {
  dummy <- caret::dummyVars(" ~ .", data=newdata)
  newdata <- data.frame(predict(dummy, newdata=newdata))
  newdata <- data.frame(lapply(newdata, jitter))
}

if(save_all_plots == "Y"){
  width = as.numeric(readline("Width of the graphics: "))
  height = as.numeric(readline("Height of the graphics: "))
  units = readline("Which units? You may use in, cm, mm or px. ")
  scale = as.numeric(readline("What multiplicative scaling factor? "))
  device = readline("Which device to use? You may enter eps, jpeg, pdf, png, svg or tiff: ")
  dpi <- as.numeric(readline("Plot resolution. Applies only to raster output types (jpeg, png, tiff): "))
}

head_df <- reactable::reactable(head(df, n = 10),
                                searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Head of the data frame")

## Set baseline RMSE and Standard Deviation (SD) based on the full data set
actual_RMSE <- Metrics::rmse(actual = df$y, predicted = df$y)
actual_mean <- round(mean(df$y), 4)
actual_sd <- round(sd(df$y), 4)

# Data summary
data_summary <- summary(df)
data_summary <- reactable::reactable(round(as.data.frame(do.call(cbind, lapply(df, summary))), 4),
                                     searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                     striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Data summary")

## Correlation data and plots ##
df1 <- df %>% purrr::keep(is.numeric)
M1 <- stats::cor(df1)
data_correlation <- reactable::reactable(round(cor(df), 4),
                                         searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                         striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Correlation of the data")

title <- "Correlation plot of the numerical data"
corrplot::corrplot(stats::cor(df1), method = "number", title = title, mar = c(0, 0, 1, 0)) # http://stackoverflow.com/a/14754408/54964)

corrplot::corrplot(stats::cor(df1), method = "circle", title = title, mar = c(0, 0, 1, 0)) # http://stackoverflow.com/a/14754408/54964)
tempdir1 <- tempdir()

## Boxplots of the numeric data ##
boxplots <- df %>%
  tidyr::gather(key = "var", value = "value") %>%
  ggplot2::ggplot(aes(x = "", y = value)) +
  ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  ggplot2::facet_wrap(~var, scales = "free") +
  ggplot2::labs(title = "Boxplots of the numeric data")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("boxplots.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("boxplots.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("boxplots.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("boxplots.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("boxplots.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("boxplots.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
# Thanks to https://rstudio-pubs-static.s3.amazonaws.com/388596_e21196f1adf04e0ea7cd68edd9eba966.html

histograms <- ggplot2::ggplot(tidyr::gather(df1, cols, value), aes(x = value)) +
  ggplot2::geom_histogram(bins = round(nrow(df1) / 10)) +
  ggplot2::facet_wrap(. ~ cols, scales = "free") +
  ggplot2::labs(title = "Histograms of each numeric column. Each bar = 10 rows of data")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("histograms.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("histograms.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("histograms.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("histograms.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("histograms.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("histograms.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

predictor_vs_target <- df %>%
  tidyr::gather(-y, key = "var", value = "value") %>%
  ggplot2::ggplot(aes(x = value, y = y)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ var, scales = "free") +
  ggplot2::theme_bw()+
  ggplot2::labs(title = "y (predictor variable) vs target variables")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("predictor_vs_target.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("predictor_vs_target.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("predictor_vs_target.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("predictor_vs_target.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("predictor_vs_target.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("predictor_vs_target.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### Full analysis starts here ####

bag_rf_train_RMSE <- 0
bag_rf_test_RMSE <- 0
bag_rf_validation_RMSE <- 0
bag_rf_sd <- 0
bag_rf_holdout_vs_train <- 0
bag_rf_duration <- 0
bag_rf_holdout_RMSE <- 0
bag_rf_holdout_RMSE_mean <- 0
bag_rf_bias <- 0
bag_rf_MAE <- 0
bag_rf_MSE <- 0
bag_rf_SSE <- 0
bag_rf_ks_stat <- 0
bag_rf_ks_p_value <- 0
bag_rf_ks_p_value_sd <- 0

bagging_train_RMSE <- 0
bagging_test_RMSE <- 0
bagging_validation_RMSE <- 0
bagging_sd <- 0
bagging_holdout_vs_train <- 0
bagging_duration <- 0
bagging_holdout_RMSE <- 0
bagging_holdout_RMSE_mean <- 0
bagging_bias <- 0
bagging_MAE <- 0
bagging_MSE <- 0
bagging_SSE <- 0
bagging_ks_stat <- 0
bagging_ks_p_value <- 0

bayesglm_train_RMSE <- 0
bayesglm_test_RMSE <- 0
bayesglm_validation_RMSE <- 0
bayesglm_sd <- 0
bayesglm_holdout_vs_train <- 0
bayesglm_duration <- 0
bayesglm_duration_mean <- 0
bayesglm_holdout_mean <- 0
bayesglm_holdout_RMSE <- 0
bayesglm_holdout_RMSE_mean <- 0
bayesglm_bias <- 0
bayesglm_MAE <- 0
bayesglm_MSE <- 0
bayesglm_SSE <- 0
bayesglm_ks_stat <- 0
bayesglm_ks_p_value <- 0

bayesrnn_train_RMSE <- 0
bayesrnn_test_RMSE <- 0
bayesrnn_validation_RMSE <- 0
bayesrnn_sd <- 0
bayesrnn_holdout_vs_train <- 0
bayesrnn_duration <- 0
bayesrnn_duration_mean <- 0
bayesrnn_holdout_mean <- 0
bayesrnn_holdout_RMSE <- 0
bayesrnn_holdout_RMSE_mean <- 0
bayesrnn_bias <- 0
bayesrnn_MAE <- 0
bayesrnn_MSE <- 0
bayesrnn_SSE <- 0
bayesrnn_ks_stat <- 0
bayesrnn_ks_p_value <- 0

boost_rf_train_RMSE <- 0
boost_rf_test_RMSE <- 0
boost_rf_validation_RMSE <- 0
boost_rf_sd <- 0
boost_rf_holdout_vs_train <- 0
boost_rf_duration <- 0
boost_rf_duration_mean <- 0
boost_rf_holdout_mean <- 0
boost_rf_holdout_RMSE <- 0
boost_rf_holdout_RMSE_mean <- 0
boost_rf_bias <- 0
boost_rf_MAE <- 0
boost_rf_MSE <- 0
boost_rf_SSE <- 0
boost_rf_ks_stat <- 0
boost_rf_ks_p_value <- 0

cubist_train_RMSE <- 0
cubist_test_RMSE <- 0
cubist_validation_RMSE <- 0
cubist_sd <- 0
cubist_holdout_vs_train <- 0
cubist_duration <- 0
cubist_duration_mean <- 0
cubist_holdout_mean <- 0
cubist_holdout_RMSE <- 0
cubist_holdout_RMSE_mean <- 0
cubist_bias <- 0
cubist_MAE <- 0
cubist_MSE <- 0
cubist_SSE <- 0
cubist_ks_stat <- 0
cubist_ks_p_value <- 0

earth_train_RMSE <- 0
earth_test_RMSE <- 0
earth_validation_RMSE <- 0
earth_sd <- 0
earth_holdout_vs_train <- 0
earth_duration <- 0
earth_duration_mean <- 0
earth_holdout_mean <- 0
earth_holdout_RMSE <- 0
earth_holdout_RMSE_mean <- 0
earth_bias <- 0
earth_MAE <- 0
earth_MSE <- 0
earth_SSE <- 0
earth_ks_stat <- 0
earth_ks_p_value <- 0

elastic_train_RMSE <- 0
elastic_train_RMSE_df <- data.frame(elastic_train_RMSE)
elastic_test_RMSE <- 0
elastic_test_RMSE_df <- data.frame(elastic_test_RMSE)
elastic_test_pred <- 0
elastic_validation_RMSE <- 0
elastic_validation_RMSE_df <- data.frame(elastic_validation_RMSE)
elastic_validation_RMSE_mean <- 0
elastic_validation_predict_value <- 0
elastic_validation_predict_value_mean <- 0
elastic_test_predict_value <- 0
elastic_validation_predict_value <- 0
elastic_validation_predict_value_sd <- 0
elastic_test_predict_value_mean <- 0
elastic_test_predict_value_sd <- 0
elastic_duration <- 0
elastic_holdout_RMSE <- 0
elastic_holdout_RMSE_df <- data.frame(elastic_holdout_RMSE)
elastic_holdout_RMSE_sd <- 0
elastic_holdout_RMSE_sd_mean <- 0
elastic_holdout_RMSE_sd_df <- data.frame(elastic_holdout_RMSE_sd)
elastic_holdout_vs_train <- 0
elastic_holdout_vs_train_df <- data.frame(elastic_holdout_vs_train)
elastic_sd <- 0
elastic_sd_mean <- 0
y_hat_elastic <- 0
elastic_bias <- 0
elastic_MAE <- 0
elastic_MSE <- 0
elastic_SSE <- 0
elastic_ks_stat <- 0
elastic_ks_p_value <- 0

gam_train_RMSE <- 0
gam_test_RMSE <- 0
gam_validation_RMSE <- 0
gam_sd <- 0
gam_holdout_vs_train <- 0
gam_duration <- 0
gam_duration_mean <- 0
gam_holdout_mean <- 0
gam_holdout_RMSE <- 0
gam_holdout_RMSE_mean <- 0
gam_bias <- 0
gam_MAE <- 0
gam_MSE <- 0
gam_SSE <- 0
gam_ks_stat <- 0
gam_ks_p_value <- 0

gb_train_RMSE <- 0
gb_test_RMSE <- 0
gb_validation_RMSE <- 0
gb_sd <- 0
gb_holdout_vs_train <- 0
gb_duration <- 0
gb_duration_mean <- 0
gb_holdout_mean <- 0
gb_holdout_RMSE <- 0
gb_holdout_RMSE_mean <- 0
gb_bias <- 0
gb_MAE <- 0
gb_MSE <- 0
gb_SSE <- 0
gb_ks_stat <- 0
gb_ks_p_value <- 0

knn_train_RMSE <- 0
knn_test_RMSE <- 0
knn_validation_RMSE <- 0
knn_sd <- 0
knn_holdout_vs_train <- 0
knn_duration <- 0
knn_duration_mean <- 0
knn_holdout_mean <- 0
knn_holdout_RMSE <- 0
knn_holdout_RMSE_mean <- 0
knn_bias <- 0
knn_MAE <- 0
knn_MSE <- 0
knn_SSE <- 0
knn_ks_stat <- 0
knn_ks_p_value <- 0

lasso_train_RMSE <- 0
lasso_train_RMSE_df <- data.frame(lasso_train_RMSE)
lasso_test_RMSE <- 0
lasso_test_RMSE_df <- data.frame(lasso_test_RMSE)
lasso_test_pred <- 0
lasso_validation_RMSE <- 0
lasso_validation_RMSE_df <- data.frame(lasso_validation_RMSE)
lasso_validation_RMSE_mean <- 0
lasso_validation_predict_value <- 0
lasso_validation_predict_value_mean <- 0
lasso_test_predict_value <- 0
lasso_validation_predict_value <- 0
lasso_validation_predict_value_sd <- 0
lasso_test_predict_value_mean <- 0
lasso_test_predict_value_sd <- 0
lasso_duration <- 0
lasso_holdout_RMSE <- 0
lasso_holdout_RMSE_df <- data.frame(lasso_holdout_RMSE)
lasso_holdout_RMSE_sd <- 0
lasso_holdout_RMSE_sd_mean <- 0
lasso_holdout_RMSE_sd_df <- data.frame(lasso_holdout_RMSE_sd)
lasso_holdout_vs_train <- 0
lasso_holdout_vs_train_df <- data.frame(lasso_holdout_vs_train)
lasso_sd <- 0
lasso_sd_mean <- 0
y_hat_lasso <- 0
lasso_bias <- 0
lasso_MAE <- 0
lasso_MSE <- 0
lasso_SSE <- 0
lasso_ks_stat <- 0
lasso_ks_p_value <- 0

linear_train_RMSE <- 0
linear_test_RMSE <- 0
linear_validation_RMSE <- 0
linear_sd <- 0
linear_holdout_vs_train <- 0
linear_duration <- 0
linear_holdout_RMSE <- 0
linear_holdout_RMSE_mean <- 0
linear_bias <- 0
linear_MAE <- 0
linear_MSE <- 0
linear_SSE <- 0
linear_ks_stat <- 0
linear_ks_p_value <- 0

neuralnet_train_RMSE <- 0
neuralnet_test_RMSE <- 0
neuralnet_validation_RMSE <- 0
neuralnet_sd <- 0
neuralnet_holdout_vs_train <- 0
neuralnet_duration <- 0
neuralnet_holdout_RMSE <- 0
neuralnet_holdout_RMSE_mean <- 0
neuralnet_bias <- 0
neuralnet_MAE <- 0
neuralnet_MSE <- 0
neuralnet_SSE <- 0
neuralnet_ks_stat <- 0
neuralnet_ks_p_value <- 0
neuralnet_test_predict_value_mean <- 0

pls_train_RMSE <- 0
pls_test_RMSE <- 0
pls_validation_RMSE <- 0
pls_sd <- 0
pls_holdout_vs_train <- 0
pls_duration <- 0
pls_duration_mean <- 0
pls_holdout_mean <- 0
pls_holdout_RMSE <- 0
pls_holdout_RMSE_mean <- 0
pls_bias <- 0
pls_MAE <- 0
pls_MSE <- 0
pls_SSE <- 0
pls_ks_stat <- 0
pls_ks_p_value <- 0

pcr_train_RMSE <- 0
pcr_test_RMSE <- 0
pcr_validation_RMSE <- 0
pcr_sd <- 0
pcr_holdout_vs_train <- 0
pcr_duration <- 0
pcr_duration_mean <- 0
pcr_holdout_mean <- 0
pcr_holdout_RMSE <- 0
pcr_holdout_RMSE_mean <- 0
pcr_bias <- 0
pcr_MAE <- 0
pcr_MSE <- 0
pcr_SSE <- 0
pcr_ks_stat <- 0
pcr_ks_p_value <- 0

rf_train_RMSE <- 0
rf_test_RMSE <- 0
rf_validation_RMSE <- 0
rf_sd <- 0
rf_holdout_vs_train <- 0
rf_duration <- 0
rf_duration_mean <- 0
rf_holdout_mean <- 0
rf_holdout_RMSE <- 0
rf_holdout_RMSE_mean <- 0
rf_bias <- 0
rf_MAE <- 0
rf_MSE <- 0
rf_SSE <- 0
rf_ks_stat <- 0
rf_ks_p_value <- 0

ridge_train_RMSE <- 0
ridge_train_RMSE_df <- data.frame(ridge_train_RMSE)
ridge_test_RMSE <- 0
ridge_test_RMSE_df <- data.frame(ridge_test_RMSE)
ridge_test_pred <- 0
ridge_validation_RMSE <- 0
ridge_validation_RMSE_df <- data.frame(ridge_validation_RMSE)
ridge_validation_RMSE_mean <- 0
ridge_validation_predict_value <- 0
ridge_validation_predict_value_mean <- 0
ridge_test_predict_value <- 0
ridge_validation_predict_value <- 0
ridge_validation_predict_value_sd <- 0
ridge_test_predict_value_mean <- 0
ridge_test_predict_value_sd <- 0
ridge_duration <- 0
ridge_holdout_RMSE <- 0
ridge_holdout_RMSE_df <- data.frame(ridge_holdout_RMSE)
ridge_holdout_RMSE_sd <- 0
ridge_holdout_RMSE_sd_df <- data.frame(ridge_holdout_RMSE_sd)
ridge_holdout_RMSE_sd_mean <- 0
ridge_holdout_vs_train <- 0
ridge_holdout_vs_train_df <- data.frame(ridge_holdout_vs_train)
ridge_sd <- 0
ridge_sd_mean <- 0
ridge_bias <- 0
ridge_MAE <- 0
ridge_MSE <- 0
ridge_SSE <- 0
y_hat_ridge <- 0
ridge_ks_stat <- 0
ridge_ks_p_value <- 0

rpart_train_RMSE <- 0
rpart_test_RMSE <- 0
rpart_validation_RMSE <- 0
rpart_sd <- 0
rpart_holdout_vs_train <- 0
rpart_duration <- 0
rpart_duration_mean <- 0
rpart_holdout_mean <- 0
rpart_holdout_RMSE <- 0
rpart_holdout_RMSE_mean <- 0
rpart_bias <- 0
rpart_MAE <- 0
rpart_MSE <- 0
rpart_SSE <- 0
rpart_ks_stat <- 0
rpart_ks_p_value <- 0

svm_train_RMSE <- 0
svm_test_RMSE <- 0
svm_validation_RMSE <- 0
svm_sd <- 0
svm_holdout_vs_train <- 0
svm_duration <- 0
svm_duration_mean <- 0
svm_holdout_mean <- 0
svm_holdout_RMSE <- 0
svm_holdout_RMSE_mean <- 0
svm_bias <- 0
svm_MAE <- 0
svm_MSE <- 0
svm_SSE <- 0
svm_ks_stat <- 0
svm_ks_p_value <- 0

tree_train_RMSE <- 0
tree_test_RMSE <- 0
tree_validation_RMSE <- 0
tree_sd <- 0
tree_holdout_vs_train <- 0
tree_duration <- 0
tree_duration_mean <- 0
tree_holdout_mean <- 0
tree_holdout_RMSE <- 0
tree_holdout_RMSE_mean <- 0
tree_bias <- 0
tree_MAE <- 0
tree_MSE <- 0
tree_SSE <- 0
tree_ks_stat <- 0
tree_ks_p_value <- 0

xgb_train_RMSE <- 0
xgb_test_RMSE <- 0
xgb_validation_RMSE <- 0
xgb_sd <- 0
xgb_holdout_vs_train <- 0
xgb_duration <- 0
xgb_duration_mean <- 0
xgb_holdout_mean <- 0
xgb_holdout_RMSE <- 0
xgb_holdout_RMSE_mean <- 0
xgb_bias <- 0
xgb_MAE <- 0
xgb_MSE <- 0
xgb_SSE <- 0
xgb_ks_stat <- 0
xgb_ks_p_value <- 0

ensemble_bag_rf_train_RMSE <- 0
ensemble_bag_rf_test_RMSE <- 0
ensemble_bag_rf_validation_RMSE <- 0
ensemble_bag_rf_sd <- 0
ensemble_bag_rf_holdout_vs_train <- 0
ensemble_bag_rf_duration <- 0
ensemble_bag_rf_holdout_RMSE <- 0
ensemble_bag_rf_holdout_RMSE_mean <- 0
ensemble_bag_rf_predict_value_mean <- 0
ensemble_bag_rf_bias <- 0
ensemble_bag_rf_MAE <- 0
ensemble_bag_rf_MSE <- 0
ensemble_bag_rf_SSE <- 0
ensemble_bag_rf_ks_stat <- 0
ensemble_bag_rf_ks_p_value <- 0

ensemble_bagging_train_RMSE <- 0
ensemble_bagging_test_RMSE <- 0
ensemble_bagging_validation_RMSE <- 0
ensemble_bagging_sd <- 0
ensemble_bagging_holdout_vs_train <- 0
ensemble_bagging_duration <- 0
ensemble_bagging_holdout_RMSE <- 0
ensemble_bagging_holdout_RMSE_mean <- 0
ensemble_bagging_predict_value_mean <- 0
ensemble_bagging_bias <- 0
ensemble_bagging_MAE <- 0
ensemble_bagging_MSE <- 0
ensemble_bagging_SSE <- 0
ensemble_bagging_ks_stat <- 0
ensemble_bagging_ks_p_value <- 0

ensemble_bayesglm_train_RMSE <- 0
ensemble_bayesglm_test_RMSE <- 0
ensemble_bayesglm_validation_RMSE <- 0
ensemble_bayesglm_sd <- 0
ensemble_bayesglm_holdout_vs_train <- 0
ensemble_bayesglm_duration <- 0
ensemble_bayesglm_holdout_RMSE <- 0
ensemble_bayesglm_holdout_RMSE_mean <- 0
ensemble_bayesglm_predict_value_mean <- 0
ensemble_bayesglm_bias <- 0
ensemble_bayesglm_MAE <- 0
ensemble_bayesglm_MSE <- 0
ensemble_bayesglm_SSE <- 0
ensemble_bayesglm_ks_stat <- 0
ensemble_bayesglm_ks_p_value <- 0

ensemble_bayesrnn_train_RMSE <- 0
ensemble_bayesrnn_test_RMSE <- 0
ensemble_bayesrnn_validation_RMSE <- 0
ensemble_bayesrnn_sd <- 0
ensemble_bayesrnn_holdout_vs_train <- 0
ensemble_bayesrnn_duration <- 0
ensemble_bayesrnn_holdout_RMSE <- 0
ensemble_bayesrnn_holdout_RMSE_mean <- 0
ensemble_bayesrnn_predict_value_mean <- 0
ensemble_bayesrnn_bias <- 0
ensemble_bayesrnn_MAE <- 0
ensemble_bayesrnn_MSE <- 0
ensemble_bayesrnn_SSE <- 0
ensemble_bayesrnn_ks_stat <- 0
ensemble_bayesrnn_ks_p_value <- 0

ensemble_boost_rf_train_RMSE <- 0
ensemble_boost_rf_test_RMSE <- 0
ensemble_boost_rf_validation_RMSE <- 0
ensemble_boost_rf_sd <- 0
ensemble_boost_rf_holdout_vs_train <- 0
ensemble_boost_rf_duration <- 0
ensemble_boost_rf_holdout_RMSE <- 0
ensemble_boost_rf_holdout_RMSE_mean <- 0
ensemble_boost_rf_predict_value_mean <- 0
ensemble_boost_rf_bias <- 0
ensemble_boost_rf_MAE <- 0
ensemble_boost_rf_MSE <- 0
ensemble_boost_rf_SSE <- 0
ensemble_boost_rf_ks_stat <- 0
ensemble_boost_rf_ks_p_value <- 0

ensemble_cubist_train_RMSE <- 0
ensemble_cubist_test_RMSE <- 0
ensemble_cubist_validation_RMSE <- 0
ensemble_cubist_sd <- 0
ensemble_cubist_holdout_vs_train <- 0
ensemble_cubist_duration <- 0
ensemble_cubist_holdout_RMSE <- 0
ensemble_cubist_holdout_RMSE_mean <- 0
ensemble_cubist_predict_value_mean <- 0
ensemble_cubist_bias <- 0
ensemble_cubist_MAE <- 0
ensemble_cubist_MSE <- 0
ensemble_cubist_SSE <- 0
ensemble_cubist_ks_stat <- 0
ensemble_cubist_ks_p_value <- 0

ensemble_earth_train_RMSE <- 0
ensemble_earth_test_RMSE <- 0
ensemble_earth_validation_RMSE <- 0
ensemble_earth_sd <- 0
ensemble_earth_holdout_vs_train <- 0
ensemble_earth_duration <- 0
ensemble_earth_holdout_RMSE <- 0
ensemble_earth_holdout_RMSE_mean <- 0
ensemble_earth_predict_value_mean <- 0
ensemble_earth_bias <- 0
ensemble_earth_MAE <- 0
ensemble_earth_MSE <- 0
ensemble_earth_SSE <- 0
ensemble_earth_ks_stat <- 0
ensemble_earth_ks_p_value <- 0

ensemble_elastic_test_RMSE <- 0
ensemble_elastic_test_RMSE_df <- data.frame(ensemble_elastic_test_RMSE)
ensemble_elastic_validation_RMSE <- 0
ensemble_elastic_validation_RMSE_df <- data.frame(ensemble_elastic_validation_RMSE)
ensemble_elastic_validation_RMSE_mean <- 0
ensemble_elastic_validation_predict_value <- 0
ensemble_elastic_validation_predict_value_mean <- 0
ensemble_elastic_validation_predict_value_df <- data.frame(ensemble_elastic_validation_predict_value)
ensemble_elastic_test_predict_value <- 0
ensemble_elastic_test_predict_value_df <- data.frame(ensemble_elastic_test_predict_value)
ensemble_elastic_validation_predict_value <- 0
ensemble_elastic_test_predict_value_mean <- 0
ensemble_elastic_test_predict_value_df <- data.frame(ensemble_elastic_test_predict_value)
ensemble_elastic_test_predict_value_sd <- 0
ensemble_y_hat_elastic <- 0
ensemble_elastic_sd <- 0
ensemble_elastic_sd_df <- data.frame(ensemble_elastic_sd)
ensemble_elastic_sd_mean <- 0
ensemble_elastic_validation_predict_value_sd <- 0
ensemble_lasso_validation_predict_value_sd <- 0
ensemble_elastic_validation_sd <- 0
ensemble_elastic_validation_sd_df <- data.frame(ensemble_elastic_validation_sd)
ensemble_elastic_validation_predict_value_sd_df <- data.frame(ensemble_elastic_validation_predict_value_sd)
ensemble_elastic_test_sd <- 0
ensemble_elastic_test_sd_df <- data.frame(ensemble_elastic_test_sd)
ensemble_elastic_validation_sd <- 0
y_hat_ensemble_elastic <- 0
ensemble_elastic_train_RMSE <- 0
ensemble_elastic_train_RMSE_df <- data.frame(ensemble_elastic_train_RMSE)
ensemble_elastic_train_RMSE_mean <- 0
ensemble_elastic_holdout_RMSE <- 0
ensemble_elastic_holdout_RMSE_mean <- 0
ensemble_elastic_holdout_RMSE_sd_mean <- 0
ensemble_elastic_holdout_vs_train <- 0
ensemble_elastic_holdout_vs_train_df <- data.frame(ensemble_elastic_holdout_vs_train)
ensemble_elastic_holdout_vs_train_mean <- 0
ensemble_elastic_predict_value_mean <- 0
ensemble_elastic_duration <- 0
ensemble_elastic_duration_mean <- 0
ensemble_elastic_bias <- 0
ensemble_elastic_MAE <- 0
ensemble_elastic_MSE <- 0
ensemble_elastic_SSE <- 0
ensemble_elastic_ks_stat <- 0
ensemble_elastic_ks_p_value <- 0

ensemble_gb_train_RMSE <- 0
ensemble_gb_test_RMSE <- 0
ensemble_gb_validation_RMSE <- 0
ensemble_gb_sd <- 0
ensemble_gb_holdout_vs_train <- 0
ensemble_gb_duration <- 0
ensemble_gb_holdout_RMSE <- 0
ensemble_gb_holdout_RMSE_mean <- 0
ensemble_gb_predict_value_mean <- 0
ensemble_gb_bias <- 0
ensemble_gb_MAE <- 0
ensemble_gb_MSE <- 0
ensemble_gb_SSE <- 0
ensemble_gb_ks_stat <- 0
ensemble_gb_ks_p_value <- 0

ensemble_knn_test_RMSE <- 0
ensemble_knn_test_RMSE_df <- data.frame(ensemble_knn_test_RMSE)
ensemble_knn_test_RMSE_mean <- 0
ensemble_knn_test_mean <- 0
ensemble_knn_test_mean_df <- data.frame(ensemble_knn_test_mean)
ensemble_knn_validation_RMSE <- 0
ensemble_knn_validation_RMSE_df <- data.frame(ensemble_knn_validation_RMSE)
ensemble_knn_validation_RMSE_mean <- 0
ensemble_knn_RMSE <- 0
ensemble_knn_RMSE_mean <- 0
ensemble_knn_test_sd <- 0
ensemble_y_hat_knn <- 0
ensemble_knn_test_sd <- 0
ensemble_knn_test_sd_df <- data.frame(ensemble_knn_test_sd)
ensemble_knn_validation_sd <- 0
ensemble_knn_validation_sd_df <- data.frame(ensemble_knn_validation_sd)
ensemble_knn_sd_test_df <- data.frame(ensemble_knn_test_sd)
ensemble_knn_test <- 0
ensemble_knn_test_predict_value <- 0
ensemble_knn_train_RMSE <- 0
ensemble_knn_train_RMSE_df <- data.frame(ensemble_knn_train_RMSE)
ensemble_knn_train_RMSE_mean <- 0
ensemble_knn_holdout_vs_train <- 0
ensemble_knn_holdout_vs_train_df <- data.frame(ensemble_knn_holdout_vs_train)
ensemble_knn_holdout_vs_train_mean <- 0
ensemble_knn_duration <- 0
ensemble_knn_holdout_RMSE <- 0
ensemble_knn_holdout_RMSE_mean <- 0
ensemble_knn_holdout_RMSE_sd_mean <- 0
ensemble_knn_sd <- 0
ensemble_knn_sd_mean <- 0
ensemble_knn_bias <- 0
ensemble_knn_MAE <- 0
ensemble_knn_MSE <- 0
ensemble_knn_SSE <- 0
ensemble_knn_ks_stat <- 0
ensemble_knn_ks_p_value <- 0

ensemble_lasso_test_RMSE <- 0
ensemble_lasso_test_RMSE_df <- data.frame(ensemble_lasso_test_RMSE)
ensemble_lasso_validation_RMSE <- 0
ensemble_lasso_validation_RMSE_df <- data.frame(ensemble_lasso_validation_RMSE)
ensemble_lasso_validation_RMSE_mean <- 0
ensemble_lasso_validation_predict_value <- 0
ensemble_lasso_validation_predict_value_mean <- 0
ensemble_lasso_validation_predict_value_df <- data.frame(ensemble_lasso_validation_predict_value)
ensemble_lasso_test_predict_value <- 0
ensemble_lasso_test_predict_value_df <- data.frame(ensemble_lasso_test_predict_value)
ensemble_lasso_validation_predict_value <- 0
ensemble_lasso_test_predict_value_mean <- 0
ensemble_lasso_test_predict_value_df <- data.frame(ensemble_lasso_test_predict_value)
ensemble_lasso_test_predict_value_sd <- 0
ensemble_y_hat_lasso <- 0
ensemble_lasso_sd <- 0
ensemble_lasso_sd_df <- data.frame(ensemble_lasso_sd)
ensemble_lasso_sd_mean <- 0
ensemble_lasso_validation_predict_value_sd <- 0
ensemble_lasso_validation_predict_value_sd <- 0
ensemble_lasso_validation_predict_value_sd_df <- data.frame(ensemble_lasso_validation_predict_value_sd)
ensemble_lasso_test_sd <- 0
ensemble_lasso_test_sd_df <- data.frame(ensemble_lasso_test_sd)
ensemble_lasso_validation_sd <- 0
ensemble_lasso_validation_sd_df <- data.frame(ensemble_lasso_validation_sd)
y_hat_ensemble_lasso <- 0
ensemble_lasso_train_RMSE <- 0
ensemble_lasso_train_RMSE_df <- data.frame(ensemble_lasso_train_RMSE)
ensemble_lasso_train_RMSE_mean <- 0
ensemble_lasso_holdout_RMSE <- 0
ensemble_lasso_holdout_RMSE_mean <- 0
ensemble_lasso_holdout_RMSE_sd_mean <- 0
ensemble_lasso_holdout_vs_train <- 0
ensemble_lasso_holdout_vs_train_df <- data.frame(ensemble_lasso_holdout_vs_train)
ensemble_lasso_holdout_vs_train_mean <- 0
ensemble_lasso_predict_value_mean <- 0
ensemble_lasso_duration <- 0
ensemble_lasso_duration_mean <- 0
ensemble_lasso_bias <- 0
ensemble_lasso_MAE <- 0
ensemble_lasso_MSE <- 0
ensemble_lasso_SSE <- 0
ensemble_lasso_ks_stat <- 0
ensemble_lasso_ks_p_value <- 0

ensemble_linear_train_RMSE <- 0
ensemble_linear_test_RMSE <- 0
ensemble_linear_validation_RMSE <- 0
ensemble_linear_sd <- 0
ensemble_linear_holdout_vs_train <- 0
ensemble_linear_duration <- 0
ensemble_linear_holdout_RMSE <- 0
ensemble_linear_holdout_RMSE_mean <- 0
ensemble_linear_predict_value_mean <- 0
ensemble_linear_bias <- 0
ensemble_linear_MAE <- 0
ensemble_linear_MSE <- 0
ensemble_linear_SSE <- 0
ensemble_linear_ks_stat <- 0
ensemble_linear_ks_p_value <- 0

ensemble_rf_train_RMSE <- 0
ensemble_rf_test_RMSE <- 0
ensemble_rf_validation_RMSE <- 0
ensemble_rf_sd <- 0
ensemble_rf_holdout_vs_train <- 0
ensemble_rf_duration <- 0
ensemble_rf_holdout_RMSE <- 0
ensemble_rf_holdout_RMSE_mean <- 0
ensemble_rf_predict_value_mean <- 0
ensemble_rf_bias <- 0
ensemble_rf_MAE <- 0
ensemble_rf_MSE <- 0
ensemble_rf_SSE <- 0
ensemble_rf_ks_stat <- 0
ensemble_rf_ks_p_value <- 0

ensemble_ridge_test_RMSE <- 0
ensemble_ridge_test_RMSE_df <- data.frame(ensemble_ridge_test_RMSE)
ensemble_ridge_validation_RMSE <- 0
ensemble_ridge_validation_RMSE_df <- data.frame(ensemble_ridge_validation_RMSE)
ensemble_ridge_validation_RMSE_mean <- 0
ensemble_ridge_validation_predict_value <- 0
ensemble_ridge_validation_predict_value_mean <- 0
ensemble_ridge_validation_predict_value_df <- data.frame(ensemble_ridge_validation_predict_value)
ensemble_ridge_test_predict_value <- 0
ensemble_ridge_test_predict_value_df <- data.frame(ensemble_ridge_test_predict_value)
ensemble_ridge_validation_predict_value <- 0
ensemble_ridge_test_predict_value_mean <- 0
ensemble_ridge_test_predict_value_df <- data.frame(ensemble_ridge_test_predict_value)
ensemble_ridge_test_predict_value_sd <- 0
ensemble_y_hat_ridge <- 0
ensemble_ridge_sd <- 0
ensemble_ridge_sd_df <- data.frame(ensemble_ridge_sd)
ensemble_ridge_sd_mean <- 0
ensemble_ridge_validation_predict_value_sd <- 0
ensemble_lasso_validation_predict_value_sd <- 0
ensemble_ridge_validation_predict_value_sd_df <- data.frame(ensemble_ridge_validation_predict_value_sd)
ensemble_ridge_test_sd <- 0
ensemble_ridge_test_sd_df <- data.frame(ensemble_ridge_test_sd)
ensemble_ridge_validation_sd <- 0
ensemble_ridge_validation_sd_mean <- 0
ensemble_ridge_validation_sd_df <- data.frame(ensemble_ridge_validation_sd)
y_hat_ensemble_ridge <- 0
ensemble_ridge_train_RMSE <- 0
ensemble_ridge_train_RMSE_df <- data.frame(ensemble_ridge_train_RMSE)
ensemble_ridge_train_RMSE_mean <- 0
ensemble_ridge_holdout_RMSE <- 0
ensemble_ridge_holdout_RMSE_mean <- 0
ensemble_ridge_holdout_RMSE_sd_mean <- 0
ensemble_ridge_holdout_vs_train <- 0
ensemble_ridge_holdout_vs_train_df <- data.frame(ensemble_ridge_holdout_vs_train)
ensemble_ridge_holdout_vs_train_mean <- 0
ensemble_ridge_predict_value_mean <- 0
ensemble_ridge_duration <- 0
ensemble_ridge_duration_mean <- 0
ensemble_ridge_bias <- 0
ensemble_ridge_MAE <- 0
ensemble_ridge_MSE <- 0
ensemble_ridge_SSE <- 0
ensemble_ridge_ks_stat <- 0
ensemble_ridge_ks_p_value <- 0

ensemble_rpart_train_RMSE <- 0
ensemble_rpart_test_RMSE <- 0
ensemble_rpart_validation_RMSE <- 0
ensemble_rpart_sd <- 0
ensemble_rpart_holdout_vs_train <- 0
ensemble_rpart_duration <- 0
ensemble_rpart_holdout_RMSE <- 0
ensemble_rpart_holdout_RMSE_mean <- 0
ensemble_rpart_predict_value_mean <- 0
ensemble_rpart_train_RMSE <- 0
ensemble_rpart_test_RMSE <- 0
ensemble_rpart_validation_RMSE <- 0
ensemble_rpart_sd <- 0
ensemble_rpart_holdout_vs_train <- 0
ensemble_rpart_duration <- 0
ensemble_rpart_holdout_RMSE <- 0
ensemble_rpart_holdout_RMSE_mean <- 0
ensemble_rpart_predict_value_mean <- 0
ensemble_rpart_bias <- 0
ensemble_rpart_MAE <- 0
ensemble_rpart_MSE <- 0
ensemble_rpart_SSE <- 0
ensemble_rpart_ks_stat <- 0
ensemble_rpart_ks_p_value <- 0

ensemble_svm_train_RMSE <- 0
ensemble_svm_test_RMSE <- 0
ensemble_svm_validation_RMSE <- 0
ensemble_svm_sd <- 0
ensemble_svm_holdout_vs_train <- 0
ensemble_svm_duration <- 0
ensemble_svm_holdout_RMSE <- 0
ensemble_svm_holdout_RMSE_mean <- 0
ensemble_svm_predict_value_mean <- 0
ensemble_svm_bias <- 0
ensemble_svm_MAE <- 0
ensemble_svm_MSE <- 0
ensemble_svm_SSE <- 0
ensemble_svm_bias <- 0
ensemble_svm_MAE <- 0
ensemble_svm_MSE <- 0
ensemble_svm_SSE <- 0
ensemble_svm_ks_stat <- 0
ensemble_svm_ks_p_value <- 0

ensemble_tree_train_RMSE <- 0
ensemble_tree_test_RMSE <- 0
ensemble_tree_validation_RMSE <- 0
ensemble_tree_sd <- 0
ensemble_tree_holdout_vs_train <- 0
ensemble_tree_duration <- 0
ensemble_tree_holdout_RMSE <- 0
ensemble_tree_holdout_RMSE_mean <- 0
ensemble_tree_predict_value_mean <- 0
ensemble_tree_bias <- 0
ensemble_tree_MAE <- 0
ensemble_tree_MSE <- 0
ensemble_tree_SSE <- 0
ensemble_tree_ks_stat <- 0
ensemble_tree_ks_p_value <- 0

ensemble_xgb_test_RMSE <- 0
ensemble_xgb_test_RMSE_df <- data.frame(ensemble_xgb_test_RMSE)
ensemble_xgb_test_RMSE_mean <- 0
ensemble_xgb_test_mean <- 0
ensemble_xgb_test_mean_df <- data.frame(ensemble_xgb_test_mean)
ensemble_xgb_validation_RMSE <- 0
ensemble_xgb_validation_RMSE_df <- data.frame(ensemble_xgb_validation_RMSE)
ensemble_xgb_validation_RMSE_mean <- 0
ensemble_xgb_test_sd <- 0
ensemble_y_hat_xgb <- 0
ensemble_xgb_test_sd <- 0
ensemble_xgb_test_sd_df <- data.frame(ensemble_xgb_test_sd)
ensemble_xgb_train_RMSE <- 0
ensemble_xgb_train_RMSE_df <- data.frame(ensemble_xgb_train_RMSE)
ensemble_xgb_train_RMSE_mean <- 0
ensemble_xgb_holdout_vs_train <- 0
ensemble_xgb_holdout_vs_train_df <- data.frame(ensemble_xgb_holdout_vs_train)
ensemble_xgb_holdout_vs_train_mean <- 0
ensemble_xgb_duration <- 0
ensemble_xgb_duration_mean <- 0
ensemble_xgb_holdout_RMSE <- 0
ensemble_xgb_holdout_RMSE_mean <- 0
ensemble_xgb_holdout_RMSE_sd_mean <- 0
ensemble_xgb_predict_value_mean <- 0
ensemble_xgb_sd_mean <- 0
ensemble_xgb_bias <- 0
ensemble_xgb_MAE <- 0
ensemble_xgb_MSE <- 0
ensemble_xgb_SSE <- 0
ensemble_xgb_ks_stat <- 0
ensemble_xgb_ks_p_value <- 0

actual <- 0
cols <- 0
actual <- 0
predicted <- 0
value <- 0
grid.arrange <- 0
holdout <- 0
Mean_holdout_RMSE <- 0
count <- 0
model <- 0
mallows_cp <- 0
holdout_vs_train <- 0
Duration <- 0
Model <- 0
holdout_vs_train_mean <- 0
holdout_vs_train_sd <- 0
Mean_Bias <- 0
Mean_MAE <- 0
Mean_MAE_sd <- 0
Mean_MSE <- 0
Mean_MSE_sd <- 0
Mean_SSE <- 0
Mean_SSE_sd <- 0

outliers_df <- data.frame()
Std_Deviation_of_holdout_RMSE <- 0
holdout_vs_train_sd <- 0
Bias <- 0

Mean_MAE_sd <- 0
Mean_MSE_sd <- 0
Mean_SSE_sd <- 0
Duration_sd <- 0
KS_Test_P_Value_mean <- 0
KS_Test_P_Value_std_dev <- 0


for (i in 1:numresamples) {
  message(noquote(""))
  message(paste0("Resampling number ", i, " of ", numresamples, sep = ','))
  message(noquote(""))

  idx <- sample(seq(1, 3), size = nrow(df), replace = TRUE, prob = c(train_amount, test_amount, validation_amount))
  train <- df[idx == 1, ]
  test <- df[idx == 2, ]
  validation <- df[idx == 3, ]

  ####  Model #1 Bagged Random Forest tuned ####
  bag_rf_start <- Sys.time()
  bag_rf_train_fit <- e1071::tune.randomForest(x = train, y = train$y, mtry = ncol(train) - 1)
  bag_rf_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(
    object = bag_rf_train_fit$best.model,
    newdata = train
  ))
  bag_rf_train_RMSE_mean <- mean(bag_rf_train_RMSE)
  bag_rf_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(
    object = bag_rf_train_fit$best.model,
    newdata = test
  ))
  bag_rf_test_RMSE_mean <- mean(bag_rf_test_RMSE)
  bag_rf_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(
    object = bag_rf_train_fit$best.model,
    newdata = validation
  ))
  bag_rf_validation_RMSE_mean <- mean(bag_rf_validation_RMSE)
  bag_rf_holdout_RMSE[i] <- mean(bag_rf_test_RMSE_mean, bag_rf_validation_RMSE_mean)
  bag_rf_holdout_RMSE_mean <- mean(c(bag_rf_holdout_RMSE))
  bag_rf_holdout_RMSE_sd_mean <- sd(c(bag_rf_test_RMSE_mean, bag_rf_validation_RMSE_mean))
  bag_rf_train_predict_value <- as.numeric(predict(object = bag_rf_train_fit$best.model, newdata = train))
  bag_rf_test_predict_value <- as.numeric(predict(object = bag_rf_train_fit$best.model, newdata = test))
  bag_rf_validation_predict_value <- as.numeric(predict(object = bag_rf_train_fit$best.model, newdata = validation))
  bag_rf_predict_value_mean <- mean(c(bag_rf_test_predict_value, bag_rf_validation_predict_value))
  bag_rf_sd[i] <- sd(c(bag_rf_test_predict_value, bag_rf_validation_predict_value))
  bag_rf_sd_mean <- mean(bag_rf_sd)
  bag_rf_holdout_vs_train[i] <- bag_rf_holdout_RMSE_mean / bag_rf_train_RMSE_mean
  bag_rf_holdout_vs_train_mean <- mean(bag_rf_holdout_vs_train)
  bag_rf_holdout_vs_train_range <- range(bag_rf_holdout_vs_train)
  bag_rf_holdout_vs_train_sd <- sd(bag_rf_holdout_vs_train)
  bag_rf_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(bag_rf_test_predict_value, bag_rf_validation_predict_value))
  bag_rf_bias_mean <- mean(bag_rf_bias)
  bag_rf_bias_sd <- sd(bag_rf_bias)
  bag_rf_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(bag_rf_test_predict_value, bag_rf_validation_predict_value))
  bag_rf_MAE_mean <- mean(bag_rf_MAE)
  bag_rf_MAE_sd <- sd(bag_rf_MAE)
  bag_rf_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(bag_rf_test_predict_value, bag_rf_validation_predict_value))
  bag_rf_MSE_mean <- mean(bag_rf_MSE)
  bag_rf_MSE_sd <- sd(bag_rf_MSE)
  bag_rf_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(bag_rf_test_predict_value, bag_rf_validation_predict_value))
  bag_rf_SSE_mean <- mean(bag_rf_SSE)
  bag_rf_SSE_sd <- sd(bag_rf_SSE)
  y_hat_bag_rf <- c(bag_rf_test_predict_value, bag_rf_validation_predict_value)
  bag_rf_ks_p_value[i] <- stats::ks.test(x = y_hat_bag_rf, y = c(train$y, validation$y), exact = TRUE)$p.value
  bag_rf_ks_p_value_mean <- mean(bag_rf_ks_p_value)
  bag_rf_ks_p_value_sd <- sd(bag_rf_ks_p_value)
  bag_rf_ks_stat[i] <- stats::ks.test(x = y_hat_bag_rf, y = c(train$y, validation$y), exact = TRUE)$statistic
  bag_rf_ks_stat_mean <- mean(bag_rf_ks_stat)
  bag_rf_ks_test <- c(bag_rf_ks_stat_mean, bag_rf_ks_p_value_mean)

  bag_rf_end <- Sys.time()
  bag_rf_duration[i] <- bag_rf_end - bag_rf_start
  bag_rf_duration_mean <- mean(bag_rf_duration)
  bag_rf_duration_sd <- sd(bag_rf_duration)

  ####  Model #2 Bagging ####
  bagging_start <- Sys.time()
  bagging_train_fit <- ipred::bagging(formula = y ~ ., data = train)
  bagging_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = bagging_train_fit, newdata = train))
  bagging_train_RMSE_mean <- mean(bagging_train_RMSE)
  bagging_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = bagging_train_fit, newdata = test))
  bagging_test_RMSE_mean <- mean(bagging_test_RMSE)
  bagging_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = bagging_train_fit, newdata = validation))
  bagging_validation_RMSE_mean <- mean(bagging_validation_RMSE)
  bagging_holdout_RMSE[i] <- mean(bagging_test_RMSE_mean, bagging_validation_RMSE_mean)
  bagging_holdout_RMSE_mean <- mean(bagging_holdout_RMSE)
  bagging_holdout_RMSE_sd_mean <- sd(c(bagging_test_RMSE_mean, bagging_validation_RMSE_mean))
  bagging_train_predict_value <- as.numeric(predict(object = bagging_train_fit, newdata = train))
  bagging_test_predict_value <- as.numeric(predict(object = bagging_train_fit, newdata = test))
  bagging_validation_predict_value <- as.numeric(predict(object = bagging_train_fit, newdata = validation))
  bagging_predict_value_mean <- mean(c(bagging_test_predict_value, bagging_validation_predict_value))
  bagging_sd[i] <- sd(c(bagging_test_predict_value, bagging_validation_predict_value))
  bagging_sd_mean <- mean(bagging_sd)
  bagging_holdout_vs_train[i] <- bagging_holdout_RMSE_mean / bagging_train_RMSE_mean
  bagging_holdout_vs_train_mean <- mean(bagging_holdout_vs_train)
  bagging_holdout_vs_train_range <- range(bagging_holdout_vs_train)
  bagging_holdout_vs_train_sd <- sd(bagging_holdout_vs_train)
  y_hat_bagging <- c(bagging_test_predict_value, bagging_validation_predict_value)
  bagging_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(bagging_test_predict_value, bagging_validation_predict_value))
  bagging_bias_mean <- mean(bagging_bias)
  bagging_bias_sd <- sd(bagging_bias)
  bagging_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(bagging_test_predict_value, bagging_validation_predict_value))
  bagging_MAE_mean <- mean(bagging_MAE)
  bagging_MAE_sd <- sd(bagging_MAE)
  bagging_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(bagging_test_predict_value, bagging_validation_predict_value))
  bagging_MSE_mean <- mean(bagging_MSE)
  bagging_MSE_sd <- sd(bagging_MSE)
  bagging_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(bagging_test_predict_value, bagging_validation_predict_value))
  bagging_SSE_mean <- mean(bagging_SSE)
  bagging_SSE_sd <- sd(bagging_SSE)
  bagging_ks_p_value[i] <- stats::ks.test(x = y_hat_bagging, y = c(train$y, validation$y), exact = TRUE)$p.value
  bagging_ks_p_value_mean <- mean(bagging_ks_p_value)
  bagging_ks_p_value_sd <- sd(bagging_ks_p_value)
  bagging_ks_stat[i] <- stats::ks.test(x = y_hat_bagging, y = c(train$y, validation$y), exact = TRUE)$statistic
  bagging_ks_stat_mean <- mean(bagging_ks_stat)
  bagging_ks_test <- c(bagging_ks_stat_mean, bagging_ks_p_value_mean)
  bagging_end <- Sys.time()
  bagging_duration[i] <- bagging_end - bagging_start
  bagging_duration_mean <- mean(bagging_duration)
  bagging_duration_sd <- sd(bagging_duration)

  ####  Model #3 Bayes Generalized Linear Model (GLM) ####
  bayesglm_start <- Sys.time()
  bayesglm_train_fit <- arm::bayesglm(y ~ ., data = train, family = gaussian(link = "identity"))
  bayesglm_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = bayesglm_train_fit, newdata = train))
  bayesglm_train_RMSE_mean <- mean(bayesglm_train_RMSE)
  bayesglm_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = bayesglm_train_fit, newdata = test))
  bayesglm_test_RMSE_mean <- mean(bayesglm_test_RMSE)
  bayesglm_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = bayesglm_train_fit, newdata = validation))
  bayesglm_validation_RMSE_mean <- mean(bayesglm_validation_RMSE)
  bayesglm_holdout_RMSE[i] <- mean(bayesglm_test_RMSE_mean, bayesglm_validation_RMSE_mean)
  bayesglm_holdout_RMSE_mean <- mean(bayesglm_holdout_RMSE)
  bayesglm_holdout_RMSE_sd_mean <- sd(c(bayesglm_test_RMSE_mean, bayesglm_validation_RMSE_mean))
  bayesglm_train_predict_value <- as.numeric(predict(object = bayesglm_train_fit, newdata = train))
  bayesglm_test_predict_value <- as.numeric(predict(object = bayesglm_train_fit, newdata = test))
  bayesglm_validation_predict_value <- as.numeric(predict(object = bayesglm_train_fit, newdata = validation))
  bayesglm_predict_value_mean <- mean(c(bayesglm_test_predict_value, bayesglm_validation_predict_value))
  bayesglm_sd[i] <- sd(c(bayesglm_test_predict_value, bayesglm_validation_predict_value))
  bayesglm_sd_mean <- mean(bayesglm_sd)
  bayesglm_holdout_vs_train[i] <- bayesglm_holdout_RMSE_mean / bayesglm_train_RMSE_mean
  bayesglm_holdout_vs_train_mean <- mean(bayesglm_holdout_vs_train)
  bayesglm_holdout_vs_train_range <- range(bayesglm_holdout_vs_train)
  bayesglm_holdout_vs_train_sd <- sd(bayesglm_holdout_vs_train)
  y_hat_bayesglm <- c(bayesglm_test_predict_value, bayesglm_validation_predict_value)
  bayesglm_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(bayesglm_test_predict_value, bayesglm_validation_predict_value))
  bayesglm_bias_mean <- mean(bayesglm_bias)
  bayesglm_bias_sd <- sd(bayesglm_bias)
  bayesglm_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(bayesglm_test_predict_value, bayesglm_validation_predict_value))
  bayesglm_MAE_mean <- mean(bayesglm_MAE)
  bayesglm_MAE_sd <- sd(bayesglm_MAE)
  bayesglm_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(bayesglm_test_predict_value, bayesglm_validation_predict_value))
  bayesglm_MSE_mean <- mean(bayesglm_MSE)
  bayesglm_MSE_sd <- sd(bayesglm_MSE)
  bayesglm_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(bayesglm_test_predict_value, bayesglm_validation_predict_value))
  bayesglm_SSE_mean <- mean(bayesglm_SSE)
  bayesglm_SSE_sd <- sd(bayesglm_SSE)
  bayesglm_ks_p_value[i] <- stats::ks.test(x = y_hat_bayesglm, y = c(train$y, validation$y), exact = TRUE)$p.value
  bayesglm_ks_p_value_mean <- mean(bayesglm_ks_p_value)
  bayesglm_ks_p_value_sd <- sd(bayesglm_ks_p_value)
  bayesglm_ks_stat[i] <- stats::ks.test(x = y_hat_bayesglm, y = c(train$y, validation$y), exact = TRUE)$statistic
  bayesglm_ks_stat_mean <- mean(bayesglm_ks_stat)
  bayesglm_ks_test <- c(bayesglm_ks_stat_mean, bayesglm_ks_p_value_mean)

  bayesglm_end <- Sys.time()
  bayesglm_duration[i] <- bayesglm_end - bayesglm_start
  bayesglm_duration_mean <- mean(bayesglm_duration)
  bayesglm_duration_sd <- sd(bayesglm_duration)

  ####  Model #4 Bayes RNN: Bayes Regularization for feed forward neural networks ####
  bayesrnn_start <- Sys.time()
  bayesrnn_train_fit <- brnn::brnn(x = as.matrix(train), y = train$y)
  bayesrnn_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = bayesrnn_train_fit, newdata = train))
  bayesrnn_train_RMSE_mean <- mean(bayesrnn_train_RMSE)
  bayesrnn_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = bayesrnn_train_fit, newdata = test))
  bayesrnn_test_RMSE_mean <- mean(bayesrnn_test_RMSE)
  bayesrnn_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = bayesrnn_train_fit, newdata = validation))
  bayesrnn_validation_RMSE_mean <- mean(bayesrnn_validation_RMSE)
  bayesrnn_holdout_RMSE[i] <- mean(c(bayesrnn_test_RMSE_mean, bayesrnn_validation_RMSE_mean))
  bayesrnn_holdout_RMSE_mean <- mean(bayesrnn_holdout_RMSE)
  bayesrnn_holdout_RMSE_sd_mean <- sd(c(bayesrnn_test_RMSE_mean, bayesrnn_validation_RMSE_mean))
  bayesrnn_train_predict_value <- as.numeric(predict(object = bayesrnn_train_fit, newdata = train))
  bayesrnn_test_predict_value <- as.numeric(predict(object = bayesrnn_train_fit, newdata = test))
  bayesrnn_validation_predict_value <- as.numeric(predict(object = bayesrnn_train_fit, newdata = validation))
  bayesrnn_predict_value_mean <- mean(c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value))
  bayesrnn_sd_mean <- sd(c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value))
  bayesrnn_holdout_vs_train[i] <- bayesrnn_holdout_RMSE_mean / bayesrnn_train_RMSE_mean
  bayesrnn_holdout_vs_train_mean <- mean(bayesrnn_holdout_vs_train)
  bayesrnn_holdout_vs_train_range <- range(bayesrnn_holdout_vs_train)
  bayesrnn_holdout_vs_train_sd <- sd(bayesrnn_holdout_vs_train)
  y_hat_bayesrnn <- c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value)
  bayesrnn_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value))
  bayesrnn_bias_mean <- mean(bayesrnn_bias)
  bayesrnn_bias_sd <- sd(bayesrnn_bias)
  bayesrnn_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value))
  bayesrnn_MAE_mean <- mean(bayesrnn_MAE)
  bayesrnn_MAE_sd <- sd(bayesrnn_MAE)
  bayesrnn_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value))
  bayesrnn_MSE_mean <- mean(bayesrnn_MSE)
  bayesrnn_MSE_sd <- sd(bayesrnn_MSE)
  bayesrnn_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value))
  bayesrnn_SSE_mean <- mean(bayesrnn_SSE)
  bayesrnn_SSE_sd <- sd(bayesrnn_SSE)
  bayesrnn_ks_p_value[i] <- stats::ks.test(x = y_hat_bayesrnn, y = c(train$y, validation$y), exact = TRUE)$p.value
  bayesrnn_ks_p_value_mean <- mean(bayesrnn_ks_p_value)
  bayesrnn_ks_p_value_sd <- sd(bayesrnn_ks_p_value)
  bayesrnn_ks_stat[i] <- stats::ks.test(x = y_hat_bayesrnn, y = c(train$y, validation$y), exact = TRUE)$statistic
  bayesrnn_ks_stat_mean <- mean(bayesrnn_ks_stat)
  bayesrnn_ks_test <- c(bayesrnn_ks_stat_mean, bayesrnn_ks_p_value_mean)

  bayesrnn_end <- Sys.time()
  bayesrnn_duration[i] <- bayesrnn_end - bayesrnn_start
  bayesrnn_duration_mean <- mean(bayesrnn_duration)
  bayesrnn_duration_sd <- sd(bayesrnn_duration)

  ####  Model #5 Boosted Random Forest ####
  boost_rf_start <- Sys.time()
  boost_rf_train_fit <- e1071::tune.randomForest(x = train, y = train$y, mtry = ncol(train) - 1)
  boost_rf_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(
    object = boost_rf_train_fit$best.model,
    newdata = train
  ))
  boost_rf_train_RMSE_mean <- mean(boost_rf_train_RMSE)
  boost_rf_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(
    object = boost_rf_train_fit$best.model,
    newdata = test
  ))
  boost_rf_test_RMSE_mean <- mean(boost_rf_test_RMSE)
  boost_rf_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(
    object = boost_rf_train_fit$best.model,
    newdata = validation
  ))
  boost_rf_validation_RMSE_mean <- mean(boost_rf_validation_RMSE)
  boost_rf_holdout_RMSE[i] <- mean(boost_rf_test_RMSE_mean, boost_rf_validation_RMSE_mean)
  boost_rf_holdout_RMSE_mean <- mean(boost_rf_holdout_RMSE)
  boost_rf_holdout_RMSE_sd_mean <- sd(c(boost_rf_test_RMSE_mean, boost_rf_validation_RMSE_mean))
  boost_rf_train_predict_value <- as.numeric(predict(object = boost_rf_train_fit$best.model, newdata = train))
  boost_rf_test_predict_value <- as.numeric(predict(object = boost_rf_train_fit$best.model, newdata = test))
  boost_rf_validation_predict_value <- as.numeric(predict(object = boost_rf_train_fit$best.model, newdata = validation))
  boost_rf_predict_value_mean <- mean(c(boost_rf_test_predict_value, boost_rf_validation_predict_value))
  boost_rf_sd[i] <- sd(c(boost_rf_test_predict_value, boost_rf_validation_predict_value))
  boost_rf_sd_mean <- mean(boost_rf_sd)
  boost_rf_holdout_vs_train[i] <- boost_rf_holdout_RMSE_mean / boost_rf_train_RMSE_mean
  boost_rf_holdout_vs_train_mean <- mean(boost_rf_holdout_vs_train)
  boost_rf_holdout_vs_train_range <- range(boost_rf_holdout_vs_train)
  boost_rf_holdout_vs_train_sd <- sd(boost_rf_holdout_vs_train)
  y_hat_boost_rf <- c(boost_rf_test_predict_value, boost_rf_validation_predict_value)
  boost_rf_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(boost_rf_test_predict_value, boost_rf_validation_predict_value))
  boost_rf_bias_mean <- mean(boost_rf_bias)
  boost_rf_bias_sd <- sd(boost_rf_bias)
  boost_rf_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(boost_rf_test_predict_value, boost_rf_validation_predict_value))
  boost_rf_MAE_mean <- mean(boost_rf_MAE)
  boost_rf_MAE_sd <- sd(boost_rf_MAE)
  boost_rf_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(boost_rf_test_predict_value, boost_rf_validation_predict_value))
  boost_rf_MSE_mean <- mean(boost_rf_MSE)
  boost_rf_MSE_sd <- sd(boost_rf_MSE)
  boost_rf_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(boost_rf_test_predict_value, boost_rf_validation_predict_value))
  boost_rf_SSE_mean <- mean(boost_rf_SSE)
  boost_rf_SSE_sd <- sd(boost_rf_SSE)
  boost_rf_ks_p_value[i] <- stats::ks.test(x = y_hat_boost_rf, y = c(train$y, validation$y), exact = TRUE)$p.value
  boost_rf_ks_p_value_mean <- mean(boost_rf_ks_p_value)
  boost_rf_ks_p_value_sd <- sd(boost_rf_ks_p_value)
  boost_rf_ks_stat[i] <- stats::ks.test(x = y_hat_boost_rf, y = c(train$y, validation$y), exact = TRUE)$statistic
  boost_rf_ks_stat_mean <- mean(boost_rf_ks_stat)
  boost_rf_ks_test <- c(boost_rf_ks_stat_mean, boost_rf_ks_p_value_mean)

  boost_rf_end <- Sys.time()
  boost_rf_duration[i] <- boost_rf_end - boost_rf_start
  boost_rf_duration_mean <- mean(boost_rf_duration)
  boost_rf_duration_sd <- sd(boost_rf_duration)

  ####  Model #6 Cubist ####
  cubist_start <- Sys.time()
  cubist_train_fit <- Cubist::cubist(x = train[, 1:ncol(train) - 1], y = train$y)
  cubist_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = cubist_train_fit, newdata = train))
  cubist_train_RMSE_mean <- mean(cubist_train_RMSE)
  cubist_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = cubist_train_fit, newdata = test))
  cubist_test_RMSE_mean <- mean(cubist_test_RMSE)
  cubist_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = cubist_train_fit, newdata = validation))
  cubist_validation_RMSE_mean <- mean(cubist_validation_RMSE)
  cubist_holdout_RMSE[i] <- mean(cubist_test_RMSE_mean, cubist_validation_RMSE_mean)
  cubist_holdout_RMSE_mean <- mean(cubist_holdout_RMSE)
  cubist_holdout_RMSE_sd_mean <- sd(c(cubist_test_RMSE_mean, cubist_validation_RMSE_mean))
  cubist_train_predict_value <- as.numeric(predict(object = cubist_train_fit, newdata = train))
  cubist_test_predict_value <- as.numeric(predict(object = cubist_train_fit, newdata = test))
  cubist_validation_predict_value <- as.numeric(predict(object = cubist_train_fit, newdata = validation))
  cubist_predict_value_mean <- mean(c(cubist_test_predict_value, cubist_validation_predict_value))
  cubist_sd[i] <- sd(c(cubist_test_predict_value, cubist_validation_predict_value))
  cubist_sd_mean <- mean(cubist_sd)
  cubist_holdout_vs_train[i] <- cubist_holdout_RMSE_mean / cubist_train_RMSE_mean
  cubist_holdout_vs_train_mean <- mean(cubist_holdout_vs_train)
  cubist_holdout_vs_train_range <- range(cubist_holdout_vs_train)
  cubist_holdout_vs_train_sd <- sd(cubist_holdout_vs_train)
  y_hat_cubist <- c(cubist_test_predict_value, cubist_validation_predict_value)
  cubist_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(cubist_test_predict_value, cubist_validation_predict_value))
  cubist_bias_mean <- mean(cubist_bias)
  cubist_bias_sd <- sd(cubist_bias)
  cubist_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(cubist_test_predict_value, cubist_validation_predict_value))
  cubist_MAE_mean <- mean(cubist_MAE)
  cubist_MAE_sd <- sd(cubist_MAE)
  cubist_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(cubist_test_predict_value, cubist_validation_predict_value))
  cubist_MSE_mean <- mean(cubist_MSE)
  cubist_MSE_sd <- sd(cubist_MSE)
  cubist_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(cubist_test_predict_value, cubist_validation_predict_value))
  cubist_SSE_mean <- mean(cubist_SSE)
  cubist_SSE_sd <- sd(cubist_SSE)
  cubist_ks_p_value[i] <- stats::ks.test(x = y_hat_cubist, y = c(train$y, validation$y), exact = TRUE)$p.value
  cubist_ks_p_value_mean <- mean(cubist_ks_p_value)
  cubist_ks_p_value_sd <- sd(cubist_ks_p_value)
  cubist_ks_stat[i] <- stats::ks.test(x = y_hat_cubist, y = c(train$y, validation$y), exact = TRUE)$statistic
  cubist_ks_stat_mean <- mean(cubist_ks_stat)
  cubist_ks_test <- c(cubist_ks_stat_mean, cubist_ks_p_value_mean)

  cubist_end <- Sys.time()
  cubist_duration[i] <- cubist_end - cubist_start
  cubist_duration_mean <- mean(cubist_duration)
  cubist_duration_sd <- sd(cubist_duration)

  ####  Model #6 earth ####
  earth_start <- Sys.time()
  earth_train_fit <- earth::earth(x = train[, 1:ncol(train) - 1], y = train$y)
  earth_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = earth_train_fit, newdata = train))
  earth_train_RMSE_mean <- mean(earth_train_RMSE)
  earth_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = earth_train_fit, newdata = test))
  earth_test_RMSE_mean <- mean(earth_test_RMSE)
  earth_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = earth_train_fit, newdata = validation))
  earth_validation_RMSE_mean <- mean(earth_validation_RMSE)
  earth_holdout_RMSE[i] <- mean(earth_test_RMSE_mean, earth_validation_RMSE_mean)
  earth_holdout_RMSE_mean <- mean(earth_holdout_RMSE)
  earth_holdout_RMSE_sd_mean <- sd(c(earth_test_RMSE_mean, earth_validation_RMSE_mean))
  earth_train_predict_value <- as.numeric(predict(object = earth_train_fit, newdata = train))
  earth_test_predict_value <- as.numeric(predict(object = earth_train_fit, newdata = test))
  earth_validation_predict_value <- as.numeric(predict(object = earth_train_fit, newdata = validation))
  earth_predict_value_mean <- mean(c(earth_test_predict_value, earth_validation_predict_value))
  earth_sd[i] <- sd(c(earth_test_predict_value, earth_validation_predict_value))
  earth_sd_mean <- mean(earth_sd)
  earth_holdout_vs_train[i] <- earth_holdout_RMSE_mean / earth_train_RMSE_mean
  earth_holdout_vs_train_mean <- mean(earth_holdout_vs_train)
  earth_holdout_vs_train_range <- range(earth_holdout_vs_train)
  earth_holdout_vs_train_sd <- sd(earth_holdout_vs_train)
  y_hat_earth <- c(earth_test_predict_value, earth_validation_predict_value)
  earth_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(earth_test_predict_value, earth_validation_predict_value))
  earth_bias_mean <- mean(earth_bias)
  earth_bias_sd <- sd(earth_bias)
  earth_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(earth_test_predict_value, earth_validation_predict_value))
  earth_MAE_mean <- mean(earth_MAE)
  earth_MAE_sd <- sd(earth_MAE)
  earth_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(earth_test_predict_value, earth_validation_predict_value))
  earth_MSE_mean <- mean(earth_MSE)
  earth_MSE_sd <- sd(earth_MSE)
  earth_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(earth_test_predict_value, earth_validation_predict_value))
  earth_SSE_mean <- mean(earth_SSE)
  earth_SSE_sd <- sd(earth_SSE)
  earth_ks_p_value[i] <- stats::ks.test(x = y_hat_earth, y = c(train$y, validation$y), exact = TRUE)$p.value
  earth_ks_p_value_mean <- mean(earth_ks_p_value)
  earth_ks_p_value_sd <- sd(earth_ks_p_value)
  earth_ks_stat[i] <- stats::ks.test(x = y_hat_earth, y = c(train$y, validation$y), exact = TRUE)$statistic
  earth_ks_stat_mean <- mean(earth_ks_stat)
  earth_ks_test <- c(earth_ks_stat_mean, earth_ks_p_value_mean)

  earth_end <- Sys.time()
  earth_duration[i] <- earth_end - earth_start
  earth_duration_mean <- mean(earth_duration)
  earth_duration_sd <- sd(earth_duration)

  #### Model #7  Elastic Net ####
  elastic_start <- Sys.time()
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  elastic_model <- glmnet::glmnet(x, y, alpha = 0.5)
  elastic_cv <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  best_elastic_lambda <- elastic_cv$lambda.min
  best_elastic_model <- glmnet::glmnet(x, y, alpha = 0, lambda = best_elastic_lambda)
  elastic_train_pred <- predict(best_elastic_model, s = best_elastic_lambda, newx = data.matrix(train %>% dplyr::select(-y)))
  elastic_train_RMSE <- Metrics::rmse(actual = y, predicted = elastic_train_pred)
  elastic_train_RMSE_df <- rbind(elastic_train_RMSE_df, elastic_train_RMSE)
  elastic_train_RMSE_mean <- mean(elastic_train_RMSE_df$elastic_train_RMSE[2:nrow(elastic_train_RMSE_df)])
  ## Elastic using the test data set ##

  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  elastic_model <- glmnet::glmnet(x, y, alpha = 0.5)
  elastic_cv <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  best_elastic_lambda <- elastic_cv$lambda.min
  best_elastic_model <- glmnet::glmnet(x, y, alpha = 0, lambda = best_elastic_lambda)
  elastic_test_pred <- predict(best_elastic_model, s = best_elastic_lambda, newx = data.matrix(test %>% dplyr::select(-y)))

  elastic_test_RMSE <- Metrics::rmse(actual = test$y, predicted = elastic_test_pred)
  elastic_test_RMSE_df <- rbind(elastic_test_RMSE_df, elastic_test_RMSE)
  elastic_test_RMSE_mean <- mean(elastic_test_RMSE_df$elastic_test_RMSE[2:nrow(elastic_test_RMSE_df)])
  elastic_test_predict_value[i] <- round(mean(elastic_test_pred), 4)
  elastic_test_predict_value_mean <- mean(elastic_test_predict_value)
  elastic_test_predict_value_sd[i] <- round(sd(elastic_test_pred), 4)
  elastic_test_predict_value_sd_mean <- mean(elastic_test_predict_value_sd)
  ## Elastic using the validation data set
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  elastic_model <- glmnet::glmnet(x, y, alpha = 0.5)
  elastic_cv <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  best_elastic_lambda <- elastic_cv$lambda.min
  best_elastic_model <- glmnet::glmnet(x, y, alpha = 0, lambda = best_elastic_lambda)
  elastic_validation_pred <- predict(best_elastic_model, s = best_elastic_lambda, newx = data.matrix(validation %>% dplyr::select(-y)))
  elastic_validation_RMSE <- Metrics::rmse(actual = validation$y, predicted = elastic_validation_pred)
  elastic_validation_RMSE_df <- rbind(elastic_validation_RMSE_df, elastic_validation_RMSE)
  elastic_validation_RMSE_mean <- mean(elastic_validation_RMSE_df$elastic_validation_RMSE[2:nrow(elastic_validation_RMSE_df)])
  elastic_validation_predict_value[i] <- round(mean(elastic_validation_pred), 4)
  elastic_validation_predict_value_mean <- mean(elastic_validation_predict_value)
  elastic_validation_predict_value_sd[i] <- round(sd(elastic_validation_pred), 4)
  elastic_validation_predict_value_sd_mean <- mean(elastic_validation_predict_value_sd)

  elastic_holdout_RMSE <- mean(elastic_test_RMSE_mean, elastic_validation_RMSE_mean)
  elastic_holdout_RMSE_df <- rbind(elastic_holdout_RMSE_df, elastic_holdout_RMSE)
  elastic_holdout_RMSE_mean <- mean(elastic_holdout_RMSE_df$elastic_holdout_RMSE[2:nrow(elastic_holdout_RMSE_df)])

  elastic_holdout_RMSE_sd <- sd(c(elastic_test_RMSE_mean, elastic_validation_RMSE_mean))
  elastic_holdout_RMSE_sd_df <- rbind(elastic_holdout_RMSE_sd, elastic_holdout_RMSE_sd_df)
  elastic_holdout_RMSE_sd_mean <- mean(elastic_holdout_RMSE_sd_df$elastic_holdout_RMSE_sd[2:nrow(elastic_holdout_RMSE_sd_df)])

  elastic_holdout_vs_train <- c(elastic_holdout_RMSE / elastic_train_RMSE)
  elastic_holdout_vs_train_df <- rbind(elastic_holdout_vs_train_df, elastic_holdout_vs_train)
  elastic_holdout_vs_train_mean <- mean(elastic_holdout_vs_train_df$elastic_holdout_vs_train[2:nrow(elastic_holdout_vs_train_df)])
  elastic_holdout_vs_train_sd <- sd(elastic_holdout_vs_train_df$elastic_holdout_vs_train)

  elastic_test_predict_value_mean <- mean(c(elastic_test_predict_value_mean, elastic_validation_predict_value_mean))

  elastic_sd[i] <- mean(elastic_test_predict_value_sd_mean, elastic_validation_predict_value_sd_mean)
  elastic_sd_mean <- mean(elastic_sd)

  y_hat_elastic <- as.numeric(c(rowMeans(elastic_test_pred), rowMeans(elastic_validation_pred)))

  length(c(test$y, validation$y))
  length(y_hat_elastic)

  elastic_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = as.numeric(c(elastic_test_pred, elastic_validation_pred)))
  elastic_bias_mean <- mean(elastic_bias)
  elastic_bias_sd <- sd(elastic_bias)
  elastic_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = as.numeric(c(elastic_test_pred, elastic_validation_pred)))
  elastic_MAE_mean <- mean(elastic_MAE)
  elastic_MAE_sd <- sd(elastic_MAE)
  elastic_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = as.numeric(c(elastic_test_pred, elastic_validation_pred)))
  elastic_MSE_mean <- mean(elastic_MSE)
  elastic_MSE_sd <- sd(elastic_MSE)
  elastic_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = as.numeric(c(elastic_test_pred, elastic_validation_pred)))
  elastic_SSE_mean <- mean(elastic_SSE)
  elastic_SSE_sd <- sd(elastic_SSE)
  elastic_ks_p_value[i] <- stats::ks.test(x = y_hat_elastic, y = c(train$y, validation$y), exact = TRUE)$p.value
  elastic_ks_p_value_mean <- mean(elastic_ks_p_value)
  elastic_ks_p_value_sd <- sd(elastic_ks_p_value)
  elastic_ks_stat[i] <- stats::ks.test(x = y_hat_elastic, y = c(train$y, validation$y), exact = TRUE)$statistic
  elastic_ks_stat_mean <- mean(elastic_ks_stat)
  elastic_ks_test <- c(elastic_ks_stat_mean, elastic_ks_p_value_mean)

  elastic_end <- Sys.time()
  elastic_duration[i] <- elastic_end - elastic_start
  elastic_duration_mean <- mean(elastic_duration)
  elastic_duration_sd <- sd(elastic_duration)


  #### Model #8 GAM (Generalized Additive Models) with Smoothing Splines ####
  gam_start <- Sys.time()
  n_unique_vals <- purrr::map_dbl(df, dplyr::n_distinct)

  # Names of columns with >= 4 unique vals
  keep <- names(n_unique_vals)[n_unique_vals >= 4]

  gam_data <- df %>%
    dplyr::select(dplyr::all_of(keep))

  # Model data
  train1 <- train %>%
    dplyr::select(dplyr::all_of(keep))

  test1 <- test %>%
    dplyr::select(dplyr::all_of(keep))

  validation1 <- validation %>%
    dplyr::select(dplyr::all_of(keep))

  names_df <- names(gam_data[, 1:ncol(gam_data) - 1])
  f2 <- stats::as.formula(paste0("y ~", paste0("gam::s(", names_df, ")", collapse = "+")))
  gam_train_fit <- gam(f2, data = train1)
  gam_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = gam_train_fit, newdata = train))
  gam_train_RMSE_mean <- mean(gam_train_RMSE)
  gam_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = gam_train_fit, newdata = test))
  gam_test_RMSE_mean <- mean(gam_test_RMSE)
  gam_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = gam_train_fit, newdata = validation))
  gam_validation_RMSE_mean <- mean(gam_validation_RMSE)
  gam_holdout_RMSE[i] <- mean(gam_test_RMSE_mean, gam_validation_RMSE_mean)
  gam_holdout_RMSE_mean <- mean(gam_holdout_RMSE)
  gam_holdout_RMSE_sd_mean <- sd(c(gam_test_RMSE_mean, gam_validation_RMSE_mean))
  gam_train_predict_value <- as.numeric(predict(object = gam_train_fit, newdata = train))
  gam_test_predict_value <- as.numeric(predict(object = gam_train_fit, newdata = test))
  gam_validation_predict_value <- as.numeric(predict(object = gam_train_fit, newdata = validation))
  gam_predict_value_mean <- mean(c(gam_test_predict_value, gam_validation_predict_value))
  gam_sd[i] <- sd(c(gam_test_predict_value, gam_validation_predict_value))
  gam_sd_mean <- mean(gam_sd)
  gam_holdout_vs_train[i] <- gam_holdout_RMSE_mean / gam_train_RMSE_mean
  gam_holdout_vs_train_mean <- mean(gam_holdout_vs_train)
  gam_holdout_vs_train_range <- range(gam_holdout_vs_train)
  gam_holdout_vs_train_sd <- sd(gam_holdout_vs_train)
  gam_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(gam_test_predict_value, gam_validation_predict_value))
  gam_bias_mean <- mean(gam_bias)
  gam_bias_sd <- sd(gam_bias)
  gam_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(gam_test_predict_value, gam_validation_predict_value))
  gam_MAE_mean <- mean(gam_MAE)
  gam_MAE_sd <- sd(gam_MAE)
  gam_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(gam_test_predict_value, gam_validation_predict_value))
  gam_MSE_mean <- mean(gam_MSE)
  gam_MSE_sd <- sd(gam_MSE)
  gam_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(gam_test_predict_value, gam_validation_predict_value))
  gam_SSE_mean <- mean(gam_SSE)
  gam_SSE_sd <- sd(gam_SSE)
  y_hat_gam <- c(gam_test_predict_value, gam_validation_predict_value)
  gam_ks_p_value[i] <- stats::ks.test(x = y_hat_gam, y = c(train$y, validation$y), exact = TRUE)$p.value
  gam_ks_p_value_mean <- mean(gam_ks_p_value)
  gam_ks_p_value_sd <- sd(gam_ks_p_value)
  gam_ks_stat[i] <- stats::ks.test(x = y_hat_gam, y = c(train$y, validation$y), exact = TRUE)$statistic
  gam_ks_stat_mean <- mean(gam_ks_stat)
  gam_ks_test <- c(gam_ks_stat_mean, gam_ks_p_value_mean)

  gam_end <- Sys.time()
  gam_duration[i] <- gam_end - gam_start
  gam_duration_mean <- mean(gam_duration)
  gam_duration_sd <- sd(gam_duration)

  ####  Model #9 Gradient Boosted ####
  gb_start <- Sys.time()
  gb_train_fit <- gbm::gbm(train$y ~ ., data = train, distribution = "gaussian", n.trees = 100, shrinkage = 0.1, interaction.depth = 10)
  gb_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = gb_train_fit, newdata = train))
  gb_train_RMSE_mean <- mean(gb_train_RMSE)
  gb_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = gb_train_fit, newdata = test))
  gb_test_RMSE_mean <- mean(gb_test_RMSE)
  gb_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = gb_train_fit, newdata = validation))
  gb_validation_RMSE_mean <- mean(gb_validation_RMSE)
  gb_holdout_RMSE[i] <- mean(c(gb_test_RMSE_mean, gb_validation_RMSE_mean))
  gb_holdout_RMSE_mean <- mean(gb_holdout_RMSE)
  gb_holdout_RMSE_sd_mean <- sd(c(gb_test_RMSE_mean, gb_validation_RMSE_mean))
  gb_train_predict_value <- as.numeric(predict(object = gb_train_fit, newdata = train))
  gb_test_predict_value <- as.numeric(predict(object = gb_train_fit, newdata = test))
  gb_validation_predict_value <- as.numeric(predict(object = gb_train_fit, newdata = validation))
  gb_predict_value_mean <- mean(c(gb_test_predict_value, gb_validation_predict_value))
  gb_sd[i] <- sd(c(gb_test_predict_value, gb_validation_predict_value))
  gb_sd_mean <- mean(gb_sd)
  gb_holdout_vs_train[i] <- gb_holdout_RMSE_mean / gb_train_RMSE_mean
  gb_holdout_vs_train_mean <- mean(gb_holdout_vs_train)
  gb_holdout_vs_train_range <- range(gb_holdout_vs_train)
  gb_holdout_vs_train_sd <- sd(gb_holdout_vs_train)
  y_hat_gb <- c(gb_test_predict_value, gb_validation_predict_value)
  gb_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(gb_test_predict_value, gb_validation_predict_value))
  gb_bias_mean <- mean(gb_bias)
  gb_bias_sd <- sd(gb_bias)
  gb_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(gb_test_predict_value, gb_validation_predict_value))
  gb_MAE_mean <- mean(gb_MAE)
  gb_MAE_sd <- sd(gb_MAE)
  gb_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(gb_test_predict_value, gb_validation_predict_value))
  gb_MSE_mean <- mean(gb_MSE)
  gb_MSE_sd <- sd(gb_MSE)
  gb_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(gb_test_predict_value, gb_validation_predict_value))
  gb_SSE_mean <- mean(gb_SSE)
  gb_SSE_sd <- sd(gb_SSE)
  gb_ks_p_value[i] <- stats::ks.test(x = y_hat_gb, y = c(train$y, validation$y), exact = TRUE)$p.value
  gb_ks_p_value_mean <- mean(gb_ks_p_value)
  gb_ks_p_value_sd <- sd(gb_ks_p_value)
  gb_ks_stat[i] <- stats::ks.test(x = y_hat_gb, y = c(train$y, validation$y), exact = TRUE)$statistic
  gb_ks_stat_mean <- mean(gb_ks_stat)
  gb_ks_test <- c(gb_ks_stat_mean, gb_ks_p_value_mean)

  gb_end <- Sys.time()
  gb_duration[i] <- gb_end - gb_start
  gb_duration_mean <- mean(gb_duration)
  gb_duration_sd <- sd(gb_duration)

  ####  Model #10 K-Nearest Neighbors ####
  knn_start <- Sys.time()
  knn_train_fit <- e1071::tune.gknn(x = train[, 1:ncol(train) - 1], y = train$y, scale = TRUE, k = c(1:25))
  knn_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(
    object = knn_train_fit$best.model,
    newdata = train[, 1:ncol(train) - 1], k = knn_train_fit$best_model$k
  ))
  knn_train_RMSE_mean <- mean(knn_train_RMSE)
  knn_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(
    object = knn_train_fit$best.model,
    k = knn_train_fit$best_model$k, newdata = test[, 1:ncol(test) - 1]
  ))
  knn_test_RMSE_mean <- mean(knn_test_RMSE)
  knn_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(
    object = knn_train_fit$best.model,
    newdata = validation[, 1:ncol(validation) - 1], k = knn_train_fit$best_model$k
  ))
  knn_validation_RMSE_mean <- mean(knn_validation_RMSE)
  knn_holdout_RMSE[i] <- mean(c(knn_test_RMSE_mean, knn_validation_RMSE_mean))
  knn_holdout_RMSE_mean <- mean(knn_holdout_RMSE)
  knn_holdout_RMSE_sd_mean <- sd(c(knn_test_RMSE_mean, knn_validation_RMSE_mean))
  knn_train_predict_value <- as.numeric(predict(
    object = knn_train_fit$best.model, newdata = train[, 1:ncol(train) - 1],
    k = knn_train_fit$best_model$k
  ))
  knn_test_predict_value <- as.numeric(predict(
    object = knn_train_fit$best.model, newdata = test[, 1:ncol(test) - 1],
    k = knn_train_fit$best_model$k
  ))
  knn_validation_predict_value <- as.numeric(predict(
    object = knn_train_fit$best.model, newdata = validation[, 1:ncol(test) - 1],
    k = knn_train_fit$best_model$k
  ))
  knn_predict_value <- mean(c(knn_test_predict_value, knn_validation_predict_value))
  knn_predict_value_mean <- mean(c(knn_test_predict_value, knn_validation_predict_value))
  knn_sd[i] <- sd(c(knn_test_predict_value, knn_validation_predict_value))
  knn_sd_mean <- mean(knn_sd)
  knn_holdout_vs_train[i] <- knn_holdout_RMSE_mean / knn_train_RMSE_mean
  knn_holdout_vs_train_mean <- mean(knn_holdout_vs_train)
  knn_holdout_vs_train_range <- range(knn_holdout_vs_train)
  knn_holdout_vs_train_sd <- sd(knn_holdout_vs_train)
  y_hat_knn <- c(knn_test_predict_value, knn_validation_predict_value)
  knn_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(knn_test_predict_value, knn_validation_predict_value))
  knn_bias_mean <- mean(knn_bias)
  knn_bias_sd <- sd(knn_bias)
  knn_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(knn_test_predict_value, knn_validation_predict_value))
  knn_MAE_mean <- mean(knn_MAE)
  knn_MAE_sd <- sd(knn_MAE)
  knn_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(knn_test_predict_value, knn_validation_predict_value))
  knn_MSE_mean <- mean(knn_MSE)
  knn_MSE_sd <- sd(knn_MSE)
  knn_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(knn_test_predict_value, knn_validation_predict_value))
  knn_SSE_mean <- mean(knn_SSE)
  knn_SSE_sd <- sd(knn_SSE)
  knn_ks_p_value[i] <- stats::ks.test(x = y_hat_knn, y = c(train$y, validation$y), exact = TRUE)$p.value
  knn_ks_p_value_mean <- mean(knn_ks_p_value)
  knn_ks_p_value_sd <- sd(knn_ks_p_value)
  knn_ks_stat[i] <- stats::ks.test(x = y_hat_knn, y = c(train$y, validation$y), exact = TRUE)$statistic
  knn_ks_stat_mean <- mean(knn_ks_stat)
  knn_ks_test <- c(knn_ks_stat_mean, knn_ks_p_value_mean)

  knn_end <- Sys.time()
  knn_duration[i] <- knn_end - knn_start
  knn_duration_mean <- mean(knn_duration)
  knn_duration_sd <- sd(knn_duration)

  ####  11. Lasso Net ####
  lasso_start <- Sys.time()
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  lasso_model <- glmnet(x, y, alpha = 1)
  lasso_cv <- glmnet::cv.glmnet(x, y, alpha = 1)
  best_lasso_lambda <- lasso_cv$lambda.min
  best_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lasso_lambda)
  lasso_train_pred <- predict(best_lasso_model, s = best_lasso_lambda, newx = data.matrix(train %>% dplyr::select(-y)))
  lasso_train_RMSE <- Metrics::rmse(actual = y, predicted = lasso_train_pred)
  lasso_train_RMSE_df <- rbind(lasso_train_RMSE_df, lasso_train_RMSE)
  lasso_train_RMSE_mean <- mean(lasso_train_RMSE_df$lasso_train_RMSE[2:nrow(lasso_train_RMSE_df)])
  ## lasso using the test data set ##

  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  lasso_model <- glmnet(x, y, alpha = 1)
  lasso_cv <- glmnet::cv.glmnet(x, y, alpha = 1)
  best_lasso_lambda <- lasso_cv$lambda.min
  best_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lasso_lambda)
  lasso_test_pred <- predict(best_lasso_model, s = best_lasso_lambda, newx = data.matrix(test %>% dplyr::select(-y)))
  lasso_test_RMSE <- Metrics::rmse(actual = test$y, predicted = lasso_test_pred)
  lasso_test_RMSE_df <- rbind(lasso_test_RMSE_df, lasso_test_RMSE)
  lasso_test_RMSE_mean <- mean(lasso_test_RMSE_df$lasso_test_RMSE[2:nrow(lasso_test_RMSE_df)])
  lasso_test_predict_value[i] <- round(mean(lasso_test_pred), 4)
  lasso_test_predict_value_mean <- mean(lasso_test_predict_value)
  lasso_test_predict_value_sd[i] <- round(sd(lasso_test_pred), 4)
  lasso_test_predict_value_sd_mean <- mean(lasso_test_predict_value_sd)
  ## lasso using the validation data set
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  lasso_model <- glmnet(x, y, alpha = 1)
  lasso_cv <- glmnet::cv.glmnet(x, y, alpha = 1)
  best_lasso_lambda <- lasso_cv$lambda.min
  best_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lasso_lambda)
  lasso_validation_pred <- predict(best_lasso_model, s = best_lasso_lambda, newx = data.matrix(validation %>% dplyr::select(-y)))
  lasso_validation_RMSE <- Metrics::rmse(actual = validation$y, predicted = lasso_validation_pred)
  lasso_validation_RMSE_df <- rbind(lasso_validation_RMSE_df, lasso_validation_RMSE)
  lasso_validation_RMSE_mean <- mean(lasso_validation_RMSE_df$lasso_validation_RMSE[2:nrow(lasso_validation_RMSE_df)])
  lasso_validation_predict_value[i] <- round(mean(lasso_validation_pred), 4)
  lasso_validation_predict_value_mean <- mean(lasso_validation_predict_value)
  lasso_validation_predict_value_sd[i] <- round(sd(lasso_validation_pred), 4)
  lasso_validation_predict_value_sd_mean <- mean(lasso_validation_predict_value_sd)

  lasso_holdout_RMSE <- mean(lasso_test_RMSE_mean, lasso_validation_RMSE_mean)
  lasso_holdout_RMSE_df <- rbind(lasso_holdout_RMSE_df, lasso_holdout_RMSE)
  lasso_holdout_RMSE_mean <- mean(lasso_holdout_RMSE_df$lasso_holdout_RMSE[2:nrow(lasso_holdout_RMSE_df)])

  lasso_holdout_RMSE_sd <- sd(c(lasso_test_RMSE_mean, lasso_validation_RMSE_mean))
  lasso_holdout_RMSE_sd_df <- rbind(lasso_holdout_RMSE_sd_df, lasso_holdout_RMSE_sd)
  lasso_holdout_RMSE_sd_mean <- mean(lasso_holdout_RMSE_sd_df$lasso_holdout_RMSE_sd[2:nrow(lasso_holdout_RMSE_sd_df)])

  lasso_holdout_vs_train <- c(lasso_holdout_RMSE / lasso_train_RMSE)
  lasso_holdout_vs_train_df <- rbind(lasso_holdout_vs_train_df, lasso_holdout_vs_train)
  lasso_holdout_vs_train_mean <- mean(lasso_holdout_vs_train_df$lasso_holdout_vs_train[2:nrow(lasso_holdout_vs_train_df)])
  lasso_holdout_vs_train_sd <- sd(lasso_holdout_vs_train_df$lasso_holdout_vs_train)

  lasso_predict_value_mean <- mean(c(lasso_test_predict_value_mean, lasso_validation_predict_value_mean))

  lasso_sd[i] <- mean(lasso_test_predict_value_sd_mean, lasso_validation_predict_value_sd_mean)
  lasso_sd_mean <- mean(lasso_sd)

  lasso_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = as.numeric(c(lasso_test_pred, lasso_validation_pred)))
  lasso_bias_mean <- mean(lasso_bias)
  lasso_bias_sd <- sd(lasso_bias)
  lasso_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = as.numeric(c(lasso_test_pred, lasso_validation_pred)))
  lasso_MAE_mean <- mean(lasso_MAE)
  lasso_MAE_sd <- sd(lasso_MAE)
  lasso_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = as.numeric(c(lasso_test_pred, lasso_validation_pred)))
  lasso_MSE_mean <- mean(lasso_MSE)
  lasso_MSE_sd <- sd(lasso_MSE)
  lasso_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = as.numeric(c(lasso_test_pred, lasso_validation_pred)))
  lasso_SSE_mean <- mean(lasso_SSE)
  lasso_SSE_sd <- sd(lasso_SSE)

  lasso_ks_p_value[i] <- stats::ks.test(x = y_hat_lasso, y = c(train$y, validation$y), exact = TRUE)$p.value
  lasso_ks_p_value_mean <- mean(lasso_ks_p_value)
  lasso_ks_p_value_sd <- sd(lasso_ks_p_value)
  lasso_ks_stat[i] <- stats::ks.test(x = y_hat_lasso, y = c(train$y, validation$y), exact = TRUE)$statistic
  lasso_ks_stat_mean <- mean(lasso_ks_stat)
  lasso_ks_test <- c(lasso_ks_stat_mean, lasso_ks_p_value_mean)

  y_hat_lasso <- as.numeric(c(rowMeans(lasso_test_pred), rowMeans(lasso_validation_pred)))

  lasso_end <- Sys.time()
  lasso_duration[i] <- lasso_end - lasso_start
  lasso_duration_mean <- mean(lasso_duration)
  lasso_duration_sd <- sd(lasso_duration)


  ####  Model 12 Linear ####
  linear_start <- Sys.time()
  linear_train_fit <- e1071::tune.rpart(formula = y ~ ., data = train)
  linear_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = linear_train_fit$best.model, newdata = train))
  linear_train_RMSE_mean <- mean(linear_train_RMSE)
  linear_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = linear_train_fit$best.model, newdata = test))
  linear_test_RMSE_mean <- mean(linear_test_RMSE)
  linear_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = linear_train_fit$best.model, newdata = validation))
  linear_validation_RMSE_mean <- mean(linear_validation_RMSE)
  linear_holdout_RMSE[i] <- mean(c(linear_test_RMSE_mean, linear_validation_RMSE_mean))
  linear_holdout_RMSE_mean <- mean(linear_holdout_RMSE)
  linear_holdout_RMSE_sd_mean <- sd(c(linear_test_RMSE_mean, linear_validation_RMSE_mean))
  linear_train_predict_value <- as.numeric(predict(object = linear_train_fit$best.model, newdata = train))
  linear_test_predict_value <- as.numeric(predict(object = linear_train_fit$best.model, newdata = test))
  linear_validation_predict_value <- as.numeric(predict(object = linear_train_fit$best.model, newdata = validation))
  linear_predict_value_mean <- mean(c(linear_test_predict_value, linear_validation_predict_value))
  linear_sd[i] <- sd(c(linear_test_predict_value, linear_validation_predict_value))
  linear_sd_mean <- mean(linear_sd)
  linear_holdout_vs_train[i] <- linear_holdout_RMSE_mean / linear_train_RMSE_mean
  linear_holdout_vs_train_mean <- mean(linear_holdout_vs_train)
  linear_holdout_vs_train_range <- range(linear_holdout_vs_train)
  linear_holdout_vs_train_sd <- sd(linear_holdout_vs_train)
  linear_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(linear_test_predict_value, linear_validation_predict_value))
  linear_bias_mean <- mean(linear_bias)
  linear_bias_sd <- sd(linear_bias)
  linear_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(linear_test_predict_value, linear_validation_predict_value))
  linear_MAE_mean <- mean(linear_MAE)
  linear_MAE_sd <- sd(linear_MAE)
  linear_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(linear_test_predict_value, linear_validation_predict_value))
  linear_MSE_mean <- mean(linear_MSE)
  linear_MSE_sd <- sd(linear_MSE)
  linear_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(linear_test_predict_value, linear_validation_predict_value))
  linear_SSE_mean <- mean(linear_SSE)
  linear_SSE_sd <- sd(linear_SSE)
  y_hat_linear <- c(linear_test_predict_value, linear_validation_predict_value)
  linear_ks_p_value[i] <- stats::ks.test(x = y_hat_linear, y = c(train$y, validation$y), exact = TRUE)$p.value
  linear_ks_p_value_mean <- mean(linear_ks_p_value)
  linear_ks_p_value_sd <- sd(linear_ks_p_value)
  linear_ks_stat[i] <- stats::ks.test(x = y_hat_linear, y = c(train$y, validation$y), exact = TRUE)$statistic
  linear_ks_stat_mean <- mean(linear_ks_stat)
  linear_ks_test <- c(linear_ks_stat_mean, linear_ks_p_value_mean)

  linear_end <- Sys.time()
  linear_duration[i] <- linear_end - linear_start
  linear_duration_mean <- mean(linear_duration)
  linear_duration_sd <- sd(linear_duration)

  ####  Model 14 Neuralnet ####
  neuralnet_start <- Sys.time()
  neuralnet_train_fit <- nnet::nnet(train$y ~ ., data = train, size = 0, linout = TRUE, skip = TRUE)
  neuralnet_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = neuralnet_train_fit, newdata = train))
  neuralnet_train_RMSE_mean <- mean(neuralnet_train_RMSE)
  neuralnet_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = neuralnet_train_fit, newdata = test))
  neuralnet_test_RMSE_mean <- mean(neuralnet_test_RMSE)
  neuralnet_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = neuralnet_train_fit, newdata = validation))
  neuralnet_validation_RMSE_mean <- mean(neuralnet_validation_RMSE)
  neuralnet_holdout_RMSE[i] <- mean(c(neuralnet_test_RMSE_mean, neuralnet_validation_RMSE_mean))
  neuralnet_holdout_RMSE_mean <- mean(neuralnet_holdout_RMSE)
  neuralnet_holdout_RMSE_sd_mean <- sd(c(neuralnet_test_RMSE_mean, neuralnet_validation_RMSE_mean))
  neuralnet_train_predict_value <- predict(object = neuralnet_train_fit, newdata = train)
  neuralnet_test_predict_value <- predict(object = neuralnet_train_fit, newdata = test)
  neuralnet_validation_predict_value <- predict(object = neuralnet_train_fit, newdata = validation)
  neuralnet_predict_value_mean <- mean(c(neuralnet_test_predict_value, neuralnet_validation_predict_value))
  neuralnet_sd[i] <- sd(c(neuralnet_test_predict_value, neuralnet_validation_predict_value))
  neuralnet_sd_mean <- mean(neuralnet_sd)
  neuralnet_holdout_vs_train[i] <- neuralnet_holdout_RMSE_mean / neuralnet_train_RMSE_mean
  neuralnet_holdout_vs_train_mean <- mean(neuralnet_holdout_vs_train)
  neuralnet_holdout_vs_train_range <- range(neuralnet_holdout_vs_train)
  neuralnet_holdout_vs_train_sd <- sd(neuralnet_holdout_vs_train)
  y_hat_neuralnet <- c(neuralnet_test_predict_value, neuralnet_validation_predict_value)
  neuralnet_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(neuralnet_test_predict_value, neuralnet_validation_predict_value))
  neuralnet_bias_mean <- mean(neuralnet_bias)
  neuralnet_bias_sd <- sd(neuralnet_bias)
  neuralnet_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(neuralnet_test_predict_value, neuralnet_validation_predict_value))
  neuralnet_MAE_mean <- mean(neuralnet_MAE)
  neuralnet_MAE_sd <- sd(neuralnet_MAE)
  neuralnet_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(neuralnet_test_predict_value, neuralnet_validation_predict_value))
  neuralnet_MSE_mean <- mean(neuralnet_MSE)
  neuralnet_MSE_sd <- sd(neuralnet_MSE)
  neuralnet_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(neuralnet_test_predict_value, neuralnet_validation_predict_value))
  neuralnet_SSE_mean <- mean(neuralnet_SSE)
  neuralnet_SSE_sd <- sd(neuralnet_SSE)
  neuralnet_ks_p_value[i] <- stats::ks.test(x = y_hat_neuralnet, y = c(train$y, validation$y), exact = TRUE)$p.value
  neuralnet_ks_p_value_mean <- mean(neuralnet_ks_p_value)
  neuralnet_ks_p_value_sd <- sd(neuralnet_ks_p_value)
  neuralnet_ks_stat[i] <- stats::ks.test(x = y_hat_neuralnet, y = c(train$y, validation$y), exact = TRUE)$statistic
  neuralnet_ks_stat_mean <- mean(neuralnet_ks_stat)
  neuralnet_ks_test <- c(neuralnet_ks_stat_mean, neuralnet_ks_p_value_mean)

  neuralnet_end <- Sys.time()
  neuralnet_duration[i] <- neuralnet_end - neuralnet_start
  neuralnet_duration_mean <- mean(neuralnet_duration)
  neuralnet_duration_sd <- sd(neuralnet_duration)

  #### Model 15 Partial Least Squares ####
  pls_start <- Sys.time()
  pls_train_fit <- pls::plsr(train$y ~ ., data = train)
  pls_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = pls_train_fit, newdata = train))
  pls_train_RMSE_mean <- mean(pls_train_RMSE)
  pls_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = pls_train_fit, newdata = test))
  pls_test_RMSE_mean <- mean(pls_test_RMSE)
  pls_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = pls_train_fit, newdata = validation))
  pls_validation_RMSE_mean <- mean(pls_validation_RMSE)
  pls_holdout_RMSE[i] <- mean(c(pls_test_RMSE_mean, pls_validation_RMSE_mean))
  pls_holdout_RMSE_mean <- mean(pls_holdout_RMSE)
  pls_holdout_RMSE_sd_mean <- sd(c(pls_test_RMSE_mean, pls_validation_RMSE_mean))
  pls_train_predict_value <- predict(object = pls_train_fit, newdata = train)
  pls_test_predict_value <- predict(object = pls_train_fit, newdata = test)
  pls_validation_predict_value <- predict(object = pls_train_fit, newdata = validation)
  pls_predict_value_mean <- mean(c(pls_test_predict_value, pls_validation_predict_value))
  pls_sd[i] <- sd(c(pls_test_predict_value, pls_validation_predict_value))
  pls_sd_mean <- mean(pls_sd)
  pls_holdout_vs_train[i] <- pls_holdout_RMSE_mean / pls_train_RMSE_mean
  pls_holdout_vs_train_mean <- mean(pls_holdout_vs_train)
  pls_holdout_vs_train_range <- range(pls_holdout_vs_train)
  pls_holdout_vs_train_sd <- sd(pls_holdout_vs_train)
  y_hat_pls <- c(pls_test_predict_value[, , 1], pls_validation_predict_value[, , 1])
  pls_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(pls_test_predict_value, pls_validation_predict_value))
  pls_bias_mean <- mean(pls_bias)
  pls_bias_sd <- sd(pls_bias)
  pls_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(pls_test_predict_value, pls_validation_predict_value))
  pls_MAE_mean <- mean(pls_MAE)
  pls_MAE_sd <- sd(pls_MAE)
  pls_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(pls_test_predict_value, pls_validation_predict_value))
  pls_MSE_mean <- mean(pls_MSE)
  pls_MSE_sd <- sd(pls_MSE)
  pls_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(pls_test_predict_value, pls_validation_predict_value))
  pls_SSE_mean <- mean(pls_SSE)
  pls_SSE_sd <- sd(pls_SSE)
  pls_ks_p_value[i] <- stats::ks.test(x = y_hat_pls, y = c(train$y, validation$y), exact = TRUE)$p.value
  pls_ks_p_value_mean <- mean(pls_ks_p_value)
  pls_ks_p_value_sd <- sd(pls_ks_p_value)
  pls_ks_stat[i] <- stats::ks.test(x = y_hat_pls, y = c(train$y, validation$y), exact = TRUE)$statistic
  pls_ks_stat_mean <- mean(pls_ks_stat)
  pls_ks_test <- c(pls_ks_stat_mean, pls_ks_p_value_mean)

  pls_end <- Sys.time()
  pls_duration[i] <- pls_end - pls_start
  pls_duration_mean <- mean(pls_duration)
  pls_duration_sd <- sd(pls_duration)

  ####  Model 16 Principal Components Analysis ####
  pcr_start <- Sys.time()
  pcr_train_fit <- pls::pcr(train$y ~ ., data = train)
  pcr_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = pcr_train_fit, newdata = train))
  pcr_train_RMSE_mean <- mean(pcr_train_RMSE)
  pcr_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = pcr_train_fit, newdata = test))
  pcr_test_RMSE_mean <- mean(pcr_test_RMSE)
  pcr_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = pcr_train_fit, newdata = validation))
  pcr_validation_RMSE_mean <- mean(pcr_validation_RMSE)
  pcr_holdout_RMSE[i] <- mean(pcr_test_RMSE_mean, pcr_validation_RMSE_mean)
  pcr_holdout_RMSE_mean <- mean(pcr_holdout_RMSE)
  pcr_holdout_RMSE_sd_mean <- sd(c(pcr_test_RMSE_mean, pcr_validation_RMSE_mean))
  pcr_train_predict_value <- predict(object = pcr_train_fit, newdata = train)
  pcr_test_predict_value <- predict(object = pcr_train_fit, newdata = test)
  pcr_validation_predict_value <- predict(object = pcr_train_fit, newdata = validation)
  pcr_predict_value_mean <- mean(c(pcr_test_predict_value, pcr_validation_predict_value))
  pcr_sd[i] <- sd(c(pcr_test_predict_value, pcr_validation_predict_value))
  pcr_sd_mean <- mean(pcr_sd)
  pcr_holdout_vs_train[i] <- pcr_holdout_RMSE_mean / pcr_train_RMSE_mean
  pcr_holdout_vs_train_mean <- mean(pcr_holdout_vs_train)
  pcr_holdout_vs_train_range <- range(pcr_holdout_vs_train)
  pcr_holdout_vs_train_sd <- sd(pcr_holdout_vs_train)
  y_hat_pcr <- c(pcr_test_predict_value[, , 1], pcr_validation_predict_value[, , 1])
  pcr_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(pcr_test_predict_value, pcr_validation_predict_value))
  pcr_bias_mean <- mean(pcr_bias)
  pcr_bias_sd <- sd(pcr_bias)
  pcr_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(pcr_test_predict_value, pcr_validation_predict_value))
  pcr_MAE_mean <- mean(pcr_MAE)
  pcr_MAE_sd <- sd(pcr_MAE)
  pcr_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(pcr_test_predict_value, pcr_validation_predict_value))
  pcr_MSE_mean <- mean(pcr_MSE)
  pcr_MSE_sd <- sd(pcr_MSE)
  pcr_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(pcr_test_predict_value, pcr_validation_predict_value))
  pcr_SSE_mean <- mean(pcr_SSE)
  pcr_SSE_sd <- sd(pcr_SSE)
  pcr_ks_p_value[i] <- stats::ks.test(x = y_hat_pcr, y = c(train$y, validation$y), exact = TRUE)$p.value
  pcr_ks_p_value_mean <- mean(pcr_ks_p_value)
  pcr_ks_p_value_sd <- sd(pcr_ks_p_value)
  pcr_ks_stat[i] <- stats::ks.test(x = y_hat_pcr, y = c(train$y, validation$y), exact = TRUE)$statistic
  pcr_ks_stat_mean <- mean(pcr_ks_stat)
  pcr_ks_test <- c(pcr_ks_stat_mean, pcr_ks_p_value_mean)

  pcr_end <- Sys.time()
  pcr_duration[i] <- pcr_end - pcr_start
  pcr_duration_mean <- mean(pcr_duration)
  pcr_duration_sd <- sd(pcr_duration)

  ####  Model 17 Random Forest ####
  rf_start <- Sys.time()
  rf_train_fit <- e1071::tune.randomForest(x = train, y = train$y, data = train)
  rf_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = rf_train_fit$best.model, newdata = train))
  rf_train_RMSE_mean <- mean(rf_train_RMSE)
  rf_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = rf_train_fit$best.model, newdata = test))
  rf_test_RMSE_mean <- mean(rf_test_RMSE)
  rf_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = rf_train_fit$best.model, newdata = validation))
  rf_validation_RMSE_mean <- mean(rf_validation_RMSE)
  rf_holdout_RMSE[i] <- mean(c(rf_test_RMSE_mean, rf_validation_RMSE_mean))
  rf_holdout_RMSE_mean <- mean(rf_holdout_RMSE)
  rf_holdout_RMSE_sd_mean <- sd(c(rf_test_RMSE_mean, rf_validation_RMSE_mean))
  rf_train_predict_value <- predict(object = rf_train_fit$best.model, newdata = train)
  rf_test_predict_value <- predict(object = rf_train_fit$best.model, newdata = test)
  rf_validation_predict_value <- predict(object = rf_train_fit$best.model, newdata = validation)
  rf_predict_value_mean <- mean(c(rf_test_predict_value, rf_validation_predict_value))
  rf_sd[i] <- sd(c(rf_test_predict_value, rf_validation_predict_value))
  rf_sd_mean <- mean(rf_sd)
  rf_holdout_vs_train[i] <- rf_holdout_RMSE_mean / rf_train_RMSE_mean
  rf_holdout_vs_train_mean <- mean(rf_holdout_vs_train)
  rf_holdout_vs_train_range <- range(rf_holdout_vs_train)
  rf_holdout_vs_train_sd <- sd(rf_holdout_vs_train)
  y_hat_rf <- c(rf_test_predict_value, rf_validation_predict_value)
  rf_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(rf_test_predict_value, rf_validation_predict_value))
  rf_bias_mean <- mean(rf_bias)
  rf_bias_sd <- sd <- sd(rf_bias)
  rf_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(rf_test_predict_value, rf_validation_predict_value))
  rf_MAE_mean <- mean(rf_MAE)
  rf_MAE_sd <- sd(rf_MAE)
  rf_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(rf_test_predict_value, rf_validation_predict_value))
  rf_MSE_mean <- mean(rf_MSE)
  rf_MSE_sd <- sd(rf_MSE)
  rf_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(rf_test_predict_value, rf_validation_predict_value))
  rf_SSE_mean <- mean(rf_SSE)
  rf_SSE_sd <- sd(rf_SSE)
  rf_ks_p_value[i] <- stats::ks.test(x = y_hat_rf, y = c(train$y, validation$y), exact = TRUE)$p.value
  rf_ks_p_value_mean <- mean(rf_ks_p_value)
  rf_ks_p_value_sd <- sd(rf_ks_p_value)
  rf_ks_stat[i] <- stats::ks.test(x = y_hat_rf, y = c(train$y, validation$y), exact = TRUE)$statistic
  rf_ks_stat_mean <- mean(rf_ks_stat)
  rf_ks_test <- c(rf_ks_stat_mean, rf_ks_p_value_mean)

  rf_end <- Sys.time()
  rf_duration[i] <- rf_end - rf_start
  rf_duration_mean <- mean(rf_duration)
  rf_duration_sd <- sd(rf_duration)

  ####  Model 18 Ridge Net ####
  ridge_start <- Sys.time()
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  ridge_model <- glmnet(x, y, alpha = 0)
  ridge_cv <- glmnet::cv.glmnet(x, y, alpha = 0)
  best_ridge_lambda <- ridge_cv$lambda.min
  best_ridge_model <- glmnet(x, y, alpha = 0, lambda = best_ridge_lambda)
  ridge_train_pred <- predict(best_ridge_model, s = best_ridge_lambda, newx = data.matrix(train %>% dplyr::select(-y)))
  ridge_train_RMSE <- Metrics::rmse(actual = y, predicted = ridge_train_pred)
  ridge_train_RMSE_df <- rbind(ridge_train_RMSE_df, ridge_train_RMSE)
  ridge_train_RMSE_mean <- mean(ridge_train_RMSE_df$ridge_train_RMSE[2:nrow(ridge_train_RMSE_df)])
  ## Ridge using the test data set ##

  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  ridge_model <- glmnet(x, y, alpha = 0)
  ridge_cv <- glmnet::cv.glmnet(x, y, alpha = 0)
  best_ridge_lambda <- ridge_cv$lambda.min
  best_ridge_model <- glmnet(x, y, alpha = 0, lambda = best_ridge_lambda)
  ridge_test_pred <- predict(best_ridge_model, s = best_ridge_lambda, newx = data.matrix(test %>% dplyr::select(-y)))
  ridge_test_RMSE <- Metrics::rmse(actual = test$y, predicted = ridge_test_pred)
  ridge_test_RMSE_df <- rbind(ridge_test_RMSE_df, ridge_test_RMSE)
  ridge_test_RMSE_mean <- mean(ridge_test_RMSE_df$ridge_test_RMSE[2:nrow(ridge_test_RMSE_df)])
  ridge_test_predict_value[i] <- round(mean(ridge_test_pred), 4)
  ridge_test_predict_value_mean <- mean(ridge_test_predict_value)
  ridge_test_predict_value_sd[i] <- round(sd(ridge_test_pred), 4)
  ridge_test_predict_value_sd_mean <- mean(ridge_test_predict_value_sd)
  ## Ridge using the validation data set
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  ridge_model <- glmnet(x, y, alpha = 0)
  ridge_cv <- glmnet::cv.glmnet(x, y, alpha = 0)
  best_ridge_lambda <- ridge_cv$lambda.min
  best_ridge_model <- glmnet(x, y, alpha = 0, lambda = best_ridge_lambda)
  ridge_validation_pred <- predict(best_ridge_model, s = best_ridge_lambda, newx = data.matrix(validation %>% dplyr::select(-y)))
  ridge_validation_RMSE <- Metrics::rmse(actual = validation$y, predicted = ridge_validation_pred)
  ridge_validation_RMSE_df <- rbind(ridge_validation_RMSE_df, ridge_validation_RMSE)
  ridge_validation_RMSE_mean <- mean(ridge_validation_RMSE_df$ridge_validation_RMSE[2:nrow(ridge_validation_RMSE_df)])
  ridge_validation_predict_value[i] <- round(mean(ridge_validation_pred), 4)
  ridge_validation_predict_value_mean <- mean(ridge_validation_predict_value)
  ridge_validation_predict_value_sd[i] <- round(sd(ridge_validation_pred), 4)
  ridge_validation_predict_value_sd_mean <- mean(ridge_validation_predict_value_sd)

  ridge_holdout_RMSE <- mean(ridge_test_RMSE_mean, ridge_validation_RMSE_mean)
  ridge_holdout_RMSE_df <- rbind(ridge_holdout_RMSE_df, ridge_holdout_RMSE)
  ridge_holdout_RMSE_mean <- mean(ridge_holdout_RMSE_df$ridge_holdout_RMSE[2:nrow(ridge_holdout_RMSE_df)])

  ridge_holdout_RMSE_sd <- sd(c(ridge_test_RMSE_mean, ridge_validation_RMSE_mean))
  ridge_holdout_RMSE_sd_df <- rbind(ridge_holdout_RMSE_sd, ridge_holdout_RMSE_sd_df)
  ridge_holdout_RMSE_sd_mean <- mean(ridge_holdout_RMSE_sd_df$ridge_holdout_RMSE_sd[2:nrow(ridge_holdout_RMSE_sd_df)])

  ridge_holdout_vs_train <- c(ridge_holdout_RMSE / ridge_train_RMSE)
  ridge_holdout_vs_train_df <- rbind(ridge_holdout_vs_train_df, ridge_holdout_vs_train)
  ridge_holdout_vs_train_mean <- mean(ridge_holdout_vs_train_df$ridge_holdout_vs_train[2:nrow(ridge_holdout_vs_train_df)])
  ridge_holdout_vs_train_sd <- sd(ridge_holdout_vs_train_df$ridge_holdout_vs_train)

  ridge_test_predict_value_mean <- mean(c(ridge_test_predict_value_mean, ridge_validation_predict_value_mean))

  ridge_sd[i] <- mean(c(ridge_test_predict_value_sd_mean, ridge_validation_predict_value_sd_mean))
  ridge_sd_mean <- mean(ridge_sd)

  y_hat_ridge <- as.numeric(c(rowMeans(ridge_test_pred), rowMeans(ridge_validation_pred)))

  ridge_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = as.numeric(c(ridge_test_pred, ridge_validation_pred)))
  ridge_bias_mean <- mean(ridge_bias)
  ridge_bias_sd <- sd(ridge_bias)
  ridge_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = as.numeric(c(ridge_test_pred, ridge_validation_pred)))
  ridge_MAE_mean <- mean(ridge_MAE)
  ridge_MAE_sd <- sd(ridge_MAE)
  ridge_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = as.numeric(c(ridge_test_pred, ridge_validation_pred)))
  ridge_MSE_mean <- mean(ridge_MSE)
  ridge_MSE_sd <- sd(ridge_MSE)
  ridge_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = as.numeric(c(ridge_test_pred, ridge_validation_pred)))
  ridge_SSE_mean <- mean(ridge_SSE)
  ridge_SSE_sd <- sd(ridge_SSE)
  ridge_ks_p_value[i] <- stats::ks.test(x = y_hat_ridge, y = c(train$y, validation$y), exact = TRUE)$p.value
  ridge_ks_p_value_mean <- mean(ridge_ks_p_value)
  ridge_ks_p_value_sd <- sd(ridge_ks_p_value)
  ridge_ks_stat[i] <- stats::ks.test(x = y_hat_ridge, y = c(train$y, validation$y), exact = TRUE)$statistic
  ridge_ks_stat_mean <- mean(ridge_ks_stat)
  ridge_ks_test <- c(ridge_ks_stat_mean, ridge_ks_p_value_mean)

  ridge_end <- Sys.time()
  ridge_duration[i] <- ridge_end - ridge_start
  ridge_duration_mean <- mean(ridge_duration)
  ridge_duration_sd <- sd(ridge_duration)

  ####  Model 20 Rpart (also known as cart) ####
  rpart_start <- Sys.time()
  rpart_train_fit <- rpart::rpart(train$y ~ ., data = train)
  rpart_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = rpart_train_fit, newdata = train))
  rpart_train_RMSE_mean <- mean(rpart_train_RMSE)
  rpart_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = rpart_train_fit, newdata = test))
  rpart_test_RMSE_mean <- mean(rpart_test_RMSE)
  rpart_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = rpart_train_fit, newdata = validation))
  rpart_validation_RMSE_mean <- mean(rpart_validation_RMSE)
  rpart_holdout_RMSE[i] <- mean(c(rpart_test_RMSE_mean, rpart_validation_RMSE_mean))
  rpart_holdout_RMSE_mean <- mean(rpart_holdout_RMSE)
  rpart_holdout_RMSE_sd_mean <- sd(c(rpart_test_RMSE_mean, rpart_validation_RMSE_mean))
  rpart_train_predict_value <- as.numeric(predict(object = rpart::rpart(y ~ ., data = train), newdata = train))
  rpart_test_predict_value <- as.numeric(predict(object = rpart::rpart(y ~ ., data = train), newdata = test))
  rpart_validation_predict_value <- as.numeric(predict(object = rpart::rpart(y ~ ., data = train), newdata = validation))
  rpart_predict_value_mean <- mean(c(rpart_test_predict_value, rpart_validation_predict_value))
  rpart_sd[i] <- sd(rpart_test_predict_value)
  rpart_sd_mean <- mean(rpart_sd)
  rpart_holdout_vs_train[i] <- rpart_holdout_RMSE_mean / rpart_train_RMSE_mean
  rpart_holdout_vs_train_mean <- mean(rpart_holdout_vs_train)
  rpart_holdout_vs_train_range <- range(rpart_holdout_vs_train)
  rpart_holdout_vs_train_sd <- sd(rpart_holdout_vs_train)
  y_hat_rpart <- c(rpart_test_predict_value, rpart_validation_predict_value)
  rpart_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(rpart_test_predict_value, rpart_validation_predict_value))
  rpart_bias_mean <- mean(rpart_bias)
  rpart_bias_sd <- sd(rpart_bias)
  rpart_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(rpart_test_predict_value, rpart_validation_predict_value))
  rpart_MAE_mean <- mean(rpart_MAE)
  rpart_MAE_sd <- sd(rpart_MAE)
  rpart_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(rpart_test_predict_value, rpart_validation_predict_value))
  rpart_MSE_mean <- mean(rpart_MSE)
  rpart_MSE_sd <- sd(rpart_MSE)
  rpart_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(rpart_test_predict_value, rpart_validation_predict_value))
  rpart_SSE_mean <- mean(rpart_SSE)
  rpart_SSE_sd <- sd(rpart_SSE)
  rpart_ks_p_value[i] <- stats::ks.test(x = y_hat_rpart, y = c(train$y, validation$y), exact = TRUE)$p.value
  rpart_ks_p_value_mean <- mean(rpart_ks_p_value)
  rpart_ks_p_value_sd <- sd(rpart_ks_p_value)
  rpart_ks_stat[i] <- stats::ks.test(x = y_hat_rpart, y = c(train$y, validation$y), exact = TRUE)$statistic
  rpart_ks_stat_mean <- mean(rpart_ks_stat)
  rpart_ks_test <- c(rpart_ks_stat_mean, rpart_ks_p_value_mean)

  rpart_end <- Sys.time()
  rpart_duration[i] <- rpart_end - rpart_start
  rpart_duration_mean <- mean(rpart_duration)
  rpart_duration_sd <- sd(rpart_duration)


  ####  Model 21 Support Vector Machines ####
  svm_start <- Sys.time()
  svm_train_fit <- e1071::tune.svm(x = train, y = train$y, data = train)
  svm_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = svm_train_fit$best.model, newdata = train))
  svm_train_RMSE_mean <- mean(svm_train_RMSE)
  svm_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = svm_train_fit$best.model, newdata = test))
  svm_test_RMSE_mean <- mean(svm_test_RMSE)
  svm_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = svm_train_fit$best.model, newdata = validation))
  svm_validation_RMSE_mean <- mean(svm_validation_RMSE)
  svm_holdout_RMSE[i] <- mean(c(svm_test_RMSE_mean, svm_validation_RMSE_mean))
  svm_holdout_RMSE_mean <- mean(svm_holdout_RMSE)
  svm_holdout_RMSE_sd_mean <- sd(svm_validation_RMSE)
  svm_train_predict_value <- as.numeric(predict(object = svm_train_fit$best.model, newdata = train))
  svm_test_predict_value <- as.numeric(predict(object = svm_train_fit$best.model, newdata = test))
  svm_validation_predict_value <- as.numeric(predict(object = svm_train_fit$best.model, newdata = validation))
  svm_predict_value_mean <- mean(c(svm_test_predict_value, svm_validation_predict_value))
  svm_sd[i] <- sd(c(svm_test_predict_value, svm_validation_predict_value))
  svm_sd_mean <- mean(svm_sd)
  svm_holdout_vs_train[i] <- svm_holdout_RMSE_mean / svm_train_RMSE_mean
  svm_holdout_vs_train_mean <- mean(svm_holdout_vs_train)
  svm_holdout_vs_train_range <- range(svm_holdout_vs_train)
  svm_holdout_vs_train_sd <- sd(svm_holdout_vs_train)
  y_hat_svm <- c(svm_test_predict_value, svm_validation_predict_value)
  svm_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(svm_test_predict_value, svm_validation_predict_value))
  svm_bias_mean <- mean(svm_bias)
  svm_bias_sd <- sd(svm_bias)
  svm_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(svm_test_predict_value, svm_validation_predict_value))
  svm_MAE_mean <- mean(svm_MAE)
  svm_MAE_sd <- sd(svm_MAE)
  svm_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(svm_test_predict_value, svm_validation_predict_value))
  svm_MSE_mean <- mean(svm_MSE)
  svm_MSE_sd <- sd(svm_MSE)
  svm_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(svm_test_predict_value, svm_validation_predict_value))
  svm_SSE_mean <- mean(svm_SSE)
  svm_SSE_sd <- sd(svm_SSE)
  svm_ks_p_value[i] <- stats::ks.test(x = y_hat_svm, y = c(train$y, validation$y), exact = TRUE)$p.value
  svm_ks_p_value_mean <- mean(svm_ks_p_value)
  svm_ks_p_value_sd <- sd(svm_ks_p_value)
  svm_ks_stat[i] <- stats::ks.test(x = y_hat_svm, y = c(train$y, validation$y), exact = TRUE)$statistic
  svm_ks_stat_mean <- mean(svm_ks_stat)
  svm_ks_test <- c(svm_ks_stat_mean, svm_ks_p_value_mean)

  svm_end <- Sys.time()
  svm_duration[i] <- svm_end - svm_start
  svm_duration_mean <- mean(svm_duration)
  svm_duration_sd <- sd(svm_duration)


  ####  Model 22 Trees ####
  tree_start <- Sys.time()
  tree_train_fit <- tree::tree(train$y ~ ., data = train)
  tree_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = tree_train_fit, newdata = train))
  tree_train_RMSE_mean <- mean(tree_train_RMSE)
  tree_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = tree_train_fit, newdata = test))
  tree_test_RMSE_mean <- mean(tree_test_RMSE)
  tree_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = tree_train_fit, newdata = validation))
  tree_validation_RMSE_mean <- mean(tree_validation_RMSE)
  tree_holdout_RMSE[i] <- mean(c(tree_test_RMSE_mean, tree_validation_RMSE_mean))
  tree_holdout_RMSE_mean <- mean(tree_holdout_RMSE)
  tree_holdout_RMSE_sd_mean <- sd(c(tree_test_RMSE_mean, tree_validation_RMSE_mean))
  tree_train_predict_value <- as.numeric(predict(object = tree::tree(y ~ ., data = train), newdata = train))
  tree_test_predict_value <- as.numeric(predict(object = tree::tree(y ~ ., data = train), newdata = test))
  tree_validation_predict_value <- as.numeric(predict(object = tree::tree(y ~ ., data = train), newdata = validation))
  tree_predict_value_mean <- mean(c(tree_test_predict_value, tree_validation_predict_value))
  tree_sd[i] <- sd(tree_test_predict_value)
  tree_sd_mean <- mean(tree_sd)
  tree_holdout_vs_train[i] <- tree_holdout_RMSE_mean / tree_train_RMSE_mean
  tree_holdout_vs_train_mean <- mean(tree_holdout_vs_train)
  tree_holdout_vs_train_range <- range(tree_holdout_vs_train)
  tree_holdout_vs_train_sd <- sd(tree_holdout_vs_train)
  y_hat_tree <- c(tree_test_predict_value, tree_validation_predict_value)
  tree_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(tree_test_predict_value, tree_validation_predict_value))
  tree_bias_mean <- mean(tree_bias)
  tree_bias_sd <- sd(tree_bias)
  tree_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = c(tree_test_predict_value, tree_validation_predict_value))
  tree_MAE_mean <- mean(tree_MAE)
  tree_MAE_sd <- sd(tree_MAE)
  tree_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = c(tree_test_predict_value, tree_validation_predict_value))
  tree_MSE_mean <- mean(tree_MSE)
  tree_MSE_sd <- sd(tree_MSE)
  tree_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = c(tree_test_predict_value, tree_validation_predict_value))
  tree_SSE_mean <- mean(tree_SSE)
  tree_SSE_sd <- sd(tree_SSE)
  tree_ks_p_value[i] <- stats::ks.test(x = y_hat_tree, y = c(train$y, validation$y), exact = TRUE)$p.value
  tree_ks_p_value_mean <- mean(tree_ks_p_value)
  tree_ks_p_value_sd <- sd(tree_ks_p_value)
  tree_ks_stat[i] <- stats::ks.test(x = y_hat_tree, y = c(train$y, validation$y), exact = TRUE)$statistic
  tree_ks_stat_mean <- mean(tree_ks_stat)
  tree_ks_test <- c(tree_ks_stat_mean, tree_ks_p_value_mean)

  tree_end <- Sys.time()
  tree_duration[i] <- tree_end - tree_start
  tree_duration_mean <- mean(tree_duration)
  tree_duration_sd <- sd(tree_duration)

  ####  Model 23 XGBoost ####
  xgb_start <- Sys.time()
  train_x <- data.matrix(train[, -ncol(train)])
  train_y <- train[, ncol(train)]

  # define predictor and response variables in test set
  test_x <- data.matrix(test[, -ncol(test)])
  test_y <- test[, ncol(test)]

  # define predictor and response variables in validation set
  validation_x <- data.matrix(validation[, -ncol(validation)])
  validation_y <- validation[, ncol(validation)]

  # define final train, test and validation sets
  xgb_train <- xgboost::xgb.DMatrix(data = train_x, label = train_y)
  xgb_test <- xgboost::xgb.DMatrix(data = test_x, label = test_y)
  xgb_validation <- xgboost::xgb.DMatrix(data = validation_x, label = validation_y)

  # define watchlist
  watchlist <- list(train = xgb_train, validation = xgb_validation)
  watchlist_test <- list(train = xgb_train, test = xgb_test)
  watchlist_validation <- list(train = xgb_train, validation = xgb_validation)

  # fit XGBoost model and display training and validation data at each round

  xgb_model <- xgboost::xgb.train(data = xgb_train, max.depth = 3, watchlist = watchlist_test, nrounds = 70)
  xgb_model_validation <- xgboost::xgb.train(data = xgb_train, max.depth = 3, watchlist = watchlist_validation, nrounds = 70)

  xgboost_min <- which.min(xgb_model$evaluation_log$validation_rmse)
  xgboost_validation.min <- which.min(xgb_model$evaluation_log$validation_rmse)

  xgb_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = xgb_model, newdata = train_x))
  xgb_train_RMSE_mean <- mean(xgb_train_RMSE)
  xgb_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = xgb_model, newdata = test_x))
  xgb_test_RMSE_mean <- mean(xgb_test_RMSE)
  xgb_validation_RMSE[i] <- round(Metrics::rmse(actual = validation$y, predicted = predict(object = xgb_model, newdata = validation_x)), 4)
  xgb_validation_RMSE_mean <- mean(xgb_validation_RMSE)

  xgb_holdout_RMSE[i] <- mean(xgb_test_RMSE_mean, xgb_validation_RMSE_mean)
  xgb_holdout_RMSE_mean <- mean(xgb_holdout_RMSE)
  xgb_holdout_RMSE_sd_mean <- sd(c(xgb_test_RMSE_mean, xgb_validation_RMSE_mean))

  y_hat_xgb <- c(predict(object = xgb_model, newdata = test_x), predict(object = xgb_model, newdata = validation_x))
  xgb_predict_value_mean <- mean(y_hat_xgb)
  xgb_sd_mean <- sd(y_hat_xgb)
  xgb_holdout_vs_train[i] <- xgb_holdout_RMSE_mean / xgb_train_RMSE_mean
  xgb_holdout_vs_train_mean <- mean(xgb_holdout_vs_train)
  xgb_holdout_vs_train_range <- range(xgb_holdout_vs_train)
  xgb_holdout_vs_train_sd <- sd(xgb_holdout_vs_train)

  xgb_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = y_hat_xgb)
  xgb_bias_mean <- mean(xgb_bias)
  xgb_bias_sd <- sd(xgb_bias)
  xgb_MAE[i] <- Metrics::mae(actual = c(test$y, validation$y), predicted = y_hat_xgb)
  xgb_MAE_mean <- mean(xgb_MAE)
  xgb_MAE_sd <- sd(xgb_MAE)
  xgb_MSE[i] <- Metrics::mse(actual = c(test$y, validation$y), predicted = y_hat_xgb)
  xgb_MSE_mean <- mean(xgb_MSE)
  xgb_MSE_sd <- sd(xgb_MSE)
  xgb_SSE[i] <- Metrics::sse(actual = c(test$y, validation$y), predicted = y_hat_xgb)
  xgb_SSE_mean <- mean(xgb_SSE)
  xgb_SSE_sd <- sd(xgb_SSE)
  xgb_ks_p_value[i] <- stats::ks.test(x = y_hat_xgb, y = c(train$y, validation$y), exact = TRUE)$p.value
  xgb_ks_p_value_mean <- mean(xgb_ks_p_value)
  xgb_ks_p_value_sd <- sd(xgb_ks_p_value)
  xgb_ks_stat[i] <- stats::ks.test(x = y_hat_xgb, y = c(train$y, validation$y), exact = TRUE)$statistic
  xgb_ks_stat_mean <- mean(xgb_ks_stat)
  xgb_ks_test <- c(xgb_ks_stat_mean, xgb_ks_p_value_mean)

  xgb_end <- Sys.time()
  xgb_duration[i] <- xgb_end - xgb_start
  xgb_duration_mean <- mean(xgb_duration)
  xgb_duration_sd <- sd(xgb_duration)


  #### Begin weighted ensembles here ####

  ensemble <- data.frame(
    "BagRF" = y_hat_bag_rf * 1 / bag_rf_holdout_RMSE_mean,
    "Bagging" = y_hat_bagging * 1 / bagging_holdout_RMSE_mean,
    "BayesGLM" = y_hat_bayesglm * 1 / bayesglm_holdout_RMSE_mean,
    "BayesRNN" = y_hat_bayesrnn * 1 / bayesrnn_holdout_RMSE_mean,
    "BoostRF" = y_hat_boost_rf * 1 / boost_rf_holdout_RMSE_mean,
    "Cubist" = y_hat_cubist * 1 / cubist_holdout_RMSE_mean,
    "Earth" = y_hat_earth * 1 / earth_holdout_RMSE_mean,
    "Elastic" = y_hat_elastic / elastic_holdout_RMSE,
    "GAM" = y_hat_gam * 1 / gam_holdout_RMSE_mean,
    "GBM" = y_hat_gb * 1 / gb_holdout_RMSE_mean,
    "KNN" = y_hat_knn * 1 / knn_holdout_RMSE_mean,
    "Lasso" = y_hat_lasso / lasso_holdout_RMSE_mean,
    "Linear" = y_hat_linear * 1 / linear_holdout_RMSE_mean,
    "Neuralnet" = y_hat_neuralnet / neuralnet_holdout_RMSE_mean,
    "PCR" = y_hat_pcr * 1 / pcr_holdout_RMSE_mean,
    "PLS" = y_hat_pls * 1 / pls_holdout_RMSE_mean,
    "RandomForest" = y_hat_rf * 1 / rf_holdout_RMSE_mean,
    "Ridge" = y_hat_ridge / ridge_holdout_RMSE_mean,
    "Rpart" = y_hat_rpart * 1 / rpart_holdout_RMSE_mean,
    "SVM" = y_hat_svm * 1 / svm_holdout_RMSE_mean,
    "Tree" = y_hat_tree * 1 / tree_holdout_RMSE_mean,
    "XGBoost" = y_hat_xgb * 1 / xgb_holdout_RMSE_mean
  )

  ensemble$y_ensemble <- c(test$y, validation$y)
  y_ensemble <- c(test$y, validation$y)

  if(sum(is.na(ensemble > 0))){
    ensemble <- ensemble[stats::complete.cases(ensemble), ] # Removes rows with NAs
  }

  ensemble <- Filter(function(x) stats::var(x) != 0, ensemble) # Removes columns with no variation

  tmp <- stats::cor(ensemble) # This section removes strongly correlated (>0.995) rows and columns from the ensemble
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  data_new <- ensemble[, !apply(tmp, 2, function(x) any(abs(x) > remove_ensemble_correlations_greater_than, na.rm = TRUE))]
  ensemble <- data_new # new ensemble without strongly correlated predictors

  if(ensemble_reduction_method == 1){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "exhaustive")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(BIC == min(BIC)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 2){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "forward")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(BIC == min(BIC)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 3){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "backward")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(BIC == min(BIC)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 4){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "seqrep")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(BIC == min(BIC)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 5){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "exhaustive")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(mallows_cp == min(mallows_cp)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }


  if(ensemble_reduction_method == 6){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "forward")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(mallows_cp == min(mallows_cp)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 7){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "backward")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(mallows_cp == min(mallows_cp)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 8){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "seqrep")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(mallows_cp == min(mallows_cp)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  message(noquote(""))
  message("Working on the Ensembles section")
  message(noquote(""))

  head_ensemble <- head(ensemble)
  head_ensemble <- # Head of the ensemble
    reactable::reactable(round(head_ensemble, 4),
                         searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                         striped = TRUE, highlight = TRUE, resizable = TRUE
    )%>%
    reactablefmtr::add_title("Head of the ensemble")


  ensemble_correlation <- cor(ensemble)
  ensemble_correlation <- reactable::reactable(round(cor(ensemble), 4),
                                               searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                               striped = TRUE, highlight = TRUE, resizable = TRUE
  )%>%
    reactablefmtr::add_title("Correlation of the ensemble")


  #### Split the ensemble data into train (60%), test (20%) and validation (20%) ####
  ensemble_idx <- sample(seq(1, 3), size = nrow(ensemble), replace = TRUE, prob = c(train_amount, test_amount, validation_amount))
  ensemble_train <- ensemble[ensemble_idx == 1, ]
  ensemble_test <- ensemble[ensemble_idx == 2, ]
  ensemble_validation <- ensemble[ensemble_idx == 3, ]

  #### Model 21: Ensemble Using Bagged Random Forest tuned ####
  ensemble_bag_rf_start <- Sys.time()
  ensemble_bag_rf_train_fit <- e1071::tune.randomForest(x = ensemble_train, y = ensemble_train$y_ensemble, mtry = ncol(ensemble_train) - 1)
  ensemble_bag_rf_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_bag_rf_train_fit$best.model,
    newdata = ensemble_train
  ))
  ensemble_bag_rf_train_RMSE_mean <- mean(ensemble_bag_rf_train_RMSE)
  ensemble_bag_rf_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_bag_rf_train_fit$best.model,
    newdata = ensemble_test
  ))
  ensemble_bag_rf_test_RMSE_mean <- mean(ensemble_bag_rf_test_RMSE)
  ensemble_bag_rf_validation_RMSE[i] <- Metrics::rmse(
    actual = ensemble_validation$y_ensemble,
    predicted = predict(
      object = ensemble_bag_rf_train_fit$best.model,
      newdata = ensemble_validation
    )
  )
  ensemble_bag_rf_validation_RMSE_mean <- mean(ensemble_bag_rf_validation_RMSE)
  ensemble_bag_rf_holdout_RMSE[i] <- mean(c(ensemble_bag_rf_test_RMSE_mean, ensemble_bag_rf_validation_RMSE_mean))
  ensemble_bag_rf_holdout_RMSE_mean <- mean(ensemble_bag_rf_holdout_RMSE)
  ensemble_bag_rf_holdout_RMSE_sd_mean <- sd(c(ensemble_bag_rf_test_RMSE_mean, ensemble_bag_rf_validation_RMSE_mean))
  ensemble_bag_rf_train_predict_value <- as.numeric(predict(object = ensemble_bag_rf_train_fit$best.model, newdata = ensemble_train))
  ensemble_bag_rf_test_predict_value <- as.numeric(predict(object = ensemble_bag_rf_train_fit$best.model, newdata = ensemble_test))
  ensemble_bag_rf_validation_predict_value <- as.numeric(predict(object = ensemble_bag_rf_train_fit$best.model, newdata = ensemble_validation))
  ensemble_bag_rf_predict_value_mean <- mean(c(ensemble_bag_rf_test_predict_value, ensemble_bag_rf_validation_predict_value))
  ensemble_bag_rf_sd[i] <- sd(c(ensemble_bag_rf_test_predict_value, ensemble_bag_rf_validation_predict_value))
  ensemble_bag_rf_sd_mean <- mean(ensemble_bag_rf_sd)
  ensemble_bag_rf_holdout_vs_train[i] <- ensemble_bag_rf_holdout_RMSE_mean / ensemble_bag_rf_train_RMSE_mean
  ensemble_bag_rf_holdout_vs_train_mean <- mean(ensemble_bag_rf_holdout_vs_train)
  ensemble_bag_rf_holdout_vs_train_range <- range(ensemble_bag_rf_holdout_vs_train)
  ensemble_bag_rf_holdout_vs_train_sd <- sd(ensemble_bag_rf_holdout_vs_train)
  ensemble_y_hat_bag_rf <- c(ensemble_bag_rf_test_predict_value, ensemble_bag_rf_validation_predict_value)
  ensemble_bag_rf_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bag_rf_test_predict_value, ensemble_bag_rf_validation_predict_value))
  ensemble_bag_rf_bias_mean <- mean(ensemble_bag_rf_bias)
  ensemble_bag_rf_bias_sd <- sd(ensemble_bag_rf_bias)
  ensemble_bag_rf_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bag_rf_test_predict_value, ensemble_bag_rf_validation_predict_value))
  ensemble_bag_rf_MAE_mean <- mean(ensemble_bag_rf_MAE)
  ensemble_bag_rf_MAE_sd <- sd(ensemble_bag_rf_MAE)
  ensemble_bag_rf_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bag_rf_test_predict_value, ensemble_bag_rf_validation_predict_value))
  ensemble_bag_rf_MSE_mean <- mean(ensemble_bag_rf_MSE)
  ensemble_bag_rf_MSE_sd <- sd(ensemble_bag_rf_MSE)
  ensemble_bag_rf_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bag_rf_test_predict_value, ensemble_bag_rf_validation_predict_value))
  ensemble_bag_rf_SSE_mean <- mean(ensemble_bag_rf_SSE)
  ensemble_bag_rf_SSE_sd <- sd(ensemble_bag_rf_SSE)
  ensemble_bag_rf_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_bag_rf, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_bag_rf_ks_p_value_mean <- mean(ensemble_bag_rf_ks_p_value)
  ensemble_bag_rf_ks_p_value_sd <- sd(ensemble_bag_rf_ks_p_value)
  ensemble_bag_rf_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_bag_rf, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_bag_rf_ks_stat_mean <- mean(ensemble_bag_rf_ks_stat)
  ensemble_bag_rf_ks_test <- c(ensemble_bag_rf_ks_stat_mean, ensemble_bag_rf_ks_p_value_mean)

  ensemble_bag_rf_end <- Sys.time()
  ensemble_bag_rf_duration[i] <- ensemble_bag_rf_end - ensemble_bag_rf_start
  ensemble_bag_rf_duration_mean <- mean(ensemble_bag_rf_duration)
  ensemble_bag_rf_duration_sd <- sd(ensemble_bag_rf_duration)

  #### Model 22: Ensemble Using Bagging tuned ####
  ensemble_bagging_start <- Sys.time()
  ensemble_bagging_train_fit <- ipred::bagging(formula = y_ensemble ~ ., data = ensemble_train)
  ensemble_bagging_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_bagging_train_fit,
    newdata = ensemble_train
  ))
  ensemble_bagging_train_RMSE_mean <- mean(ensemble_bagging_train_RMSE)
  ensemble_bagging_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_bagging_train_fit,
    newdata = ensemble_test
  ))
  ensemble_bagging_test_RMSE_mean <- mean(ensemble_bagging_test_RMSE)
  ensemble_bagging_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_bagging_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_bagging_validation_RMSE_mean <- mean(ensemble_bagging_validation_RMSE)
  ensemble_bagging_holdout_RMSE[i] <- mean(c(ensemble_bagging_test_RMSE_mean, ensemble_bagging_validation_RMSE_mean))
  ensemble_bagging_holdout_RMSE_mean <- mean(ensemble_bagging_holdout_RMSE)
  ensemble_bagging_holdout_RMSE_sd_mean <- sd(c(ensemble_bagging_test_RMSE_mean, ensemble_bagging_validation_RMSE_mean))
  ensemble_bagging_train_predict_value <- as.numeric(predict(object = ensemble_bagging_train_fit, newdata = ensemble_train))
  ensemble_bagging_test_predict_value <- as.numeric(predict(object = ensemble_bagging_train_fit, newdata = ensemble_test))
  ensemble_bagging_validation_predict_value <- as.numeric(predict(object = ensemble_bagging_train_fit, newdata = ensemble_validation))
  ensemble_bagging_predict_value_mean <- mean(c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value))
  ensemble_bagging_sd[i] <- sd(c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value))
  ensemble_bagging_sd_mean <- mean(ensemble_bagging_sd)
  ensemble_bagging_holdout_vs_train[i] <- ensemble_bagging_holdout_RMSE_mean / ensemble_bagging_train_RMSE_mean
  ensemble_bagging_holdout_vs_train_mean <- mean(ensemble_bagging_holdout_vs_train)
  ensemble_bagging_holdout_vs_train_range <- range(ensemble_bagging_holdout_vs_train)
  ensemble_bagging_holdout_vs_train_sd <- sd(ensemble_bagging_holdout_vs_train)
  ensemble_y_hat_bagging <- c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value)
  ensemble_bagging_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value))
  ensemble_bagging_bias_mean <- mean(ensemble_bagging_bias)
  ensemble_bagging_bias_sd <- sd(ensemble_bagging_bias)
  ensemble_bagging_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value))
  ensemble_bagging_MAE_mean <- mean(ensemble_bagging_MAE)
  ensemble_bagging_MAE_sd <- sd(ensemble_bagging_MAE)
  ensemble_bagging_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value))
  ensemble_bagging_MSE_mean <- mean(ensemble_bagging_MSE)
  ensemble_bagging_MSE_sd <- sd(ensemble_bagging_MSE)
  ensemble_bagging_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value))
  ensemble_bagging_SSE_mean <- mean(ensemble_bagging_SSE)
  ensemble_bagging_SSE_sd <- sd(ensemble_bagging_SSE)
  ensemble_bagging_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_bagging, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_bagging_ks_p_value_mean <- mean(ensemble_bagging_ks_p_value)
  ensemble_bagging_ks_p_value_sd <- sd(ensemble_bag_rf_ks_p_value)
  ensemble_bagging_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_bagging, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_bagging_ks_stat_mean <- mean(ensemble_bagging_ks_stat)
  ensemble_bagging_ks_test <- c(ensemble_bagging_ks_stat_mean, ensemble_bagging_ks_p_value_mean)

  ensemble_bagging_end <- Sys.time()
  ensemble_bagging_duration[i] <- ensemble_bagging_end - ensemble_bagging_start
  ensemble_bagging_duration_mean <- mean(ensemble_bagging_duration)
  ensemble_bagging_duration_sd <- sd(ensemble_bagging_duration)

  #### Model 23: Ensemble Using BayesGLM ####
  ensemble_bayesglm_start <- Sys.time()
  ensemble_bayesglm_train_fit <- arm::bayesglm(y_ensemble ~ ., data = ensemble_train, family = gaussian(link = "identity"))
  ensemble_bayesglm_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_bayesglm_train_fit,
    newdata = ensemble_train
  ))
  ensemble_bayesglm_train_RMSE_mean <- mean(ensemble_bayesglm_train_RMSE)
  ensemble_bayesglm_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_bayesglm_train_fit,
    newdata = ensemble_test
  ))
  ensemble_bayesglm_test_RMSE_mean <- mean(ensemble_bayesglm_test_RMSE)
  ensemble_bayesglm_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_bayesglm_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_bayesglm_validation_RMSE_mean <- mean(ensemble_bayesglm_validation_RMSE)
  ensemble_bayesglm_holdout_RMSE[i] <- mean(c(ensemble_bayesglm_test_RMSE_mean, ensemble_bayesglm_validation_RMSE_mean))
  ensemble_bayesglm_holdout_RMSE_mean <- mean(ensemble_bayesglm_holdout_RMSE)
  ensemble_bayesglm_holdout_RMSE_sd_mean <- sd(c(ensemble_bayesglm_test_RMSE_mean, ensemble_bayesglm_validation_RMSE_mean))
  ensemble_bayesglm_train_predict_value <- as.numeric(predict(object = ensemble_bayesglm_train_fit, newdata = ensemble_train))
  ensemble_bayesglm_test_predict_value <- as.numeric(predict(object = ensemble_bayesglm_train_fit, newdata = ensemble_test))
  ensemble_bayesglm_validation_predict_value <- as.numeric(predict(object = ensemble_bayesglm_train_fit, newdata = ensemble_validation))
  ensemble_bayesglm_predict_value_mean <- mean(c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value))
  ensemble_bayesglm_sd[i] <- sd(c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value))
  ensemble_bayesglm_sd_mean <- mean(ensemble_bayesglm_sd)
  ensemble_bayesglm_holdout_vs_train[i] <- ensemble_bayesglm_holdout_RMSE_mean / ensemble_bayesglm_train_RMSE_mean
  ensemble_bayesglm_holdout_vs_train_mean <- mean(ensemble_bayesglm_holdout_vs_train)
  ensemble_bayesglm_holdout_vs_train_range <- range(ensemble_bayesglm_holdout_vs_train)
  ensemble_bayesglm_holdout_vs_train_sd <- sd(ensemble_bayesglm_holdout_vs_train)
  ensemble_y_hat_bayesglm <- c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value)
  ensemble_bayesglm_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value))
  ensemble_bayesglm_bias_mean <- mean(ensemble_bayesglm_bias)
  ensemble_bayesglm_bias_sd <- sd(ensemble_bayesglm_bias)
  ensemble_bayesglm_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value))
  ensemble_bayesglm_MAE_mean <- mean(ensemble_bayesglm_MAE)
  ensemble_bayesglm_MAE_sd <- sd(ensemble_bayesglm_MAE)
  ensemble_bayesglm_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value))
  ensemble_bayesglm_MSE_mean <- mean(ensemble_bayesglm_MSE)
  ensemble_bayesglm_MSE_sd <- sd(ensemble_bayesglm_MSE)
  ensemble_bayesglm_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value))
  ensemble_bayesglm_SSE_mean <- mean(ensemble_bayesglm_SSE)
  ensemble_bayesglm_SSE_sd <- sd(ensemble_bayesglm_SSE)
  ensemble_bayesglm_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_bayesglm , y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_bayesglm_ks_p_value_mean <- mean(ensemble_bayesglm_ks_p_value)
  ensemble_bayesglm_ks_p_value_sd <- sd(ensemble_bayesglm_ks_p_value)
  ensemble_bayesglm_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_bayesglm , y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_bayesglm_ks_stat_mean <- mean(ensemble_bayesglm_ks_stat)
  ensemble_bayesglm_ks_test <- c(ensemble_bayesglm_ks_stat_mean, ensemble_bayesglm_ks_p_value_mean)

  ensemble_bayesglm_end <- Sys.time()
  ensemble_bayesglm_duration[i] <- ensemble_bayesglm_end - ensemble_bayesglm_start
  ensemble_bayesglm_duration_mean <- mean(ensemble_bayesglm_duration)
  ensemble_bayesglm_duration_sd <- sd(ensemble_bayesglm_duration)

  #### Model 24: Ensemble Using Bayes RNN ####
  ensemble_bayesrnn_start <- Sys.time()
  ensemble_bayesrnn_train_fit <- brnn::brnn(x = as.matrix(ensemble_train), y = ensemble_train$y_ensemble)
  ensemble_bayesrnn_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_bayesrnn_train_fit,
    newdata = ensemble_train
  ))
  ensemble_bayesrnn_train_RMSE_mean <- mean(ensemble_bayesrnn_train_RMSE)
  ensemble_bayesrnn_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_bayesrnn_train_fit,
    newdata = ensemble_test
  ))
  ensemble_bayesrnn_test_RMSE_mean <- mean(ensemble_bayesrnn_test_RMSE)
  ensemble_bayesrnn_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_bayesrnn_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_bayesrnn_validation_RMSE_mean <- mean(ensemble_bayesrnn_validation_RMSE)
  ensemble_bayesrnn_holdout_RMSE[i] <- mean(c(ensemble_bayesrnn_test_RMSE_mean, ensemble_bayesrnn_validation_RMSE_mean))
  ensemble_bayesrnn_holdout_RMSE_mean <- mean(ensemble_bayesrnn_holdout_RMSE)
  ensemble_bayesrnn_holdout_RMSE_sd_mean <- sd(c(ensemble_bayesrnn_test_RMSE_mean, ensemble_bayesrnn_validation_RMSE_mean))
  ensemble_bayesrnn_train_predict_value <- as.numeric(predict(object = ensemble_bayesrnn_train_fit, newdata = ensemble_train))
  ensemble_bayesrnn_test_predict_value <- as.numeric(predict(object = ensemble_bayesrnn_train_fit, newdata = ensemble_test))
  ensemble_bayesrnn_validation_predict_value <- as.numeric(predict(object = ensemble_bayesrnn_train_fit, newdata = ensemble_validation))
  ensemble_bayesrnn_predict_value_mean <- mean(c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value))
  ensemble_bayesrnn_sd[i] <- sd(c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value))
  ensemble_bayesrnn_sd_mean <- mean(ensemble_bayesrnn_sd)
  ensemble_bayesrnn_holdout_vs_train[i] <- ensemble_bayesrnn_holdout_RMSE_mean / ensemble_bayesrnn_train_RMSE_mean
  ensemble_bayesrnn_holdout_vs_train_mean <- mean(ensemble_bayesrnn_holdout_vs_train)
  ensemble_bayesrnn_holdout_vs_train_range <- range(ensemble_bayesrnn_holdout_vs_train)
  ensemble_bayesrnn_holdout_vs_train_sd <- sd(ensemble_bayesrnn_holdout_vs_train)
  ensemble_y_hat_bayesrnn <- c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value)
  ensemble_bayesrnn_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value))
  ensemble_bayesrnn_bias_mean <- mean(ensemble_bayesrnn_bias)
  ensemble_bayesrnn_bias_sd <- sd(ensemble_bayesrnn_bias)
  ensemble_bayesrnn_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value))
  ensemble_bayesrnn_MAE_mean <- mean(ensemble_bayesrnn_MAE)
  ensemble_bayesrnn_MAE_sd <- sd(ensemble_bayesrnn_MAE)
  ensemble_bayesrnn_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value))
  ensemble_bayesrnn_MSE_mean <- mean(ensemble_bayesrnn_MSE)
  ensemble_bayesrnn_MSE_sd <- sd(ensemble_bayesrnn_MSE)
  ensemble_bayesrnn_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value))
  ensemble_bayesrnn_SSE_mean <- mean(ensemble_bayesrnn_SSE)
  ensemble_bayesrnn_SSE_sd <- sd(ensemble_bayesrnn_SSE)
  ensemble_bayesrnn_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_bayesrnn, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_bayesrnn_ks_p_value_mean <- mean(ensemble_bayesrnn_ks_p_value)
  ensemble_bayesrnn_ks_p_value_sd <- sd(ensemble_bayesrnn_ks_p_value)
  ensemble_bayesrnn_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_bayesrnn, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_bayesrnn_ks_stat_mean <- mean(ensemble_bayesrnn_ks_stat)
  ensemble_bayesrnn_ks_test <- c(ensemble_bayesrnn_ks_stat_mean, ensemble_bayesrnn_ks_p_value_mean)

  ensemble_bayesrnn_end <- Sys.time()
  ensemble_bayesrnn_duration[i] <- ensemble_bayesrnn_end - ensemble_bayesrnn_start
  ensemble_bayesrnn_duration_mean <- mean(ensemble_bayesrnn_duration)
  ensemble_bayesrnn_duration_sd <- sd(ensemble_bayesrnn_duration)

  #### Model 25: Ensemble Using Boosted Random Forest tuned ####
  ensemble_boost_rf_start <- Sys.time()
  ensemble_boost_rf_train_fit <- e1071::tune.randomForest(x = ensemble_train, y = ensemble_train$y_ensemble, mtry = ncol(ensemble_train) - 1)
  ensemble_boost_rf_train_RMSE[i] <- Metrics::rmse(
    actual = ensemble_train$y_ensemble,
    predicted = predict(
      object = ensemble_boost_rf_train_fit$best.model,
      newdata = ensemble_train
    )
  )
  ensemble_boost_rf_train_RMSE_mean <- mean(ensemble_boost_rf_train_RMSE)
  ensemble_boost_rf_test_RMSE[i] <- Metrics::rmse(
    actual = ensemble_test$y_ensemble,
    predicted = predict(
      object = ensemble_boost_rf_train_fit$best.model,
      newdata = ensemble_test
    )
  )
  ensemble_boost_rf_test_RMSE_mean <- mean(ensemble_boost_rf_test_RMSE)
  ensemble_boost_rf_validation_RMSE[i] <- Metrics::rmse(
    actual = ensemble_validation$y_ensemble,
    predicted = predict(
      object = ensemble_boost_rf_train_fit$best.model,
      newdata = ensemble_validation
    )
  )
  ensemble_boost_rf_validation_RMSE_mean <- mean(ensemble_boost_rf_validation_RMSE)
  ensemble_boost_rf_holdout_RMSE[i] <- mean(c(ensemble_boost_rf_test_RMSE_mean, ensemble_boost_rf_validation_RMSE_mean))
  ensemble_boost_rf_holdout_RMSE_mean <- mean(ensemble_boost_rf_holdout_RMSE)
  ensemble_boost_rf_holdout_RMSE_sd_mean <- sd(c(ensemble_boost_rf_test_RMSE_mean, ensemble_boost_rf_validation_RMSE_mean))
  ensemble_boost_rf_train_predict_value <- as.numeric(predict(object = ensemble_boost_rf_train_fit$best.model, newdata = ensemble_train))
  ensemble_boost_rf_test_predict_value <- as.numeric(predict(object = ensemble_boost_rf_train_fit$best.model, newdata = ensemble_test))
  ensemble_boost_rf_validation_predict_value <- as.numeric(predict(object = ensemble_boost_rf_train_fit$best.model, newdata = ensemble_validation))
  ensemble_boost_rf_predict_value_mean <- mean(c(ensemble_boost_rf_test_predict_value, ensemble_boost_rf_validation_predict_value))
  ensemble_boost_rf_sd[i] <- sd(c(ensemble_boost_rf_test_predict_value, ensemble_boost_rf_validation_predict_value))
  ensemble_boost_rf_sd_mean <- mean(ensemble_boost_rf_sd)
  ensemble_boost_rf_holdout_vs_train[i] <- ensemble_boost_rf_holdout_RMSE_mean / ensemble_boost_rf_train_RMSE_mean
  ensemble_boost_rf_holdout_vs_train_mean <- mean(ensemble_boost_rf_holdout_vs_train)
  ensemble_boost_rf_holdout_vs_train_range <- range(ensemble_boost_rf_holdout_vs_train)
  ensemble_boost_rf_holdout_vs_train_sd <- sd(ensemble_boost_rf_holdout_vs_train)
  ensemble_y_hat_boost_rf <- c(ensemble_boost_rf_test_predict_value, ensemble_boost_rf_validation_predict_value)
  ensemble_boost_rf_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_boost_rf_test_predict_value, ensemble_boost_rf_validation_predict_value))
  ensemble_boost_rf_bias_mean <- mean(ensemble_boost_rf_bias)
  ensemble_boost_rf_bias_sd <- sd(ensemble_boost_rf_bias)
  ensemble_boost_rf_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_boost_rf_test_predict_value, ensemble_boost_rf_validation_predict_value))
  ensemble_boost_rf_MAE_mean <- mean(ensemble_boost_rf_MAE)
  ensemble_boost_rf_MAE_sd <- sd(ensemble_boost_rf_MAE)
  ensemble_boost_rf_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_boost_rf_test_predict_value, ensemble_boost_rf_validation_predict_value))
  ensemble_boost_rf_MSE_mean <- mean(ensemble_boost_rf_MSE)
  ensemble_boost_rf_MSE_sd <- sd(ensemble_boost_rf_MSE)
  ensemble_boost_rf_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_boost_rf_test_predict_value, ensemble_boost_rf_validation_predict_value))
  ensemble_boost_rf_SSE_mean <- mean(ensemble_boost_rf_SSE)
  ensemble_boost_rf_SSE_sd <- sd(ensemble_boost_rf_SSE)
  ensemble_boost_rf_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_boost_rf, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_boost_rf_ks_p_value_mean <- mean(ensemble_boost_rf_ks_p_value)
  ensemble_boost_rf_ks_p_value_sd <- sd(ensemble_boost_rf_ks_p_value)
  ensemble_boost_rf_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_boost_rf, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_boost_rf_ks_stat_mean <- mean(ensemble_boost_rf_ks_stat)
  ensemble_boost_rf_ks_test <- c(ensemble_boost_rf_ks_stat_mean, ensemble_boost_rf_ks_p_value_mean)

  ensemble_boost_rf_end <- Sys.time()
  ensemble_boost_rf_duration[i] <- ensemble_boost_rf_end - ensemble_boost_rf_start
  ensemble_boost_rf_duration_mean <- mean(ensemble_boost_rf_duration)
  ensemble_boost_rf_duration_sd <- sd(ensemble_boost_rf_duration)

  #### Model 26: Ensemble Using Cubist ####
  ensemble_cubist_start <- Sys.time()
  ensemble_cubist_train_fit <- Cubist::cubist(x = ensemble_train[, 1:ncol(ensemble_train) - 1], y = ensemble_train$y_ensemble)
  ensemble_cubist_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_cubist_train_fit,
    newdata = ensemble_train
  ))
  ensemble_cubist_train_RMSE_mean <- mean(ensemble_cubist_train_RMSE)
  ensemble_cubist_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_cubist_train_fit,
    newdata = ensemble_test
  ))
  ensemble_cubist_test_RMSE_mean <- mean(ensemble_cubist_test_RMSE)
  ensemble_cubist_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_cubist_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_cubist_validation_RMSE_mean <- mean(ensemble_cubist_validation_RMSE)
  ensemble_cubist_holdout_RMSE[i] <- mean(c(ensemble_cubist_test_RMSE_mean, ensemble_cubist_validation_RMSE_mean))
  ensemble_cubist_holdout_RMSE_mean <- mean(ensemble_cubist_holdout_RMSE)
  ensemble_cubist_holdout_RMSE_sd_mean <- sd(c(ensemble_cubist_test_RMSE_mean, ensemble_cubist_validation_RMSE_mean))
  ensemble_cubist_train_predict_value <- as.numeric(predict(object = ensemble_cubist_train_fit, newdata = ensemble_train))
  ensemble_cubist_test_predict_value <- as.numeric(predict(object = ensemble_cubist_train_fit, newdata = ensemble_test))
  ensemble_cubist_validation_predict_value <- as.numeric(predict(object = ensemble_cubist_train_fit, newdata = ensemble_validation))
  ensemble_cubist_predict_value_mean <- mean(c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value))
  ensemble_cubist_sd[i] <- sd(c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value))
  ensemble_cubist_sd_mean <- mean(ensemble_cubist_sd)
  ensemble_cubist_holdout_vs_train[i] <- ensemble_cubist_holdout_RMSE_mean / ensemble_cubist_train_RMSE_mean
  ensemble_cubist_holdout_vs_train_mean <- mean(ensemble_cubist_holdout_vs_train)
  ensemble_cubist_holdout_vs_train_range <- range(ensemble_cubist_holdout_vs_train)
  ensemble_cubist_holdout_vs_train_sd <- sd(ensemble_cubist_holdout_vs_train)
  ensemble_y_hat_cubist <- c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value)
  ensemble_cubist_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value))
  ensemble_cubist_bias_mean <- mean(ensemble_cubist_bias)
  ensemble_cubist_bias_sd <- sd(ensemble_cubist_bias)
  ensemble_cubist_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value))
  ensemble_cubist_MAE_mean <- mean(ensemble_cubist_MAE)
  ensemble_cubist_MAE_sd <- sd(ensemble_cubist_MAE)
  ensemble_cubist_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value))
  ensemble_cubist_MSE_mean <- mean(ensemble_cubist_MSE)
  ensemble_cubist_MSE_sd <- sd(ensemble_cubist_MSE)
  ensemble_cubist_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value))
  ensemble_cubist_SSE_mean <- mean(ensemble_cubist_SSE)
  ensemble_cubist_SSE_sd <- sd(ensemble_cubist_SSE)
  ensemble_cubist_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_cubist, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_cubist_ks_p_value_mean <- mean(ensemble_cubist_ks_p_value)
  ensemble_cubist_ks_p_value_sd <- sd(ensemble_cubist_ks_p_value)
  ensemble_cubist_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_cubist, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_cubist_ks_stat_mean <- mean(ensemble_cubist_ks_stat)
  ensemble_cubist_ks_test <- c(ensemble_cubist_ks_stat_mean, ensemble_cubist_ks_p_value_mean)

  ensemble_cubist_end <- Sys.time()
  ensemble_cubist_duration[i] <- ensemble_cubist_end - ensemble_cubist_start
  ensemble_cubist_duration_mean <- mean(ensemble_cubist_duration)
  ensemble_cubist_duration_sd <- sd(ensemble_cubist_duration)

  #### Model 26: Ensemble Using Earth ####
  ensemble_earth_start <- Sys.time()
  ensemble_earth_train_fit <- earth::earth(x = ensemble_train[, 1:ncol(ensemble_train) - 1], y = ensemble_train$y_ensemble)
  ensemble_earth_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_earth_train_fit,
    newdata = ensemble_train
  ))
  ensemble_earth_train_RMSE_mean <- mean(ensemble_earth_train_RMSE)
  ensemble_earth_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_earth_train_fit,
    newdata = ensemble_test
  ))
  ensemble_earth_test_RMSE_mean <- mean(ensemble_earth_test_RMSE)
  ensemble_earth_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_earth_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_earth_validation_RMSE_mean <- mean(ensemble_earth_validation_RMSE)
  ensemble_earth_holdout_RMSE[i] <- mean(c(ensemble_earth_test_RMSE_mean, ensemble_earth_validation_RMSE_mean))
  ensemble_earth_holdout_RMSE_mean <- mean(ensemble_earth_holdout_RMSE)
  ensemble_earth_holdout_RMSE_sd_mean <- sd(c(ensemble_earth_test_RMSE_mean, ensemble_earth_validation_RMSE_mean))
  ensemble_earth_train_predict_value <- as.numeric(predict(object = ensemble_earth_train_fit, newdata = ensemble_train))
  ensemble_earth_test_predict_value <- as.numeric(predict(object = ensemble_earth_train_fit, newdata = ensemble_test))
  ensemble_earth_validation_predict_value <- as.numeric(predict(object = ensemble_earth_train_fit, newdata = ensemble_validation))
  ensemble_earth_predict_value_mean <- mean(c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value))
  ensemble_earth_sd[i] <- sd(c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value))
  ensemble_earth_sd_mean <- mean(ensemble_earth_sd)
  ensemble_earth_holdout_vs_train[i] <- ensemble_earth_holdout_RMSE_mean / ensemble_earth_train_RMSE_mean
  ensemble_earth_holdout_vs_train_mean <- mean(ensemble_earth_holdout_vs_train)
  ensemble_earth_holdout_vs_train_range <- range(ensemble_earth_holdout_vs_train)
  ensemble_earth_holdout_vs_train_sd <- sd(ensemble_earth_holdout_vs_train)
  ensemble_y_hat_earth <- c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value)
  ensemble_earth_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value))
  ensemble_earth_bias_mean <- mean(ensemble_earth_bias)
  ensemble_earth_bias_sd <- sd(ensemble_earth_bias)
  ensemble_earth_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value))
  ensemble_earth_MAE_mean <- mean(ensemble_earth_MAE)
  ensemble_earth_MAE_sd <- sd(ensemble_earth_MAE)
  ensemble_earth_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value))
  ensemble_earth_MSE_mean <- mean(ensemble_earth_MSE)
  ensemble_earth_MSE_sd <- sd(ensemble_earth_MSE)
  ensemble_earth_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value))
  ensemble_earth_SSE_mean <- mean(ensemble_earth_SSE)
  ensemble_earth_SSE_sd <- sd(ensemble_earth_SSE)
  ensemble_earth_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_earth, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_earth_ks_p_value_mean <- mean(ensemble_earth_ks_p_value)
  ensemble_earth_ks_p_value_sd <- sd(ensemble_earth_ks_p_value)
  ensemble_earth_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_earth, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_earth_ks_stat_mean <- mean(ensemble_earth_ks_stat)
  ensemble_earth_ks_test <- c(ensemble_earth_ks_stat_mean, ensemble_earth_ks_p_value_mean)

  ensemble_earth_end <- Sys.time()
  ensemble_earth_duration[i] <- ensemble_earth_end - ensemble_earth_start
  ensemble_earth_duration_mean <- mean(ensemble_earth_duration)
  ensemble_earth_duration_sd <- sd(ensemble_earth_duration)

  #### Ensembles Using Elastic ####
  ensemble_elastic_start <- Sys.time()

  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_elastic_model <- glmnet(ensemble_x, ensemble_y, alpha = 0.5)
  ensemble_elastic_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 0.5)
  ensemble_best_elastic_lambda <- ensemble_elastic_cv$lambda.min
  ensemble_best_elastic_model <- glmnet(ensemble_x, ensemble_y, alpha = 0, lambda = ensemble_best_elastic_lambda)
  ensemble_elastic_train_RMSE <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))))
  ensemble_elastic_train_RMSE_df <- rbind(ensemble_elastic_train_RMSE_df, ensemble_elastic_train_RMSE)
  ensemble_elastic_train_RMSE_mean <- mean(ensemble_elastic_train_RMSE_df$ensemble_elastic_train_RMSE[2:nrow(ensemble_elastic_train_RMSE_df)])
  ensemble_elastic_test_RMSE <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_elastic_test_RMSE_df <- rbind(ensemble_elastic_test_RMSE_df, ensemble_elastic_test_RMSE)
  ensemble_elastic_test_RMSE_mean <- mean(ensemble_elastic_test_RMSE_df$ensemble_elastic_test_RMSE[2:nrow(ensemble_elastic_test_RMSE_df)])
  ensemble_elastic_test_predict_value <- rowMeans(predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_elastic_test_predict_value_mean <- mean(ensemble_elastic_test_predict_value)
  ensemble_elastic_test_predict_value_sd <- sd(ensemble_elastic_test_predict_value)
  ensemble_elastic_test_predict_value_sd_mean <- mean(ensemble_elastic_test_predict_value_sd, na.rm = TRUE)
  ## Elastic using the validation data set
  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_elastic_model <- glmnet(ensemble_x, ensemble_y, alpha = 0.5)
  ensemble_elastic_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 0.5)
  ensemble_best_elastic_lambda <- ensemble_elastic_cv$lambda.min
  ensemble_best_elastic_model <- glmnet(ensemble_x, ensemble_y, alpha = 0, lambda = ensemble_best_elastic_lambda)
  ensemble_elastic_validation_pred <- predict(ensemble_best_elastic_model, s = ensemble_best_elastic_lambda, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble)))
  ensemble_elastic_validation_RMSE <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_elastic_validation_RMSE_df <- rbind(ensemble_elastic_validation_RMSE_df, ensemble_elastic_validation_RMSE)
  ensemble_elastic_validation_RMSE_mean <- mean(ensemble_elastic_validation_RMSE_df$ensemble_elastic_validation_RMSE[2:nrow(ensemble_elastic_validation_RMSE_df)])
  ensemble_elastic_validation_predict_value <- rowMeans(predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_elastic_validation_predict_value_mean <- mean(ensemble_elastic_validation_predict_value)
  ensemble_elastic_validation_predict_value_sd <- round(sd(ensemble_elastic_validation_pred), 4)
  ensemble_elastic_validation_predict_value_sd_df <- rbind(ensemble_elastic_validation_predict_value_sd_df, ensemble_elastic_validation_predict_value_sd)
  ensemble_elastic_validation_predict_value_sd_mean <- mean(ensemble_elastic_validation_predict_value_sd_df$ensemble_elastic_validation_predict_value_sd[2:nrow(ensemble_elastic_validation_predict_value_sd_df)])
  ensemble_elastic_holdout_RMSE[i] <- mean(c(ensemble_elastic_test_RMSE, ensemble_elastic_validation_RMSE))
  ensemble_elastic_holdout_RMSE_mean <- mean(ensemble_elastic_holdout_RMSE)
  ensemble_elastic_holdout_RMSE_sd_mean <- mean(sd(c(ensemble_elastic_holdout_RMSE)))
  ensemble_elastic_test_sd <- sd(ensemble_elastic_test_predict_value, na.rm = TRUE)
  ensemble_elastic_test_sd_df <- rbind(ensemble_elastic_test_sd_df, ensemble_elastic_test_sd)
  ensemble_elastic_test_sd_mean <- mean(ensemble_elastic_test_sd_df$ensemble_elastic_test_sd[2:nrow(ensemble_elastic_test_sd_df)])
  ensemble_elastic_validation_sd <- sd(ensemble_elastic_validation_predict_value, na.rm = TRUE)
  ensemble_elastic_validation_sd_df <- rbind(ensemble_elastic_validation_sd_df, ensemble_elastic_validation_sd)
  ensemble_elastic_validation_sd_mean <- mean(ensemble_elastic_validation_sd_df$ensemble_elastic_validation_sd[2:nrow(ensemble_elastic_validation_sd_df)])
  ensemble_elastic_holdout_vs_train <- ensemble_elastic_holdout_RMSE_mean / ensemble_elastic_train_RMSE_mean
  ensemble_elastic_holdout_vs_train_df <- rbind(ensemble_elastic_holdout_vs_train_df, ensemble_elastic_holdout_vs_train)
  ensemble_elastic_holdout_vs_train_mean <- mean(ensemble_elastic_holdout_vs_train_df$ensemble_elastic_holdout_vs_train[2:nrow(ensemble_elastic_holdout_vs_train_df)])
  ensemble_elastic_holdout_vs_train_range <- range(ensemble_elastic_holdout_vs_train_df$ensemble_elastic_holdout_vs_train[2:nrow(ensemble_elastic_holdout_vs_train_df)])
  ensemble_elastic_holdout_vs_train_sd <- sd(ensemble_elastic_holdout_vs_train_df$ensemble_elastic_holdout_vs_train)
  ensemble_elastic_predict_value_mean <- mean(c(ensemble_elastic_test_predict_value_mean, ensemble_elastic_validation_predict_value_mean))
  ensemble_elastic_sd_mean <- mean(c(ensemble_elastic_test_sd_mean, ensemble_elastic_validation_sd_mean))
  ensemble_y_hat_elastic <- predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble)))
  y_hat_ensemble_elastic <- c(ensemble_elastic_test_predict_value, ensemble_elastic_validation_predict_value)
  ensemble_elastic_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_elastic_test_predict_value, ensemble_elastic_validation_predict_value))
  ensemble_elastic_bias_mean <- mean(ensemble_elastic_bias)
  ensemble_elastic_bias_sd <- sd(ensemble_elastic_bias)
  ensemble_elastic_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_elastic_test_predict_value, ensemble_elastic_validation_predict_value))
  ensemble_elastic_MAE_mean <- mean(ensemble_elastic_MAE)
  ensemble_elastic_MAE_sd <- sd(ensemble_elastic_MAE)
  ensemble_elastic_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_elastic_test_predict_value, ensemble_elastic_validation_predict_value))
  ensemble_elastic_MSE_mean <- mean(ensemble_elastic_MSE)
  ensemble_elastic_MSE_sd <- sd(ensemble_elastic_MSE)
  ensemble_elastic_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_elastic_test_predict_value, ensemble_elastic_validation_predict_value))
  ensemble_elastic_SSE_mean <- mean(ensemble_elastic_SSE)
  ensemble_elastic_SSE_sd <- sd(ensemble_elastic_SSE)
  ensemble_elastic_ks_p_value[i] <- stats::ks.test(x = y_hat_ensemble_elastic, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_elastic_ks_p_value_mean <- mean(ensemble_elastic_ks_p_value)
  ensemble_elastic_ks_p_value_sd <- sd(ensemble_elastic_ks_p_value)
  ensemble_elastic_ks_stat[i] <- stats::ks.test(x = y_hat_ensemble_elastic, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_elastic_ks_stat_mean <- mean(ensemble_elastic_ks_stat)
  ensemble_elastic_ks_test <- c(ensemble_elastic_ks_stat_mean, ensemble_elastic_ks_p_value_mean)

  ensemble_elastic_end <- Sys.time()
  ensemble_elastic_duration[i] <- ensemble_elastic_end - ensemble_elastic_start
  ensemble_elastic_duration_mean <- mean(ensemble_elastic_duration)
  ensemble_elastic_duration_sd <- sd(ensemble_elastic_duration)


  #### Model 29: Ensemble Gradient Boosted ####
  ensemble_gb_start <- Sys.time()
  ensemble_gb_train_fit <- gbm::gbm(ensemble_train$y_ensemble ~ .,
                                    data = ensemble_train, distribution = "gaussian", n.trees = 100,
                                    shrinkage = 0.1, interaction.depth = 10
  )
  ensemble_gb_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_gb_train_fit,
    newdata = ensemble_train
  ))
  ensemble_gb_train_RMSE_mean <- mean(ensemble_gb_train_RMSE)
  ensemble_gb_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_gb_train_fit,
    newdata = ensemble_test
  ))
  ensemble_gb_test_RMSE_mean <- mean(ensemble_gb_test_RMSE)
  ensemble_gb_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_gb_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_gb_validation_RMSE_mean <- mean(ensemble_gb_validation_RMSE)
  ensemble_gb_holdout_RMSE[i] <- mean(c(ensemble_gb_test_RMSE_mean, ensemble_gb_validation_RMSE_mean))
  ensemble_gb_holdout_RMSE_mean <- mean(ensemble_gb_holdout_RMSE)
  ensemble_gb_holdout_RMSE_sd_mean <- sd(c(ensemble_gb_test_RMSE_mean, ensemble_gb_validation_RMSE_mean))
  ensemble_gb_train_predict_value <- as.numeric(predict(object = ensemble_gb_train_fit, newdata = ensemble_train))
  ensemble_gb_test_predict_value <- as.numeric(predict(object = ensemble_gb_train_fit, newdata = ensemble_test))
  ensemble_gb_validation_predict_value <- as.numeric(predict(object = ensemble_gb_train_fit, newdata = ensemble_validation))
  ensemble_gb_predict_value_mean <- mean(c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value))
  ensemble_gb_sd[i] <- sd(c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value))
  ensemble_gb_sd_mean <- mean(ensemble_gb_sd)
  ensemble_gb_holdout_vs_train[i] <- ensemble_gb_holdout_RMSE_mean / ensemble_gb_train_RMSE_mean
  ensemble_gb_holdout_vs_train_mean <- mean(ensemble_gb_holdout_vs_train)
  ensemble_gb_holdout_vs_train_range <- range(ensemble_gb_holdout_vs_train)
  ensemble_gb_holdout_vs_train_sd <- sd(ensemble_gb_holdout_vs_train)
  ensemble_y_hat_gb <- c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value)
  ensemble_gb_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value))
  ensemble_gb_bias_mean <- mean(ensemble_gb_bias)
  ensemble_gb_bias_sd <- sd(ensemble_gb_bias)
  ensemble_gb_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value))
  ensemble_gb_MAE_mean <- mean(ensemble_gb_MAE)
  ensemble_gb_MAE_sd <- sd(ensemble_gb_MAE)
  ensemble_gb_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value))
  ensemble_gb_MSE_mean <- mean(ensemble_gb_MSE)
  ensemble_gb_MSE_sd <- sd(ensemble_gb_MSE)
  ensemble_gb_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value))
  ensemble_gb_SSE_mean <- mean(ensemble_gb_SSE)
  ensemble_gb_SSE_sd <- sd(ensemble_gb_SSE)
  ensemble_gb_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_gb, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_gb_ks_p_value_mean <- mean(ensemble_gb_ks_p_value)
  ensemble_gb_ks_p_value_sd <- sd(ensemble_gb_ks_p_value)
  ensemble_gb_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_gb, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_gb_ks_stat_mean <- mean(ensemble_gb_ks_stat)
  ensemble_gb_ks_test <- c(ensemble_gb_ks_stat_mean, ensemble_gb_ks_p_value_mean)

  ensemble_gb_end <- Sys.time()
  ensemble_gb_duration[i] <- ensemble_gb_end - ensemble_gb_start
  ensemble_gb_duration_mean <- mean(ensemble_gb_duration)
  ensemble_gb_duration_sd <- sd(ensemble_gb_duration)

  #### Ensemble Using K-Nearest Neighbors ####
  ensemble_knn_start <- Sys.time()

  ensemble_knn_model <- e1071::tune.gknn(x = ensemble_train, y = ensemble_train$y_ensemble, k = c(1:25), scale = TRUE)
  ensemble_knn_train_RMSE <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_knn_model$best.model, k = ensemble_knn_model$best.model$k, newdata = ensemble_train))
  ensemble_knn_train_RMSE_df <- rbind(ensemble_knn_train_RMSE_df, ensemble_knn_train_RMSE)
  ensemble_knn_train_RMSE_mean <- mean(ensemble_knn_train_RMSE_df$ensemble_knn_train_RMSE[2:nrow(ensemble_knn_train_RMSE_df)])
  ensemble_knn_test_RMSE <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_knn_model$best.model, k = ensemble_knn_model$best.model$k, newdata = ensemble_test))
  ensemble_knn_test_RMSE_df <- rbind(ensemble_knn_test_RMSE_df, ensemble_knn_test_RMSE)
  ensemble_knn_test_RMSE_mean <- mean(ensemble_knn_test_RMSE_df$ensemble_knn_test_RMSE[2:nrow(ensemble_knn_test_RMSE_df)])
  ensemble_knn_validation_RMSE <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_knn_model$best.model, k = ensemble_knn_model$best.model$k, newdata = ensemble_validation))
  ensemble_knn_validation_RMSE_df <- rbind(ensemble_knn_validation_RMSE_df, ensemble_knn_validation_RMSE)
  ensemble_knn_validation_RMSE_mean <- mean(ensemble_knn_validation_RMSE_df$ensemble_knn_validation_RMSE[2:nrow(ensemble_knn_validation_RMSE_df)])
  ensemble_knn_holdout_RMSE[i] <- mean(c(ensemble_knn_test_RMSE, ensemble_knn_validation_RMSE))
  ensemble_knn_holdout_RMSE_mean <- mean(ensemble_knn_holdout_RMSE)
  ensemble_knn_holdout_RMSE_sd_mean <- mean(sd(c(ensemble_knn_test_RMSE, ensemble_knn_validation_RMSE)))
  ensemble_knn_test_predict_value <- as.numeric(predict(object = ensemble_knn_model$best.model, k = ensemble_knn_model$best.model$k, newdata = ensemble_test))
  ensemble_knn_test_predict_value_mean <- mean(ensemble_knn_test_predict_value)
  ensemble_knn_validation_predict_value <- as.numeric(predict(object = ensemble_knn_model$best.model, k = ensemble_knn_model$best.model$k, newdata = ensemble_validation))
  ensemble_knn_validation_predict_value_mean <- mean(ensemble_knn_validation_predict_value)
  ensemble_knn_predict_value_mean <- mean(ensemble_knn_test_predict_value_mean, ensemble_knn_validation_predict_value_mean)
  ensemble_knn_test_sd <- sd(ensemble_knn_test_predict_value)
  ensemble_knn_test_sd_df <- rbind(ensemble_knn_test_sd_df, ensemble_knn_test_sd)
  ensemble_knn_test_sd_mean <- mean(ensemble_knn_test_sd_df$ensemble_knn_test_sd[2:nrow(ensemble_knn_test_sd_df)])
  ensemble_knn_validation_sd <- sd(ensemble_knn_validation_predict_value)
  ensemble_knn_validation_sd_df <- rbind(ensemble_knn_validation_sd_df, ensemble_knn_validation_sd)
  ensemble_knn_validation_sd_mean <- mean(ensemble_knn_validation_sd_df$ensemble_knn_validation_sd[2:nrow(ensemble_knn_validation_sd_df)])
  ensemble_knn_sd_mean <- mean(c(ensemble_knn_test_sd_mean, ensemble_knn_validation_sd_mean))
  ensemble_knn_holdout_vs_train[i] <- ensemble_knn_holdout_RMSE_mean / ensemble_knn_train_RMSE_mean
  ensemble_knn_holdout_vs_train_mean <- mean(ensemble_knn_holdout_vs_train)
  ensemble_knn_holdout_vs_train_range <- range(ensemble_knn_holdout_vs_train)
  ensemble_knn_holdout_vs_train_sd <- sd(ensemble_knn_holdout_vs_train)
  ensemble_y_hat_knn <- c(ensemble_knn_test_predict_value, ensemble_knn_validation_predict_value)
  ensemble_knn_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_knn_test_predict_value, ensemble_knn_validation_predict_value))
  ensemble_knn_bias_mean <- mean(ensemble_knn_bias)
  ensemble_knn_bias_sd <- sd(ensemble_knn_bias)
  ensemble_knn_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_knn_test_predict_value, ensemble_knn_validation_predict_value))
  ensemble_knn_MAE_mean <- mean(ensemble_knn_MAE)
  ensemble_knn_MAE_sd <- sd(ensemble_knn_MAE)
  ensemble_knn_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_knn_test_predict_value, ensemble_knn_validation_predict_value))
  ensemble_knn_MSE_mean <- mean(ensemble_knn_MSE)
  ensemble_knn_MSE_sd <- sd(ensemble_knn_MSE)
  ensemble_knn_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_knn_test_predict_value, ensemble_knn_validation_predict_value))
  ensemble_knn_SSE_mean <- mean(ensemble_knn_SSE)
  ensemble_knn_SSE_sd <- sd(ensemble_knn_SSE)
  ensemble_knn_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_knn, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_knn_ks_p_value_mean <- mean(ensemble_knn_ks_p_value)
  ensemble_knn_ks_p_value_sd <- sd(ensemble_knn_ks_p_value)
  ensemble_knn_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_knn, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_knn_ks_stat_mean <- mean(ensemble_knn_ks_stat)
  ensemble_knn_ks_test <- c(ensemble_knn_ks_stat_mean, ensemble_knn_ks_p_value_mean)

  ensemble_knn_end <- Sys.time()
  ensemble_knn_duration[i] <- ensemble_knn_end - ensemble_knn_start
  ensemble_knn_duration_mean <- mean(ensemble_knn_duration)
  ensemble_knn_duration_sd <- sd(ensemble_knn_duration)

  #### Ensembles Using lasso ####
  ensemble_lasso_start <- Sys.time()

  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_lasso_model <- glmnet(ensemble_x, ensemble_y, alpha = 1)
  ensemble_lasso_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 1)
  ensemble_best_lasso_lambda <- ensemble_lasso_cv$lambda.min
  ensemble_best_lasso_model <- glmnet(ensemble_x, ensemble_y, alpha = 1, lambda = ensemble_best_lasso_lambda)
  ensemble_lasso_train_RMSE <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))))
  ensemble_lasso_train_RMSE_df <- rbind(ensemble_lasso_train_RMSE_df, ensemble_lasso_train_RMSE)
  ensemble_lasso_train_RMSE_mean <- mean(ensemble_lasso_train_RMSE_df$ensemble_lasso_train_RMSE[2:nrow(ensemble_lasso_train_RMSE_df)])
  ensemble_lasso_test_RMSE <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_lasso_test_RMSE_df <- rbind(ensemble_lasso_test_RMSE_df, ensemble_lasso_test_RMSE)
  ensemble_lasso_test_RMSE_mean <- mean(ensemble_lasso_test_RMSE_df$ensemble_lasso_test_RMSE[2:nrow(ensemble_lasso_test_RMSE_df)])
  ensemble_lasso_test_predict_value <- rowMeans(predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_lasso_test_predict_value_mean <- mean(ensemble_lasso_test_predict_value)
  ensemble_lasso_test_predict_value_sd <- sd(ensemble_lasso_test_predict_value)
  ensemble_lasso_test_predict_value_sd_mean <- mean(ensemble_lasso_test_predict_value_sd, na.rm = TRUE)
  ## lasso using the validation data set
  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_lasso_model <- glmnet(ensemble_x, ensemble_y, alpha = 1)
  ensemble_lasso_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 1)
  ensemble_best_lasso_lambda <- ensemble_lasso_cv$lambda.min
  ensemble_best_lasso_model <- glmnet(ensemble_x, ensemble_y, alpha = 1, lambda = ensemble_best_lasso_lambda)
  ensemble_lasso_validation_pred <- predict(ensemble_best_lasso_model, s = ensemble_best_lasso_lambda, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble)))
  ensemble_lasso_validation_RMSE <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_lasso_validation_RMSE_df <- rbind(ensemble_lasso_validation_RMSE_df, ensemble_lasso_validation_RMSE)
  ensemble_lasso_validation_RMSE_mean <- mean(ensemble_lasso_validation_RMSE_df$ensemble_lasso_validation_RMSE[2:nrow(ensemble_lasso_validation_RMSE_df)])
  ensemble_lasso_validation_predict_value <- rowMeans(predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_lasso_validation_predict_value_mean <- mean(ensemble_lasso_validation_predict_value)
  ensemble_lasso_validation_predict_value_sd <- round(sd(ensemble_lasso_validation_pred), 4)
  ensemble_lasso_validation_predict_value_sd_df <- rbind(ensemble_lasso_validation_predict_value_sd_df, ensemble_lasso_validation_predict_value_sd)
  ensemble_lasso_validation_predict_value_sd_mean <- mean(ensemble_lasso_validation_predict_value_sd_df$ensemble_lasso_validation_predict_value_sd[2:nrow(ensemble_lasso_validation_predict_value_sd_df)])
  ensemble_lasso_holdout_RMSE[i] <- mean(c(ensemble_lasso_test_RMSE, ensemble_lasso_validation_RMSE))
  ensemble_lasso_holdout_RMSE_mean <- mean(ensemble_lasso_holdout_RMSE)
  ensemble_lasso_holdout_RMSE_sd_mean <- mean(sd(c(ensemble_lasso_test_RMSE, ensemble_lasso_validation_RMSE)))
  ensemble_lasso_test_sd <- sd(ensemble_lasso_test_predict_value, na.rm = TRUE)
  ensemble_lasso_test_sd_df <- rbind(ensemble_lasso_test_sd_df, ensemble_lasso_test_sd)
  ensemble_lasso_test_sd_mean <- mean(ensemble_lasso_test_sd_df$ensemble_lasso_test_sd[2:nrow(ensemble_lasso_test_sd_df)])
  ensemble_lasso_validation_sd <- sd(ensemble_lasso_validation_predict_value, na.rm = TRUE)
  ensemble_lasso_validation_sd_df <- rbind(ensemble_lasso_validation_sd_df, ensemble_lasso_validation_sd)
  ensemble_lasso_validation_sd_mean <- mean(ensemble_lasso_validation_sd_df$ensemble_lasso_validation_sd[2:nrow(ensemble_lasso_validation_sd_df)])
  ensemble_lasso_holdout_vs_train <- ensemble_lasso_holdout_RMSE_mean / ensemble_lasso_train_RMSE_mean
  ensemble_lasso_holdout_vs_train_df <- rbind(ensemble_lasso_holdout_vs_train_df, ensemble_lasso_holdout_vs_train)
  ensemble_lasso_holdout_vs_train_mean <- mean(ensemble_lasso_holdout_vs_train_df$ensemble_lasso_holdout_vs_train[2:nrow(ensemble_lasso_holdout_vs_train_df)])
  ensemble_lasso_holdout_vs_train_range <- range(ensemble_lasso_holdout_vs_train_df$ensemble_lasso_holdout_vs_train[2:nrow(ensemble_lasso_holdout_vs_train_df)])
  ensemble_lasso_holdout_vs_train_sd <- sd(ensemble_lasso_holdout_vs_train_df$ensemble_lasso_holdout_vs_train)
  ensemble_lasso_predict_value_mean <- mean(c(ensemble_lasso_test_predict_value_mean, ensemble_lasso_validation_predict_value_mean))
  ensemble_lasso_sd[i] <- sd(c(ensemble_lasso_test_predict_value_mean, ensemble_lasso_validation_predict_value_mean))
  ensemble_lasso_sd_mean <- mean(c(ensemble_lasso_test_sd, ensemble_lasso_validation_sd))
  ensemble_y_hat_lasso <- predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble)))
  y_hat_ensemble_lasso <- c(ensemble_lasso_test_predict_value, ensemble_lasso_validation_predict_value)
  ensemble_lasso_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_lasso_test_predict_value, ensemble_lasso_validation_predict_value))
  ensemble_lasso_bias_mean <- mean(ensemble_lasso_bias)
  ensemble_lasso_bias_sd <- sd(ensemble_lasso_bias)
  ensemble_lasso_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_lasso_test_predict_value, ensemble_lasso_validation_predict_value))
  ensemble_lasso_MAE_mean <- mean(ensemble_lasso_MAE)
  ensemble_lasso_MAE_sd <- sd(ensemble_lasso_MAE)
  ensemble_lasso_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_lasso_test_predict_value, ensemble_lasso_validation_predict_value))
  ensemble_lasso_MSE_mean <- mean(ensemble_lasso_MSE)
  ensemble_lasso_MSE_sd <- sd(ensemble_lasso_MSE)
  ensemble_lasso_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_lasso_test_predict_value, ensemble_lasso_validation_predict_value))
  ensemble_lasso_SSE_mean <- mean(ensemble_lasso_SSE)
  ensemble_lasso_SSE_sd <- sd(ensemble_lasso_SSE)
  ensemble_lasso_ks_p_value[i] <- stats::ks.test(x = y_hat_ensemble_lasso, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_lasso_ks_p_value_mean <- mean(ensemble_lasso_ks_p_value)
  ensemble_lasso_ks_p_value_sd <- sd(ensemble_lasso_ks_p_value)
  ensemble_lasso_ks_stat[i] <- stats::ks.test(x = y_hat_ensemble_lasso, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_lasso_ks_stat_mean <- mean(ensemble_lasso_ks_stat)
  ensemble_lasso_ks_test <- c(ensemble_lasso_ks_stat_mean, ensemble_lasso_ks_p_value_mean)

  ensemble_lasso_end <- Sys.time()
  ensemble_lasso_duration[i] <- ensemble_lasso_end - ensemble_lasso_start
  ensemble_lasso_duration_mean <- mean(ensemble_lasso_duration)
  ensemble_lasso_duration_sd <- sd(ensemble_lasso_duration)


  #### Model 31: Ensembles Using Linear tuned ####
  ensemble_linear_start <- Sys.time()
  ensemble_linear_train_fit <- e1071::tune.rpart(formula = y_ensemble ~ ., data = ensemble_train)
  ensemble_linear_train_RMSE[i] <- Metrics::rmse(
    actual = ensemble_train$y_ensemble,
    predicted = predict(
      object = ensemble_linear_train_fit$best.model,
      newdata = ensemble_train
    )
  )
  ensemble_linear_train_RMSE_mean <- mean(ensemble_linear_train_RMSE)
  ensemble_linear_test_RMSE[i] <- Metrics::rmse(
    actual = ensemble_test$y_ensemble,
    predicted = predict(
      object = ensemble_linear_train_fit$best.model,
      newdata = ensemble_test
    )
  )
  ensemble_linear_test_RMSE_mean <- mean(ensemble_linear_test_RMSE)
  ensemble_linear_validation_RMSE[i] <- Metrics::rmse(
    actual = ensemble_validation$y_ensemble,
    predicted = predict(
      object = ensemble_linear_train_fit$best.model,
      newdata = ensemble_validation
    )
  )
  ensemble_linear_validation_RMSE_mean <- mean(ensemble_linear_validation_RMSE)
  ensemble_linear_holdout_RMSE[i] <- mean(c(ensemble_linear_test_RMSE_mean, ensemble_linear_validation_RMSE_mean))
  ensemble_linear_holdout_RMSE_mean <- mean(ensemble_linear_holdout_RMSE)
  ensemble_linear_holdout_RMSE_sd_mean <- sd(c(ensemble_linear_test_RMSE_mean, ensemble_linear_validation_RMSE_mean))
  ensemble_linear_train_predict_value <- as.numeric(predict(object = ensemble_linear_train_fit$best.model, newdata = ensemble_train))
  ensemble_linear_test_predict_value <- as.numeric(predict(object = ensemble_linear_train_fit$best.model, newdata = ensemble_test))
  ensemble_linear_validation_predict_value <- as.numeric(predict(object = ensemble_linear_train_fit$best.model, newdata = ensemble_validation))
  ensemble_linear_predict_value_mean <- mean(c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value))
  ensemble_linear_sd[i] <- sd(c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value))
  ensemble_linear_sd_mean <- mean(ensemble_linear_sd)
  ensemble_linear_holdout_vs_train[i] <- ensemble_linear_holdout_RMSE_mean / ensemble_linear_train_RMSE_mean
  ensemble_linear_holdout_vs_train_mean <- mean(ensemble_linear_holdout_vs_train)
  ensemble_linear_holdout_vs_train_range <- range(ensemble_linear_holdout_vs_train)
  ensemble_linear_holdout_vs_train_sd <- sd(ensemble_linear_holdout_vs_train)
  ensemble_y_hat_linear <- c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value)
  ensemble_linear_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value))
  ensemble_linear_bias_mean <- mean(ensemble_linear_bias)
  ensemble_linear_bias_sd <- sd(ensemble_linear_bias)
  ensemble_linear_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value))
  ensemble_linear_MAE_mean <- mean(ensemble_linear_MAE)
  ensemble_linear_MAE_sd <- sd(ensemble_linear_MAE)
  ensemble_linear_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value))
  ensemble_linear_MSE_mean <- mean(ensemble_linear_MSE)
  ensemble_linear_MSE_sd <- sd(ensemble_linear_MSE)
  ensemble_linear_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value))
  ensemble_linear_SSE_mean <- mean(ensemble_linear_SSE)
  ensemble_linear_SSE_sd <- sd(ensemble_linear_SSE)
  ensemble_linear_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_linear, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_linear_ks_p_value_mean <- mean(ensemble_linear_ks_p_value)
  ensemble_linear_ks_p_value_sd <- sd(ensemble_linear_ks_p_value)
  ensemble_linear_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_linear, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_linear_ks_stat_mean <- mean(ensemble_linear_ks_stat)
  ensemble_linear_ks_test <- c(ensemble_linear_ks_stat_mean, ensemble_linear_ks_p_value_mean)

  ensemble_linear_end <- Sys.time()
  ensemble_linear_duration[i] <- ensemble_linear_end - ensemble_linear_start
  ensemble_linear_duration_mean <- mean(ensemble_linear_duration)
  ensemble_linear_duration_sd <- sd(ensemble_linear_duration)


  #### Model 34: Ensembles Using Random Forest tuned ####
  ensemble_rf_start <- Sys.time()
  ensemble_rf_train_fit <- e1071::tune.randomForest(x = ensemble_train, y = ensemble_train$y_ensemble, data = ensemble_train)
  ensemble_rf_train_RMSE[i] <- Metrics::rmse(
    actual = ensemble_train$y_ensemble,
    predicted = predict(
      object = ensemble_rf_train_fit$best.model,
      newdata = ensemble_train
    )
  )
  ensemble_rf_train_RMSE_mean <- mean(ensemble_rf_train_RMSE)
  ensemble_rf_test_RMSE[i] <- Metrics::rmse(
    actual = ensemble_test$y_ensemble,
    predicted = predict(
      object = ensemble_rf_train_fit$best.model,
      newdata = ensemble_test
    )
  )
  ensemble_rf_test_RMSE_mean <- mean(ensemble_rf_test_RMSE)
  ensemble_rf_validation_RMSE[i] <- Metrics::rmse(
    actual = ensemble_validation$y_ensemble,
    predicted = predict(
      object = ensemble_rf_train_fit$best.model,
      newdata = ensemble_validation
    )
  )
  ensemble_rf_validation_RMSE_mean <- mean(ensemble_rf_validation_RMSE)
  ensemble_rf_holdout_RMSE[i] <- mean(c(ensemble_rf_test_RMSE_mean, ensemble_rf_validation_RMSE_mean))
  ensemble_rf_holdout_RMSE_mean <- mean(ensemble_rf_holdout_RMSE)
  ensemble_rf_holdout_RMSE_sd_mean <- sd(c(ensemble_rf_test_RMSE_mean, ensemble_rf_validation_RMSE_mean))
  ensemble_rf_train_predict_value <- predict(object = ensemble_rf_train_fit$best.model, newdata = ensemble_train)
  ensemble_rf_test_predict_value <- predict(object = ensemble_rf_train_fit$best.model, newdata = ensemble_test)
  ensemble_rf_validation_predict_value <- predict(object = ensemble_rf_train_fit$best.model, newdata = ensemble_validation)
  ensemble_rf_predict_value_mean <- mean(c(ensemble_rf_test_predict_value, ensemble_rf_validation_predict_value))
  ensemble_rf_sd_mean <- sd(c(ensemble_rf_test_predict_value, ensemble_rf_validation_predict_value))
  ensemble_rf_holdout_vs_train[i] <- ensemble_rf_holdout_RMSE_mean / ensemble_rf_train_RMSE_mean
  ensemble_rf_holdout_vs_train_mean <- mean(ensemble_rf_holdout_vs_train)
  ensemble_rf_holdout_vs_train_range <- range(ensemble_rf_holdout_vs_train)
  ensemble_rf_holdout_vs_train_sd <- sd(ensemble_rf_holdout_vs_train)
  ensemble_y_hat_rf <- c(ensemble_rf_test_predict_value, ensemble_rf_validation_predict_value)
  ensemble_rf_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_rf_test_predict_value, ensemble_rf_validation_predict_value))
  ensemble_rf_bias_mean <- mean(ensemble_rf_bias)
  ensemble_rf_bias_sd <- sd(ensemble_rf_bias)
  ensemble_rf_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_rf_test_predict_value, ensemble_rf_validation_predict_value))
  ensemble_rf_MAE_mean <- mean(ensemble_rf_MAE)
  ensemble_rf_MAE_sd <- sd(ensemble_rf_MAE)
  ensemble_rf_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_rf_test_predict_value, ensemble_rf_validation_predict_value))
  ensemble_rf_MSE_mean <- mean(ensemble_rf_MSE)
  ensemble_rf_MSE_sd <- sd(ensemble_rf_MSE)
  ensemble_rf_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_rf_test_predict_value, ensemble_rf_validation_predict_value))
  ensemble_rf_SSE_mean <- mean(ensemble_rf_SSE)
  ensemble_rf_SSE_sd <- sd(ensemble_rf_SSE)
  ensemble_rf_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_rf, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_rf_ks_p_value_mean <- mean(ensemble_rf_ks_p_value)
  ensemble_rf_ks_p_value_sd <- sd(ensemble_rf_ks_p_value)
  ensemble_rf_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_rf, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_rf_ks_stat_mean <- mean(ensemble_rf_ks_stat)
  ensemble_rf_ks_test <- c(ensemble_rf_ks_stat_mean, ensemble_rf_ks_p_value_mean)

  ensemble_rf_end <- Sys.time()
  ensemble_rf_duration[i] <- ensemble_rf_end - ensemble_rf_start
  ensemble_rf_duration_mean <- mean(ensemble_rf_duration)
  ensemble_rf_duration_sd <- sd(ensemble_rf_duration)


  #### Ensembles Using Ridge ####
  ensemble_ridge_start <- Sys.time()

  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_ridge_model <- glmnet(ensemble_x, ensemble_y, alpha = 0)
  ensemble_ridge_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 0)
  ensemble_best_ridge_lambda <- ensemble_ridge_cv$lambda.min
  ensemble_best_ridge_model <- glmnet(ensemble_x, ensemble_y, alpha = 0, lambda = ensemble_best_ridge_lambda)
  ensemble_ridge_train_RMSE <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))))
  ensemble_ridge_train_RMSE_df <- rbind(ensemble_ridge_train_RMSE_df, ensemble_ridge_train_RMSE)
  ensemble_ridge_train_RMSE_mean <- mean(ensemble_ridge_train_RMSE_df$ensemble_ridge_train_RMSE[2:nrow(ensemble_ridge_train_RMSE_df)])
  ensemble_ridge_test_RMSE <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_ridge_test_RMSE_df <- rbind(ensemble_ridge_test_RMSE_df, ensemble_ridge_test_RMSE)
  ensemble_ridge_test_RMSE_mean <- mean(ensemble_ridge_test_RMSE_df$ensemble_ridge_test_RMSE[2:nrow(ensemble_ridge_test_RMSE_df)])
  ensemble_ridge_test_predict_value <- rowMeans(predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_ridge_test_predict_value_mean <- mean(ensemble_ridge_test_predict_value)
  ensemble_ridge_test_predict_value_sd <- sd(ensemble_ridge_test_predict_value)
  ensemble_ridge_test_predict_value_sd_mean <- mean(ensemble_ridge_test_predict_value_sd, na.rm = TRUE)
  ## ridge using the validation data set
  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_ridge_model <- glmnet(ensemble_x, ensemble_y, alpha = 0)
  ensemble_ridge_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 0)
  ensemble_best_ridge_lambda <- ensemble_ridge_cv$lambda.min
  ensemble_best_ridge_model <- glmnet(ensemble_x, ensemble_y, alpha = 0, lambda = ensemble_best_ridge_lambda)
  ensemble_ridge_validation_pred <- predict(ensemble_best_ridge_model, s = ensemble_best_ridge_lambda, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble)))
  ensemble_ridge_validation_RMSE <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_ridge_validation_RMSE_df <- rbind(ensemble_ridge_validation_RMSE_df, ensemble_ridge_validation_RMSE)
  ensemble_ridge_validation_RMSE_mean <- mean(ensemble_ridge_validation_RMSE_df$ensemble_ridge_validation_RMSE[2:nrow(ensemble_ridge_validation_RMSE_df)])
  ensemble_ridge_validation_predict_value <- rowMeans(predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_ridge_validation_predict_value_mean <- mean(ensemble_ridge_validation_predict_value)
  ensemble_ridge_validation_predict_value_sd <- round(sd(ensemble_ridge_validation_pred), 4)
  ensemble_ridge_validation_predict_value_sd_df <- rbind(ensemble_ridge_validation_predict_value_sd_df, ensemble_ridge_validation_predict_value_sd)
  ensemble_ridge_validation_predict_value_sd_mean <- mean(ensemble_ridge_validation_predict_value_sd_df$ensemble_ridge_validation_predict_value_sd[2:nrow(ensemble_ridge_validation_predict_value_sd_df)])
  ensemble_ridge_holdout_RMSE[i] <- mean(c(ensemble_ridge_test_RMSE, ensemble_ridge_validation_RMSE))
  ensemble_ridge_holdout_RMSE_mean <- mean(ensemble_ridge_holdout_RMSE)
  ensemble_ridge_holdout_RMSE_sd_mean <- mean(sd(c(ensemble_ridge_test_RMSE, ensemble_ridge_validation_RMSE)))
  ensemble_ridge_test_sd <- sd(ensemble_ridge_test_predict_value, na.rm = TRUE)
  ensemble_ridge_test_sd_df <- rbind(ensemble_ridge_test_sd_df, ensemble_ridge_test_sd)
  ensemble_ridge_test_sd_mean <- mean(ensemble_ridge_test_sd_df$ensemble_ridge_test_sd[2:nrow(ensemble_ridge_test_sd_df)])
  ensemble_ridge_validation_sd <- sd(ensemble_ridge_validation_predict_value, na.rm = TRUE)
  ensemble_ridge_validation_sd_df <- rbind(ensemble_ridge_validation_sd_df, ensemble_ridge_validation_sd)
  ensemble_ridge_validation_sd_mean <- mean(ensemble_ridge_validation_sd_df$ensemble_ridge_validation_sd[2:nrow(ensemble_ridge_validation_sd_df)])
  ensemble_ridge_holdout_vs_train <- ensemble_ridge_holdout_RMSE_mean / ensemble_ridge_train_RMSE_mean
  ensemble_ridge_holdout_vs_train_df <- rbind(ensemble_ridge_holdout_vs_train_df, ensemble_ridge_holdout_vs_train)
  ensemble_ridge_holdout_vs_train_mean <- mean(ensemble_ridge_holdout_vs_train_df$ensemble_ridge_holdout_vs_train[2:nrow(ensemble_ridge_holdout_vs_train_df)])
  ensemble_ridge_holdout_vs_train_range <- range(ensemble_ridge_holdout_vs_train_df$ensemble_ridge_holdout_vs_train[2:nrow(ensemble_ridge_holdout_vs_train_df)])
  ensemble_ridge_holdout_vs_train_sd <- sd(ensemble_ridge_holdout_vs_train_df$ensemble_ridge_holdout_vs_train)
  ensemble_ridge_predict_value_mean <- mean(c(ensemble_ridge_test_predict_value_mean, ensemble_ridge_validation_predict_value_mean))
  ensemble_ridge_sd[i] <- mean(ensemble_ridge_test_sd_mean, ensemble_ridge_validation_sd_mean)
  ensemble_ridge_sd_mean <- mean(ensemble_ridge_sd)
  ensemble_y_hat_ridge <- predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble)))
  y_hat_ensemble_ridge <- c(ensemble_ridge_test_predict_value, ensemble_ridge_validation_predict_value)
  ensemble_ridge_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_ridge_test_predict_value, ensemble_ridge_validation_predict_value))
  ensemble_ridge_bias_mean <- mean(ensemble_ridge_bias)
  ensemble_ridge_bias_sd <- sd(ensemble_ridge_bias)
  ensemble_ridge_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_ridge_test_predict_value, ensemble_ridge_validation_predict_value))
  ensemble_ridge_MAE_mean <- mean(ensemble_ridge_MAE)
  ensemble_ridge_MAE_sd <- sd(ensemble_ridge_MAE)
  ensemble_ridge_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_ridge_test_predict_value, ensemble_ridge_validation_predict_value))
  ensemble_ridge_MSE_mean <- mean(ensemble_ridge_MSE)
  ensemble_ridge_MSE_sd <- sd(ensemble_ridge_MSE)
  ensemble_ridge_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_ridge_test_predict_value, ensemble_ridge_validation_predict_value))
  ensemble_ridge_SSE_mean <- mean(ensemble_ridge_SSE)
  ensemble_ridge_SSE_sd <- sd(ensemble_ridge_SSE)
  ensemble_ridge_ks_p_value[i] <- stats::ks.test(x = y_hat_ensemble_ridge, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_ridge_ks_p_value_mean <- mean(ensemble_ridge_ks_p_value)
  ensemble_ridge_ks_p_value_sd <- sd(ensemble_ridge_ks_p_value)
  ensemble_ridge_ks_stat[i] <- stats::ks.test(x = y_hat_ensemble_ridge, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_ridge_ks_stat_mean <- mean(ensemble_ridge_ks_stat)
  ensemble_ridge_ks_test <- c(ensemble_ridge_ks_stat_mean, ensemble_ridge_ks_p_value_mean)

  ensemble_ridge_end <- Sys.time()
  ensemble_ridge_duration[i] <- ensemble_ridge_end - ensemble_ridge_start
  ensemble_ridge_duration_mean <- mean(ensemble_ridge_duration)
  ensemble_ridge_duration_sd <- sd(ensemble_ridge_duration)


  #### Model #35: Ensembles Using Rpart ####
  ensemble_rpart_start <- Sys.time()
  ensemble_rpart_train_fit <- rpart::rpart(ensemble_train$y_ensemble ~ ., data = ensemble_train)
  ensemble_rpart_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_rpart_train_fit,
    newdata = ensemble_train
  ))
  ensemble_rpart_train_RMSE_mean <- mean(ensemble_rpart_train_RMSE)
  ensemble_rpart_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_rpart_train_fit,
    newdata = ensemble_test
  ))
  ensemble_rpart_test_RMSE_mean <- mean(ensemble_rpart_test_RMSE)
  ensemble_rpart_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_rpart_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_rpart_validation_RMSE_mean <- mean(ensemble_rpart_validation_RMSE)
  ensemble_rpart_holdout_RMSE[i] <- mean(c(ensemble_rpart_test_RMSE_mean, ensemble_rpart_validation_RMSE_mean))
  ensemble_rpart_holdout_RMSE_mean <- mean(ensemble_rpart_holdout_RMSE)
  ensemble_rpart_holdout_RMSE_sd_mean <- sd(c(ensemble_rpart_test_RMSE_mean, ensemble_rpart_validation_RMSE_mean))
  ensemble_rpart_train_predict_value <- as.numeric(predict(
    object = rpart::rpart(y_ensemble ~ ., data = ensemble_train),
    newdata = ensemble_train
  ))
  ensemble_rpart_test_predict_value <- as.numeric(predict(
    object = rpart::rpart(y_ensemble ~ ., data = ensemble_train),
    newdata = ensemble_test
  ))
  ensemble_rpart_validation_predict_value <- as.numeric(predict(
    object = rpart::rpart(y_ensemble ~ ., data = ensemble_train),
    newdata = ensemble_validation
  ))
  ensemble_rpart_predict_value_mean <- mean(c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value))
  ensemble_rpart_sd[i] <- sd(c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value))
  ensemble_rpart_sd_mean <- mean(ensemble_rpart_sd)
  ensemble_rpart_holdout_vs_train[i] <- ensemble_rpart_holdout_RMSE_mean / ensemble_rpart_train_RMSE_mean
  ensemble_rpart_holdout_vs_train_mean <- mean(ensemble_rpart_holdout_vs_train)
  ensemble_rpart_holdout_vs_train_range <- range(ensemble_rpart_holdout_vs_train)
  ensemble_rpart_holdout_vs_train_sd <- sd(ensemble_rpart_holdout_vs_train)
  ensemble_y_hat_rpart <- c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value)
  ensemble_rpart_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value))
  ensemble_rpart_bias_mean <- mean(ensemble_rpart_bias)
  ensemble_rpart_bias_sd <- sd(ensemble_rpart_bias)
  ensemble_rpart_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value))
  ensemble_rpart_MAE_mean <- mean(ensemble_rpart_MAE)
  ensemble_rpart_MAE_sd <- sd(ensemble_rpart_MAE)
  ensemble_rpart_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value))
  ensemble_rpart_MSE_mean <- mean(ensemble_rpart_MSE)
  ensemble_rpart_MSE_sd <- sd(ensemble_rpart_MSE)
  ensemble_rpart_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value))
  ensemble_rpart_SSE_mean <- mean(ensemble_rpart_SSE)
  ensemble_rpart_SSE_sd <- sd(ensemble_rpart_SSE)
  ensemble_rpart_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_rpart, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_rpart_ks_p_value_mean <- mean(ensemble_rpart_ks_p_value)
  ensemble_rpart_ks_p_value_sd <- sd(ensemble_rpart_ks_p_value)
  ensemble_rpart_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_rpart, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_rpart_ks_stat_mean <- mean(ensemble_rpart_ks_stat)
  ensemble_rpart_ks_test <- c(ensemble_rpart_ks_stat_mean, ensemble_rpart_ks_p_value_mean)

  ensemble_rpart_end <- Sys.time()
  ensemble_rpart_duration[i] <- ensemble_rpart_end - ensemble_rpart_start
  ensemble_rpart_duration_mean <- mean(ensemble_rpart_duration)
  ensemble_rpart_duration_sd <- sd(ensemble_rpart_duration)

  #### Ensemble using Support Vector Machines ####
  ensemble_svm_start <- Sys.time()
  ensemble_svm_train_fit <- e1071::tune.svm(x = ensemble_train, y = ensemble_train$y_ensemble, data = ensemble_train)
  ensemble_svm_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_train))
  ensemble_svm_train_RMSE_mean <- mean(ensemble_svm_train_RMSE)
  ensemble_svm_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_test))
  ensemble_svm_test_RMSE_mean <- mean(ensemble_svm_test_RMSE)
  ensemble_svm_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_validation))
  ensemble_svm_validation_RMSE_mean <- mean(ensemble_svm_validation_RMSE)
  ensemble_svm_holdout_RMSE[i] <- mean(c(ensemble_svm_test_RMSE_mean, ensemble_svm_validation_RMSE_mean))
  ensemble_svm_holdout_RMSE_mean <- mean(ensemble_svm_holdout_RMSE)
  ensemble_svm_holdout_RMSE_sd_mean <- sd(c(ensemble_svm_test_RMSE_mean, ensemble_svm_validation_RMSE_mean))
  ensemble_svm_train_predict_value <- as.numeric(predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_train))
  ensemble_svm_test_predict_value <- as.numeric(predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_test))
  ensemble_svm_validation_predict_value <- as.numeric(predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_validation))
  ensemble_svm_predict_value_mean <- mean(c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value))
  ensemble_svm_sd[i] <- sd(c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value))
  ensemble_svm_sd_mean <- mean(ensemble_svm_sd)
  ensemble_svm_holdout_vs_train[i] <- ensemble_svm_holdout_RMSE_mean / ensemble_svm_train_RMSE_mean
  ensemble_svm_holdout_vs_train_mean <- mean(ensemble_svm_holdout_vs_train)
  ensemble_svm_holdout_vs_train_range <- range(ensemble_svm_holdout_vs_train)
  ensemble_svm_holdout_vs_train_sd <- sd(ensemble_svm_holdout_vs_train)

  ensemble_y_hat_svm <- c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value)
  ensemble_svm_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value))
  ensemble_svm_bias_mean <- mean(ensemble_svm_bias)
  ensemble_svm_bias_sd <- sd(ensemble_svm_bias)
  ensemble_svm_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value))
  ensemble_svm_MAE_mean <- mean(ensemble_svm_MAE)
  ensemble_svm_MAE_sd <- sd(ensemble_svm_MAE)
  ensemble_svm_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value))
  ensemble_svm_MSE_mean <- mean(ensemble_svm_MSE)
  ensemble_svm_MSE_sd <- sd(ensemble_svm_MSE)
  ensemble_svm_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value))
  ensemble_svm_SSE_mean <- mean(ensemble_svm_SSE)
  ensemble_svm_SSE_sd <- sd(ensemble_svm_SSE)
  ensemble_svm_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_svm , y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_svm_ks_p_value_mean <- mean(ensemble_svm_ks_p_value)
  ensemble_svm_ks_p_value_sd <- sd(ensemble_svm_ks_p_value)
  ensemble_svm_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_svm , y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_svm_ks_stat_mean <- mean(ensemble_svm_ks_stat)
  ensemble_svm_ks_test <- c(ensemble_svm_ks_stat_mean, ensemble_svm_ks_p_value_mean)

  ensemble_svm_end <- Sys.time()
  ensemble_svm_duration[i] <- ensemble_svm_end - ensemble_svm_start
  ensemble_svm_duration_mean <- mean(ensemble_svm_duration)
  ensemble_svm_duration_sd <- sd(ensemble_svm_duration)

  #### Model 37: Ensemble Using Trees ####
  ensemble_tree_start <- Sys.time()
  ensemble_tree_train_fit <- tree::tree(ensemble_train$y_ensemble ~ ., data = ensemble_train)
  ensemble_tree_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_tree_train_fit,
    newdata = ensemble_train
  ))
  ensemble_tree_train_RMSE_mean <- mean(ensemble_tree_train_RMSE)
  ensemble_tree_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_tree_train_fit,
    newdata = ensemble_test
  ))
  ensemble_tree_test_RMSE_mean <- mean(ensemble_tree_test_RMSE)
  ensemble_tree_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_tree_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_tree_validation_RMSE_mean <- mean(ensemble_tree_validation_RMSE)
  ensemble_tree_holdout_RMSE[i] <- mean(c(ensemble_tree_test_RMSE_mean, ensemble_tree_validation_RMSE_mean))
  ensemble_tree_holdout_RMSE_mean <- mean(ensemble_tree_holdout_RMSE)
  ensemble_tree_holdout_RMSE_sd_mean <- sd(c(ensemble_tree_test_RMSE_mean, ensemble_tree_validation_RMSE_mean))
  ensemble_tree_train_predict_value <- predict(object = ensemble_tree_train_fit, newdata = ensemble_train)
  ensemble_tree_test_predict_value <- predict(object = ensemble_tree_train_fit, newdata = ensemble_test)
  ensemble_tree_validation_predict_value <- predict(object = ensemble_tree_train_fit, newdata = ensemble_validation)
  ensemble_tree_predict_value_mean <- mean(c(ensemble_tree_test_predict_value, ensemble_tree_validation_predict_value))
  ensemble_tree_sd_mean <- sd(ensemble_tree_test_predict_value)
  ensemble_tree_holdout_vs_train[i] <- ensemble_tree_holdout_RMSE_mean / ensemble_tree_train_RMSE_mean
  ensemble_tree_holdout_vs_train_mean <- mean(ensemble_tree_holdout_vs_train)
  ensemble_tree_holdout_vs_train_range <- range(ensemble_tree_holdout_vs_train)
  ensemble_tree_holdout_vs_train_sd <- sd(ensemble_tree_holdout_vs_train)
  ensemble_y_hat_tree <- c(ensemble_tree_test_predict_value, ensemble_tree_validation_predict_value)
  ensemble_tree_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_tree_test_predict_value, ensemble_tree_validation_predict_value))
  ensemble_tree_bias_mean <- mean(ensemble_tree_bias)
  ensemble_tree_bias_sd <- sd(ensemble_tree_bias)
  ensemble_tree_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_tree_test_predict_value, ensemble_tree_validation_predict_value))
  ensemble_tree_MAE_mean <- mean(ensemble_tree_MAE)
  ensemble_tree_MAE_sd <- sd(ensemble_tree_MAE)
  ensemble_tree_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_tree_test_predict_value, ensemble_tree_validation_predict_value))
  ensemble_tree_MSE_mean <- mean(ensemble_tree_MSE)
  ensemble_tree_MSE_sd <- sd(ensemble_tree_MSE)
  ensemble_tree_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_tree_test_predict_value, ensemble_tree_validation_predict_value))
  ensemble_tree_SSE_mean <- mean(ensemble_tree_SSE)
  ensemble_tree_SSE_sd <- sd(ensemble_tree_SSE)
  ensemble_tree_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_tree, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_tree_ks_p_value_mean <- mean(ensemble_tree_ks_p_value)
  ensemble_tree_ks_p_value_sd <- sd(ensemble_tree_ks_p_value)
  ensemble_tree_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_tree, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_tree_ks_stat_mean <- mean(ensemble_tree_ks_stat)
  ensemble_tree_ks_test <- c(ensemble_tree_ks_stat_mean, ensemble_tree_ks_p_value_mean)

  ensemble_tree_end <- Sys.time()
  ensemble_tree_duration[i] <- ensemble_tree_end - ensemble_tree_start
  ensemble_tree_duration_mean <- mean(ensemble_tree_duration)
  ensemble_tree_duration_sd <- sd(ensemble_tree_duration)

  #### Ensemble Using XGBoost ####
  ensemble_xgb_start <- Sys.time()

  ensemble_train_x <- data.matrix(ensemble_train[, -ncol(ensemble_train)])
  ensemble_train_y <- ensemble_train[, ncol(ensemble_train)]

  # define predictor and response variables in test set
  ensemble_test_x <- data.matrix(ensemble_test[, -ncol(ensemble_test)])
  ensemble_test_y <- ensemble_test[, ncol(ensemble_test)]

  # define predictor and response variables in validation set
  ensemble_validation_x <- data.matrix(ensemble_validation[, -ncol(ensemble_validation)])
  ensemble_validation_y <- ensemble_validation[, ncol(ensemble_validation)]

  # define final train, test and validationing sets
  ensemble_xgb_train <- xgboost::xgb.DMatrix(data = ensemble_train_x, label = ensemble_train_y)
  ensemble_xgb_test <- xgboost::xgb.DMatrix(data = ensemble_test_x, label = ensemble_test_y)
  ensemble_xgb_validation <- xgboost::xgb.DMatrix(data = ensemble_validation_x, label = ensemble_validation_y)

  # define watchlist
  ensemble_watchlist <- list(train = ensemble_xgb_train, validation = ensemble_xgb_validation)
  ensemble_watchlist_test <- list(train = ensemble_xgb_train, test = ensemble_xgb_test)
  ensemble_watchlist_validation <- list(train = ensemble_xgb_train, validation = ensemble_xgb_validation)

  # fit XGBoost model and display training and validation data at each round

  ensemble_xgb_model <- xgboost::xgb.train(data = ensemble_xgb_train, max.depth = 3, watchlist = ensemble_watchlist_test, nrounds = 70)
  ensemble_xgb_model_validation <- xgboost::xgb.train(data = ensemble_xgb_train, max.depth = 3, watchlist = ensemble_watchlist_validation, nrounds = 70)

  ensemble_xgboost.min <- which.min(ensemble_xgb_model$evaluation_log$validation_rmse)
  ensemble_xgboost_validation.min <- which.min(ensemble_xgb_model$evaluation_log$validation_rmse)

  ensemble_xgb_train_RMSE[i] <- round(Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_xgb_model, newdata = ensemble_train_x)), 4)
  ensemble_xgb_train_RMSE_mean <- mean(ensemble_xgb_train_RMSE)

  ensemble_xgb_test_RMSE[i] <- round(Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_xgb_model, newdata = ensemble_test_x)), 4)
  ensemble_xgb_test_RMSE_mean <- mean(ensemble_xgb_test_RMSE)

  ensemble_xgb_validation_RMSE[i] <- round(Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_xgb_model, newdata = ensemble_validation_x)), 4)
  ensemble_xgb_validation_RMSE_mean <- mean(ensemble_xgb_validation_RMSE)

  ensemble_xgb_holdout_RMSE[i] <- mean(c(ensemble_xgb_test_RMSE, ensemble_xgb_validation_RMSE))
  ensemble_xgb_holdout_RMSE_mean <- mean(ensemble_xgb_holdout_RMSE)
  ensemble_xgb_holdout_RMSE_sd_mean <- mean(sd(c(ensemble_xgb_test_RMSE, ensemble_xgb_validation_RMSE)))

  ensemble_y_hat_xgb <- c(predict(object = ensemble_xgb_model, newdata = ensemble_test_x), predict(object = ensemble_xgb_model, newdata = ensemble_validation_x))
  ensemble_xgb_predict_value_mean <- round(mean(ensemble_y_hat_xgb), 4)
  ensemble_xgb_sd_mean <- round(sd(ensemble_y_hat_xgb), 4)

  ensemble_xgb_holdout_vs_train <- ensemble_xgb_holdout_RMSE_mean / ensemble_xgb_train_RMSE_mean
  ensemble_xgb_holdout_vs_train_df <- rbind(ensemble_xgb_holdout_vs_train_df, ensemble_xgb_holdout_vs_train)
  ensemble_xgb_holdout_vs_train_mean <- mean(ensemble_xgb_holdout_vs_train_df$ensemble_xgb_holdout_vs_train[2:nrow(ensemble_xgb_holdout_vs_train_df)])
  ensemble_xgb_holdout_vs_train_range <- range(ensemble_xgb_holdout_vs_train_df$ensemble_xgb_holdout_vs_train[2:nrow(ensemble_xgb_holdout_vs_train_df)])
  ensemble_xgb_holdout_vs_train_sd <- sd(ensemble_xgb_holdout_vs_train_df$ensemble_xgb_holdout_vs_train)

  ensemble_xgb_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = ensemble_y_hat_xgb)
  ensemble_xgb_bias_mean <- mean(ensemble_xgb_bias)
  ensemble_xgb_bias_sd <- sd(ensemble_xgb_bias)
  ensemble_xgb_MAE[i] <- Metrics::mae(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = ensemble_y_hat_xgb)
  ensemble_xgb_MAE_mean <- mean(ensemble_xgb_MAE)
  ensemble_xgb_MAE_sd <- sd(ensemble_xgb_MAE)
  ensemble_xgb_MSE[i] <- Metrics::mse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = ensemble_y_hat_xgb)
  ensemble_xgb_MSE_mean <- mean(ensemble_xgb_MSE)
  ensemble_xgb_MSE_sd <- sd(ensemble_xgb_MSE)
  ensemble_xgb_SSE[i] <- Metrics::sse(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = ensemble_y_hat_xgb)
  ensemble_xgb_SSE_mean <- mean(ensemble_xgb_SSE)
  ensemble_xgb_SSE_sd <- sd(ensemble_xgb_SSE)
  ensemble_xgb_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_xgb, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_xgb_ks_p_value_mean <- mean(ensemble_xgb_ks_p_value)
  ensemble_xgb_ks_p_value_sd <- sd(ensemble_xgb_ks_p_value)
  ensemble_xgb_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_xgb, y = c(ensemble_train$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_xgb_ks_stat_mean <- mean(ensemble_xgb_ks_stat)
  ensemble_xgb_ks_test <- c(ensemble_xgb_ks_stat_mean, ensemble_xgb_ks_p_value_mean)

  ensemble_xgb_end <- Sys.time()
  ensemble_xgb_duration[i] <- ensemble_xgb_end - ensemble_xgb_start
  ensemble_xgb_duration_mean <- mean(ensemble_xgb_duration)
  ensemble_xgb_duration_sd <- sd(ensemble_xgb_duration)
}


##########################################

####### Start summary results here #######

##########################################

summary_results <- data.frame(
  "Model" = c(
    "Actual data", "Bagged Random Forest", "Bagging", "BayesGLM", "BayesRNN",
    "BoostRF", "Cubist", "Earth", "Elastic", "GAM", "Gradient Boosted", "KNN", "Lasso", "Linear", "Neuralnet", "PLS",
    "PCR", "RF", "Ridge", "Rpart", "SVM", "Tree", "XGBoost", "Ensemble Bagged Random Forest",
    "Ensemble Bagging", "Ensemble BayesGLM", "Ensemble BayesRNN", "Ensemble Boosted RF", "Ensemble Cubist",
    "Ensemble Earth", "Ensemble Elastic", "Ensemble Gradient Boosted", "Ensemble K-Nearest Neighbors",
    "Ensemble Lasso", "Ensemble Linear",  "Ensemble RF", "Ensemble Ridge", "Ensemble Rpart",
    "Ensemble SVM", "Ensemble Trees", "Ensemble XGBoost"
  ),
  "Mean_holdout_RMSE" = round(c(
    actual_RMSE, bag_rf_holdout_RMSE_mean, bagging_holdout_RMSE_mean, bayesglm_holdout_RMSE_mean,
    bayesrnn_holdout_RMSE_mean, boost_rf_holdout_RMSE_mean, cubist_holdout_RMSE_mean, earth_holdout_RMSE_mean, elastic_holdout_RMSE_mean,
    gam_holdout_RMSE_mean, gb_holdout_RMSE_mean, knn_holdout_RMSE_mean, lasso_holdout_RMSE_mean, linear_holdout_RMSE_mean,
    neuralnet_holdout_RMSE_mean, pls_holdout_RMSE_mean, pcr_holdout_RMSE_mean, rf_holdout_RMSE_mean,
    ridge_holdout_RMSE_mean, rpart_holdout_RMSE_mean, svm_holdout_RMSE_mean, tree_holdout_RMSE_mean, xgb_holdout_RMSE_mean,
    ensemble_bag_rf_holdout_RMSE_mean, ensemble_bagging_holdout_RMSE_mean, ensemble_bayesglm_holdout_RMSE_mean,
    ensemble_bayesrnn_holdout_RMSE_mean, ensemble_boost_rf_holdout_RMSE_mean, ensemble_cubist_holdout_RMSE_mean, ensemble_earth_holdout_RMSE_mean,
    ensemble_elastic_holdout_RMSE_mean, ensemble_gb_holdout_RMSE_mean,
    ensemble_knn_holdout_RMSE_mean, ensemble_lasso_holdout_RMSE_mean, ensemble_linear_holdout_RMSE_mean,
    ensemble_rf_holdout_RMSE_mean, ensemble_ridge_holdout_RMSE_mean, ensemble_rpart_holdout_RMSE_mean,
    ensemble_svm_holdout_RMSE_mean, ensemble_tree_holdout_RMSE_mean, ensemble_xgb_holdout_RMSE_mean
  ), 4),
  "Std_Deviation_of_holdout_RMSE" = round(c(
    actual_RMSE, bag_rf_holdout_RMSE_sd_mean, bagging_holdout_RMSE_sd_mean, bayesglm_holdout_RMSE_sd_mean,
    bayesrnn_holdout_RMSE_sd_mean, boost_rf_holdout_RMSE_sd_mean, cubist_holdout_RMSE_sd_mean, earth_holdout_RMSE_sd_mean, elastic_holdout_RMSE_sd_mean,
    gam_holdout_RMSE_sd_mean, gb_holdout_RMSE_sd_mean, knn_holdout_RMSE_sd_mean, lasso_holdout_RMSE_sd_mean, linear_holdout_RMSE_sd_mean,
    neuralnet_holdout_RMSE_sd_mean, pls_holdout_RMSE_sd_mean, pcr_holdout_RMSE_sd_mean, rf_holdout_RMSE_sd_mean,
    ridge_holdout_RMSE_sd_mean, rpart_holdout_RMSE_sd_mean, svm_holdout_RMSE_sd_mean, tree_holdout_RMSE_sd_mean, xgb_holdout_RMSE_sd_mean,
    ensemble_bag_rf_holdout_RMSE_sd_mean, ensemble_bagging_holdout_RMSE_sd_mean, ensemble_bayesglm_holdout_RMSE_sd_mean,
    ensemble_bayesrnn_holdout_RMSE_sd_mean, ensemble_boost_rf_holdout_RMSE_sd_mean, ensemble_cubist_holdout_RMSE_sd_mean, ensemble_earth_holdout_RMSE_sd_mean,
    ensemble_elastic_holdout_RMSE_sd_mean, ensemble_gb_holdout_RMSE_sd_mean,
    ensemble_knn_holdout_RMSE_sd_mean, ensemble_lasso_holdout_RMSE_sd_mean, ensemble_linear_holdout_RMSE_sd_mean,
    ensemble_rf_holdout_RMSE_sd_mean, ensemble_ridge_holdout_RMSE_sd_mean, ensemble_rpart_holdout_RMSE_sd_mean,
    ensemble_svm_holdout_RMSE_sd_mean, ensemble_tree_holdout_RMSE_sd_mean, ensemble_xgb_holdout_RMSE_sd_mean
  ), 4),
  "KS_Test_Stat_mean" = round(c(
    0, bag_rf_ks_stat_mean, bagging_ks_stat_mean, bayesglm_ks_stat_mean,
    bayesrnn_ks_stat_mean, boost_rf_ks_stat_mean, cubist_ks_stat_mean, earth_ks_stat_mean, elastic_ks_stat_mean,
    gam_ks_stat_mean, gb_ks_stat_mean, knn_ks_stat_mean, lasso_ks_stat_mean, linear_ks_stat_mean,
    neuralnet_ks_stat_mean, pls_ks_stat_mean, pcr_ks_stat_mean, rf_ks_stat_mean,
    ridge_ks_stat_mean, rpart_ks_stat_mean, svm_ks_stat_mean, tree_ks_stat_mean, xgb_ks_stat_mean,
    ensemble_bag_rf_ks_stat_mean, ensemble_bagging_ks_stat_mean,  ensemble_bayesglm_ks_stat_mean,
    ensemble_bayesrnn_ks_stat_mean, ensemble_boost_rf_ks_stat_mean, ensemble_cubist_ks_stat_mean, ensemble_earth_ks_stat_mean,
    ensemble_elastic_ks_stat_mean, ensemble_gb_ks_stat_mean,
    ensemble_knn_ks_stat_mean, ensemble_lasso_ks_stat_mean, ensemble_linear_ks_stat_mean,
    ensemble_rf_ks_stat_mean, ensemble_ridge_ks_stat_mean, ensemble_rpart_ks_stat_mean,
    ensemble_svm_ks_stat_mean, ensemble_tree_ks_stat_mean, ensemble_xgb_ks_stat_mean
  ), 4),
  "KS_Test_P_Value_mean" = round(c(
    0, bag_rf_ks_p_value_mean, bagging_ks_p_value_mean, bayesglm_ks_p_value_mean,
    bayesrnn_ks_p_value_mean, boost_rf_ks_p_value_mean, cubist_ks_p_value_mean, earth_ks_p_value_mean, elastic_ks_p_value_mean,
    gam_ks_p_value_mean, gb_ks_p_value_mean, knn_ks_p_value_mean, lasso_ks_p_value_mean, linear_ks_p_value_mean,
    neuralnet_ks_p_value_mean, pls_ks_p_value_mean, pcr_ks_p_value_mean, rf_ks_p_value_mean,
    ridge_ks_p_value_mean, rpart_ks_p_value_mean, svm_ks_p_value_mean, tree_ks_p_value_mean, xgb_ks_p_value_mean,
    ensemble_bag_rf_ks_p_value_mean, ensemble_bagging_ks_p_value_mean,  ensemble_bayesglm_ks_p_value_mean,
    ensemble_bayesrnn_ks_p_value_mean, ensemble_boost_rf_ks_p_value_mean, ensemble_cubist_ks_p_value_mean, ensemble_earth_ks_p_value_mean,
    ensemble_elastic_ks_p_value_mean, ensemble_gb_ks_p_value_mean,
    ensemble_knn_ks_p_value_mean, ensemble_lasso_ks_p_value_mean, ensemble_linear_ks_p_value_mean,
    ensemble_rf_ks_p_value_mean, ensemble_ridge_ks_p_value_mean, ensemble_rpart_ks_p_value_mean,
    ensemble_svm_ks_p_value_mean, ensemble_tree_ks_p_value_mean, ensemble_xgb_ks_p_value_mean
  ), 4),
  "KS_Test_P_Value_std_dev" = round(c(
    0, bag_rf_ks_p_value_sd, bagging_ks_p_value_sd, bayesglm_ks_p_value_sd,
    bayesrnn_ks_p_value_sd, boost_rf_ks_p_value_sd, cubist_ks_p_value_sd, earth_ks_p_value_sd, elastic_ks_p_value_sd,
    gam_ks_p_value_sd, gb_ks_p_value_sd, knn_ks_p_value_sd, lasso_ks_p_value_sd, linear_ks_p_value_sd,
    neuralnet_ks_p_value_sd, pls_ks_p_value_sd, pcr_ks_p_value_sd, rf_ks_p_value_sd,
    ridge_ks_p_value_sd, rpart_ks_p_value_sd, svm_ks_p_value_sd, tree_ks_p_value_sd, xgb_ks_p_value_sd,
    ensemble_bag_rf_ks_p_value_sd, ensemble_bagging_ks_p_value_sd,  ensemble_bayesglm_ks_p_value_sd,
    ensemble_bayesrnn_ks_p_value_sd, ensemble_boost_rf_ks_p_value_sd, ensemble_cubist_ks_p_value_sd, ensemble_earth_ks_p_value_sd,
    ensemble_elastic_ks_p_value_sd, ensemble_gb_ks_p_value_sd,
    ensemble_knn_ks_p_value_sd, ensemble_lasso_ks_p_value_sd, ensemble_linear_ks_p_value_sd,
    ensemble_rf_ks_p_value_sd, ensemble_ridge_ks_p_value_sd, ensemble_rpart_ks_p_value_sd,
    ensemble_svm_ks_p_value_sd, ensemble_tree_ks_p_value_sd, ensemble_xgb_ks_p_value_sd
  ), 4),
  "Bias" = round(c(
    0, bag_rf_bias_mean, bagging_bias_mean, bayesglm_bias_mean,
    bayesrnn_bias_mean, boost_rf_bias_mean, cubist_bias_mean, earth_bias_mean, elastic_bias_mean,
    gam_bias_mean, gb_bias_mean, knn_bias_mean, lasso_bias_mean, linear_bias_mean,
    neuralnet_bias_mean, pls_bias_mean, pcr_bias_mean, rf_bias_mean,
    ridge_bias_mean, rpart_bias_mean, svm_bias_mean, tree_bias_mean, xgb_bias_mean,
    ensemble_bag_rf_bias_mean, ensemble_bagging_bias_mean, ensemble_bayesglm_bias_mean,
    ensemble_bayesrnn_bias_mean, ensemble_boost_rf_bias_mean, ensemble_cubist_bias_mean, ensemble_earth_bias_mean,
    ensemble_elastic_bias_mean, ensemble_gb_bias_mean,
    ensemble_knn_bias_mean, ensemble_lasso_bias_mean, ensemble_linear_bias_mean,
    ensemble_rf_bias_mean, ensemble_ridge_bias_mean, ensemble_rpart_bias_mean,
    ensemble_svm_bias_mean, ensemble_tree_bias_mean, ensemble_xgb_bias_mean
  ), 4),
  "Bias_sd" = round(c(
    0, bag_rf_bias_sd, bagging_bias_sd, bayesglm_bias_sd,
    bayesrnn_bias_sd, boost_rf_bias_sd, cubist_bias_sd, earth_bias_sd, elastic_bias_sd,
    gam_bias_sd, gb_bias_sd, knn_bias_sd, lasso_bias_sd, linear_bias_sd,
    neuralnet_bias_sd, pls_bias_sd, pcr_bias_sd, rf_bias_sd,
    ridge_bias_sd, rpart_bias_sd, svm_bias_sd, tree_bias_sd, xgb_bias_sd,
    ensemble_bag_rf_bias_sd, ensemble_bagging_bias_sd, ensemble_bayesglm_bias_sd,
    ensemble_bayesrnn_bias_sd, ensemble_boost_rf_bias_sd, ensemble_cubist_bias_sd, ensemble_earth_bias_sd,
    ensemble_elastic_bias_sd, ensemble_gb_bias_sd,
    ensemble_knn_bias_sd, ensemble_lasso_bias_sd, ensemble_linear_bias_sd,
    ensemble_rf_bias_sd, ensemble_ridge_bias_sd, ensemble_rpart_bias_sd,
    ensemble_svm_bias_sd, ensemble_tree_bias_sd, ensemble_xgb_bias_sd
  ), 4),
  "Mean_MAE" = round(c(
    0, bag_rf_MAE_mean, bagging_MAE_mean, bayesglm_MAE_mean,
    bayesrnn_MAE_mean, boost_rf_MAE_mean, cubist_MAE_mean, earth_MAE_mean, elastic_MAE_mean,
    gam_MAE_mean, gb_MAE_mean, knn_MAE_mean, lasso_MAE_mean, linear_MAE_mean,
    neuralnet_MAE_mean, pls_MAE_mean, pcr_MAE_mean, rf_MAE_mean,
    ridge_MAE_mean, rpart_MAE_mean, svm_MAE_mean, tree_MAE_mean, xgb_MAE_mean,
    ensemble_bag_rf_MAE_mean, ensemble_bagging_MAE_mean, ensemble_bayesglm_MAE_mean,
    ensemble_bayesrnn_MAE_mean, ensemble_boost_rf_MAE_mean, ensemble_cubist_MAE_mean, ensemble_earth_MAE_mean,
    ensemble_elastic_MAE_mean, ensemble_gb_MAE_mean,
    ensemble_knn_MAE_mean, ensemble_lasso_MAE_mean, ensemble_linear_MAE_mean,
    ensemble_rf_MAE_mean, ensemble_ridge_MAE_mean, ensemble_rpart_MAE_mean,
    ensemble_svm_MAE_mean, ensemble_tree_MAE_mean, ensemble_xgb_MAE_mean
  ), 4),
  "Mean_MAE_sd" = round(c(
    0, bag_rf_MAE_sd, bagging_MAE_sd, bayesglm_MAE_sd,
    bayesrnn_MAE_sd, boost_rf_MAE_sd, cubist_MAE_sd, earth_MAE_sd, elastic_MAE_sd,
    gam_MAE_sd, gb_MAE_sd, knn_MAE_sd, lasso_MAE_sd, linear_MAE_sd,
    neuralnet_MAE_sd, pls_MAE_sd, pcr_MAE_sd, rf_MAE_sd,
    ridge_MAE_sd, rpart_MAE_sd, svm_MAE_sd, tree_MAE_sd, xgb_MAE_sd,
    ensemble_bag_rf_MAE_sd, ensemble_bagging_MAE_sd, ensemble_bayesglm_MAE_sd,
    ensemble_bayesrnn_MAE_sd, ensemble_boost_rf_MAE_sd, ensemble_cubist_MAE_sd, ensemble_earth_MAE_sd,
    ensemble_elastic_MAE_sd, ensemble_gb_MAE_sd,
    ensemble_knn_MAE_sd, ensemble_lasso_MAE_sd, ensemble_linear_MAE_sd,
    ensemble_rf_MAE_sd, ensemble_ridge_MAE_sd, ensemble_rpart_MAE_sd,
    ensemble_svm_MAE_sd, ensemble_tree_MAE_sd, ensemble_xgb_MAE_sd
  ), 4),
  "Mean_MSE" = round(c(
    0, bag_rf_MSE_mean, bagging_MSE_mean, bayesglm_MSE_mean,
    bayesrnn_MSE_mean, boost_rf_MSE_mean, cubist_MSE_mean, earth_MSE_mean, elastic_MSE_mean,
    gam_MSE_mean, gb_MSE_mean, knn_MSE_mean, lasso_MSE_mean, linear_MSE_mean,
    neuralnet_MSE_mean, pls_MSE_mean, pcr_MSE_mean, rf_MSE_mean,
    ridge_MSE_mean, rpart_MSE_mean, svm_MSE_mean, tree_MSE_mean, xgb_MSE_mean,
    ensemble_bag_rf_MSE_mean, ensemble_bagging_MSE_mean, ensemble_bayesglm_MSE_mean,
    ensemble_bayesrnn_MSE_mean, ensemble_boost_rf_MSE_mean, ensemble_cubist_MSE_mean, ensemble_earth_MSE_mean,
    ensemble_elastic_MSE_mean, ensemble_gb_MSE_mean,
    ensemble_knn_MSE_mean, ensemble_lasso_MSE_mean, ensemble_linear_MSE_mean,
    ensemble_rf_MSE_mean, ensemble_ridge_MSE_mean, ensemble_rpart_MSE_mean,
    ensemble_svm_MSE_mean, ensemble_tree_MSE_mean, ensemble_xgb_MSE_mean
  ), 4),
  "Mean_MSE_sd" = round(c(
    0, bag_rf_MSE_sd, bagging_MSE_sd, bayesglm_MSE_sd,
    bayesrnn_MSE_sd, boost_rf_MSE_sd, cubist_MSE_sd, earth_MSE_sd, elastic_MSE_sd,
    gam_MSE_sd, gb_MSE_sd, knn_MSE_sd, lasso_MSE_sd, linear_MSE_sd,
    neuralnet_MSE_sd, pls_MSE_sd, pcr_MSE_sd, rf_MSE_sd,
    ridge_MSE_sd, rpart_MSE_sd, svm_MSE_sd, tree_MSE_sd, xgb_MSE_sd,
    ensemble_bag_rf_MSE_sd, ensemble_bagging_MSE_sd, ensemble_bayesglm_MSE_sd,
    ensemble_bayesrnn_MSE_sd, ensemble_boost_rf_MSE_sd, ensemble_cubist_MSE_sd, ensemble_earth_MSE_sd,
    ensemble_elastic_MSE_sd, ensemble_gb_MSE_sd,
    ensemble_knn_MSE_sd, ensemble_lasso_MSE_sd, ensemble_linear_MSE_sd,
    ensemble_rf_MSE_sd, ensemble_ridge_MSE_sd, ensemble_rpart_MSE_sd,
    ensemble_svm_MSE_sd, ensemble_tree_MSE_sd, ensemble_xgb_MSE_sd
  ), 4),
  "Mean_SSE" = round(c(
    0, bag_rf_SSE_mean, bagging_SSE_mean, bayesglm_SSE_mean,
    bayesrnn_SSE_mean, boost_rf_SSE_mean, cubist_SSE_mean, earth_SSE_mean, elastic_SSE_mean,
    gam_SSE_mean, gb_SSE_mean, knn_SSE_mean, lasso_SSE_mean, linear_SSE_mean,
    neuralnet_SSE_mean, pls_SSE_mean, pcr_SSE_mean, rf_SSE_mean,
    ridge_SSE_mean, rpart_SSE_mean, svm_SSE_mean, tree_SSE_mean, xgb_SSE_mean,
    ensemble_bag_rf_SSE_mean, ensemble_bagging_SSE_mean, ensemble_bayesglm_SSE_mean,
    ensemble_bayesrnn_SSE_mean, ensemble_boost_rf_SSE_mean, ensemble_cubist_SSE_mean, ensemble_earth_SSE_mean,
    ensemble_elastic_SSE_mean, ensemble_gb_SSE_mean,
    ensemble_knn_SSE_mean, ensemble_lasso_SSE_mean, ensemble_linear_SSE_mean,
    ensemble_rf_SSE_mean, ensemble_ridge_SSE_mean, ensemble_rpart_SSE_mean,
    ensemble_svm_SSE_mean, ensemble_tree_SSE_mean, ensemble_xgb_SSE_mean
  ), 4),
  "Mean_SSE_sd" = round(c(
    0, bag_rf_SSE_sd, bagging_SSE_sd, bayesglm_SSE_sd,
    bayesrnn_SSE_sd, boost_rf_SSE_sd, cubist_SSE_sd, earth_SSE_sd, elastic_SSE_sd,
    gam_SSE_sd, gb_SSE_sd, knn_SSE_sd, lasso_SSE_sd, linear_SSE_sd,
    neuralnet_SSE_sd, pls_SSE_sd, pcr_SSE_sd, rf_SSE_sd,
    ridge_SSE_sd, rpart_SSE_sd, svm_SSE_sd, tree_SSE_sd, xgb_SSE_sd,
    ensemble_bag_rf_SSE_sd, ensemble_bagging_SSE_sd, ensemble_bayesglm_SSE_sd,
    ensemble_bayesrnn_SSE_sd, ensemble_boost_rf_SSE_sd, ensemble_cubist_SSE_sd, ensemble_earth_SSE_sd,
    ensemble_elastic_SSE_sd, ensemble_gb_SSE_sd,
    ensemble_knn_SSE_sd, ensemble_lasso_SSE_sd, ensemble_linear_SSE_sd,
    ensemble_rf_SSE_sd, ensemble_ridge_SSE_sd, ensemble_rpart_SSE_sd,
    ensemble_svm_SSE_sd, ensemble_tree_SSE_sd, ensemble_xgb_SSE_sd
  ), 4),
  "Mean_data" = round(c(
    actual_mean, bag_rf_predict_value_mean, bagging_predict_value_mean, bayesglm_predict_value_mean,
    bayesrnn_predict_value_mean, boost_rf_predict_value_mean, cubist_predict_value_mean, earth_predict_value_mean, elastic_test_predict_value_mean,
    gam_predict_value_mean, gb_predict_value_mean, knn_predict_value_mean, lasso_predict_value_mean, linear_predict_value_mean,
    neuralnet_test_predict_value_mean, pls_predict_value_mean, pcr_predict_value_mean, rf_predict_value_mean,
    ridge_test_predict_value_mean, rpart_predict_value_mean, svm_predict_value_mean, tree_predict_value_mean, xgb_predict_value_mean,
    ensemble_bag_rf_predict_value_mean, ensemble_bagging_predict_value_mean, ensemble_bayesglm_predict_value_mean,
    ensemble_bayesrnn_predict_value_mean, ensemble_boost_rf_predict_value_mean, ensemble_cubist_predict_value_mean, ensemble_earth_predict_value_mean,
    ensemble_elastic_predict_value_mean, ensemble_gb_predict_value_mean, ensemble_knn_predict_value_mean,
    ensemble_lasso_predict_value_mean, ensemble_linear_predict_value_mean, ensemble_rf_predict_value_mean, ensemble_ridge_predict_value_mean,
    ensemble_rpart_predict_value_mean, ensemble_svm_predict_value_mean, ensemble_tree_predict_value_mean, ensemble_xgb_predict_value_mean
  ), 4),
  "Std_Dev_of_the_model" = round(c(
    actual_sd, bag_rf_sd_mean, bagging_sd_mean, bayesglm_sd_mean, bayesrnn_sd_mean,
    boost_rf_sd_mean, cubist_sd_mean, earth_sd_mean, elastic_sd_mean, gam_sd_mean, gb_sd_mean, knn_sd_mean, lasso_sd_mean,
    linear_sd_mean, neuralnet_sd_mean, pls_sd_mean, pcr_sd_mean, rf_sd_mean, ridge_sd_mean,
    rpart_sd_mean, svm_sd_mean, tree_sd_mean, xgb_sd_mean,
    ensemble_bag_rf_sd_mean, ensemble_bagging_sd_mean, ensemble_bayesglm_sd_mean,
    ensemble_bayesrnn_sd_mean, ensemble_boost_rf_sd_mean, ensemble_cubist_sd_mean, ensemble_earth_sd_mean,
    ensemble_elastic_sd_mean, ensemble_gb_sd_mean, ensemble_knn_sd_mean,
    ensemble_lasso_sd_mean, ensemble_linear_sd_mean,  ensemble_rf_sd_mean, ensemble_ridge_sd_mean,
    ensemble_rpart_sd_mean, ensemble_svm_sd_mean, ensemble_tree_sd_mean, ensemble_xgb_sd_mean
  ), 4),
  "Mean_train_RMSE" = round(c(
    0, bag_rf_train_RMSE_mean, bagging_train_RMSE_mean, bayesglm_train_RMSE_mean, bayesrnn_train_RMSE_mean,
    boost_rf_train_RMSE_mean, cubist_train_RMSE_mean, earth_train_RMSE_mean, elastic_train_RMSE_mean, gam_train_RMSE_mean, gb_train_RMSE_mean,
    knn_train_RMSE_mean, lasso_train_RMSE_mean, linear_train_RMSE_mean, neuralnet_train_RMSE_mean,
    pls_train_RMSE_mean, pcr_train_RMSE_mean, rf_train_RMSE_mean, ridge_train_RMSE_mean,
    rpart_train_RMSE_mean, svm_train_RMSE_mean, tree_train_RMSE_mean, xgb_train_RMSE_mean,
    ensemble_bag_rf_train_RMSE_mean, ensemble_bagging_train_RMSE_mean,ensemble_bayesglm_train_RMSE_mean,
    ensemble_bayesrnn_train_RMSE_mean, ensemble_boost_rf_train_RMSE_mean, ensemble_cubist_train_RMSE_mean, ensemble_earth_train_RMSE_mean,
    ensemble_elastic_train_RMSE_mean, ensemble_gb_train_RMSE_mean, ensemble_knn_train_RMSE_mean,
    ensemble_lasso_train_RMSE_mean, ensemble_linear_train_RMSE_mean, ensemble_rf_train_RMSE_mean, ensemble_ridge_train_RMSE_mean,
    ensemble_rpart_train_RMSE_mean, ensemble_svm_train_RMSE_mean, ensemble_tree_train_RMSE_mean, ensemble_xgb_train_RMSE_mean
  ), 4),
  "Mean_test_RMSE" = round(c(
    0, bag_rf_test_RMSE_mean, bagging_test_RMSE_mean, bayesglm_test_RMSE_mean,
    bayesrnn_test_RMSE_mean, boost_rf_test_RMSE_mean, cubist_test_RMSE_mean, earth_test_RMSE_mean, elastic_test_RMSE_mean,
    gam_test_RMSE_mean, gb_test_RMSE_mean, knn_test_RMSE_mean, lasso_test_RMSE_mean, linear_test_RMSE_mean,
    neuralnet_test_RMSE_mean, pls_test_RMSE_mean, pcr_test_RMSE_mean, rf_test_RMSE_mean, ridge_test_RMSE_mean,
    rpart_test_RMSE_mean, svm_test_RMSE_mean, tree_test_RMSE_mean, xgb_test_RMSE_mean,
    ensemble_bag_rf_test_RMSE_mean, ensemble_bagging_test_RMSE_mean, ensemble_bayesglm_test_RMSE_mean,
    ensemble_bayesrnn_test_RMSE_mean, ensemble_boost_rf_test_RMSE_mean, ensemble_cubist_test_RMSE_mean, ensemble_earth_test_RMSE_mean,
    ensemble_elastic_test_RMSE_mean, ensemble_gb_test_RMSE_mean, ensemble_knn_test_RMSE_mean,
    ensemble_lasso_test_RMSE_mean, ensemble_linear_test_RMSE_mean,ensemble_rf_test_RMSE_mean, ensemble_ridge_test_RMSE,
    ensemble_rpart_test_RMSE_mean, ensemble_svm_test_RMSE_mean, ensemble_tree_test_RMSE_mean, ensemble_xgb_test_RMSE_mean
  ), 4),
  "Mean_validation_RMSE" = round(c(
    0, bag_rf_validation_RMSE_mean, bagging_validation_RMSE_mean, bayesglm_validation_RMSE_mean,
    bayesrnn_validation_RMSE_mean, boost_rf_validation_RMSE_mean, cubist_validation_RMSE_mean, earth_validation_RMSE_mean, elastic_validation_RMSE_mean,
    gam_validation_RMSE_mean, gb_validation_RMSE_mean, knn_validation_RMSE_mean, lasso_validation_RMSE_mean,
    linear_validation_RMSE_mean, neuralnet_validation_RMSE_mean, pls_validation_RMSE_mean,
    pcr_validation_RMSE_mean, rf_validation_RMSE_mean, ridge_validation_RMSE_mean,
    rpart_validation_RMSE_mean, svm_validation_RMSE_mean, tree_validation_RMSE_mean, xgb_validation_RMSE_mean,
    ensemble_bag_rf_validation_RMSE_mean, ensemble_bagging_validation_RMSE_mean, ensemble_bayesglm_validation_RMSE_mean,
    ensemble_bayesrnn_validation_RMSE_mean, ensemble_boost_rf_validation_RMSE_mean, ensemble_cubist_validation_RMSE_mean, ensemble_earth_validation_RMSE_mean,
    ensemble_elastic_validation_RMSE_mean, ensemble_gb_validation_RMSE_mean, ensemble_knn_validation_RMSE_mean,
    ensemble_lasso_validation_RMSE_mean, ensemble_linear_validation_RMSE_mean, ensemble_rf_validation_RMSE_mean, ensemble_ridge_validation_RMSE_mean,
    ensemble_rpart_validation_RMSE_mean, ensemble_svm_validation_RMSE_mean, ensemble_tree_validation_RMSE_mean, ensemble_xgb_validation_RMSE_mean
  ), 4),
  "Holdout_vs_train_mean" = round(c(
    0, bag_rf_holdout_vs_train_mean, bagging_holdout_vs_train_mean, bayesglm_holdout_vs_train_mean, bayesrnn_holdout_vs_train_mean,
    boost_rf_holdout_vs_train_mean, cubist_holdout_vs_train_mean, earth_holdout_vs_train_mean, elastic_holdout_vs_train_mean, gam_holdout_vs_train_mean, gb_holdout_vs_train_mean,
    knn_holdout_vs_train_mean, lasso_holdout_vs_train_mean, linear_holdout_vs_train_mean, neuralnet_holdout_vs_train_mean,
    pls_holdout_vs_train_mean, pcr_holdout_vs_train_mean, rf_holdout_vs_train_mean, ridge_holdout_vs_train_mean,
    rpart_holdout_vs_train_mean, svm_holdout_vs_train_mean, tree_holdout_vs_train_mean, xgb_holdout_vs_train_mean,
    ensemble_bag_rf_holdout_vs_train_mean, ensemble_bagging_holdout_vs_train_mean,  ensemble_bayesglm_holdout_vs_train_mean,
    ensemble_bayesrnn_holdout_vs_train_mean, ensemble_boost_rf_holdout_vs_train_mean, ensemble_cubist_holdout_vs_train_mean, ensemble_earth_holdout_vs_train_mean,
    ensemble_elastic_holdout_vs_train_mean, ensemble_gb_holdout_vs_train_mean, ensemble_knn_holdout_vs_train_mean,
    ensemble_lasso_holdout_vs_train_mean, ensemble_linear_holdout_vs_train_mean, ensemble_rf_holdout_vs_train_mean, ensemble_ridge_holdout_vs_train_mean,
    ensemble_rpart_holdout_vs_train_mean, ensemble_svm_holdout_vs_train_mean, ensemble_tree_holdout_vs_train_mean, ensemble_xgb_holdout_vs_train_mean
  ), 4),
  "Holdout_vs_train_sd" = round(c(
    0, bag_rf_holdout_vs_train_sd, bagging_holdout_vs_train_sd, bayesglm_holdout_vs_train_sd, bayesrnn_holdout_vs_train_sd,
    boost_rf_holdout_vs_train_sd, cubist_holdout_vs_train_sd, earth_holdout_vs_train_sd, elastic_holdout_vs_train_sd, gam_holdout_vs_train_sd, gb_holdout_vs_train_sd,
    knn_holdout_vs_train_sd, lasso_holdout_vs_train_sd, linear_holdout_vs_train_sd, neuralnet_holdout_vs_train_sd,
    pls_holdout_vs_train_sd, pcr_holdout_vs_train_sd, rf_holdout_vs_train_sd, ridge_holdout_vs_train_sd,
    rpart_holdout_vs_train_sd, svm_holdout_vs_train_sd, tree_holdout_vs_train_sd, xgb_holdout_vs_train_sd,
    ensemble_bag_rf_holdout_vs_train_sd, ensemble_bagging_holdout_vs_train_sd,  ensemble_bayesglm_holdout_vs_train_sd,
    ensemble_bayesrnn_holdout_vs_train_sd, ensemble_boost_rf_holdout_vs_train_sd, ensemble_cubist_holdout_vs_train_sd, ensemble_earth_holdout_vs_train_sd,
    ensemble_elastic_holdout_vs_train_sd, ensemble_gb_holdout_vs_train_sd, ensemble_knn_holdout_vs_train_sd,
    ensemble_lasso_holdout_vs_train_sd, ensemble_linear_holdout_vs_train_sd, ensemble_rf_holdout_vs_train_sd, ensemble_ridge_holdout_vs_train_sd,
    ensemble_rpart_holdout_vs_train_sd, ensemble_svm_holdout_vs_train_sd, ensemble_tree_holdout_vs_train_sd, ensemble_xgb_holdout_vs_train_sd
  ), 4),
  "Duration" = round(c(
    0, bag_rf_duration_mean, bagging_duration_mean, bayesglm_duration_mean, bayesrnn_duration_mean,
    boost_rf_duration_mean, cubist_duration_mean, earth_duration_mean, elastic_duration_mean, gam_duration_mean, gb_duration_mean,
    knn_duration_mean, lasso_duration_mean, linear_duration_mean, neuralnet_duration_mean,
    pls_duration_mean, pcr_duration_mean, rf_duration_mean, ridge_duration_mean,
    rpart_duration_mean, svm_duration_mean, tree_duration_mean, xgb_duration_mean,
    ensemble_bag_rf_duration_mean, ensemble_bagging_duration_mean, ensemble_bayesglm_duration_mean,
    ensemble_bayesrnn_duration_mean, ensemble_boost_rf_duration_mean, ensemble_cubist_duration_mean, ensemble_earth_duration_mean,
    ensemble_elastic_duration_mean, ensemble_gb_duration_mean, ensemble_knn_duration_mean,
    ensemble_lasso_duration_mean, ensemble_linear_duration_mean, ensemble_rf_duration_mean, ensemble_ridge_duration_mean,
    ensemble_rpart_duration_mean, ensemble_svm_duration_mean, ensemble_tree_duration_mean, ensemble_xgb_duration_mean
  ), 4),
  "Duration_sd" = round(c(
    0, bag_rf_duration_sd, bagging_duration_sd, bayesglm_duration_sd, bayesrnn_duration_sd,
    boost_rf_duration_sd, cubist_duration_sd, earth_duration_sd, elastic_duration_sd, gam_duration_sd, gb_duration_sd,
    knn_duration_sd, lasso_duration_sd, linear_duration_sd, neuralnet_duration_sd,
    pls_duration_sd, pcr_duration_sd, rf_duration_sd, ridge_duration_sd,
    rpart_duration_sd, svm_duration_sd, tree_duration_sd, xgb_duration_sd,
    ensemble_bag_rf_duration_sd, ensemble_bagging_duration_sd, ensemble_bayesglm_duration_sd,
    ensemble_bayesrnn_duration_sd, ensemble_boost_rf_duration_sd, ensemble_cubist_duration_sd, ensemble_earth_duration_sd,
    ensemble_elastic_duration_sd, ensemble_gb_duration_sd, ensemble_knn_duration_sd,
    ensemble_lasso_duration_sd, ensemble_linear_duration_sd, ensemble_rf_duration_sd, ensemble_ridge_duration_sd,
    ensemble_rpart_duration_sd, ensemble_svm_duration_sd, ensemble_tree_duration_sd, ensemble_xgb_duration_sd
  ), 4)
)

holdout_vs_train_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" = c(
      rep("Bagged Random Forest", numresamples), c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
      c(rep("BayesRNN", numresamples)), c(rep("Boost Random Forest", numresamples)),
      c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
      c(rep("Gradient Boosted", numresamples)), c(rep("K-Nearest Neighbors", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
      c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
      c(rep("Random Forest", numresamples)), c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
      c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
      c(rep("Ensemble Bagged Random Forest", numresamples)), c(rep("Ensemble Bagging", numresamples)),
      c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
      c(rep("Ensemble Boosted Random Forest", numresamples)), c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
      c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble K-Nearest Neighbors", numresamples)), c(rep("Ensemble Lasso", numresamples)),
      c(rep("Ensemble Linear", numresamples)), c(rep("Ensemble Random Forest", numresamples)), c(rep("Ensemble Ridge", numresamples)),
      c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
      c(rep("Ensemble Trees", numresamples)), c(rep("Ensemble XGBoost", numresamples))
    ),
    "data" = c(
      bag_rf_holdout_vs_train, bagging_holdout_vs_train, bayesglm_holdout_vs_train,
      bayesrnn_holdout_vs_train, boost_rf_holdout_vs_train,
      cubist_holdout_vs_train, earth_holdout_vs_train, elastic_holdout_vs_train_df$elastic_holdout_vs_train[2:nrow(elastic_holdout_vs_train_df)], gam_holdout_vs_train,
      gb_holdout_vs_train, knn_holdout_vs_train, lasso_holdout_vs_train_df$lasso_holdout_vs_train[2:nrow(lasso_holdout_vs_train_df)], linear_holdout_vs_train,
      neuralnet_holdout_vs_train, pcr_holdout_vs_train, pls_holdout_vs_train,
      rf_holdout_vs_train, ridge_holdout_vs_train_df$ridge_holdout_vs_train[2:nrow(ridge_holdout_vs_train_df)], rpart_holdout_vs_train,
      svm_holdout_vs_train, tree_holdout_vs_train, xgb_holdout_vs_train,
      ensemble_bag_rf_holdout_vs_train, ensemble_bagging_holdout_vs_train,
      ensemble_bayesglm_holdout_vs_train, ensemble_bayesrnn_holdout_vs_train,
      ensemble_boost_rf_holdout_vs_train, ensemble_cubist_holdout_vs_train, ensemble_earth_holdout_vs_train,
      ensemble_elastic_holdout_vs_train_df$ensemble_elastic_holdout_vs_train[2:nrow(ensemble_elastic_holdout_vs_train_df)],
      ensemble_gb_holdout_vs_train, ensemble_knn_holdout_vs_train, ensemble_lasso_holdout_vs_train_df$ensemble_lasso_holdout_vs_train[2:nrow(ensemble_lasso_holdout_vs_train_df)],
      ensemble_linear_holdout_vs_train, ensemble_rf_holdout_vs_train, ensemble_ridge_holdout_vs_train_df$ensemble_ridge_holdout_vs_train[2:nrow(ensemble_ridge_holdout_vs_train_df)],
      ensemble_rpart_holdout_vs_train, ensemble_svm_holdout_vs_train,
      ensemble_tree_holdout_vs_train, ensemble_xgb_holdout_vs_train_df$ensemble_xgb_holdout_vs_train[2:nrow(ensemble_xgb_holdout_vs_train_df)]
    ),
    "mean" = rep(c(
      bag_rf_holdout_vs_train_mean, bagging_holdout_vs_train_mean, bayesglm_holdout_vs_train_mean,
      bayesrnn_holdout_vs_train_mean, boost_rf_holdout_vs_train_mean,
      cubist_holdout_vs_train_mean, earth_holdout_vs_train_mean, elastic_holdout_vs_train_mean, gam_holdout_vs_train_mean,
      gb_holdout_vs_train_mean, knn_holdout_vs_train_mean, lasso_holdout_vs_train_mean, linear_holdout_vs_train_mean,
      neuralnet_holdout_vs_train_mean, pcr_holdout_vs_train_mean, pls_holdout_vs_train_mean,
      rf_holdout_vs_train_mean, ridge_holdout_vs_train_mean, rpart_holdout_vs_train_mean,
      svm_holdout_vs_train_mean, tree_holdout_vs_train_mean, xgb_holdout_vs_train_mean,
      ensemble_bag_rf_holdout_vs_train_mean, ensemble_bagging_holdout_vs_train_mean,
      ensemble_bayesglm_holdout_vs_train_mean, ensemble_bayesrnn_holdout_vs_train_mean,
      ensemble_boost_rf_holdout_vs_train_mean, ensemble_cubist_holdout_vs_train_mean, ensemble_earth_holdout_vs_train_mean,
      ensemble_elastic_holdout_vs_train_mean,
      ensemble_gb_holdout_vs_train_mean, ensemble_knn_holdout_vs_train_mean, ensemble_lasso_holdout_vs_train_mean, ensemble_linear_holdout_vs_train_mean,
      ensemble_rf_holdout_vs_train_mean, ensemble_ridge_holdout_vs_train_mean,
      ensemble_rpart_holdout_vs_train_mean, ensemble_svm_holdout_vs_train_mean,
      ensemble_tree_holdout_vs_train_mean, ensemble_xgb_holdout_vs_train_mean
    ), each = numresamples)
  )

holdout_vs_train_plot <- ggplot2::ggplot(data = holdout_vs_train_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean)) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("Holdout RMSE / train RMSE, fixed scales\nHoldout RMSE / train RMSE by model, fixed scales, closer to one is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Holdout RMSE / train RMSE, fixed scales closer to one is better \n The black horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("holdout_vs_train_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("holdout_vs_train_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("holdout_vs_train_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("holdout_vs_train_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("holdout_vs_train_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("holdout_vs_train_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

holdout_vs_train_plot2 <- ggplot2::ggplot(data = holdout_vs_train_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean)) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Holdout RMSE / train RMSE, free scales\nHoldout RMSE / train RMSE by model, closer to one is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Holdout RMSE / train RMSE, free scales closer to one is better \n The black horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("Holdout_vs_train_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("Holdout_vs_train_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("Holdout_vs_train_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("Holdout_vs_train_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("Holdout_vs_train_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("Holdout_vs_train_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

bias_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" = c(
      rep("Bagged Random Forest", numresamples), c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
      c(rep("BayesRNN", numresamples)), c(rep("Boost Random Forest", numresamples)),
      c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
      c(rep("Gradient Boosted", numresamples)), c(rep("K-Nearest Neighbors", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
      c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
      c(rep("Random Forest", numresamples)), c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
      c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
      c(rep("Ensemble Bagged Random Forest", numresamples)), c(rep("Ensemble Bagging", numresamples)),
      c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
      c(rep("Ensemble Boosted Random Forest", numresamples)), c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
      c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble K-Nearest Neighbors", numresamples)), c(rep("Ensemble Lasso", numresamples)),
      c(rep("Ensemble Linear", numresamples)), c(rep("Ensemble Random Forest", numresamples)), c(rep("Ensemble Ridge", numresamples)),
      c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
      c(rep("Ensemble Trees", numresamples)), c(rep("Ensemble XGBoost", numresamples))
    ),
    "data" = c(
      bag_rf_bias_mean, bagging_bias_mean, bayesglm_bias_mean,
      bayesrnn_bias_mean, boost_rf_bias_mean,
      cubist_bias_mean, earth_bias_mean, elastic_bias_mean, gam_bias_mean,
      gb_bias_mean, knn_bias_mean, lasso_bias_mean, linear_bias_mean,
      neuralnet_bias_mean, pcr_bias_mean, pls_bias_mean,
      rf_bias_mean, ridge_bias_mean, rpart_bias_mean,
      svm_bias_mean, tree_bias_mean, xgb_bias_mean,
      ensemble_bag_rf_bias_mean, ensemble_bagging_bias_mean,
      ensemble_bayesglm_bias_mean, ensemble_bayesrnn_bias_mean,
      ensemble_boost_rf_bias_mean, ensemble_cubist_bias_mean, ensemble_earth_bias_mean,
      ensemble_elastic_bias_mean, ensemble_gb_bias_mean, ensemble_knn_bias_mean, ensemble_lasso_bias_mean,
      ensemble_linear_bias_mean, ensemble_rf_bias_mean, ensemble_ridge_bias_mean,
      ensemble_rpart_bias_mean, ensemble_svm_bias_mean,
      ensemble_tree_bias_mean, ensemble_xgb_bias_mean
    )
  )

bias_plot <- ggplot2::ggplot(data = bias_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5) +
  ggplot2::ggtitle("Bias plot\nBias value by model, closer to zero is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "Bias value, closer to zero is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("bias_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("bias_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("bias_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("bias_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("bias_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("bias_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

MAE_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" = c(
      rep("Bagged Random Forest", numresamples), c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
      c(rep("BayesRNN", numresamples)), c(rep("Boost Random Forest", numresamples)),
      c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
      c(rep("Gradient Boosted", numresamples)), c(rep("K-Nearest Neighbors", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
      c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
      c(rep("Random Forest", numresamples)), c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
      c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
      c(rep("Ensemble Bagged Random Forest", numresamples)), c(rep("Ensemble Bagging", numresamples)),
      c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
      c(rep("Ensemble Boosted Random Forest", numresamples)), c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
      c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble K-Nearest Neighbors", numresamples)), c(rep("Ensemble Lasso", numresamples)),
      c(rep("Ensemble Linear", numresamples)), c(rep("Ensemble Random Forest", numresamples)), c(rep("Ensemble Ridge", numresamples)),
      c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
      c(rep("Ensemble Trees", numresamples)), c(rep("Ensemble XGBoost", numresamples))
    ),
    "data" = c(
      bag_rf_MAE_mean, bagging_MAE_mean, bayesglm_MAE_mean,
      bayesrnn_MAE_mean, boost_rf_MAE_mean,
      cubist_MAE_mean, earth_MAE_mean, elastic_MAE_mean, gam_MAE_mean,
      gb_MAE_mean, knn_MAE_mean, lasso_MAE_mean, linear_MAE_mean,
      neuralnet_MAE_mean, pcr_MAE_mean, pls_MAE_mean,
      rf_MAE_mean, ridge_MAE_mean, rpart_MAE_mean,
      svm_MAE_mean, tree_MAE_mean, xgb_MAE_mean,
      ensemble_bag_rf_MAE_mean, ensemble_bagging_MAE_mean,
      ensemble_bayesglm_MAE_mean, ensemble_bayesrnn_MAE_mean,
      ensemble_boost_rf_MAE_mean, ensemble_cubist_MAE_mean, ensemble_earth_MAE_mean, ensemble_elastic_MAE_mean,
      ensemble_gb_MAE_mean, ensemble_knn_MAE_mean, ensemble_lasso_MAE_mean,
      ensemble_linear_MAE_mean, ensemble_rf_MAE_mean, ensemble_ridge_MAE_mean,
      ensemble_rpart_MAE_mean, ensemble_svm_MAE_mean,
      ensemble_tree_MAE_mean, ensemble_xgb_MAE_mean
    )
  )

MAE_plot <- ggplot2::ggplot(data = MAE_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5) +
  ggplot2::ggtitle("MAE (Mean Absolute Error) plot\nMAE value by model, closer to zero is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "MAE (Mean Absolute Error) value, closer to zero is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("MAE_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("MAE_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("MAE_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("MAE_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("MAE_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("MAE_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

MSE_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" = c(
      rep("Bagged Random Forest", numresamples), c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
      c(rep("BayesRNN", numresamples)), c(rep("Boost Random Forest", numresamples)),
      c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
      c(rep("Gradient Boosted", numresamples)), c(rep("K-Nearest Neighbors", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
      c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
      c(rep("Random Forest", numresamples)), c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
      c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
      c(rep("Ensemble Bagged Random Forest", numresamples)), c(rep("Ensemble Bagging", numresamples)),
      c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
      c(rep("Ensemble Boosted Random Forest", numresamples)), c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
      c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble K-Nearest Neighbors", numresamples)), c(rep("Ensemble Lasso", numresamples)),
      c(rep("Ensemble Linear", numresamples)), c(rep("Ensemble Random Forest", numresamples)), c(rep("Ensemble Ridge", numresamples)),
      c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
      c(rep("Ensemble Trees", numresamples)), c(rep("Ensemble XGBoost", numresamples))
    ),
    "data" = c(
      bag_rf_MSE_mean, bagging_MSE_mean, bayesglm_MSE_mean,
      bayesrnn_MSE_mean, boost_rf_MSE_mean,
      cubist_MSE_mean, earth_MSE_mean, elastic_MSE_mean, gam_MSE_mean,
      gb_MSE_mean, knn_MSE_mean, lasso_MSE_mean, linear_MSE_mean,
      neuralnet_MSE_mean, pcr_MSE_mean, pls_MSE_mean,
      rf_MSE_mean, ridge_MSE_mean, rpart_MSE_mean,
      svm_MSE_mean, tree_MSE_mean, xgb_MSE_mean,
      ensemble_bag_rf_MSE_mean, ensemble_bagging_MSE_mean,
      ensemble_bayesglm_MSE_mean, ensemble_bayesrnn_MSE_mean,
      ensemble_boost_rf_MSE_mean, ensemble_cubist_MSE_mean, ensemble_earth_test_RMSE_mean, ensemble_elastic_MSE_mean,
      ensemble_gb_MSE_mean, ensemble_knn_MSE_mean, ensemble_lasso_MSE_mean,
      ensemble_linear_MSE_mean, ensemble_rf_MSE_mean, ensemble_ridge_MSE_mean,
      ensemble_rpart_MSE_mean, ensemble_svm_MSE_mean,
      ensemble_tree_MSE_mean, ensemble_xgb_MSE_mean
    )
  )

MSE_plot <- ggplot2::ggplot(data = MSE_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5) +
  ggplot2::ggtitle("MSE (Mean Squared Error) plot\nMSE value by model, closer to zero is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "MSE (Mean Squared Error) value, closer to zero is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("MSE_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("MSE_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("MSE_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("MSE_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("MSE_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("MSE_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

SSE_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" = c(
      rep("Bagged Random Forest", numresamples), c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
      c(rep("BayesRNN", numresamples)), c(rep("Boost Random Forest", numresamples)),
      c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
      c(rep("Gradient Boosted", numresamples)), c(rep("K-Nearest Neighbors", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
      c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
      c(rep("Random Forest", numresamples)), c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
      c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
      c(rep("Ensemble Bagged Random Forest", numresamples)), c(rep("Ensemble Bagging", numresamples)),
      c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
      c(rep("Ensemble Boosted Random Forest", numresamples)), c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
      c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble K-Nearest Neighbors", numresamples)), c(rep("Ensemble Lasso", numresamples)),
      c(rep("Ensemble Linear", numresamples)), c(rep("Ensemble Random Forest", numresamples)), c(rep("Ensemble Ridge", numresamples)),
      c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
      c(rep("Ensemble Trees", numresamples)), c(rep("Ensemble XGBoost", numresamples))
    ),
    "data" = c(
      bag_rf_SSE_mean, bagging_SSE_mean, bayesglm_SSE_mean,
      bayesrnn_SSE_mean, boost_rf_SSE_mean,
      cubist_SSE_mean, earth_SSE_mean, elastic_SSE_mean, gam_SSE_mean,
      gb_SSE_mean, knn_SSE_mean, lasso_SSE_mean, linear_SSE_mean,
      neuralnet_SSE_mean, pcr_SSE_mean, pls_SSE_mean,
      rf_SSE_mean, ridge_SSE_mean, rpart_SSE_mean,
      svm_SSE_mean, tree_SSE_mean, xgb_SSE_mean,
      ensemble_bag_rf_SSE_mean, ensemble_bagging_SSE_mean,
      ensemble_bayesglm_SSE_mean, ensemble_bayesrnn_SSE_mean,
      ensemble_boost_rf_SSE_mean, ensemble_cubist_SSE_mean, ensemble_earth_SSE_mean, ensemble_ridge_SSE_mean,
      ensemble_gb_SSE_mean, ensemble_knn_SSE_mean, ensemble_lasso_SSE_mean,
      ensemble_linear_SSE_mean, ensemble_rf_SSE_mean, ensemble_ridge_SSE_mean,
      ensemble_rpart_SSE_mean, ensemble_svm_SSE_mean,
      ensemble_tree_SSE_mean, ensemble_xgb_SSE_mean
    )
  )

SSE_plot <- ggplot2::ggplot(data = SSE_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5) +
  ggplot2::ggtitle("SSE (Sum of Squares Error) plot\nSSE value by model, closer to zero is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "SSE (Sum of Squares Error) value, closer to zero is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("SSE_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("SSE_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("SSE_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("SSE_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("SSE_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("SSE_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

summary_results <- summary_results %>% dplyr::arrange(Mean_holdout_RMSE)

final_results <- reactable::reactable(summary_results,
                                      searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                      striped = TRUE, highlight = TRUE, resizable = TRUE
) %>%
  reactablefmtr::add_title("RMSE, means, fitting, model summaries of the train, test and validation sets")


#### <-----------------------------------------  8. Summary data visualizations ----------------------------------------------------> ####

#### Bagged random forest visualizations ####
bag_rf_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_bag_rf),
  residuals = c(test$y, validation$y) - y_hat_bag_rf
)

bag_rf_pred_vs_actual <- ggplot2::ggplot(bag_rf_df, mapping = aes(x = actual, y = as.numeric(y_hat_bag_rf))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Bagged Random Forest model: Predicted vs actual", x = "Actual", y = "Predicted")

bag_rf_resid_vs_actual <- ggplot2::ggplot(bag_rf_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Bagged Random Forest model: Residuals", x = "Actual", y = "Predicted")

bag_rf_hist_residuals <- ggplot2::ggplot(bag_rf_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Bagged Random Forest model: Histogram of residuals, each bar = 10 rows of data")

bag_rf_qq <- ggplot2::ggplot(bag_rf_df, aes(sample = as.numeric(y_hat_bag_rf))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Bagged Random forest model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### bagging data visualizations ####
bagging_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_bagging),
  residuals = c(test$y, validation$y) - y_hat_bagging
)

bagging_pred_vs_actual <- ggplot2::ggplot(bagging_df, mapping = aes(x = actual, y = as.numeric(y_hat_bagging))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "bagging model: Predicted vs actual", x = "Actual", y = "Predicted")

bagging_resid_vs_actual <- ggplot2::ggplot(bagging_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "bagging model: Residuals", x = "Actual", y = "Predicted")

bagging_hist_residuals <- ggplot2::ggplot(bagging_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "bagging model: Histogram of residuals, each bar = 10 rows of data")

bagging_qq <- ggplot2::ggplot(bagging_df, aes(sample = as.numeric(y_hat_bagging))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Bagging model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Bayes GLM visualizations ####
bayesglm_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_bayesglm),
  residuals = c(test$y, validation$y) - y_hat_bayesglm
)

bayesglm_pred_vs_actual <- ggplot2::ggplot(bayesglm_df, mapping = aes(x = actual, y = as.numeric(y_hat_bayesglm))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "BayesGLM model: Predicted vs actual", x = "Actual", y = "Predicted")

bayesglm_resid_vs_actual <- ggplot2::ggplot(bayesglm_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "BayesGLM model: Residuals", x = "Actual", y = "Predicted")

bayesglm_hist_residuals <- ggplot2::ggplot(bayesglm_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "BayesGLM model: Histogram of residuals, each bar = 10 rows of data")

bayesglm_qq <- ggplot2::ggplot(bayesglm_df, aes(sample = as.numeric(y_hat_bayesglm))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "BayesGLM model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Bayes RNN visualizations ####
bayesrnn_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_bayesrnn),
  residuals = c(test$y, validation$y) - y_hat_bayesrnn
)

bayesrnn_pred_vs_actual <- ggplot2::ggplot(bayesrnn_df, mapping = aes(x = actual, y = as.numeric(y_hat_bayesrnn))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Bayes RNN model: Predicted vs actual", x = "Actual", y = "Predicted")

bayesrnn_resid_vs_actual <- ggplot2::ggplot(bayesrnn_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Bayes RNN model: Residuals", x = "Actual", y = "Predicted")

bayesrnn_hist_residuals <- ggplot2::ggplot(bayesrnn_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Bayes RNN model: Histogram of residuals, each bar = 10 rows of data")

bayesrnn_qq <- ggplot2::ggplot(bayesrnn_df, aes(sample = as.numeric(y_hat_bayesrnn))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "BayesRNN model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")


#### BoostRF visualizations ####
boost_rf_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_boost_rf),
  residuals = c(test$y, validation$y) - y_hat_boost_rf
)

boost_rf_pred_vs_actual <- ggplot2::ggplot(boost_rf_df, mapping = aes(x = actual, y = as.numeric(y_hat_boost_rf))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Boost_rf model: Predicted vs actual", x = "Actual", y = "Predicted")

boost_rf_resid_vs_actual <- ggplot2::ggplot(boost_rf_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Boost_rf model: Residuals", x = "Actual", y = "Predicted")

boost_rf_hist_residuals <- ggplot2::ggplot(boost_rf_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Boost_rf model: Histogram of residuals, each bar = 10 rows of data")

boost_rf_qq <- ggplot2::ggplot(boost_rf_df, aes(sample = as.numeric(y_hat_boost_rf))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Boost Random Forest model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Cubist visualizations
cubist_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_cubist),
  residuals = c(test$y, validation$y) - y_hat_cubist
)

cubist_pred_vs_actual <- ggplot2::ggplot(cubist_df, mapping = aes(x = actual, y = as.numeric(y_hat_cubist))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "cubist model: Predicted vs actual", x = "Actual", y = "Predicted")

cubist_resid_vs_actual <- ggplot2::ggplot(cubist_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "cubist model: Residuals", x = "Actual", y = "Predicted")

cubist_hist_residuals <- ggplot2::ggplot(cubist_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "cubist model: Histogram of residuals, each bar = 10 rows of data")

cubist_qq <- ggplot2::ggplot(cubist_df, aes(sample = as.numeric(y_hat_cubist))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Cubist model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Earth visualizations
earth_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_earth),
  residuals = c(test$y, validation$y) - y_hat_earth
)

earth_pred_vs_actual <- ggplot2::ggplot(earth_df, mapping = aes(x = actual, y = as.numeric(y_hat_earth))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "earth model: Predicted vs actual", x = "Actual", y = "Predicted")

earth_resid_vs_actual <- ggplot2::ggplot(earth_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "earth model: Residuals", x = "Actual", y = "Predicted")

earth_hist_residuals <- ggplot2::ggplot(earth_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "earth model: Histogram of residuals, each bar = 10 rows of data")

earth_qq <- ggplot2::ggplot(earth_df, aes(sample = as.numeric(y_hat_earth))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "earth model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")


#### Elastic Net visualizations ####
elastic_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_elastic),
  residuals = c(test$y, validation$y) - y_hat_elastic
)

elastic_pred_vs_actual <- ggplot2::ggplot(elastic_df, mapping = aes(x = actual, y = as.numeric(y_hat_elastic))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Elastic Net Model model: Predicted vs actual", x = "Actual", y = "Predicted")

elastic_resid_vs_actual <- ggplot2::ggplot(elastic_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Elastic Net Model model: Residuals", x = "Actual", y = "Predicted")

elastic_hist_residuals <- ggplot2::ggplot(elastic_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Elastic Net Model model: Histogram of residuals, each bar = 10 rows of data")

elastic_qq <- ggplot2::ggplot(elastic_df, aes(sample = as.numeric(y_hat_elastic))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Elastic model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")


#### Generalized Additve Model with splines (GAM) ####
gam_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_gam),
  residuals = c(test$y, validation$y) - y_hat_gam
)

gam_pred_vs_actual <- ggplot2::ggplot(gam_df, mapping = aes(x = actual, y = as.numeric(y_hat_gam))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Gam model: Predicted vs actual", x = "Actual", y = "Predicted")

gam_resid_vs_actual <- ggplot2::ggplot(gam_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Gam model: Residuals", x = "Actual", y = "Predicted")

gam_hist_residuals <- ggplot2::ggplot(gam_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Gam model: Histogram of residuals, each bar = 10 rows of data")

gam_qq <- ggplot2::ggplot(gam_df, aes(sample = as.numeric(y_hat_gam))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Generalized Additive Model with splines: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Gradient Boosted visualizations ####
gb_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_gb),
  residuals = c(test$y, validation$y) - y_hat_gb
)

gb_pred_vs_actual <- ggplot2::ggplot(gb_df, mapping = aes(x = actual, y = as.numeric(y_hat_gb))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Gradient Boosted model: Predicted vs actual", x = "Actual", y = "Predicted")

gb_resid_vs_actual <- ggplot2::ggplot(gb_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Gradient Boosted model: Residuals", x = "Actual", y = "Predicted")

gb_hist_residuals <- ggplot2::ggplot(gb_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Gradient Boosted model: Histogram of residuals, each bar = 10 rows of data")

gb_qq <- ggplot2::ggplot(gb_df, aes(sample = as.numeric(y_hat_gb))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Gradient Boosted model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### K-Nearest Neighbors visualizations ####
knn_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_knn),
  residuals = c(test$y, validation$y) - y_hat_knn
)

knn_pred_vs_actual <- ggplot2::ggplot(knn_df, mapping = aes(x = actual, y = as.numeric(y_hat_knn))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "K-Nearest Neighbors model: Predicted vs actual", x = "Actual", y = "Predicted")

knn_resid_vs_actual <- ggplot2::ggplot(knn_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "K-Nearest Neighbors model: Residuals", x = "Actual", y = "Predicted")

knn_hist_residuals <- ggplot2::ggplot(knn_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "K-Nearest Neighbors model: Histogram of residuals, each bar = 10 rows of data")

knn_qq <- ggplot2::ggplot(knn_df, aes(sample = as.numeric(y_hat_knn))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "KNN model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### lasso data visualizations ####
lasso_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_lasso),
  residuals = c(test$y, validation$y) - y_hat_lasso
)

lasso_pred_vs_actual <- ggplot2::ggplot(lasso_df, mapping = aes(x = actual, y = as.numeric(y_hat_lasso))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "lasso model: Predicted vs actual", x = "Actual", y = "Predicted")

lasso_resid_vs_actual <- ggplot2::ggplot(lasso_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "lasso model: Residuals", x = "Actual", y = "Predicted")

lasso_hist_residuals <- ggplot2::ggplot(lasso_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "lasso model: Histogram of residuals, each bar = 10 rows of data")

lasso_qq <- ggplot2::ggplot(lasso_df, aes(sample = as.numeric(y_hat_lasso))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Lasso model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")


####  Linear visualizations ####
linear_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_linear),
  residuals = c(test$y, validation$y) - y_hat_linear
)

linear_pred_vs_actual <- ggplot2::ggplot(linear_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "linear model: Predicted vs actual", x = "Actual", y = "Predicted")

linear_resid_vs_actual <- ggplot2::ggplot(linear_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "linear model: Residuals", x = "Actual", y = "Predicted")

linear_hist_residuals <- ggplot2::ggplot(linear_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "linear model: Histogram of residuals, each bar = 10 rows of data")

linear_qq <- ggplot2::ggplot(linear_df, aes(sample = as.numeric(y_hat_linear))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Linear model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Neuralnet data visualizations ####
neuralnet_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_neuralnet),
  residuals = c(test$y, validation$y) - y_hat_neuralnet
)

neuralnet_pred_vs_actual <- ggplot2::ggplot(neuralnet_df, mapping = aes(x = actual, y = as.numeric(y_hat_neuralnet))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Neuralnet model: Predicted vs actual", x = "Actual", y = "Predicted")

neuralnet_resid_vs_actual <- ggplot2::ggplot(neuralnet_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Neuralnet model: Residuals", x = "Actual", y = "Predicted")

neuralnet_hist_residuals <- ggplot2::ggplot(neuralnet_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Neuralnet model: Histogram of residuals, each bar = 10 rows of data")

neuralnet_qq <- ggplot2::ggplot(neuralnet_df, aes(sample = as.numeric(y_hat_neuralnet))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Neuralnet model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Partial Least Squares Regression visualizations ####
pls_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_pls),
  residuals = c(test$y, validation$y) - y_hat_pls
)

pls_pred_vs_actual <- ggplot2::ggplot(pls_df, mapping = aes(x = actual, y = as.numeric(y_hat_pls))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "pls model: Predicted vs actual", x = "Actual", y = "Predicted")

pls_resid_vs_actual <- ggplot2::ggplot(pls_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "pls model: Residuals", x = "Actual", y = "Predicted")

pls_hist_residuals <- ggplot2::ggplot(pls_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "pls model: Histogram of residuals, each bar = 10 rows of data")

pls_qq <- ggplot2::ggplot(pls_df, aes(sample = as.numeric(y_hat_pls))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "PLS model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Principal Components Regression visualizations ####
pcr_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_pcr),
  residuals = c(test$y, validation$y) - y_hat_pcr
)

pcr_pred_vs_actual <- ggplot2::ggplot(pcr_df, mapping = aes(x = actual, y = as.numeric(y_hat_pcr))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Principal Components Regression model: Predicted vs actual", x = "Actual", y = "Predicted")

pcr_resid_vs_actual <- ggplot2::ggplot(pcr_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Principal Components Regression model: Residuals", x = "Actual", y = "Predicted")

pcr_hist_residuals <- ggplot2::ggplot(pcr_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Principal Components Regression model: Histogram of residuals, each bar = 10 rows of data")

pcr_qq <- ggplot2::ggplot(pcr_df, aes(sample = as.numeric(y_hat_pcr))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "PCR model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Random Forest visualizations ####
rf_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_rf),
  residuals = c(test$y, validation$y) - y_hat_rf
)

rf_pred_vs_actual <- ggplot2::ggplot(rf_df, mapping = aes(x = actual, y = as.numeric(y_hat_rf))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Random Forest model: Predicted vs actual", x = "Actual", y = "Predicted")

rf_resid_vs_actual <- ggplot2::ggplot(rf_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Random Forest model: Residuals", x = "Actual", y = "Predicted")

rf_hist_residuals <- ggplot2::ggplot(rf_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Random Forest model: Histogram of residuals, each bar = 10 rows of data")

rf_qq <- ggplot2::ggplot(rf_df, aes(sample = as.numeric(y_hat_rf))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Random Forest model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ridge data visualizations ####
ridge_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_ridge),
  residuals = c(test$y, validation$y) - y_hat_ridge
)

ridge_pred_vs_actual <- ggplot2::ggplot(ridge_df, mapping = aes(x = actual, y = as.numeric(y_hat_ridge))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ridge model: Predicted vs actual", x = "Actual", y = "Predicted")

ridge_resid_vs_actual <- ggplot2::ggplot(ridge_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ridge model: Residuals", x = "Actual", y = "Predicted")

ridge_hist_residuals <- ggplot2::ggplot(ridge_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ridge model: Histogram of residuals, each bar = 10 rows of data")

ridge_qq <- ggplot2::ggplot(ridge_df, aes(sample = as.numeric(y_hat_ridge))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ridge model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Rpart visualizations ####
rpart_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_rpart),
  residuals = c(test$y, validation$y) - y_hat_rpart
)

rpart_pred_vs_actual <- ggplot2::ggplot(rpart_df, mapping = aes(x = actual, y = as.numeric(y_hat_rpart))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Random Forest model: Predicted vs actual", x = "Actual", y = "Predicted")

rpart_resid_vs_actual <- ggplot2::ggplot(rpart_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Random Forest model: Residuals", x = "Actual", y = "Predicted")

rpart_hist_residuals <- ggplot2::ggplot(rpart_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Random Forest model: Histogram of residuals, each bar = 10 rows of data")

rpart_qq <- ggplot2::ggplot(rpart_df, aes(sample = as.numeric(y_hat_rpart))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "RPart model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Support Vector Machines (SVM) visualizations ####
svm_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_svm),
  residuals = c(test$y, validation$y) - y_hat_svm
)

svm_pred_vs_actual <- ggplot2::ggplot(svm_df, mapping = aes(x = actual, y = as.numeric(y_hat_svm))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Random Forest model: Predicted vs actual", x = "Actual", y = "Predicted")

svm_resid_vs_actual <- ggplot2::ggplot(svm_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Random Forest model: Residuals", x = "Actual", y = "Predicted")

svm_hist_residuals <- ggplot2::ggplot(svm_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Random Forest model: Histogram of residuals, each bar = 10 rows of data")

svm_qq <- ggplot2::ggplot(svm_df, aes(sample = as.numeric(y_hat_svm))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Support Vector Machines model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")


#### Tree visualizations ####
tree_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_tree),
  residuals = c(test$y, validation$y) - y_hat_tree
)

tree_pred_vs_actual <- ggplot2::ggplot(tree_df, mapping = aes(x = actual, y = as.numeric(y_hat_tree))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Random Forest model: Predicted vs actual", x = "Actual", y = "Predicted")

tree_resid_vs_actual <- ggplot2::ggplot(tree_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Random Forest model: Residuals", x = "Actual", y = "Predicted")

tree_hist_residuals <- ggplot2::ggplot(tree_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Random Forest model: Histogram of residuals, each bar = 10 rows of data")

tree_qq <- ggplot2::ggplot(tree_df, aes(sample = as.numeric(y_hat_tree))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Tree model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### XGBoost visualizations ####
xgb_df <- data.frame(
  actual = c(test$y, validation$y), predicted = as.numeric(y_hat_xgb),
  residuals = c(test$y, validation$y) - y_hat_xgb
)

xgb_pred_vs_actual <- ggplot2::ggplot(xgb_df, mapping = aes(x = actual, y = as.numeric(y_hat_xgb))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "XGBoost model: Predicted vs actual", x = "Actual", y = "Predicted")

xgb_resid_vs_actual <- ggplot2::ggplot(xgb_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "XGBoost model: Residuals", x = "Actual", y = "Predicted")

xgb_hist_residuals <- ggplot2::ggplot(xgb_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "XGBoost model: Histogram of residuals, each bar = 10 rows of data")

xgb_qq <- ggplot2::ggplot(xgb_df, aes(sample = as.numeric(y_hat_xgb))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "XGBoost model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

### Ensemble Bagged Random Forest Visualizations ####
ensemble_bag_rf_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_bag_rf_test_predict_value, ensemble_bag_rf_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_bag_rf
)

ensemble_bag_rf_pred_vs_actual <- ggplot2::ggplot(ensemble_bag_rf_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Bagged Random Forest model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_bag_rf_resid_vs_actual <- ggplot2::ggplot(ensemble_bag_rf_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Bagged Random Forest model: Residuals", x = "Actual", y = "Predicted")

ensemble_bag_rf_hist_residuals <- ggplot2::ggplot(ensemble_bag_rf_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Bagged Random Forest model: Histogram of residuals, each bar = 10 rows of data")

ensemble_bag_rf_qq <- ggplot2::ggplot(ensemble_bag_rf_df, aes(sample = as.numeric(ensemble_y_hat_bag_rf))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Bagged Random Forest model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble Bagging Visualizations ####
ensemble_bagging_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_bagging
)

ensemble_bagging_pred_vs_actual <- ggplot2::ggplot(ensemble_bagging_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Bagging model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_bagging_resid_vs_actual <- ggplot2::ggplot(ensemble_bagging_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Bagging model: Residuals", x = "Actual", y = "Predicted")

ensemble_bagging_hist_residuals <- ggplot2::ggplot(ensemble_bagging_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Bagging model: Histogram of residuals, each bar = 10 rows of data")

ensemble_bagging_qq <- ggplot2::ggplot(ensemble_bagging_df, aes(sample = as.numeric(ensemble_y_hat_bagging))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Bagging model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble BayesGLM Visualizations ####
ensemble_bayesglm_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_bayesglm
)

ensemble_bayesglm_pred_vs_actual <- ggplot2::ggplot(ensemble_bayesglm_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Bayesglm model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_bayesglm_resid_vs_actual <- ggplot2::ggplot(ensemble_bayesglm_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Bayesglm model: Residuals", x = "Actual", y = "Predicted")

ensemble_bayesglm_hist_residuals <- ggplot2::ggplot(ensemble_bayesglm_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Bayesglm model: Histogram of residuals, each bar = 10 rows of data")

ensemble_bayesglm_qq <- ggplot2::ggplot(ensemble_bayesglm_df, aes(sample = as.numeric(ensemble_y_hat_bayesglm))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble BayesGLM model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble BayesRNN Visualizations ####
ensemble_bayesrnn_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_bayesrnn
)

ensemble_bayesrnn_pred_vs_actual <- ggplot2::ggplot(ensemble_bayesrnn_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Bayesrnn model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_bayesrnn_resid_vs_actual <- ggplot2::ggplot(ensemble_bayesrnn_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Bayesrnn model: Residuals", x = "Actual", y = "Predicted")

ensemble_bayesrnn_hist_residuals <- ggplot2::ggplot(ensemble_bayesrnn_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Bayesrnn model: Histogram of residuals, each bar = 10 rows of data")

ensemble_bayesrnn_qq <- ggplot2::ggplot(ensemble_bayesrnn_df, aes(sample = as.numeric(ensemble_y_hat_bayesrnn))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble BayesRNN model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble Boosted Random Forest Visualizations ####
ensemble_boost_rf_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_boost_rf_test_predict_value, ensemble_boost_rf_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_boost_rf
)

ensemble_boost_rf_pred_vs_actual <- ggplot2::ggplot(ensemble_boost_rf_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Boosted Random Forest model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_boost_rf_resid_vs_actual <- ggplot2::ggplot(ensemble_boost_rf_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Boosted Random Forest model: Residuals", x = "Actual", y = "Predicted")

ensemble_boost_rf_hist_residuals <- ggplot2::ggplot(ensemble_boost_rf_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Boosted Random Forest model: Histogram of residuals, each bar = 10 rows of data")

ensemble_boost_rf_qq <- ggplot2::ggplot(ensemble_boost_rf_df, aes(sample = as.numeric(ensemble_y_hat_boost_rf))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Boosted Random Forest model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

### Ensemble Cubist Visualizations ####
ensemble_cubist_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_cubist
)

ensemble_cubist_pred_vs_actual <- ggplot2::ggplot(ensemble_cubist_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Cubist model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_cubist_resid_vs_actual <- ggplot2::ggplot(ensemble_cubist_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Cubist model: Residuals", x = "Actual", y = "Predicted")

ensemble_cubist_hist_residuals <- ggplot2::ggplot(ensemble_cubist_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Cubist model: Histogram of residuals, each bar = 10 rows of data")

ensemble_cubist_qq <- ggplot2::ggplot(ensemble_cubist_df, aes(sample = as.numeric(ensemble_y_hat_cubist))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Cubist model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")


### Ensemble Earth Visualizations ####
ensemble_earth_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_earth
)

ensemble_earth_pred_vs_actual <- ggplot2::ggplot(ensemble_earth_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble earth model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_earth_resid_vs_actual <- ggplot2::ggplot(ensemble_earth_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble earth model: Residuals", x = "Actual", y = "Predicted")

ensemble_earth_hist_residuals <- ggplot2::ggplot(ensemble_earth_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble earth model: Histogram of residuals, each bar = 10 rows of data")

ensemble_earth_qq <- ggplot2::ggplot(ensemble_earth_df, aes(sample = as.numeric(ensemble_y_hat_earth))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble earth model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble Elastic data visualizations ####
ensemble_elastic_df <- data.frame(
  actual = c(ensemble_test$y, ensemble_validation$y), predicted = as.numeric(y_hat_ensemble_elastic),
  residuals = c(ensemble_test$y, ensemble_validation$y) - y_hat_ensemble_elastic
)

ensemble_elastic_pred_vs_actual <- ggplot2::ggplot(ensemble_elastic_df, mapping = aes(x = actual, y = as.numeric(y_hat_ensemble_elastic))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Elastic model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_elastic_resid_vs_actual <- ggplot2::ggplot(ensemble_elastic_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Elastic model: Residuals", x = "Actual", y = "Predicted")

ensemble_elastic_hist_residuals <- ggplot2::ggplot(ensemble_elastic_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Elastic model: Histogram of residuals, each bar = 10 rows of data")

ensemble_elastic_qq <- ggplot2::ggplot(ensemble_elastic_df, aes(sample = as.numeric(y_hat_ensemble_elastic))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Elastic model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")


#### Ensemble Gradient Boosted Visualizations ####
ensemble_gb_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_gb
)

ensemble_gb_pred_vs_actual <- ggplot2::ggplot(ensemble_gb_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Gradient Boosted model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_gb_resid_vs_actual <- ggplot2::ggplot(ensemble_gb_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Gradient Boosted model: Residuals", x = "Actual", y = "Predicted")

ensemble_gb_hist_residuals <- ggplot2::ggplot(ensemble_gb_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Gradient Boosted model: Histogram of residuals, each bar = 10 rows of data")

ensemble_gb_qq <- ggplot2::ggplot(ensemble_gb_df, aes(sample = as.numeric(ensemble_y_hat_gb))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Gradient Boosted model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble KNN data visualizations ####
ensemble_knn_df <- data.frame(
  actual = c(ensemble_test$y, ensemble_validation$y), predicted = as.numeric(ensemble_y_hat_knn),
  residuals = c(ensemble_test$y, ensemble_validation$y) - ensemble_y_hat_knn
)

ensemble_knn_pred_vs_actual <- ggplot2::ggplot(ensemble_knn_df, mapping = aes(x = actual, y = as.numeric(ensemble_y_hat_knn))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble KNN model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_knn_resid_vs_actual <- ggplot2::ggplot(ensemble_knn_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble KNN model: Residuals", x = "Actual", y = "Predicted")

ensemble_knn_hist_residuals <- ggplot2::ggplot(ensemble_knn_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble KNN model: Histogram of residuals, each bar = 10 rows of data")

ensemble_knn_qq <- ggplot2::ggplot(ensemble_knn_df, aes(sample = as.numeric(ensemble_y_hat_knn))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble KNN model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble Lasso data visualizations ####
ensemble_lasso_df <- data.frame(
  actual = c(ensemble_test$y, ensemble_validation$y), predicted = as.numeric(y_hat_ensemble_lasso),
  residuals = c(ensemble_test$y, ensemble_validation$y) - y_hat_ensemble_lasso
)

ensemble_lasso_pred_vs_actual <- ggplot2::ggplot(ensemble_lasso_df, mapping = aes(x = actual, y = as.numeric(y_hat_ensemble_lasso))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble lasso model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_lasso_resid_vs_actual <- ggplot2::ggplot(ensemble_lasso_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble lasso model: Residuals", x = "Actual", y = "Predicted")

ensemble_lasso_hist_residuals <- ggplot2::ggplot(ensemble_lasso_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble lasso model: Histogram of residuals, each bar = 10 rows of data")

ensemble_lasso_qq <- ggplot2::ggplot(ensemble_lasso_df, aes(sample = as.numeric(y_hat_ensemble_lasso))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Lasso model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")


#### Ensemble Linear Visualizations ####
ensemble_linear_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_linear
)

ensemble_linear_pred_vs_actual <- ggplot2::ggplot(ensemble_linear_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Linear Model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_linear_resid_vs_actual <- ggplot2::ggplot(ensemble_linear_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Linear Model: Residuals", x = "Actual", y = "Residuals")

ensemble_linear_hist_residuals <- ggplot2::ggplot(ensemble_linear_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Linear Model: Histogram of residuals, each bar = 10 rows of data")

ensemble_linear_qq <- ggplot2::ggplot(ensemble_linear_df, aes(sample = as.numeric(ensemble_y_hat_linear))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Linear model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")


#### Ensemble Random Forest Visualizations ####
ensemble_rf_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_rf_test_predict_value, ensemble_rf_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_rf
)

ensemble_rf_pred_vs_actual <- ggplot2::ggplot(ensemble_rf_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Random Forest Regression model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_rf_resid_vs_actual <- ggplot2::ggplot(ensemble_rf_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Random Forest Regression model: Residuals", x = "Actual", y = "Predicted")

ensemble_rf_hist_residuals <- ggplot2::ggplot(ensemble_rf_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Random Forest Regression model: Histogram of residuals, each bar = 10 rows of data")

ensemble_rf_qq <- ggplot2::ggplot(ensemble_rf_df, aes(sample = as.numeric(ensemble_y_hat_rf))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Random Forest model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble Ridge data visualizations ####
ensemble_ridge_df <- data.frame(
  actual = c(ensemble_test$y, ensemble_validation$y), predicted = as.numeric(y_hat_ensemble_ridge),
  residuals = c(ensemble_test$y, ensemble_validation$y) - y_hat_ensemble_ridge
)

ensemble_ridge_pred_vs_actual <- ggplot2::ggplot(ensemble_ridge_df, mapping = aes(x = actual, y = as.numeric(y_hat_ensemble_ridge))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble ridge model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_ridge_resid_vs_actual <- ggplot2::ggplot(ensemble_ridge_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble ridge model: Residuals", x = "Actual", y = "Predicted")

ensemble_ridge_hist_residuals <- ggplot2::ggplot(ensemble_ridge_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble ridge model: Histogram of residuals, each bar = 10 rows of data")

ensemble_ridge_qq <- ggplot2::ggplot(ensemble_ridge_df, aes(sample = as.numeric(y_hat_ensemble_ridge))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Ridge model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble Rpart Visualizations ####
ensemble_rpart_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_rpart
)

ensemble_rpart_pred_vs_actual <- ggplot2::ggplot(ensemble_rpart_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Rpart Regression model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_rpart_resid_vs_actual <- ggplot2::ggplot(ensemble_rpart_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Rpart Regression model: Residuals", x = "Actual", y = "Predicted")

ensemble_rpart_hist_residuals <- ggplot2::ggplot(ensemble_rpart_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Rpart Regression model: Histogram of residuals, each bar = 10 rows of data")

ensemble_rpart_qq <- ggplot2::ggplot(ensemble_rpart_df, aes(sample = as.numeric(ensemble_y_hat_rpart))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble RPart model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble Support Vector Machines Visualizations ####
ensemble_svm_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_svm
)

ensemble_svm_pred_vs_actual <- ggplot2::ggplot(ensemble_svm_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Support Vector Machines model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_svm_resid_vs_actual <- ggplot2::ggplot(ensemble_svm_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Support Vector Machines Regression model: Residuals", x = "Actual", y = "Predicted")

ensemble_svm_hist_residuals <- ggplot2::ggplot(ensemble_svm_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Support Vector Machines Regression model: Histogram of residuals, each bar = 10 rows of data")

ensemble_svm_qq <- ggplot2::ggplot(ensemble_svm_df, aes(sample = as.numeric(ensemble_y_hat_svm))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Support Vector Machines model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble Tree Visualizations ####
ensemble_tree_df <- data.frame(
  actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble),
  predicted = as.numeric(c(ensemble_tree_test_predict_value, ensemble_tree_validation_predict_value)),
  residuals = as.numeric(c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)) - ensemble_y_hat_tree
)

ensemble_tree_pred_vs_actual <- ggplot2::ggplot(ensemble_tree_df, mapping = aes(x = actual, y = as.numeric(predicted))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Trees Regression model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_tree_resid_vs_actual <- ggplot2::ggplot(ensemble_tree_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Trees Regression model: Residuals", x = "Actual", y = "Predicted")

ensemble_tree_hist_residuals <- ggplot2::ggplot(ensemble_tree_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Trees Regression model: Histogram of residuals, each bar = 10 rows of data")

ensemble_tree_qq <- ggplot2::ggplot(ensemble_tree_df, aes(sample = as.numeric(ensemble_y_hat_tree))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Tree model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

#### Ensemble XGBoost data visualizations ####
ensemble_xgb_df <- data.frame(
  actual = c(ensemble_test$y, ensemble_validation$y), predicted = as.numeric(ensemble_y_hat_xgb),
  residuals = c(ensemble_test$y, ensemble_validation$y) - ensemble_y_hat_xgb
)

ensemble_xgb_pred_vs_actual <- ggplot2::ggplot(ensemble_xgb_df, mapping = aes(x = actual, y = as.numeric(ensemble_y_hat_xgb))) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble xgb model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_xgb_resid_vs_actual <- ggplot2::ggplot(ensemble_xgb_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble xgb model: Residuals", x = "Actual", y = "Predicted")

ensemble_xgb_hist_residuals <- ggplot2::ggplot(ensemble_xgb_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df) / 10)) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble xgb model: Histogram of residuals, each bar = 10 rows of data")

ensemble_xgb_qq <- ggplot2::ggplot(ensemble_xgb_df, aes(sample = as.numeric(ensemble_y_hat_xgb))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble XGBoost model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")


accuracy_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Mean_holdout_RMSE), y = Mean_holdout_RMSE)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Holdout RMSE Mean", title = "Model accuracy by RMSE, lower is better, 1 std deviation error bars") +
  ggplot2::geom_text(aes(label = Mean_holdout_RMSE), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, 1.5*max(summary_results$Mean_holdout_RMSE)) +
  ggplot2::geom_errorbar(aes(x=Model, ymin=Mean_holdout_RMSE-Std_Deviation_of_holdout_RMSE, ymax = Mean_holdout_RMSE+Std_Deviation_of_holdout_RMSE))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

k_s_test_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, KS_Test_P_Value_mean, decreasing = TRUE), y = KS_Test_P_Value_mean)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "P-Value", title = "Kolmogorov-Smirnov test, p-value, 1 std deviation error bars, above your p-value is better") +
  ggplot2::geom_text(aes(label = KS_Test_P_Value_mean), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::scale_y_continuous(breaks = c(0.05, 0.10)) +
  ggplot2::geom_hline(yintercept = c(0.05, 0.10), linetype='dashed', color=c('blue', 'blue')) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = KS_Test_P_Value_mean - KS_Test_P_Value_std_dev, ymax = KS_Test_P_Value_mean + KS_Test_P_Value_std_dev))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("k_s_test_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("k_s_test_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("k_s_test_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("k_s_test_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("k_s_test_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("k_s_test_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
holdout_vs_train_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Mean_holdout_RMSE), y = Mean_holdout_RMSE)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Holdout RMSE / train RMSE mean", title = "Holdout RMSE / train RMSE, closer to 1 is better, 1 std deviation error bars") +
  ggplot2::geom_text(aes(label = Mean_holdout_RMSE), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, max(max(summary_results$Mean_holdout_RMSE[!is.infinite(summary_results$Mean_holdout_RMSE)])) +2) +
  ggplot2::geom_errorbar(aes(x=Model, ymin=Mean_holdout_RMSE-Std_Deviation_of_holdout_RMSE, ymax = Mean_holdout_RMSE+Std_Deviation_of_holdout_RMSE))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("Holdout_vs_train_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("Holdout_vs_train_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("Holdout_vs_train_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("Holdout_vs_train_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("Holdout_vs_train_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("Holdout_vs_train_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
duration_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Duration), y = Duration)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Duration", title = "Duration, shorter is better, 1 std deviation error bars") +
  ggplot2::geom_text(aes(label = Duration), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, 1.5*max(summary_results$Duration)) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Duration - Duration_sd, ymax = Duration + Duration_sd))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("duration_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("duration_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("duration_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("duration_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("duration_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("duration_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
bias_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Bias), y = Bias)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Holdout bias", title = "Mean bias, closer to zero is better") +
  ggplot2::geom_text(aes(label = Bias), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::ylim(min(summary_results$Bias), 1.5*max(summary_results$Bias))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("bias_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("bias_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("bias_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("bias_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("bias_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("bias_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
MAE_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Mean_MAE), y = Mean_MAE)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Holdout MAE", title = "Mean MAE (Mean Absolute Error), closer to zero is better, 1 std deviation error bars") +
  ggplot2::geom_text(aes(label = Mean_MAE), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::ylim(min(summary_results$Mean_MAE), 1.5*max(summary_results$Mean_MAE)) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Mean_MAE - Mean_MAE_sd, ymax = Mean_MAE + Mean_MAE_sd))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("MAE_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("MAE_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("MAE_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("MAE_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("MAE_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("MAE_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
MSE_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Mean_MSE), y = Mean_MSE)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Holdout MSE", title = "Mean MSE (Mean Squared Error), lower is better, 1 std deviation error bars") +
  ggplot2::geom_text(aes(label = Mean_MSE), vjust = -0.5, hjust = -0.5, angle = 90)  +
  ggplot2::ylim(min(summary_results$Mean_MSE), 1.5*max(summary_results$Mean_MSE)) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Mean_MSE - Mean_MSE_sd, ymax = Mean_MSE + Mean_MSE_sd))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("MSE_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("MSE_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("MSE_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("MSE_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("MSE_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("MSE_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
SSE_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Mean_SSE), y = Mean_SSE)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Holdout SSE", title = "Mean SSE (Sum of Squared Error), lower is better, 1 std deviation error bars") +
  ggplot2::geom_text(aes(label = Mean_SSE), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::ylim(min(summary_results$Mean_SSE), 1.5*max(summary_results$Mean_SSE)) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Mean_SSE - Mean_SSE_sd, ymax = Mean_SSE + Mean_SSE_sd))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("SSE_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("SSE_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("SSE_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("SSE_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("SSE_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("SSE_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

data_visualizations <- summary_results[2, 1]

if (data_visualizations[1] == "Bagged Random Forest") {
  gridExtra::grid.arrange(bag_rf_pred_vs_actual, bag_rf_resid_vs_actual, bag_rf_hist_residuals, bag_rf_qq, ncol = 2)
  gridExtra::grid.arrange(bag_rf_pred_vs_actual)
  gridExtra::grid.arrange(bag_rf_resid_vs_actual)
  gridExtra::grid.arrange(bag_rf_hist_residuals)
  gridExtra::grid.arrange(bag_rf_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagged Random Forest" && device == "eps"){
    message(bag_rf_pred_vs_actual); ggplot2::ggsave("bag_rf_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_resid_vs_actual); ggplot2::ggsave("bag_rf_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_hist_residuals); ggplot2::ggsave("bag_rf_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_qq); ggplot2::ggsave("bag_rf_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagged Random Forest" && device == "pdf"){
    message(bag_rf_pred_vs_actual); ggplot2::ggsave("bag_rf_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_resid_vs_actual); ggplot2::ggsave("bag_rf_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_hist_residuals); ggplot2::ggsave("bag_rf_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_qq); ggplot2::ggsave("bag_rf_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagged Random Forest" && device == "jpeg"){
    message(bag_rf_pred_vs_actual); ggplot2::ggsave("bag_rf_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_resid_vs_actual); ggplot2::ggsave("bag_rf_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_hist_residuals); ggplot2::ggsave("bag_rf_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_qq); ggplot2::ggsave("bag_rf_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagged Random Forest" && device == "tiff"){
    message(bag_rf_pred_vs_actual); ggplot2::ggsave("bag_rf_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_resid_vs_actual); ggplot2::ggsave("bag_rf_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_hist_residuals); ggplot2::ggsave("bag_rf_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_qq); ggplot2::ggsave("bag_rf_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagged Random Forest" && device == "png"){
    message(bag_rf_pred_vs_actual); ggplot2::ggsave("bag_rf_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_resid_vs_actual); ggplot2::ggsave("bag_rf_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_hist_residuals); ggplot2::ggsave("bag_rf_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_qq); ggplot2::ggsave("bag_rf_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagged Random Forest" && device == "svg"){
    message(bag_rf_pred_vs_actual); ggplot2::ggsave("bag_rf_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_resid_vs_actual); ggplot2::ggsave("bag_rf_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_hist_residuals); ggplot2::ggsave("bag_rf_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bag_rf_qq); ggplot2::ggsave("bag_rf_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Bagging") {
  grid.arrange(bagging_pred_vs_actual, bagging_resid_vs_actual, bagging_hist_residuals, bagging_qq, ncol = 2)
  gridExtra::grid.arrange(bagging_pred_vs_actual)
  gridExtra::grid.arrange(bagging_resid_vs_actual)
  gridExtra::grid.arrange(bagging_hist_residuals)
  gridExtra::grid.arrange(bagging_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagging" && device == "eps"){
    message(bagging_pred_vs_actual); ggplot2::ggsave("bagging_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_resid_vs_actual); ggplot2::ggsave("bagging_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_hist_residuals); ggplot2::ggsave("bagging_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_qq); ggplot2::ggsave("bagging_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagging" && device == "pdf"){
    message(bagging_pred_vs_actual); ggplot2::ggsave("bagging_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_resid_vs_actual); ggplot2::ggsave("bagging_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_hist_residuals); ggplot2::ggsave("bagging_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_qq); ggplot2::ggsave("bagging_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagging" && device == "jpeg"){
    message(bagging_pred_vs_actual); ggplot2::ggsave("bagging_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_resid_vs_actual); ggplot2::ggsave("bagging_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_hist_residuals); ggplot2::ggsave("bagging_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_qq); ggplot2::ggsave("bagging_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagging" && device == "tiff"){
    message(bagging_pred_vs_actual); ggplot2::ggsave("bagging_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_resid_vs_actual); ggplot2::ggsave("bagging_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_hist_residuals); ggplot2::ggsave("bagging_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_qq); ggplot2::ggsave("bagging_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagging" && device == "png"){
    message(bagging_pred_vs_actual); ggplot2::ggsave("bagging_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_resid_vs_actual); ggplot2::ggsave("bagging_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_hist_residuals); ggplot2::ggsave("bagging_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_qq); ggplot2::ggsave("bagging_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Bagging" && device == "svg"){
    message(bagging_pred_vs_actual); ggplot2::ggsave("bagging_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_resid_vs_actual); ggplot2::ggsave("bagging_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_hist_residuals); ggplot2::ggsave("bagging_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bagging_qq); ggplot2::ggsave("bagging_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "BayesGLM") {
  grid.arrange(bayesglm_pred_vs_actual, bayesglm_resid_vs_actual, bayesglm_hist_residuals, bayesglm_qq, ncol = 2)
  gridExtra::grid.arrange(bayesglm_pred_vs_actual)
  gridExtra::grid.arrange(bayesglm_resid_vs_actual)
  gridExtra::grid.arrange(bayesglm_hist_residuals)
  gridExtra::grid.arrange(bayesglm_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesGLM" && device == "eps"){
    message(bayesglm_pred_vs_actual); ggplot2::ggsave("Bayesglm_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_resid_vs_actual); ggplot2::ggsave("Bayesglm_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_hist_residuals); ggplot2::ggsave("Bayesglm_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_qq); ggplot2::ggsave("Bayesglm_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesGLM" && device == "pdf"){
    message(bayesglm_pred_vs_actual); ggplot2::ggsave("Bayesglm_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_resid_vs_actual); ggplot2::ggsave("Bayesglm_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_hist_residuals); ggplot2::ggsave("Bayesglm_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_qq); ggplot2::ggsave("Bayesglm_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesGLM" && device == "jpeg"){
    message(bayesglm_pred_vs_actual); ggplot2::ggsave("Bayesglm_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_resid_vs_actual); ggplot2::ggsave("Bayesglm_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_hist_residuals); ggplot2::ggsave("Bayesglm_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_qq); ggplot2::ggsave("Bayesglm_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesGLM" && device == "tiff"){
    message(bayesglm_pred_vs_actual); ggplot2::ggsave("Bayesglm_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_resid_vs_actual); ggplot2::ggsave("Bayesglm_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_hist_residuals); ggplot2::ggsave("Bayesglm_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_qq); ggplot2::ggsave("Bayesglm_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesGLM" && device == "png"){
    message(bayesglm_pred_vs_actual); ggplot2::ggsave("Bayesglm_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_resid_vs_actual); ggplot2::ggsave("Bayesglm_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_hist_residuals); ggplot2::ggsave("Bayesglm_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_qq); ggplot2::ggsave("Bayesglm_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesGLM" && device == "svg"){
    message(bayesglm_pred_vs_actual); ggplot2::ggsave("Bayesglm_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_resid_vs_actual); ggplot2::ggsave("Bayesglm_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_hist_residuals); ggplot2::ggsave("Bayesglm_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesglm_qq); ggplot2::ggsave("Bayesglm_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }}

if (data_visualizations[1] == "BayesRNN") {
  grid.arrange(bayesrnn_pred_vs_actual, bayesrnn_resid_vs_actual, bayesrnn_hist_residuals, bayesrnn_qq, ncol = 2)
  gridExtra::grid.arrange(bayesrnn_pred_vs_actual)
  gridExtra::grid.arrange(bayesrnn_resid_vs_actual)
  gridExtra::grid.arrange(bayesrnn_hist_residuals)
  gridExtra::grid.arrange(bayesrnn_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesRNN" && device == "eps"){
    message(bayesrnn_pred_vs_actual); ggplot2::ggsave("bayesrnn_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_resid_vs_actual); ggplot2::ggsave("bayesrnn_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_hist_residuals); ggplot2::ggsave("bayesrnn_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_qq); ggplot2::ggsave("bayesrnn_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesRNN" && device == "pdf"){
    message(bayesrnn_pred_vs_actual); ggplot2::ggsave("bayesrnn_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_resid_vs_actual); ggplot2::ggsave("bayesrnn_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_hist_residuals); ggplot2::ggsave("bayesrnn_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_qq); ggplot2::ggsave("bayesrnn_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesRNN" && device == "jpeg"){
    message(bayesrnn_pred_vs_actual); ggplot2::ggsave("bayesrnn_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_resid_vs_actual); ggplot2::ggsave("bayesrnn_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_hist_residuals); ggplot2::ggsave("bayesrnn_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_qq); ggplot2::ggsave("bayesrnn_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesRNN" && device == "tiff"){
    message(bayesrnn_pred_vs_actual); ggplot2::ggsave("bayesrnn_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_resid_vs_actual); ggplot2::ggsave("bayesrnn_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_hist_residuals); ggplot2::ggsave("bayesrnn_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_qq); ggplot2::ggsave("bayesrnn_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesRNN" && device == "png"){
    message(bayesrnn_pred_vs_actual); ggplot2::ggsave("bayesrnn_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_resid_vs_actual); ggplot2::ggsave("bayesrnn_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_hist_residuals); ggplot2::ggsave("bayesrnn_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_qq); ggplot2::ggsave("bayesrnn_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "BayesRNN" && device == "svg"){
    message(bayesrnn_pred_vs_actual); ggplot2::ggsave("bayesrnn_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_resid_vs_actual); ggplot2::ggsave("bayesrnn_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_hist_residuals); ggplot2::ggsave("bayesrnn_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(bayesrnn_qq); ggplot2::ggsave("bayesrnn_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Boost_RF") {
  grid.arrange(boost_rf_pred_vs_actual, boost_rf_resid_vs_actual, boost_rf_hist_residuals, boost_rf_qq, ncol = 2)
  gridExtra::grid.arrange(boost_rf_pred_vs_actual)
  gridExtra::grid.arrange(boost_rf_resid_vs_actual)
  gridExtra::grid.arrange(boost_rf_hist_residuals)
  gridExtra::grid.arrange(boost_rf_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Boost_RF" && device == "eps"){
    message(boost_rf_pred_vs_actual); ggplot2::ggsave("boost_rf_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_resid_vs_actual); ggplot2::ggsave("boost_rf_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_hist_residuals); ggplot2::ggsave("boost_rf_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_qq); ggplot2::ggsave("boost_rf_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Boost_RF" && device == "pdf"){
    message(boost_rf_pred_vs_actual); ggplot2::ggsave("boost_rf_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_resid_vs_actual); ggplot2::ggsave("boost_rf_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_hist_residuals); ggplot2::ggsave("boost_rf_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_qq); ggplot2::ggsave("boost_rf_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Boost_RF" && device == "jpeg"){
    message(boost_rf_pred_vs_actual); ggplot2::ggsave("boost_rf_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_resid_vs_actual); ggplot2::ggsave("boost_rf_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_hist_residuals); ggplot2::ggsave("boost_rf_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_qq); ggplot2::ggsave("boost_rf_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Boost_RF" && device == "tiff"){
    message(boost_rf_pred_vs_actual); ggplot2::ggsave("boost_rf_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_resid_vs_actual); ggplot2::ggsave("boost_rf_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_hist_residuals); ggplot2::ggsave("boost_rf_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_qq); ggplot2::ggsave("boost_rf_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Boost_RF" && device == "png"){
    message(boost_rf_pred_vs_actual); ggplot2::ggsave("boost_rf_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_resid_vs_actual); ggplot2::ggsave("boost_rf_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_hist_residuals); ggplot2::ggsave("boost_rf_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_qq); ggplot2::ggsave("boost_rf_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Boost_RF" && device == "svg"){
    message(boost_rf_pred_vs_actual); ggplot2::ggsave("boost_rf_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_resid_vs_actual); ggplot2::ggsave("boost_rf_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_hist_residuals); ggplot2::ggsave("boost_rf_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(boost_rf_qq); ggplot2::ggsave("boost_rf_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Cubist") {
  grid.arrange(cubist_pred_vs_actual, cubist_resid_vs_actual, cubist_hist_residuals, cubist_qq, ncol = 2)
  gridExtra::grid.arrange(cubist_pred_vs_actual)
  gridExtra::grid.arrange(cubist_resid_vs_actual)
  gridExtra::grid.arrange(cubist_hist_residuals)
  gridExtra::grid.arrange(cubist_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Cubist" && device == "eps"){
    message(cubist_pred_vs_actual); ggplot2::ggsave("cubist_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_resid_vs_actual); ggplot2::ggsave("cubist_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_hist_residuals); ggplot2::ggsave("cubist_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_qq); ggplot2::ggsave("cubist_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Cubist" && device == "pdf"){
    message(cubist_pred_vs_actual); ggplot2::ggsave("cubist_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_resid_vs_actual); ggplot2::ggsave("cubist_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_hist_residuals); ggplot2::ggsave("cubist_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_qq); ggplot2::ggsave("cubist_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Cubist" && device == "jpeg"){
    message(cubist_pred_vs_actual); ggplot2::ggsave("cubist_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_resid_vs_actual); ggplot2::ggsave("cubist_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_hist_residuals); ggplot2::ggsave("cubist_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_qq); ggplot2::ggsave("cubist_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Cubist" && device == "tiff"){
    message(cubist_pred_vs_actual); ggplot2::ggsave("cubist_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_resid_vs_actual); ggplot2::ggsave("cubist_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_hist_residuals); ggplot2::ggsave("cubist_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_qq); ggplot2::ggsave("cubist_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Cubist" && device == "png"){
    message(cubist_pred_vs_actual); ggplot2::ggsave("cubist_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_resid_vs_actual); ggplot2::ggsave("cubist_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_hist_residuals); ggplot2::ggsave("cubist_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_qq); ggplot2::ggsave("cubist_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Cubist" && device == "svg"){
    message(cubist_pred_vs_actual); ggplot2::ggsave("cubist_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_resid_vs_actual); ggplot2::ggsave("cubist_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_hist_residuals); ggplot2::ggsave("cubist_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(cubist_qq); ggplot2::ggsave("cubist_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Earth") {
  grid.arrange(earth_pred_vs_actual, earth_resid_vs_actual, earth_hist_residuals, earth_qq, ncol = 2)
  gridExtra::grid.arrange(earth_pred_vs_actual)
  gridExtra::grid.arrange(earth_resid_vs_actual)
  gridExtra::grid.arrange(earth_hist_residuals)
  gridExtra::grid.arrange(earth_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Earth" && device == "eps"){
    message(earth_pred_vs_actual); ggplot2::ggsave("earth_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_resid_vs_actual); ggplot2::ggsave("earth_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_hist_residuals); ggplot2::ggsave("earth_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_qq); ggplot2::ggsave("earth_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Earth" && device == "pdf"){
    message(earth_pred_vs_actual); ggplot2::ggsave("earth_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_resid_vs_actual); ggplot2::ggsave("earth_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_hist_residuals); ggplot2::ggsave("earth_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_qq); ggplot2::ggsave("earth_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Earth" && device == "jpeg"){
    message(earth_pred_vs_actual); ggplot2::ggsave("earth_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_resid_vs_actual); ggplot2::ggsave("earth_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_hist_residuals); ggplot2::ggsave("earth_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_qq); ggplot2::ggsave("earth_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Earth" && device == "tiff"){
    message(earth_pred_vs_actual); ggplot2::ggsave("earth_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_resid_vs_actual); ggplot2::ggsave("earth_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_hist_residuals); ggplot2::ggsave("earth_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_qq); ggplot2::ggsave("earth_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Earth" && device == "png"){
    message(earth_pred_vs_actual); ggplot2::ggsave("earth_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_resid_vs_actual); ggplot2::ggsave("earth_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_hist_residuals); ggplot2::ggsave("earth_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_qq); ggplot2::ggsave("earth_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Earth" && device == "svg"){
    message(earth_pred_vs_actual); ggplot2::ggsave("earth_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_resid_vs_actual); ggplot2::ggsave("earth_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_hist_residuals); ggplot2::ggsave("earth_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(earth_qq); ggplot2::ggsave("earth_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Elastic") {
  grid.arrange(elastic_pred_vs_actual, elastic_resid_vs_actual, elastic_hist_residuals, elastic_qq, ncol = 2)
  gridExtra::grid.arrange(elastic_pred_vs_actual)
  gridExtra::grid.arrange(elastic_resid_vs_actual)
  gridExtra::grid.arrange(elastic_hist_residuals)
  gridExtra::grid.arrange(elastic_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Elastic" && device == "eps"){
    message(elastic_pred_vs_actual); ggplot2::ggsave("elastic_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_resid_vs_actual); ggplot2::ggsave("elastic_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_hist_residuals); ggplot2::ggsave("elastic_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_qq); ggplot2::ggsave("elastic_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Elastic" && device == "pdf"){
    message(elastic_pred_vs_actual); ggplot2::ggsave("elastic_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_resid_vs_actual); ggplot2::ggsave("elastic_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_hist_residuals); ggplot2::ggsave("elastic_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_qq); ggplot2::ggsave("elastic_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Elastic" && device == "jpeg"){
    message(elastic_pred_vs_actual); ggplot2::ggsave("elastic_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_resid_vs_actual); ggplot2::ggsave("elastic_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_hist_residuals); ggplot2::ggsave("elastic_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_qq); ggplot2::ggsave("elastic_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Elastic" && device == "tiff"){
    message(elastic_pred_vs_actual); ggplot2::ggsave("elastic_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_resid_vs_actual); ggplot2::ggsave("elastic_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_hist_residuals); ggplot2::ggsave("elastic_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_qq); ggplot2::ggsave("elastic_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Elastic" && device == "png"){
    message(elastic_pred_vs_actual); ggplot2::ggsave("elastic_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_resid_vs_actual); ggplot2::ggsave("elastic_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_hist_residuals); ggplot2::ggsave("elastic_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_qq); ggplot2::ggsave("elastic_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Elastic" && device == "svg"){
    message(elastic_pred_vs_actual); ggplot2::ggsave("elastic_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_resid_vs_actual); ggplot2::ggsave("elastic_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_hist_residuals); ggplot2::ggsave("elastic_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(elastic_qq); ggplot2::ggsave("elastic_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "GAM") {
  grid.arrange(gam_pred_vs_actual, gam_resid_vs_actual, gam_hist_residuals, gam_qq, ncol = 2)
  gridExtra::grid.arrange(gam_pred_vs_actual)
  gridExtra::grid.arrange(gam_resid_vs_actual)
  gridExtra::grid.arrange(gam_hist_residuals)
  gridExtra::grid.arrange(gam_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "GAM" && device == "eps"){
    message(gam_pred_vs_actual); ggplot2::ggsave("gam_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_resid_vs_actual); ggplot2::ggsave("gam_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_hist_residuals); ggplot2::ggsave("gam_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_qq); ggplot2::ggsave("gam_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "GAM" && device == "pdf"){
    message(gam_pred_vs_actual); ggplot2::ggsave("gam_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_resid_vs_actual); ggplot2::ggsave("gam_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_hist_residuals); ggplot2::ggsave("gam_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_qq); ggplot2::ggsave("gam_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "GAM" && device == "jpeg"){
    message(gam_pred_vs_actual); ggplot2::ggsave("gam_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_resid_vs_actual); ggplot2::ggsave("gam_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_hist_residuals); ggplot2::ggsave("gam_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_qq); ggplot2::ggsave("gam_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "GAM" && device == "tiff"){
    message(gam_pred_vs_actual); ggplot2::ggsave("gam_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_resid_vs_actual); ggplot2::ggsave("gam_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_hist_residuals); ggplot2::ggsave("gam_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_qq); ggplot2::ggsave("gam_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "GAM" && device == "png"){
    message(gam_pred_vs_actual); ggplot2::ggsave("gam_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_resid_vs_actual); ggplot2::ggsave("gam_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_hist_residuals); ggplot2::ggsave("gam_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_qq); ggplot2::ggsave("gam_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "GAM" && device == "svg"){
    message(gam_pred_vs_actual); ggplot2::ggsave("gam_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_resid_vs_actual); ggplot2::ggsave("gam_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_hist_residuals); ggplot2::ggsave("gam_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gam_qq); ggplot2::ggsave("gam_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Gradient Boosted") {
  grid.arrange(gb_pred_vs_actual, gb_resid_vs_actual, gb_hist_residuals, gb_qq, ncol = 2)
  gridExtra::grid.arrange(gb_pred_vs_actual)
  gridExtra::grid.arrange(gb_resid_vs_actual)
  gridExtra::grid.arrange(gb_hist_residuals)
  gridExtra::grid.arrange(gb_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Gardient Boosted" && device == "eps"){
    message(gb_pred_vs_actual); ggplot2::ggsave("gb_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_resid_vs_actual); ggplot2::ggsave("gb_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_hist_residuals); ggplot2::ggsave("gb_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_qq); ggplot2::ggsave("gb_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Gardient Boosted" && device == "pdf"){
    message(gb_pred_vs_actual); ggplot2::ggsave("gb_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_resid_vs_actual); ggplot2::ggsave("gb_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_hist_residuals); ggplot2::ggsave("gb_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_qq); ggplot2::ggsave("gb_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Gardient Boosted" && device == "jpeg"){
    message(gb_pred_vs_actual); ggplot2::ggsave("gb_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_resid_vs_actual); ggplot2::ggsave("gb_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_hist_residuals); ggplot2::ggsave("gb_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_qq); ggplot2::ggsave("gb_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Gardient Boosted" && device == "tiff"){
    message(gb_pred_vs_actual); ggplot2::ggsave("gb_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_resid_vs_actual); ggplot2::ggsave("gb_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_hist_residuals); ggplot2::ggsave("gb_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_qq); ggplot2::ggsave("gb_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Gardient Boosted" && device == "png"){
    message(gb_pred_vs_actual); ggplot2::ggsave("gb_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_resid_vs_actual); ggplot2::ggsave("gb_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_hist_residuals); ggplot2::ggsave("gb_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_qq); ggplot2::ggsave("gb_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Gardient Boosted" && device == "svg"){
    message(gb_pred_vs_actual); ggplot2::ggsave("gb_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_resid_vs_actual); ggplot2::ggsave("gb_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_hist_residuals); ggplot2::ggsave("gb_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(gb_qq); ggplot2::ggsave("gb_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "KNN") {
  grid.arrange(knn_pred_vs_actual, knn_resid_vs_actual, knn_hist_residuals, knn_qq, ncol = 2)
  gridExtra::grid.arrange(knn_pred_vs_actual)
  gridExtra::grid.arrange(knn_resid_vs_actual)
  gridExtra::grid.arrange(knn_hist_residuals)
  gridExtra::grid.arrange(knn_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "KNN" && device == "eps"){
    message(knn_pred_vs_actual); ggplot2::ggsave("knn_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_resid_vs_actual); ggplot2::ggsave("knn_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_hist_residuals); ggplot2::ggsave("knn_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_qq); ggplot2::ggsave("knn_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "KNN" && device == "pdf"){
    message(knn_pred_vs_actual); ggplot2::ggsave("knn_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_resid_vs_actual); ggplot2::ggsave("knn_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_hist_residuals); ggplot2::ggsave("knn_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_qq); ggplot2::ggsave("knn_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "KNN" && device == "jpeg"){
    message(knn_pred_vs_actual); ggplot2::ggsave("knn_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_resid_vs_actual); ggplot2::ggsave("knn_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_hist_residuals); ggplot2::ggsave("knn_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_qq); ggplot2::ggsave("knn_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "KNN" && device == "tiff"){
    message(knn_pred_vs_actual); ggplot2::ggsave("knn_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_resid_vs_actual); ggplot2::ggsave("knn_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_hist_residuals); ggplot2::ggsave("knn_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_qq); ggplot2::ggsave("knn_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "KNN" && device == "png"){
    message(knn_pred_vs_actual); ggplot2::ggsave("knn_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_resid_vs_actual); ggplot2::ggsave("knn_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_hist_residuals); ggplot2::ggsave("knn_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_qq); ggplot2::ggsave("knn_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "KNN" && device == "svg"){
    message(knn_pred_vs_actual); ggplot2::ggsave("knn_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_resid_vs_actual); ggplot2::ggsave("knn_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_hist_residuals); ggplot2::ggsave("knn_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(knn_qq); ggplot2::ggsave("knn_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Lasso") {
  grid.arrange(lasso_pred_vs_actual, lasso_resid_vs_actual, lasso_hist_residuals, lasso_qq, ncol = 2)
  gridExtra::grid.arrange(lasso_pred_vs_actual)
  gridExtra::grid.arrange(lasso_resid_vs_actual)
  gridExtra::grid.arrange(lasso_hist_residuals)
  gridExtra::grid.arrange(lasso_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Lasso" && device == "eps"){
    message(lasso_pred_vs_actual); ggplot2::ggsave("lasso_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_resid_vs_actual); ggplot2::ggsave("lasso_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_hist_residuals); ggplot2::ggsave("lasso_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_qq); ggplot2::ggsave("lasso_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Lasso" && device == "pdf"){
    message(lasso_pred_vs_actual); ggplot2::ggsave("lasso_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_resid_vs_actual); ggplot2::ggsave("lasso_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_hist_residuals); ggplot2::ggsave("lasso_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_qq); ggplot2::ggsave("lasso_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Lasso" && device == "jpeg"){
    message(lasso_pred_vs_actual); ggplot2::ggsave("lasso_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_resid_vs_actual); ggplot2::ggsave("lasso_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_hist_residuals); ggplot2::ggsave("lasso_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_qq); ggplot2::ggsave("lasso_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Lasso" && device == "tiff"){
    message(lasso_pred_vs_actual); ggplot2::ggsave("lasso_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_resid_vs_actual); ggplot2::ggsave("lasso_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_hist_residuals); ggplot2::ggsave("lasso_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_qq); ggplot2::ggsave("lasso_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Lasso" && device == "png"){
    message(lasso_pred_vs_actual); ggplot2::ggsave("lasso_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_resid_vs_actual); ggplot2::ggsave("lasso_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_hist_residuals); ggplot2::ggsave("lasso_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_qq); ggplot2::ggsave("lasso_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Lasso" && device == "svg"){
    message(lasso_pred_vs_actual); ggplot2::ggsave("lasso_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_resid_vs_actual); ggplot2::ggsave("lasso_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_hist_residuals); ggplot2::ggsave("lasso_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(lasso_qq); ggplot2::ggsave("lasso_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Linear") {
  gridExtra::grid.arrange(linear_pred_vs_actual, linear_resid_vs_actual, linear_hist_residuals, linear_qq, ncol = 2)
  gridExtra::grid.arrange(linear_pred_vs_actual)
  gridExtra::grid.arrange(linear_resid_vs_actual)
  gridExtra::grid.arrange(linear_hist_residuals)
  gridExtra::grid.arrange(linear_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Linear" && device == "eps"){
    message(linear_pred_vs_actual); ggplot2::ggsave("linear_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_resid_vs_actual); ggplot2::ggsave("linear_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_hist_residuals); ggplot2::ggsave("linear_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_qq); ggplot2::ggsave("linear_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Linear" && device == "pdf"){
    message(linear_pred_vs_actual); ggplot2::ggsave("linear_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_resid_vs_actual); ggplot2::ggsave("linear_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_hist_residuals); ggplot2::ggsave("linear_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_qq); ggplot2::ggsave("linear_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Linear" && device == "jpeg"){
    message(linear_pred_vs_actual); ggplot2::ggsave("linear_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_resid_vs_actual); ggplot2::ggsave("linear_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_hist_residuals); ggplot2::ggsave("linear_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_qq); ggplot2::ggsave("linear_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Linear" && device == "tiff"){
    message(linear_pred_vs_actual); ggplot2::ggsave("linear_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_resid_vs_actual); ggplot2::ggsave("linear_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_hist_residuals); ggplot2::ggsave("linear_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_qq); ggplot2::ggsave("linear_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Linear" && device == "png"){
    message(linear_pred_vs_actual); ggplot2::ggsave("linear_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_resid_vs_actual); ggplot2::ggsave("linear_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_hist_residuals); ggplot2::ggsave("linear_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_qq); ggplot2::ggsave("linear_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Linear" && device == "svg"){
    message(linear_pred_vs_actual); ggplot2::ggsave("linear_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_resid_vs_actual); ggplot2::ggsave("linear_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_hist_residuals); ggplot2::ggsave("linear_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(linear_qq); ggplot2::ggsave("linear_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Neuralnet") {
  grid.arrange(neuralnet_pred_vs_actual, neuralnet_resid_vs_actual, neuralnet_hist_residuals, neuralnet_qq, ncol = 2)
  gridExtra::grid.arrange(neuralnet_pred_vs_actual)
  gridExtra::grid.arrange(neuralnet_resid_vs_actual)
  gridExtra::grid.arrange(neuralnet_hist_residuals)
  gridExtra::grid.arrange(neuralnet_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Neuralnet" && device == "eps"){
    message(neuralnet_pred_vs_actual); ggplot2::ggsave("neuralnet_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_resid_vs_actual); ggplot2::ggsave("neuralnet_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_hist_residuals); ggplot2::ggsave("neuralnet_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_qq); ggplot2::ggsave("neuralnet_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Neuralnet" && device == "pdf"){
    message(neuralnet_pred_vs_actual); ggplot2::ggsave("neuralnet_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_resid_vs_actual); ggplot2::ggsave("neuralnet_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_hist_residuals); ggplot2::ggsave("neuralnet_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_qq); ggplot2::ggsave("neuralnet_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Neuralnet" && device == "jpeg"){
    message(neuralnet_pred_vs_actual); ggplot2::ggsave("neuralnet_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_resid_vs_actual); ggplot2::ggsave("neuralnet_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_hist_residuals); ggplot2::ggsave("neuralnet_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_qq); ggplot2::ggsave("neuralnet_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Neuralnet" && device == "tiff"){
    message(neuralnet_pred_vs_actual); ggplot2::ggsave("neuralnet_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_resid_vs_actual); ggplot2::ggsave("neuralnet_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_hist_residuals); ggplot2::ggsave("neuralnet_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_qq); ggplot2::ggsave("neuralnet_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Neuralnet" && device == "png"){
    message(neuralnet_pred_vs_actual); ggplot2::ggsave("neuralnet_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_resid_vs_actual); ggplot2::ggsave("neuralnet_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_hist_residuals); ggplot2::ggsave("neuralnet_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_qq); ggplot2::ggsave("neuralnet_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Neuralnet" && device == "svg"){
    message(neuralnet_pred_vs_actual); ggplot2::ggsave("neuralnet_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_resid_vs_actual); ggplot2::ggsave("neuralnet_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_hist_residuals); ggplot2::ggsave("neuralnet_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(neuralnet_qq); ggplot2::ggsave("neuralnet_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "PLS") {
  grid.arrange(pls_pred_vs_actual, pls_resid_vs_actual, pls_hist_residuals, pls_qq, ncol = 2)
  gridExtra::grid.arrange(pls_pred_vs_actual)
  gridExtra::grid.arrange(pls_resid_vs_actual)
  gridExtra::grid.arrange(pls_hist_residuals)
  gridExtra::grid.arrange(pls_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "PLS" && device == "eps"){
    message(pls_pred_vs_actual); ggplot2::ggsave("pls_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_resid_vs_actual); ggplot2::ggsave("pls_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_hist_residuals); ggplot2::ggsave("pls_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_qq); ggplot2::ggsave("pls_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "PLS" && device == "pdf"){
    message(pls_pred_vs_actual); ggplot2::ggsave("pls_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_resid_vs_actual); ggplot2::ggsave("pls_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_hist_residuals); ggplot2::ggsave("pls_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_qq); ggplot2::ggsave("pls_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "PLS" && device == "jpeg"){
    message(pls_pred_vs_actual); ggplot2::ggsave("pls_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_resid_vs_actual); ggplot2::ggsave("pls_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_hist_residuals); ggplot2::ggsave("pls_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_qq); ggplot2::ggsave("pls_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "PLS" && device == "tiff"){
    message(pls_pred_vs_actual); ggplot2::ggsave("pls_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_resid_vs_actual); ggplot2::ggsave("pls_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_hist_residuals); ggplot2::ggsave("pls_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_qq); ggplot2::ggsave("pls_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "PLS" && device == "png"){
    message(pls_pred_vs_actual); ggplot2::ggsave("pls_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_resid_vs_actual); ggplot2::ggsave("pls_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_hist_residuals); ggplot2::ggsave("pls_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_qq); ggplot2::ggsave("pls_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "PLS" && device == "svg"){
    message(pls_pred_vs_actual); ggplot2::ggsave("pls_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_resid_vs_actual); ggplot2::ggsave("pls_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_hist_residuals); ggplot2::ggsave("pls_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pls_qq); ggplot2::ggsave("pls_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "PCR") {
  grid.arrange(pcr_pred_vs_actual, pcr_resid_vs_actual, pcr_hist_residuals, pcr_qq, ncol = 2)
  gridExtra::grid.arrange(pcr_pred_vs_actual)
  gridExtra::grid.arrange(pcr_resid_vs_actual)
  gridExtra::grid.arrange(pcr_hist_residuals)
  gridExtra::grid.arrange(pcr_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "PCR" && device == "eps"){
    message(pcr_pred_vs_actual); ggplot2::ggsave("pcr_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_resid_vs_actual); ggplot2::ggsave("pcr_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_hist_residuals); ggplot2::ggsave("pcr_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_qq); ggplot2::ggsave("pcr_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "PCR" && device == "pdf"){
    message(pcr_pred_vs_actual); ggplot2::ggsave("pcr_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_resid_vs_actual); ggplot2::ggsave("pcr_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_hist_residuals); ggplot2::ggsave("pcr_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_qq); ggplot2::ggsave("pcr_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "PCR" && device == "jpeg"){
    message(pcr_pred_vs_actual); ggplot2::ggsave("pcr_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_resid_vs_actual); ggplot2::ggsave("pcr_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_hist_residuals); ggplot2::ggsave("pcr_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_qq); ggplot2::ggsave("pcr_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "PCR" && device == "tiff"){
    message(pcr_pred_vs_actual); ggplot2::ggsave("pcr_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_resid_vs_actual); ggplot2::ggsave("pcr_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_hist_residuals); ggplot2::ggsave("pcr_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_qq); ggplot2::ggsave("pcr_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "PCR" && device == "png"){
    message(pcr_pred_vs_actual); ggplot2::ggsave("pcr_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_resid_vs_actual); ggplot2::ggsave("pcr_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_hist_residuals); ggplot2::ggsave("pcr_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_qq); ggplot2::ggsave("pcr_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "PCR" && device == "svg"){
    message(pcr_pred_vs_actual); ggplot2::ggsave("pcr_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_resid_vs_actual); ggplot2::ggsave("pcr_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_hist_residuals); ggplot2::ggsave("pcr_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(pcr_qq); ggplot2::ggsave("pcr_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "RF") {
  grid.arrange(rf_pred_vs_actual, rf_resid_vs_actual, rf_hist_residuals, rf_qq, ncol = 2)
  gridExtra::grid.arrange(rf_pred_vs_actual)
  gridExtra::grid.arrange(rf_resid_vs_actual)
  gridExtra::grid.arrange(rf_hist_residuals)
  gridExtra::grid.arrange(rf_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "RF" && device == "eps"){
    message(rf_pred_vs_actual); ggplot2::ggsave("rf_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_resid_vs_actual); ggplot2::ggsave("rf_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_hist_residuals); ggplot2::ggsave("rf_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_qq); ggplot2::ggsave("rf_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "RF" && device == "pdf"){
    message(rf_pred_vs_actual); ggplot2::ggsave("rf_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_resid_vs_actual); ggplot2::ggsave("rf_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_hist_residuals); ggplot2::ggsave("rf_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_qq); ggplot2::ggsave("rf_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "RF" && device == "jpeg"){
    message(rf_pred_vs_actual); ggplot2::ggsave("rf_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_resid_vs_actual); ggplot2::ggsave("rf_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_hist_residuals); ggplot2::ggsave("rf_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_qq); ggplot2::ggsave("rf_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "RF" && device == "tiff"){
    message(rf_pred_vs_actual); ggplot2::ggsave("rf_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_resid_vs_actual); ggplot2::ggsave("rf_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_hist_residuals); ggplot2::ggsave("rf_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_qq); ggplot2::ggsave("rf_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "RF" && device == "png"){
    message(rf_pred_vs_actual); ggplot2::ggsave("rf_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_resid_vs_actual); ggplot2::ggsave("rf_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_hist_residuals); ggplot2::ggsave("rf_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_qq); ggplot2::ggsave("rf_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "RF" && device == "svg"){
    message(rf_pred_vs_actual); ggplot2::ggsave("rf_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_resid_vs_actual); ggplot2::ggsave("rf_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_hist_residuals); ggplot2::ggsave("rf_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rf_qq); ggplot2::ggsave("rf_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ridge") {
  grid.arrange(ridge_pred_vs_actual, ridge_resid_vs_actual, ridge_hist_residuals, ridge_qq, ncol = 2)
  gridExtra::grid.arrange(ridge_pred_vs_actual)
  gridExtra::grid.arrange(ridge_resid_vs_actual)
  gridExtra::grid.arrange(ridge_hist_residuals)
  gridExtra::grid.arrange(ridge_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ridge" && device == "eps"){
    message(ridge_pred_vs_actual); ggplot2::ggsave("ridge_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_resid_vs_actual); ggplot2::ggsave("ridge_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_hist_residuals); ggplot2::ggsave("ridge_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_qq); ggplot2::ggsave("ridge_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ridge" && device == "pdf"){
    message(ridge_pred_vs_actual); ggplot2::ggsave("ridge_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_resid_vs_actual); ggplot2::ggsave("ridge_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_hist_residuals); ggplot2::ggsave("ridge_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_qq); ggplot2::ggsave("ridge_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ridge" && device == "jpeg"){
    message(ridge_pred_vs_actual); ggplot2::ggsave("ridge_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_resid_vs_actual); ggplot2::ggsave("ridge_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_hist_residuals); ggplot2::ggsave("ridge_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_qq); ggplot2::ggsave("ridge_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ridge" && device == "tiff"){
    message(ridge_pred_vs_actual); ggplot2::ggsave("ridge_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_resid_vs_actual); ggplot2::ggsave("ridge_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_hist_residuals); ggplot2::ggsave("ridge_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_qq); ggplot2::ggsave("ridge_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ridge" && device == "png"){
    message(ridge_pred_vs_actual); ggplot2::ggsave("ridge_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_resid_vs_actual); ggplot2::ggsave("ridge_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_hist_residuals); ggplot2::ggsave("ridge_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_qq); ggplot2::ggsave("ridge_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ridge" && device == "svg"){
    message(ridge_pred_vs_actual); ggplot2::ggsave("ridge_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_resid_vs_actual); ggplot2::ggsave("ridge_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_hist_residuals); ggplot2::ggsave("ridge_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ridge_qq); ggplot2::ggsave("ridge_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Rpart") {
  grid.arrange(rpart_pred_vs_actual, rpart_resid_vs_actual, rpart_hist_residuals, rpart_qq, ncol = 2)
  gridExtra::grid.arrange(rpart_pred_vs_actual)
  gridExtra::grid.arrange(rpart_resid_vs_actual)
  gridExtra::grid.arrange(rpart_hist_residuals)
  gridExtra::grid.arrange(rpart_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Rpart" && device == "eps"){
    message(rpart_pred_vs_actual); ggplot2::ggsave("rpart_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_resid_vs_actual); ggplot2::ggsave("rpart_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_hist_residuals); ggplot2::ggsave("rpart_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_qq); ggplot2::ggsave("rpart_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Rpart" && device == "pdf"){
    message(rpart_pred_vs_actual); ggplot2::ggsave("rpart_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_resid_vs_actual); ggplot2::ggsave("rpart_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_hist_residuals); ggplot2::ggsave("rpart_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_qq); ggplot2::ggsave("rpart_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Rpart" && device == "jpeg"){
    message(rpart_pred_vs_actual); ggplot2::ggsave("rpart_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_resid_vs_actual); ggplot2::ggsave("rpart_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_hist_residuals); ggplot2::ggsave("rpart_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_qq); ggplot2::ggsave("rpart_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Rpart" && device == "tiff"){
    message(rpart_pred_vs_actual); ggplot2::ggsave("rpart_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_resid_vs_actual); ggplot2::ggsave("rpart_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_hist_residuals); ggplot2::ggsave("rpart_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_qq); ggplot2::ggsave("rpart_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Rpart" && device == "png"){
    message(rpart_pred_vs_actual); ggplot2::ggsave("rpart_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_resid_vs_actual); ggplot2::ggsave("rpart_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_hist_residuals); ggplot2::ggsave("rpart_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_qq); ggplot2::ggsave("rpart_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Rpart" && device == "svg"){
    message(rpart_pred_vs_actual); ggplot2::ggsave("rpart_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_resid_vs_actual); ggplot2::ggsave("rpart_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_hist_residuals); ggplot2::ggsave("rpart_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(rpart_qq); ggplot2::ggsave("rpart_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "SVM") {
  grid.arrange(svm_pred_vs_actual, svm_resid_vs_actual, svm_hist_residuals, svm_qq, ncol = 2)
  gridExtra::grid.arrange(svm_pred_vs_actual)
  gridExtra::grid.arrange(svm_resid_vs_actual)
  gridExtra::grid.arrange(svm_hist_residuals)
  gridExtra::grid.arrange(svm_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "SVM" && device == "eps"){
    message(svm_pred_vs_actual); ggplot2::ggsave("svm_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_resid_vs_actual); ggplot2::ggsave("svm_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_hist_residuals); ggplot2::ggsave("svm_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_qq); ggplot2::ggsave("svm_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "SVM" && device == "pdf"){
    message(svm_pred_vs_actual); ggplot2::ggsave("svm_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_resid_vs_actual); ggplot2::ggsave("svm_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_hist_residuals); ggplot2::ggsave("svm_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_qq); ggplot2::ggsave("svm_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "SVM" && device == "jpeg"){
    message(svm_pred_vs_actual); ggplot2::ggsave("svm_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_resid_vs_actual); ggplot2::ggsave("svm_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_hist_residuals); ggplot2::ggsave("svm_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_qq); ggplot2::ggsave("svm_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "SVM" && device == "tiff"){
    message(svm_pred_vs_actual); ggplot2::ggsave("svm_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_resid_vs_actual); ggplot2::ggsave("svm_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_hist_residuals); ggplot2::ggsave("svm_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_qq); ggplot2::ggsave("svm_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "SVM" && device == "png"){
    message(svm_pred_vs_actual); ggplot2::ggsave("svm_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_resid_vs_actual); ggplot2::ggsave("svm_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_hist_residuals); ggplot2::ggsave("svm_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_qq); ggplot2::ggsave("svm_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "SVM" && device == "svg"){
    message(svm_pred_vs_actual); ggplot2::ggsave("svm_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_resid_vs_actual); ggplot2::ggsave("svm_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_hist_residuals); ggplot2::ggsave("svm_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(svm_qq); ggplot2::ggsave("svm_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Tree") {
  grid.arrange(tree_pred_vs_actual, tree_resid_vs_actual, tree_hist_residuals, tree_qq, ncol = 2)
  gridExtra::grid.arrange(tree_pred_vs_actual)
  gridExtra::grid.arrange(tree_resid_vs_actual)
  gridExtra::grid.arrange(tree_hist_residuals)
  gridExtra::grid.arrange(tree_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Tree" && device == "eps"){
    message(tree_pred_vs_actual); ggplot2::ggsave("tree_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_resid_vs_actual); ggplot2::ggsave("tree_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_hist_residuals); ggplot2::ggsave("tree_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_qq); ggplot2::ggsave("tree_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Tree" && device == "pdf"){
    message(tree_pred_vs_actual); ggplot2::ggsave("tree_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_resid_vs_actual); ggplot2::ggsave("tree_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_hist_residuals); ggplot2::ggsave("tree_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_qq); ggplot2::ggsave("tree_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Tree" && device == "jpeg"){
    message(tree_pred_vs_actual); ggplot2::ggsave("tree_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_resid_vs_actual); ggplot2::ggsave("tree_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_hist_residuals); ggplot2::ggsave("tree_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_qq); ggplot2::ggsave("tree_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Tree" && device == "tiff"){
    message(tree_pred_vs_actual); ggplot2::ggsave("tree_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_resid_vs_actual); ggplot2::ggsave("tree_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_hist_residuals); ggplot2::ggsave("tree_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_qq); ggplot2::ggsave("tree_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Tree" && device == "png"){
    message(tree_pred_vs_actual); ggplot2::ggsave("tree_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_resid_vs_actual); ggplot2::ggsave("tree_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_hist_residuals); ggplot2::ggsave("tree_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_qq); ggplot2::ggsave("tree_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Tree" && device == "svg"){
    message(tree_pred_vs_actual); ggplot2::ggsave("tree_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_resid_vs_actual); ggplot2::ggsave("tree_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_hist_residuals); ggplot2::ggsave("tree_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(tree_qq); ggplot2::ggsave("tree_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "XGBoost") {
  grid.arrange(xgb_pred_vs_actual, xgb_resid_vs_actual, xgb_hist_residuals, xgb_qq, ncol = 2)
  gridExtra::grid.arrange(xgb_pred_vs_actual)
  gridExtra::grid.arrange(xgb_resid_vs_actual)
  gridExtra::grid.arrange(xgb_hist_residuals)
  gridExtra::grid.arrange(xgb_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "XGBoost" && device == "eps"){
    message(xgb_pred_vs_actual); ggplot2::ggsave("xgb_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_resid_vs_actual); ggplot2::ggsave("xgb_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_hist_residuals); ggplot2::ggsave("xgb_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_qq); ggplot2::ggsave("xgb_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "XGBoost" && device == "pdf"){
    message(xgb_pred_vs_actual); ggplot2::ggsave("xgb_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_resid_vs_actual); ggplot2::ggsave("xgb_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_hist_residuals); ggplot2::ggsave("xgb_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_qq); ggplot2::ggsave("xgb_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "XGBoost" && device == "jpeg"){
    message(xgb_pred_vs_actual); ggplot2::ggsave("xgb_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_resid_vs_actual); ggplot2::ggsave("xgb_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_hist_residuals); ggplot2::ggsave("xgb_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_qq); ggplot2::ggsave("xgb_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "XGBoost" && device == "tiff"){
    message(xgb_pred_vs_actual); ggplot2::ggsave("xgb_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_resid_vs_actual); ggplot2::ggsave("xgb_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_hist_residuals); ggplot2::ggsave("xgb_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_qq); ggplot2::ggsave("xgb_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "XGBoost" && device == "png"){
    message(xgb_pred_vs_actual); ggplot2::ggsave("xgb_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_resid_vs_actual); ggplot2::ggsave("xgb_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_hist_residuals); ggplot2::ggsave("xgb_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_qq); ggplot2::ggsave("xgb_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "XGBoost" && device == "svg"){
    message(xgb_pred_vs_actual); ggplot2::ggsave("xgb_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_resid_vs_actual); ggplot2::ggsave("xgb_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_hist_residuals); ggplot2::ggsave("xgb_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(xgb_qq); ggplot2::ggsave("xgb_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble Bagged Random Forest") {
  gridExtra::grid.arrange(ensemble_bag_rf_pred_vs_actual, ensemble_bag_rf_resid_vs_actual, ensemble_bag_rf_hist_residuals, ensemble_bag_rf_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_bag_rf_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_bag_rf_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_bag_rf_hist_residuals)
  gridExtra::grid.arrange(ensemble_bag_rf_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagged Random Forest" && device == "eps"){
    message(ensemble_bag_rf_pred_vs_actual); ggplot2::ggsave("ensemble_bag_rf_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_resid_vs_actual); ggplot2::ggsave("ensemble_bag_rf_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_hist_residuals); ggplot2::ggsave("ensemble_bag_rf_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_qq); ggplot2::ggsave("ensemble_bag_rf_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagged Random Forest" && device == "pdf"){
    message(ensemble_bag_rf_pred_vs_actual); ggplot2::ggsave("ensemble_bag_rf_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_resid_vs_actual); ggplot2::ggsave("ensemble_bag_rf_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_hist_residuals); ggplot2::ggsave("ensemble_bag_rf_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_qq); ggplot2::ggsave("ensemble_bag_rf_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagged Random Forest" && device == "jpeg"){
    message(ensemble_bag_rf_pred_vs_actual); ggplot2::ggsave("ensemble_bag_rf_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_resid_vs_actual); ggplot2::ggsave("ensemble_bag_rf_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_hist_residuals); ggplot2::ggsave("ensemble_bag_rf_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_qq); ggplot2::ggsave("ensemble_bag_rf_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagged Random Forest" && device == "tiff"){
    message(ensemble_bag_rf_pred_vs_actual); ggplot2::ggsave("ensemble_bag_rf_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_resid_vs_actual); ggplot2::ggsave("ensemble_bag_rf_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_hist_residuals); ggplot2::ggsave("ensemble_bag_rf_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_qq); ggplot2::ggsave("ensemble_bag_rf_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagged Random Forest" && device == "png"){
    message(ensemble_bag_rf_pred_vs_actual); ggplot2::ggsave("ensemble_bag_rf_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_resid_vs_actual); ggplot2::ggsave("ensemble_bag_rf_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_hist_residuals); ggplot2::ggsave("ensemble_bag_rf_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_qq); ggplot2::ggsave("ensemble_bag_rf_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagged Random Forest" && device == "svg"){
    message(ensemble_bag_rf_pred_vs_actual); ggplot2::ggsave("ensemble_bag_rf_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_resid_vs_actual); ggplot2::ggsave("ensemble_bag_rf_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_hist_residuals); ggplot2::ggsave("ensemble_bag_rf_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bag_rf_qq); ggplot2::ggsave("ensemble_bag_rf_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble Bagging") {
  gridExtra::grid.arrange(ensemble_bagging_pred_vs_actual, ensemble_bagging_resid_vs_actual, ensemble_bagging_hist_residuals, ensemble_bagging_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_bagging_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_bagging_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_bagging_hist_residuals)
  gridExtra::grid.arrange(ensemble_bagging_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagging" && device == "eps"){
    message(ensemble_bagging_pred_vs_actual); ggplot2::ggsave("ensemble_bagging_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_resid_vs_actual); ggplot2::ggsave("ensemble_bagging_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_hist_residuals); ggplot2::ggsave("ensemble_bagging_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_qq); ggplot2::ggsave("ensemble_bagging_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagging" && device == "pdf"){
    message(ensemble_bagging_pred_vs_actual); ggplot2::ggsave("ensemble_bagging_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_resid_vs_actual); ggplot2::ggsave("ensemble_bagging_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_hist_residuals); ggplot2::ggsave("ensemble_bagging_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_qq); ggplot2::ggsave("ensemble_bagging_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagging" && device == "jpeg"){
    message(ensemble_bagging_pred_vs_actual); ggplot2::ggsave("ensemble_bagging_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_resid_vs_actual); ggplot2::ggsave("ensemble_bagging_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_hist_residuals); ggplot2::ggsave("ensemble_bagging_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_qq); ggplot2::ggsave("ensemble_bagging_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagging" && device == "tiff"){
    message(ensemble_bagging_pred_vs_actual); ggplot2::ggsave("ensemble_bagging_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_resid_vs_actual); ggplot2::ggsave("ensemble_bagging_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_hist_residuals); ggplot2::ggsave("ensemble_bagging_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_qq); ggplot2::ggsave("ensemble_bagging_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagging" && device == "png"){
    message(ensemble_bagging_pred_vs_actual); ggplot2::ggsave("ensemble_bagging_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_resid_vs_actual); ggplot2::ggsave("ensemble_bagging_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_hist_residuals); ggplot2::ggsave("ensemble_bagging_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_qq); ggplot2::ggsave("ensemble_bagging_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Bagging" && device == "svg"){
    message(ensemble_bagging_pred_vs_actual); ggplot2::ggsave("ensemble_bagging_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_resid_vs_actual); ggplot2::ggsave("ensemble_bagging_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_hist_residuals); ggplot2::ggsave("ensemble_bagging_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bagging_qq); ggplot2::ggsave("ensemble_bagging_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble BayesGLM") {
  gridExtra::grid.arrange(ensemble_bayesglm_pred_vs_actual, ensemble_bayesglm_resid_vs_actual, ensemble_bayesglm_hist_residuals, ensemble_bayesglm_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_bayesglm_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_bayesglm_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_bayesglm_hist_residuals)
  gridExtra::grid.arrange(ensemble_bayesglm_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesGLM" && device == "eps"){
    message(ensemble_bayesglm_pred_vs_actual); ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_resid_vs_actual); ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_hist_residuals); ggplot2::ggsave("ensemble_bayesglm_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_qq); ggplot2::ggsave("ensemble_bayesglm_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesGLM" && device == "pdf"){
    message(ensemble_bayesglm_pred_vs_actual); ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_resid_vs_actual); ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_hist_residuals); ggplot2::ggsave("ensemble_bayesglm_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_qq); ggplot2::ggsave("ensemble_bayesglm_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesGLM" && device == "jpeg"){
    message(ensemble_bayesglm_pred_vs_actual); ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_resid_vs_actual); ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_hist_residuals); ggplot2::ggsave("ensemble_bayesglm_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_qq); ggplot2::ggsave("ensemble_bayesglm_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesGLM" && device == "tiff"){
    message(ensemble_bayesglm_pred_vs_actual); ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_resid_vs_actual); ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_hist_residuals); ggplot2::ggsave("ensemble_bayesglm_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_qq); ggplot2::ggsave("ensemble_bayesglm_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesGLM" && device == "png"){
    message(ensemble_bayesglm_pred_vs_actual); ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_resid_vs_actual); ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_hist_residuals); ggplot2::ggsave("ensemble_bayesglm_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_qq); ggplot2::ggsave("ensemble_bayesglm_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesGLM" && device == "svg"){
    message(ensemble_bayesglm_pred_vs_actual); ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_resid_vs_actual); ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_hist_residuals); ggplot2::ggsave("ensemble_bayesglm_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesglm_qq); ggplot2::ggsave("ensemble_bayesglm_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble BayesRNN") {
  gridExtra::grid.arrange(ensemble_bayesrnn_pred_vs_actual, ensemble_bayesrnn_resid_vs_actual, ensemble_bayesrnn_hist_residuals, ensemble_bayesrnn_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_bayesrnn_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_bayesrnn_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_bayesrnn_hist_residuals)
  gridExtra::grid.arrange(ensemble_bayesrnn_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesRNN" && device == "eps"){
    message(ensemble_bayesrnn_pred_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_resid_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_hist_residuals); ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_qq); ggplot2::ggsave("ensemble_bayesrnn_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesRNN" && device == "pdf"){
    message(ensemble_bayesrnn_pred_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_resid_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_hist_residuals); ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_qq); ggplot2::ggsave("ensemble_bayesrnn_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesRNN" && device == "jpeg"){
    message(ensemble_bayesrnn_pred_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_resid_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_hist_residuals); ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_qq); ggplot2::ggsave("ensemble_bayesrnn_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesRNN" && device == "tiff"){
    message(ensemble_bayesrnn_pred_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_resid_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_hist_residuals); ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_qq); ggplot2::ggsave("ensemble_bayesrnn_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesRNN" && device == "png"){
    message(ensemble_bayesrnn_pred_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_resid_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_hist_residuals); ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_qq); ggplot2::ggsave("ensemble_bayesrnn_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble BayesRNN" && device == "svg"){
    message(ensemble_bayesrnn_pred_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_resid_vs_actual); ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_hist_residuals); ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_bayesrnn_qq); ggplot2::ggsave("ensemble_bayesrnn_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble Boosted RF") {
  gridExtra::grid.arrange(ensemble_boost_rf_pred_vs_actual, ensemble_boost_rf_resid_vs_actual, ensemble_boost_rf_hist_residuals, ensemble_boost_rf_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_boost_rf_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_boost_rf_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_boost_rf_hist_residuals)
  gridExtra::grid.arrange(ensemble_boost_rf_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Boosted RF" && device == "eps"){
    message(ensemble_boost_rf_pred_vs_actual); ggplot2::ggsave("ensemble_boost_rf_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_resid_vs_actual); ggplot2::ggsave("ensemble_boost_rf_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_hist_residuals); ggplot2::ggsave("ensemble_boost_rf_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_qq); ggplot2::ggsave("ensemble_boost_rf_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Boosted RF" && device == "pdf"){
    message(ensemble_boost_rf_pred_vs_actual); ggplot2::ggsave("ensemble_boost_rf_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_resid_vs_actual); ggplot2::ggsave("ensemble_boost_rf_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_hist_residuals); ggplot2::ggsave("ensemble_boost_rf_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_qq); ggplot2::ggsave("ensemble_boost_rf_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Boosted RF" && device == "jpeg"){
    message(ensemble_boost_rf_pred_vs_actual); ggplot2::ggsave("ensemble_boost_rf_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_resid_vs_actual); ggplot2::ggsave("ensemble_boost_rf_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_hist_residuals); ggplot2::ggsave("ensemble_boost_rf_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_qq); ggplot2::ggsave("ensemble_boost_rf_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Boosted RF" && device == "tiff"){
    message(ensemble_boost_rf_pred_vs_actual); ggplot2::ggsave("ensemble_boost_rf_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_resid_vs_actual); ggplot2::ggsave("ensemble_boost_rf_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_hist_residuals); ggplot2::ggsave("ensemble_boost_rf_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_qq); ggplot2::ggsave("ensemble_boost_rf_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Boosted RF" && device == "png"){
    message(ensemble_boost_rf_pred_vs_actual); ggplot2::ggsave("ensemble_boost_rf_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_resid_vs_actual); ggplot2::ggsave("ensemble_boost_rf_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_hist_residuals); ggplot2::ggsave("ensemble_boost_rf_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_qq); ggplot2::ggsave("ensemble_boost_rf_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Boosted RF" && device == "svg"){
    message(ensemble_boost_rf_pred_vs_actual); ggplot2::ggsave("ensemble_boost_rf_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_resid_vs_actual); ggplot2::ggsave("ensemble_boost_rf_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_hist_residuals); ggplot2::ggsave("ensemble_boost_rf_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_boost_rf_qq); ggplot2::ggsave("ensemble_boost_rf_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble Cubist") {
  gridExtra::grid.arrange(ensemble_cubist_pred_vs_actual, ensemble_cubist_resid_vs_actual, ensemble_cubist_hist_residuals, ensemble_cubist_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_cubist_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_cubist_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_cubist_hist_residuals)
  gridExtra::grid.arrange(ensemble_cubist_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Cubist" && device == "eps"){
    message(ensemble_cubist_pred_vs_actual); ggplot2::ggsave("ensemble_cubist_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_resid_vs_actual); ggplot2::ggsave("ensemble_cubist_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_hist_residuals); ggplot2::ggsave("ensemble_cubist_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_qq); ggplot2::ggsave("ensemble_cubist_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Cubist" && device == "pdf"){
    message(ensemble_cubist_pred_vs_actual); ggplot2::ggsave("ensemble_cubist_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_resid_vs_actual); ggplot2::ggsave("ensemble_cubist_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_hist_residuals); ggplot2::ggsave("ensemble_cubist_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_qq); ggplot2::ggsave("ensemble_cubist_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Cubist" && device == "jpeg"){
    message(ensemble_cubist_pred_vs_actual); ggplot2::ggsave("ensemble_cubist_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_resid_vs_actual); ggplot2::ggsave("ensemble_cubist_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_hist_residuals); ggplot2::ggsave("ensemble_cubist_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_qq); ggplot2::ggsave("ensemble_cubist_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Cubist" && device == "tiff"){
    message(ensemble_cubist_pred_vs_actual); ggplot2::ggsave("ensemble_cubist_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_resid_vs_actual); ggplot2::ggsave("ensemble_cubist_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_hist_residuals); ggplot2::ggsave("ensemble_cubist_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_qq); ggplot2::ggsave("ensemble_cubist_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Cubist" && device == "png"){
    message(ensemble_cubist_pred_vs_actual); ggplot2::ggsave("ensemble_cubist_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_resid_vs_actual); ggplot2::ggsave("ensemble_cubist_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_hist_residuals); ggplot2::ggsave("ensemble_cubist_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_qq); ggplot2::ggsave("ensemble_cubist_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Cubist" && device == "svg"){
    message(ensemble_cubist_pred_vs_actual); ggplot2::ggsave("ensemble_cubist_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_resid_vs_actual); ggplot2::ggsave("ensemble_cubist_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_hist_residuals); ggplot2::ggsave("ensemble_cubist_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_cubist_qq); ggplot2::ggsave("ensemble_cubist_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble Earth") {
  gridExtra::grid.arrange(ensemble_earth_pred_vs_actual, ensemble_earth_resid_vs_actual, ensemble_earth_hist_residuals, ensemble_earth_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_earth_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_earth_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_earth_hist_residuals)
  gridExtra::grid.arrange(ensemble_earth_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Earth" && device == "eps"){
    message(ensemble_earth_pred_vs_actual); ggplot2::ggsave("ensemble_earth_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_resid_vs_actual); ggplot2::ggsave("ensemble_earth_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_hist_residuals); ggplot2::ggsave("ensemble_earth_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_qq); ggplot2::ggsave("ensemble_earth_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Earth" && device == "pdf"){
    message(ensemble_earth_pred_vs_actual); ggplot2::ggsave("ensemble_earth_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_resid_vs_actual); ggplot2::ggsave("ensemble_earth_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_hist_residuals); ggplot2::ggsave("ensemble_earth_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_qq); ggplot2::ggsave("ensemble_earth_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Earth" && device == "jpeg"){
    message(ensemble_earth_pred_vs_actual); ggplot2::ggsave("ensemble_earth_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_resid_vs_actual); ggplot2::ggsave("ensemble_earth_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_hist_residuals); ggplot2::ggsave("ensemble_earth_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_qq); ggplot2::ggsave("ensemble_earth_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Earth" && device == "tiff"){
    message(ensemble_earth_pred_vs_actual); ggplot2::ggsave("ensemble_earth_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_resid_vs_actual); ggplot2::ggsave("ensemble_earth_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_hist_residuals); ggplot2::ggsave("ensemble_earth_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_qq); ggplot2::ggsave("ensemble_earth_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Earth" && device == "png"){
    message(ensemble_earth_pred_vs_actual); ggplot2::ggsave("ensemble_earth_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_resid_vs_actual); ggplot2::ggsave("ensemble_earth_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_hist_residuals); ggplot2::ggsave("ensemble_earth_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_qq); ggplot2::ggsave("ensemble_earth_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Earth" && device == "svg"){
    message(ensemble_earth_pred_vs_actual); ggplot2::ggsave("ensemble_earth_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_resid_vs_actual); ggplot2::ggsave("ensemble_earth_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_hist_residuals); ggplot2::ggsave("ensemble_earth_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_earth_qq); ggplot2::ggsave("ensemble_earth_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble_Elastic") {
  grid.arrange(ensemble_elastic_pred_vs_actual, ensemble_elastic_resid_vs_actual, ensemble_elastic_hist_residuals, ensemble_elastic_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_elastic_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_elastic_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_elastic_hist_residuals)
  gridExtra::grid.arrange(ensemble_elastic_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Elastic" && device == "eps"){
    message(ensemble_elastic_pred_vs_actual); ggplot2::ggsave("ensemble_elastic_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_resid_vs_actual); ggplot2::ggsave("ensemble_elastic_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_hist_residuals); ggplot2::ggsave("ensemble_elastic_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_qq); ggplot2::ggsave("ensemble_elastic_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Elastic" && device == "pdf"){
    message(ensemble_elastic_pred_vs_actual); ggplot2::ggsave("ensemble_elastic_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_resid_vs_actual); ggplot2::ggsave("ensemble_elastic_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_hist_residuals); ggplot2::ggsave("ensemble_elastic_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_qq); ggplot2::ggsave("ensemble_elastic_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Elastic" && device == "jpeg"){
    message(ensemble_elastic_pred_vs_actual); ggplot2::ggsave("ensemble_elastic_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_resid_vs_actual); ggplot2::ggsave("ensemble_elastic_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_hist_residuals); ggplot2::ggsave("ensemble_elastic_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_qq); ggplot2::ggsave("ensemble_elastic_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Elastic" && device == "tiff"){
    message(ensemble_elastic_pred_vs_actual); ggplot2::ggsave("ensemble_elastic_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_resid_vs_actual); ggplot2::ggsave("ensemble_elastic_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_hist_residuals); ggplot2::ggsave("ensemble_elastic_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_qq); ggplot2::ggsave("ensemble_elastic_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Elastic" && device == "png"){
    message(ensemble_elastic_pred_vs_actual); ggplot2::ggsave("ensemble_elastic_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_resid_vs_actual); ggplot2::ggsave("ensemble_elastic_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_hist_residuals); ggplot2::ggsave("ensemble_elastic_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_qq); ggplot2::ggsave("ensemble_elastic_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Elastic" && device == "svg"){
    message(ensemble_elastic_pred_vs_actual); ggplot2::ggsave("ensemble_elastic_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_resid_vs_actual); ggplot2::ggsave("ensemble_elastic_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_hist_residuals); ggplot2::ggsave("ensemble_elastic_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_elastic_qq); ggplot2::ggsave("ensemble_elastic_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble Gradient Boosted") {
  gridExtra::grid.arrange(ensemble_gb_pred_vs_actual, ensemble_gb_resid_vs_actual, ensemble_gb_hist_residuals, ensemble_gb_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_gb_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_gb_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_gb_hist_residuals)
  gridExtra::grid.arrange(ensemble_gb_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Gradient Boosted" && device == "eps"){
    message(ensemble_gb_pred_vs_actual); ggplot2::ggsave("ensemble_gb_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_resid_vs_actual); ggplot2::ggsave("ensemble_gb_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_hist_residuals); ggplot2::ggsave("ensemble_gb_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_qq); ggplot2::ggsave("ensemble_gb_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Gradient Boosted" && device == "pdf"){
    message(ensemble_gb_pred_vs_actual); ggplot2::ggsave("ensemble_gb_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_resid_vs_actual); ggplot2::ggsave("ensemble_gb_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_hist_residuals); ggplot2::ggsave("ensemble_gb_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_qq); ggplot2::ggsave("ensemble_gb_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Gradient Boosted" && device == "jpeg"){
    message(ensemble_gb_pred_vs_actual); ggplot2::ggsave("ensemble_gb_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_resid_vs_actual); ggplot2::ggsave("ensemble_gb_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_hist_residuals); ggplot2::ggsave("ensemble_gb_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_qq); ggplot2::ggsave("ensemble_gb_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Gradient Boosted" && device == "tiff"){
    message(ensemble_gb_pred_vs_actual); ggplot2::ggsave("ensemble_gb_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_resid_vs_actual); ggplot2::ggsave("ensemble_gb_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_hist_residuals); ggplot2::ggsave("ensemble_gb_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_qq); ggplot2::ggsave("ensemble_gb_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Gradient Boosted" && device == "png"){
    message(ensemble_gb_pred_vs_actual); ggplot2::ggsave("ensemble_gb_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_resid_vs_actual); ggplot2::ggsave("ensemble_gb_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_hist_residuals); ggplot2::ggsave("ensemble_gb_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_qq); ggplot2::ggsave("ensemble_gb_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Gradient Boosted" && device == "svg"){
    message(ensemble_gb_pred_vs_actual); ggplot2::ggsave("ensemble_gb_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_resid_vs_actual); ggplot2::ggsave("ensemble_gb_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_hist_residuals); ggplot2::ggsave("ensemble_gb_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_gb_qq); ggplot2::ggsave("ensemble_gb_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble K-Nearest Neighbors") {
  grid.arrange(ensemble_knn_pred_vs_actual, ensemble_knn_resid_vs_actual, ensemble_knn_hist_residuals, ensemble_knn_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_knn_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_knn_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_knn_hist_residuals)
  gridExtra::grid.arrange(ensemble_knn_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble K-Nearest Neighbors" && device == "eps"){
    message(ensemble_knn_pred_vs_actual); ggplot2::ggsave("ensemble_knn_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_resid_vs_actual); ggplot2::ggsave("ensemble_knn_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_hist_residuals); ggplot2::ggsave("ensemble_knn_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_qq); ggplot2::ggsave("ensemble_knn_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble K-Nearest Neighbors" && device == "pdf"){
    message(ensemble_knn_pred_vs_actual); ggplot2::ggsave("ensemble_knn_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_resid_vs_actual); ggplot2::ggsave("ensemble_knn_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_hist_residuals); ggplot2::ggsave("ensemble_knn_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_qq); ggplot2::ggsave("ensemble_knn_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble K-Nearest Neighbors" && device == "jpeg"){
    message(ensemble_knn_pred_vs_actual); ggplot2::ggsave("ensemble_knn_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_resid_vs_actual); ggplot2::ggsave("ensemble_knn_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_hist_residuals); ggplot2::ggsave("ensemble_knn_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_qq); ggplot2::ggsave("ensemble_knn_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble K-Nearest Neighbors" && device == "tiff"){
    message(ensemble_knn_pred_vs_actual); ggplot2::ggsave("ensemble_knn_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_resid_vs_actual); ggplot2::ggsave("ensemble_knn_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_hist_residuals); ggplot2::ggsave("ensemble_knn_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_qq); ggplot2::ggsave("ensemble_knn_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble K-Nearest Neighbors" && device == "png"){
    message(ensemble_knn_pred_vs_actual); ggplot2::ggsave("ensemble_knn_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_resid_vs_actual); ggplot2::ggsave("ensemble_knn_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_hist_residuals); ggplot2::ggsave("ensemble_knn_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_qq); ggplot2::ggsave("ensemble_knn_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble K-Nearest Neighbors" && device == "svg"){
    message(ensemble_knn_pred_vs_actual); ggplot2::ggsave("ensemble_knn_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_resid_vs_actual); ggplot2::ggsave("ensemble_knn_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_hist_residuals); ggplot2::ggsave("ensemble_knn_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_knn_qq); ggplot2::ggsave("ensemble_knn_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble_Lasso") {
  grid.arrange(ensemble_lasso_pred_vs_actual, ensemble_lasso_resid_vs_actual, ensemble_lasso_hist_residuals, ensemble_lasso_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_lasso_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_lasso_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_lasso_hist_residuals)
  gridExtra::grid.arrange(ensemble_lasso_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Lasso" && device == "eps"){
    message(ensemble_lasso_pred_vs_actual); ggplot2::ggsave("ensemble_lasso_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_resid_vs_actual); ggplot2::ggsave("ensemble_lasso_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_hist_residuals); ggplot2::ggsave("ensemble_lasso_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_qq); ggplot2::ggsave("ensemble_lasso_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Lasso" && device == "pdf"){
    message(ensemble_lasso_pred_vs_actual); ggplot2::ggsave("ensemble_lasso_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_resid_vs_actual); ggplot2::ggsave("ensemble_lasso_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_hist_residuals); ggplot2::ggsave("ensemble_lasso_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_qq); ggplot2::ggsave("ensemble_lasso_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Lasso" && device == "jpeg"){
    message(ensemble_lasso_pred_vs_actual); ggplot2::ggsave("ensemble_lasso_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_resid_vs_actual); ggplot2::ggsave("ensemble_lasso_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_hist_residuals); ggplot2::ggsave("ensemble_lasso_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_qq); ggplot2::ggsave("ensemble_lasso_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Lasso" && device == "tiff"){
    message(ensemble_lasso_pred_vs_actual); ggplot2::ggsave("ensemble_lasso_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_resid_vs_actual); ggplot2::ggsave("ensemble_lasso_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_hist_residuals); ggplot2::ggsave("ensemble_lasso_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_qq); ggplot2::ggsave("ensemble_lasso_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Lasso" && device == "png"){
    message(ensemble_lasso_pred_vs_actual); ggplot2::ggsave("ensemble_lasso_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_resid_vs_actual); ggplot2::ggsave("ensemble_lasso_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_hist_residuals); ggplot2::ggsave("ensemble_lasso_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_qq); ggplot2::ggsave("ensemble_lasso_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Lasso" && device == "svg"){
    message(ensemble_lasso_pred_vs_actual); ggplot2::ggsave("ensemble_lasso_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_resid_vs_actual); ggplot2::ggsave("ensemble_lasso_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_hist_residuals); ggplot2::ggsave("ensemble_lasso_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_lasso_qq); ggplot2::ggsave("ensemble_lasso_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble Linear") {
  gridExtra::grid.arrange(ensemble_linear_pred_vs_actual, ensemble_linear_resid_vs_actual, ensemble_linear_hist_residuals, ensemble_linear_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_linear_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_linear_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_linear_hist_residuals)
  gridExtra::grid.arrange(ensemble_linear_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Linear" && device == "eps"){
    message(ensemble_linear_pred_vs_actual); ggplot2::ggsave("ensemble_linear_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_resid_vs_actual); ggplot2::ggsave("ensemble_linear_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_hist_residuals); ggplot2::ggsave("ensemble_linear_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_qq); ggplot2::ggsave("ensemble_linear_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Linear" && device == "pdf"){
    message(ensemble_linear_pred_vs_actual); ggplot2::ggsave("ensemble_linear_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_resid_vs_actual); ggplot2::ggsave("ensemble_linear_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_hist_residuals); ggplot2::ggsave("ensemble_linear_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_qq); ggplot2::ggsave("ensemble_linear_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Linear" && device == "jpeg"){
    message(ensemble_linear_pred_vs_actual); ggplot2::ggsave("ensemble_linear_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_resid_vs_actual); ggplot2::ggsave("ensemble_linear_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_hist_residuals); ggplot2::ggsave("ensemble_linear_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_qq); ggplot2::ggsave("ensemble_linear_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Linear" && device == "tiff"){
    message(ensemble_linear_pred_vs_actual); ggplot2::ggsave("ensemble_linear_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_resid_vs_actual); ggplot2::ggsave("ensemble_linear_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_hist_residuals); ggplot2::ggsave("ensemble_linear_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_qq); ggplot2::ggsave("ensemble_linear_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Linear" && device == "png"){
    message(ensemble_linear_pred_vs_actual); ggplot2::ggsave("ensemble_linear_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_resid_vs_actual); ggplot2::ggsave("ensemble_linear_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_hist_residuals); ggplot2::ggsave("ensemble_linear_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_qq); ggplot2::ggsave("ensemble_linear_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Linear" && device == "svg"){
    message(ensemble_linear_pred_vs_actual); ggplot2::ggsave("ensemble_linear_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_resid_vs_actual); ggplot2::ggsave("ensemble_linear_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_hist_residuals); ggplot2::ggsave("ensemble_linear_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_linear_qq); ggplot2::ggsave("ensemble_linear_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble RF") {
  gridExtra::grid.arrange(ensemble_rf_pred_vs_actual, ensemble_rf_resid_vs_actual, ensemble_rf_hist_residuals, ensemble_rf_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_rf_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_rf_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_rf_hist_residuals)
  gridExtra::grid.arrange(ensemble_rf_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble RF" && device == "eps"){
    message(ensemble_rf_pred_vs_actual); ggplot2::ggsave("ensemble_rf_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_resid_vs_actual); ggplot2::ggsave("ensemble_rf_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_hist_residuals); ggplot2::ggsave("ensemble_rf_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_qq); ggplot2::ggsave("ensemble_rf_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble RF" && device == "pdf"){
    message(ensemble_rf_pred_vs_actual); ggplot2::ggsave("ensemble_rf_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_resid_vs_actual); ggplot2::ggsave("ensemble_rf_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_hist_residuals); ggplot2::ggsave("ensemble_rf_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_qq); ggplot2::ggsave("ensemble_rf_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble RF" && device == "jpeg"){
    message(ensemble_rf_pred_vs_actual); ggplot2::ggsave("ensemble_rf_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_resid_vs_actual); ggplot2::ggsave("ensemble_rf_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_hist_residuals); ggplot2::ggsave("ensemble_rf_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_qq); ggplot2::ggsave("ensemble_rf_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble RF" && device == "tiff"){
    message(ensemble_rf_pred_vs_actual); ggplot2::ggsave("ensemble_rf_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_resid_vs_actual); ggplot2::ggsave("ensemble_rf_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_hist_residuals); ggplot2::ggsave("ensemble_rf_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_qq); ggplot2::ggsave("ensemble_rf_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble RF" && device == "png"){
    message(ensemble_rf_pred_vs_actual); ggplot2::ggsave("ensemble_rf_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_resid_vs_actual); ggplot2::ggsave("ensemble_rf_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_hist_residuals); ggplot2::ggsave("ensemble_rf_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_qq); ggplot2::ggsave("ensemble_rf_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble RF" && device == "svg"){
    message(ensemble_rf_pred_vs_actual); ggplot2::ggsave("ensemble_rf_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_resid_vs_actual); ggplot2::ggsave("ensemble_rf_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_hist_residuals); ggplot2::ggsave("ensemble_rf_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rf_qq); ggplot2::ggsave("ensemble_rf_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble_Ridge") {
  grid.arrange(ensemble_ridge_pred_vs_actual, ensemble_ridge_resid_vs_actual, ensemble_ridge_hist_residuals, ensemble_ridge_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_ridge_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_ridge_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_ridge_hist_residuals)
  gridExtra::grid.arrange(ensemble_ridge_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Ridge" && device == "eps"){
    message(ensemble_ridge_pred_vs_actual); ggplot2::ggsave("ensemble_ridge_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_resid_vs_actual); ggplot2::ggsave("ensemble_ridge_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_hist_residuals); ggplot2::ggsave("ensemble_ridge_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_qq); ggplot2::ggsave("ensemble_ridge_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Ridge" && device == "pdf"){
    message(ensemble_ridge_pred_vs_actual); ggplot2::ggsave("ensemble_ridge_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_resid_vs_actual); ggplot2::ggsave("ensemble_ridge_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_hist_residuals); ggplot2::ggsave("ensemble_ridge_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_qq); ggplot2::ggsave("ensemble_ridge_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Ridge" && device == "jpeg"){
    message(ensemble_ridge_pred_vs_actual); ggplot2::ggsave("ensemble_ridge_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_resid_vs_actual); ggplot2::ggsave("ensemble_ridge_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_hist_residuals); ggplot2::ggsave("ensemble_ridge_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_qq); ggplot2::ggsave("ensemble_ridge_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Ridge" && device == "tiff"){
    message(ensemble_ridge_pred_vs_actual); ggplot2::ggsave("ensemble_ridge_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_resid_vs_actual); ggplot2::ggsave("ensemble_ridge_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_hist_residuals); ggplot2::ggsave("ensemble_ridge_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_qq); ggplot2::ggsave("ensemble_ridge_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Ridge" && device == "png"){
    message(ensemble_ridge_pred_vs_actual); ggplot2::ggsave("ensemble_ridge_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_resid_vs_actual); ggplot2::ggsave("ensemble_ridge_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_hist_residuals); ggplot2::ggsave("ensemble_ridge_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_qq); ggplot2::ggsave("ensemble_ridge_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble_Ridge" && device == "svg"){
    message(ensemble_ridge_pred_vs_actual); ggplot2::ggsave("ensemble_ridge_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_resid_vs_actual); ggplot2::ggsave("ensemble_ridge_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_hist_residuals); ggplot2::ggsave("ensemble_ridge_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_ridge_qq); ggplot2::ggsave("ensemble_ridge_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble Rpart") {
  gridExtra::grid.arrange(ensemble_rpart_pred_vs_actual, ensemble_rpart_resid_vs_actual, ensemble_rpart_hist_residuals, ensemble_rpart_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_rpart_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_rpart_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_rpart_hist_residuals)
  gridExtra::grid.arrange(ensemble_rpart_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Rpart" && device == "eps"){
    message(ensemble_rpart_pred_vs_actual); ggplot2::ggsave("ensemble_rpart_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_resid_vs_actual); ggplot2::ggsave("ensemble_rpart_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_hist_residuals); ggplot2::ggsave("ensemble_rpart_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_qq); ggplot2::ggsave("ensemble_rpart_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Rpart" && device == "pdf"){
    message(ensemble_rpart_pred_vs_actual); ggplot2::ggsave("ensemble_rpart_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_resid_vs_actual); ggplot2::ggsave("ensemble_rpart_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_hist_residuals); ggplot2::ggsave("ensemble_rpart_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_qq); ggplot2::ggsave("ensemble_rpart_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Rpart" && device == "jpeg"){
    message(ensemble_rpart_pred_vs_actual); ggplot2::ggsave("ensemble_rpart_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_resid_vs_actual); ggplot2::ggsave("ensemble_rpart_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_hist_residuals); ggplot2::ggsave("ensemble_rpart_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_qq); ggplot2::ggsave("ensemble_rpart_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Rpart" && device == "tiff"){
    message(ensemble_rpart_pred_vs_actual); ggplot2::ggsave("ensemble_rpart_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_resid_vs_actual); ggplot2::ggsave("ensemble_rpart_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_hist_residuals); ggplot2::ggsave("ensemble_rpart_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_qq); ggplot2::ggsave("ensemble_rpart_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Rpart" && device == "png"){
    message(ensemble_rpart_pred_vs_actual); ggplot2::ggsave("ensemble_rpart_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_resid_vs_actual); ggplot2::ggsave("ensemble_rpart_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_hist_residuals); ggplot2::ggsave("ensemble_rpart_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_qq); ggplot2::ggsave("ensemble_rpart_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Rpart" && device == "svg"){
    message(ensemble_rpart_pred_vs_actual); ggplot2::ggsave("ensemble_rpart_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_resid_vs_actual); ggplot2::ggsave("ensemble_rpart_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_hist_residuals); ggplot2::ggsave("ensemble_rpart_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_rpart_qq); ggplot2::ggsave("ensemble_rpart_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble Support Vector Machines") {
  gridExtra::grid.arrange(ensemble_svm_pred_vs_actual, ensemble_svm_resid_vs_actual, ensemble_svm_hist_residuals, ensemble_svm_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_svm_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_svm_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_svm_hist_residuals)
  gridExtra::grid.arrange(ensemble_svm_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Support Vector Machines" && device == "eps"){
    message(ensemble_svm_pred_vs_actual); ggplot2::ggsave("ensemble_svm_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_resid_vs_actual); ggplot2::ggsave("ensemble_svm_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_hist_residuals); ggplot2::ggsave("ensemble_svm_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_qq); ggplot2::ggsave("ensemble_svm_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Support Vector Machines" && device == "pdf"){
    message(ensemble_svm_pred_vs_actual); ggplot2::ggsave("ensemble_svm_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_resid_vs_actual); ggplot2::ggsave("ensemble_svm_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_hist_residuals); ggplot2::ggsave("ensemble_svm_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_qq); ggplot2::ggsave("ensemble_svm_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Support Vector Machines" && device == "jpeg"){
    message(ensemble_svm_pred_vs_actual); ggplot2::ggsave("ensemble_svm_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_resid_vs_actual); ggplot2::ggsave("ensemble_svm_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_hist_residuals); ggplot2::ggsave("ensemble_svm_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_qq); ggplot2::ggsave("ensemble_svm_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Support Vector Machines" && device == "tiff"){
    message(ensemble_svm_pred_vs_actual); ggplot2::ggsave("ensemble_svm_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_resid_vs_actual); ggplot2::ggsave("ensemble_svm_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_hist_residuals); ggplot2::ggsave("ensemble_svm_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_qq); ggplot2::ggsave("ensemble_svm_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Support Vector Machines" && device == "png"){
    message(ensemble_svm_pred_vs_actual); ggplot2::ggsave("ensemble_svm_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_resid_vs_actual); ggplot2::ggsave("ensemble_svm_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_hist_residuals); ggplot2::ggsave("ensemble_svm_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_qq); ggplot2::ggsave("ensemble_svm_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Support Vector Machines" && device == "svg"){
    message(ensemble_svm_pred_vs_actual); ggplot2::ggsave("ensemble_svm_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_resid_vs_actual); ggplot2::ggsave("ensemble_svm_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_hist_residuals); ggplot2::ggsave("ensemble_svm_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_svm_qq); ggplot2::ggsave("ensemble_svm_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble Trees") {
  gridExtra::grid.arrange(ensemble_tree_pred_vs_actual, ensemble_tree_resid_vs_actual, ensemble_tree_hist_residuals, ensemble_tree_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_tree_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_tree_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_tree_hist_residuals)
  gridExtra::grid.arrange(ensemble_tree_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Trees" && device == "eps"){
    message(ensemble_tree_pred_vs_actual); ggplot2::ggsave("ensemble_tree_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_resid_vs_actual); ggplot2::ggsave("ensemble_tree_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_hist_residuals); ggplot2::ggsave("ensemble_tree_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_qq); ggplot2::ggsave("ensemble_tree_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Trees" && device == "pdf"){
    message(ensemble_tree_pred_vs_actual); ggplot2::ggsave("ensemble_tree_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_resid_vs_actual); ggplot2::ggsave("ensemble_tree_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_hist_residuals); ggplot2::ggsave("ensemble_tree_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_qq); ggplot2::ggsave("ensemble_tree_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Trees" && device == "jpeg"){
    message(ensemble_tree_pred_vs_actual); ggplot2::ggsave("ensemble_tree_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_resid_vs_actual); ggplot2::ggsave("ensemble_tree_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_hist_residuals); ggplot2::ggsave("ensemble_tree_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_qq); ggplot2::ggsave("ensemble_tree_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Trees" && device == "tiff"){
    message(ensemble_tree_pred_vs_actual); ggplot2::ggsave("ensemble_tree_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_resid_vs_actual); ggplot2::ggsave("ensemble_tree_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_hist_residuals); ggplot2::ggsave("ensemble_tree_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_qq); ggplot2::ggsave("ensemble_tree_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Trees" && device == "png"){
    message(ensemble_tree_pred_vs_actual); ggplot2::ggsave("ensemble_tree_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_resid_vs_actual); ggplot2::ggsave("ensemble_tree_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_hist_residuals); ggplot2::ggsave("ensemble_tree_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_qq); ggplot2::ggsave("ensemble_tree_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble Trees" && device == "svg"){
    message(ensemble_tree_pred_vs_actual); ggplot2::ggsave("ensemble_tree_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_resid_vs_actual); ggplot2::ggsave("ensemble_tree_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_hist_residuals); ggplot2::ggsave("ensemble_tree_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_tree_qq); ggplot2::ggsave("ensemble_tree_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

if (data_visualizations[1] == "Ensemble XGBoost") {
  gridExtra::grid.arrange(ensemble_xgb_pred_vs_actual, ensemble_xgb_resid_vs_actual, ensemble_xgb_hist_residuals, ensemble_xgb_qq, ncol = 2)
  gridExtra::grid.arrange(ensemble_xgb_pred_vs_actual)
  gridExtra::grid.arrange(ensemble_xgb_resid_vs_actual)
  gridExtra::grid.arrange(ensemble_xgb_hist_residuals)
  gridExtra::grid.arrange(ensemble_xgb_qq)
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble XGBoost" && device == "eps"){
    message(ensemble_xgb_pred_vs_actual); ggplot2::ggsave("ensemble_xgb_pred_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_resid_vs_actual); ggplot2::ggsave("ensemble_xgb_resid_vs_actual.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_hist_residuals); ggplot2::ggsave("ensemble_xgb_hist_residuals.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_qq); ggplot2::ggsave("ensemble_xgb_qq.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble XGBoost" && device == "pdf"){
    message(ensemble_xgb_pred_vs_actual); ggplot2::ggsave("ensemble_xgb_pred_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_resid_vs_actual); ggplot2::ggsave("ensemble_xgb_resid_vs_actual.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_hist_residuals); ggplot2::ggsave("ensemble_xgb_hist_residuals.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_qq); ggplot2::ggsave("ensemble_xgb_qq.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble XGBoost" && device == "jpeg"){
    message(ensemble_xgb_pred_vs_actual); ggplot2::ggsave("ensemble_xgb_pred_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_resid_vs_actual); ggplot2::ggsave("ensemble_xgb_resid_vs_actual.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_hist_residuals); ggplot2::ggsave("ensemble_xgb_hist_residuals.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_qq); ggplot2::ggsave("ensemble_xgb_qq.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble XGBoost" && device == "tiff"){
    message(ensemble_xgb_pred_vs_actual); ggplot2::ggsave("ensemble_xgb_pred_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_resid_vs_actual); ggplot2::ggsave("ensemble_xgb_resid_vs_actual.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_hist_residuals); ggplot2::ggsave("ensemble_xgb_hist_residuals.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_qq); ggplot2::ggsave("ensemble_xgb_qq.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble XGBoost" && device == "png"){
    message(ensemble_xgb_pred_vs_actual); ggplot2::ggsave("ensemble_xgb_pred_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_resid_vs_actual); ggplot2::ggsave("ensemble_xgb_resid_vs_actual.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_hist_residuals); ggplot2::ggsave("ensemble_xgb_hist_residuals.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_qq); ggplot2::ggsave("ensemble_xgb_qq.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
  if(save_all_plots == "Y" && data_visualizations[1] == "Ensemble XGBoost" && device == "svg"){
    message(ensemble_xgb_pred_vs_actual); ggplot2::ggsave("ensemble_xgb_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_resid_vs_actual); ggplot2::ggsave("ensemble_xgb_resid_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_hist_residuals); ggplot2::ggsave("ensemble_xgb_hist_residuals.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    message(ensemble_xgb_qq); ggplot2::ggsave("ensemble_xgb_qq.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
  }
}

accuracy_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" = c(
      rep("Bagged Random Forest", numresamples), c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
      c(rep("BayesRNN", numresamples)), c(rep("Boost Random Forest", numresamples)),
      c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
      c(rep("Gradient Boosted", numresamples)), c(rep("K-Nearest Neighbors", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
      c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
      c(rep("Random Forest", numresamples)), c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
      c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
      c(rep("Ensemble Bagged Random Forest", numresamples)), c(rep("Ensemble Bagging", numresamples)),
      c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
      c(rep("Ensemble Boosted Random Forest", numresamples)), c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
      c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble K-Nearest Neighbors", numresamples)), c(rep("Ensemble Lasso", numresamples)),
      c(rep("Ensemble Linear", numresamples)), c(rep("Ensemble Random Forest", numresamples)), c(rep("Ensemble Ridge", numresamples)),
      c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
      c(rep("Ensemble Trees", numresamples)), c(rep("Ensemble XGBoost", numresamples))
    ),
    "data" = c(
      bag_rf_holdout_RMSE, bagging_holdout_RMSE, bayesglm_holdout_RMSE,
      bayesrnn_holdout_RMSE, boost_rf_holdout_RMSE,
      cubist_holdout_RMSE, earth_holdout_RMSE, elastic_holdout_RMSE_df$elastic_holdout_RMSE[2:nrow(elastic_holdout_RMSE_df)], gam_holdout_RMSE,
      gb_holdout_RMSE, knn_holdout_RMSE, lasso_holdout_RMSE_df$lasso_holdout_RMSE[2:nrow(lasso_holdout_RMSE_df)], linear_holdout_RMSE,
      neuralnet_holdout_RMSE, pcr_holdout_RMSE, pls_holdout_RMSE,
      rf_holdout_RMSE, ridge_holdout_RMSE_df$ridge_holdout_RMSE[2:nrow(ridge_holdout_RMSE_df)], rpart_holdout_RMSE,
      svm_holdout_RMSE, tree_holdout_RMSE, xgb_holdout_RMSE,
      ensemble_bag_rf_holdout_RMSE, ensemble_bagging_holdout_RMSE,
      ensemble_bayesglm_holdout_RMSE, ensemble_bayesrnn_holdout_RMSE,
      ensemble_boost_rf_holdout_RMSE, ensemble_cubist_holdout_RMSE, ensemble_earth_holdout_RMSE, ensemble_elastic_holdout_RMSE,
      ensemble_gb_holdout_RMSE, ensemble_knn_holdout_RMSE, ensemble_lasso_holdout_RMSE,
      ensemble_linear_holdout_RMSE, ensemble_rf_holdout_RMSE, ensemble_ridge_holdout_RMSE,
      ensemble_rpart_holdout_RMSE, ensemble_svm_holdout_RMSE,
      ensemble_tree_holdout_RMSE, ensemble_xgb_holdout_RMSE
    ),
    "mean" = rep(c(
      bag_rf_holdout_RMSE_mean, bagging_holdout_RMSE_mean, bayesglm_holdout_RMSE_mean,
      bayesrnn_holdout_RMSE_mean, boost_rf_test_RMSE_mean,
      cubist_holdout_RMSE_mean, earth_holdout_RMSE_mean, elastic_holdout_RMSE_mean, gam_holdout_RMSE_mean,
      gb_holdout_RMSE_mean, knn_holdout_RMSE_mean, lasso_holdout_RMSE_mean, linear_holdout_RMSE_mean,
      neuralnet_holdout_RMSE_mean, pcr_holdout_RMSE_mean, pls_holdout_RMSE_mean,
      rf_holdout_RMSE_mean, ridge_holdout_RMSE_mean, rpart_holdout_RMSE_mean,
      svm_holdout_RMSE_mean, tree_holdout_RMSE_mean, xgb_holdout_RMSE_mean,
      ensemble_bag_rf_holdout_RMSE_mean, ensemble_bagging_holdout_RMSE_mean,
      ensemble_bayesglm_holdout_RMSE_mean, ensemble_bayesrnn_holdout_RMSE_mean,
      ensemble_boost_rf_holdout_RMSE_mean, ensemble_cubist_holdout_RMSE_mean, ensemble_earth_holdout_RMSE_mean, ensemble_elastic_holdout_RMSE_mean,
      ensemble_gb_holdout_RMSE_mean, ensemble_knn_holdout_RMSE_mean, ensemble_lasso_holdout_RMSE_mean, ensemble_linear_holdout_RMSE_mean,
      ensemble_rf_holdout_RMSE_mean, ensemble_ridge_holdout_RMSE_mean,
      ensemble_rpart_holdout_RMSE_mean, ensemble_svm_holdout_RMSE_mean,
      ensemble_tree_holdout_RMSE_mean, ensemble_xgb_holdout_RMSE_mean
    ), each = numresamples)
  )

accuracy_plot <- ggplot2::ggplot(data = accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("Accuracy data (RMSE), fixed scales\nRoot Mean Squared Error by model, lower is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "Root Mean Squared Error (RMSE), lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
accuracy_plot2 <- ggplot2::ggplot(data = accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Accuracy data (RMSE), free scales\nRoot Mean Squared Error by model, lower is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "Root Mean Squared Error (RMSE), lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
total_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" =
      c(
        rep("Bagged Random Forest", numresamples), c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
        c(rep("BayesRNN", numresamples)), c(rep("Boost Random Forest", numresamples)),
        c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
        c(rep("Gradient Boosted", numresamples)), c(rep("K-Nearest Neighbors", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
        c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
        c(rep("Random Forest", numresamples)), c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
        c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
        c(rep("Ensemble Bagged Random Forest", numresamples)), c(rep("Ensemble Bagging", numresamples)),
        c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
        c(rep("Ensemble Boosted Random Forest", numresamples)), c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
        c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble K-Nearest Neighbors", numresamples)), c(rep("Ensemble Lasso", numresamples)),
        c(rep("Ensemble Linear", numresamples)), c(rep("Ensemble Random Forest", numresamples)), c(rep("Ensemble Ridge", numresamples)),
        c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
        c(rep("Ensemble Trees", numresamples)), c(rep("Ensemble XGBoost", numresamples))
      ),
    "train" = c(
      bag_rf_train_RMSE, bagging_train_RMSE, bayesglm_train_RMSE,
      bayesrnn_train_RMSE, boost_rf_train_RMSE,
      cubist_train_RMSE, earth_train_RMSE, elastic_train_RMSE_df$elastic_train_RMSE[2:nrow(elastic_train_RMSE_df)], gam_train_RMSE,
      gb_train_RMSE, knn_train_RMSE, lasso_train_RMSE_df$lasso_train_RMSE[2:nrow(lasso_train_RMSE_df)], linear_train_RMSE,
      neuralnet_train_RMSE, pcr_train_RMSE, pls_train_RMSE,
      rf_train_RMSE, ridge_train_RMSE_df$ridge_train_RMSE[2:nrow(ridge_train_RMSE_df)], rpart_train_RMSE,
      svm_train_RMSE, tree_train_RMSE, xgb_train_RMSE,
      ensemble_bag_rf_train_RMSE, ensemble_bagging_train_RMSE,
      ensemble_bayesglm_train_RMSE, ensemble_bayesrnn_train_RMSE,
      ensemble_boost_rf_train_RMSE, ensemble_cubist_train_RMSE, ensemble_earth_train_RMSE,
      ensemble_elastic_train_RMSE_df$ensemble_elastic_train_RMSE[2:nrow(ensemble_elastic_train_RMSE_df)],
      ensemble_gb_train_RMSE, ensemble_knn_train_RMSE_df$ensemble_knn_train_RMSE[2:nrow(ensemble_knn_train_RMSE_df)],
      ensemble_lasso_train_RMSE_df$ensemble_lasso_train_RMSE[2:nrow(ensemble_lasso_train_RMSE_df)],
      ensemble_linear_train_RMSE, ensemble_rf_train_RMSE,
      ensemble_ridge_train_RMSE_df$ensemble_ridge_train_RMSE[2:nrow(ensemble_ridge_train_RMSE_df)],
      ensemble_rpart_train_RMSE, ensemble_svm_train_RMSE,
      ensemble_tree_train_RMSE, ensemble_xgb_train_RMSE
    ),

    "holdout" = c(
      bag_rf_holdout_RMSE, bagging_holdout_RMSE, bayesglm_holdout_RMSE,
      bayesrnn_holdout_RMSE, boost_rf_holdout_RMSE,
      cubist_holdout_RMSE, earth_holdout_RMSE, elastic_holdout_RMSE_df$elastic_holdout_RMSE[2:nrow(elastic_holdout_RMSE_df)], gam_holdout_RMSE,
      gb_holdout_RMSE, knn_holdout_RMSE, lasso_holdout_RMSE_df$lasso_holdout_RMSE[2:nrow(lasso_holdout_RMSE_df)], linear_holdout_RMSE,
      neuralnet_holdout_RMSE, pcr_holdout_RMSE, pls_holdout_RMSE,
      rf_holdout_RMSE, ridge_holdout_RMSE_df$ridge_holdout_RMSE[2:nrow(ridge_holdout_RMSE_df)], rpart_holdout_RMSE,
      svm_holdout_RMSE, tree_holdout_RMSE, xgb_holdout_RMSE,
      ensemble_bag_rf_holdout_RMSE, ensemble_bagging_holdout_RMSE,
      ensemble_bayesglm_holdout_RMSE, ensemble_bayesrnn_holdout_RMSE,
      ensemble_boost_rf_holdout_RMSE, ensemble_cubist_holdout_RMSE, ensemble_earth_holdout_RMSE, ensemble_elastic_holdout_RMSE,
      ensemble_gb_holdout_RMSE, ensemble_knn_holdout_RMSE, ensemble_lasso_holdout_RMSE,
      ensemble_linear_holdout_RMSE, ensemble_rf_holdout_RMSE, ensemble_ridge_holdout_RMSE,
      ensemble_rpart_holdout_RMSE, ensemble_svm_holdout_RMSE,
      ensemble_tree_holdout_RMSE, ensemble_xgb_holdout_RMSE
    )
  )

total_plot <- ggplot2::ggplot(data = total_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = train, color = "train")) +
  ggplot2::geom_point(mapping = aes(x = count, y = train)) +
  ggplot2::geom_line(mapping = aes(x = count, y = holdout, color = "holdout")) +
  ggplot2::geom_point(mapping = aes(x = count, y = holdout)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "optimal")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Train vs holdout data by resample and model. Free scales \nRoot Mean Squared Error by model, lower is better. \nThe black horizontal line is 0.") +
  ggplot2::labs(y = "Root Mean Squared Error (RMSE), lower is better \nthe black line is 0.\n") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("holdout", "train"),
    values = c(
      "train" = "blue", "holdout" = "red")
  )
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("total_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("total_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("total_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("total_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("total_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("total_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
total_plot2 <- ggplot2::ggplot(data = total_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = train, color = "train")) +
  ggplot2::geom_point(mapping = aes(x = count, y = train)) +
  ggplot2::geom_line(mapping = aes(x = count, y = holdout, color = "holdout")) +
  ggplot2::geom_point(mapping = aes(x = count, y = holdout)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "optimal")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("Train vs holdout data by resample and model. Fixed scales \nRoot Mean Squared Error by model, lower is better. \nThe black horizontal line is 0.") +
  ggplot2::labs(y = "Root Mean Squared Error (RMSE), lower is better \nthe black line is 0.\n") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("holdout", "train"),
    values = c(
      "train" = "blue", "holdout" = "red")
  )
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("total_plot22.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("total_plot22.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("total_plot22.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("total_plot22.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("total_plot22.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("total_plot22.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#########################################################################

###### Start making predictions on new data here ########################

#########################################################################

if (predict_on_new_data == "Y") {
  new_bag_rf <- predict(object = bag_rf_train_fit$best.model, newdata = new_data)
  new_bagging <- predict(object = bagging_train_fit, newdata = new_data)
  new_bayesglm <- predict(object = bayesglm_train_fit, newdata = new_data)
  new_bayesrnn <- predict(object = bayesrnn_train_fit, newdata = new_data)
  new_boost_rf <- predict(object = boost_rf_train_fit$best.model, newdata = new_data)
  new_cubist <- predict(object = cubist_train_fit, newdata = new_data)
  new_earth <- as.numeric(predict(object = earth_train_fit, newdata = new_data))
  new_elastic <- rowMeans(predict(object = best_elastic_model, newdata = new_data, newx = as.matrix(new_data[, 1:ncol(new_data) - 1])))
  new_gam <- predict(object = gam_train_fit, newdata = new_data)
  new_gb <- predict(object = gb_train_fit, newdata = new_data)
  new_knn <- predict(object = knn_train_fit$best.model, newdata = new_data[, 1:ncol(new_data) - 1], k = knn_train_fit$best_model$k)
  new_lasso <- rowMeans(predict(object = best_lasso_model, newdata = new_data, newx = as.matrix(new_data[, 1:ncol(new_data) - 1])))
  new_linear <- predict(object = linear_train_fit$best.model, newdata = new_data)
  new_neuralnet <- predict(object = neuralnet_train_fit, newdata = new_data)
  new_pls <- predict(object = pls_train_fit, newdata = new_data)[, , 1]
  new_pcr <- predict(object = pcr_train_fit, newdata = new_data)[, , 1]
  new_rf <- predict(object = rf_train_fit$best.model, newdata = new_data)
  new_ridge <- rowMeans(predict(object = best_ridge_model, newdata = new_data, newx = as.matrix(new_data[, 1:ncol(new_data) - 1])))
  new_rpart <- predict(object = rpart_train_fit, newdata = new_data)
  new_svm <- predict(object = svm_train_fit$best.model, k = svm_train_fit$best_model$k, newdata = new_data)
  new_tree <- predict(object = tree_train_fit, newdata = new_data)

  # XGBoost
  # split into training and testing set
  new_train <- train
  new_test <- new_data

  # define predictor and response variables in training set
  new_train_x <- data.matrix(new_train[, -ncol(new_train)])
  new_train_y <- new_test[, ncol(new_test)]

  # define predictor and response variables in testing set
  new_test_x <- data.matrix(new_test[, -ncol(new_test)])
  new_test_y <- new_test[, ncol(new_test)]

  new_xgb_test <- xgboost::xgb.DMatrix(data = test_x, label = test_y)

  new_watchlist_test <- list(train = xgb_train, test = xgb_test)

  new_xgb_model <- xgboost::xgb.train(data = new_xgb_test, max.depth = 3, watchlist = new_watchlist_test, nrounds = 70)

  new_XGBoost <- predict(object = new_xgb_model, newdata = new_test_x)

  new_ensemble <- data.frame(
    "BagRF" = new_bag_rf / bag_rf_holdout_RMSE_mean,
    "Bagging" = new_bagging / bagging_holdout_RMSE_mean,
    "BayesGLM" = new_bayesglm / bayesglm_holdout_RMSE_mean,
    "BayesRNN" = new_bayesrnn / bayesrnn_holdout_RMSE_mean,
    "BoostRF" = new_boost_rf / boost_rf_holdout_RMSE_mean,
    "Cubist" = new_cubist / cubist_holdout_RMSE_mean,
    "Earth" = new_earth / earth_holdout_RMSE_mean,
    "Elastic" = new_elastic / elastic_holdout_RMSE_mean,
    "GAM" = new_gam / gam_holdout_RMSE_mean,
    "GBM" = new_gb / gb_holdout_RMSE_mean,
    "KNN" = new_knn / knn_holdout_RMSE_mean,
    "Lasso" = new_lasso / lasso_holdout_RMSE_mean,
    "Linear" = new_linear / linear_holdout_RMSE_mean,
    "Neuralnet" = new_pcr / pcr_holdout_RMSE_mean,
    "PCR" = new_pcr / pcr_holdout_RMSE_mean,
    "PLS" = new_pls / pls_holdout_RMSE_mean,
    "RandomForest" = new_rf / rf_holdout_RMSE_mean,
    "Ridge" = new_ridge / ridge_holdout_RMSE_mean,
    "Rpart" = new_rpart / rpart_holdout_RMSE_mean,
    "SVM" = new_svm / svm_holdout_RMSE_mean,
    "Tree" = new_tree / tree_holdout_RMSE_mean,
    "XGBoost" = new_XGBoost / xgb_holdout_RMSE_mean
  )

  new_ensemble$Row_mean <- rowMeans(new_ensemble)
  new_ensemble$y_ensemble <- new_data$y

  thing1 <- colnames(new_ensemble)

  new_ensemble <- dplyr::select(new_ensemble, thing1)

  new_ensemble_bag_rf <- predict(object = ensemble_bag_rf_train_fit$best.model, newdata = new_ensemble, mtry = ncol(new_ensemble) - 1)
  new_ensemble_bagging <- predict(object = ensemble_bagging_train_fit, newdata = new_ensemble)
  new_ensemble_bayesglm <- predict(object = ensemble_bayesglm_train_fit, newdata = new_ensemble)
  new_ensemble_bayesrnn <- predict(object = ensemble_bayesrnn_train_fit, newdata = new_ensemble)
  new_ensemble_boost_rf <- predict(object = ensemble_boost_rf_train_fit$best.model, newdata = new_ensemble)
  new_ensemble_cart <- predict(object = ensemble_rpart_train_fit, newdata = new_ensemble)
  new_ensemble_cubist <- predict(object = ensemble_cubist_train_fit, newdata = new_ensemble)
  new_ensemble_earth <- predict(object = ensemble_earth_train_fit, newdata = new_ensemble)
  new_ensemble_elastic <- rowMeans(predict(object = ensemble_elastic_model, newx = data.matrix(new_ensemble %>% dplyr::select(-y_ensemble))))
  new_ensemble_gb <- predict(object = ensemble_gb_train_fit, newdata = new_ensemble)
  new_ensemble_knn <- predict(object = ensemble_knn_model$best.model, newdata = new_ensemble)
  new_ensemble_lasso <- rowMeans(predict(object = ensemble_lasso_model, newx = data.matrix(new_ensemble %>% dplyr::select(-y_ensemble))))
  new_ensemble_linear <- predict(object = ensemble_linear_train_fit$best.model, newdata = new_ensemble)
  new_ensemble_rf <- predict(object = ensemble_rf_train_fit$best.model, newdata = new_ensemble)
  new_ensemble_rpart <- predict(object = ensemble_rpart_train_fit, newdata = new_ensemble)
  new_ensemble_ridge <- rowMeans(predict(object = ensemble_ridge_model, newx = data.matrix(new_ensemble %>% dplyr::select(-y_ensemble))))
  new_ensemble_svm <- predict(object = ensemble_svm_train_fit$best.model, newdata = new_ensemble)
  new_ensemble_tree <- predict(object = ensemble_tree_train_fit, newdata = new_ensemble)

  # XGBoost
  # split into training and testing set
  new_train <- ensemble_train
  new_test <- new_data

  # define predictor and response variables in training set
  new_train_x <- data.matrix(new_train[, -ncol(new_test)])
  new_train_y <- new_test[, ncol(new_test)]

  # define predictor and response variables in testing set
  new_test_x <- data.matrix(new_test[, -ncol(new_test)])
  new_test_y <- new_test[, ncol(new_test)]

  new_xgb_test <- xgboost::xgb.DMatrix(data = new_test_x, label = new_test_y)

  new_watchlist_test <- list(train = xgb_train, test = xgb_test)

  new_xgb_model <- xgboost::xgb.train(data = new_xgb_test, max.depth = 3, watchlist = new_watchlist_test, nrounds = 70)

  new_ensemble_xgboost <- predict(object = new_xgb_model, newdata = new_test_x)


  new_data_results <-
    data.frame(
      "True_Value" = new_ensemble$y_ensemble,
      "BagRF" = round(new_bag_rf, 4),
      "Bagging" = round(new_bagging, 4),
      "BayesGLM" = round(new_bayesglm, 4),
      "BayesRNN" = round(new_bayesrnn, 4),
      "BoostRF" = round(new_boost_rf, 4),
      "Cubist" = round(new_cubist, 4),
      "Earth" = round(new_earth, 4),
      "Elastic" = round(new_elastic, 4),
      "GAM" = round(new_gam, 4),
      "GBM" = round(new_gb, 4),
      "KNN" = round(new_knn, 4),
      "Lasso" = round(new_lasso, 4),
      "Linear" = round(new_linear, 4),
      "Neuralnet" = round(new_neuralnet, 4),
      "PLS" = round(new_pls, 4),
      "PCR" = round(new_pcr, 4),
      "RandomForest" = round(new_rf, 4),
      "Ridge" = round(new_ridge, 4),
      "Rpart" = round(new_rpart, 4),
      "SVM" = round(new_svm, 4),
      "Tree" = round(new_tree, 4),
      "XGBoost" = round(new_XGBoost, 4),
      "Ensemble_BagRF" = round(new_ensemble_bag_rf, 4),
      "Ensemble_Bagging" = round(new_ensemble_bagging, 4),
      "Ensemble_BayesGLM" = round(new_ensemble_bayesglm, 4),
      "Ensemble_BayesRNN" = round(new_ensemble_bayesrnn, 4),
      "Ensemble_BoostRF" = round(new_ensemble_boost_rf, 4),
      "Ensemble_Cart" = round(new_ensemble_cart, 4),
      "Ensemble_Cubist" = round(new_ensemble_cubist, 4),
      "Ensemble_Earth" = as.numeric(round(new_ensemble_earth, 4)),
      "Ensemble_Elastic" = round(new_ensemble_elastic, 4),
      "Ensemble_Gardient_Boosted" = round(new_ensemble_gb, 4),
      "Ensemble_Lasso" = round(new_ensemble_lasso, 4),
      "Ensemble_Linear" = round(new_ensemble_linear, 4),
      "Ensemble_RandomForest" = round(new_ensemble_rf, 4),
      "Ensemble_RPart" = round(new_ensemble_rpart, 4),
      "Ensemble_SVM" = round(new_ensemble_svm, 4),
      "Ensemble_Tree" = round(new_ensemble_tree, 4),
      "Ensemble_XGBoost" = round(new_ensemble_xgboost, 4)
    )

  df1 <- t(new_data_results)

  predictions_of_new_data <- reactable::reactable(
    data = df1, searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
    striped = TRUE, highlight = TRUE, resizable = TRUE
  ) %>%
    reactablefmtr::add_title("Predictions of new data")


  if (save_all_trained_models == "Y") {
    tempdir1 <- tempdir()
    fil <- tempfile("bagging_train_fit", fileext = ".RDS")
    bagging_train_fit <- saveRDS(bagging_train_fit, fil)

    fil <- tempfile("bayesglm_train_fit", fileext = ".RDS")
    saveRDS(bayesglm_train_fit, fil)

    fil <- tempfile("bayesrnn_train_fit", fileext = ".RDS")
    saveRDS(bayesrnn_train_fit, fil)

    fil <- tempfile("boost_rf_train_fit", fileext = ".RDS")
    saveRDS(boost_rf_train_fit, fil)

    fil <- tempfile("cubist_train_fit", fileext = ".RDS")
    saveRDS(cubist_train_fit, fil)

    fil <- tempfile("earth_train_fit", fileext = ".RDS")
    saveRDS(earth_train_fit, fil)

    fil <- tempfile("best_elastic_model", fileext = ".RDS")
    saveRDS(best_elastic_model, fil)

    fil <- tempfile("gam_train_fit", fileext = ".RDS")
    saveRDS(gam_train_fit, fil)

    fil <- tempfile("gb_train_fit", fileext = ".RDS")
    saveRDS(gb_train_fit, fil)

    fil <- tempfile("knn_train_fit", fileext = ".RDS")
    saveRDS(knn_train_fit, fil)

    fil <- tempfile("best_lasso_model", fileext = ".RDS")
    saveRDS(best_lasso_model, fil)

    fil <- tempfile("linear_train_fit", fileext = ".RDS")
    saveRDS(linear_train_fit, fil)

    fil <- tempfile("neuralnet_train_fit", fileext = ".RDS")
    saveRDS(neuralnet_train_fit, fil)

    fil <- tempfile("pls_train_fit", fileext = ".RDS")
    saveRDS(pls_train_fit, fil)

    fil <- tempfile("pcr_train_fit", fileext = ".RDS")
    saveRDS(pcr_train_fit, fil)

    fil <- tempfile("rf_train_fit", fileext = ".RDS")
    saveRDS(rf_train_fit, fil)

    fil <- tempfile("best_ridge_model", fileext = ".RDS")
    saveRDS(best_ridge_model, fil)

    fil <- tempfile("rpart_train_fit", fileext = ".RDS")
    saveRDS(rpart_train_fit, fil)

    fil <- tempfile("svm_train_fit", fileext = ".RDS")
    saveRDS(svm_train_fit, fil)

    fil <- tempfile("tree_train_fit", fileext = ".RDS")
    saveRDS(tree_train_fit, fil)

    fil <- tempfile("xgb_model", fileext = ".RDS")
    saveRDS(xgb_model, fil)

    fil <- tempfile("ensemble_bag_rf_train_fit", fileext = ".RDS")
    saveRDS(ensemble_bag_rf_train_fit, fil)

    fil <- tempfile("ensemble_bagging_train_fit", fileext = ".RDS")
    saveRDS(ensemble_bagging_train_fit, fil)

    fil <- tempfile("ensemble_bayesglm_train_fit", fileext = ".RDS")
    saveRDS(ensemble_bayesglm_train_fit, fil)

    fil <- tempfile("ensemble_bayesrnn_train_fit", fileext = ".RDS")
    saveRDS(ensemble_bayesrnn_train_fit, fil)

    fil <- tempfile("ensemble_boost_rf_train_fit", fileext = ".RDS")
    saveRDS(ensemble_boost_rf_train_fit, fil)

    fil <- tempfile("ensemble_cubist_train_fit", fileext = ".RDS")
    saveRDS(ensemble_cubist_train_fit, fil)

    fil <- tempfile("ensemble_earth_train_fit", fileext = ".RDS")
    saveRDS(ensemble_earth_train_fit, fil)

    fil <- tempfile("ensemble_best_elastic_model", fileext = ".RDS")
    saveRDS(ensemble_best_elastic_model, fil)

    fil <- tempfile("ensemble_gb_train_fit", fileext = ".RDS")
    saveRDS(ensemble_gb_train_fit, fil)

    fil <- tempfile("ensemble_knn_model", fileext = ".RDS")
    saveRDS(ensemble_knn_model, fil)

    fil <- tempfile("ensemble_best_lasso_model", fileext = ".RDS")
    saveRDS(ensemble_best_lasso_model, fil)

    fil <- tempfile("ensemble_linear_train_fit", fileext = ".RDS")
    saveRDS(ensemble_linear_train_fit, fil)

    fil <- tempfile("ensemble_rf_fit", fileext = ".RDS")
    saveRDS(ensemble_rf_train_fit, fil)

    fil <- tempfile("ensemble_bagging_fit", fileext = ".RDS")
    saveRDS(ensemble_bagging_train_fit, fil)

    fil <- tempfile("ensemble_best_ridge_model", fileext = ".RDS")
    saveRDS(ensemble_best_ridge_model, fil)

    fil <- tempfile("ensemble_rpart_train_fit", fileext = ".RDS")
    saveRDS(ensemble_rpart_train_fit, fil)

    fil <- tempfile("ensemble_svm_train_fit", fileext = ".RDS")
    saveRDS(ensemble_svm_train_fit, fil)

    fil <- tempfile("ensemble_tree_train_fit", fileext = ".RDS")
    saveRDS(ensemble_tree_train_fit, fil)

    fil <- tempfile("ensemble_xgb_model", fileext = ".RDS")
    saveRDS(ensemble_xgb_model, fil)
  }

  message('The trained models are temporariliy saved in this directory: tempdir1. This directory is automatically deleted at the end of the R session.
            You may save the trained models before you end this session if you chose to do so.')



 return(list(
    "head_of_data" = head_df, "accuracy_plot" = accuracy_plot, "accuracy_plot_free_scales" = accuracy_plot2, "holdout_vs_train_plot" = holdout_vs_train_plot, "holdout_vs_train_plot_2" = holdout_vs_train_plot2,
    "histograms" = histograms, "boxplots" = boxplots, "predictor_vs_target" = predictor_vs_target,
    "final_results_table" = final_results, "data_correlation" = data_correlation, "data_summary" = data_summary, "head_of_ensemble" = head_ensemble, "ensemble_correlation" = ensemble_correlation,
    "accuracy_barchart" = accuracy_barchart, "train_vs_holdout" = total_plot, "duration_barchart" = duration_barchart, "holdout_vs_train_barchart" = holdout_vs_train_barchart,
    "bias_barchart" = bias_barchart, "MSE_barchart" = MSE_barchart, "MAE_barchart" = MAE_barchart, "SSE_barchart" = SSE_barchart,
    "bias_plot" = bias_plot, "MSE_plot" = MSE_plot, "MAE_plot" = MAE_plot, "SSE_plot" = SSE_plot, "Kolmogorov-Smirnov test p-score" = k_s_test_barchart,
    "colnum" = colnum, "numresamples" = numresamples, "predict_on_new_data" = predictions_of_new_data, "save_all_trained_models" = save_all_trained_models,
    "how_to_handle_strings" = how_to_handle_strings, "data_reduction_method" = data_reduction_method, 'VIF' = VIF, "scale_data" = scale_all_predictors_in_data,
    "train_amount" = train_amount, "test_amount" = test_amount, "validation_amount" = validation_amount
  )
 )
}

if (save_all_trained_models == "Y") {
  tempdir1 <- tempdir()
  fil <- tempfile("bagging_train_fit", fileext = ".RDS")
  bagging_train_fit <- saveRDS(bagging_train_fit, fil)

  fil <- tempfile("bayesglm_train_fit", fileext = ".RDS")
  saveRDS(bayesglm_train_fit, fil)

  fil <- tempfile("bayesrnn_train_fit", fileext = ".RDS")
  saveRDS(bayesrnn_train_fit, fil)

  fil <- tempfile("boost_rf_train_fit", fileext = ".RDS")
  saveRDS(boost_rf_train_fit, fil)

  fil <- tempfile("cubist_train_fit", fileext = ".RDS")
  saveRDS(cubist_train_fit, fil)

  fil <- tempfile("earth_train_fit", fileext = ".RDS")
  saveRDS(earth_train_fit, fil)

  fil <- tempfile("best_elastic_model", fileext = ".RDS")
  saveRDS(best_elastic_model, fil)

  fil <- tempfile("gam_train_fit", fileext = ".RDS")
  saveRDS(gam_train_fit, fil)

  fil <- tempfile("gb_train_fit", fileext = ".RDS")
  saveRDS(gb_train_fit, fil)

  fil <- tempfile("knn_train_fit", fileext = ".RDS")
  saveRDS(knn_train_fit, fil)

  fil <- tempfile("best_lasso_model", fileext = ".RDS")
  saveRDS(best_lasso_model, fil)

  fil <- tempfile("linear_train_fit", fileext = ".RDS")
  saveRDS(linear_train_fit, fil)

  fil <- tempfile("neuralnet_train_fit", fileext = ".RDS")
  saveRDS(neuralnet_train_fit, fil)

  fil <- tempfile("pls_train_fit", fileext = ".RDS")
  saveRDS(pls_train_fit, fil)

  fil <- tempfile("pcr_train_fit", fileext = ".RDS")
  saveRDS(pcr_train_fit, fil)

  fil <- tempfile("rf_train_fit", fileext = ".RDS")
  saveRDS(rf_train_fit, fil)

  fil <- tempfile("best_ridge_model", fileext = ".RDS")
  saveRDS(best_ridge_model, fil)

  fil <- tempfile("rpart_train_fit", fileext = ".RDS")
  saveRDS(rpart_train_fit, fil)

  fil <- tempfile("svm_train_fit", fileext = ".RDS")
  saveRDS(svm_train_fit, fil)

  fil <- tempfile("tree_train_fit", fileext = ".RDS")
  saveRDS(tree_train_fit, fil)

  fil <- tempfile("xgb_model", fileext = ".RDS")
  saveRDS(xgb_model, fil)

  fil <- tempfile("ensemble_bag_rf_train_fit", fileext = ".RDS")
  saveRDS(ensemble_bag_rf_train_fit, fil)

  fil <- tempfile("ensemble_bagging_train_fit", fileext = ".RDS")
  saveRDS(ensemble_bagging_train_fit, fil)

  fil <- tempfile("ensemble_bayesglm_train_fit", fileext = ".RDS")
  saveRDS(ensemble_bayesglm_train_fit, fil)

  fil <- tempfile("ensemble_bayesrnn_train_fit", fileext = ".RDS")
  saveRDS(ensemble_bayesrnn_train_fit, fil)

  fil <- tempfile("ensemble_boost_rf_train_fit", fileext = ".RDS")
  saveRDS(ensemble_boost_rf_train_fit, fil)

  fil <- tempfile("ensemble_cubist_train_fit", fileext = ".RDS")
  saveRDS(ensemble_cubist_train_fit, fil)

  fil <- tempfile("ensemble_earth_train_fit", fileext = ".RDS")
  saveRDS(ensemble_earth_train_fit, fil)

  fil <- tempfile("ensemble_best_elastic_model", fileext = ".RDS")
  saveRDS(ensemble_best_elastic_model, fil)

  fil <- tempfile("ensemble_gb_train_fit", fileext = ".RDS")
  saveRDS(ensemble_gb_train_fit, fil)

  fil <- tempfile("ensemble_knn_model", fileext = ".RDS")
  saveRDS(ensemble_knn_model, fil)

  fil <- tempfile("ensemble_best_lasso_model", fileext = ".RDS")
  saveRDS(ensemble_best_lasso_model, fil)

  fil <- tempfile("ensemble_linear_train_fit", fileext = ".RDS")
  saveRDS(ensemble_linear_train_fit, fil)

  fil <- tempfile("ensemble_rf_fit", fileext = ".RDS")
  saveRDS(ensemble_rf_train_fit, fil)

  fil <- tempfile("ensemble_bagging_fit", fileext = ".RDS")
  saveRDS(ensemble_bagging_train_fit, fil)

  fil <- tempfile("ensemble_best_ridge_model", fileext = ".RDS")
  saveRDS(ensemble_best_ridge_model, fil)

  fil <- tempfile("ensemble_rpart_train_fit", fileext = ".RDS")
  saveRDS(ensemble_rpart_train_fit, fil)

  fil <- tempfile("ensemble_svm_train_fit", fileext = ".RDS")
  saveRDS(ensemble_svm_train_fit, fil)

  fil <- tempfile("ensemble_tree_train_fit", fileext = ".RDS")
  saveRDS(ensemble_tree_train_fit, fil)

  fil <- tempfile("ensemble_xgb_model", fileext = ".RDS")
  saveRDS(ensemble_xgb_model, fil)
}

message('The trained models are temporariliy saved in this directory: tempdir1. This directory is automatically deleted at the end of the R session.
          You may save the trained models before you end this session if you chose to do so.')


# Outliers list
df2 <- df %>% purrr::keep(is.numeric)

## Outliers list
for (i in 1:ncol(df2)) {
  Q1 <- quantile(df2[, i], .05)
  Q3 <- quantile(df2[, i], .95)
  IQR <- IQR(df2[, i])

  #subset data where lstat value is outside 1.5*IQR of Q1 and Q3
  message(noquote(""))
  outliers <- subset(df2, df2[, i]<(Q1 - 1.5*IQR) | df2[, i]>(Q3 + 1.5*IQR))
  message(paste0("0.05 and 0.95 outliers for ", colnames(df2[i]),", column number ", i))
  message(paste0("IQR = ", IQR, ", 0.05 = ", Q1, " 0.95 = ", Q3))
  outliers_df <- rbind(outliers_df, outliers)
  message(outliers_df)
  outliers_df <- data.frame()
  message(noquote(""))
}

return(list(
  "head_of_data" = head_df, "accuracy_plot" = accuracy_plot, "accuracy_free_scales" = accuracy_plot2, "holdout_vs_train_plot" = holdout_vs_train_plot, "holdout_vs_train_plot2" = holdout_vs_train_plot2,
  "histograms" = histograms, "boxplots" = boxplots, "predictor_vs_target" = predictor_vs_target, "final_results_table" = final_results,
  "data_correlation" = data_correlation, "data_summary" = data_summary, "head_of_ensemble" = head_ensemble, "ensemble_correlation" = ensemble_correlation,
  "accuracy_barchart" = accuracy_barchart, "train_vs_holdout" = total_plot, "train_vs_holdout_free_scales" = total_plot2, "duration_barchart" = duration_barchart, "holdout_vs_train_barchart" = holdout_vs_train_barchart,
  "bias_barchart" = bias_barchart, "MSE_barchart" = MSE_barchart, "MAE_barchart" = MAE_barchart, "SSE_barchart" = SSE_barchart, "Kolmogorov-Smirnov test p-score" = k_s_test_barchart,
  "bias_plot" = bias_plot, "MSE_plot" = MSE_plot, "MAE_plot" = MAE_plot, "SSE_plot" = SSE_plot,
  "colnum" = colnum, "numresamples" = numresamples, "save_all_trained_modesl" = save_all_trained_models, "how_to_handle_strings" = how_to_handle_strings,
  "data_reduction_method" = data_reduction_method, 'VIF' = VIF, "scale_data" = scale_all_predictors_in_data,
  "train_amount" = train_amount, "test_amount" = test_amount, "validation_amount" = validation_amount
)
)

}
