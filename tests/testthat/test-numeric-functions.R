test_that("Numeric master engine trains base models and outputs ordered leaderboards", {
  set.seed(123)
  n_mock <- 120
  x1 <- runif(n_mock, 1, 10)
  x2 <- runif(n_mock, 5, 15)
  y  <- 10 + 2.5 * x1 - 1.2 * x2 + rnorm(n_mock, sd = 0.2)
  mock_df <- data.frame(Y = y, X1 = x1, X2 = x2)

  pipeline_res <- Numeric(
    dataset = mock_df,
    target_col = "Y",
    config = NumericEnsemblesFastConfig(),
    verbose = FALSE
  )

  expect_s3_class(pipeline_res, "numeric_pipeline")
  expect_true(is.data.frame(pipeline_res$performance_report))

  # FIXED: Updated argument string name to target your production vif_report element
  expect_true(is.data.frame(pipeline_res$vif_report))

  expect_true(all(pipeline_res$performance_report$`RMSE 95% CI Upper` > 0))
})

test_that("predict and predict_production emit sound numeric matrices", {
  set.seed(123)
  n_mock <- 100
  x1 <- runif(n_mock, 1, 10)
  x2 <- runif(n_mock, 5, 15)
  y  <- 10 + 2.5 * x1 - 1.2 * x2 + rnorm(n_mock, sd = 0.2)
  mock_df <- data.frame(Y = y, X1 = x1, X2 = x2)

  pipeline_res <- Numeric(dataset = mock_df, target_col = "Y", config = NumericEnsemblesFastConfig(), verbose = FALSE)

  new_data <- data.frame(X1 = c(5.0, 2.5), X2 = c(10.0, 8.5))
  point_preds <- predict(pipeline_res, newdata = new_data, model_name = "best")
  expect_type(point_preds, "double")
  expect_length(point_preds, 2)

  prod_report <- predict_production(pipeline_res, newdata = new_data)
  expect_true(is.data.frame(prod_report))
  expect_equal(nrow(prod_report), 2)
})
