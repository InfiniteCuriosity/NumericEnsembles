test_that("NumericEnsemblesConfig maps variables and enforces constraints safely", {
  expect_error(NumericEnsemblesConfig(train_pct = 1.2)) # Catches boundary inflation
  expect_error(NumericEnsemblesConfig(cv_folds = 1))    # Enforces minimum cross validation folds

  cfg <- NumericEnsemblesFastConfig()
  expect_type(cfg, "list")
  expect_equal(cfg$cv_folds, 2)
})

test_that("save_pipeline and load_pipeline execute flawless environment hydration", {
  set.seed(42)
  n_mock <- 100
  x1 <- runif(n_mock, 1, 10)
  x2 <- runif(n_mock, 5, 15)
  y  <- 10 + 2.5 * x1 - 1.2 * x2 + rnorm(n_mock, sd = 0.2)
  mock_df <- data.frame(Y = y, X1 = x1, X2 = x2)

  pipeline_res <- Numeric(dataset = mock_df, target_col = "Y", config = NumericEnsemblesFastConfig(), verbose = FALSE)

  sandbox_file <- file.path(tempdir(), "test_economic_pipeline.rds")
  save_pipeline(pipeline_res, sandbox_file)

  expect_true(file.exists(sandbox_file))
  rehydrated <- load_pipeline(sandbox_file)
  expect_s3_class(rehydrated, "numeric_pipeline")

  if (file.exists(sandbox_file)) file.remove(sandbox_file)
})
