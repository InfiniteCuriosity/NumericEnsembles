# =========================================================================
# COMPREHENSIVE PERFORMANCE PIPELINE ENGINE WITH ADVANCED DIAGNOSTICS & IO
# =========================================================================

# Declare global variables to satisfy CRAN check Non-Standard Evaluation (NSE) data masking rules
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".data", "Value", "Feature", "Var1", "Var2", "Correlation",
                           "Model", "Absolute_Bias", "Variance", "Testing_RMSE",
                           "KS_p_value", "Metric", "Z_Score"))
}

#' Core Performance Pipeline Engine for Continuous Data
#'
#' @importFrom graphics abline barplot box hist lines par plot.new text
#' @importFrom stats cor na.omit predict qqline qqnorm
#' @importFrom utils combn head globalVariables

#' @param dataset A data.frame containing continuous target outputs and features.
#' @param target_col Character string specifying the name of the target column.
#' @param cv_folds Integer specifying the number of cross-validation folds. Default = 5.
#' @param train_pct Decimal fraction between 0 and 1 for the training split. Default = 0.60.
#' @param vif_threshold The maximum level of variance inflation factor, default = 5.
#' @param facet_col Character string specifying a column to facet EDA charts by. Default = "".
#' @param color_col Character string specifying a column to color EDA charts by. Default = "".
#' @param stratify_col Character string specifying a categorical column to anchor stratified sampling splits. Default = "".
#' @param palette_style Character string choosing a color palette: "standard", "viridis", or "modern".
#' @param verbose TRUE or FALSE console logging setting.

#' @export
Numeric <- function(dataset = NULL,
                    target_col = NULL,
                    cv_folds = 5,
                    train_pct = 0.60,
                    vif_threshold = 5,
                    facet_col = "",
                    color_col = "",
                    stratify_col = "",
                    palette_style = c("standard", "viridis", "modern"),
                    verbose = TRUE) {

  # 1. INITIALIZATION & DEPENDENCY CHECK
  required_packages <- c("caret", "MASS", "rpart", "Cubist",
                         "nnet", "ipred", "elasticnet", "glmnet",
                         "randomForest", "kernlab", "brnn", "earth",
                         "pls", "quantregForest", "party", "car",
                         "partykit", "doParallel", "ggplot2", "patchwork",
                         "tidyr", "GGally", "ggrepel", "tidyselect")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Required package '%s' is missing. Please install it before running the pipeline.", pkg), call. = FALSE)
    }
  }

  # Save original warning setting and suppress non-breaking warnings safely
  old_warn <- options(warn = -1)
  on.exit(options(old_warn), add = TRUE)

  # 2. RUNTIME INTERACTIVE FALLBACKS (Only triggers if arguments are missing)
  if (verbose) cat("--- Comprehensive Machine Learning Pipeline ---\n")

  if (is.null(dataset)) {
    dataset_name <- trimws(readline(prompt = "Enter the name of the dataset (e.g., MASS::Boston): "))
    df <- tryCatch({
      eval(parse(text = dataset_name))
    }, error = function(e) { stop("Could not find or load the specified dataset.", call. = FALSE) })
  } else {
    df <- dataset
  }

  if (is.null(target_col)) {
    cat("\nAvailable columns for Target Variable:\n  ->", paste(colnames(df), collapse = ", "), "\n")
    target_col <- trimws(readline(prompt = "Enter the name of the target column: "))
  }
  if (!(target_col %in% colnames(df))) stop("Target column not found in dataset.", call. = FALSE)

  # Auto-correct whole numbers (e.g. 60) to decimal percentages (0.60) to stay CRAN compliant
  if (train_pct > 1) {
    if (verbose) cat(sprintf("\n[Input Auto-Correction]: Scaling train_pct from %s down to a decimal fraction (%s).\n", train_pct, train_pct / 100))
    train_pct <- train_pct / 100
  }
  if (train_pct <= 0 || train_pct >= 1) stop("train_pct must be a decimal fraction between 0 and 1 (e.g., 0.60).", call. = FALSE)

  # STRATIFIED SAMPLING COLUMN DEFENSIVE SANITIZER RAIL
  if (stratify_col != "") {
    if (!(stratify_col %in% colnames(df))) {
      stop(sprintf("Stratification column '%s' not found in the dataset.", stratify_col), call. = FALSE)
    }
    strat_val <- df[[stratify_col]]
    if (is.numeric(strat_val) && length(unique(na.omit(strat_val))) > 15) {
      stop(sprintf("Column '%s' is continuous numeric. Stratified sampling splits require a discrete factor or categorical column (e.g., ShelveLoc).", stratify_col), call. = FALSE)
    }
  }

  # Dynamic Palette Selection Fallback Menu (Triggers if palette_style is missing or evaluated as a full vector)
  if (missing(palette_style) || length(palette_style) > 1) {
    if (interactive()) {
      cat("\nChoose a Visualization Palette Style:\n")
      cat("  [1] standard (Classic Academic Blue/Red Gradient)\n")
      cat("  [2] viridis  (Colorblind Friendly & Perceptually Uniform)\n")
      cat("  [3] modern   (High-Contrast Corporate Navy/Coral Look)\n")
      choice <- trimws(readline(prompt = "Select palette option (1, 2, or 3): "))
      palette_style <- switch(choice, "1" = "standard", "2" = "viridis", "3" = "modern", "standard")
    } else {
      palette_style = "standard"
    }
  }
  palette_style <- match.arg(palette_style)

  # DYNAMIC PALETTE STRUCT MATRIX CONFIGURATION
  theme_colors <- switch(palette_style,
                         "standard" = list(
                           primary   = "steelblue",
                           secondary = "cyan4",
                           accent    = "purple",
                           highlight = "darkgreen",
                           warning   = "tomato",
                           ks_fill   = "darkorange2",
                           ks_line1  = "blue",
                           ks_line2  = "red",
                           tiles_low = "darkgreen",
                           tiles_mid = "white",
                           tiles_high= "darkred"
                         ),
                         "viridis" = list(
                           primary   = "#21918c", # Veridian teal
                           secondary = "#3b528b", # Deep blue
                           accent    = "#440154", # Indigo/Purple
                           highlight = "#5dc963", # Light green
                           warning   = "#fde725", # Vibrant yellow
                           ks_fill   = "#21918c",
                           ks_line1  = "#440154",
                           ks_line2  = "#fde725",
                           tiles_low = "#440154",
                           tiles_mid = "#21918c",
                           tiles_high= "#fde725"
                         ),
                         "modern" = list(
                           primary   = "#111E6C", # Midnight Navy
                           secondary = "#FF6F61", # Living Coral
                           accent    = "#008080", # Deep Teal
                           highlight = "#708090", # Slate Grey
                           warning   = "#FF4500", # Orange Red
                           ks_fill   = "#FF6F61",
                           ks_line1  = "#111E6C",
                           ks_line2  = "#FF4500",
                           tiles_low = "#008080",
                           tiles_mid = "#ECEFF1",
                           tiles_high= "#FF6F61"
                         )
  )

  # --- EXTRACT BASELINE TABLES ---
  if (verbose) cat("\n[Extracting Baseline Profiles]: Capturing Head, Summary, and Correlation matrices...\n")
  data_head_table <- head(df, 6)
  data_summary_table <- summary(df)

  numeric_cols_idx <- sapply(df, is.numeric)
  if (sum(numeric_cols_idx) > 1) {
    data_correlation_matrix <- cor(df[, numeric_cols_idx], use = "complete.obs")
  } else {
    data_correlation_matrix <- "Insufficient numeric columns to establish a correlation matrix."
  }

  # --- DATA DICTIONARY GEN ENGINE ---
  if (verbose) {
    cat("\n=== DATA DICTIONARY ===\n")
    data_dict <- data.frame(
      Feature = colnames(df),
      Type = sapply(df, function(x) paste(class(x), collapse = ", ")),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Pct = paste0(round(sapply(df, function(x) sum(is.na(x)) / length(x) * 100), 2), "%"),
      Unique_Values = sapply(df, function(x) length(unique(na.omit(x)))),
      stringsAsFactors = FALSE
    )
    rownames(data_dict) <- NULL
    print(data_dict, right = FALSE)
  } else {
    data_dict <- data.frame()
  }

  # Target column row-drop sanitization
  if (any(is.na(df[[target_col]]))) {
    num_missing <- sum(is.na(df[[target_col]]))
    if (verbose) cat(sprintf("\n[Preprocessing]: Dropping %d rows with missing Target values.\n", num_missing))
    df <- df[!is.na(df[[target_col]]), ]
  }

  # =========================================================================
  # EXPLORATORY DATA ANALYSIS (EDA) PLOT GENERATION ENGINE
  # =========================================================================
  if (verbose) cat("\n[EDA Engine]: Generating data distribution, correlation, and scatter plots...\n")
  eda_plots <- list()

  # Dynamic Palette Fill Assignment Logic
  fill_aes <- if(color_col != "" && color_col %in% colnames(df)) ggplot2::aes(fill = .data[[color_col]]) else ggplot2::aes(fill = theme_colors$primary)
  color_aes <- if(color_col != "" && color_col %in% colnames(df)) ggplot2::aes(color = .data[[color_col]]) else ggplot2::aes(color = theme_colors$secondary)

  # 1. Histograms of Data
  cols_to_pivot <- colnames(df)[sapply(df, is.numeric)]
  if (color_col != "" && color_col %in% colnames(df)) cols_to_pivot <- setdiff(cols_to_pivot, color_col)
  if (facet_col != "" && facet_col %in% colnames(df)) cols_to_pivot <- setdiff(cols_to_pivot, facet_col)

  df_long_all <- tidyr::pivot_longer(df, cols = tidyselect::all_of(cols_to_pivot), names_to = "Feature", values_to = "Value")

  p_hist <- ggplot2::ggplot(df_long_all, ggplot2::aes(x = Value)) +
    ggplot2::geom_histogram(fill_aes, bins = 20, color = "white", alpha = 0.8) +
    ggplot2::facet_wrap(~Feature, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Feature Distributions (Histograms)", x = "Value", y = "Count")
  if (color_col == "") {
    p_hist <- p_hist + ggplot2::scale_fill_identity()
  }
  if (facet_col != "" && facet_col %in% colnames(df)) {
    p_hist <- p_hist + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)), scales = "free")
  }
  eda_plots$histograms <- p_hist

  # 2. Box plots of Data
  p_box <- ggplot2::ggplot(df_long_all, ggplot2::aes(y = Value, x = "Feature")) +
    ggplot2::geom_boxplot(fill_aes, alpha = 0.7, outlier.color = theme_colors$warning) +
    ggplot2::facet_wrap(~Feature, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Feature Range Profiles (Box Plots)", x = NULL, y = "Value") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  if (color_col == "") {
    p_box <- p_box + ggplot2::scale_fill_identity()
  }
  if (facet_col != "" && facet_col %in% colnames(df)) {
    p_box <- p_box + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)), scales = "free")
  }
  eda_plots$boxplots <- p_box

  # 3. Correlation Plot
  if (is.matrix(data_correlation_matrix)) {
    df_corr <- as.data.frame(data_correlation_matrix)
    df_corr$Var1 <- rownames(df_corr)
    df_corr_long <- tidyr::pivot_longer(df_corr, cols = -Var1, names_to = "Var2", values_to = "Correlation")

    p_corr <- ggplot2::ggplot(df_corr_long, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient2(low = theme_colors$tiles_low, high = theme_colors$tiles_high, mid = theme_colors$tiles_mid, limit = c(-1,1)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Feature Correlation Matrix Heatmap", x = NULL, y = NULL) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    eda_plots$correlation <- p_corr
  } else {
    eda_plots$correlation <- ggplot2::ggplot() + ggplot2::labs(title = "Correlation Map Matrix - N/A")
  }

  # 4. Scatter plots of Target vs Each Feature
  scatter_cols <- colnames(df)[sapply(df, is.numeric)]
  scatter_cols <- setdiff(scatter_cols, target_col)
  if (color_col != "" && color_col %in% colnames(df)) scatter_cols <- setdiff(scatter_cols, color_col)
  if (facet_col != "" && facet_col %in% colnames(df)) scatter_cols <- setdiff(scatter_cols, facet_col)

  df_scatter_long <- tidyr::pivot_longer(df, cols = tidyselect::all_of(scatter_cols), names_to = "Feature", values_to = "Value")

  p_scatter <- ggplot2::ggplot(df_scatter_long, ggplot2::aes(x = Value, y = .data[[target_col]])) +
    ggplot2::geom_point(color_aes, alpha = 0.5, size = 1) +
    ggplot2::geom_smooth(method = "lm", color = theme_colors$warning, se = FALSE, linetype = "dashed") +
    ggplot2::facet_wrap(~Feature, scales = "free_x") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Scatter Analysis: Target Variable (", target_col, ") vs Features"), x = "Feature Value", y = target_col)
  if (color_col == "") {
    p_scatter <- p_scatter + ggplot2::scale_color_identity()
  }
  eda_plots$scatter_matrix <- p_scatter

  # 3. PARALLEL PROCESSING ACTIVATION WITH REINFORCED ENVIRONMENT CHECK
  cores_to_use <- max(1, parallel::detectCores() - 1)

  # Comprehensive check to catch isolated vignette builders (e.g. callr sessions spawned by devtools)
  is_build_env <- nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_")) ||
    nzchar(Sys.getenv("R_CMD")) ||
    nzchar(Sys.getenv("R_TESTS")) ||
    any(blacklisted_calls <- c("pkgdown", "knitr", "rmarkdown") %in% loadedNamespaces())

  if (is_build_env) {
    cores_to_use <- min(2, cores_to_use)
  }

  cl <- parallel::makePSOCKcluster(cores_to_use)
  doParallel::registerDoParallel(cl)
  on.exit({
    try({ parallel::stopCluster(cl); foreach::registerDoSEQ() }, silent = TRUE)
  }, add = TRUE)

  if (verbose) cat(paste("\n[Parallel Backend Status]: Activated using", cores_to_use, "CPU cores.\n"))

  # 4. DATA SPLITTING (INTEGRATED STRATIFIED SAMPLING PROTOCOL)
  set.seed(42)
  if (stratify_col != "") {
    if (verbose) cat(sprintf("\n[Sampling Split]: Executing stratified partition based on factor column '%s'...\n", stratify_col))
    train_index <- caret::createDataPartition(df[[stratify_col]], p = train_pct, list = FALSE)
  } else {
    if (verbose) cat("\n[Sampling Split]: Executing regular population density split based on target feature values...\n")
    train_index <- caret::createDataPartition(df[[target_col]], p = train_pct, list = FALSE)
  }

  train_data  <- df[train_index, ]
  test_data   <- df[-train_index, ]

  if (verbose) cat("\n[Preprocessing]: Applying explicit dummy encoding to categorical predictors...\n")
  predictors_raw <- colnames(df)[colnames(df) != target_col]

  dummy_model <- caret::dummyVars(" ~ .", data = train_data[, predictors_raw, drop = FALSE], fullRank = TRUE)
  train_encoded <- data.frame(predict(dummy_model, newdata = train_data, na.action = stats::na.pass))
  test_encoded  <- data.frame(predict(dummy_model, newdata = test_data, na.action = stats::na.pass))

  train_data <- cbind(train_encoded, Target_Var = train_data[[target_col]])
  colnames(train_data)[ncol(train_data)] <- target_col

  test_data <- cbind(test_encoded, Target_Var = test_data[[target_col]])
  colnames(test_data)[ncol(test_data)] <- target_col

  numeric_features <- colnames(train_data)[colnames(train_data) != target_col]

  # VIF ENGINE MULTICOLLINEARITY FILTER
  vif_report_table <- data.frame(Feature = character(), VIF = numeric(), Status = character(), stringsAsFactors = FALSE)
  kept_vif_features <- numeric_features

  if (vif_threshold > 0 && length(numeric_features) > 1) {
    if (verbose) cat("\n[VIF Check]: Evaluating features for multicollinearity using car::vif...\n")
    vif_df <- train_data[, c(target_col, numeric_features)]
    dropped_features <- c()

    while(TRUE) {
      current_features <- colnames(vif_df)[colnames(vif_df) != target_col]
      if (length(current_features) <= 1) break
      vif_formula <- stats::as.formula(paste(target_col, "~", paste(current_features, collapse = " + ")))

      vif_values <- suppressWarnings(tryCatch({ car::vif(stats::lm(vif_formula, data = vif_df, na.action = stats::na.exclude)) }, error = function(e) { return(NULL) }))
      if (is.null(vif_values)) break

      if (is.matrix(vif_values)) {
        max_vif <- max(vif_values[, "GVIF"])
        worst_feat <- rownames(vif_values)[which.max(vif_values[, "GVIF"])]
      } else {
        max_vif <- max(vif_values)
        worst_feat <- names(vif_values)[which.max(vif_values)]
      }

      if (max_vif > vif_threshold) {
        if (verbose) cat(sprintf("  -> Dropping feature '%s' (VIF: %.2f)\n", worst_feat, max_vif))
        vif_report_table <- rbind(vif_report_table, data.frame(Feature = worst_feat, VIF = round(max_vif, 2), Status = "Dropped", stringsAsFactors = FALSE))
        vif_df <- vif_df[, colnames(vif_df) != worst_feat]
        dropped_features <- c(dropped_features, worst_feat)
      } else {
        if(is.matrix(vif_values)) {
          for(f in rownames(vif_values)) {
            vif_report_table <- rbind(vif_report_table, data.frame(Feature = f, VIF = round(vif_values[f, "GVIF"], 2), Status = "Kept", stringsAsFactors = FALSE))
          }
        } else {
          for(f in names(vif_values)) {
            vif_report_table <- rbind(vif_report_table, data.frame(Feature = f, VIF = round(vif_values[f], 2), Status = "Kept", stringsAsFactors = FALSE))
          }
        }
        break
      }
    }

    if (length(dropped_features) > 0) {
      train_data <- train_data[, !(colnames(train_data) %in% dropped_features)]
      test_data  <- test_data[, !(colnames(test_data) %in% dropped_features)]
      kept_vif_features <- colnames(vif_df)[colnames(vif_df) != target_col]
    }
    vif_report_table <- vif_report_table[!duplicated(vif_report_table$Feature, fromLast = TRUE), ]
    rownames(vif_report_table) <- NULL
  } else {
    vif_report_table <- "VIF step bypassed."
  }

  formula_obj <- stats::as.formula(paste(target_col, "~ ."))

  # 5. CORE BASE MODEL ARCHITECTURES TRAINING
  transform_steps <- c("nzv", "medianImpute", "center", "scale", "YeoJohnson")
  cv_control <- caret::trainControl(method = "cv", number = cv_folds, allowParallel = TRUE)

  rf_grid <- expand.grid(mtry = intersect(c(2, 4, 6, 8), 1:(ncol(train_data) - 1)))
  glmnet_grid <- expand.grid(alpha = seq(0, 1, length = 5), lambda = seq(0.001, 0.2, length = 10))

  if (verbose) cat("\nTraining 17 base architectures concurrently across core structures...\n")

  start_t <- proc.time()
  model_bag     <- caret::train(formula_obj, data = train_data, method = "treebag", trControl = cv_control, na.action = stats::na.pass)
  dur_bag       <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_brnn    <- caret::train(formula_obj, data = train_data, method = "brnn", trControl = cv_control, preProcess = transform_steps, verbose = FALSE, na.action = stats::na.pass)
  dur_brnn      <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_cubist  <- caret::train(formula_obj, data = train_data, method = "cubist", trControl = cv_control, na.action = stats::na.pass)
  dur_cubist    <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_elastic <- caret::train(formula_obj, data = train_data, method = "glmnet", trControl = cv_control, preProcess = transform_steps, tuneGrid = glmnet_grid, na.action = stats::na.pass)
  dur_elastic   <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_lasso   <- caret::train(formula_obj, data = train_data, method = "glmnet", trControl = cv_control, preProcess = transform_steps, tuneGrid = expand.grid(alpha = 1, lambda = glmnet_grid$lambda), na.action = stats::na.pass)
  dur_lasso     <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_lm      <- caret::train(formula_obj, data = train_data, method = "lm", trControl = cv_control, preProcess = transform_steps, na.action = stats::na.pass)
  dur_lm        <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_mars    <- caret::train(formula_obj, data = train_data, method = "earth", trControl = cv_control, tuneLength = 5, na.action = stats::na.pass)
  dur_mars      <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_nnet    <- caret::train(formula_obj, data = train_data, method = "nnet", trControl = cv_control, preProcess = transform_steps, linout = TRUE, trace = FALSE, na.action = stats::na.pass)
  dur_nnet      <- (proc.time() - start_t)[3]

  start_t + proc.time()
  model_pcr     <- caret::train(formula_obj, data = train_data, method = "pcr", trControl = cv_control, preProcess = transform_steps, tuneLength = 10, na.action = stats::na.pass)
  dur_pcr       <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_qrf     <- caret::train(formula_obj, data = train_data, method = "qrf", trControl = cv_control, na.action = stats::na.pass)
  dur_qrf       <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_ridge   <- caret::train(formula_obj, data = train_data, method = "glmnet", trControl = cv_control, preProcess = transform_steps, tuneGrid = expand.grid(alpha = 0, lambda = glmnet_grid$lambda), na.action = stats::na.pass)
  dur_ridge     <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_rf      <- caret::train(formula_obj, data = train_data, method = "rf", trControl = cv_control, tuneGrid = rf_grid, importance = TRUE, na.action = stats::na.pass)
  dur_rf        <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_svm     <- caret::train(formula_obj, data = train_data, method = "svmRadial", trControl = cv_control, preProcess = transform_steps, tuneLength = 8, na.action = stats::na.pass)
  dur_svm       <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_tree    <- caret::train(formula_obj, data = train_data, method = "rpart", trControl = cv_control, tuneLength = 10, na.action = stats::na.pass)
  dur_tree      <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_bagEarth<- caret::train(formula_obj, data = train_data, method = "bagEarth", trControl = cv_control, preProcess = transform_steps, na.action = stats::na.pass)
  dur_bagEarth  <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_cforest <- caret::train(formula_obj, data = train_data, method = "cforest", trControl = cv_control, controls = party::cforest_unbiased(ntree = 150))
  dur_cforest   <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  model_avNNet  <- caret::train(formula_obj, data = train_data, method = "avNNet", trControl = cv_control, preProcess = transform_steps, linout = TRUE, trace = FALSE, na.action = stats::na.pass)
  dur_avNNet    <- (proc.time() - start_t)[3]

  models_list <- list(
    Linear = model_lm, Tree = model_tree, Cubist = model_cubist,
    NeuralNet = model_nnet, Bagging = model_bag, Ridge = model_ridge, ElasticNet = model_elastic,
    Lasso = model_lasso, RandomForest = model_rf, SVM_Radial = model_svm,
    BayesRNN = model_brnn, MARS = model_mars, PCR = model_pcr,
    QuantileRF = model_qrf, Bagged_MARS = model_bagEarth,
    Cond_Inf_Forest = model_cforest, Averaged_NNet = model_avNNet
  )

  durations_list <- list(
    Linear = dur_lm, Tree = dur_tree, Cubist = dur_cubist,
    NeuralNet = dur_nnet, Bagging = dur_bag, Ridge = dur_ridge, ElasticNet = dur_elastic,
    Lasso = dur_lasso, RandomForest = dur_rf, SVM_Radial = dur_svm,
    BayesRNN = dur_brnn, MARS = dur_mars, PCR = dur_pcr,
    QuantileRF = dur_qrf, Bagged_MARS = dur_bagEarth,
    Cond_Inf_Forest = dur_cforest, Averaged_NNet = dur_avNNet
  )

  # 6. VECTORIZED MODEL EVALUATION PIPELINE
  actual_test  <- test_data[[target_col]]
  actual_train <- train_data[[target_col]]
  n_models     <- length(models_list)
  m_names      <- names(models_list)

  n_test <- length(actual_test)
  p_features <- length(kept_vif_features)
  ss_tot_test <- sum((actual_test - mean(actual_test))^2)
  var_actual_test <- stats::var(actual_test)

  pred_train_list <- lapply(models_list, function(mod) as.numeric(predict(mod, newdata = train_data, na.action = stats::na.pass)))
  pred_test_list  <- lapply(models_list, function(mod) as.numeric(predict(mod, newdata = test_data, na.action = stats::na.pass)))

  v_model        <- character(n_models)
  v_testing_rmse <- numeric(n_models)
  v_rmse_ci_lower<- numeric(n_models)
  v_rmse_ci_upper<- numeric(n_models)
  v_testing_mae  <- numeric(n_models)
  v_mae_ci_lower <- numeric(n_models)
  v_mae_ci_upper <- numeric(n_models)
  v_adj_r2       <- numeric(n_models)
  v_adj_r2_lower <- numeric(n_models)
  v_adj_r2_upper <- numeric(n_models)
  v_duration     <- numeric(n_models)
  v_overfitting  <- numeric(n_models)
  v_bias         <- numeric(n_models)
  v_variance     <- numeric(n_models)
  v_ks_p         <- numeric(n_models)
  top_pred_names  <- list()

  for (i in seq_len(n_models)) {
    model_name <- m_names[i]
    mod <- models_list[[model_name]]

    pred_train <- pred_train_list[[model_name]]
    pred_test  <- pred_test_list[[model_name]]

    rmse_train <- sqrt(mean((actual_train - pred_train)^2))
    rmse_test  <- sqrt(mean((actual_test - pred_test)^2))
    mae_test   <- mean(abs(actual_test - pred_test))

    test_residuals <- actual_test - pred_test
    se_mse <- stats::sd(test_residuals^2) / sqrt(length(test_residuals))
    mse_test <- mean(test_residuals^2)

    rmse_ci_lower <- sqrt(max(0, mse_test - (1.96 * se_mse)))
    rmse_ci_upper <- sqrt(mse_test + (1.96 * se_mse))

    abs_errors <- abs(test_residuals)
    se_mae <- stats::sd(abs_errors) / sqrt(length(abs_errors))
    mae_ci_lower <- max(0, mae_test - (1.96 * se_mae))
    mae_ci_upper <- mae_test + (1.96 * se_mae)

    ss_res_test <- sum(test_residuals^2)
    r2_val <- if (ss_tot_test > 0) 1 - (ss_res_test / ss_tot_test) else 0
    adj_r2_val <- if (n_test > p_features + 1) {
      1 - ((1 - r2_val) * (n_test - 1) / (n_test - p_features - 1))
    } else {
      r2_val
    }

    # Unified Asymptotic Delta Method for Adjusted R2 Confidence Bounds
    r2_ci_lower <- 1 - ((mse_test + (1.96 * se_mse)) / var_actual_test)
    r2_ci_upper <- 1 - ((max(0, mse_test - (1.96 * se_mse))) / var_actual_test)

    adj_r2_lower_val <- if (n_test > p_features + 1) {
      1 - ((1 - r2_ci_lower) * (n_test - 1) / (n_test - p_features - 1))
    } else {
      r2_ci_lower
    }
    adj_r2_upper_val <- if (n_test > p_features + 1) {
      1 - ((1 - r2_ci_upper) * (n_test - 1) / (n_test - p_features - 1))
    } else {
      r2_ci_upper
    }

    variance_val <- stats::var(pred_test)

    v_model[i]        <- model_name
    v_testing_rmse[i] <- round(rmse_test, 4)
    v_rmse_ci_lower[i]<- round(rmse_ci_lower, 4)
    v_rmse_ci_upper[i]<- round(rmse_ci_upper, 4)
    v_testing_mae[i]  <- round(mae_test, 4)
    v_mae_ci_lower[i] <- round(mae_ci_lower, 4)
    v_mae_ci_upper[i] <- round(mae_ci_upper, 4)
    v_adj_r2[i]       <- round(adj_r2_val, 4)
    v_adj_r2_lower[i] <- round(adj_r2_lower_val, 4)
    v_adj_r2_upper[i] <- round(adj_r2_upper_val, 4)
    v_duration[i]     <- round(durations_list[[model_name]], 4)
    v_overfitting[i]  <- round(rmse_test / rmse_train, 4)
    v_bias[i]         <- round(mean(pred_test - actual_test), 4)
    v_variance[i]     <- round(variance_val, 4)

    ks_res            <- stats::ks.test(pred_test, actual_train)
    v_ks_p[i]         <- round(ks_res$p.value, 4)

    top_pred_names[[model_name]] <- tryCatch({
      imp <- caret::varImp(mod)
      imp_df <- if (!is.null(imp$importance)) imp$importance else imp
      rownames(imp_df)[order(rowSums(as.matrix(imp_df)), decreasing = TRUE)[1]]
    }, error = function(e) {
      colnames(train_data)[colnames(train_data) != target_col][1]
    })
  }

  report <- data.frame(
    Model = v_model,
    Testing_RMSE = v_testing_rmse,
    `RMSE 95% CI Lower` = v_rmse_ci_lower,
    `RMSE 95% CI Upper` = v_rmse_ci_upper,
    Testing_MAE = v_testing_mae,
    `MAE 95% CI Lower` = v_mae_ci_lower,
    `MAE 95% CI Upper` = v_mae_ci_upper,
    Adjusted_R2 = v_adj_r2,
    `Adjusted R2 95% CI Lower` = v_adj_r2_lower,
    `Adjusted R2 95% CI Upper` = v_adj_r2_upper,
    Duration = v_duration,
    Overfitting = v_overfitting,
    Bias = v_bias,
    Variance = v_variance,
    KS_p_value = v_ks_p,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # 7. STACKING META-LEARNERS ENGINE
  if (verbose) cat("\n[Meta-Learner Engine]: Training 3 Stacking Meta-Learners (GLM, RF, SVM)...\n")
  meta_train_df <- as.data.frame(pred_train_list)
  meta_train_df[[target_col]] <- actual_train

  meta_test_df <- as.data.frame(pred_test_list)
  meta_test_df[[target_col]] <- actual_test

  meta_control <- caret::trainControl(method = "cv", number = cv_folds, allowParallel = TRUE)
  meta_formula <- stats::as.formula(paste(target_col, "~ ."))

  start_t <- proc.time()
  meta_model_glm <- caret::train(meta_formula, data = meta_train_df, method = "lm", trControl = meta_control)
  dur_meta_glm <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  meta_model_rf  <- caret::train(meta_formula, data = meta_train_df, method = "rf", trControl = meta_control, tuneLength = 3)
  dur_meta_rf <- (proc.time() - start_t)[3]

  start_t <- proc.time()
  meta_model_svm <- caret::train(meta_formula, data = meta_train_df, method = "svmRadial", trControl = meta_control, tuneLength = 3)
  dur_meta_svm <- (proc.time() - start_t)[3]

  meta_durations <- list(Meta_GLM = dur_meta_glm, Meta_RF = dur_meta_rf, Meta_SVM = dur_meta_svm)

  pred_train_list[["Meta_GLM"]] <- as.numeric(predict(meta_model_glm, newdata = meta_train_df))
  pred_test_list[["Meta_GLM"]]  <- as.numeric(predict(meta_model_glm, newdata = meta_test_df))
  pred_train_list[["Meta_RF"]]  <- as.numeric(predict(meta_model_rf, newdata = meta_train_df))
  pred_test_list[["Meta_RF"]]   <- as.numeric(predict(meta_model_rf, newdata = meta_test_df))
  pred_train_list[["Meta_SVM"]] <- as.numeric(predict(meta_model_svm, newdata = meta_train_df))
  pred_test_list[["Meta_SVM"]]  <- as.numeric(predict(meta_model_svm, newdata = meta_test_df))

  models_list[["Meta_GLM"]] <- meta_model_glm
  models_list[["Meta_RF"]]  <- meta_model_rf
  models_list[["Meta_SVM"]] <- meta_model_svm

  meta_names <- c("Meta_GLM", "Meta_RF", "Meta_SVM")
  meta_reports <- list()

  for (m_name in meta_names) {
    p_train <- pred_train_list[[m_name]]
    p_test  <- pred_test_list[[m_name]]

    rmse_train <- sqrt(mean((actual_train - p_train)^2))
    rmse_test  <- sqrt(mean((actual_test - p_test)^2))
    mae_test   <- mean(abs(actual_test - p_test))

    res_meta <- actual_test - p_test
    se_mse_m <- stats::sd(res_meta^2) / sqrt(length(res_meta))
    mse_test_m <- mean(res_meta^2)

    rmse_ci_low_m <- sqrt(max(0, mse_test_m - (1.96 * se_mse_m)))
    rmse_ci_upp_m <- sqrt(mse_test_m + (1.96 * se_mse_m))

    abs_err_m <- abs(res_meta)
    se_mae_m <- stats::sd(abs_err_m) / sqrt(length(abs_err_m))
    mae_ci_low_m <- max(0, mae_test - (1.96 * se_mae_m))
    mae_ci_upp_m <- mae_test + (1.96 * se_mae_m)

    ss_res_m <- sum(res_meta^2)
    r2_m <- if (ss_tot_test > 0) 1 - (ss_res_m / ss_tot_test) else 0
    adj_r2_m <- if (n_test > n_models + 1) {
      1 - ((1 - r2_m) * (n_test - 1) / (n_test - n_models - 1))
    } else {
      r2_m
    }

    r2_ci_low_m <- 1 - ((mse_test_m + (1.96 * se_mse_m)) / var_actual_test)
    r2_ci_upp_m <- 1 - ((max(0, mse_test_m - (1.96 * se_mse_m))) / var_actual_test)

    adj_r2_low_m <- if (n_test > n_models + 1) {
      1 - ((1 - r2_ci_low_m) * (n_test - 1) / (n_test - n_models - 1))
    } else {
      r2_ci_low_m
    }
    adj_r2_upp_m <- if (n_test > n_models + 1) {
      1 - ((1 - r2_ci_upp_m) * (n_test - 1) / (n_test - n_models - 1))
    } else {
      r2_ci_upp_m
    }

    var_m <- stats::var(p_test)
    ks_m <- stats::ks.test(p_test, actual_train)

    meta_reports[[m_name]] <- data.frame(
      Model = m_name,
      Testing_RMSE = round(rmse_test, 4),
      `RMSE 95% CI Lower` = round(rmse_ci_low_m, 4),
      `RMSE 95% CI Upper` = round(rmse_ci_upp_m, 4),
      Testing_MAE = round(mae_test, 4),
      `MAE 95% CI Lower` = round(mae_ci_low_m, 4),
      `MAE 95% CI Upper` = round(mae_ci_upp_m, 4),
      Adjusted_R2 = round(adj_r2_m, 4),
      `Adjusted R2 95% CI Lower` = round(adj_r2_low_m, 4),
      `Adjusted R2 95% CI Upper` = round(adj_r2_upp_m, 4),
      Duration = round(meta_durations[[m_name]], 4),
      Overfitting = round(rmse_test / rmse_train, 4),
      Bias = round(mean(p_test - actual_test), 4),
      Variance = round(var_m, 4),
      KS_p_value = round(ks_m$p.value, 4),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  meta_master_df <- do.call(rbind, meta_reports)

  # 8. DYNAMIC BLENDED ENSEMBLE COMBINATION ENGINE (WITH PROGRESS BAR & VERBOSITY)
  pairs_grid <- combn(m_names, 2, simplify = FALSE)
  n_pairs <- length(pairs_grid)
  ens_reports <- list()

  if (verbose) {
    cat(sprintf("\n[Ensemble Engine]: Evaluating all %d pairwise architectural combinations...\n", n_pairs))
    pb <- utils::txtProgressBar(min = 0, max = n_pairs, style = 3)
  }

  for (idx in seq_along(pairs_grid)) {
    m1 <- pairs_grid[[idx]][1]
    m2 <- pairs_grid[[idx]][2]
    ens_name <- paste0(m1, "+", m2)

    p_train_comb <- (pred_train_list[[m1]] + pred_train_list[[m2]]) / 2
    p_test_comb  <- (pred_test_list[[m1]] + pred_test_list[[m2]]) / 2

    pred_train_list[[ens_name]] <- p_train_comb
    pred_test_list[[ens_name]]  <- p_test_comb

    rmse_train_c <- sqrt(mean((actual_train - p_train_comb)^2))
    rmse_test_c  <- sqrt(mean((actual_test - p_test_comb)^2))
    mae_test_c   <- mean(abs(actual_test - p_test_comb))

    res_c <- actual_test - p_test_comb
    se_mse_c <- stats::sd(res_c^2) / sqrt(length(res_c))
    text_mse_c <- mean(res_c^2)

    rmse_ci_low_c <- sqrt(max(0, text_mse_c - (1.96 * se_mse_c)))
    rmse_ci_upp_c <- sqrt(text_mse_c + (1.96 * se_mse_c))

    abs_err_c <- abs(res_c)
    se_mae_c <- stats::sd(abs_err_c) / sqrt(length(abs_err_c))
    mae_ci_low_c <- max(0, mae_test_c - (1.96 * se_mae_c))
    mae_ci_upp_c <- mae_test_c + (1.96 * se_mae_c)

    ss_res_c <- sum(res_c^2)
    r2_c <- if (ss_tot_test > 0) 1 - (ss_res_c / ss_tot_test) else 0
    adj_r2_c <- if (n_test > 2 + 1) {
      1 - ((1 - r2_c) * (n_test - 1) / (n_test - 2 - 1))
    } else {
      r2_c
    }

    r2_ci_low_c <- 1 - ((text_mse_c + (1.96 * se_mse_c)) / var_actual_test)
    r2_ci_upp_c <- 1 - ((max(0, text_mse_c - (1.96 * se_mse_c))) / var_actual_test)

    adj_r2_low_c <- if (n_test > 2 + 1) {
      1 - ((1 - r2_ci_low_c) * (n_test - 1) / (n_test - 2 - 1))
    } else {
      r2_ci_low_c
    }
    adj_r2_upp_c <- if (n_test > 2 + 1) {
      1 - ((1 - r2_ci_upp_c) * (n_test - 1) / (n_test - 2 - 1))
    } else {
      r2_ci_upp_c
    }

    var_c <- stats::var(p_test_comb)
    ks_c <- stats::ks.test(p_test_comb, actual_train)

    ens_reports[[idx]] <- data.frame(
      Model = ens_name,
      Testing_RMSE = round(rmse_test_c, 4),
      `RMSE 95% CI Lower` = round(rmse_ci_low_c, 4),
      `RMSE 95% CI Upper` = round(rmse_ci_upp_c, 4),
      Testing_MAE = round(mae_test_c, 4),
      `MAE 95% CI Lower` = round(mae_ci_low_c, 4),
      `MAE 95% CI Upper` = round(mae_ci_upp_c, 4),
      Adjusted_R2 = round(adj_r2_c, 4),
      `Adjusted R2 95% CI Lower` = round(adj_r2_low_c, 4),
      `Adjusted R2 95% CI Upper` = round(adj_r2_upp_c, 4),
      Duration = round(durations_list[[m1]] + durations_list[[m2]], 4),
      Overfitting = round(rmse_test_c / rmse_train_c, 4),
      Bias = round(mean(p_test_comb - actual_test), 4),
      Variance = round(var_c, 4),
      KS_p_value = round(ks_c$p.value, 4),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (verbose) utils::setTxtProgressBar(pb, idx)
  }
  if (verbose) close(pb)

  ens_master_df <- do.call(rbind, ens_reports)
  ens_master_df <- ens_master_df[order(ens_master_df$Testing_RMSE), ]
  top_10_ensembles <- head(ens_master_df, 10)

  report <- rbind(report, meta_master_df, top_10_ensembles)
  report <- report[order(report$Testing_RMSE), ]

  # =========================================================================
  # AUTOMATED RESIDUAL DIAGNOSTIC AUDIT SUBSYSTEM ("SANITY CHECK ENGINE")
  # =========================================================================
  audit_logs <- list()
  top_3_models <- head(report$Model, 3)

  for (m_name in top_3_models) {
    p_test <- pred_test_list[[m_name]]
    res_test <- actual_test - p_test

    sw_test <- tryCatch({ stats::shapiro.test(head(res_test, 5000)) }, error = function(e) NULL)
    norm_status <- if (!is.null(sw_test) && sw_test$p.value < 0.05) "Non-Normal (Biased Tail Risks)" else "Normal"

    het_cor <- stats::cor.test(p_test, abs(res_test), method = "spearman", exact = FALSE)
    het_status <- if (!is.null(het_cor) && het_cor$p.value < 0.05) "Heteroscedastic (Unstable Variance)" else "Homoscedastic"

    dw_stat <- sum(diff(res_test)^2) / sum(res_test^2)
    ac_status <- if (dw_stat < 1.5 || dw_stat > 2.5) "Autocorrelated Error Structs" else "Independent"

    audit_logs[[m_name]] <- data.frame(
      Model = m_name,
      Residual_Normality = norm_status,
      Variance_Stability = het_status,
      Error_Independence = ac_status,
      stringsAsFactors = FALSE
    )
  }
  audit_report_matrix <- do.call(rbind, audit_logs)
  rownames(audit_report_matrix) <- NULL

  # =========================================================================
  # GRAPHICS GENERATION SUBSYSTEMS (GGPLOT2 & PATCHWORK)
  # =========================================================================

  make_metric_plot <- function(data, metric_col, title, fill_color, is_overfit = FALSE, show_ci = FALSE, ci_lower_col = NULL, ci_upper_col = NULL) {
    data$Model <- factor(data$Model, levels = rev(report$Model))
    p <- ggplot2::ggplot(data, ggplot2::aes(x = Model, y = .data[[metric_col]]))

    if (show_ci) {
      p <- p +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[[ci_lower_col]], ymax = .data[[ci_upper_col]]), width = 0.3, color = fill_color, linewidth = 0.7) +
        ggplot2::geom_point(color = theme_colors$accent, size = 2.5) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.4f", .data[[metric_col]])), vjust = -0.8, size = 2.5, fontface = "bold")
    } else {
      p <- p +
        ggplot2::geom_col(fill = fill_color, width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.4f", .data[[metric_col]])), hjust = -0.1, size = 2.5, fontface = "bold")
    }

    p = p + ggplot2::coord_flip() + ggplot2::theme_minimal(base_size = 9) + ggplot2::labs(title = title, x = NULL, y = NULL) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"), plot.title = ggplot2::element_text(face = "bold", size = 11))

    if (is_overfit) p <- p + ggplot2::geom_hline(yintercept = 1.0, color = theme_colors$warning, linetype = "dashed")
    p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = if (show_ci) c(0.08, 0.22) else c(0, 0.22)))
    return(p)
  }

  make_bias_variance_combined_plot <- function(data, title, palette = "standard") {
    data$Absolute_Bias <- abs(data$Bias)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = Absolute_Bias, y = Variance, label = Model)) +
      ggplot2::geom_point(ggplot2::aes(color = Testing_RMSE), size = 4, alpha = 0.8)

    if (palette == "viridis") {
      p <- p + ggplot2::scale_color_viridis_c(option = "viridis", direction = -1, name = "Testing RMSE")
    } else if (palette == "modern") {
      p <- p + ggplot2::scale_color_gradient(low = theme_colors$primary, high = theme_colors$secondary, name = "Testing RMSE")
    } else {
      p <- p + ggplot2::scale_color_gradient(low = "darkgreen", high = "firebrick2", name = "Testing RMSE")
    }

    p <- p +
      ggrepel::geom_text_repel(size = 2.5, fontface = "bold", max.overlaps = 15, box.padding = 0.3) +
      ggplot2::annotate("point", x = 0, y = 0, color = "gold", shape = 18, size = 6) +
      ggplot2::annotate("text", x = 0, y = 0, label = "Ideal (0,0)", vjust = -1.2, color = theme_colors$primary, fontface = "bold", size = 3) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::labs(title = title, subtitle = "Lower values are better. Color scales indicate overall Testing RMSE.", x = "Absolute Bias", y = "Prediction Variance") +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13), legend.position = "right")
    return(p)
  }

  # --- DE-CROWDING INTENT REFACTORIZATION: THE ACCURACY HOLY TRINITY PANEL ---
  p_kpi_metrics <- (make_metric_plot(report, "Testing_RMSE", "Testing RMSE (Lower is Better)", theme_colors$primary, show_ci = TRUE, ci_lower_col = "RMSE 95% CI Lower", ci_upper_col = "RMSE 95% CI Upper") +
                      make_metric_plot(report, "Testing_MAE", "Testing MAE (Lower is Better)", theme_colors$secondary, show_ci = TRUE, ci_lower_col = "MAE 95% CI Lower", ci_upper_col = "MAE 95% CI Upper") +
                      make_metric_plot(report, "Adjusted_R2", "Adjusted R-Squared (Higher is Better)", theme_colors$accent, show_ci = TRUE, ci_lower_col = "Adjusted R2 95% CI Lower", ci_upper_col = "Adjusted R2 95% CI Upper")) +
    patchwork::plot_layout(ncol = 3) +
    patchwork::plot_annotation(title = "Core Model Performance Metrics & KPIs", theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)))

  p_risk_metrics <- (make_metric_plot(report, "Overfitting", "Overfitting Ratio (Ideal = 1.0)", theme_colors$warning, is_overfit = TRUE) +
                       make_metric_plot(report, "Bias", "Directional Model Bias", theme_colors$accent)) +
    patchwork::plot_layout(ncol = 2) +
    patchwork::plot_annotation(title = "Generalization Risks and Structural Bias Diagnostics", theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)))

  p_tradeoff_assembled <- make_bias_variance_combined_plot(report, "Bias-Variance Joint Mapping Space", palette_style) +
    patchwork::plot_annotation(title = "Comprehensive Bias-Variance Tradeoff Dashboard", theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)))

  ks_data <- report
  ks_data$Model <- factor(ks_data$Model, levels = ks_data$Model[order(ks_data$KS_p_value)])
  p_ks_assembled <- ggplot2::ggplot(ks_data, ggplot2::aes(x = Model, y = KS_p_value)) +
    ggplot2::geom_col(fill = theme_colors$ks_fill, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.4f", KS_p_value)), hjust = -0.1, size = 2.5, fontface = "bold") +
    ggplot2::geom_hline(yintercept = 0.05, color = theme_colors$ks_line1, linetype = "dotted", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = 0.10, color = theme_colors$ks_line2, linetype = "dotted", linewidth = 0.8) +
    ggplot2::coord_flip() + ggplot2::theme_minimal(base_size = 10) +
    ggplot2::labs(title = "Kolmogorov-Smirnov Test p-values", x = "Model Architecture", y = "KS Test p-value") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"), plot.title = ggplot2::element_text(face = "bold", size = 13)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.22)))

  p_ops_metrics <- (make_metric_plot(report, "Variance", "Model Prediction Variance", theme_colors$highlight) +
                      make_metric_plot(report, "Duration", "Training Duration (Seconds)", theme_colors$highlight)) +
    patchwork::plot_layout(ncol = 2) +
    patchwork::plot_annotation(title = "Operational Footprints and Variance Metrics", theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)))

  heat_data <- report
  rownames(heat_data) <- heat_data$Model
  heat_metrics <- heat_data[, c("Testing_RMSE", "Testing_MAE", "Adjusted_R2", "Overfitting", "Bias", "Variance", "Duration")]
  heat_scaled <- as.data.frame(scale(heat_metrics))
  heat_scaled$Model <- rownames(heat_scaled)
  heat_long <- tidyr::pivot_longer(heat_scaled, cols = -Model, names_to = "Metric", values_to = "Z_Score")

  p_heatmap <- ggplot2::ggplot(heat_long, ggplot2::aes(x = Metric, y = Model, fill = Z_Score)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = theme_colors$tiles_low, mid = theme_colors$tiles_mid, high = theme_colors$tiles_high, name = "Relative Score\n(Z-Score)") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Comparative Performance Metric Heatmap Matrix", subtitle = "Standardized scale comparison across structural model signatures.", x = "Evaluation Metric", y = "Architecture Selection")
  eda_plots$metric_heatmap <- p_heatmap

  draw_top3_panel <- function() {
    is_interactive <- interactive()
    plot_margins   <- if (is_interactive) c(4.5, 4.5, 2.5, 1.5) else c(2.0, 2.0, 1.5, 0.5)
    axis_padding   <- if (is_interactive) c(3.0, 1.0, 0.0) else c(1.2, 0.3, 0.0)
    label_scaling  <- if (is_interactive) 1.0 else 0.75

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par))

    graphics::par(mfrow = c(3, 4), mar = plot_margins, mgp = axis_padding, cex.lab = label_scaling, cex.axis = label_scaling, cex.main = label_scaling)

    for (m_name in top_3_models) {
      p_test <- pred_test_list[[m_name]]
      residuals <- actual_test - p_test
      plot(actual_test, p_test, xlab = "Actual Value", ylab = "Predicted Value", main = paste(m_name, ": Pred vs Act"), col = theme_colors$primary, pch = 16)
      abline(a = 0, b = 1, col = theme_colors$warning, lwd = 2, lty = 2)
      hist(residuals, xlab = "Residual Error", col = theme_colors$primary, main = paste(m_name, ": Res Hist"), breaks = 15, border = "white")
      box()
      qqnorm(residuals, main = paste(m_name, ": Normal Q-Q Plot"), col = theme_colors$highlight, pch = 16)
      qqline(residuals, col = theme_colors$warning, lwd = 2)

      if (grepl("\\+", m_name) || grepl("Meta_", m_name)) {
        graphics::par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Ensemble / Meta:\nVarImp Matrix\nNot Supported", cex = if(is_interactive) 1.0 else 0.7, col = theme_colors$secondary, font = 2)
        graphics::par(mar = plot_margins)
      } else {
        tryCatch({
          mod <- models_list[[m_name]]
          imp <- caret::varImp(mod)
          imp_df <- if (!is.null(imp$importance)) imp$importance else imp
          score_vec <- rowSums(as.matrix(imp_df))
          barplot(rev(head(sort(score_vec, decreasing = TRUE), 5)), horiz = TRUE, col = theme_colors$secondary, border = "white", las = 1, main = paste(m_name, ": Top 5"), cex.names = (label_scaling * 0.9))
          box()
        }, error = function(e) {
          graphics::par(mar = c(0, 0, 0, 0))
          plot.new()
          text(0.5, 0.5, "Variable Importance\nNot Supported", cex = if(is_interactive) 1.1 else 0.7, col = theme_colors$warning)
          graphics::par(mar = plot_margins)
        })
      }
    }
  }

  draw_diagnostics_panel <- function() {
    is_interactive <- interactive()
    plot_margins   <- if (is_interactive) c(4.5, 4.5, 2.5, 1.5) else c(2.0, 2.0, 1.5, 0.5)
    axis_padding   <- if (is_interactive) c(3.0, 1.0, 0.0) else c(1.2, 0.3, 0.0)
    label_scaling  <- if (is_interactive) 1.0 else 0.75

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(par(old_par))

    graphics::par(mfrow = c(3, 3), mar = plot_margins, mgp = axis_padding, cex.lab = label_scaling, cex.axis = label_scaling, cex.main = label_scaling)

    for (m_name in top_3_models) {
      p_test <- pred_test_list[[m_name]]
      p_train <- pred_train_list[[m_name]]
      res_test <- actual_test - p_test
      res_train <- actual_train - p_train
      sqrt_abs_res <- sqrt(abs(res_test / stats::sd(res_test)))

      plot(p_test, sqrt_abs_res, xlab = "Predicted", ylab = "Sqrt Abs Res", main = paste(m_name, ": Scale-Loc"), col = theme_colors$accent, pch = 16)
      try(lines(stats::lowess(p_test, sqrt_abs_res), col = theme_colors$warning, lwd = 2), silent = TRUE)

      strong_feature <- NULL
      if (!grepl("\\+", m_name) && !grepl("Meta_", m_name)) { strong_feature <- tryCatch({ top_pred_names[[m_name]] }, error = function(e) { NULL }) }
      if (is.null(strong_feature) || !(strong_feature %in% colnames(test_data))) {
        available_cols <- colnames(test_data)[colnames(test_data) != target_col]
        numeric_avail = available_cols[sapply(test_data[available_cols], is.numeric)]
        if (length(numeric_avail) > 0) strong_feature <- numeric_avail[1]
      }
      feature_vector <- test_data[[strong_feature]]

      if (!is.null(feature_vector) && any(is.finite(feature_vector))) {
        plot(feature_vector, res_test, xlab = strong_feature, ylab = "Residuals", main = paste(m_name, ": Res vs Feat"), col = theme_colors$ks_fill, pch = 16)
        abline(h = 0, col = theme_colors$highlight, lwd = 2, lty = 3)
      } else {
        graphics::par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Feature Vector\nNot Available", col = theme_colors$warning, cex = if(is_interactive) 1.0 else 0.7)
        graphics::par(mar = plot_margins)
      }

      dens_train <- stats::density(res_train); dens_test = stats::density(res_test)
      plot(dens_train, xlim = range(c(dens_train$x, dens_test$x)), ylim = range(c(dens_train$y, dens_test$y)), main = paste(m_name, ": Density"), col = theme_colors$secondary, lwd = 2)
      lines(dens_test, col = theme_colors$primary, lwd = 2, lty = 2)
    }
  }

  graphics::par(mfrow = c(1, 1))

  # 9. CONSTRUCT WRAPPED RETURN OBJECT
  output_results <- list(
    performance_report = report,
    audit_report = audit_report_matrix,
    base_models = models_list,
    meta_models = list(GLM = meta_model_glm, RF = meta_model_rf, SVM = meta_model_svm),
    data_dictionary = data_dict,
    data_head = data_head_table,
    data_summary = data_summary_table,
    data_correlation = data_correlation_matrix,
    vif_report = vif_report_table,
    pipeline_meta = list(
      target_col = target_col,
      stratify_col = stratify_col,
      kept_features = kept_vif_features,
      dummy_model = dummy_model,
      palette_style = palette_style,
      train_data_ref = train_data
    ),
    plots = list(
      histograms = eda_plots$histograms,
      boxplots = eda_plots$boxplots,
      correlation = eda_plots$correlation,
      scatter_matrix = eda_plots$scatter_matrix,
      metric_heatmap = eda_plots$metric_heatmap,
      kpis = p_kpi_metrics,
      risks = p_risk_metrics,
      ops = p_ops_metrics,
      tradeoff = p_tradeoff_assembled,
      ks_test = p_ks_assembled,
      draw_top3 = draw_top3_panel,
      draw_diagnostics = draw_diagnostics_panel
    )
  )

  class(output_results) <- "numeric_pipeline"
  return(invisible(output_results))
}

# =========================================================================
# S3 OBJECT METHOD DEFINITIONS & SERIALIZATION MODULES
# =========================================================================

#' Print Numeric Pipeline Summary Report
#'
#' @param x A numeric_pipeline object generated by the Numeric() engine.
#' @param ... Additional arguments passed to underlying print methods.
#' @export
#' @method print numeric_pipeline
print.numeric_pipeline <- function(x, ...) {
  cat("\n=== NUMERIC PIPELINE EVALUATION OBJECT ===\n")
  cat(sprintf("Total Models Analyzed: %d\n", nrow(x$performance_report)))
  if (!is.null(x$pipeline_meta$stratify_col) && x$pipeline_meta$stratify_col != "") {
    cat(sprintf("Sampling Protocol: Stratified Sampling based on column '%s'\n", x$pipeline_meta$stratify_col))
  } else {
    cat("Sampling Protocol: Standard Population Split\n")
  }

  cat("\nTop 5 Architectures By Testing RMSE:\n")
  print(head(x$performance_report[, c("Model", "Testing_RMSE", "Testing_MAE", "Adjusted_R2", "Overfitting", "Bias", "Duration")], 5), row.names = FALSE)

  cat("\n=== AUTOMATED RESIDUAL DIAGNOSTIC AUDIT (TOP 3) ===\n")
  print(x$audit_report, row.names = FALSE)

  if (any(x$audit_report$Variance_Stability == "Heteroscedastic (Unstable Variance)")) {
    cat("\n[Audit Alert]: Heteroscedasticity caught. Intervals could degrade away from spatial dense zones.\n")
  }
  if (any(x$audit_report$Residual_Normality == "Non-Normal (Biased Tail Risks)")) {
    cat("[Audit Alert]: Non-normal residuals mapped. Downstream point inferences possess non-Gaussian fat tails.\n")
  }

  cat("\nTo render active diagnostic dashboards, evaluate:\n  plot(object)\n")
}

#' Plot Numeric Pipeline Performance Metrics and Visual Diagnostics
#'
#' @param x A numeric_pipeline object generated by the Numeric() engine.
#' @param pace_output Logical. If TRUE and session is interactive, paces chart pages. Default = TRUE.
#' @param ... Additional arguments passed to underlying plot methods.
#' @export
#' @method plot numeric_pipeline
plot.numeric_pipeline <- function(x, pace_output = TRUE, ...) {
  if (pace_output && interactive()) {
    old_ask <- grDevices::devAskNewPage(ask = TRUE)
    on.exit(grDevices::devAskNewPage(old_ask))
    cat("\n[Interactive Mode]: Pacing layouts. Press <Return> or click on the device to cycle frames.\n")
  }

  # --- WINDOW 1: EXPLORATORY DATA ANALYSIS CHARTS ---
  if (!is.null(x$plots$histograms))     { cat("Plotting Feature Histograms...\n"); print(x$plots$histograms) }
  if (!is.null(x$plots$boxplots))       { cat("Plotting Feature Range Profiles...\n"); print(x$plots$boxplots) }
  if (!is.null(x$plots$correlation))    { cat("Plotting Correlation Space...\n"); print(x$plots$correlation) }
  if (!is.null(x$plots$scatter_matrix)) { cat("Plotting Continuous Target Scatters...\n"); print(x$plots$scatter_matrix) }

  # --- REFOCUSED PERFORMANCE WINDOWS (THE HOLY TRINITY DASHBOARD) ---
  cat("Plotting Core Predictive KPIs (RMSE, MAE, Adj R2 with 95% CIs) (Window 1)...\n")
  print(x$plots$kpis)

  cat("Plotting Generalization Risks & Overfit Metrics (Window 2)...\n")
  print(x$plots$risks)

  cat("Plotting Operational Execution Footprints (Window 3)...\n")
  print(x$plots$ops)

  cat("Plotting Structural Bias-Variance Joint Spatial Maps...\n")
  print(x$plots$tradeoff)

  cat("Plotting Empirical Distribution KS p-values...\n")
  print(x$plots$ks_test)

  if (!is.null(x$plots$metric_heatmap)) { cat("Plotting Comprehensive Score Heatmaps...\n"); print(x$plots$metric_heatmap) }

  # --- WINDOWS 4 & 5: BASE GRAPHICS PERFORMANCE INTERFACES ---
  cat("Rendering Base Residual Fit Matrix Panels...\n")
  x$plots$draw_top3()

  cat("Rendering Base Scale-Location Diagnostics & Residual Density Curves...\n")
  x$plots$draw_diagnostics()
}

#' Predict with Numeric Pipeline Framework
#'
#' Generates predictions across new datasets matching models evaluated in the pipeline.
#'
#' @param object A trained \code{numeric_pipeline} object.
#' @param newdata A data.frame containing new data configurations to score.
#' @param model_name Character string specifying the target model from the leaderboard to score. Default = "best".
#' @param ... Additional arguments passed to underlying predict methods.
#' @return A numeric vector of predictions.
#' @export
#' @method predict numeric_pipeline
predict.numeric_pipeline <- function(object, newdata, model_name = "best", ...) {
  if (is.null(object)) stop("Argument 'object' must be a valid trained numeric_pipeline.", call. = FALSE)
  if (missing(newdata)) stop("Argument 'newdata' must be provided.", call. = FALSE)

  df_new <- as.data.frame(newdata)

  if (model_name == "best") {
    model_name <- object$performance_report$Model[1]
  }

  # DEFENSIVE METADATA DISCOVERY PROTOCOL
  expected_variables <- NULL

  if (!is.null(object$pipeline_meta$dummy_model)) {
    dm <- object$pipeline_meta$dummy_model
    if (is.list(dm) && "vars" %in% names(dm)) {
      if (is.list(dm$vars) && "predictors" %in% names(dm$vars)) {
        expected_variables <- names(dm$vars$predictors)
      } else if (is.character(dm$vars)) {
        expected_variables <- dm$vars
      }
    }
    if (is.null(expected_variables) && "lvls" %in% names(dm)) {
      expected_variables <- colnames(dm$lvls)
    }
  }

  if (is.null(expected_variables)) {
    expected_variables <- colnames(object$pipeline_meta$train_data_ref)
    expected_variables <- expected_variables[expected_variables != object$pipeline_meta$target_col]
  }

  # CRITICAL ENFORCEMENT RAIL: Safely fill dropped collinear predictors to satisfy dummyVars
  for (v in expected_variables) {
    if (!(v %in% colnames(df_new))) {
      df_new[[v]] <- 0
    }
  }

  # Isolate and align columns via cached baseline configurations
  new_encoded <- data.frame(predict(object$pipeline_meta$dummy_model, newdata = df_new, na.action = stats::na.pass))

  # Impute missing indicators via cached training matrix profiles
  for (col in colnames(new_encoded)) {
    if (col %in% colnames(object$pipeline_meta$train_data_ref) && any(is.na(new_encoded[[col]]))) {
      fallback_median <- stats::median(object$pipeline_meta$train_data_ref[[col]], na.rm = TRUE)
      new_encoded[is.na(new_encoded[[col]]), col] <- fallback_median
    }
  }

  # Routing prediction paths matching compound ensemble keys vs meta-learners vs base algorithms
  if (grepl("\\+", model_name)) {
    sub_models <- strsplit(model_name, "\\+")[[1]]
    p1 <- as.numeric(predict(object$base_models[[sub_models[1]]], newdata = new_encoded, na.action = stats::na.pass))
    p2 <- as.numeric(predict(object$base_models[[sub_models[2]]], newdata = new_encoded, na.action = stats::na.pass))
    return((p1 + p2) / 2)
  }

  if (grepl("Meta_", model_name)) {
    meta_key <- strsplit(model_name, "Meta_")[[1]][2]
    base_preds <- lapply(object$base_models[1:17], function(mod) {
      as.numeric(predict(mod, newdata = new_encoded, na.action = stats::na.pass))
    })
    meta_features <- as.data.frame(base_preds)
    return(as.numeric(predict(object$meta_models[[meta_key]], newdata = meta_features)))
  }

  if (model_name %in% names(object$base_models)) {
    return(as.numeric(predict(object$base_models[[model_name]], newdata = new_encoded, na.action = stats::na.pass)))
  }

  stop(sprintf("Model identifier '%s' not recognized within this pipeline's asset collection.", model_name), call. = FALSE)
}

#' Save Serialized Numeric Pipeline Environment to Disk File
#'
#' @param object A numeric_pipeline object.
#' @param file_path Character string tracking destination directory file path.
#' @export
save_pipeline <- function(object, file_path) {
  if (!inherits(object, "numeric_pipeline")) {
    stop("Object must be a valid 'numeric_pipeline' generated by the Numeric() engine.", call. = FALSE)
  }

  saved_bundle <- list(
    performance_report = object$performance_report,
    audit_report       = object$audit_report,
    base_models        = object$base_models,
    meta_models        = object$meta_models,
    pipeline_meta      = object$pipeline_meta
  )
  class(saved_bundle) <- "serialized_numeric_pipeline"

  tryCatch({
    saveRDS(saved_bundle, file = file_path)
    cat(sprintf("\n[Pipeline Serialization]: Successfully saved pipeline environment to: '%s'\n", file_path))
  }, error = function(e) {
    stop(sprintf("Failed to write pipeline disk file. System trace: %s", e$message), call. = FALSE)
  })
}

#' Load Serialized Numeric Pipeline Environment from Disk File
#'
#' @param file_path Character string tracking destination directory file path.
#' @return A re-hydrated numeric_pipeline object.
#' @export
load_pipeline <- function(file_path) {
  if (!file.exists(file_path)) stop(sprintf("Target serialized pipeline file not found at path: '%s'", file_path), call. = FALSE)

  bundle <- tryCatch({
    readRDS(file_path)
  }, error = function(e) {
    stop(sprintf("Failed to read serialized RDS structure. Trace: %s", e$message), call. = FALSE)
  })

  if (!inherits(bundle, "serialized_numeric_pipeline")) {
    stop("The file provided does not contain a valid structured 'serialized_numeric_pipeline' footprint.", call. = FALSE)
  }

  class(bundle) <- "numeric_pipeline"
  cat(sprintf("\n[Pipeline Serialization]: Successfully re-hydrated pipeline environment from: '%s'\n", file_path))
  return(bundle)
}

#' Launch Interactive NumericEnsembles Web Interface App
#'
#' Fires up a local instance of the interactive OLS vs. GLM tuning,
#' diagnostic, and residual validation web dashboard system.
#'
#' @export
LaunchNumericApp <- function() {
  app_path <- system.file("shiny-apps", "NumericEnsemblesApp", package = "NumericEnsembles")
  if (app_path == "") {
    stop("Shiny application dashboard directory not found within package files library installation.", call. = FALSE)
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' must be installed to activate the web interface app framework.", call. = FALSE)
  }

  launch_opt <- getOption("shiny.launch.browser", TRUE)
  is_launch_active <- if (is.logical(launch_opt)) launch_opt else TRUE

  if (is_launch_active && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    shiny::runApp(app_path, launch.browser = rstudioapi::viewer, display.mode = "normal")
  } else {
    shiny::runApp(app_path, display.mode = "normal")
  }
}
