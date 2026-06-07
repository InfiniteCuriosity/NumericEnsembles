# =========================================================================
# COMPREHENSIVE PERFORMANCE PIPELINE ENGINE WITH ADVANCED DIAGNOSTICS & IO
# =========================================================================

#' @importFrom dplyr select mutate across everything group_by summarise n rename arrange desc all_of
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal labs geom_abline geom_hline geom_col geom_histogram geom_boxplot geom_tile scale_fill_gradient2 scale_fill_identity scale_color_identity scale_y_continuous scale_color_viridis_c scale_color_gradient expansion element_text element_blank
#' @importFrom patchwork plot_layout wrap_plots plot_annotation
#' @importFrom rstudioapi isAvailable viewer
#' @importFrom shiny runApp fluidPage titlePanel sidebarLayout sidebarPanel helpText fileInput uiOutput sliderInput actionButton selectInput mainPanel tabsetPanel tabPanel verbatimTextOutput plotOutput tableOutput renderUI renderPrint renderPlot renderTable shinyApp numericInput p hr br req reactive eventReactive
#' @importFrom stats na.omit var cor as.formula predict glm median sd rnorm runif rpois rgamma rbinom ks.test shapiro.test cor.test density lowess qqnorm qqline na.exclude
#' @importFrom utils globalVariables head write.csv txtProgressBar setTxtProgressBar read.csv combn
#' @importFrom grDevices png dev.off devAskNewPage
#' @importFrom ggrepel geom_text_repel
#' @importFrom car vif
#' @importFrom caret train trainControl dummyVars createDataPartition varImp
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".data", "Value", "Feature", "Var1", "Var2", "Correlation",
    "Model", "Absolute_Bias", "Variance", "Testing_RMSE", "Testing_MAE",
    "Adjusted_R2", "KS_p_value", "Metric", "Z_Score", "Bias", "Overfitting",
    "RMSE 95% CI Lower", "RMSE 95% CI Upper", "MAE 95% CI Lower", "MAE 95% CI Upper",
    "Adjusted R2 95% CI Lower", "Adjusted R2 95% CI Upper"
  ))
}

# -------------------------------------------------------------------------
# 1. ARCHITECTURAL PARAMETER CONFIGURATION MATRIX
# -------------------------------------------------------------------------

#' Configuration Parameters Matrix for Numeric Ensemble Pipeline
#'
#' @param cv_folds Integer. Number of cross-validation folds. Default 5.
#' @param train_pct Decimal fraction between 0 and 1 for the training split. Default 0.60.
#' @param vif_threshold Numeric. Maximum allowed Variance Inflation Factor. Default 5.
#' @param transform_steps Character vector. Preprocessing transformations applied via caret.
#' @param rf_grid Data frame or NULL. Explicit tuning grid for Random Forest.
#' @param glmnet_grid Data frame. Hyperparameter grid for elastic net.
#' @param svm_tune_length Integer. Total tuning resolution steps for svmRadial. Default 8.
#' @param mars_tune_length Integer. Total tuning resolution steps for MARS. Default 5.
#' @param pcr_tune_length Integer. Total tuning resolution steps for PCR. Default 10.
#' @param tree_tune_length Integer. Total tuning resolution steps for decision trees. Default 10.
#' @return A structured list containing isolated operational tuning parameters.
#' @export
NumericEnsemblesConfig <- function(cv_folds = 5,
                                   train_pct = 0.60,
                                   vif_threshold = 5,
                                   transform_steps = c("nzv", "medianImpute", "center", "scale", "YeoJohnson"),
                                   rf_grid = NULL,
                                   glmnet_grid = expand.grid(alpha = seq(0, 1, length = 5),
                                                             lambda = seq(0.001, 0.2, length = 10)),
                                   svm_tune_length = 8,
                                   mars_tune_length = 5,
                                   pcr_tune_length = 10,
                                   tree_tune_length = 10) {
  if (train_pct <= 0 || train_pct >= 1) {
    stop("Argument 'train_pct' must be a decimal fraction strictly between 0 and 1.", call. = FALSE)
  }
  if (cv_folds < 2) {
    stop("Argument 'cv_folds' must be an integer greater than or equal to 2.", call. = FALSE)
  }

  list(
    cv_folds         = as.integer(cv_folds),
    train_pct        = train_pct,
    vif_threshold    = vif_threshold,
    transform_steps  = transform_steps,
    rf_grid          = rf_grid,
    glmnet_grid      = glmnet_grid,
    svm_tune_length  = as.integer(svm_tune_length),
    mars_tune_length = as.integer(mars_tune_length),
    pcr_tune_length  = as.integer(pcr_tune_length),
    tree_tune_length = as.integer(tree_tune_length)
  )
}

#' Fast-Execution Configuration Matrix
#'
#' @return A structured list containing optimized, fast-track hyperparameter settings.
#' @export
NumericEnsemblesFastConfig <- function() {
  NumericEnsemblesConfig(
    cv_folds         = 2,
    train_pct        = 0.60,
    vif_threshold    = 5,
    transform_steps  = c("nzv", "medianImpute", "center", "scale"),
    rf_grid          = expand.grid(mtry = 2),
    glmnet_grid      = expand.grid(alpha = c(0, 0.5, 1), lambda = c(0.01, 0.1)),
    svm_tune_length  = 2,
    mars_tune_length = 2,
    pcr_tune_length  = 2,
    tree_tune_length = 2
  )
}

# -------------------------------------------------------------------------
# 2. INTERNAL ISOLATED GRAPHICS & DIAGNOSTIC SUB-ENGINES
# -------------------------------------------------------------------------

.make_metric_plot <- function(data, metric_col, title, fill_color, theme_colors, is_overfit = FALSE, show_ci = FALSE, ci_lower_col = NULL, ci_upper_col = NULL) {
  data$Model <- factor(data$Model, levels = rev(data$Model))
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

  p <- p + ggplot2::coord_flip() + ggplot2::theme_minimal(base_size = 9) + ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"), plot.title = ggplot2::element_text(face = "bold", size = 11))

  if (is_overfit) p <- p + ggplot2::geom_hline(yintercept = 1.0, color = theme_colors$warning, linetype = "dashed")
  p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = if (show_ci) c(0.08, 0.22) else c(0, 0.22)))
  return(p)
}

.make_bias_variance_combined_plot <- function(data, title, theme_colors, palette_style = "standard") {
  data$Absolute_Bias <- abs(data$Bias)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = Absolute_Bias, y = Variance, label = Model)) +
    ggplot2::geom_point(ggplot2::aes(color = Testing_RMSE), size = 4, alpha = 0.8)

  if (palette_style == "viridis") {
    p <- p + ggplot2::scale_color_viridis_c(option = "viridis", direction = -1, name = "Testing RMSE")
  } else if (palette_style == "modern") {
    p <- p + ggplot2::scale_color_gradient(low = theme_colors$primary, high = theme_colors$secondary, name = "Testing RMSE")
  } else {
    p <- p + ggplot2::scale_color_gradient(low = "darkgreen", high = "firebrick2", name = "Testing RMSE")
  }

  p = p +
    ggrepel::geom_text_repel(size = 2.5, fontface = "bold", max.overlaps = 15, box.padding = 0.3) +
    ggplot2::annotate("point", x = 0, y = 0, color = "gold", shape = 18, size = 6) +
    ggplot2::annotate("text", x = 0, y = 0, label = "Ideal (0,0)", vjust = -1.2, color = theme_colors$primary, fontface = "bold", size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title, subtitle = "Lower values are better. Color scales indicate overall Testing RMSE.", x = "Absolute Bias", y = "Prediction Variance") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13), legend.position = "right")
  return(p)
}

.draw_top3_panel <- function(top_3_models, pred_test_list, actual_test, models_list, train_data, target_col, theme_colors) {
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
    graphics::plot(actual_test, p_test, xlab = "Actual Value", ylab = "Predicted Value", main = paste(m_name, ": Pred vs Act"), col = theme_colors$primary, pch = 16)
    graphics::abline(a = 0, b = 1, col = theme_colors$warning, lwd = 2, lty = 2)
    graphics::hist(residuals, xlab = "Residual Error", col = theme_colors$primary, main = paste(m_name, ": Res Hist"), breaks = 15, border = "white")
    graphics::box()
    stats::qqnorm(residuals, main = paste(m_name, ": Normal Q-Q Plot"), col = theme_colors$highlight, pch = 16)
    stats::qqline(residuals, col = theme_colors$warning, lwd = 2)

    if (grepl("\\+", m_name) || grepl("Meta_", m_name)) {
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Ensemble / Meta:\nVarImp Matrix\nNot Supported", cex = if(is_interactive) 1.0 else 0.7, col = theme_colors$secondary, font = 2)
      graphics::par(mar = plot_margins)
    } else {
      tryCatch({
        mod <- models_list[[m_name]]
        imp <- caret::varImp(mod)
        imp_df <- if (!is.null(imp$importance)) imp$importance else imp
        score_vec <- rowSums(as.matrix(imp_df))
        graphics::barplot(rev(utils::head(sort(score_vec, decreasing = TRUE), 5)), horiz = TRUE, col = theme_colors$secondary, border = "white", las = 1, main = paste(m_name, ": Top 5"), cex.names = (label_scaling * 0.9))
        graphics::box()
      }, error = function(e) {
        graphics::par(mar = c(0, 0, 0, 0))
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Variable Importance\nNot Supported", cex = if(is_interactive) 1.1 else 0.7, col = theme_colors$warning)
        graphics::par(mar = plot_margins)
      })
    }
  }
}

.draw_diagnostics_panel <- function(top_3_models, pred_test_list, pred_train_list, actual_test, actual_train, test_data, target_col, theme_colors, top_pred_names) {
  is_interactive <- interactive()
  plot_margins   <- if (is_interactive) c(4.5, 4.5, 2.5, 1.5) else c(2.0, 2.0, 1.5, 0.5)
  axis_padding   <- if (is_interactive) c(3.0, 1.0, 0.0) else c(1.2, 0.3, 0.0)
  label_scaling  <- if (is_interactive) 1.0 else 0.75

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  graphics::par(mfrow = c(3, 3), mar = plot_margins, mgp = axis_padding, cex.lab = label_scaling, cex.axis = label_scaling, cex.main = label_scaling)

  for (m_name in top_3_models) {
    p_test <- pred_test_list[[m_name]]
    p_train <- pred_train_list[[m_name]]
    res_test <- actual_test - p_test
    res_train <- actual_train - p_train
    sqrt_abs_res <- sqrt(abs(res_test / stats::sd(res_test)))

    graphics::plot(p_test, sqrt_abs_res, xlab = "Predicted", ylab = "Sqrt Abs Res", main = paste(m_name, ": Scale-Loc"), col = theme_colors$accent, pch = 16)
    try(graphics::lines(stats::lowess(p_test, sqrt_abs_res), col = theme_colors$warning, lwd = 2), silent = TRUE)

    strong_feature <- NULL
    if (!grepl("\\+", m_name) && !grepl("Meta_", m_name)) { strong_feature <- tryCatch({ top_pred_names[[m_name]] }, error = function(e) { NULL }) }
    if (is.null(strong_feature) || !(strong_feature %in% colnames(test_data))) {
      available_cols <- colnames(test_data)[colnames(test_data) != target_col]
      numeric_avail = available_cols[sapply(test_data[available_cols], is.numeric)]
      if (length(numeric_avail) > 0) strong_feature <- numeric_avail[1]
    }
    feature_vector <- test_data[[strong_feature]]

    if (!is.null(feature_vector) && any(is.finite(feature_vector))) {
      graphics::plot(feature_vector, res_test, xlab = strong_feature, ylab = "Residuals", main = paste(m_name, ": Res vs Feat"), col = theme_colors$ks_fill, pch = 16)
      graphics::abline(h = 0, col = theme_colors$highlight, lwd = 2, lty = 3)
    } else {
      graphics::plot.new()
    }

    dens_train <- stats::density(res_train); dens_test = stats::density(res_test)
    graphics::plot(dens_train, xlim = range(c(dens_train$x, dens_test$x)), ylim = range(c(dens_train$y, dens_test$y)), main = paste(m_name, ": Density"), col = theme_colors$secondary, lwd = 2)
    graphics::lines(dens_test, col = theme_colors$primary, lwd = 2, lty = 2)
  }
}

# -------------------------------------------------------------------------
# 3. CORE PERFORMANCE PIPELINE ENGINE INTERFACE
# -------------------------------------------------------------------------

#' Core Performance Pipeline Engine for Continuous Data
#'
#' @param dataset A data.frame containing continuous target outputs and features.
#' @param target_col Character string specifying the name of the target column.
#' @param facet_col Character string specifying a column to facet EDA charts by. Default = "".
#' @param color_col Character string specifying a column to color EDA charts by. Default = "".
#' @param stratify_col Character string specifying a categorical column to anchor stratified sampling splits. Default = "".
#' @param palette_style Character string choosing a color palette: "standard", "viridis", or "modern".
#' @param config A pre-configured architecture parameter matrix block from \code{NumericEnsemblesConfig()}.
#' @param verbose Logical console logging status indicator. Default TRUE.
#' @return An integrated execution bundle data object of class type \code{numeric_pipeline}.
#' @export
Numeric <- function(dataset = NULL,
                    target_col = NULL,
                    facet_col = "",
                    color_col = "",
                    stratify_col = "",
                    palette_style = c("standard", "viridis", "modern"),
                    config = NumericEnsemblesConfig(),
                    verbose = TRUE) {

  required_packages <- c("caret", "MASS", "rpart", "Cubist",
                         "nnet", "ipred", "elasticnet", "glmnet",
                         "randomForest", "kernlab", "brnn", "earth",
                         "pls", "quantregForest", "party", "car",
                         "partykit", "doParallel", "ggplot2", "patchwork",
                         "tidyr", "GGally", "ggrepel", "tidyselect", "gam")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Required package '%s' is missing. Please install it before running the pipeline.", pkg), call. = FALSE)
    }
  }

  old_warn <- options(warn = -1)
  on.exit(options(old_warn), add = TRUE)

  if (verbose) cat("--- Comprehensive Machine Learning Pipeline ---\n")

  if (is.null(dataset)) {
    if (interactive()) {
      dataset_name <- trimws(readline(prompt = "Enter the name of the dataset (e.g., MASS::Boston): "))
      df <- tryCatch({
        eval(parse(text = dataset_name))
      }, error = function(e) { stop("Could not find or load the specified dataset.", call. = FALSE) })
    } else {
      stop("Argument 'dataset' must be supplied explicitly in non-interactive mode.", call. = FALSE)
    }
  } else {
    df <- dataset
  }

  if (is.null(target_col)) {
    if (interactive()) {
      cat("\nAvailable columns for Target Variable:\n  ->", paste(colnames(df), collapse = ", "), "\n")
      target_col <- trimws(readline(prompt = "Enter the name of the target column: "))
    } else {
      stop("Argument 'target_col' must be supplied explicitly in non-interactive mode.", call. = FALSE)
    }
  }
  if (!(target_col %in% colnames(df))) stop("Target column not found in dataset.", call. = FALSE)

  if (stratify_col != "") {
    if (!(stratify_col %in% colnames(df))) {
      stop(sprintf("Stratification column '%s' not found in the dataset.", stratify_col), call. = FALSE)
    }
    strat_val <- df[[stratify_col]]
    if (is.numeric(strat_val) && length(unique(stats::na.omit(strat_val))) > 15) {
      stop(sprintf("Column '%s' is continuous numeric. Stratified sampling splits require a discrete factor or categorical column.", stratify_col), call. = FALSE)
    }
  }

  palette_style <- match.arg(palette_style)
  theme_colors <- switch(palette_style,
                         "standard" = list(
                           primary   = "steelblue", secondary = "cyan4", accent = "purple",
                           highlight = "darkgreen", warning = "tomato", ks_fill = "darkorange2",
                           ks_line1  = "blue", ks_line2 = "red", tiles_low = "darkgreen",
                           tiles_mid = "white", tiles_high = "darkred"
                         ),
                         "viridis" = list(
                           primary   = "#21918c", secondary = "#3b528b", accent = "#440154",
                           highlight = "#5dc963", warning = "#fde725", ks_fill = "#21918c",
                           ks_line1  = "#440154", ks_line2 = "#fde725", tiles_low = "#440154",
                           tiles_mid = "#21918c", tiles_high = "#fde725"
                         ),
                         "modern" = list(
                           primary   = "#111E6C", secondary = "#FF6F61", accent = "#008080",
                           highlight = "#708090", warning = "#FF4500", ks_fill = "#FF6F61",
                           ks_line1  = "#111E6C", ks_line2 = "#FF4500", tiles_low = "#008080",
                           tiles_mid = "#ECEFF1", tiles_high = "#FF6F61"
                         )
  )

  if (verbose) cat("\n[Extracting Baseline Profiles]: Capturing Head, Summary, and Correlation matrices...\n")
  data_head_table <- utils::head(df, 6)
  data_summary_table <- summary(df)

  numeric_cols_idx <- sapply(df, is.numeric)
  if (sum(numeric_cols_idx) > 1) {
    data_correlation_matrix <- stats::cor(df[, numeric_cols_idx], use = "complete.obs")
  } else {
    data_correlation_matrix <- "Insufficient numeric columns to establish a correlation matrix."
  }

  data_dict <- data.frame(
    Feature = colnames(df),
    Type = sapply(df, function(x) paste(class(x), collapse = ", ")),
    Missing_Count = sapply(df, function(x) sum(is.na(x))),
    Missing_Pct = paste0(round(sapply(df, function(x) sum(is.na(x)) / length(x) * 100), 2), "%"),
    Unique_Values = sapply(df, function(x) length(unique(stats::na.omit(x)))),
    stringsAsFactors = FALSE
  )
  rownames(data_dict) <- NULL

  if (any(is.na(df[[target_col]]))) {
    num_missing <- sum(is.na(df[[target_col]]))
    if (verbose) cat(sprintf("\n[Preprocessing]: Dropping %d rows with missing Target values.\n", num_missing))
    df <- df[!is.na(df[[target_col]]), ]
  }

  if (verbose) cat("\n[EDA Engine]: Generating data distribution, correlation, and scatter plots...\n")
  eda_plots <- list()

  fill_aes <- if(color_col != "" && color_col %in% colnames(df)) ggplot2::aes(fill = .data[[color_col]]) else ggplot2::aes(fill = theme_colors$primary)
  color_aes <- if(color_col != "" && color_col %in% colnames(df)) ggplot2::aes(color = .data[[color_col]]) else ggplot2::aes(color = theme_colors$secondary)

  cols_to_pivot <- colnames(df)[sapply(df, is.numeric)]
  if (color_col != "" && color_col %in% colnames(df)) cols_to_pivot <- setdiff(cols_to_pivot, color_col)
  if (facet_col != "" && facet_col %in% colnames(df)) cols_to_pivot <- setdiff(cols_to_pivot, facet_col)

  df_long_all <- tidyr::pivot_longer(df, cols = tidyselect::all_of(cols_to_pivot), names_to = "Feature", values_to = "Value")

  p_hist <- ggplot2::ggplot(df_long_all, ggplot2::aes(x = Value)) +
    ggplot2::geom_histogram(fill_aes, bins = 20, color = "white", alpha = 0.8) +
    ggplot2::facet_wrap(~Feature, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Feature Distributions (Histograms)", x = "Value", y = "Count")
  if (color_col == "") p_hist <- p_hist + ggplot2::scale_fill_identity()
  if (facet_col != "" && facet_col %in% colnames(df)) p_hist <- p_hist + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)), scales = "free")
  eda_plots$histograms <- p_hist

  p_box <- ggplot2::ggplot(df_long_all, ggplot2::aes(y = Value, x = "Feature")) +
    ggplot2::geom_boxplot(fill_aes, alpha = 0.7, outlier.color = theme_colors$warning) +
    ggplot2::facet_wrap(~Feature, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Feature Range Profiles (Box Plots)", x = NULL, y = "Value") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  if (color_col == "") p_box <- p_box + ggplot2::scale_fill_identity()
  if (facet_col != "" && facet_col %in% colnames(df)) p_box <- p_box + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)), scales = "free")
  eda_plots$boxplots <- p_box

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
  if (color_col == "") p_scatter <- p_scatter + ggplot2::scale_color_identity()
  eda_plots$scatter_matrix <- p_scatter

  cores_to_use <- max(1, parallel::detectCores() - 1)
  is_build_env <- nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_")) ||
    nzchar(Sys.getenv("R_CMD")) ||
    nzchar(Sys.getenv("R_TESTS")) ||
    any(c("pkgdown", "knitr", "rmarkdown") %in% loadedNamespaces())

  if (is_build_env) cores_to_use <- min(2, cores_to_use)

  cl <- parallel::makePSOCKcluster(cores_to_use)
  doParallel::registerDoParallel(cl)
  on.exit({
    try({ parallel::stopCluster(cl); foreach::registerDoSEQ() }, silent = TRUE)
  }, add = TRUE)

  set.seed(42)
  if (stratify_col != "") {
    if (verbose) cat(sprintf("\n[Sampling Split]: Executing stratified partition based on factor column '%s'...\n", stratify_col))
    train_index <- caret::createDataPartition(df[[stratify_col]], p = config$train_pct, list = FALSE)
  } else {
    if (verbose) cat("\n[Sampling Split]: Executing regular population density split based on target feature values...\n")
    train_index <- caret::createDataPartition(df[[target_col]], p = config$train_pct, list = FALSE)
  }

  train_data  <- df[train_index, ]
  test_data   <- df[-train_index, ]

  if (verbose) cat("\n[Preprocessing]: Applying explicit dummy encoding to categorical predictors...\n")
  predictors_raw <- colnames(df)[colnames(df) != target_col]

  dummy_model <- caret::dummyVars(" ~ .", data = train_data[, predictors_raw, drop = FALSE], fullRank = TRUE)
  train_encoded <- data.frame(stats::predict(dummy_model, newdata = train_data, na.action = stats::na.pass))
  test_encoded  <- data.frame(stats::predict(dummy_model, newdata = test_data, na.action = stats::na.pass))

  train_data <- cbind(train_encoded, Target_Var = train_data[[target_col]])
  colnames(train_data)[ncol(train_data)] <- target_col

  test_data <- cbind(test_encoded, Target_Var = test_data[[target_col]])
  colnames(test_data)[ncol(test_data)] <- target_col

  numeric_features <- colnames(train_data)[colnames(train_data) != target_col]

  vif_report_table <- data.frame(Feature = character(), VIF = numeric(), Status = character(), stringsAsFactors = FALSE)
  kept_vif_features <- numeric_features

  if (config$vif_threshold > 0 && length(numeric_features) > 1) {
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

      if (max_vif > config$vif_threshold) {
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
    vif_report_table <- data.frame(Feature = "All", VIF = 0, Status = "VIF step bypassed", stringsAsFactors = FALSE)
  }

  formula_obj <- stats::as.formula(paste(target_col, "~ ."))
  cv_control <- caret::trainControl(method = "cv", number = config$cv_folds, allowParallel = TRUE)

  if (verbose) cat("\n[Modeling Phase]: Launching 17 base architectures concurrently...\n")
  final_predictor_count <- ncol(train_data) - 1
  rf_grid_dispatch <- if (is.null(config$rf_grid)) {
    expand.grid(mtry = intersect(c(2, 4, 6, 8), 1:final_predictor_count))
  } else {
    config$rf_grid
  }

  models_list <- list()
  durations_list <- list()

  start_time <- proc.time(); models_list[["Linear"]] <- caret::train(formula_obj, data = train_data, method = "lm", trControl = cv_control, preProcess = config$transform_steps, na.action = stats::na.pass); durations_list[["Linear"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["Tree"]] <- caret::train(formula_obj, data = train_data, method = "rpart", trControl = cv_control, tuneLength = config$tree_tune_length, na.action = stats::na.pass); durations_list[["Tree"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["Cubist"]] <- caret::train(formula_obj, data = train_data, method = "cubist", trControl = cv_control, na.action = stats::na.pass); durations_list[["Cubist"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["NeuralNet"]] <- caret::train(formula_obj, data = train_data, method = "nnet", trControl = cv_control, preProcess = config$transform_steps, linout = TRUE, trace = FALSE, na.action = stats::na.pass); durations_list[["NeuralNet"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["Bagging"]] <- caret::train(formula_obj, data = train_data, method = "treebag", trControl = cv_control, na.action = stats::na.pass); durations_list[["Bagging"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["Ridge"]] <- caret::train(formula_obj, data = train_data, method = "glmnet", trControl = cv_control, preProcess = config$transform_steps, tuneGrid = expand.grid(alpha = 0, lambda = config$glmnet_grid$lambda), na.action = stats::na.pass); durations_list[["Ridge"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["ElasticNet"]] <- caret::train(formula_obj, data = train_data, method = "glmnet", trControl = cv_control, preProcess = config$transform_steps, tuneGrid = config$glmnet_grid, na.action = stats::na.pass); durations_list[["ElasticNet"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["Lasso"]] <- caret::train(formula_obj, data = train_data, method = "glmnet", trControl = cv_control, preProcess = config$transform_steps, tuneGrid = expand.grid(alpha = 1, lambda = config$glmnet_grid$lambda), na.action = stats::na.pass); durations_list[["Lasso"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["RandomForest"]] <- caret::train(formula_obj, data = train_data, method = "rf", trControl = cv_control, tuneGrid = rf_grid_dispatch, importance = TRUE, na.action = stats::na.pass); durations_list[["RandomForest"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["SVM_Radial"]] <- caret::train(formula_obj, data = train_data, method = "svmRadial", trControl = cv_control, preProcess = config$transform_steps, tuneLength = config$svm_tune_length, na.action = stats::na.pass); durations_list[["SVM_Radial"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["BayesRNN"]] <- caret::train(formula_obj, data = train_data, method = "brnn", trControl = cv_control, preProcess = config$transform_steps, verbose = FALSE, na.action = stats::na.pass); durations_list[["BayesRNN"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["MARS"]] <- caret::train(formula_obj, data = train_data, method = "earth", trControl = cv_control, tuneLength = config$mars_tune_length, na.action = stats::na.pass); durations_list[["MARS"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["PCR"]] <- caret::train(formula_obj, data = train_data, method = "pcr", trControl = cv_control, preProcess = config$transform_steps, tuneLength = config$pcr_tune_length, na.action = stats::na.pass); durations_list[["PCR"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["QuantileRF"]] <- caret::train(formula_obj, data = train_data, method = "qrf", trControl = cv_control, na.action = stats::na.pass); durations_list[["QuantileRF"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["Bagged_MARS"]] <- caret::train(formula_obj, data = train_data, method = "bagEarth", trControl = cv_control, preProcess = config$transform_steps, na.action = stats::na.pass); durations_list[["Bagged_MARS"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["Cond_Inf_Forest"]] <- caret::train(formula_obj, data = train_data, method = "cforest", trControl = cv_control, controls = party::cforest_unbiased(ntree = 150), na.action = stats::na.pass); durations_list[["Cond_Inf_Forest"]] <- (proc.time() - start_time)[3]
  start_time <- proc.time(); models_list[["Averaged_NNet"]] <- caret::train(formula_obj, data = train_data, method = "avNNet", trControl = cv_control, preProcess = config$transform_steps, linout = TRUE, trace = FALSE, na.action = stats::na.pass); durations_list[["Averaged_NNet"]] <- (proc.time() - start_time)[3]

  actual_test  <- test_data[[target_col]]
  actual_train <- train_data[[target_col]]
  n_models     <- length(models_list)
  m_names      <- names(models_list)

  n_test <- length(actual_test)
  p_features <- length(kept_vif_features)
  ss_tot_test <- sum((actual_test - mean(actual_test))^2)
  var_actual_test <- stats::var(actual_test)

  pred_train_list <- lapply(models_list, function(mod) as.numeric(stats::predict(mod, newdata = train_data, na.action = stats::na.pass)))
  pred_test_list  <- lapply(models_list, function(mod) as.numeric(stats::predict(mod, newdata = test_data, na.action = stats::na.pass)))

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
    v_adj_r2_lower[i] <- round(adj_r2_lower_val, 4) # FIXED: Reference tracking variables correctly
    v_adj_r2_upper[i] <- round(adj_r2_upper_val, 4) # FIXED: Reference tracking variables correctly
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
    Model = v_model, Testing_RMSE = v_testing_rmse,
    `RMSE 95% CI Lower` = v_rmse_ci_lower, `RMSE 95% CI Upper` = v_rmse_ci_upper,
    Testing_MAE = v_testing_mae, `MAE 95% CI Lower` = v_mae_ci_lower, `MAE 95% CI Upper` = v_mae_ci_upper,
    Adjusted_R2 = v_adj_r2, `Adjusted R2 95% CI Lower` = v_adj_r2_lower, `Adjusted R2 95% CI Upper` = v_adj_r2_upper,
    Duration = v_duration, Overfitting = v_overfitting, Bias = v_bias, Variance = v_variance, KS_p_value = v_ks_p,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  if (verbose) cat("\n[Meta-Learner Engine]: Training 6 Advanced Stacking Meta-Learners...\n")
  meta_train_df <- as.data.frame(pred_train_list); meta_train_df[[target_col]] <- actual_train
  meta_test_df  <- as.data.frame(pred_test_list);  meta_test_df[[target_col]]  <- actual_test

  meta_control <- caret::trainControl(method = "cv", number = config$cv_folds, allowParallel = TRUE)
  meta_formula <- stats::as.formula(paste(target_col, "~ ."))

  start_t <- proc.time(); meta_model_glm <- caret::train(meta_formula, data = meta_train_df, method = "lm", trControl = meta_control); dur_meta_glm <- (proc.time() - start_t)[3]
  start_t <- proc.time()
  meta_grid <- expand.grid(alpha = seq(0, 1, length = 11), lambda = 10^seq(-4, -1, length = 30))
  meta_model_enet <- caret::train(meta_formula, data = meta_train_df, method = "glmnet", trControl = meta_control, tuneGrid = meta_grid, preProcess = c("center", "scale")); dur_meta_enet <- (proc.time() - start_t)[3]
  start_t <- proc.time(); meta_model_gam <- caret::train(meta_formula, data = meta_train_df, method = "gamSpline", trControl = meta_control); dur_meta_gam <- (proc.time() - start_t)[3]
  start_t <- proc.time(); meta_model_pls <- caret::train(meta_formula, data = meta_train_df, method = "pls", trControl = meta_control, tuneLength = 6, preProcess = c("center", "scale")); dur_meta_pls <- (proc.time() - start_t)[3]
  start_t <- proc.time(); meta_model_rf  <- caret::train(meta_formula, data = meta_train_df, method = "rf", trControl = meta_control, tuneLength = 3); dur_meta_rf <- (proc.time() - start_t)[3]
  start_t = proc.time(); meta_model_svm <- caret::train(meta_formula, data = meta_train_df, method = "svmRadial", trControl = meta_control, tuneLength = 3); dur_meta_svm <- (proc.time() - start_t)[3]

  meta_durations <- list(Meta_GLM = dur_meta_glm, Meta_Enet = dur_meta_enet, Meta_GAM = dur_meta_gam, Meta_PLS = dur_meta_pls, Meta_RF = dur_meta_rf, Meta_SVM = dur_meta_svm)

  pred_train_list[["Meta_GLM"]]  <- as.numeric(stats::predict(meta_model_glm, newdata = meta_train_df))
  pred_test_list[["Meta_GLM"]]   <- as.numeric(stats::predict(meta_model_glm, newdata = meta_test_df))
  pred_train_list[["Meta_Enet"]] <- as.numeric(stats::predict(meta_model_enet, newdata = meta_train_df))
  pred_test_list[["Meta_Enet"]]  <- as.numeric(stats::predict(meta_model_enet, newdata = meta_test_df))
  pred_train_list[["Meta_GAM"]]  <- as.numeric(stats::predict(meta_model_gam, newdata = meta_train_df))
  pred_test_list[["Meta_GAM"]]   <- as.numeric(stats::predict(meta_model_gam, newdata = meta_test_df))
  pred_train_list[["Meta_PLS"]]  <- as.numeric(stats::predict(meta_model_pls, newdata = meta_train_df))
  pred_test_list[["Meta_PLS"]]   <- as.numeric(stats::predict(meta_model_pls, newdata = meta_test_df))
  pred_train_list[["Meta_RF"]]   <- as.numeric(stats::predict(meta_model_rf, newdata = meta_train_df))
  pred_test_list[["Meta_RF"]]    <- as.numeric(stats::predict(meta_model_rf, newdata = meta_test_df))
  pred_train_list[["Meta_SVM"]]  <- as.numeric(stats::predict(meta_model_svm, newdata = meta_train_df))
  pred_test_list[["Meta_SVM"]]   <- as.numeric(stats::predict(meta_model_svm, newdata = meta_test_df))

  models_list[["Meta_GLM"]]  <- meta_model_glm
  models_list[["Meta_Enet"]] <- meta_model_enet
  models_list[["Meta_GAM"]]  <- meta_model_gam
  models_list[["Meta_PLS"]]  <- meta_model_pls
  models_list[["Meta_RF"]]   <- meta_model_rf
  models_list[["Meta_SVM"]]  <- meta_model_svm

  meta_names <- c("Meta_GLM", "Meta_Enet", "Meta_GAM", "Meta_PLS", "Meta_RF", "Meta_SVM")
  meta_reports <- list()

  for (m_name in meta_names) {
    p_train <- pred_train_list[[m_name]]; p_test = pred_test_list[[m_name]]
    rmse_train <- sqrt(mean((actual_train - p_train)^2)); rmse_test = sqrt(mean((actual_test - p_test)^2)); mae_test = mean(abs(actual_test - p_test))

    res_meta <- actual_test - p_test; se_mse_m = stats::sd(res_meta^2) / sqrt(length(res_meta)); mse_test_m = mean(res_meta^2)
    rmse_ci_low_m <- sqrt(max(0, mse_test_m - (1.96 * se_mse_m))); rmse_ci_upp_m = sqrt(mse_test_m + (1.96 * se_mse_m))

    abs_err_m <- abs(res_meta); se_mae_m = stats::sd(abs_err_m) / sqrt(length(abs_err_m)); mae_ci_low_m = max(0, mae_test - (1.96 * se_mae_m)); mae_ci_upp_m = mae_test + (1.96 * se_mae_m)

    ss_res_m <- sum(res_meta^2); r2_m = if (ss_tot_test > 0) 1 - (ss_res_m / ss_tot_test) else 0
    adj_r2_m <- if (n_test > n_models + 1) 1 - ((1 - r2_m) * (n_test - 1) / (n_test - n_models - 1)) else r2_m

    r2_ci_low_m <- 1 - ((mse_test_m + (1.96 * se_mse_m)) / var_actual_test); r2_ci_upp_m = 1 - ((max(0, mse_test_m - (1.96 * se_mse_m))) / var_actual_test)
    adj_r2_low_m <- if (n_test > n_models + 1) 1 - ((1 - r2_ci_low_m) * (n_test - 1) / (n_test - n_models - 1)) else r2_ci_low_m
    adj_r2_upp_m <- if (n_test > n_models + 1) 1 - ((1 - r2_ci_upp_m) * (n_test - 1) / (n_test - n_models - 1)) else r2_ci_upp_m

    var_m <- stats::var(p_test); ks_m = stats::ks.test(p_test, actual_train)

    # FIXED: STRIPPED OUT BAD VECTOR OVERRIDE WRAPPERS THAT RUINED ARRAY LENGTH SIGNATURES HERE

    meta_reports[[m_name]] <- data.frame(
      Model = m_name, Testing_RMSE = round(rmse_test, 4), `RMSE 95% CI Lower` = round(rmse_ci_low_m, 4), `RMSE 95% CI Upper` = round(rmse_ci_upp_m, 4),
      Testing_MAE = round(mae_test, 4), `MAE 95% CI Lower` = round(mae_ci_low_m, 4), `MAE 95% CI Upper` = round(mae_ci_upp_m, 4),
      Adjusted_R2 = round(adj_r2_m, 4), `Adjusted R2 95% CI Lower` = round(adj_r2_low_m, 4), `Adjusted R2 95% CI Upper` = round(adj_r2_upp_m, 4),
      Duration = round(meta_durations[[m_name]], 4), Overfitting = round(rmse_test / rmse_train, 4), Bias = round(mean(p_test - actual_test), 4),
      Variance = round(var_m, 4), KS_p_value = round(ks_m$p.value, 4), stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  meta_master_df <- do.call(rbind, meta_reports)

  pairs_grid <- utils::combn(m_names, 2, simplify = FALSE)
  n_pairs <- length(pairs_grid)
  ens_reports <- list()

  if (verbose) {
    cat(sprintf("\n[Ensemble Engine]: Evaluating all %d pairwise architectural combinations...\n", n_pairs))
    pb <- utils::txtProgressBar(min = 0, max = n_pairs, style = 3)
  }

  for (idx in seq_along(pairs_grid)) {
    m1 <- pairs_grid[[idx]][1]; m2 = pairs_grid[[idx]][2]; ens_name = paste0(m1, "+", m2)

    p_train_comb <- (pred_train_list[[m1]] + pred_train_list[[m2]]) / 2
    p_test_comb  <- (pred_test_list[[m1]] + pred_test_list[[m2]]) / 2

    pred_train_list[[ens_name]] <- p_train_comb; pred_test_list[[ens_name]] = p_test_comb

    rmse_train_c <- sqrt(mean((actual_train - p_train_comb)^2)); rmse_test_c = sqrt(mean((actual_test - p_test_comb)^2)); mae_test_c = mean(abs(actual_test - p_test_comb))

    res_c <- actual_test - p_test_comb; se_mse_c = stats::sd(res_c^2) / sqrt(length(res_c)); text_mse_c = mean(res_c^2)
    rmse_ci_low_c <- sqrt(max(0, text_mse_c - (1.96 * se_mse_c))); rmse_ci_upp_c = sqrt(text_mse_c + (1.96 * se_mse_c))

    abs_err_c <- abs(res_c); se_mae_c = stats::sd(abs_err_c) / sqrt(length(abs_err_c)); mae_ci_low_c = max(0, mae_test_c - (1.96 * se_mae_c)); mae_ci_upp_c = mae_test_c + (1.96 * se_mae_c)

    ss_res_c <- sum(res_c^2); r2_c = if (ss_tot_test > 0) 1 - (ss_res_c / ss_tot_test) else 0
    adj_r2_c <- if (n_test > 2 + 1) 1 - ((1 - r2_c) * (n_test - 1) / (n_test - 2 - 1)) else r2_c

    r2_ci_low_c <- 1 - ((text_mse_c + (1.96 * se_mse_c)) / var_actual_test); r2_ci_upp_c = 1 - ((max(0, text_mse_c - (1.96 * se_mse_c))) / var_actual_test)
    adj_r2_low_c <- if (n_test > 2 + 1) 1 - ((1 - r2_ci_low_c) * (n_test - 1) / (n_test - 2 - 1)) else r2_ci_low_c
    adj_r2_upp_c <- if (n_test > 2 + 1) 1 - ((1 - r2_ci_upp_c) * (n_test - 1) / (n_test - 2 - 1)) else r2_ci_upp_c

    var_c <- stats::var(p_test_comb); ks_c = stats::ks.test(p_test_comb, actual_train)

    ens_reports[[idx]] <- data.frame(
      Model = ens_name, Testing_RMSE = round(rmse_test_c, 4), `RMSE 95% CI Lower` = round(rmse_ci_low_c, 4), `RMSE 95% CI Upper` = round(rmse_ci_upp_c, 4),
      Testing_MAE = round(mae_test_c, 4), `MAE 95% CI Lower` = round(mae_ci_low_c, 4), `MAE 95% CI Upper` = round(mae_ci_upp_c, 4),
      Adjusted_R2 = round(adj_r2_c, 4), `Adjusted R2 95% CI Lower` = round(adj_r2_low_c, 4), `Adjusted R2 95% CI Upper` = round(adj_r2_upp_c, 4),
      Duration = round(durations_list[[m1]] + durations_list[[m2]], 4), Overfitting = round(rmse_test_c / rmse_train_c, 4), Bias = round(mean(p_test_comb - actual_test), 4),
      Variance = round(var_c, 4), KS_p_value = round(ks_c$p.value, 4), stringsAsFactors = FALSE, check.names = FALSE
    )
    if (verbose) utils::setTxtProgressBar(pb, idx)
  }
  if (verbose) close(pb)

  ens_master_df <- do.call(rbind, ens_reports)
  ens_master_df <- ens_master_df[order(ens_master_df$Testing_RMSE), ]
  top_10_ensembles <- utils::head(ens_master_df, 10)

  master_reporting_base <- rbind(report, meta_master_df)
  report <- rbind(master_reporting_base, top_10_ensembles)
  report <- report[order(report$Testing_RMSE), ]

  rownames(report) <- NULL

  audit_logs <- list()
  all_pipeline_models <- report$Model

  for (m_name in all_pipeline_models) {
    p_test <- pred_test_list[[m_name]]
    res_test <- actual_test - p_test

    sw_test <- tryCatch({ stats::shapiro.test(utils::head(res_test, 5000)) }, error = function(e) NULL)
    norm_status <- if (!is.null(sw_test) && !is.na(sw_test$p.value) && sw_test$p.value < 0.05) "Non-Normal" else "Normal"

    het_cor <- tryCatch({ stats::cor.test(p_test, abs(res_test), method = "spearman", exact = FALSE) }, error = function(e) NULL)
    het_status <- if (!is.null(het_cor) && !is.na(het_cor$p.value) && het_cor$p.value < 0.05) "Heteroscedastic" else "Homoscedastic"

    dw_stat <- sum(diff(res_test)^2) / sum(res_test^2)
    ac_status <- if (is.na(dw_stat) || dw_stat < 1.5 || dw_stat > 2.5) "Autocorrelated" else "Independent"

    audit_logs[[m_name]] <- data.frame(
      Model = m_name, Residual_Normality = norm_status, Variance_Stability = het_status, Error_Independence = ac_status, stringsAsFactors = FALSE
    )
  }
  audit_report_matrix <- do.call(rbind, audit_logs)
  rownames(audit_report_matrix) <- NULL

  p_kpi_metrics <- (.make_metric_plot(report, "Testing_RMSE", "Testing RMSE", theme_colors$primary, theme_colors, show_ci = TRUE, ci_lower_col = "RMSE 95% CI Lower", ci_upper_col = "RMSE 95% CI Upper") +
                      .make_metric_plot(report, "Testing_MAE", "Testing MAE", theme_colors$secondary, theme_colors, show_ci = TRUE, ci_lower_col = "MAE 95% CI Lower", ci_upper_col = "MAE 95% CI Upper") +
                      .make_metric_plot(report, "Adjusted_R2", "Adjusted R-Squared", theme_colors$accent, theme_colors, show_ci = TRUE, ci_lower_col = "Adjusted R2 95% CI Lower", ci_upper_col = "Adjusted R2 95% CI Upper")) +
    patchwork::plot_layout(ncol = 3) +
    patchwork::plot_annotation(title = "Core Model Performance Metrics & KPIs")

  p_risk_metrics <- (.make_metric_plot(report, "Overfitting", "Overfitting Ratio", theme_colors$warning, theme_colors, is_overfit = TRUE) +
                       .make_metric_plot(report, "Bias", "Directional Model Bias", theme_colors$accent, theme_colors)) +
    patchwork::plot_layout(ncol = 2) +
    patchwork::plot_annotation(title = "Generalization Risks and Structural Bias Diagnostics")

  p_tradeoff_assembled <- .make_bias_variance_combined_plot(report, "Bias-Variance Joint Mapping Space", theme_colors, palette_style)

  ks_data <- report; ks_data$Model = factor(ks_data$Model, levels = ks_data$Model[order(ks_data$KS_p_value)])
  p_ks_assembled <- ggplot2::ggplot(ks_data, ggplot2::aes(x = Model, y = KS_p_value)) +
    ggplot2::geom_col(fill = theme_colors$ks_fill, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.4f", KS_p_value)), hjust = -0.1, size = 2.5, fontface = "bold") +
    ggplot2::geom_hline(yintercept = 0.05, color = theme_colors$ks_line1, linetype = "dotted", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = 0.10, color = theme_colors$ks_line2, linetype = "dotted", linewidth = 0.8) +
    ggplot2::coord_flip() + ggplot2::theme_minimal(base_size = 10) +
    ggplot2::labs(title = "Kolmogorov-Smirnov Test p-values", x = "Model Architecture", y = "KS Test p-value") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.22)))

  heat_data <- report; rownames(heat_data) <- heat_data$Model
  heat_metrics <- heat_data[, c("Testing_RMSE", "Testing_MAE", "Adjusted_R2", "Overfitting", "Bias", "Variance", "Duration")]
  heat_scaled <- as.data.frame(scale(heat_metrics)); heat_scaled$Model = rownames(heat_scaled)
  heat_long <- tidyr::pivot_longer(heat_scaled, cols = -Model, names_to = "Metric", values_to = "Z_Score")

  p_heatmap <- ggplot2::ggplot(heat_long, ggplot2::aes(x = Metric, y = Model, fill = Z_Score)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = theme_colors$tiles_low, mid = theme_colors$tiles_mid, high = theme_colors$tiles_high, name = "Z-Score") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Comparative Performance Metric Heatmap Matrix")
  eda_plots$metric_heatmap <- p_heatmap

  graphics::par(mfrow = c(1, 1))
  top_3_models <- utils::head(report$Model, 3)

  output_results <- list(
    performance_report = report,
    audit_report       = audit_report_matrix,
    base_models        = models_list,
    meta_models        = list(GLM = meta_model_glm, Enet = meta_model_enet, GAM = meta_model_gam, PLS = meta_model_pls, RF = meta_model_rf, SVM = meta_model_svm),
    data_dictionary    = data_dict,
    data_head          = data_head_table,
    data_summary       = data_summary_table,
    data_correlation   = data_correlation_matrix,
    vif_report         = vif_report_table,
    pipeline_meta      = list(
      target_col = target_col, stratify_col = stratify_col, kept_features = kept_vif_features,
      dummy_model = dummy_model, palette_style = palette_style, train_data_ref = train_data,
      top_3_models = top_3_models, pred_test_list = pred_test_list, pred_train_list = pred_train_list,
      actual_test = actual_test, actual_train = actual_train, test_data = test_data,
      theme_colors = theme_colors, top_pred_names = top_pred_names
    ),
    plots = list(
      histograms = eda_plots$histograms, boxplots = eda_plots$boxplots,
      correlation = eda_plots$correlation, scatter_matrix = eda_plots$scatter_matrix,
      metric_heatmap = eda_plots$metric_heatmap, kpis = p_kpi_metrics, risks = p_risk_metrics,
      tradeoff = p_tradeoff_assembled, ks_test = p_ks_assembled,
      draw_top3 = function() { .draw_top3_panel(top_3_models, pred_test_list, actual_test, models_list, train_data, target_col, theme_colors) },
      draw_diagnostics = function() { .draw_diagnostics_panel(top_3_models, pred_test_list, pred_train_list, actual_test, actual_train, test_data, target_col, theme_colors, top_pred_names) }
    )
  )

  class(output_results) <- "numeric_pipeline"
  return(invisible(output_results))
}

# -------------------------------------------------------------------------
# 4. S3 OBJECT-ORIENTED INTERFACE METADATA METHOD MODULES
# -------------------------------------------------------------------------

#' Print Numeric Pipeline Summary Report
#'
#' @param x A numeric_pipeline object.
#' @param ... Additional arguments.
#' @export
#' @method print numeric_pipeline
print.numeric_pipeline <- function(x, ...) {
  cat("\n=========================================================================\n")
  cat("                  NUMERIC PIPELINE PROFILE EXPORTS             \n")
  cat("=========================================================================\n")
  cat("\n[1. BASELINE DATA SAMPLE HEAD]\n")
  print(x$data_head)
  cat("\n[2. STRUCTURAL DATA DICTIONARY]\n")
  print(x$data_dictionary, right = FALSE)
  cat("\n[3. STATISTICAL POPULATION DESCRIPTIVE SUMMARY]\n")
  print(x$data_summary)
  cat("\n[4. MULTICOLLINEARITY VIF FILTERS REPORT]\n")
  if (is.data.frame(x$vif_report)) {
    print(x$vif_report, row.names = FALSE)
  } else {
    cat("  ->", x$vif_report, "\n")
  }
  cat("\n=========================================================================\n")
  cat("                     LEADERBOARD & PREDICTIVE KPIS                       \n")
  cat("=========================================================================\n")
  cat(sprintf("Total Models Run: %d\n", nrow(x$performance_report)))
  cat("\nTop 10 Architectures By Testing RMSE:\n")
  print(utils::head(x$performance_report[, c("Model", "Testing_RMSE", "Testing_MAE", "Adjusted_R2", "Variance", "KS_p_value", "Overfitting")], 10), row.names = FALSE)
  cat("\n=========================================================================\n")
  cat("               AUTOMATED RESIDUAL DIAGNOSTIC LEADERBOARD                 \n")
  cat("=========================================================================\n")
  print(utils::head(x$audit_report, 10), row.names = FALSE)
}

#' Plot Numeric Pipeline Performance Metrics and Visual Diagnostics
#'
#' @param x A numeric_pipeline object.
#' @param pace_output Logical. If TRUE and session is interactive, paces chart pages. Default = TRUE.
#' @param ... Additional arguments.
#' @export
#' @method plot numeric_pipeline
plot.numeric_pipeline <- function(x, pace_output = TRUE, ...) {
  if (pace_output && interactive()) {
    old_ask <- grDevices::devAskNewPage(ask = TRUE)
    on.exit(grDevices::devAskNewPage(old_ask))
  }
  if (!is.null(x$plots$histograms))     print(x$plots$histograms)
  if (!is.null(x$plots$boxplots))       print(x$plots$boxplots)
  if (!is.null(x$plots$correlation))    print(x$plots$correlation)
  if (!is.null(x$plots$scatter_matrix)) print(x$plots$scatter_matrix)
  print(x$plots$kpis)
  print(x$plots$risks)
  print(x$plots$tradeoff)
  print(x$plots$ks_test)
  if (!is.null(x$plots$metric_heatmap)) print(x$plots$metric_heatmap)
  x$plots$draw_top3()
  x$plots$draw_diagnostics()
}

#' Predict with Numeric Pipeline Framework
#'
#' @param object A numeric_pipeline object.
#' @param newdata A data.frame containing new data configurations to score.
#' @param model_name Character string specifying the target model from the leaderboard to score. Default = "best".
#' @param ... Additional arguments.
#' @return A numeric vector of predictions.
#' @export
#' @method predict numeric_pipeline
predict.numeric_pipeline <- function(object, newdata, model_name = "best", ...) {
  if (is.null(object)) stop("Argument 'object' must be a valid trained numeric_pipeline.", call. = FALSE)
  if (missing(newdata)) stop("Argument 'newdata' must be provided.", call. = FALSE)

  df_new <- as.data.frame(newdata)
  if (model_name == "best") model_name <- object$performance_report$Model[1]

  expected_variables <- NULL
  if (!is.null(object$pipeline_meta$dummy_model)) {
    dm <- object$pipeline_meta$dummy_model
    if (is.list(dm) && "vars" %in% names(dm)) {
      if (is.list(dm$vars) && "predictors" %in% names(dm$vars)) expected_variables <- names(dm$vars$predictors)
    }
  }
  if (is.null(expected_variables)) {
    expected_variables <- colnames(object$pipeline_meta$train_data_ref)
    expected_variables <- expected_variables[expected_variables != object$pipeline_meta$target_col]
  }

  for (v in expected_variables) {
    if (!(v %in% colnames(df_new))) df_new[[v]] <- 0
  }

  new_encoded <- data.frame(stats::predict(object$pipeline_meta$dummy_model, newdata = df_new, na.action = stats::na.pass))

  for (col in colnames(new_encoded)) {
    if (col %in% colnames(object$pipeline_meta$train_data_ref) && any(is.na(new_encoded[[col]]))) {
      fallback_median <- stats::median(object$pipeline_meta$train_data_ref[[col]], na.rm = TRUE)
      new_encoded[is.na(new_encoded[[col]]), col] <- fallback_median
    }
  }

  if (grepl("\\+", model_name)) {
    sub_models <- strsplit(model_name, "\\+")[[1]]
    p1 <- as.numeric(stats::predict(object$base_models[[sub_models[1]]], newdata = new_encoded, na.action = stats::na.pass))
    p2 <- as.numeric(stats::predict(object$base_models[[sub_models[2]]], newdata = new_encoded, na.action = stats::na.pass))
    return((p1 + p2) / 2)
  }

  if (grepl("Meta_", model_name)) {
    meta_key <- strsplit(model_name, "Meta_")[[1]][2]
    base_preds <- lapply(object$base_models[1:17], function(mod) {
      as.numeric(stats::predict(mod, newdata = new_encoded, na.action = stats::na.pass))
    })
    meta_features <- as.data.frame(base_preds)
    return(as.numeric(stats::predict(object$meta_models[[meta_key]], newdata = meta_features)))
  }

  if (model_name %in% names(object$base_models)) {
    return(as.numeric(stats::predict(object$base_models[[model_name]], newdata = new_encoded, na.action = stats::na.pass)))
  }

  stop(sprintf("Model identifier '%s' not recognized within this pipeline.", model_name), call. = FALSE)
}

#' Generate Executive Production Projections with Row-Level 95 percent Confidence Intervals
#'
#' @param object A trained `numeric_pipeline` object.
#' @param newdata A data.frame containing new data configurations to score.
#' @return A data.frame structured with prediction vectors and interval metrics for the top 3 models.
#' @export
predict_production <- function(object, newdata) {
  if (is.null(object)) stop("Argument 'object' must be a valid trained numeric_pipeline.", call. = FALSE)
  if (missing(newdata)) stop("Argument 'newdata' must be provided.", call. = FALSE)

  top_3_names <- utils::head(object$performance_report$Model, 3)
  production_report <- data.frame(Row_Index = seq_len(nrow(as.data.frame(newdata))))
  actual_test <- object$pipeline_meta$actual_test

  for (i in seq_along(top_3_names)) {
    m_name <- top_3_names[i]
    clean_m_label <- gsub("\\+", "_and_", m_name)
    point_predictions <- predict(object = object, newdata = newdata, model_name = m_name)

    historical_preds <- object$pipeline_meta$pred_test_list[[m_name]]
    model_residuals <- if (is.null(historical_preds)) actual_test - mean(actual_test) else actual_test - historical_preds
    sigma_model <- stats::sd(model_residuals, na.rm = TRUE)

    lower_bound <- point_predictions - (1.96 * sigma_model)
    upper_bound <- point_predictions + (1.96 * sigma_model)

    production_report[[sprintf("Rank_%d_%s_Prediction", i, clean_m_label)]]  <- round(point_predictions, 2)
    production_report[[sprintf("Rank_%d_%s_95_LowerBound", i, clean_m_label)]] <- round(lower_bound, 2)
    production_report[[sprintf("Rank_%d_%s_95_UpperBound", i, clean_m_label)]] <- round(upper_bound, 2)
  }
  return(production_report)
}

#' Save Serialized Numeric Pipeline Environment to Disk File
#'
#' @param object A numeric_pipeline object.
#' @param file_path Character string tracking destination directory file path.
#' @export
save_pipeline <- function(object, file_path) {
  if (!inherits(object, "numeric_pipeline")) stop("Object must be a valid 'numeric_pipeline'.", call. = FALSE)
  saved_bundle <- list(
    performance_report = object$performance_report,
    audit_report       = object$audit_report,
    base_models        = object$base_models,
    meta_models        = object$meta_models,
    pipeline_meta      = object$pipeline_meta
  )
  class(saved_bundle) <- "serialized_numeric_pipeline"
  saveRDS(saved_bundle, file = file_path)
}

#' Load Serialized Numeric Pipeline Environment from Disk File
#'
#' @param file_path Character string tracking destination directory file path.
#' @return A re-hydrated numeric_pipeline object.
#' @export
load_pipeline <- function(file_path) {
  if (!file.exists(file_path)) stop(sprintf("Target serialized pipeline file not found at path: '%s'", file_path), call. = FALSE)
  bundle <- readRDS(file_path)
  if (!inherits(bundle, "serialized_numeric_pipeline")) stop("Invalid structured 'serialized_numeric_pipeline' footprint.", call. = FALSE)
  class(bundle) <- "numeric_pipeline"
  return(bundle)
}

#' Export Numeric Pipeline Outcomes and Graphs
#'
#' @param pipeline_object An object generated by the \code{Numeric()} engine.
#' @param export_directory Character string path targeting file output folders.
#' @return Invisible character string tracking file save directory destinations.
#' @export
ExportNumericResults <- function(pipeline_object = NULL, export_directory = NULL) {
  if (is.null(pipeline_object)) stop("Must provide a valid pipeline execution object.", call. = FALSE)
  save_dir <- if (is.null(export_directory)) tempdir() else export_directory
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

  if (!is.null(pipeline_object$performance_report)) utils::write.csv(pipeline_object$performance_report, file = file.path(save_dir, "algorithm_performance_leaderboard.csv"), row.names = FALSE)
  if (!is.null(pipeline_object$audit_report)) utils::write.csv(pipeline_object$audit_report, file = file.path(save_dir, "residual_diagnostic_audit.csv"), row.names = FALSE)
  if (!is.null(pipeline_object$data_dictionary)) utils::write.csv(pipeline_object$data_dictionary, file = file.path(save_dir, "features_data_dictionary.csv"), row.names = FALSE)
  if (!is.null(pipeline_object$vif_report)) utils::write.csv(pipeline_object$vif_report, file = file.path(save_dir, "multicollinearity_vif_report.csv"), row.names = FALSE)

  grDevices::png(filename = file.path(save_dir, "core_performance_kpis.png"), width = 1400, height = 600, res = 130)
  print(pipeline_object$plots$kpis)
  grDevices::dev.off()

  message("Success! All metrics tables and dashboards saved cleanly to disk locations.")
  return(invisible(save_dir))
}

#' Render Automated Quarto Numeric Executive Report
#'
#' @param pipeline_object An object generated by the \code{Numeric()} engine.
#' @param output_directory Character string specifying where to save the report files.
#' @export
RenderExecutiveReport <- function(pipeline_object = NULL, output_directory = getwd()) {
  if (is.null(pipeline_object)) stop("Must provide a valid pipeline return object.", call. = FALSE)
  if (!requireNamespace("quarto", quietly = TRUE)) stop("The 'quarto' package is required to compile this report.", call. = FALSE)

  qmd_path <- file.path(output_directory, "Executive_Continuous_Regression_Analysis_Report.qmd")
  rds_path <- file.path(output_directory, "numeric_pipeline_data_cache.rds")

  save_pipeline(pipeline_object, rds_path)

  qmd_content <- c(
    "---",
    "title: 'Enterprise Continuous Regression Optimization Report'",
    "author: 'Russ Conte'",
    "date: '`r Sys.Date()`'",
    "format:",
    "  html:",
    "    theme: cosmo",
    "    toc: true",
    "---",
    "",
    "```{r setup, include=FALSE}",
    sprintf("pipeline <- load_pipeline('%s')", gsub("\\\\", "/", rds_path)),
    "```",
    "",
    "## 1. Executive Operational Summary",
    "The parallel multi-model regression ensemble pipeline evaluated continuous prediction spaces across 17 base architectures, 6 stacking meta-learners, and 136 pairwise hybrid combinations.",
    "The optimized champion configuration is determined to be the **`r pipeline$performance_report$Model[1]`** model.",
    "",
    "## 2. Predictor Feature Set Properties",
    "```{r show-dict, echo=FALSE}",
    "knitr::kable(pipeline$data_dictionary, caption = 'Predictors Summary')",
    "```",
    "",
    "## 3. Top Leaderboard Standings",
    "```{r show-leaderboard, echo=FALSE}",
    "knitr::kable(head(pipeline$performance_report[, c('Model', 'Testing_RMSE', 'Testing_MAE', 'Adjusted_R2')], 5), caption = 'Performance Leaderboard')",
    "```"
  )

  writeLines(qmd_content, qmd_path)
  quarto::quarto_render(input = qmd_path, quiet = TRUE)

  if (file.exists(rds_path)) file.remove(rds_path)
  message(sprintf("Success! Executive Continuous Regression report generated cleanly at: %s", sub("\\.qmd$", ".html", qmd_path)))
}

#' Run a Demonstration of the Numeric Ensembles Pipeline
#'
#' @export
NumericEnsemblesDemo <- function() {
  message("Initializing NumericEnsembles Comprehensive Validation Demo...")

  set.seed(42)
  n_samples <- 60
  housing_index <- runif(n_samples, min = 80, max = 150)
  unemploy_rate <- rnorm(n_samples, mean = 5.5, sd = 1.2)
  gdp_growth <- 2.5 + (0.04 * housing_index) - (0.35 * unemploy_rate) + rnorm(n_samples, sd = 0.5)

  economic_registry <- data.frame(
    GDP_Growth      = gdp_growth,
    Housing_Index   = housing_index,
    Unemployment    = unemploy_rate
  )

  fast_config <- NumericEnsemblesFastConfig()
  test_run <- Numeric(
    dataset       = economic_registry,
    target_col    = "GDP_Growth",
    palette_style = "modern",
    config        = fast_config,
    verbose       = TRUE
  )

  print(test_run)
  return(invisible(test_run))
}
#' Launch Interactive NumericEnsembles Web Interface App
#'
#' Fires up a local instance of the interactive OLS vs. GLM tuning,
#' diagnostic, and residual validation web dashboard system.
#'
#' @export
LaunchNumericApp <- function() {
  # FIXED: Changed prefix from utils::system.file to base::system.file
  app_path <- base::system.file("shiny-apps", "NumericEnsemblesApp", package = "NumericEnsembles")
  if (app_path == "") {
    stop("Shiny application dashboard directory not found within package files library installation.", call. = FALSE)
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' must be installed to activate the web interface app framework.", call. = FALSE)
  }

  # Defensive browser lookup conversion handling closure environments safely
  launch_opt <- getOption("shiny.launch.browser", TRUE)
  is_launch_active <- if (is.logical(launch_opt)) launch_opt else TRUE

  if (is_launch_active && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    shiny::runApp(app_path, launch.browser = rstudioapi::viewer, display.mode = "normal")
  } else {
    shiny::runApp(app_path, display.mode = "normal")
  }
}
