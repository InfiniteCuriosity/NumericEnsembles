library(shiny)
library(ggplot2)
library(patchwork)
library(ggrepel)

# Diagnostic evaluation hook for cosmetic corporate theme matrix overlays
if (requireNamespace("shinythemes", quietly = TRUE)) {
  app_theme <- shinythemes::shinytheme("flatly")
} else {
  app_theme <- NULL
}

# -------------------------------------------------------------------------
# USER INTERFACE LAYOUT PROFILE
# -------------------------------------------------------------------------
ui <- fluidPage(
  theme = app_theme,

  titlePanel("NumericEnsembles Continuous Machine Learning Engine App"),

  sidebarLayout(
    sidebarPanel(
      h4("Operational Inputs"),
      helpText("Upload a continuous target numeric dataset to train and evaluate 17 base architectures and ensembles concurrently."),

      fileInput("file_input", "Choose CSV Matrix File", accept = c(".csv")),

      uiOutput("target_select_ui"),

      hr(),
      h4("Algorithmic Hyperparameters"),
      sliderInput("train_pct", "Training Partition Percentage", min = 0.40, max = 0.90, value = 0.60, step = 0.05),
      sliderInput("cv_folds", "Cross-Validation Folds (K)", min = 2, max = 10, value = 5, step = 1),
      sliderInput("vif_threshold", "Max Multi-Collinearity VIF Limit", min = 2, max = 20, value = 5, step = 1),

      selectInput("palette_style", "Visualization Color Palette",
                  choices = c("standard", "viridis", "modern"), selected = "standard"),

      actionButton("run_pipeline", "Deploy Core Pipeline", class = "btn-primary btn-block"),

      hr(),
      h4("Out-of-Sample Score Projection"),
      helpText("Isolate any champion archetype from the validation leaderboard to evaluate custom row profiles instantly."),
      uiOutput("model_select_ui"),
      uiOutput("scoring_inputs_ui"),
      actionButton("run_prediction", "Score Profiler Row", class = "btn-success btn-block"),
      br(),
      tableOutput("prediction_result_table")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Executive Leaderboard",
                 verbatimTextOutput("console_report")),
        tabPanel("Core Performance KPIs",
                 plotOutput("kpi_plot", height = "550px")),
        tabPanel("Generalization & Tradeoffs",
                 plotOutput("risk_plot", height = "550px")),
        tabPanel("Comparative Heatmap Matrix",
                 plotOutput("heatmap_plot", height = "550px"))
      )
    )
  )
)

# -------------------------------------------------------------------------
# SERVER SIDE CORE PROCESSING LOGIC
# -------------------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive file loader
  raw_data <- reactive({
    req(input$file_input)
    utils::read.csv(input$file_input$datapath, stringsAsFactors = TRUE)
  })

  # Dynamic Target Variable Selector (filtering for strictly continuous numeric values)
  output$target_select_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    numeric_cols <- colnames(df)[sapply(df, is.numeric)]

    if (length(numeric_cols) == 0) {
      return(p("Error: No continuous numeric target vectors detected within this dataset."))
    }
    selectInput("target_col", "Target Continuous Variable (Y)", choices = numeric_cols)
  })

  # Reactive pipeline container triggered by the user action button
  pipeline_object <- eventReactive(input$run_pipeline, {
    req(raw_data(), input$target_col)

    # Fire the full 17-model multi-core architecture engine inside your package
    NumericEnsembles::Numeric(
      dataset       = raw_data(),
      target_col    = input$target_col,
      cv_folds      = input$cv_folds,
      train_pct     = input$train_pct,
      vif_threshold = input$vif_threshold,
      palette_style = input$palette_style,
      verbose       = FALSE
    )
  })

  # Dynamic Model Selector populated directly from the active leaderboard
  output$model_select_ui <- renderUI({
    req(pipeline_object())
    models_available <- pipeline_object()$performance_report$Model
    selectInput("selected_model", "Target Model for Inference", choices = models_available)
  })

  # Dynamic feature form fields rendering for active row profile scoring
  output$scoring_inputs_ui <- renderUI({
    req(pipeline_object())
    features <- pipeline_object()$pipeline_meta$kept_features
    df_ref <- raw_data()

    # Generate individual text input controls matching required numeric variables
    lapply(features, function(feat) {
      baseline_val <- round(mean(df_ref[[feat]], na.rm = TRUE), 2)
      numericInput(paste0("feat_", feat), sprintf("Variable: %s (Mean = %s)", feat, baseline_val), value = baseline_val)
    })
  })

  # Dynamic prediction execution channel
  predicted_value <- eventReactive(input$run_prediction, {
    req(pipeline_object(), input$selected_model)
    features <- pipeline_object()$pipeline_meta$kept_features

    # Scrape the dynamically generated input elements back into a clean row data frame
    scoring_row <- data.frame(matrix(ncol = length(features), nrow = 1))
    colnames(scoring_row) <- features
    for (feat in features) {
      scoring_row[[feat]] <- input[[paste0("feat_", feat)]]
    }

    # Route row entries straight back into your package's S3 predict method
    res <- NumericEnsembles:::predict.numeric_pipeline(
      object     = pipeline_object(),
      newdata    = scoring_row,
      model_name = input$selected_model
    )
    return(res)
  })

  # Render Tab 1: S3 print output layout
  output$console_report <- renderPrint({
    req(pipeline_object())
    NumericEnsembles:::print.numeric_pipeline(pipeline_object())
  })

  # Render Tab 2: Core Performance KPIs Dashboard
  output$kpi_plot <- renderPlot({
    req(pipeline_object())
    print(pipeline_object()$plots$kpis)
  })

  # Render Tab 3: Generalization Risks & Tradeoffs Dashboard
  output$risk_plot <- renderPlot({
    req(pipeline_object())
    # Arrange the tradeoff and risk metrics inside a side-by-side grid panel layout
    (pipeline_object()$plots$risks / pipeline_object()$plots$tradeoff) +
      patchwork::plot_layout(heights = c(1, 1))
  })

  # Render Tab 4: Scaled Performance Heatmap Matrix
  output$heatmap_plot <- renderPlot({
    req(pipeline_object())
    print(pipeline_object()$plots$metric_heatmap)
  })

  # Output tabular row inference scores window
  output$prediction_result_table <- renderTable({
    req(predicted_value())
    data.frame(
      Target_Metric = input$target_col,
      Selected_Model = input$selected_model,
      Projected_Score = predicted_value(),
      stringsAsFactors = FALSE
    )
  }, digits = 4)
}

shinyApp(ui = ui, server = server)
