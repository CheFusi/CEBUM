library(shiny)
library(readxl)
library(tidyverse)

# Define the UI
ui <- fluidPage(
  titlePanel("PLSR Model Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Upload XLS Files (MIR, NIR, MIR_NIR)", 
                multiple = TRUE, accept = c(".xls", ".xlsx")),
      h4("R² Filter"),
      sliderInput("r2_min_filter", "Minimum R² Value:", min = 0, max = 1, value = 0, step = 0.05),
      sliderInput("r2_max_filter", "Maximum R² Value:", min = 0, max = 1, value = 1, step = 0.05),
      
      h4("RMSEP Range Ratio Filter"),
      sliderInput("rmsep_min_filter", "Minimum RMSEP Range Ratio:", min = 0, max = 100, value = 0, step = 0.1),
      sliderInput("rmsep_max_filter", "Maximum RMSEP Range Ratio:", min = 0, max = 100, value = 100, step = 0.1),
      
      h4("Bias Filter"),
      sliderInput("bias_min_filter", "Minimum Bias:", min = -1000, max = 1000, value = -1000, step = 50),
      sliderInput("bias_max_filter", "Maximum Bias:", min = -1000, max = 1000, value = 1000, step = 50),
      
      h4("N Filter"),
      sliderInput("n_min_filter", "Minimum N:", min = 0, max = 250, value = 0, step = 1),
      sliderInput("n_max_filter", "Maximum N:", min = 0, max = 250, value = 250, step = 1),
      
      selectInput("metric", "Select Metric:", 
                  choices = c("N", "R2", "RMSEP_range_ratio", "bias"), 
                  selected = "R2"),
      selectInput("property", "Select Property:", 
                  choices = NULL, multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Plot", plotOutput("metricPlot")),
        tabPanel("Heatmap", plotOutput("heatmapPlot")),
        tabPanel("Metrics Table", dataTableOutput("metricsTable"))
      )
    )
  )
)

# Define the Server Logic
server <- function(input, output, session) {
  # Reactive to upload and process the files
  metrics_data <- reactive({
    req(input$files)
    file_list <- input$files
    
    # Load and combine files
    data_list <- lapply(seq_len(nrow(file_list)), function(i) {
      file <- file_list$datapath[i]
      model_name <- file_list$name[i]
      
      data <- read_xlsx(file)
      
      # Ensure Data_Type exists
      if (!"Data_Type" %in% colnames(data)) {
        stop("Data_Type column is missing from the file. Please include it to indicate Calibration/Validation rows.")
      }
      
      # Add Model column based on filename
      data <- data %>% mutate(Model = case_when(
        grepl("MIR_NIR", model_name, ignore.case = TRUE) ~ "MIR_NIR",
        grepl("NIR", model_name, ignore.case = TRUE) ~ "NIR",
        grepl("MIR", model_name, ignore.case = TRUE) ~ "MIR",
        TRUE ~ "Unknown"
      ))
      
      return(data)
    })
    
    combined_data <- bind_rows(data_list)
    combined_data <- combined_data %>% filter(Data_Type != "Skipped")
    combined_data <- combined_data %>% pivot_longer(
      cols = c(N, R2, RMSEP_range_ratio, bias),
      names_to = "Metric",
      values_to = "Value"
    )
    return(combined_data)
  })
  
  # Update property selection dynamically
  observe({
    req(metrics_data())
    properties <- unique(metrics_data()$Property)
    updateSelectInput(session, "property", choices = properties, selected = properties)
  })
  
  # Filtered data for display
  filtered_data <- reactive({
    req(metrics_data())
    
    wide_data <- metrics_data() %>%
      pivot_wider(names_from = Metric, values_from = Value)
    
    # Group by Property and filter paired Calibration and Validation
    filtered <- wide_data %>%
      group_by(Property) %>%
      filter(
        all(c("Calibration", "Validation") %in% Data_Type) & # Ensure both Data_Types exist
          all(R2[Data_Type == "Calibration"] >= input$r2_min_filter & R2[Data_Type == "Calibration"] <= input$r2_max_filter) &
          all(R2[Data_Type == "Validation"] >= input$r2_min_filter & R2[Data_Type == "Validation"] <= input$r2_max_filter) &
          all(RMSEP_range_ratio[Data_Type == "Calibration"] >= input$rmsep_min_filter & RMSEP_range_ratio[Data_Type == "Calibration"] <= input$rmsep_max_filter) &
          all(RMSEP_range_ratio[Data_Type == "Validation"] >= input$rmsep_min_filter & RMSEP_range_ratio[Data_Type == "Validation"] <= input$rmsep_max_filter) &
          all(bias[Data_Type == "Calibration"] >= input$bias_min_filter & bias[Data_Type == "Calibration"] <= input$bias_max_filter) &
          all(bias[Data_Type == "Validation"] >= input$bias_min_filter & bias[Data_Type == "Validation"] <= input$bias_max_filter) &
          all(N[Data_Type == "Calibration"] >= input$n_min_filter & N[Data_Type == "Calibration"] <= input$n_max_filter) &
          all(N[Data_Type == "Validation"] >= input$n_min_filter & N[Data_Type == "Validation"] <= input$n_max_filter)
      ) %>%
      ungroup()
    
    filtered_long <- filtered %>%
      pivot_longer(cols = c(N, R2, RMSEP_range_ratio, bias),
                   names_to = "Metric",
                   values_to = "Value")
    
    return(filtered_long)
  })
  
  # Render metrics table
  output$metricsTable <- renderDataTable({
    req(filtered_data())
    filtered_data() %>% 
      select(Property, Data_Type, Metric, Value, Model) %>%
      arrange(Property)
  })
  
  # Render metric plot
  output$metricPlot <- renderPlot({
    req(filtered_data())
    ggplot(
      filtered_data() %>% filter(Metric == input$metric),
      aes(x = Property, y = Value, fill = Data_Type)
    ) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      facet_wrap(~ Model, scales = "free_x") + # Facet by Model
      scale_fill_manual(values = c("Calibration" = "purple", "Validation" = "darkgreen")) + # Custom Colors
      labs(title = paste(input$metric, "by Calibration/Validation"), y = input$metric, x = "Property") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank()
      )
  })
  
  # Render heatmap
  output$heatmapPlot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Property, y = Metric, fill = Value)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colors = c("blue", "white", "red")) +
      facet_wrap(~ Data_Type + Model, scales = "free_x") + # Facet by Data_Type and Model
      labs(title = "Heatmap of Metrics", x = "Property", y = "Metric") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank()
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)