library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyWidgets)

# Load the cleaned dataset
#data_cleaned <- read.csv("~/Downloads/cleaned_data.csv", stringsAsFactors = FALSE)

# Convert report_date column to Date format
data_cleaned$report_date <- as.Date(data_cleaned$report_date, format = "%Y-%m-%d")

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # Use modern theme
  
  titlePanel("Risk & Mitigation Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      
      selectInput("organisation", "Select Organisation", 
                  choices = unique(data_cleaned$organisation), selected = NULL, multiple = TRUE),
      
      selectInput("portfolio_id", "Select Portfolio", 
                  choices = unique(data_cleaned$portfolio_id), selected = NULL, multiple = TRUE),
      
      selectInput("business_area", "Select Business Area", 
                  choices = unique(data_cleaned$business_area), selected = NULL, multiple = TRUE),
      
      selectInput("project_id", "Select Project", 
                  choices = unique(data_cleaned$project_id), selected = NULL, multiple = TRUE),
      
      selectInput("risk_type", "Select Risk Type", 
                  choices = unique(data_cleaned$main_risk_cat), selected = NULL, multiple = TRUE),
      
      sliderInput("report_date", "Filter by Report Date", 
                  min = min(data_cleaned$report_date, na.rm = TRUE), 
                  max = max(data_cleaned$report_date, na.rm = TRUE),
                  value = range(data_cleaned$report_date, na.rm = TRUE),
                  timeFormat = "%Y-%m-%d"),
      
      width = 3  # Sidebar width
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", 
                 h3("Evidence-Based Recommendations"),
                 tags$ul(
                   tags$li("Recommendation 1"),
                   tags$li("Recommendation 2"),
                   tags$li("Recommendation 3")
                 ),
                 
                 br(), br(),  # Add spacing
                 
                 h3("Key Risk Metrics"),
                 br(),
                 
                 # Centered Total Risks and Total Mitigations cards
                 fluidRow(
                   column(4, offset = 2, uiOutput("card_total_risks")),  
                   column(4, uiOutput("card_total_mitigations"))
                 ),
                 
                 br(), br(),
                 
                 # Single row with Total Cost, Avg Cost, and Avg Probability
                 fluidRow(
                   column(4, uiOutput("card_total_costs")),  
                   column(4, uiOutput("card_avg_costs")),
                   column(4, uiOutput("card_avg_probability"))
                 )
        ),
        
        tabPanel("Risks", 
                 h3("Risk Trends Over Time"),
                 plotOutput("risk_trend_plot"),
                 br(),
                 h3("Risks & Mitigations per Project"),
                 plotOutput("risk_mitigation_per_project")),
        
        tabPanel("Mitigations", 
                 DTOutput("mitigations_table")),
        
        tabPanel("Opportunities", 
                 h3("Coming Soon"),
                 p("This section will include opportunities analysis.")),
        
        tabPanel("Predictive Analysis", 
                 h3("Coming Soon"),
                 p("This section will include predictive models for risk trends."))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Filter data based on user input
  filtered_data <- reactive({
    data_cleaned %>%
      filter(
        (organisation %in% input$organisation | is.null(input$organisation)) &
          (portfolio_id %in% input$portfolio_id | is.null(input$portfolio_id)) &
          (business_area %in% input$business_area | is.null(input$business_area)) &
          (project_id %in% input$project_id | is.null(input$project_id)) &
          (main_risk_cat %in% input$risk_type | is.null(input$risk_type)) &
          (report_date >= input$report_date[1] & report_date <= input$report_date[2])
      )
  })
  
  # Compute summary statistics
  summary_stats <- reactive({
    data <- filtered_data()
    list(
      total_risks = sum(!is.na(data$risk_unique_id)),
      total_mitigations = sum(!is.na(data$unique_mitigation_id)),
      total_cost_pre = sum(data$pre_cost, na.rm = TRUE),
      total_cost_post = sum(data$post_cost, na.rm = TRUE),
      avg_cost_pre = mean(data$pre_cost, na.rm = TRUE),
      avg_cost_post = mean(data$post_cost, na.rm = TRUE),
      avg_prob_pre = mean(data$pre_prob, na.rm = TRUE),
      avg_prob_post = mean(data$post_prob, na.rm = TRUE)
    )
  })
  
  # Generate card UI components
  card_style <- "padding: 15px; background-color: #d9edf7; border-radius: 8px; text-align: center; margin-bottom: 20px;"
  small_text_style <- "font-size: 20px;"  # Smaller font for Total Cost card
  
  output$card_total_risks <- renderUI({
    stats <- summary_stats()
    div(style = card_style, h4("Total Risks"), h3(stats$total_risks))
  })
  
  output$card_total_mitigations <- renderUI({
    stats <- summary_stats()
    div(style = card_style, h4("Total Mitigations"), h3(stats$total_mitigations))
  })
  
  output$card_total_costs <- renderUI({
    stats <- summary_stats()
    div(style = card_style, h4("Total Risk Cost (Pre & Post)"),
        h4(style = small_text_style, paste("£", format(stats$total_cost_pre, big.mark = ","), 
                                           " vs £", format(stats$total_cost_post, big.mark = ","))))
  })
  
  output$card_avg_costs <- renderUI({
    stats <- summary_stats()
    div(style = card_style, h4("Avg Cost (Pre & Post)"),
        h3(paste("£", format(round(stats$avg_cost_pre, 2), big.mark = ","), 
                 " vs £", format(round(stats$avg_cost_post, 2), big.mark = ","))))
  })
  
  output$card_avg_probability <- renderUI({
    stats <- summary_stats()
    div(style = card_style, h4("Avg Probability (Pre & Post)"),
        h3(paste(round(stats$avg_prob_pre * 100, 1), "% vs", round(stats$avg_prob_post * 100, 1), "%")))
  })
  
  # Risk Trend Over Time Plot
  output$risk_trend_plot <- renderPlot({
    data <- filtered_data() %>%
      group_by(report_date) %>%
      summarise(total_risks = n_distinct(risk_unique_id), .groups = "drop")
    
    ggplot(data, aes(x = report_date, y = total_risks)) +
      geom_line(color = "blue", size = 1) +
      theme_minimal() +
      labs(title = "Risk Trend Over Time", x = "Report Date", y = "Number of Risks")
  })
  
  # Risks & Mitigations per Project Plot
  output$risk_mitigation_per_project <- renderPlot({
    data <- filtered_data() %>%
      group_by(project_id) %>%
      summarise(total_risks = n_distinct(risk_unique_id),
                total_mitigations = n_distinct(unique_mitigation_id), .groups = "drop")
    
    ggplot(data, aes(x = project_id)) +
      geom_bar(aes(y = total_risks, fill = "Risks"), stat = "identity", alpha = 0.7) +
      geom_bar(aes(y = total_mitigations, fill = "Mitigations"), stat = "identity", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Number of Risks & Mitigations per Project", x = "Project ID", y = "Count") +
      scale_fill_manual(values = c("Risks" = "red", "Mitigations" = "green")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$mitigations_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

shinyApp(ui = ui, server = server)
