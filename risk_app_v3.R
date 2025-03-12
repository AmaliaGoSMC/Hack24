library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyWidgets)  # For styled cards

# Load the cleaned dataset
data_cleaned <- read.csv("~/Downloads/cleaned_data.csv", stringsAsFactors = FALSE)

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
                 h3("Key Risk Metrics"),
                 br(),
                 fluidRow(
                   column(4, div(style = "padding: 15px; background-color: #d9edf7; border-radius: 8px; text-align: center; margin-bottom: 20px;", uiOutput("card_total_risks"))),
                   column(4, div(style = "padding: 15px; background-color: #d9edf7; border-radius: 8px; text-align: center; margin-bottom: 20px;", uiOutput("card_total_mitigations"))),
                   column(4, div(style = "padding: 15px; background-color: #d9edf7; border-radius: 8px; text-align: center; margin-bottom: 20px;", uiOutput("card_total_cost")))
                 ),
                 br(), br(),
                 fluidRow(
                   column(4, div(style = "padding: 15px; background-color: #d9edf7; border-radius: 8px; text-align: center; margin-bottom: 20px;", uiOutput("card_avg_cost_pre"))),
                   column(4, div(style = "padding: 15px; background-color: #d9edf7; border-radius: 8px; text-align: center; margin-bottom: 20px;", uiOutput("card_avg_cost_post"))),
                   column(4, div(style = "padding: 15px; background-color: #d9edf7; border-radius: 8px; text-align: center; margin-bottom: 20px;", uiOutput("card_avg_probability")))
                 )
        ),
        
        tabPanel("Risks", 
                 h3("Risk Trends Over Time"),
                 plotOutput("risk_trend_plot")),
        
        tabPanel("Mitigations", 
                 DTOutput("mitigations_table")),
        
        tabPanel("Predictive Analysis", 
                 h3("Predictive Risk Analysis"),
                 
                 selectInput("prediction_type", "Select Prediction Type", 
                             choices = c("Risk Escalation (Risk Velocity)", 
                                         "Mitigation Effectiveness (Resolution Rate)",
                                         "Risk Recurrence", 
                                         "Emerging Risks (Emergence Rate)",
                                         "Risk Cost")),
                 
                 br(),
                 textOutput("prediction_explanation"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Filtered Data
  filtered_data <- reactive({
    data_cleaned %>%
      filter(
        (is.null(input$organisation) | organisation %in% input$organisation) &
          (is.null(input$portfolio_id) | portfolio_id %in% input$portfolio_id) &
          (is.null(input$business_area) | business_area %in% input$business_area) &
          (is.null(input$project_id) | project_id %in% input$project_id) &
          (report_date >= input$report_date[1] & report_date <= input$report_date[2])
      )
  })
  
  # Compute summary statistics
  summary_stats <- reactive({
    data <- filtered_data()
    list(
      total_risks = sum(!is.na(data$risk_unique_id)),
      total_mitigations = sum(!is.na(data$unique_mitigation_id)),
      total_cost = sum(data$pre_cost, na.rm = TRUE),
      avg_cost_pre = mean(data$pre_cost, na.rm = TRUE),
      avg_cost_post = mean(data$post_cost, na.rm = TRUE),
      avg_prob_pre = mean(data$pre_probability, na.rm = TRUE),
      avg_prob_post = mean(data$post_probability, na.rm = TRUE)
    )
  })
  
  output$prediction_explanation <- renderText({
    switch(input$prediction_type,
           "Risk Escalation (Risk Velocity)" = "Predict how quickly a risk might escalate from low to high criticality.",
           "Mitigation Effectiveness (Resolution Rate)" = "Assess whether mitigation actions reduce risk probability and cost effectively.",
           "Risk Recurrence" = "Identify risks that are likely to reappear in future projects.",
           "Emerging Risks (Emergence Rate)" = "Forecast when new risks are likely to emerge.",
           "Risk Cost" = "Forecast the financial impact of risks.")
  })
}

shinyApp(ui = ui, server = server)