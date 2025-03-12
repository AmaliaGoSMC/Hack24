library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)

# Load the cleaned dataset
data_cleaned <- read.csv("~/Downloads/cleaned_data.csv", stringsAsFactors = FALSE)

# Convert Start column to Date format
data_cleaned$start <- as.Date(data_cleaned$start, format = "%Y-%m-%d")

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # Use modern theme
  
  titlePanel("Risk & Mitigation Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      
      selectInput("organisation", "Select Organisation", 
                  choices = unique(data_cleaned$organisation), selected = NULL, multiple = TRUE),
      
      selectInput("portfolio", "Select Portfolio", 
                  choices = unique(data_cleaned$portfolio_id), selected = NULL, multiple = TRUE),
      
      selectInput("business_area", "Select Business Area", 
                  choices = unique(data_cleaned$business_area), selected = NULL, multiple = TRUE),
      
      selectInput("project", "Select Project", 
                  choices = unique(data_cleaned$project_id), selected = NULL, multiple = TRUE),
      
      sliderInput("start_date", "Filter by Start Date", 
                  min = min(data_cleaned$start, na.rm = TRUE), 
                  max = max(data_cleaned$start, na.rm = TRUE),
                  value = range(data_cleaned$start, na.rm = TRUE),
                  timeFormat = "%Y-%m-%d"),
      
      width = 3  # Sidebar width
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", 
                 h3("Risk Overview"),
                 plotOutput("risk_plot"),
                 tableOutput("summary_table")),
        
        tabPanel("Risks", 
                 DTOutput("risks_table")),
        
        tabPanel("Mitigations", 
                 DTOutput("mitigations_table")),
        
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
          (portfolio_id %in% input$portfolio | is.null(input$portfolio_id)) &
          (business_area %in% input$business_area | is.null(input$business_area)) &
          (project_id %in% input$project_id | is.null(input$project_id)) &
          (start >= input$start_date[1] & start <= input$start_date[2])
      )
  })
  
  # Summary Table
  output$summary_table <- renderTable({
    filtered_data() %>%
      summarise(
        Total_Risks = n(),
        Avg_Cost = mean(pre_cost, na.rm = TRUE),
        Avg_Probability = mean(pre_prob, na.rm = TRUE)
      )
  })
  
  # Risk Overview Plot
  output$risk_plot <- renderPlot({
    data <- filtered_data() %>%
      group_by(criticality) %>%
      summarise(Count = n(), .groups = "drop")
    
    ggplot(data, aes(x = criticality, y = Count, fill = criticality)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Number of Risks by Criticality", x = "Criticality Level", y = "Count") +
      theme(legend.position = "none")
  })
  
  # Risks Table
  output$risks_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  # Mitigations Table (Assuming mitigations are a subset of the dataset)
  output$mitigations_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

# Run App
shinyApp(ui = ui, server = server)
