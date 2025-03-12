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
                   tags$li("Recommendation 1: Based on the predictive analysis from the risk velocity graphs, it is evident that the highest rate of change for risk criticality, probability, and cost occurs early in the risk lifecycle (within 300 days). Specifically, these changes are most impactful within the initial period after a risk is reported. Therefore, it is crucial to implement mitigation strategies as soon as possible after risks are identified to prevent escalation and effectively manage their impact."),
                   tags$li("Recommendation 2: Based on the clustering analysis from the dendrogram, it is evident which risk categories tend to co-occur. This insight is valuable for identifying potential risks that are likely to arise in a project when certain other risks are present. By understanding these co-occurrence patterns, project managers can better anticipate and prepare for potential risks, ensuring they stay on track and are proactive in managing risk dependencies."),
                   tags$li("Recommendation 3: Based on the predictive analysis from the risk cost graphs, it is projected that both pre- and post-mitigation costs will decrease in 2020, as indicated by the moving average over the past 30 months. This trend provides valuable insights for project planning and cost allocation, enabling more accurate budgeting and resource management. By leveraging these projections, project managers can optimise financial planning and ensure more efficient use of resources.")
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
                 HTML(paste0("The graph on the <b>right</b> shows the number of risks reported over a 30-month period.")),
                 br(),
                 HTML(paste0("<br> The graph on the <b>left</b> shows the monthly emergence rate of risks. Notably, it indicates that May and September have the highest number of risks raised, while June and December have lower emergence rates.")),
                 fluidRow(
                     column(6, 
                            plotOutput("risk_trend_plot")),
                     column(6, 
                            tags$img(src = "images/monthly_emergence_rate.svg", width = "500", height = "500"))
                 ),
                 br(),
                 h3("Risks & Mitigations per Project"),
                 p("The graph highlights the number of mitigations and risks per project, indicating which project required the most mitigations to address risks. This is important because it reveals the distribution of mitigations across projects and allows you to start asking questions about why some projects require more mitigations, such as differences in risk impact, project size, or cost."),
                 plotOutput("risk_mitigation_per_project")
        ),
        
        tabPanel("Mitigations", 
                 h3("Mitigation impact"),
                 p("The chart below illustrates the success of mitigation by comparing the average change in risk probability and cost before and after mitigation."),
                 p("On average, mitigation resulted in a decrease in both risk probability and cost, indicating a reduced impact on the organisation overall. This demonstrates the effectiveness of mitigation strategies in managing risk."),
                 tags$img(src = "images/evaluating_success_rate_of_mitigation.svg", width = "500", height = "500")
                 ),
        tabPanel("Opportunities", 
                 h3("Risk Opportunities"),
                 p("This plot shows how different risk categories tend to occur together in projects. The closer two risk categories are in the diagram, the more often they co-occur. This helps identify potential risk dependencies and informs mitigation strategies."),
                 div(style = "margin-top: -50px;", 
                     tags$img(src = "images/risk_dendrogram.svg", width = "500", height = "500")),
                 p("This chart shows the Top 20 Projects with the highest mitigation savings, grouped by risk clusters: Strategic & Technical, Supply Chain & Management, Financial & Operational, and Business & Bidding Risks. It helps identify which projects have benefited most from mitigation strategies and how savings are distributed across risk categories."),
                 div(style = "margin-bottom: -50px;", 
                     tags$img(src = "images/top_20_plot.svg", width = "500", height = "500")),
        ),
        
        tabPanel("Predictive Analysis", 
                 h3("Forecasting Risk"),
                 p("This section will include predictive models for risk trends."),
                 selectInput("analysis_type", "Select Analysis Type:", 
                             choices = c("Risk Velocity", "Risk Cost", "Regression")),
                 uiOutput("selected_analysis")
        )
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
  
  output$selected_analysis <- renderUI({
      if (input$analysis_type == "Risk Velocity") {
          fluidRow(
              column(12, 
                     p("This graph illustrates the rate at which risks escalate in criticality over time, with H1 being the highest level of criticality. Focusing on the pre-mitigation criticality of risks, it is evident that the first 300 days after a risk is reported are crucial. During this period, the highest rate of escalation occurs."),
                     p("The blue points indicate risks transitioning from H3 to H2, while the red points show risks escalating from H2 to the most critical level, H1.")
                     ),
              column(12, 
                     tags$img(src = "images/risk_velocity_criticality.svg", width = "500", height = "500")),
              fluidRow(
                  p("The following two graphs build upon the previous analysis by examining the rate of change in risk probability and cost before mitigation. Consistent with the earlier findings, these graphs also demonstrate that the highest rate of change occurs within the first 300 days after a risk is reported"),
                  column(6, 
                         tags$img(src = "images/cost_change.svg", width = "500", height = "500")),
                  column(6, 
                         tags$img(src = "images/likelihood_impact.svg", width = "500", height = "500"))
              ),
          )
      } else if (input$analysis_type == "Risk Cost") {
          # Add content for Risk Cost here
          tags$p("This graph shows the total pre- and post-risk costs over a recent period and forecasts these costs for the next year, helping managers plan and allocate resources effectively.")
          tags$img(src = "images/pre_post_cost_forecast.png", width = "700", height = "500")
      } else {
          # Add content for Regression here
          p("Regression analysis will be displayed here.")
      }
  })
}

shinyApp(ui = ui, server = server)
