
install.packages(c("shiny", "bslib", "DT", "ggplot2", "dplyr", "readr"))

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)

# Define the path to the Downloads folder (update if needed)
risks_data <- read.csv("/Users/Rosa/Downloads/risks.csv", fileEncoding = "UTF-8")
mitigations_data <- read.csv("/Users/Rosa/Downloads/mitigations.csv", fileEncoding = "UTF-8")

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # Use bslib theme
  
  titlePanel("Risk & Mitigation Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Source: Downloads Folder"),
      p("Risks and mitigations are loaded from the Downloads folder."),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Risks Data", DTOutput("risks_table")),
        tabPanel("Mitigations Data", DTOutput("mitigations_table")),
        tabPanel("Risk Summary", plotOutput("risk_plot"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Display risks table
  output$risks_table <- renderDT({
    req(risks_data)
    datatable(risks_data, options = list(pageLength = 10))
  })
  
  # Display mitigations table
  output$mitigations_table <- renderDT({
    req(mitigations_data)
    datatable(mitigations_data, options = list(pageLength = 10))
  })
  
  # Risk summary plot
  output$risk_plot <- renderPlot({
    req(risks_data)
    
    data <- risks_data %>% 
      group_by(Criticality) %>% 
      summarise(Count = n(), .groups = "drop")
    
    ggplot(data, aes(x = Criticality, y = Count, fill = Criticality)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Number of Risks by Criticality", x = "Criticality Level", y = "Count") +
      theme(legend.position = "none")
  })
}

# Run App
shinyApp(ui = ui, server = server)
