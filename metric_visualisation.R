# Author: Catherine Macfarlane 
# Date: 2025-03-11
# Description: This script visulises the metrics calculated for the challenge.
library("readxl")
library("janitor")
library("tidyverse")
library("plotly")
library("htmlwidgets")

source("metric_calculation.R")
install.packages("svglite")


# Risk Velocity: Measuring the speed at which risks escalate -----------------------------------------------------
#1# Plot criticality levels over time before mitigation

# Plot scatter plot of time difference vs. rate of change
# Risks change more rapidly at first and then stablise over time
plot1 <- ggplot(criticality_range, aes(x = time_diff,
                                       y = rate_of_change, 
                                       color = factor(criticality_level))) +
    geom_point(size = 1.5, aes(text = paste0("Time difference: ", time_diff,
                                             "<br> Rate of critical change: ", round(rate_of_change, 2),  # Round to 2 decimal places
                                             "<br> Trend: ", trend,
                                             "<br> Strategy: ", strategy)), alpha = 0.7) +
    labs(title = "Time Difference vs. Rate of Critical Change", 
         x = "Time Difference (Days)", 
         y = "Rate of Change",
         color = "Criticality Level") +
    theme_classic() +
    geom_vline(xintercept = 300, linetype = "dashed", color = "red") 

# Convert to Plotly and customize
plotly_plot <- ggplotly(plot1, tooltip = "text")
plotly_plot

ggsave("images/risk_velocity_criticality.svg", plot = plot1, width = 8, height = 6, dpi = 300)

#2 Plot of change in critcial level before and after mitigation.

# Prepare data for Sankey diagram
sankey_data <- postmit_criticality_range %>%
    mutate(
        from = paste0("Before: H", criticality_level),
        to = paste0("After: H", postmit_criticality_level)
    )
# Count transitions
transition_counts <- sankey_data %>%
    group_by(from, to) %>%
    summarise(count = n())

# Prepare nodes and links
sankey_nodes <- data.frame(
    name = unique(c(transition_counts$from, transition_counts$to))
)
sankey_links <- transition_counts %>%
    mutate(
        source = match(from, sankey_nodes$name) - 1,
        target = match(to, sankey_nodes$name) - 1
    )

# Define color scale
my_color <- 'd3.scaleOrdinal() .domain(["Before: 1", "Before: 2", "Before: 3", "After: 1", "After: 2", "After: 3"]) .range(["blue", "blue", "blue", "red", "red", "red"])'

# Create Sankey diagram
sankeyNetwork(Links = sankey_links, Nodes = sankey_nodes, Source = "source", Target = "target", Value = "count", NodeID = "name", colourScale = JS(my_color))



saveWidget(sankeyNetwork(Links = sankey_links, Nodes = sankey_nodes, Source = "source", Target = "target", Value = "count", NodeID = "name", colourScale = JS(my_color)), "sankey_diagram.html")
# #2. Evaluating the success rate of mitigating risks over time  -----------------------------------------------------

# Density plot of average probability changes before and after mit

plot2 <- ggplot(avg_changes, aes(x = avg_prob_change, y = avg_cost_change, text = paste("Probability Change:", round(avg_prob_change, 2), "\nCost Change:", round(avg_cost_change, 2)))) +
    geom_point(aes(color = avg_prob_change + avg_cost_change), alpha = 1, size = 3) +
    scale_color_gradient2(low = "darkgreen", high = "darkred", midpoint = 0, name = "Combined\nChange") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(title = "Impact of Mitigations on Probability vs Cost",
         subtitle = "Each point represents a unique risk",
         x = "Average Probability Change", 
         y = "Average Cost Change") +
    theme_minimal() +
    annotate("text", x = min(avg_changes$avg_prob_change)/2, y = max(avg_changes$avg_cost_change)/2, 
             label = "Lower probability\nHigher cost", color = "gray30") +
    annotate("text", x = max(avg_changes$avg_prob_change)/2, y = max(avg_changes$avg_cost_change)/2, 
             label = "Higher probability\nHigher cost", color = "gray30") +
    annotate("text", x = min(avg_changes$avg_prob_change)/2, y = min(avg_changes$avg_cost_change)/2, 
             label = "Lower probability\nLower cost", color = "gray30") +
    annotate("text", x = max(avg_changes$avg_prob_change)/2, y = min(avg_changes$avg_cost_change)/2, 
             label = "Higher probability\nLower cost", color = "gray30")

plotly_plot2 <- ggplotly(plot2, tooltip = "text")
plotly_plot2

ggsave("images/evaluating_success_rate_of_mitigation.svg", plot = plot2, width = 8, height = 6, dpi = 300)


#  3. Emergence Rate: Identifying periods of triggers associated with new risk idenitifcation ----------------------------------------------------- 

plot3 <- ggplot(emergence_rate, aes(x = month_name, y = emergence_rate, 
                                    text = paste0("Emergence Rate: ", round(emergence_rate, 2)))) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Emergence Rate per Month",
         x = "Month", y = "Emergence Rate") +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )

plotly_plot3 <- ggplotly(plot3, tooltip = "text")
plotly_plot3

ggsave("images/monthly_emergence_rate.svg", plot = plot3, width = 8, height = 6, dpi = 300)


#4. Likelihood of Risk and Impact Drift: Tracking shifts in project risk exposure ----------------------------------------------------- 
plot4 <- ggplot(probability_change, aes(x = time_diff, y = rate_of_change,  text = paste0("Rate of change: ", round(rate_of_change, 4), 
                                                                                          "<br>Time difference:",time_diff ))) +
    geom_point(color = "skyblue") +
    labs(title = "Rate of Change in Probability Over Time",
         x = "Time Difference (Days)", y = "Rate of Change") +
    theme_classic() +
    geom_vline(xintercept = 300, linetype = "dashed", color = "red")

ggsave("images/likelihood_impact.svg", plot = plot4, width = 8, height = 6, dpi = 300)


plotly_plot4 <- ggplotly(plot4, tooltip = "text")
plotly_plot4

# Plot rate of change in cost over time
plot5 <- ggplot(cost_change, aes(x = time_diff, y = rate_of_change,  text = paste0("Rate of change: ", round(rate_of_change, 4), 
                                                                                   "<br>Time difference: ",time_diff ))) +
    geom_point(color = "skyblue") +
    labs(title = "Rate of Change in Cost Over Time",
         x = "Time Difference (Days)", y = "Rate of Change") +
    theme_classic() +
    geom_vline(xintercept = 300, linetype = "dashed", color = "red")

ggsave("images/cost_change.svg", plot = plot5, width = 8, height = 6, dpi = 300)


plotly_plot5 <- ggplotly(plot5, tooltip = "text")
plotly_plot5
