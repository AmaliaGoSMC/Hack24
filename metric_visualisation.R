# Author: Catherine Macfarlane 
# Date: 2025-03-11
# Description: This script visulises the metrics calculated for the challenge.
library("readxl")
library("janitor")
library("tidyverse")
library("plotly")

source("metric_calculation.R")


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
                                             "<br> Strategy: ", strategy))) +
    labs(title = "Time Difference vs. Rate of Critical Change", 
         x = "Time Difference (Days)", 
         y = "Rate of Change",
         color = "Criticality Level") +
    theme_classic() +
    geom_vline(xintercept = 300, linetype = "dashed", color = "red") 

# Convert to Plotly and customize
plotly_plot <- ggplotly(plot1, tooltip = "text")
plotly_plot


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


# #2. Evaluating the success rate of mitigating risks over time  -----------------------------------------------------

# Density plot of average probability changes before and after mit

ggplot(avg_changes, aes(x = avg_prob_change, y = avg_cost_change)) +
    geom_point(aes(color = avg_prob_change + avg_cost_change), alpha = 1, size = 3) +
    scale_color_gradient2(low = "darkgreen", high = "darkred", midpoint = 0,
                          name = "Combined\nChange") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(title = "Impact of Mitigations on Probability vs Cost",
         subtitle = "Each point represents a unique risk",
         x = "Average Probability Change", 
         y = "Average Cost Change") +
    theme_minimal() +
    # Add quadrant labels
    annotate("text", x = min(avg_changes$avg_prob_change)/2, y = max(avg_changes$avg_cost_change)/2, 
             label = "Lower probability\nHigher cost", color = "gray30") +
    annotate("text", x = max(avg_changes$avg_prob_change)/2, y = max(avg_changes$avg_cost_change)/2, 
             label = "Higher probability\nHigher cost", color = "gray30") +
    annotate("text", x = min(avg_changes$avg_prob_change)/2, y = min(avg_changes$avg_cost_change)/2, 
             label = "Lower probability\nLower cost", color = "gray30") +
    annotate("text", x = max(avg_changes$avg_prob_change)/2, y = min(avg_changes$avg_cost_change)/2, 
             label = "Higher probability\nLower cost", color = "gray30")


#  3. Emergence Rate: Identifying periods of triggers associated with new risk idenitifcation ----------------------------------------------------- 

ggplot(emergence_rate, aes(x = month_name, y = emergence_rate)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Emergence Rate per Month",
         x = "Month", y = "Emergence Rate") +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )

#4. Likelihood of Risk and Impact Drift: Tracking shifts in project risk exposure ----------------------------------------------------- 
ggplot(probability_change, aes(x = report_date, y = rate_of_change)) +
    geom_point() +
    labs(title = "Rate of Change in Probability Over Time",
         x = "Report Date", y = "Rate of Change") +
    theme_classic()

# Plot rate of change in cost over time
ggplot(cost_change, aes(x = report_date, y = rate_of_change)) +
    geom_point() +
    labs(title = "Rate of Change in Cost Over Time",
         x = "Report Date", y = "Rate of Change") +
    theme_classic()


