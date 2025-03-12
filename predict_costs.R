library(tidymodels)
library(rsample)
library(parsnip)
library(workflows)
library(recipes)
#install.packages("ranger", repos = "https://cloud.r-project.org/")
library(ranger)

# Load and clean data
data_model <- data_cleaned %>%
    mutate(Cluster = case_when(
        main_risk_cat %in% c(
            "1. Business Strategy and Objectives", 
            "10. System Integration, Verification & Validation", 
            "6. Contractual/Legal",
            "9. Hardware/Software Development"
        ) ~ 1,
        
        main_risk_cat %in% c(
            "11. Supply Chain", 
            "5. Project Management",
            "12. Acquisition and Offsets",
            "15. Project Management"
        ) ~ 2,
        
        main_risk_cat %in% c(
            "8. System Engineering",
            "10. System Integration",
            "13. Logistic Support and Services",
            "9. Hardware/Software development",
            "14. Other Risks and Contingencies"
        ) ~ 3,
        
        main_risk_cat %in% c(
            "4. Customers", 
            "7. Finance",
            "2. Bid Management"
        ) ~ 4,
        
        TRUE ~ NA_real_ # Assign NA if no match (optional)
    )) %>%
    mutate(Cluster = as.factor(Cluster)) %>% # Convert to factor
    select(Cluster, criticality, strategy, pre_cost, post_cost  ) %>%
    mutate(
        Cluster = as.factor(Cluster),
        Criticality = factor(criticality, levels = c("H1", "H2", "H3")), # Ensure correct ordering
        Strategy = as.factor(strategy)
    ) %>%
    na.omit() # Remove rows with missing values

# Split data into training and test sets
set.seed(123)

data_split <- initial_split(data_model, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define Random Forest Model
rf_model <- rand_forest(trees = 500) %>%
    set_engine("ranger") %>%
    set_mode("regression")

# Workflow for Pre-Mitigation Cost
pre_mit_wf <- workflow() %>%
    add_recipe(recipe(pre_cost ~ Cluster + Criticality + Strategy, data = train_data)) %>%
    add_model(rf_model)

# Workflow for Post-Mitigation Cost
post_mit_wf <- workflow() %>%
    add_recipe(recipe(post_cost ~ Cluster + Criticality + Strategy, data = train_data)) %>%
    add_model(rf_model)

# Train models
pre_mit_fit <- fit(pre_mit_wf, data = train_data)
post_mit_fit <- fit(post_mit_wf, data = train_data)

# Example new project data
set.seed(123) # For reproducibility

new_project <- tibble(
    Cluster = factor(sample(1:4, 100, replace = TRUE)), # Randomly assign clusters
    Criticality = factor(sample(c("H1", "H2", "H3"), 100, replace = TRUE)), # Random criticality
    Strategy = factor(sample(c("Reduce", "Accept", "Transfer", "Reject"), 100, replace = TRUE)) # Random strategies
) %>%  mutate(cluster_name = 
                  case_when(
                      Cluster == 1 ~ "Strategic & Technical Risks",
                      Cluster == 2 ~ "Supply Chain & Management Risks",
                      Cluster == 3 ~ "Financial & Operational Risks",
                      Cluster == 4 ~ "Business & Bidding Risks"),
              criticality_name = 
                  case_when(
                      Criticality == "H1" ~ "High Criticality",
                      Criticality == "H2" ~ "Medium Criticality",
                      Criticality == "H3" ~ "Low Criticality"),
              criticality_name = factor(criticality_name, 
                                        levels = c("High Criticality", "Medium Criticality", "Low Criticality")))




# Predict pre-mitigation cost
new_project$Pre_Mitigation_Cost <- predict(pre_mit_fit, new_data = new_project)$.pred

# Predict post-mitigation cost
new_project$Post_Mitigation_Cost <- predict(post_mit_fit, new_data = new_project)$.pred

# Calculate savings
new_project$Cost_Savings <- new_project$Pre_Mitigation_Cost - new_project$Post_Mitigation_Cost

# View results
print(new_project)


# Plot

prediction_plot <- ggplot(new_project, aes(x = Pre_Mitigation_Cost*1000, y = Cost_Savings*1000, 
                        color = cluster_name, size = criticality_name, shape = cluster_name)) +
    geom_point(alpha = 0.8) +
    # scale_color_manual(values = c("red", "blue", "green", "purple")) + # Customize cluster colors
    scale_size_manual(values = c(6, 4, 2)) + # H1 largest, H3 smallest
    scale_shape_manual(values = c(15, 16, 17, 18)) + # Different shapes for clusters
    labs(title = "Pre-Mitigation Cost vs. Cost Savings",
         x = "Pre-Mitigation Cost",
         y = "Cost Savings",
         color = "Risk Cluster",
         size = "Criticality",
         shape = "Risk Cluster") +
    theme_minimal()

# Open SVG device using svglite
svglite("prediction_plot.svg", width = 11, height = 8)

# Plot the dendrogram
plot(prediction_plot, labels = colnames(co_occ_matrix),
     main = "Risk Co-Occurrence Clustering",
     cex = 0.8, ylab = "", xlab = "", yaxt = "n")

# Close the SVG device
dev.off()
