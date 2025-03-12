# Risk Clustering: Highlighting interdependent risks prone to cascading failures.
library(cluster)
library(RColorBrewer)

#  calculate clusters of risks --------------------------------------------

# Count co-occurrences of risk categories within the same Project_ID
risk_co_occurrence <- data_cleaned %>%
    select(project_id, main_risk_cat) %>%
    distinct() %>% # Avoid double-counting the same risk in the same project
    count(main_risk_cat, project_id) %>%
    spread(key = main_risk_cat, value = n, fill = 0)

# Convert to a co-occurrence matrix (Risk Category x Risk Category)
co_occ_matrix <- as.matrix(risk_co_occurrence[,-1]) # Remove Project_ID column
co_occ_matrix <- t(co_occ_matrix) %*% co_occ_matrix # Compute co-occurrences
    

# Perform hierarchical clustering
risk_dist <- dist(co_occ_matrix, method = "euclidean")
risk_hclust <- hclust(risk_dist, method = "ward.D")

# Plot dendrogram to visualize risk dependencies
plot(risk_hclust, labels = colnames(co_occ_matrix), main = "Risk Co-Occurrence Clustering")

# Assign risk categories to clusters
num_clusters <- 4 # Choose based on the dendrogram structure
risk_clusters <- cutree(risk_hclust, k = num_clusters)

# View cluster assignments
risk_cluster_map <- tibble(Risk_Category = names(risk_clusters), Cluster = risk_clusters)
print(risk_cluster_map)

# Load necessary libraries
library(igraph)
library(cluster)

# Step 1: Compute Risk Co-Occurrence Matrix
risk_co_occurrence <- data_cleaned %>%
    select(project_id, main_risk_cat) %>%
    distinct() %>% # Avoid double-counting the same risk in the same project
    count(main_risk_cat, project_id) %>%
    spread(key = main_risk_cat, value = n, fill = 0)

# Convert to a co-occurrence matrix (Risk Category x Risk Category)
co_occ_matrix <- as.matrix(risk_co_occurrence[, -1]) # Remove Project_ID column
co_occ_matrix <- t(co_occ_matrix) %*% co_occ_matrix # Compute co-occurrences

# Normalize matrix to prevent bias from high-frequency risks
co_occ_matrix <- scale(co_occ_matrix)

# Step 2: Perform Hierarchical Clustering
risk_dist <- dist(co_occ_matrix, method = "euclidean")
risk_hclust <- hclust(risk_dist, method = "ward.D")
# Catherine to make pretty
# Plot dendrogram to visualize risk dependencies
library(ggdendro)

plot(risk_hclust, labels = colnames(co_occ_matrix), 
                     main = "Risk Co-Occurrence Clustering", 
                     cex = 0.8, ylab = "", xlab = "", yaxt = "n")

cluster_plot <- recordPlot()

# Convert the co-occurrence matrix into a valid adjacency matrix
co_occ_matrix[co_occ_matrix < 0] <- 0 # Ensure no negative values

# Create the graph with only positive weights
risk_graph <- graph_from_adjacency_matrix(co_occ_matrix, mode = "undirected", weighted = TRUE)

# Check if weights exist
E(risk_graph)$weight <- ifelse(E(risk_graph)$weight > 0, E(risk_graph)$weight, 1) # Set minimum weight to 1

# Compute centrality measures
risk_centrality <- tibble(
    Risk_Category = colnames(co_occ_matrix),
    Degree_Centrality = degree(risk_graph),
    Betweenness_Centrality = betweenness(risk_graph, weights = E(risk_graph)$weight),
    Closeness_Centrality = closeness(risk_graph, weights = E(risk_graph)$weight),
    Co_Occurrence_Importance = rowSums(co_occ_matrix) # Total connections
)

# Normalize Scores for Better Comparison
risk_centrality <- risk_centrality %>%
    mutate(across(Degree_Centrality:Co_Occurrence_Importance, ~ . / max(.)))

# Display results
print(risk_centrality)

# Step 3: Assign Risk Categories to Clusters
num_clusters <- 4 # Choose based on dendrogram structure
risk_clusters <- cutree(risk_hclust, k = num_clusters)

# Store Cluster Assignments
risk_cluster_map <- tibble(Risk_Category = names(risk_clusters), Cluster = risk_clusters)

# Step 4: Compute Centrality Metrics
risk_graph <- graph_from_adjacency_matrix(co_occ_matrix, mode = "undirected", weighted = TRUE)

risk_centrality <- tibble(
    Risk_Category = colnames(co_occ_matrix),
    Degree_Centrality = degree(risk_graph),
    Betweenness_Centrality = betweenness(risk_graph),
    Closeness_Centrality = closeness(risk_graph),
    Co_Occurrence_Importance = rowSums(co_occ_matrix) # Total connections
)

# Normalize Scores for Better Comparison
risk_centrality <- risk_centrality %>%
    mutate(across(Degree_Centrality:Co_Occurrence_Importance, ~ . / max(.)))

# Merge Cluster Assignments with Centrality Scores
risk_cluster_summary <- risk_cluster_map %>%
    left_join(risk_centrality, by = "Risk_Category") %>%
    arrange(Cluster, desc(Co_Occurrence_Importance))

# Step 5: View Results
print(risk_cluster_summary)

# Ensure project_exposure_summary exists
project_exposure_summary <- data_cleaned %>%
    left_join(risk_centrality, by = c("main_risk_cat" = "Risk_Category")) %>%
    group_by(project_id) %>%
    summarise(
        total_risk_exposure = sum(Co_Occurrence_Importance, na.rm = TRUE),
        avg_degree_centrality = mean(Degree_Centrality, na.rm = TRUE),
        avg_betweenness_centrality = mean(Betweenness_Centrality, na.rm = TRUE),
        avg_closeness_centrality = mean(Closeness_Centrality, na.rm = TRUE)
    ) %>%
    arrange(desc(total_risk_exposure))

# Check if the dataframe was created
print(head(project_exposure_summary))
# Select top 10 highest-risk projects
top_projects <- project_exposure_summary %>% 
    slice_max(total_risk_exposure, n = 10)


# cluster costs -----------------------------------------------------------
# Merge project data with clusters and cost information
project_cluster_costs <- data_cleaned %>%
    left_join(risk_cluster_summary, c("main_risk_cat" = "Risk_Category")) %>% # Assign clusters
    group_by(project_id, Cluster) %>%
    summarise(
        total_pre_mitigation_cost = sum(pre_factored_cost, na.rm = TRUE),
        total_post_mitigation_cost = sum(post_factored_cost, na.rm = TRUE),
        mitigation_savings = total_pre_mitigation_cost - total_post_mitigation_cost,
        mitigation_effectiveness = (mitigation_savings / total_pre_mitigation_cost) * 100
    ) %>%
    arrange(desc(total_pre_mitigation_cost))

# View project-cluster cost breakdown
print(project_cluster_costs)
# Select the top 20 projects with the highest total savings
top_20_savings_projects <- project_cluster_costs %>%
    group_by(project_id) %>%
    summarise(total_mitigation_savings = sum(mitigation_savings, na.rm = TRUE)) %>%
    arrange(desc(total_mitigation_savings)) %>%
    slice_head(n = 20) # Select only the top 20

# Filter the cost data for only these projects
top_20_project_savings <- project_cluster_costs %>%
    filter(project_id %in% top_20_savings_projects$project_id)

# Catherine to make pretty

top_20_project_savings = top_20_project_savings %>% 
    mutate(cluster_name = 
               case_when(
                   Cluster == 1 ~ "Strategic & Technical Risks",
                   Cluster == 2 ~ "Supply Chain & Management Risks",
                   Cluster == 3 ~ "Financial & Operational Risks",
                   Cluster == 4 ~ "Business & Bidding Risks"))


plot6 <- ggplot(top_20_project_savings, aes(x = reorder(project_id, mitigation_savings), 
                                   y = mitigation_savings, fill = as.factor(cluster_name), text = paste0("Cluster name:",cluster_name,
                                                                                                         "<br>Mitigation savings:",round(mitigation_savings,2)))) +
    geom_bar(stat = "identity", alpha = 0.8) +
    coord_flip() +
    theme_minimal() +
    labs(title = "Top 20 Projects with the Highest Mitigation Savings: Cost Breakdown by Cluster", 
         x = "", y = "Total Savings from Mitigation", fill = "Risk Cluster") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE) %>%
                           gsub("000$", "K", .)) +
    scale_fill_brewer(palette = "Set2")  # Assign distinct colors to clusters


plotly_plot6 <- ggplotly(plot6, tooltip = "text")
plotly_plot6

ggsave("app/www/images/top_20_plot.svg", plot = plot6, width = 8, height = 6, dpi = 300)



# Comments for Rosa to add to narrative
# 🔴 Cluster 1: Strategic & Business-Critical Risks
# 
# These are big-picture risks that can impact the entire direction of a project. If these go wrong, the project could fail due to poor planning, weak leadership, or compliance failures.
# 
# 📌 Includes:
#     
#     Business Strategy & Objectives → Poor planning, changing goals.
# 
# System Integration, Verification & Validation → Tech systems not working together.
# 
# Project Management → Poor leadership, bad coordination, missed deadlines.
# 
# Contractual/Legal → Contract disputes, regulatory problems.
# 
# Hardware/Software Development → Tech failures that prevent project completion.
# 
# 
# 🛑 Why It’s Risky:
#     If a project doesn’t have a strong plan, leadership, or legal foundation, it may never succeed.

#     
#     🟠 Cluster 2: Operational & Execution Risks
# 
# These risks impact the execution of a project—whether it has the right resources, people, and systems in place to succeed.
# 
# 📌 Includes:
#     
#     Supply Chain → Delays or shortages in materials.
# 
# System Engineering → Complex technical failures.
# 
# System Integration → Issues when merging different components.
# 
# Acquisition & Offsets → Buying the wrong resources or trade-offs.
# 
# Operations & Maintenance → Problems with ongoing project upkeep.
# 
# 
# 🛑 Why It’s Risky:
#     Even a well-planned project can fail if it doesn’t have the right materials, engineering, or maintenance.
# 

#     
#     🟡 Cluster 3: Support & Customer-Facing Risks
# 
# These risks affect the people and processes that support the project. If these fail, the project may suffer from delays, customer complaints, or financial trouble.
# 
# 📌 Includes:
#     
#     Quality → Defective products or unmet standards.
# 
# Logistic Support & Services → Problems with staffing, transportation, and maintenance.
# 
# Customers → Not meeting client needs or complaints.
# 
# Finance → Budget overruns, financial mismanagement.
# 
# 
# 🛑 Why It’s Risky:
#     A project might technically work but still fail due to poor quality, bad logistics, or financial losses.
# 

#     
#     🟢 Cluster 4: Business & Competitive Risks
# 
# These risks impact whether a project can be funded and remain competitive.
# 
# 📌 Includes:
#     
#     Bid Management → Losing funding or contracts.
# 
# Other Risks & Contingencies → Unexpected problems that disrupt work.
# 
# 
# 🛑 Why It’s Risky:
#     Even if a project is well-planned and executed, it might never start if it loses funding.

#     
#     📢 Final Takeaways
# 
# If Cluster 1 risks are high → The whole project direction and leadership is in danger.
# 
# If Cluster 2 risks are high → The project might lack the right materials, engineering, or maintenance.
# 
# If Cluster 3 risks are high → The project may suffer from financial problems, poor quality, or customer issues.
# 
# If Cluster 4 risks are high → The project might fail to get funding or be stopped by unexpected events.

# save the clusters as pictures

# Open JPEG device
jpeg("risk_dendrogram.jpeg", width = 11, height = 8, units = "in", res = 300)

library(Cairo)
library(svglite)

# Open SVG device using svglite
svglite("risk_dendrogram.svg", width = 11, height = 8)

# Plot the dendrogram
plot(risk_hclust, labels = colnames(co_occ_matrix),
     main = "Risk Co-Occurrence Clustering",
     cex = 0.8, ylab = "", xlab = "", yaxt = "n")

# Close the SVG device
dev.off()
