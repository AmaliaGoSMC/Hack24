# Risk Clustering: Highlighting interdependent risks prone to cascading failures.


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

# This dendrogram shows how different Risk Categories are grouped based on their co-occurrence patterns within projects. It helps identify interdependent risks that are more likely to cascade.
# 
# Key Takeaways from Your Dendrogram
# 
# 1. Hierarchical Structure
# 
# Risks closer together on the tree are more likely to co-occur in projects.
# 
# Risks merged at lower heights (shorter branches) are highly interdependent.
# 
# Risks merged at higher heights are less related but still share some co-occurrence.
# 

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
plot(risk_hclust, labels = colnames(co_occ_matrix), main = "Risk Co-Occurrence Clustering", cex = 0.8)

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
ggplot(top_20_project_savings, aes(x = reorder(project_id, mitigation_savings), 
                                   y = mitigation_savings, fill = as.factor(Cluster))) +
    geom_bar(stat = "identity", alpha = 0.8) +
    coord_flip() +
    theme_minimal() +
    labs(title = "Top 20 Projects with the Highest Mitigation Savings: Cost Breakdown by Cluster", 
         x = "Project ID", y = "Total Savings from Mitigation", fill = "Risk Cluster") +
    scale_fill_brewer(palette = "Set2") # Assign distinct colors to clusters

# Comments for Rosa to add to narrative
# 🔴 Cluster 1: Strategic & Technical Risks
# 
# These risks affect the overall direction and technical success of a project. If they go wrong, the project could fail due to poor planning, integration issues, or legal complications.
# 📌 Includes:
#     
#     Business Strategy & Objectives → Poor planning, unclear goals, shifting priorities.
# 
# System Integration, Verification & Validation → Failures when combining different systems.
# 
# Contractual/Legal → Legal disputes, compliance failures, contract breaches.
# 
# Hardware/Software Development → Problems in building or coding technology.
# 
# 
# 🛑 Why It’s Risky:
#     If you don’t get these right at the start, your project may never be completed successfully.
# 
# 
#     
#     🟠 Cluster 2: Supply Chain & Management Risks
# 
# These risks involve managing resources, logistics, and supplies. If things go wrong, the project might face delays, shortages, or extra costs.
# 📌 Includes:
#     
#     Supply Chain → Delivery failures, missing parts, supplier issues.
# 
# Project Management → Poor leadership, bad coordination, missed deadlines.
# 
# 
# 🛑 Why It’s Risky:
#     Even if everything else is perfect, a project can fail if it lacks resources or isn’t managed well.
# 
# 
#     
#     🟡 Cluster 3: Financial & Operational Risks
# 
# These risks impact how money, customers, and operations are handled. If they go wrong, the project may become too expensive or fail to meet expectations.
# 📌 Includes:
#     
#     Acquisition and Offsets → Buying the wrong resources or bad trade-offs.
# 
# Logistic Support & Services → Problems with maintenance, staffing, and day-to-day operations.
# 
# Other Risks & Contingencies → Unexpected problems affecting operations.
# 
# Customers → Issues with meeting client needs or losing customers.
# 
# Finance → Budget overruns, financial mismanagement.
# 
# System Engineering → Complex technical failures.
# 
# 
# 🛑 Why It’s Risky:
#     Even well-managed projects can collapse due to financial issues or operational failures.
# 
# 
# ---
#     
#     🟢 Cluster 4: Business & Bidding Risks
# 
# These risks affect the business side of the project—whether it wins funding, secures contracts, or stays competitive.
# 📌 Includes:
#     
#     Bid Management → Losing funding or contracts.
# 
# 
# 🛑 Why It’s Risky:
#     A project can fail before it even starts if it doesn’t win bids or funding.
# 
# 
#     
#     📢 Final Takeaway
# 
# If Cluster 1 risks are high → The whole project strategy and technical execution is at risk.
# 
# If Cluster 2 risks are high → The project may struggle with delays and poor coordination.
# 
# If Cluster 3 risks are high → The project may suffer from financial issues or operational failures.
# 
# If Cluster 4 risks are high → The project may not even get off the ground due to lost bids or funding.
