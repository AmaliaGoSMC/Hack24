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
# 
# 
# 2. Clusters of Interdependent Risks
# 
# Cluster 1: 5. Project Management, 1. Business Strategy, and 10. System Integration
# → These risks often appear together, likely because poor strategy and management lead to integration failures.
# 
# Cluster 2: 2. Bid Management, 12. Acquisition, 13. Logistic Support
# → These risks are supply-chain related, meaning projects with procurement issues tend to face multiple related risks.
# 
# Cluster 3: 4. Customers and 7. Finance
# → Financial risks and customer-related issues frequently co-occur, possibly due to budget constraints affecting customer satisfaction.
# 
# Cluster 4: 8. System Engineering and 9. Hardware/Software Development
# → Technical risks tend to cluster together, suggesting that problems in one area (e.g., system engineering) often lead to software/hardware failures.
# 
# 
# 
# 3. High-Interdependency Risks (Bridge Risks)
# 
# 5. Project Management appears in multiple clusters, meaning it is a critical risk category that influences multiple other risks.
# 
# 10. System Integration connects to business strategy and engineering, meaning projects with integration issues are prone to cascading failures.
# 
#     
#     What This Means for Risk Mitigation
# 
# Focus on Highly Connected Risks
# 
# Risks like Project Management (5) and System Integration (10) should be prioritized since they impact multiple areas.
# 
# 
# Mitigate Risks in Groups
# 
# If a project faces Bid Management risks (2), it’s likely to also have Logistics and Acquisition issues. Addressing them together is more effective.
# 
# 
# Monitor Risks That Bridge Clusters
# 
# Risks that connect different clusters (e.g., Supply Chain (11)) indicate areas where cascading failures may start.
# 
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


# 1. Cluster Column
# 
# Each risk category is assigned to one of four clusters.
# 
# Risks in the same cluster tend to co-occur more frequently and are likely interdependent.
# 
# Example:
#     
# Cluster 1 includes "System Integration, Verification & Validation", "Business Strategy and Objectives", etc.
# 
# Cluster 2 includes "Project Management" (which appears twice, possibly due to naming issues).
# 
# Cluster 3 includes "Customers", "Finance", "Acquisition and Offsets", etc.
# 
# Cluster 4 includes "Bid Management".
# 
# 
# 
# Key Insight:
#     
#     Clustered risks should be managed together because failures in one risk type could trigger failures in others.

#     
#     2. Degree Centrality (How Connected a Risk Is)
# 
# A measure of how many other risks a risk is directly linked to.
# 
# Higher values (closer to 1) mean the risk is highly interconnected with others.
# 
# Example:
#     
#     "Project Management" (1.000) has the highest degree centrality → It is connected to all other risks.
# 
# "System Integration, Verification & Validation" (0.933) is also very central.
# 
# 
# 
# Key Insight:
#     
#     High-degree risks should be monitored closely because they have the most direct dependencies with other risks.
# 

#     
#     3. Betweenness Centrality (Bridge Risks)
# 
# Measures how often a risk acts as a bridge between different clusters.
# 
# Higher values mean the risk is a critical connector in risk interactions.
# 
# Example:
#     
#     "System Integration, Verification & Validation" (0.71) has high betweenness → Likely to be a bottleneck or failure point that could disrupt multiple areas.
# 
# "Finance" (0.86) also has a high value, indicating that financial risks link multiple risk types.
# 
# 
# 
# Key Insight:
#     
#     Risks with high betweenness are key failure points—if they fail, they could disrupt multiple risk clusters.
# 
#     
#     4. Closeness Centrality (How Fast a Risk Can Spread)
# 
# Represents how quickly information (or failure) can spread through the network.
# 
# Higher values mean the risk is centrally positioned and can influence other risks faster.
# 
# Example:
#     
#     "Contractual/Legal" (0.95) has high closeness centrality, meaning it can quickly affect the entire system.
# 
# "Business Strategy and Objectives" (0.96) is another fast-spreading risk.
# 
# 
# 
# Key Insight:
#     
#      These risks can rapidly impact the entire system—they should be proactively mitigated.
# 

#     5. Co-Occurrence Importance (How Often the Risk Appears)
# 
# The total number of connections a risk has.
# 
# Higher values indicate that a risk appears frequently across multiple projects.
# 
# Example:
#     
#     "Supply Chain" (1.000) appears in every cluster → A major risk factor.
# 
# "Project Management" (0.97) also appears frequently.
# 
# 
# 
# Key Insight:
#     
#     Frequent risks require continuous monitoring and mitigation because they affect many different projects.
# 
#     
#     Final Summary: What Actions Can You Take?
#     
#     1. Focus on Clustered Risks Together
# 
# Risks in the same cluster should be analyzed together.
# 
# Example: "System Integration" and "Business Strategy" are in the same cluster, so strategy failures might lead to integration issues.
# 
# 
# 
# 2. Prioritize High-Centrality Risks
# 
# High Degree Centrality → "Project Management" and "System Integration" → Need strong oversight.
# 
# High Betweenness → "Finance" and "System Integration" → Prevent failures in these areas to avoid cascading problems.
# 
# 
# 
# 3. Watch Out for Fast-Spreading Risks
# 
# "Contractual/Legal" and "Business Strategy" have high Closeness Centrality.
# 
# If these go wrong, they could quickly escalate across projects.
# 
# 
# 
# 4. Mitigate Risks That Appear Frequently
# 
# "Supply Chain" and "Project Management" have high Co-Occurrence Importance.
# 
# These risks are present across many projects and need robust contingency plans.

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
