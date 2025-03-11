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
