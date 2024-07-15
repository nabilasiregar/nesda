library(igraph)

load("causal_graph_0.05_with_whitelist.RData")
original_igraph <- convert_to_igraph(original_graph)

load("bootstrap_graph.RData")
bootstrap_igraph_0 <- lapply(results_0, convert_to_igraph)

load("bootstrap_graph_10.RData")
bootstrap_igraph_10 <- lapply(results_10, convert_to_igraph)

load("bootstrap_graph_20.RData")
bootstrap_igraph_20 <- lapply(results_20, convert_to_igraph)

bootstrap_igraph_list <- c(bootstrap_igraph_0, bootstrap_igraph_10, bootstrap_igraph_20)  

# extract edges from the original graph
original_edges <- get.edgelist(original_igraph)
original_edges_set <- unique(paste(original_edges[,1], original_edges[,2], sep="-"))

# check presence of original edges in a bootstrap graph
check_edge_presence <- function(bootstrap_graph, original_edges_set) {
  bootstrap_edges <- get.edgelist(bootstrap_graph)
  bootstrap_edges_set <- unique(paste(bootstrap_edges[,1], bootstrap_edges[,2], sep="-"))
  intersect(original_edges_set, bootstrap_edges_set)
}

edges_present_in_bootstrap <- lapply(bootstrap_igraph_list, check_edge_presence, original_edges_set)

#frequency of each edge across all bootstraps
edge_frequencies <- table(unlist(edges_present_in_bootstrap))

# determine which edges appear in more than 80% of bootstrap samples
threshold <- length(bootstrap_igraph_list) * 0.8
common_edges <- names(edge_frequencies[edge_frequencies > threshold])

common_edges

max_frequency <- max(edge_frequencies)
most_frequent_edges <- names(edge_frequencies[edge_frequencies == max_frequency])

most_frequent_edges_df <- data.frame(
  Edge = most_frequent_edges,
  Frequency = as.numeric(edge_frequencies[most_frequent_edges]),
  Percentage = (as.numeric(edge_frequencies[most_frequent_edges]) / length(bootstrap_igraph_list)) * 100
)

most_frequent_edges_df
