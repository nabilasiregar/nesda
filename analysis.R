library(igraph)

convert_to_igraph <- function(graph) {
  adj_matrix <- as(graph@graph, "matrix")
  igraph_obj <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
  return(igraph_obj)
}

compare_graph_edges <- function(g1, g2, description) {
  edges_g1 <- get.edgelist(g1)
  edges_g2 <- get.edgelist(g2)
  
  common_edges <- intersect(paste(edges_g1[, 1], edges_g1[, 2], sep = "-"), paste(edges_g2[, 1], edges_g2[, 2], sep = "-"))
  return(data.frame(
    Comparison = description,
    Total_Edges_G1 = nrow(edges_g1),
    Total_Edges_G2 = nrow(edges_g2),
    Common_Edges = length(common_edges),
    Common_Percentage = length(common_edges) / nrow(edges_g1) * 100
  ))
}

perform_path_analysis <- function(graph, start_node, end_node) {
  # find all simple paths
  paths <- all_simple_paths(graph, from = start_node, to = end_node)
  path_strings <- lapply(paths, function(path) paste(V(graph)[path]$name, collapse = "->"))
  
  # determine the shortest path
  shortest_path <- which.min(sapply(paths, length))
  shortest_path_string <- path_strings[[shortest_path]]
  
  # determine the most frequent path
  path_table <- table(unlist(path_strings))
  most_frequent_path <- names(which.max(path_table))
  
  # count node frequencies across all paths
  node_frequency <- table(unlist(lapply(paths, function(path) V(graph)[path]$name)))
  most_frequent_nodes <- names(sort(node_frequency, decreasing = TRUE)[1:3])
  
  return(list(
    Shortest_Path = shortest_path_string,
    Most_Frequent_Path = most_frequent_path,
    Most_Frequent_Nodes = most_frequent_nodes
  ))
}

load("original_causal_graph.RData")
original_igraph <- convert_to_igraph(original_graph)

load("bootstrap_graph_10.RData")
bootstrap_igraph_10 <- convert_to_igraph(bootstrap_graph_10)

load("bootstrap_graph_20.RData")
bootstrap_igraph_20 <- convert_to_igraph(bootstrap_graph_20)

# Compare edges
edge_comparison_results <- rbind(
  compare_graph_edges(original_igraph, bootstrap_igraph_10, "Original vs. Bootstrap 10%"),
  compare_graph_edges(original_igraph, bootstrap_igraph_20, "Original vs. Bootstrap 20%")
)
write.csv(edge_comparison_results, "edge_comparison_results.csv", row.names = FALSE)

# Perform path analysis
original_analysis <- perform_path_analysis(original_igraph, 'acidep09', 'ecidep09')
bootstrap_10_analysis <- perform_path_analysis(bootstrap_igraph_10, 'acidep09', 'ecidep09')
bootstrap_20_analysis <- perform_path_analysis(bootstrap_igraph_20, 'acidep09', 'ecidep09')

path_analysis_results <- rbind(
  cbind(Analysis = "Original", t(original_analysis)),
  cbind(Analysis = "Bootstrap 10%", t(bootstrap_10_analysis)),
  cbind(Analysis = "Bootstrap 20%", t(bootstrap_20_analysis))
)
write.csv(path_analysis_results, "analysis_results.csv", row.names = FALSE)
