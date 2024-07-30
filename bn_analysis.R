library(bnlearn)
library(igraph)
library(glue)

# Load the data
load("pc_graph.RData")
original_graph <- pc_network

load("bootstrap_bn.RData")
graphs_0 <- results_0

load("bootstrap_bn10.RData")
graphs_10 <- results_10

load("bootstrap_bn20.RData")
graphs_20 <- results_20

all_graphs <- c(graphs_0, graphs_10, graphs_20)

check_edge_presence <- function(graph, edge) {
  arcs_in_graph <- arcs(graph)
  any(arcs_in_graph[,1] == edge[1] & arcs_in_graph[,2] == edge[2])
}

# Calculate the presence of original edges in bootstrap graphs
original_edges <- arcs(original_graph)
edge_presence <- sapply(1:nrow(original_edges), function(i) {
  edge <- original_edges[i, ]
  mean(sapply(all_graphs, check_edge_presence, edge = edge))
})

stable_edges <- original_edges[edge_presence > 0.8, , drop = FALSE]
print("Edges found in more than 80% of bootstrap graphs:")
print(stable_edges)

node_a <- "aHDL_C" 
node_b <- "amet_syn2"

original_igraph <- as.igraph(original_graph)

find_shortest_path <- function(graph, from, to) {
  if (from %in% V(graph)$name && to %in% V(graph)$name) {
    sp_result <- get.shortest.paths(graph, from = from, to = to, output = "vpath")
    if (length(sp_result$vpath[[1]]) > 0) {
      path_names <- V(graph)[sp_result$vpath[[1]]]$name
      return(path_names)
    } else {
      return(NULL)  # Return NULL if no path is found
    }
  } else {
    return(NULL)  # Return NULL if nodes do not exist
  }
}

# 1. Get the shortest path from the original graph
shortest_path_original <- find_shortest_path(original_igraph, node_a, node_b)
if (!is.null(shortest_path_original)) {
  print(glue("Shortest path from {node_a} to {node_b} in the original graph: {paste(shortest_path_original, collapse = ' -> ')}"))
} else {
  print(glue("No path exists between {node_a} and {node_b} in the original graph."))
}

# 2. How many paths found from node A to node B in the original graph
extract_all_paths <- function(graph, from, to) {
  if (from %in% V(graph)$name && to %in% V(graph)$name) {
    paths <- all_simple_paths(graph, from = from, to = to)
    lapply(paths, function(path) V(graph)[path]$name)
  } else {
    return(list())
  }
}

all_paths_original <- extract_all_paths(original_igraph, node_a, node_b)
num_paths_original <- length(all_paths_original)
print(glue("Number of paths from {node_a} to {node_b} in the original graph: {num_paths_original}"))

# 3. How many times the shortest path in the original graph was also found in the bootstrap graphs
shortest_paths <- lapply(all_graphs, function(g) {
  g_igraph <- as.igraph(g)
  find_shortest_path(g_igraph, node_a, node_b)
})
shortest_paths <- Filter(Negate(is.null), shortest_paths)
path_strings <- sapply(shortest_paths, paste, collapse = " -> ")

shortest_path_original_str <- paste(shortest_path_original, collapse = " -> ")
shortest_path_count_in_bootstrap <- sum(path_strings == shortest_path_original_str)
print(glue("Shortest path in the original graph was found {shortest_path_count_in_bootstrap} times in the bootstrap graphs."))

# 4. The most frequent path (based on all paths, not the specified path from A to B) across bootstrap graphs and how many times it was found
all_paths <- lapply(all_graphs, function(g) {
  g_igraph <- as.igraph(g)
  extract_all_paths(g_igraph, node_a, node_b)
})
all_paths <- do.call(c, all_paths)
all_path_strings <- sapply(all_paths, paste, collapse = " -> ")
all_path_table <- table(all_path_strings)

if (length(all_path_table) > 0) {
  most_frequent_path <- names(which.max(all_path_table))
  most_frequent_path_count <- max(all_path_table)
  print(glue("Most frequent path across all paths: {most_frequent_path}"))
  cat(sprintf("Number of times the most frequent path appears: %d\n", most_frequent_path_count))
}

# 5. The 3 most frequent visited nodes across bootstrap graphs and the percentage of it
node_visits <- table(unlist(all_paths))
top_three_nodes <- names(sort(node_visits, decreasing = TRUE)[1:3])
top_three_nodes_percentages <- sort(node_visits, decreasing = TRUE)[1:3] / sum(node_visits) * 100

print("Top three most visited nodes and their percentages:")
print(top_three_nodes)
print(top_three_nodes_percentages)
