library(bnlearn)
library(igraph)
library(glue)
library(graph)

load("tpc_graph_mice.RData")
original_graph <- graph
print(graph)

load("bootstrap_tpc.RData")
graphs_0 <- results_0

load("bootstrap_tpc_10.RData")
graphs_10 <- results_10

load("bootstrap_tpc_20.RData")
graphs_20 <- results_20

all_graphs <- c(graphs_0, graphs_10, graphs_20)

convert_to_igraph <- function(graph) {
  if (inherits(graph, "pcAlgo")) {
    graph <- graph@graph
  }
  igraph_graph <- igraph::igraph.from.graphNEL(graph)
  return(igraph_graph)
}

check_edge_presence <- function(graph, edge) {
  edges_in_graph <- as_edgelist(graph)
  any(edges_in_graph[, 1] == edge[1] & edges_in_graph[, 2] == edge[2])
}

original_igraph <- convert_to_igraph(original_graph)

original_edges <- as_edgelist(original_igraph)
edge_presence <- sapply(1:nrow(original_edges), function(i) {
  edge <- original_edges[i, ]
  mean(sapply(all_graphs, function(g) {
    g_igraph <- convert_to_igraph(g)
    check_edge_presence(g_igraph, edge)
  }))
})

stable_edges <- original_edges[edge_presence > 0.8, , drop = FALSE]
print("Edges found in more than 80% of bootstrap graphs:")
print(stable_edges)

node_a <- "acidep09" 
node_b <- "egluc_med"

find_shortest_path <- function(graph, from, to) {
  if (from %in% V(graph)$name && to %in% V(graph)$name) {
    sp_result <- get.shortest.paths(graph, from = from, to = to, output = "vpath")
    if (length(sp_result$vpath[[1]]) > 0) {
      all_paths <- lapply(sp_result$vpath, function(path) V(graph)[path]$name)
      return(all_paths)
    } else {
      return(NULL)  # No path found
    }
  } else {
    return(NULL)  # Nodes do not exist
  }
}

# 1. Get the shortest path(s) from the original graph
shortest_paths_original <- find_shortest_path(original_igraph, node_a, node_b)
if (!is.null(shortest_paths_original) && length(shortest_paths_original) > 0) {
  for (i in seq_along(shortest_paths_original)) {
    path_str <- paste(shortest_paths_original[[i]], collapse = " -> ")
    print(glue("Shortest path {i} from {node_a} to {node_b} in the original graph: {path_str}"))
  }
} else {
  print(glue("No path exists between {node_a} and {node_b} in the original graph."))
}

# 2. How many paths are found from node A to node B in the original graph
extract_all_paths <- function(graph, from, to) {
  if (from %in% V(graph)$name && to %in% V(graph)$name) {
    paths <- all_simple_paths(graph, from = from, to = to)
    all_paths <- lapply(paths, function(path) V(graph)[path]$name)
    path_strings <- sapply(all_paths, function(path) paste(path, collapse = " -> "))
    unique_path_strings <- unique(path_strings)
    unique_paths_list <- lapply(unique_path_strings, function(p) strsplit(p, " -> ")[[1]])
    
    return(unique_paths_list)
  } else {
    return(list())
  }
}

all_paths_original <- extract_all_paths(original_igraph, node_a, node_b)
num_paths_original <- length(all_paths_original)
print(glue("Number of unique paths from {node_a} to {node_b} in the original graph: {num_paths_original}"))

# 3. How many times the shortest path in the original graph was also found in the bootstrap graphs
if (!is.null(shortest_paths_original) && length(shortest_paths_original) > 0) {
  shortest_path_counts <- numeric(length(shortest_paths_original))
  
  # Convert the shortest path from the original graph to string format
  shortest_path_original_str <- paste(shortest_paths_original[[1]], collapse = " -> ")
  
  # Iterate over all bootstrap graphs to count how many times this shortest path appears
  all_paths <- lapply(all_graphs, function(g) {
    g_igraph <- convert_to_igraph(g)
    extract_all_paths(g_igraph, node_a, node_b) 
  })
  
  # Combine all paths from all bootstrap graphs into a single list
  all_paths <- do.call(c, all_paths)
  
  # Convert all bootstrap paths to string format
  all_path_strings <- sapply(all_paths, function(path) paste(path, collapse = " -> "))
  
  # Count how many times the shortest path from the original graph appears in the bootstrap graphs
  shortest_path_count_in_bootstrap <- sum(all_path_strings == shortest_path_original_str)
  
  cat(glue("The shortest path from the original graph ({shortest_path_original_str}) appears {shortest_path_count_in_bootstrap} times in the bootstrap graphs.\n"))
  
} else {
  print(glue("No path exists between {node_a} and {node_b} in the original graph. Skipping bootstrap analysis for shortest path."))
}

# 4. Most frequent path from A to B across bootstrap graphs and how many times it was found
all_paths <- lapply(all_graphs, function(g) {
  g_igraph <- convert_to_igraph(g)
  extract_all_paths(g_igraph, node_a, node_b)
})

all_paths <- do.call(c, all_paths)

if (length(all_paths) == 0) {
  print(glue("No paths found from {node_a} to {node_b} across all bootstrap graphs."))
} else {
  # Convert each path to a string representation
  all_path_strings <- sapply(all_paths, function(path) paste(path, collapse = " -> "))
  
  # Create a frequency table for the path strings
  path_table <- table(all_path_strings)
  
  # Get the maximum frequency
  max_frequency <- max(path_table)
  
  # Get the most frequent path(s) based on the maximum frequency
  most_frequent_paths <- names(path_table[path_table == max_frequency])
  
  print("Most frequent path(s) across all paths:")
  print(most_frequent_paths)
  cat(sprintf("Number of times the most frequent path(s) appear(s): %d\n", max_frequency))
}

# 5. The 3 most frequent visited intermediate nodes across bootstrap graphs and their percentages
all_paths <- lapply(all_graphs, function(g) {
  g_igraph <- convert_to_igraph(g)
  extract_all_paths(g_igraph, node_a, node_b)
})
all_paths <- do.call(c, all_paths)
all_nodes <- unlist(all_paths)
intermediate_nodes <- all_nodes[!(all_nodes %in% c(node_a, node_b))]
node_visits <- table(intermediate_nodes)
top_three_nodes <- names(sort(node_visits, decreasing = TRUE)[1:3])
top_three_nodes_percentages <- sort(node_visits, decreasing = TRUE)[1:3] / sum(node_visits) * 100
print("Top three most visited intermediate nodes and their percentages:")
print(top_three_nodes)
print(top_three_nodes_percentages)

# 6. Longest path from A to B across bootstrap graphs and how many times it was found
all_paths <- lapply(all_graphs, function(g) {
  g_igraph <- convert_to_igraph(g)
  extract_all_paths(g_igraph, node_a, node_b)
})
all_paths <- do.call(c, all_paths)
if (length(all_paths) == 0) {
  print(glue("No paths found from {node_a} to {node_b} across all bootstrap graphs."))
} else {
  path_lengths <- sapply(all_paths, length)
  max_length <- max(path_lengths)
  longest_paths <- all_paths[path_lengths == max_length]
  longest_path_strings <- sapply(longest_paths, paste, collapse = " -> ")
  longest_path_table <- table(longest_path_strings)
  most_frequent_longest_path <- names(which.max(longest_path_table))
  most_frequent_longest_path_count <- max(longest_path_table)
  print(glue("Longest path across all paths: {most_frequent_longest_path}"))
  cat(sprintf("Number of times the longest path appears: %d\n", most_frequent_longest_path_count))
}

