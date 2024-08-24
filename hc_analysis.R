library(bnlearn)
library(igraph)
library(glue)

load("hc_graph_mice.RData")
original_graph <- hc_network
print(hc_network)

load("bootstrap_hc.RData")
graphs_0 <- results_0

load("bootstrap_hc10.RData")
graphs_10 <- results_10

load("bootstrap_hc20.RData")
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

stable_edges <- original_edges[edge_presence > 0.99, , drop = FALSE]
print("Edges found in more than 80% of bootstrap graphs:")
print(stable_edges)

node_a <- "aHDL_C" 
node_b <- "emet_syn2"

original_igraph <- as.igraph(original_graph)

find_shortest_path <- function(graph, from, to) {
  if (from %in% V(graph)$name && to %in% V(graph)$name) {
    sp_result <- get.shortest.paths(graph, from = from, to = to, output = "vpath")
    if (length(sp_result$vpath[[1]]) > 0) {
      all_paths <- lapply(sp_result$vpath, function(path) V(graph)[path]$name)
      return(all_paths)
    } else {
      # no path found
      return(NULL)
    }
  } else {
    # nodes do not exist
    return(NULL)
  }
}

# 1. Get the shortest path from the original graph
shortest_paths_original <- find_shortest_path(original_igraph, node_a, node_b)
if (!is.null(shortest_paths_original) && length(shortest_paths_original) > 0) {
  for (i in seq_along(shortest_paths_original)) {
    path_str <- paste(shortest_paths_original[[i]], collapse = " -> ")
    print(glue("Shortest path {i} from {node_a} to {node_b} in the original graph: {path_str}"))
  }
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
if (!is.null(shortest_paths_original) && length(shortest_paths_original) > 0) {
  shortest_path_counts <- numeric(length(shortest_paths_original))
  
  # Iterate through each shortest path in the original graph
  for (i in seq_along(shortest_paths_original)) {
    shortest_path_original_str <- paste(shortest_paths_original[[i]], collapse = " -> ")
    
    # Check how many times this specific shortest path appears in the bootstrap graphs
    shortest_paths <- lapply(all_graphs, function(g) {
      g_igraph <- as.igraph(g)
      find_shortest_path(g_igraph, node_a, node_b)
    })
    
    # Filter out NULL results
    shortest_paths <- Filter(Negate(is.null), shortest_paths)
    
    if (length(shortest_paths) > 0) {
      shortest_path_strings <- unlist(lapply(shortest_paths, function(paths) {
        sapply(paths, function(path) paste(path, collapse = " -> "))
      }))
      shortest_path_count_in_bootstrap <- sum(shortest_path_strings == shortest_path_original_str)
      shortest_path_counts[i] <- shortest_path_count_in_bootstrap
    } else {
      shortest_path_counts[i] <- 0
    }
  }
  for (i in seq_along(shortest_paths_original)) {
    path_str <- paste(shortest_paths_original[[i]], collapse = " -> ")
    cat(glue("Number of times the shortest path {i} ({path_str}) in the original graph was found in bootstrap graphs: {shortest_path_counts[i]}\n"))
  }
} else {
  print(glue("No path exists between {node_a} and {node_b} in the original graph. Skipping bootstrap analysis for shortest path."))
}

# 4. Most frequent path from A to B across bootstrap graphs and how many times it was found
all_paths <- lapply(all_graphs, function(g) {
  g_igraph <- as.igraph(g)
  extract_all_paths(g_igraph, node_a, node_b)
})
all_paths <- do.call(c, all_paths)
if (length(all_paths) == 0) {
  print(glue("No paths found from {node_a} to {node_b} across all bootstrap graphs."))
} else {
  all_path_strings <- sapply(all_paths, paste, collapse = " -> ")
  path_table <- table(all_path_strings)
  max_frequency <- max(path_table)
  most_frequent_paths <- names(path_table[path_table == max_frequency])
  
  print("Most frequent path(s) across all paths:")
  print(most_frequent_paths)
  cat(sprintf("Number of times the most frequent path(s) appear(s): %d\n", max_frequency))
}

# 5. The 3 most frequent visited intermediate nodes across bootstrap graphs and the percentage of it
all_paths <- lapply(all_graphs, function(g) {
  g_igraph <- as.igraph(g)
  extract_all_paths(g_igraph, node_a, node_b)
})
all_paths <- do.call(c, all_paths)
all_nodes <- unlist(all_paths)
intermediate_nodes <- all_nodes[!(all_nodes %in% c(node_a, node_b))]
node_visits <- table(intermediate_nodes)
top_three_nodes <- names(sort(node_visits, decreasing = TRUE)[1:3])
top_three_nodes_percentages <- sort(node_visits, decreasing = TRUE)[1:3] / sum(node_visits) * 100
print(node_visits)
print("Top three most visited intermediate nodes and their percentages:")
print(top_three_nodes)
print(top_three_nodes_percentages)

# 6. Longest path from A to B across bootstrap graphs and how many times it was found
all_paths <- lapply(all_graphs, function(g) {
  g_igraph <- as.igraph(g)
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
