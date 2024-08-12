library(bnlearn)

load("hc_graph_mice.RData") 
load("hc_graph_mice_without_wl_bl.RData")

print(hc_network)
print(hc_network_without_wlbl)

# Extract edges
edges_with_wl_bl <- arcs(hc_network)
edges_without_wl_bl <- arcs(hc_network_without_wlbl)

# Count the number of edges
num_edges_with_wl_bl <- nrow(edges_with_wl_bl)
num_edges_without_wl_bl <- nrow(edges_without_wl_bl)

cat("Number of edges with WL and BL:", num_edges_with_wl_bl, "\n")
cat("Number of edges without WL and BL:", num_edges_without_wl_bl, "\n")

# See the connection differences
added_edges <- setdiff(as.data.frame(edges_with_wl_bl), as.data.frame(edges_without_wl_bl))
removed_edges <- setdiff(as.data.frame(edges_without_wl_bl), as.data.frame(edges_with_wl_bl))

cat("Added edges:\n")
print(added_edges)
cat("Removed edges:\n")
print(removed_edges)

data <- read.csv("data/discrete_mice.csv")
data[] <- lapply(data, as.factor)

log_likelihood_with_wl_bl <- score(hc_network, data, type = "loglik")
log_likelihood_without_wl_bl <- score(hc_network_without_wlbl, data, type = "loglik")

bic_with_wl_bl <- score(hc_network, data, type = "bic")
bic_without_wl_bl <- score(hc_network_without_wlbl, data, type = "bic")

aic_with_wl_bl <- score(hc_network, data, type = "aic")
aic_without_wl_bl <- score(hc_network_without_wlbl, data, type = "aic")

cat("Log-likelihood with WL and BL:", log_likelihood_with_wl_bl, "\n")
cat("Log-likelihood without WL and BL:", log_likelihood_without_wl_bl, "\n")

cat("BIC with WL and BL:", bic_with_wl_bl, "\n")
cat("BIC without WL and BL:", bic_without_wl_bl, "\n")

cat("AIC with WL and BL:", aic_with_wl_bl, "\n")
cat("AIC without WL and BL:", aic_without_wl_bl, "\n")


