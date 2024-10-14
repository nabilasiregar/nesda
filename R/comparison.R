library(bnlearn)

load("hc_graph.RData") 
load("hc_graph_without_knowledge.RData")

data <- read.csv("data/updated_discrete_data.csv")
vars <- c('Age', 'Sexe', 'aedu','acidep09', 'esmokstat', 'eauditsc', 'eipmeto2', 'eIRSsum9', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eVLDLTG', 'eGp', 'eIle', 'eGlc', 'eTyr')
data <- data[, vars]
data[vars] <- lapply(data[vars], as.factor)

print(hc_network)
print(hc_network_without_knowledge)

# Extract edges
edges_with_wl_bl <- arcs(hc_network)
edges_without_wl_bl <- arcs(hc_network_without_knowledge)

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

log_likelihood_with_wl_bl <- score(hc_network, data, type = "loglik")
log_likelihood_without_wl_bl <- score(hc_network_without_knowledge, data, type = "loglik")

bic_with_wl_bl <- score(hc_network, data, type = "bic")
bic_without_wl_bl <- score(hc_network_without_knowledge, data, type = "bic")

aic_with_wl_bl <- score(hc_network, data, type = "aic")
aic_without_wl_bl <- score(hc_network_without_knowledge, data, type = "aic")

cat("Log-likelihood with WL and BL:", log_likelihood_with_wl_bl, "\n")
cat("Log-likelihood without WL and BL:", log_likelihood_without_wl_bl, "\n")

cat("BIC with WL and BL:", bic_with_wl_bl, "\n")
cat("BIC without WL and BL:", bic_without_wl_bl, "\n")

cat("AIC with WL and BL:", aic_with_wl_bl, "\n")
cat("AIC without WL and BL:", aic_without_wl_bl, "\n")
