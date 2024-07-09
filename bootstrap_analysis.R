library(tpc)
library(micd)
library(parallel)

data <- read.csv("data/discrete_data.csv")

tier1 <- c('Age', 'Sexe')
tier2 <- c('aedu', 'asmokstat', 'AIPMETO2', 'aauditsc', 'aIRSsum9', 'abaiscal', 'aids', 'acidep09', 'amet_syn2', 'ams_waist', 'ams_hpt', 'ams_trig2', 'ams_hdl2', 'ams_gluc2', 'atri_med', 'ahdl_med', 'asbp_med', 'adbp_med', 'agluc_med', 'ahsCRP', 'aIL6', 'aApoB', 'aHDL_C', 'aTotFA', 'aSerum_TG', 'aGp', 'aIle')
tier3 <- c('eage', 'sex')
tier4 <- c('eipmeto2', 'eauditsc', 'eIRSsum9', 'ebaiscal', 'eids', 'ecidep09', 'emet_syn2', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'etri_med', 'ehdl_med', 'esbp_med', 'edbp_med', 'egluc_med', 'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eSerumTG', 'eGp', 'eIle')

tiered_variables <- c(tier1, tier2, tier3, tier4)
data <- data[, tiered_variables]

# Create the tiers vector based on the reordered data
tiers <- c(rep(1, length(tier1)), rep(2, length(tier2)), rep(3, length(tier3)), rep(4, length(tier4)))

# Blacklist
bl <- c("sex", "Age", "eage", "Sexe")

# Whitelist
wl_tier1 <- rbind(
  data.frame(from = "Age", to = c("aSerum_TG", "aHDL_C", "aApoB")),
  data.frame(from = "Sexe", to = c("aSerum_TG", "aHDL_C", "aApoB")),
  data.frame(from = "Sexe", to = c("asmokstat", "acidep09"))
)

wl_tier2 <- rbind(
  data.frame(from = "asmokstat", to = c("aSerum_TG", "aHDL_C", "aApoB")),
  data.frame(from = c("aSerum_TG", "aHDL_C", "aApoB"), to = c("atri_med")),
  data.frame(from = c("aSerum_TG", "aHDL_C", "aApoB"), to = c("ahdl_med")),
  data.frame(from = "ams_gluc2", to = c("agluc_med")),
  data.frame(from = "ams_hpt", to = c("asbp_med", "adbp_med")),
  data.frame(from = "acidep09", to = c("aedu")),
  data.frame(from = "aauditsc", to = c("asmokstat")),
  data.frame(from = c("aSerum_TG", "aHDL_C", "aApoB"), to = c("eSerumTG", "eHDLC", "eApoB")),
  data.frame(from = c("aIL6", "ahsCRP"), to = c("eIL6", "eHSCRP")),
  data.frame(from = c("amet_syn2"), to = c("emet_syn2")),
  data.frame(from = c("ams_waist"), to = c("ems_waist")),
  data.frame(from = c("ams_hpt"), to = c("ems_hpt")),
  data.frame(from = c("ams_gluc2"), to = c("ems_gluc2")),
  data.frame(from = c("atri_med", "ahdl_med", "asbp_med", "adbp_med", "agluc_med"), to = c("etri_med", "ehdl_med", "esbp_med", "edbp_med", "egluc_med")),
  data.frame(from = "acidep09", to = c("ecidep09")),
  data.frame(from = "aauditsc", to = c("eauditsc")),
  data.frame(from = "asmokstat", to = c("eSerumTG", "eHDLC", "eApoB")),
  data.frame(from = c("ahsCRP"), to = c("eIL6")),
  data.frame(from = "acidep09", to = c("emet_syn2")),
  data.frame(from = "aedu", to = c("eauditsc"))
)

wl <- rbind(wl_tier1, wl_tier2)
context_tier1 <- unique(wl_tier1$from)
context_tier2 <- unique(wl_tier2$from)

# Create context.tier vector
context_tier <- rep(NA, ncol(data))
context_tier[1:length(tier1)] <- ifelse(tier1 %in% context_tier1, tier1, NA)
context_tier[(length(tier1) + 1):(length(tier1) + length(tier2))] <- ifelse(tier2 %in% context_tier2, tier2, NA)

# Ensure context_tier only includes valid variable names
context_tier <- context_tier[context_tier %in% colnames(data)]
context_tier <- as.character(context_tier)

# Prepare forbidden edges
forbEdges <- matrix(FALSE, ncol = length(colnames(data)), nrow = length(colnames(data)), 
                    dimnames = list(colnames(data), colnames(data)))

# Populate the forbidden edges matrix
for (var in bl) {
  if (var %in% colnames(data)) {
    forbEdges[, var] <- TRUE 
  }
}

run_causal_discovery <- function(data) {
  suff.all <- getSuff(data, test = "flexCItest")
  graph <- tpc(
    suffStat = suff.all,
    indepTest = flexCItest,
    skel.method = "stable.parallel",
    labels = as.character(colnames(data)),
    alpha = 0.05,
    tiers = tiers,
    forbEdges = forbEdges,
    numCores = detectCores() - 1,
    verbose = FALSE
  )
  return(graph)
}

# Run the original causal discovery for comparison
original_graph <- run_causal_discovery(data)
save(original_graph, file = "original_causal_graph.RData")

# Validate
bootstrap_analysis <- function(data, drop_percent) {
  sample_size <- nrow(data)
  drop_count <- round(sample_size * drop_percent / 100)
  
  set.seed(123)
  reduced_data <- data[-sample(1:sample_size, drop_count), ]
  
  bootstrap_graph <- run_causal_discovery(reduced_data)
  
  return(bootstrap_graph)
}

# Perform the bootstrap analysis for 10% and 20% drops
bootstrap_graph_10 <- bootstrap_analysis(data, 10)
save(bootstrap_graph_10, file = "bootstrap_graph_10.RData")

bootstrap_graph_20 <- bootstrap_analysis(data, 20)
save(bootstrap_graph_20, file = "bootstrap_graph_20.RData")


