library(tpc)
library(micd)
library(parallel)

data <- read.csv("data/discrete_mice.csv")

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

# Prepare forbidden edges
forbEdges <- matrix(FALSE, ncol = length(colnames(data)), nrow = length(colnames(data)), 
                    dimnames = list(colnames(data), colnames(data)))

# Populate the forbidden edges matrix
for (var in bl) {
  if (var %in% colnames(data)) {
    forbEdges[, var] <- TRUE 
  }
}

bootstrap_analysis <- function(data, drop_rate) {
  suff_stat_list <- list()
  for (i in 1:10) {
    print(paste("Starting bootstrap iteration:", i))
    boot_data <- data[sample(1:nrow(data), replace = TRUE), ]
    final_data <- boot_data[sample(nrow(boot_data), nrow(boot_data) * (1 - drop_rate)), ]
    
    suff_stat <- getSuff(final_data, test = "flexCItest")
    graph <- tpc(
      suffStat = suff_stat,
      indepTest = flexCItest,
      skel.method = "stable.parallel",
      labels = as.character(colnames(final_data)),
      alpha = 0.05,
      tiers = tiers,
      forbEdges = forbEdges,
      numCores = detectCores() - 1,
      verbose = FALSE
    )
    suff_stat_list[[i]] <- graph
  }
  return(suff_stat_list)
}

set.seed(123) 

results_0 <- bootstrap_analysis(data, 0.0)
save(results_0, file = "bootstrap_tpc.RData")

results_10 <- bootstrap_analysis(data, 0.1)
save(results_10, file = "bootstrap_tpc_10.RData")

results_20 <- bootstrap_analysis(data, 0.2)
save(results_20, file = "bootstrap_tpc_20.RData")