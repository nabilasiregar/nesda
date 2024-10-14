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
print(tiers)

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

suff.all <- getSuff(data, test = "flexCItest")
str(suff.all)
is.list(suff.all)

graph <- tpc(
  suffStat = suff.all,
  indepTest = flexCItest,
  skel.method = "stable.parallel",
  labels = as.character(colnames(data)),
  alpha = 0.05,
  tiers = tiers,
  forbEdges = forbEdges,
  numCores = detectCores()-1,
  verbose = FALSE
)

save(graph, file = "tpc_graph.RData")