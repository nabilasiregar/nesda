library(tpc)
library(micd)
library(parallel)

data <- read.csv("data/discrete_data.csv")

tier1 <- c('Age', 'Sexe', 'aedu', 'asmokstat', 'AIPMETO2', 'aauditsc', 'aIRSsum9', 'abaiscal', 'aids', 'acidep09', 'amet_syn2', 'ams_waist', 'ams_hpt', 'ams_trig2', 'ams_hdl2', 'ams_gluc2', 'atri_med', 'ahdl_med', 'asbp_med', 'adbp_med', 'agluc_med', 'ahsCRP', 'aIL6', 'aApoB', 'aHDL_C', 'aTotFA', 'aSerum_TG', 'aGp', 'aIle')
tier2 <- c('eage', 'sex', 'eipmeto2', 'eauditsc', 'eIRSsum9', 'ebaiscal', 'eids', 'ecidep09', 'emet_syn2', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'etri_med', 'ehdl_med', 'esbp_med', 'edbp_med', 'egluc_med', 'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eSerumTG', 'eGp', 'eIle')

data <- data[, c(tier1, tier2)]

tiers <- c(rep(1, length(tier1)), rep(2, length(tier2)))
print(tiers)

# Blacklist
forbEdges <- matrix(FALSE, ncol = length(colnames(data)), nrow = length(colnames(data)), dimnames = list(colnames(data), colnames(data)))
forbEdges[ , tier1] <- FALSE
blacklist <- c("Age", "Sexe", "eage", "sex")
for (var in blacklist) {
  forbEdges[, which(colnames(data) == var)] <- TRUE
}

print(forbEdges)

# Whitelist of known connections
wl <- rbind(
  data.frame(from = "Age", to = c("aSerum_TG", "aHDL_C", "aApoB")),
  data.frame(from = "Sexe", to = c("aSerum_TG", "aHDL_C", "aApoB")),
  data.frame(from = "Sexe", to = c("asmokstat", "acidep09")),
  data.frame(from = "asmokstat", to = c("aSerum_TG", "aHDL_C", "aApoB")),
  expand.grid(from = c("aSerum_TG", "aHDL_C", "aApoB"), to = c("atri_med")),
  expand.grid(from = c("aSerum_TG", "aHDL_C", "aApoB"), to = c("ahdl_med")),
  data.frame(from = "ams_gluc2", to = c("agluc_med")),
  expand.grid(from = "ams_hpt", to = c("asbp_med", "adbp_med")),
  data.frame(from = "acidep09", to = c("aedu")),
  data.frame(from = "aauditsc", to = c("asmokstat")),
  expand.grid(from = "asmokstat", to = c("eSerumTG", "eHDLC", "eApoB")),
  expand.grid(from = c("ahsCRP"), to = c("eIL6")),
  data.frame(from = "acidep09", to = c("emet_syn2")),
  data.frame(from = "aedu", to = c("eauditsc"))
)

context_tier <- setdiff(unique(wl$from), unique(wl$to))
print(context_tier)

suff.all <- getSuff(data, test = "flexCItest")
str(suff.all)

graph <- tpc(
  suffStat = suff.all,
  indepTest = flexCItest,
  skel.method = "stable.parallel",
  labels = as.character(colnames(data)),
  alpha = 0.05,
  tiers = tiers,
  forbEdges = forbEdges,
  numCores = detectCores() - 1,
  verbose = FALSE,
  context.tier = c("Age", "Sexe", "eage", "sex")
)

save(graph, file = "2tiers_causal_graph_with_wl_only_age_gender.RData")
