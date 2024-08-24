library(bnlearn)

data <- read.csv("data/discrete_mice.csv")

discrete_vars <- c('Age', 'Sexe', 'aedu', 'asmokstat', 'AIPMETO2', 'aauditsc', 'aIRSsum9', 'abaiscal', 'aids', 'acidep09', 'amet_syn2', 'ams_waist', 'ams_hpt', 'ams_trig2', 'ams_hdl2', 'ams_gluc2', 'atri_med', 'ahdl_med', 'asbp_med', 'adbp_med', 'agluc_med', 'ahsCRP', 'aIL6', 'aApoB', 'aHDL_C', 'aTotFA', 'aSerum_TG', 'aGp', 'aIle', 'eage', 'sex', 'eipmeto2', 'eauditsc', 'eIRSsum9', 'ebaiscal', 'eids', 'ecidep09', 'emet_syn2', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'etri_med', 'ehdl_med', 'esbp_med', 'edbp_med', 'egluc_med', 'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eSerumTG', 'eGp', 'eIle')
data[discrete_vars] <- lapply(data[discrete_vars], factor)

tiers <- list(
  tier1 = c('Age', 'Sexe'),
  tier2 = c('aedu', 'asmokstat', 'AIPMETO2', 'aauditsc', 'aIRSsum9', 'abaiscal', 'aids', 'acidep09', 'amet_syn2', 'ams_waist', 'ams_hpt', 'ams_trig2', 'ams_hdl2', 'ams_gluc2', 'atri_med', 'ahdl_med', 'asbp_med', 'adbp_med', 'agluc_med', 'ahsCRP', 'aIL6', 'aApoB', 'aHDL_C', 'aTotFA', 'aSerum_TG', 'aGp', 'aIle'),
  tier3 = c('eage', 'sex'),
  tier4 = c('eipmeto2', 'eauditsc', 'eIRSsum9', 'ebaiscal', 'eids', 'ecidep09', 'emet_syn2', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'etri_med', 'ehdl_med', 'esbp_med', 'edbp_med', 'egluc_med', 'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eSerumTG', 'eGp', 'eIle')
)

blacklist <- rbind(
  expand.grid(from = setdiff(discrete_vars, c("Age", "Sexe", "eage", "sex")), to = c("Age", "Sexe", "eage", "sex")),
  tiers2blacklist(tiers),
  data.frame(from = c("Age", "Sexe"), to = c("eage", "sex"))
)
print(blacklist)

whitelist <- rbind(
  data.frame(from = "Age", to = c("aSerum_TG", "aHDL_C", "aApoB")),
  data.frame(from = "Sexe", to = c("aSerum_TG", "aHDL_C", "aApoB")),
  data.frame(from = "Sexe", to = c("asmokstat", "acidep09")),
  data.frame(from = "asmokstat", to = c("aSerum_TG", "aHDL_C", "aApoB")),
  expand.grid(from = c("aSerum_TG", "aHDL_C", "aApoB"), to = c("amet_syn2")),
  data.frame(from = "ams_waist", to = "amet_syn2"),
  data.frame(from = "ahsCRP", to = "aIL6"),
  expand.grid(from = c("aSerum_TG", "aHDL_C", "aApoB"), to = c("atri_med", "ahdl_med")),
  data.frame(from = "ams_gluc2", to = "agluc_med"),
  expand.grid(from = "ams_hpt", to = c("asbp_med", "adbp_med")),
  data.frame(from = "acidep09", to = "aedu"),
  data.frame(from = "aauditsc", to = "asmokstat"),
  expand.grid(from = c("aSerum_TG", "aHDL_C", "aApoB"), to = c("eSerumTG", "eHDLC", "eApoB")),
  expand.grid(from = c("aIL6", "ahsCRP"), to = c("eIL6", "eHSCRP")),
  data.frame(from = "amet_syn2", to = "emet_syn2"),
  data.frame(from = "ams_waist", to = "ems_waist"),
  data.frame(from = "ams_hpt", to = "ems_hpt"),
  data.frame(from = "ams_gluc2", to = "ems_gluc2"),
  expand.grid(from = c("atri_med", "ahdl_med", "asbp_med", "adbp_med", "agluc_med"), to = c("etri_med", "ehdl_med", "esbp_med", "edbp_med", "egluc_med")),
  data.frame(from = "acidep09", to = "ecidep09"),
  data.frame(from = "aauditsc", to = "eauditsc"),
  expand.grid(from = "asmokstat", to = c("eSerumTG", "eHDLC", "eApoB")),
  expand.grid(from = "ahsCRP", to = "eIL6"),
  data.frame(from = "acidep09", to = "emet_syn2"),
  data.frame(from = "aedu", to = "eauditsc")
)
hc_network <- hc(data, whitelist = whitelist, blacklist = blacklist, score = "bde", max.iter=100, optimized=FALSE)
#pc_network <- pc.stable(data, blacklist = blacklist, whitelist = whitelist, test = "mi", alpha = 0.05)

print(hc_network)
save(hc_network, file = "hc_graph_mice.RData")
