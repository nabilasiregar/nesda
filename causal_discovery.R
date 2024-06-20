library(bnlearn)  # For structural learning in Bayesian networks

data <- read.csv('data/bnlearn.csv')

for(col in names(data)) {
  data[[col]] <- as.factor(data[[col]])
}

tiers <- list(
  'Context B' = c('Age', 'Sexe'),
  'B' = c('aedu', 'asmokstat', 'AIPMETO2', 'aauditsc', 'aIRSsum9', 'abaiscal', 'aids', 'acidep09', 'amet_syn2', 'ams_waist', 'ams_hpt', 'ams_trig2', 'ams_hdl2', 'ams_gluc2', 'atri_med', 'ahdl_med', 'asbp_med', 'adbp_med', 'agluc_med', 'ahsCRP', 'aIL6', 'aApoB', 'aHDL_C', 'aTotFA', 'aSerum_TG', 'aGp', 'aIle'),
  'Context FU' = c('eage', 'sex'),
  'FU' = c('eipmeto2', 'eauditsc', 'eIRSsum9', 'ebaiscal', 'eids', 'ecidep09', 'emet_syn2', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'etri_med', 'ehdl_med', 'esbp_med', 'edbp_med', 'egluc_med', 'eApoB', 'eHDLC', 'eTotFA', 'eSerumTG', 'eGp', 'eIle', 'eHSCRP', 'eIL6')
)

tier_levels <- unlist(lapply(seq_along(tiers), function(i) setNames(rep(i, length(tiers[[i]])), tiers[[i]])))

# Whitelist and blacklist
whitelist <- data.frame(
  from = c(
    "Age", "Age", "Age",
    "Sexe", "Sexe", "Sexe", "Sexe", "Sexe",
    "asmokstat", "asmokstat", "asmokstat",
    "aSerum_TG", "aHDL_C", "aApoB",
    "ams_waist",
    "ahsCRP",
    "aSerum_TG", "aHDL_C", "aApoB",
    "ams_gluc2",
    "ams_hpt", "ams_hpt",
    "acidep09",
    "aauditsc",
    "aSerum_TG", "aHDL_C", "aApoB", "aSerum_TG", "aHDL_C", "aApoB", "aIL6", "ahsCRP", "amet_syn2", "ams_waist", "ams_hpt", "ams_gluc2",
    "atri_med", "ahdl_med", "asbp_med", "adbp_med", "agluc_med",
    "acidep09",
    "aauditsc",
    "asmokstat", "asmokstat", "asmokstat",
    "ahsCRP",
    "acidep09",
    "aedu"
  ),
  to = c(
    "aSerum_TG", "aHDL_C", "aApoB",
    "aSerum_TG", "aHDL_C", "aApoB", "asmokstat", "acidep09",
    "aSerum_TG", "aHDL_C", "aApoB",
    "amet_syn2", "amet_syn2", "amet_syn2",
    "amet_syn2",
    "aIL6",
    "atri_med", "ahdl_med", "atri_med",
    "agluc_med",
    "asbp_med", "adbp_med",
    "aedu",
    "asmokstat",
    "eSerumTG", "eHDLC", "eApoB", "eSerumTG", "eHDLC", "eApoB", "eIL6", "eHSCRP", "emet_syn2", "ems_waist", "ems_hpt", "ems_gluc2",
    "etri_med", "ehdl_med", "esbp_med", "edbp_med", "egluc_med",
    "ecidep09",
    "eauditsc",
    "eSerumTG", "eHDLC", "eApoB",
    "eIL6",
    "emet_syn2",
    "eauditsc"
  )
)

# Ensure it's converted to a matrix for bnlearn usage
whitelist_matrix <- as.matrix(whitelist)

tier_levels <- c(
  'Age' = 1, 'Sexe' = 1,
  'aedu' = 2, 'asmokstat' = 2, 'AIPMETO2' = 2, 'aauditsc' = 2, 'aIRSsum9' = 2, 'abaiscal' = 2, 'aids' = 2, 'acidep09' = 2, 
  'ams_waist' = 2, 'ams_hpt' = 2, 'ams_trig2' = 2, 'ams_hdl2' = 2, 'ams_gluc2' = 2, 'atri_med' = 2, 'ahdl_med' = 2, 
  'asbp_med' = 2, 'adbp_med' = 2, 'agluc_med' = 2, 'ahsCRP' = 2, 'aIL6' = 2, 'aApoB' = 2, 'aHDL_C' = 2, 'aTotFA' = 2, 
  'aSerum_TG' = 2, 'aGp' = 2, 'aIle' = 2, 'amet_syn2' = 2,
  'eage' = 3, 'sex' = 3,
  'eipmeto2' = 4, 'eauditsc' = 4, 'eIRSsum9' = 4, 'ebaiscal' = 4, 'eids' = 4, 'ecidep09' = 4, 'ems_waist' = 4, 'ems_hpt' = 4, 
  'ems_trig2' = 4, 'ems_hdl2' = 4, 'ems_gluc2' = 4, 'etri_med' = 4, 'ehdl_med' = 4, 'esbp_med' = 4, 'edbp_med' = 4, 'egluc_med' = 4, 
  'eApoB' = 4, 'eHDLC' = 4, 'eTotFA' = 4, 'eSerumTG' = 4, 'eGp' = 4, 'eIle' = 4, 'eHSCRP' = 4, 'eIL6' = 4, 'emet_syn2' = 4
)

blacklist <- list()

# Populate the blacklist enforcing tier constraints
for (var in names(tier_levels)) {
  for (ivar in names(tier_levels)) {
    if (tier_levels[var] < tier_levels[ivar]) {  # Allow only edges that go from lower to higher tier
      potential_pair <- c(var, ivar)
      if (length(potential_pair) == 2) {  # Check if it is indeed a pair
        blacklist[[length(blacklist) + 1]] <- potential_pair
      } else {
        cat("Invalid pair detected:", potential_pair, "\n")  # Log the invalid pair
      }
    }
  }
}

valid_pairs <- sapply(blacklist, function(x) length(x) == 2)

# Identify and print any invalid pairs
if (any(!valid_pairs)) {
  cat("Invalid pairs:\n")
  print(blacklist[!valid_pairs])
  stop("Not all elements in the blacklist are pairs.")
} else {
  blacklist_matrix <- do.call(rbind, blacklist)
  print("All pairs are valid, and blacklist_matrix has been created successfully.")
}

valid_whitelist <- all(whitelist_matrix %in% colnames(data))
print(paste("Whitelist valid:", valid_whitelist))

pc_result <- pc.stable(data, blacklist = blacklist_matrix, whitelist = whitelist_matrix, test = "mi")

