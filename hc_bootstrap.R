library(bnlearn)

data <- read.csv("data/updated_discrete_data.csv")
vars <- c('Age', 'Sexe', 'aedu','acidep09', 'esmokstat', 'eauditsc', 'eipmeto2', 'eIRSsum9', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eVLDLTG', 'eGp', 'eIle', 'eGlc', 'eTyr')
data <- data[, vars]
data[vars] <- lapply(data[vars], factor)

tiers <- list(
  tier1 = c('Age', 'Sexe', 'aedu','acidep09'),
  tier2 = c('esmokstat', 'eauditsc', 'eipmeto2', 'eIRSsum9', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eVLDLTG', 'eGp', 'eIle', 'eGlc', 'eTyr')
)

bidirectional_pairs <- data.frame(
  from = c("ems_waist", "ems_waist", "ems_waist", "ems_waist",
           "ems_hpt", "ems_hpt", "ems_hpt",
           "ems_hdl2", "ems_hdl2", "ems_gluc2",
           "ems_hdl2", "ems_hdl2", "ems_gluc2"),
  to = c("ems_hpt", "ems_hdl2", "ems_gluc2", "ems_trig2",
         "ems_hdl2", "ems_gluc2", "ems_trig2",
         "ems_gluc2", "ems_trig2", "ems_trig2",
         "ems_gluc2", "ems_trig2", "ems_trig2")
)

bidirectional_forbidden <- rbind(bidirectional_pairs, bidirectional_pairs[2:1])

blacklist <- rbind(
  bidirectional_forbidden,
  tiers2blacklist(tiers),
  data.frame(
    from = c("Age", "Age", "Sexe", "Sexe", "aedu", "aedu", "eGlc"),
    to = c("Sexe", "aedu", "Age", "aedu", "Age", "Sexe", "eTyr")
  )
)

whitelist <- data.frame(
  from = c("Age", "Sexe", "aedu", "esmokstat", "esmokstat", "eipmeto2", "eauditsc", "eipmeto2",
           "eApoB", "eApoB", "eTotFA", "Age", "Age", "Age", "Age", "Sexe", "Sexe", "Sexe",
           "eTyr", "eipmeto2", "eipmeto2", "eauditsc", "eIRSsum9"),
  to = c("acidep09", "acidep09", "acidep09", "eHSCRP", "eIL6", "eHSCRP", "ems_waist", "ems_waist",
         "ems_waist", "ems_trig2", "ems_trig2", "ems_hdl2", "ems_waist", "ems_trig2", "eipmeto2",
         "ems_hdl2", "ems_trig2", "eipmeto2", "eGlc", "eGlc", "eGlc", "eGlc", "eGlc")
)

bootstrap_hc <- function(data, drop_rate, num_bootstraps) {
  network_list <- list()
  for (i in 1:num_bootstraps) {
    sample_indices <- sample(nrow(data), replace = TRUE)
    boot_data <- data[sample_indices, ]
    final_data <- boot_data[sample(1:nrow(boot_data), nrow(boot_data) * (1 - drop_rate), replace = FALSE), ]
    learned_network <- hc(final_data, whitelist = whitelist, blacklist = blacklist, score = "bde", max.iter=100, optimized=TRUE)
    network_list[[i]] <- learned_network
    cat(sprintf("Bootstrap %d/%d completed.\n", i, num_bootstraps))
  }
  return(network_list)
}

set.seed(123)

results_0 <- bootstrap_hc(data, 0.0, 100)
save(results_0, file = "updated_bootstrap_hc.RData")

results_10 <- bootstrap_hc(data, 0.1, 100)
save(results_10, file = "updated_bootstrap_hc10.RData")

results_20 <- bootstrap_hc(data, 0.2, 100)
save(results_20, file = "updated_bootstrap_hc20.RData")