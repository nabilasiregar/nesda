library(bnlearn)

# use the merged dataset from both waves. 
# can be obtained from running prepare_wave1.ipynb first and then run prepare_wave5.ipynb 
data <- read.csv("data/updated_discrete_data.csv")

# state all variables we want to include in the graph
vars <- c('Age', 'Sexe', 'aedu','acidep09', 'esmokstat', 'eauditsc', 'eipmeto2', 'eIRSsum9', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eVLDLTG', 'eGp', 'eIle', 'eGlc', 'eTyr')
data <- data[, vars]
data[vars] <- lapply(data[vars], factor)
colnames(data)

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

create_hc_network <- function(data, blacklist, whitelist, score_type, max_iterations, optimized, output_filename) {
  network <- hc(data, blacklist = blacklist, whitelist = whitelist, score = score_type, max.iter = max_iterations, optimized = optimized)
  # network <- pc.stable(data, blacklist = blacklist, whitelist = whitelist, test = "mi", alpha = 0.05)
  print(network)
  save(network, file = output_filename)
  
  return(network)
}

hc_network <- create_hc_network(data, blacklist, whitelist, "bde", 100, FALSE, "hc_graph.RData")
#hc_network_without_knowledge <- create_hc_network(data, blacklist, NULL, "bde", 100, FALSE, "hc_graph_without_knowledge.RData")
