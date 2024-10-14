library(visNetwork)
library(glue)
library(igraph)
library(htmlwidgets)

load("hc_graph.RData")
g <- as.igraph(hc_network)
nodes <- data.frame(id = V(g)$name, label = V(g)$name)
edges <- get.data.frame(g, what = "edges")
print(hc_network)

tier1 <- c('Age', 'Sexe', 'aedu', 'asmokstat', 'AIPMETO2', 'aauditsc', 'aIRSsum9', 'abaiscal', 'aids', 'acidep09', 'amet_syn2', 'ams_waist', 'ams_hpt', 'ams_trig2', 'ams_hdl2', 'ams_gluc2', 'atri_med', 'ahdl_med', 'asbp_med', 'adbp_med', 'agluc_med', 'ahsCRP', 'aIL6', 'aApoB', 'aHDL_C', 'aTotFA', 'aVLDL_TG', 'aGp', 'aIle', 'aGlc', 'aTyr')
tier2 <- c('eipmeto2', 'eauditsc', 'esmokstat', 'eIRSsum9', 'ebaiscal', 'eids', 'ecidep09', 'emet_syn2', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'etri_med', 'ehdl_med', 'esbp_med', 'edbp_med', 'egluc_med', 'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eVLDLTG', 'eGp', 'eIle', 'eGlc', 'eTyr')

renamed_labels <- list(
  'Age' = 'Age', 'eage' = 'Age',
  'Sexe' = 'Gender', 'sex' = 'Gender',
  'aedu' = 'Education years',
  'asmokstat' = 'Smoking status', 'esmokstat' = 'Smoking status',
  'aauditsc' = 'Alcohol consumption', 'eauditsc' = 'Alcohol consumption',
  'AIPMETO2' = 'Physical activity', 'eipmeto2' = 'Physical activity',
  'aIRSsum9' = 'Sleeping pattern', 'eIRSsum9' = 'Sleeping pattern',
  'acidep09' = 'MDD', 'ecidep09' = 'MDD',
  'amet_syn2' = 'MetS', 'emet_syn2' = 'MetS',
  'ams_waist' = 'Obesity', 'ems_waist' = 'Obesity',
  'ams_hpt' = 'Hypertension', 'ems_hpt' = 'Hypertension',
  'ams_trig2' = 'Hypertriglyceridemia', 'ems_trig2' = 'Hypertriglyceridemia',
  'ams_hdl2' = 'Low HDL cholesterol', 'ems_hdl2' = 'Low HDL cholesterol',
  'ams_gluc2' = 'Hyperglycemia', 'ems_gluc2' = 'Hyperglycemia',
  'atri_med' = 'Triglycerides', 'etri_med' = 'Triglycerides',
  'ahdl_med' = 'HDL cholesterol', 'ehdl_med' = 'HDL cholesterol',
  'asbp_med' = 'Systolic BP', 'esbp_med' = 'Systolic BP',
  'adbp_med' = 'Diastolic BP', 'edbp_med' = 'Diastolic BP',
  'agluc_med' = 'Glucose', 'egluc_med' = 'Glucose',
  'ahsCRP' = 'hs-CRP', 'eHSCRP' = 'hs-CRP',
  'aIL6' = 'IL-6', 'eIL6' = 'IL-6',
  'aApoB' = 'ApoB', 'eApoB' = 'ApoB',
  'aHDL_C' = 'HDLC', 'eHDLC' = 'HDLC',
  'aTotFA' = 'Total FA', 'eTotFA' = 'Total FA',
  'aVLDL_TG' = 'VLDL TG', 'eVLDLTG' = 'VLDL TG',
  'aGp' = 'AGP', 'eGp' = 'AGP',
  'aIle' = 'Ile', 'eIle' = 'Ile',
  'aGlc' = 'Glc', 'eGlc' = 'Glc', 
  'aTyr' = 'Tyr', 'eTyr' = 'Tyr',
  'abaiscal' = 'Anxiety', 'ebaiscal' = 'Anxiety',
  'aids' = 'MDD severity', 'eids' = 'MDD severity'
)

# Rename node labels
nodes$label <- sapply(nodes$label, function(x) renamed_labels[[x]])

# Assign node shapes
baseline_variables <- c('Age', 'Sexe', 'aedu', 'asmokstat', 'AIPMETO2', 'aauditsc', 'aIRSsum9', 'aids', 
                        'abaiscal', 'acidep09', 'amet_syn2', 'ams_waist', 'ams_hpt', 'ams_trig2', 
                        'ams_hdl2', 'ams_gluc2', 'atri_med', 'ahdl_med', 'asbp_med', 'adbp_med', 
                        'agluc_med', 'ahsCRP', 'aIL6', 'aApoB', 'aHDL_C', 'aTotFA', 'aVLDL_TG', 
                        'aGp', 'aIle', 'aGlc', 'aTyr')
follow_up_variables <- c('eage', 'sex', 'eipmeto2', 'esmokstat', 'eauditsc', 'eIRSsum9', 'ebaiscal', 'eids', 
                         'ecidep09', 'emet_syn2', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 
                         'ems_gluc2', 'etri_med', 'ehdl_med', 'esbp_med', 'edbp_med', 'egluc_med', 
                         'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eVLDLTG', 'eGp', 'eIle', 'eGlc', 'eTyr')


# Assign shapes based on baseline or follow-up
nodes$shape <- ifelse(nodes$id %in% baseline_variables, 'square', 'dot')

# Define colors for different variable groups
color_map <- list(
  'Background variables' = '#ADD7F6',
  'Major Depressive Disorder' = '#FF6347',
  'MetS components' = '#ffb703',
  'Lifestyle' = '#2667FF',
  'Metabolites' = '#90EE90'
)

variable_groups <- list(
  'Background variables' = c('Age', 'Gender', 'Education years'),
  'Major Depressive Disorder' = c('MDD'),
  'Lifestyle' = c('Smoking status', 'Physical activity', 'Alcohol consumption', 'Sleeping pattern'),
  'Metabolites' = c('IL-6', 'hs-CRP', 'ApoB', 'Total FA', 'VLDL TG', 'AGP', 'Ile', 'HDLC', 'Glc', 'Tyr')
)

all_variables <- unique(unlist(renamed_labels))
assigned_variables <- unlist(variable_groups)
variable_groups$`MetS components` <- setdiff(all_variables, assigned_variables)

nodes$color <- sapply(nodes$label, function(x) {
  group <- names(variable_groups)[sapply(variable_groups, function(v) x %in% v)]
  if (length(group) > 0) {
    color_map[[group]]
  } else {
    '#FFD700' 
  }
})

# Nodes position
all_nodes <- c(tier1, tier2)
nodes <- nodes[nodes$id %in% all_nodes,]
nodes <- nodes[order(match(nodes$id, all_nodes)),]

# Adjust this if you want to change how many nodes per row
nodes$y <- (seq_along(nodes$id) - 1) %/% 5 * 150 + 100
nodes$x <- (seq_along(nodes$id) - 1) %% 5 * 150

# Define legend for shapes and colors
legend_nodes <- data.frame(
  label = c('B', 'FU', names(color_map)),
  shape = c('square', 'dot', rep('dot', length(color_map))),
  color = c(NA, NA, unlist(color_map)),
  stringsAsFactors = FALSE
)

nodes <- nodes[!(nodes$id %in% c('eage', 'sex')),]

net <- visNetwork(nodes, edges) %>%
  visEdges(arrows = 'to') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visPhysics(stabilization = FALSE) %>%
  visLayout(randomSeed = 123) %>%
  visNodes(fixed = TRUE) %>%
  visLegend(addNodes = legend_nodes, useGroups = FALSE)

saveWidget(net, file = "hc_network.html", selfcontained = TRUE)
net
