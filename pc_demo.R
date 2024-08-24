library(visNetwork)
library(glue)
library(igraph)
library(htmlwidgets)


load("pc_graph.RData")
g <- as.igraph(pc_network)
nodes <- data.frame(id = V(g)$name, label = V(g)$name)
edges <- get.data.frame(g, what = "edges")
print(pc_network)

tier1 <- c('Age', 'Sexe')
tier2 <- c('aedu', 'asmokstat', 'AIPMETO2', 'aauditsc', 'aIRSsum9', 'abaiscal', 'aids', 'acidep09', 'amet_syn2', 'ams_waist', 'ams_hpt', 'ams_trig2', 'ams_hdl2', 'ams_gluc2', 'atri_med', 'ahdl_med', 'asbp_med', 'adbp_med', 'agluc_med', 'ahsCRP', 'aIL6', 'aApoB', 'aHDL_C', 'aTotFA', 'aSerum_TG', 'aGp', 'aIle')
tier3 <- c('eage', 'sex')
tier4 <- c('eipmeto2', 'eauditsc', 'eIRSsum9', 'ebaiscal', 'eids', 'ecidep09', 'emet_syn2', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 'ems_gluc2', 'etri_med', 'ehdl_med', 'esbp_med', 'edbp_med', 'egluc_med', 'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eSerumTG', 'eGp', 'eIle')

renamed_labels <- list(
  'Age' = 'Age', 'eage' = 'Age',
  'Sexe' = 'Gender', 'sex' = 'Gender',
  'aedu' = 'Education',
  'asmokstat' = 'Smoking Status',
  'aauditsc' = 'Alcohol Consumption', 'eauditsc' = 'Alcohol Consumption',
  'AIPMETO2' = 'Physical Activity', 'eipmeto2' = 'PhysicalAactivity',
  'aIRSsum9' = 'Sleep Pattern', 'eIRSsum9' = 'Sleep Pattern',
  'acidep09' = 'Major Depression', 'ecidep09' = 'Major Depression',
  'amet_syn2' = 'MetS', 'emet_syn2' = 'MetS',
  'ams_waist' = 'Obesity', 'ems_waist' = 'Obesity',
  'ams_hpt' = 'Hypertension', 'ems_hpt' = 'Hypertension',
  'ams_trig2' = 'Hypertriglyceridemia', 'ems_trig2' = 'Hypertriglyceridemia',
  'ams_hdl2' = 'Low HDL Cholesterol', 'ems_hdl2' = 'Low HDL Cholesterol',
  'ams_gluc2' = 'Hyperglycemia', 'ems_gluc2' = 'Hyperglycemia',
  'atri_med' = 'Triglycerides', 'etri_med' = 'Triglycerides',
  'ahdl_med' = 'HDL Cholesterol', 'ehdl_med' = 'HDL Cholesterol',
  'asbp_med' = 'Systolic BP', 'esbp_med' = 'Systolic BP',
  'adbp_med' = 'Diastolic BP', 'edbp_med' = 'Diastolic BP',
  'agluc_med' = 'Glucose', 'egluc_med' = 'Glucose',
  'ahsCRP' = 'hs-CRP', 'eHSCRP' = 'hs-CRP',
  'aIL6' = 'IL-6', 'eIL6' = 'IL-6',
  'aApoB' = 'ApoB', 'eApoB' = 'ApoB',
  'aHDL_C' = 'Total Cholesterol in HDL', 'eHDLC' = 'Total Cholesterol in HDL',
  'aTotFA' = 'Total Fatty Acids', 'eTotFA' = 'Total Fatty Acids',
  'aSerum_TG' = 'Serum Total Triglycerides', 'eSerumTG' = 'Serum Total Triglycerides',
  'aGp' = 'Glycoprotein acetyls', 'eGp' = 'Glycoprotein acetyls',
  'aIle' = 'Isoleucine', 'eIle' = 'Isoleucine',
  'abaiscal' = 'Anxiety', 'ebaiscal' = 'Anxiety',
  'aids' = 'Depression Severity', 'eids' = 'Depression Severity'
)

# Rename node labels
nodes$label <- sapply(nodes$label, function(x) renamed_labels[[x]])

# Assign node shapes
baseline_variables <- c('Age', 'Sexe', 'aedu', 'asmokstat', 'AIPMETO2', 'aauditsc', 'aIRSsum9', 'aids', 
                        'abaiscal', 'acidep09', 'amet_syn2', 'ams_waist', 'ams_hpt', 'ams_trig2', 
                        'ams_hdl2', 'ams_gluc2', 'atri_med', 'ahdl_med', 'asbp_med', 'adbp_med', 
                        'agluc_med', 'ahsCRP', 'aIL6', 'aApoB', 'aHDL_C', 'aTotFA', 'aSerum_TG', 
                        'aGp', 'aIle')
follow_up_variables <- c('eage', 'sex', 'eipmeto2', 'eauditsc', 'eIRSsum9', 'ebaiscal', 'eids', 
                         'ecidep09', 'emet_syn2', 'ems_waist', 'ems_hpt', 'ems_trig2', 'ems_hdl2', 
                         'ems_gluc2', 'etri_med', 'ehdl_med', 'esbp_med', 'edbp_med', 'egluc_med', 
                         'eHSCRP', 'eIL6', 'eApoB', 'eHDLC', 'eTotFA', 'eSerumTG', 'eGp', 'eIle')


# Assign shapes based on baseline or follow-up
nodes$shape <- ifelse(nodes$id %in% baseline_variables, 'square', 'dot')

# Define colors for different variable groups
color_map <- list(
  'Background variables' = '#ADD7F6',
  'Conditional variables' = '#2667FF',
  'Metabolites' = '#90EE90',
  'Intermediate variables' = '#ffb703',
  'Diagnostic variables' = '#FF6347'
)

variable_groups <- list(
  'Background variables' = c('Age', 'Gender'),
  'Metabolites' = c('ApoB', 'Total FA', 'Serum TG', 'AGP', 'Ile', 'HDLC'),
  'Diagnostic variables' = c('Major depression', 'MetS'),
  'Conditional variables' = c('Smoking status', 'Physical activity', 'Education', 'Alcohol consumption', 'Sleep pattern', 'Anxiety', 'Depression severity')
)

all_variables <- unique(unlist(renamed_labels))
assigned_variables <- unlist(variable_groups)
variable_groups$`Intermediate variables` <- setdiff(all_variables, assigned_variables)

nodes$color <- sapply(nodes$label, function(x) {
  group <- names(variable_groups)[sapply(variable_groups, function(v) x %in% v)]
  if (length(group) > 0) {
    color_map[[group]]
  } else {
    '#FFD700' 
  }
})

# Nodes position
all_nodes <- c(tier1, tier2, tier3, tier4)
nodes <- nodes[nodes$id %in% all_nodes,]
nodes <- nodes[order(match(nodes$id, all_nodes)),]

# Maximum 8 nodes per row
nodes$y <- (seq_along(nodes$id) - 1) %/% 8 * 150 + 100
nodes$x <- (seq_along(nodes$id) - 1) %% 8 * 150

# Define legend for shapes and colors
legend_nodes <- data.frame(
  label = c('B', 'FU', names(color_map)),
  shape = c('square', 'dot', rep('dot', length(color_map))),
  color = c(NA, NA, unlist(color_map)),
  stringsAsFactors = FALSE
)

net <- visNetwork(nodes, edges) %>%
  visEdges(arrows = 'to') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visPhysics(stabilization = FALSE) %>%
  visLayout(randomSeed = 123) %>%
  visNodes(fixed = TRUE) %>%
  visLegend(addNodes = legend_nodes, useGroups = FALSE)

#saveWidget(net, file = "pc_network.html", selfcontained = TRUE)
net