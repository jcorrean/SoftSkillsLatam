load("Results/Argentina.RData")
load("Results/Brazil.RData")
load("Results/Chile.RData")
load("Results/Colombia.RData")
load("Results/CostaRica.RData")
load("Results/Ecuador.RData")
load("Results/Mexico.RData")
load("Results/Uruguay.RData")
load("Results/Venezuela.RData")

library(stringr)
library(tidyverse)
ARG %>%
  mutate(initial_digit = str_extract(doc_id, "^\\d+")) %>%
  count(initial_digit) #29

BRA %>%
  mutate(initial_digit = str_extract(doc_id, "^\\d+")) %>%
  count(initial_digit) #268

CHL %>%
  mutate(initial_digit = str_extract(doc_id, "^\\d+")) %>%
  count(initial_digit) #51

COL %>%
  mutate(initial_digit = str_extract(doc_id, "^\\d+")) %>%
  count(initial_digit) #50

CORI %>%
  mutate(initial_digit = str_extract(doc_id, "^\\d+")) %>%
  count(initial_digit) # 2

ECU %>%
  mutate(initial_digit = str_extract(doc_id, "^\\d+")) %>%
  count(initial_digit) #29

MEX %>%
  mutate(initial_digit = str_extract(doc_id, "^\\d+")) %>%
  count(initial_digit) #27

URU %>%
  mutate(initial_digit = str_extract(doc_id, "^\\d+")) %>%
  count(initial_digit) #8

VEN %>%
  mutate(initial_digit = str_extract(doc_id, "^\\d+")) %>%
  count(initial_digit) #17

Sampled.Universities <- 29+268+51+50+2+29+27+8+17
Sampled.Universities

268/Sampled.Universities

RegionNetwork <- do.call(rbind, list(edges_args, 
                                     edges_br, 
                                     edges_chl, 
                                     edges_col, 
                                     edges_cr, 
                                     edges_ecu, 
                                     edges_mx, 
                                     edges_uy, 
                                     edges_ven))
rm(list=setdiff(ls(), c("RegionNetwork")))
load("LatamNetwork.RData")

library(igraph)
bnR <- graph_from_data_frame(RegionNetwork, directed = F)
bipartite_mapping(bnR)
V(bnR)$type <- bipartite_mapping(bnR)$type
V(bnR)$shape <- ifelse(V(bnR)$type, "circle", "square")
V(bnR)$color <- ifelse(V(bnR)$type, "green4", "red3")
V(bnR)$label.cex <- ifelse(V(bnR)$type, 0.5, 1)
V(bnR)$size <- sqrt(igraph::degree(bnR))
E(bnR)$color <- "lightgrey"
png(filename = "FR.png", width = 40, height = 18, units = "in", res = 300)
set.seed(8970)
plot(bnR, vertex.label = NA, layout = layout.bipartite, arrow.width = 0.1, arrow.size = 0.1)
dev.off()

ProgramsRegion <- data.frame(Degree.centrality = igraph::degree(bnR),
                          Closeness.centrality = igraph::closeness(bnR),
                          Betweennes.centrality = igraph::betweenness(bnR),
                          Eigenvector.centrality = igraph::eigen_centrality(bnR)$vector)
rownames(ProgramsRegion)
ProgramsRegion$Node <- rownames(ProgramsRegion)
ProgramsRegion <- ProgramsRegion[order(-ProgramsRegion$Degree), ]
ProgramsRegion <- mutate(ProgramsRegion, 
                      Partition = ifelse(
                        grepl("text", Node), "Program", "Skill"))
ProgramsRegion <- mutate(ProgramsRegion, 
                       Country = ifelse(
                         grepl("ARG_text", Node), "Argentina", 
                         ifelse(grepl("BRA_text", Node), "Brazil",
                         ifelse(grepl("CHL_text", Node), "Chile",
                         ifelse(grepl("COL_text", Node), "Colombia",
                         ifelse(grepl("CR_text", Node), "Costa Rica",
                         ifelse(grepl("ECU_text", Node), "Ecuador",
                         ifelse(grepl("MEX_text", Node), "Mexico",
                         ifelse(grepl("URU_text", Node), "Uruguay",
                         ifelse(grepl("VEN_text", Node), "Venezuela", "Latam"))))))))))

table(ProgramsRegion$Partition)

library(dplyr)
result <- ProgramsRegion %>%
  group_by(Partition) %>%
  summarize(
    Mean = mean(Degree.centrality),
    SD = sd(Degree.centrality)
  )

result
library(network)


library(psych)
describeBy(ProgramsRegion$Eigenvector, group = ProgramsRegion$Partition, mat = TRUE, digits = 2)

rm(list = ls())

load("Results/Argentina.RData")
load("Results/Brazil.RData")
load("Results/Chile.RData")
load("Results/Colombia.RData")
load("Results/CostaRica.RData")
load("Results/Ecuador.RData")
load("Results/Mexico.RData")
load("Results/Uruguay.RData")
load("Results/Venezuela.RData")

RegionNetwork <- do.call(rbind, list(ProgramsARG, 
                                     ProgramsBRA, 
                                     ProgramsCHL, 
                                     ProgramsCOL, 
                                     ProgramsCORI, 
                                     ProgramsECU, 
                                     ProgramsMEX, 
                                     ProgramsURU, 
                                     ProgramsVEN))
SkillsRegion <- RegionNetwork %>% filter(., Partition == "Skill")
RegionalSkills <- describeBy(SkillsRegion$Eigenvector, group = SkillsRegion$Country, mat = TRUE, digits = 2)
mean(RegionalSkills$mean)
mean(RegionalSkills$sd)
ProgramsRegion <- RegionNetwork %>% filter(., Partition == "Program")
RegionalPrograms <- describeBy(ProgramsRegion$Eigenvector, group = ProgramsRegion$Country, mat = TRUE, digits = 2)
mean(RegionalPrograms$mean)
mean(RegionalPrograms$sd)

mean(RegionalSkills$mean)/mean(RegionalPrograms$mean)
rm(list=setdiff(ls(), c("RegionNetwork")))

Promedios <- RegionNetwork %>%
  group_by(Country) %>%
  group_by(Node) %>% 
  summarize(Average = mean(Eigenvector))
describeBy(RegionNetwork$Eigenvector, group = RegionNetwork$Country, mat = TRUE, digits = 2)
Connectivity <- data.frame(Country = c("Argentina",
                                       "Brazil",
                                       "Chile",
                                       "Colombia",
                                       "Costa Rica",
                                       "Ecuador",
                                       "Mexico",
                                       "Uruguay",
                                       "Venezuela"),
                           Brochures = c(514,922,208,230,120,731,553,147,210),
                           Universities = c(146,1258,128,297,68,111,1105,40,71),
                           Connectivity = c(0.37/0.06,
                                            0.34/0.04,
                                            0.47/0.11,
                                            0.42/0.09,
                                            0.57/0.13,
                                            0.37/0.04,
                                            0.52/0.07,
                                            0.18/0.11,
                                            0.54/0.10))
