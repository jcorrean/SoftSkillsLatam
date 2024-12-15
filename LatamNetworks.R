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

RegionNetwork <- do.call(rbind, list(edges_arg, 
                                     edges_br, 
                                     edges_chl, 
                                     edges_col, 
                                     edges_cr, 
                                     edges_ec, 
                                     edges_mx, 
                                     edges_ur, 
                                     edges_ve))
rm(list=setdiff(ls(), c("RegionNetwork")))
RegionNetwork <- RegionNetwork[,c(2,1,3)]
colnames(RegionNetwork)[1] <- "Skill"
colnames(RegionNetwork)[2] <- "Brochure"

library(dplyr)
RegionNetwork %>%
  group_by(Country) %>%
  summarize(count = length(unique(Skill)))

library(igraph)
bnR <- graph_from_data_frame(RegionNetwork,directed=FALSE)
bipartite_mapping(bnR)
V(bnR)$type <- bipartite_mapping(bnR)$type
V(bnR)$type <- bipartite_mapping(bnR)$type
V(bnR)$shape <- ifelse(V(bnR)$type, "circle", "square")
V(bnR)$label.cex <- ifelse(V(bnR)$type, 0.5, 1)
V(bnR)$size <- sqrt(igraph::degree(bnR))
E(bnR)$color <- "lightgrey"
Matrix <- as.matrix(as_adjacency_matrix(bnR))
nrow(Matrix) - 10
ncol(Matrix)
Matrix <- Matrix[1:10,11:ncol(Matrix)]
Columnas <- data.frame(colnames(Matrix))


Centralities <- data.frame(Degree = igraph::degree(bnR),
                          Closeness = igraph::closeness(bnR),
                          Betweennes = igraph::betweenness(bnR),
                          Eigen = igraph::eigen_centrality(bnR))
Centralities <- Centralities[ -c(5:25) ]
rownames(Centralities)
Centralities$Node <- rownames(Centralities)
Centralities <- Centralities[order(-Centralities$Degree), ]
#Centralities <- Centralities[!grepl("text", Centralities$SS), ]
#Centralities <- Centralities[1:4]
colnames(Centralities)[4] <- "Eigenvector"
Centralities <- mutate(Centralities, 
                      Partition = ifelse(
                        grepl("text", Node), "Program", "Skill"))
Centralities <- mutate(Centralities, 
                       Country = ifelse(
                         grepl("ARG_text", Node), "Argentina", 
                         ifelse(grepl("BRA_text", Node), "Brazil",
                         ifelse(grepl("CHL_text", Node), "Chile",
                         ifelse(grepl("COL_text", Node), "Colombia",
                         ifelse(grepl("CR_text", Node), "Costa Rica",
                         ifelse(grepl("ECU_text", Node), "Ecuador",
                         ifelse(grepl("MEX_text", Node), "Mexico",
                         ifelse(grepl("URU_text", Node), "Uruguay",
                         ifelse(grepl("VEN_text", Node), "Venezuela", ""))))))))))

table(Centralities$Partition)

library(dplyr)
result <- Centralities %>%
  group_by(Country, Partition) %>%
  summarize(
    Mean = mean(Eigenvector),
    SD = sd(Eigenvector)
  )

result
library(network)
RegionNetwork <- network(Matrix, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
RegionNetwork
SizeRN <- network::network.size(RegionNetwork)
DensityRN <- network::network.density(RegionNetwork)
#ClusteringRN <- tnet::clustering_tm(Matrix)

library(ggplot2)
library(ggridges)
png(filename = "f1.png", width = 10, height = 8, units = "in", res = 300)
ggplot(Centralities, aes(x = Eigenvector, y = Partition, fill = Partition)) +
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.35) +
  theme_bw() +
  ylab("Network Partition") +
  xlab("Eigenvector centrality degree") +
  coord_cartesian(xlim = c(0, 1.1)) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.x = element_text(size= 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none") +
  annotate("text", x = 0.35, y = 1.9, label = "Average centrality = 0.41", hjust = 0, vjust = 1) +
  annotate("text", x = 0.35, y = 1.8, label = "Standard deviation centrality = 0.26", hjust = 0, vjust = 1) +
  annotate("text", x = 0.02, y = 0.9, label = "Average centrality = 0.02", hjust = 0, vjust = 1) +
  annotate("text", x = 0.02, y = 0.8, label = "Standard deviation centrality = 0.01", hjust = 0, vjust = 1)
dev.off()

library(psych)
describeBy(Centralities$Eigenvector, group = Centralities$Partition, mat = TRUE, digits = 2)

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
0.42/0.08
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
