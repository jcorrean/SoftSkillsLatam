load("Results/Argentina.RData")
load("Results/Brazil.RData")
load("Results/Chile.RData")
load("Results/Colombia.RData")
load("Results/CostaRica.RData")
load("Results/Ecuador.RData")
load("Results/Mexico.RData")
load("Results/Uruguay.RData")
load("Results/Venezuela.RData")

RegionNetwork <- do.call(rbind, list(edges_ar, 
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
Matrix <- Matrix[1:10,11:3165]


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
Centralities$Partition <- "Skill" 
Centralities$Partition[c(11:3165)] <- "Program" 

table(Centralities$Partition)


library(network)
RegionNetwork <- network(Matrix, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
RegionNetwork
SizeRN <- network::network.size(RegionNetwork)
DensityRN <- network::network.density(RegionNetwork)
ClusteringRN <- tnet::clustering_tm(Matrix)

library(ggplot2)
library(ggridges)
ggplot(Centralities, aes(x = Eigenvector, y = Partition, fill = Partition)) +
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.35) +
  theme_classic() +
  ylab("Network Partition") +
  xlab("Eigenvector centrality")+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size= 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none") +
  annotate("text", x = 0.2, y = 1.9, label = "Average centrality = 3.5", hjust = 0, vjust = 1) +
  annotate("text", x = 0.2, y = 1.8, label = "Standard deviation centrality = 3.5", hjust = 0, vjust = 1) +
  annotate("text", x = 0.2, y = 0.9, label = "Average centrality = 1.7", hjust = 0, vjust = 1) +
  annotate("text", x = 0.2, y = 0.8, label = "Standard deviation centrality = 1.7", hjust = 0, vjust = 1)


library(psych)
describeBy(Centralities$Eigenvector, group = Centralities$Partition, mat = TRUE, digits = 2)
