Argentina <- readRDS("NetworkData/Argentina.RDS")
Brazil <- readRDS("NetworkData/Brazil.RDS")
Chile <- readRDS("NetworkData/Chile.RDS")
Colombia <- readRDS("NetworkData/Colombia.RDS")
CostaRica <- readRDS("NetworkData/CostaRica.RDS")
Ecuador <- readRDS("NetworkData/Ecuador.RDS")
Mexico <- readRDS("NetworkData/Mexico.RDS")
Uruguay <- readRDS("NetworkData/Uruguay.RDS")
Venezuela <- readRDS("NetworkData/Venezuela.RDS")

RegionalNetworks <- list(Argentina, Brazil, Chile, Colombia, CostaRica, Ecuador, Mexico, Uruguay, Venezuela)
library(network)
RegionalNetworks
country_names <- c("Argentina", "Brazil", "Chile", "Colombia", "CostaRica", "Ecuador", "Mexico", "Uruguay", "Venezuela")
RegionalNetworks <- setNames(RegionalNetworks[1:9],country_names)
saveRDS(RegionalNetworks, file = "NetworkData/RegionalNetworks.RDS")
rm(list=setdiff(ls(), c("RegionalNetworks")))
library(purrr)
library(tibble)
library(knitr)
library(dplyr)
RegionalNetworks <- readRDS("NetworkData/RegionalNetworks.RDS")
RegionalNetworks

library(igraph)
load("~/Documents/GitHub/SoftSkillsLatam/LatamNetwork.RData")
#RN <- graph_from_data_frame(RegionNetwork, directed = FALSE)
#RN <- as_adjacency_matrix(RN, attr = "Weight", sparse = FALSE)
library(tnet)
Region.network <- RegionNetwork[1:2]
Region.network$Source <- as.integer(factor(Region.network$Source))
Region.network$Target <- as.integer(factor(Region.network$Target)) 
pave <- as.tnet(Region.network, type = "binary two-mode tnet")
Region.Clustering <- tnet::reinforcement_tm(pave)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))

length(RegionalNetworks)
RegionalNetworks
RegionalNetworks %>% keep(`%n%`, "OECD")
RegionalNetworks %>% discard(`%n%`, "OECD") %>% map(as_tibble, unit="vertices")
RegionalNetworks  %>% map(as_tibble, unit="vertices")


RegionalNetworks %>%
  imap(~ {
    mnext_value <- .x$gal$mnext
    list(
      Country = .y,
      OECD.Member = .x %n% "OECD",
      n = network.size(.x),
      d = network.density(.x),
      Clustering = get.network.attribute(.x, "Clustering"),
      mnext = ifelse(is.null(mnext_value), NA_integer_, mnext_value)
    )
  }) %>%
  bind_rows() %>%
  group_by(Country) %>%
  summarize(
    OECD.Member = first(OECD.Member),
    Edges = mnext,
    Size = sum(n),
    Density = mean(d),
    Clustering = first(Clustering)
  ) %>%
  kable()

load("~/Documents/GitHub/SoftSkillsLatam/Curated_Data/SkillsARG.RData")
load("~/Documents/GitHub/SoftSkillsLatam/Curated_Data/SkillsBRA.RData")
load("~/Documents/GitHub/SoftSkillsLatam/Curated_Data/SkillsCHL.RData")
load("~/Documents/GitHub/SoftSkillsLatam/Curated_Data/SkillsCOL.RData")
load("~/Documents/GitHub/SoftSkillsLatam/Curated_Data/SkillsCR.RData")
load("~/Documents/GitHub/SoftSkillsLatam/Curated_Data/SkillsECU.RData")
load("~/Documents/GitHub/SoftSkillsLatam/Curated_Data/SkillsMEX.RData")
load("~/Documents/GitHub/SoftSkillsLatam/Curated_Data/SkillsURU.RData")
load("~/Documents/GitHub/SoftSkillsLatam/Curated_Data/SkillsVEN.RData")
Latam <- list(SkillsARG, SkillsBRA, SkillsCHL, SkillsCOL, SkillsCR, SkillsECU, SkillsMEX, SkillsURU, SkillsVEN)
Latam <- do.call(rbind, Latam)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
SKILLS <- Latam[c(1,7,10)]

png(filename = "FS.png", width = 40, height = 18, units = "in", res = 300)
ggplot(SKILLS, aes(x=vertex.names, y=Eigenvector))+
  geom_bar(stat = "identity", color = "black", fill = "#09419e") +
  facet_wrap(. ~ country) +
  theme_linedraw() +
  coord_flip()+
  theme(axis.text.x = element_text(angle=0, hjust=1, size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 30, colour = "black"),
        axis.title.y = element_text(size = 50, colour = "black"),
        legend.text = element_text(size = 30),  
        legend.title = element_text(size = 20), 
        strip.text = element_text(face="bold", size=rel(3.5), colour = "black"),
        strip.background = element_rect(fill="grey", colour="grey",
                                        size=30))+
  xlab("") + ylab("Eigenvector centrality of basic skills") +
  labs(fill="")
dev.off()





library(ergm.multi)
SampledNetworks <- Networks(RegionalNetworks)
# debugging
# head_networks <- head(RegionalNetworks, 9)
# SampledNetworks_head <- tryCatch(Networks(head_networks), error = function(e) e)
# print(SampledNetworks_head)
class(SampledNetworks)
SampledNetworks
# the term b1degree(3) specifies that programs, on average, should connect to 
# three skills (3 is the average degree centrality). If the estimated term is
# negative, that means programs would tend to specialize in fewer skills, if
# the estimated term is positive, that means programs would tend to connect to more skills

mod0 <- ergm(SampledNetworks ~ N(~ edges))
summary(mod0)
mod1 <- ergm(SampledNetworks ~ N(~edges + b1nodematch("Country", diff = FALSE)))
summary(mod1)
mod2 <- ergm(SampledNetworks ~ N(~edges + b1degree(3)))
summary(mod2)
mod3 <- ergm(SampledNetworks ~ N(~edges + b1degree(3) + b1factor("Program")))
summary(mod3)
mod4 <- ergm(SampledNetworks ~ N(~edges + b1degree(3) + b1factor("Program") + b1cov("Brochure.Length")))
summary(mod4)
mod5 <- ergm(SampledNetworks ~ N(~ edges + b1cov("Brochure.Length")))
summary(mod5)
#mcmc.diagnostics(mod1)

GOF1 <- gof(mod1)

mod6 <- ergm(SampledNetworks ~ N(~edges + b1degree(3,by="Program", levels = "Doctorado")))
summary(mod6)
exp(mod1$coefficients)/(1+exp(mod1$coefficients))
# About 28.56% of all possible edges actually exist.