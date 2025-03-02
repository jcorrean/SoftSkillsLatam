load("~/Documents/GitHub/SoftSkillsLatam/SkillsARG.RData")
load("~/Documents/GitHub/SoftSkillsLatam/SkillsBRA.RData")
load("~/Documents/GitHub/SoftSkillsLatam/SkillsCHL.RData")
load("~/Documents/GitHub/SoftSkillsLatam/SkillsCOL.RData")
load("~/Documents/GitHub/SoftSkillsLatam/SkillsCR.RData")
load("~/Documents/GitHub/SoftSkillsLatam/SkillsECU.RData")
load("~/Documents/GitHub/SoftSkillsLatam/SkillsMEX.RData")
load("~/Documents/GitHub/SoftSkillsLatam/SkillsURU.RData")
load("~/Documents/GitHub/SoftSkillsLatam/SkillsVEN.RData")
Latam <- list(SkillsARG, SkillsBRA, SkillsCHL, SkillsCOL, SkillsCR, SkillsECU, SkillsMEX, SkillsURU, SkillsVEN)
Latam <- do.call(rbind, Latam)
library(tidyr)
RadarSkills <- Latam %>%
  mutate(skill_num = rep(1:10, 9)) %>%
  select(country, skill_num, Eigenvector) %>%
  pivot_wider(names_from = skill_num, values_from = Eigenvector) %>%
  column_to_rownames(var = "country")
colnames(RadarSkills) <- unique(Latam$vertex.names) 

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

length(RegionalNetworks)
RegionalNetworks
RegionalNetworks %>% keep(`%n%`, "OECD")
RegionalNetworks %>% discard(`%n%`, "OECD") %>% map(as_tibble, unit="vertices")
RegionalNetworks  %>% map(as_tibble, unit="vertices")
map

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