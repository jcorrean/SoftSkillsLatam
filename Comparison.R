load("Results/Argentina.RData")
load("Results/Brazil.RData")
load("Results/Chile.RData")
load("Results/Colombia.RData")
load("Results/CostaRica.RData")
load("Results/Ecuador.RData")
load("Results/Mexico.RData")
load("Results/Uruguay.RData")
load("Results/Venezuela.RData")
RegionalNetworks <- list(Argentina, Brazil, Chile, Colombia, CostaRica, Ecuador, Mexico, Uruguay, Venezuela)
class(RegionalNetworks)
rm(list=setdiff(ls(), c("RegionalNetworks")))
library(network)
library(purrr)
library(tibble)
library(knitr)
library(dplyr)
length(RegionalNetworks)
RegionalNetworks
RegionalNetworks %>% keep(`%n%`, "OECD")
RegionalNetworks %>% discard(`%n%`, "OECD") %>% map(as_tibble, unit="edges")
RegionalNetworks %>% map(~list(OECD.Member = . %n% "OECD",
                n = network.size(.),
                d = network.density(.))) %>% bind_rows() %>%
  group_by(OECD.Member) %>%
  summarize(Countries = n(), Size = sum(n), Average.Density = mean(d)) %>% kable()

