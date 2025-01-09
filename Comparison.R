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
RegionalNetworks %>% discard(`%n%`, "OECD")
RegionalNetworks %>% discard(`%n%`, "OECD") %>% map(as_tibble, unit="edges")
RegionalNetworks %>% map(~list(development = . %n% "OECD",
                n = network.size(.),
                d = network.density(.))) %>% bind_rows() %>%
  group_by(development) %>%
  summarize(nnets = n(), p1 = mean(d==1), m = mean(d)) %>% kable()
