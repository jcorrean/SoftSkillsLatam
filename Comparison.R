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
class(RegionalNetworks)
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
SampledNetworks
mod1 <- ergm(SampledNetworks ~ N(~edges))
summary(mod1)
exp(mod1$coefficients)/(1+exp(mod1$coefficients))
# About 28.56% of all possible edges actually exist.