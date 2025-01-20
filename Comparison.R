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
country_names <- c("Argentina", "Brazil", "Chile", "Colombia", "CostaRica", "Ecuador", "Mexico", "Uruguay", "Venezuela")
RegionalNetworks <- setNames(RegionalNetworks[1:9],country_names)
saveRDS(RegionalNetworks, file = "RegionalNetworks.RDS")
library(network)
library(purrr)
library(tibble)
library(knitr)
library(dplyr)
RegionalNetworks <- readRDS("RegionalNetworks.RDS")
RegionalNetworks
length(RegionalNetworks)
RegionalNetworks
RegionalNetworks %>% keep(`%n%`, "OECD")
RegionalNetworks %>% discard(`%n%`, "OECD") %>% map(as_tibble, unit="edges")



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
ergm(SampledNetworks ~ N(~edges))
