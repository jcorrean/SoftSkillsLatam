library(network)
RegionalNetworks <- readRDS("NetworkData/RegionalNetworks.RDS")
library(ergm)
library(ergm.multi)
SampledNetworks <- Networks(RegionalNetworks)


model1 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names", levels = c(8, 9, 2, 4, 1)))
summary(model1) # AIC: 37397
model4 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names") + b1cov("Brochure.Length"))
summary(model4) # AIC: 35759
model5 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names") + b1cov("Brochure.Length") + b1factor("Country"))
summary(model5) # AIC: 34821
