library(network)
RegionalNetworks <- readRDS("NetworkData/RegionalNetworks.RDS")
library(ergm)
library(ergm.multi)
SampledNetworks <- Networks(RegionalNetworks)


model1 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names", levels = c(8, 9, 2, 4, 1)))
summary(model1) # AIC: 37397
model2 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names", levels = c(8, 9, 2, 4, 1)) + b1cov("Brochure.Length"))
summary(model2) # AIC: 36919
model3 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names", levels = c(8, 9, 2, 4, 1)) + b1cov("Brochure.Length") + b1factor("Country", levels = NULL))
summary(model3) # AIC: 36004
