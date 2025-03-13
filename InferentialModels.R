library(network)
RegionalNetworks <- readRDS("NetworkData/RegionalNetworks.RDS")
library(ergm)
library(ergm.multi)
SampledNetworks <- Networks(RegionalNetworks)


model1 <- ergm(SampledNetworks ~ edges)
summary(model1) # AIC: 43477
model1B <- ergm(SampledNetworks ~ edges + b1factor("Country", levels = NULL))
summary(model1B) # AIC: 42617
model2 <- ergm(SampledNetworks ~ edges + b1cov("Brochure.Length"))
summary(model2) # AIC: 43157
model3A <- ergm(SampledNetworks ~ edges + b2factor("vertex.names", levels = NULL))
summary(model3A)
model3B <- ergm(SampledNetworks ~ edges + b2factor("vertex.names", levels = c(8, 9, 2, 4, 1)))
summary(model3B) # AIC: 36306
model4 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names") + b1cov("Brochure.Length"))
summary(model4) # AIC: 35759
model5 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names") + b1cov("Brochure.Length") + b1factor("Country"))
summary(model5) # AIC: 34821
