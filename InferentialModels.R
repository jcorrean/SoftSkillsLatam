library(network)
RegionalNetworks <- readRDS("NetworkData/RegionalNetworks.RDS")
library(ergm)
library(ergm.multi)
library(coda)
SampledNetworks <- Networks(RegionalNetworks)

set.seed(2758)
model1 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names", levels = c(8, 9, 2, 4, 1)), 
                                                  control = control.ergm(MCMC.samplesize = 100000, 
                                                                         MCMC.burnin = 10000, 
                                                                         MCMLE.maxit = 10))
summary(model1) # AIC: 37397
GOF <- gof(model1)
plot(GOF)

set.seed(2758)
Simuladas1 <- simulate(model1, nsim = 1000, 
                       coef = model1$coefficients,
                       control = 
                         control.simulate.ergm(
                           MCMC.burnin = 100000, 
                           MCMC.interval = 500))

library(tidyverse)
extract_coefs_simulations <- function(Simuladas1, model) {
  # Obtener el número de simulaciones
  num_sims <- length(Simuladas1)
  
  # Crear un data frame vacío para almacenar los coeficientes
  coef_df1 <- data.frame(
    sim = 1:num_sims,
    nodos = numeric(num_sims),
    edges = numeric(num_sims),
    densidad = numeric(num_sims)
  )
  
  # Obtener los nombres de los coeficientes del modelo original
  coef_names <- names(coef(model))
  
  # Iterar sobre cada red simulada y extraer los coeficientes
  for (i in 1:num_sims) {
    sim_net <- Simuladas1[[i]]
    sim_model <- ergm(sim_net ~ edges + b2factor("vertex.names", levels = c(8, 9, 2, 4, 1)))
    sim_coefs <- coef(sim_model)
    
    # Agregar los coeficientes al data frame
    coef_df1[i, coef_names] <- sim_coefs
  }
  
  return(coef_df1)
}

coef_df1 <- extract_coefs_simulations(Simuladas1, model1)

model2 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names", levels = c(8, 9, 2, 4, 1)) + b1cov("Brochure.Length"))
summary(model2) # AIC: 36919
model3 <- ergm(SampledNetworks ~ edges + b2factor("vertex.names", levels = c(8, 9, 2, 4, 1)) + b1cov("Brochure.Length") + b1factor("Country", levels = NULL))
summary(model3) # AIC: 36004
