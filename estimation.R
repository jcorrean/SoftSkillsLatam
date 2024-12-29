#################################
# Libraries and Initial Setup
#################################
library(tidyverse)
library(igraph)
library(network)
library(ergm)
library(intergraph)
library(parallel)
library(ggplot2)

# Set working directory
setwd("C:/Users/LAVERDE/Desktop/Juan Redes")

# Define key parameters
countries <- c("Argentina", "Brazil", "Chile", "Colombia", "CostaRica", "Ecuador", "Mexico", "Uruguay", "Venezuela")
country_abbr <- c("ARG", "BRA", "CHI", "COL", "CR", "ECU", "MEX", "URU", "VEN")
levels <- c("SPEC", "MS", "PHD")

#################################
# Load and Combine Matrices
#################################
matrix_list <- list()
column_attr_list <- list()

# Helper function to retrieve a matrix
get_matrix <- function(abbr, level) {
  level_name <- paste0("Matrix", abbr, level)
  if (exists(level_name)) return(get(level_name))
  return(NULL)
}

for (i in seq_along(countries)) {
  country <- countries[i]
  abbr <- country_abbr[i]
  rdata_file <- paste0(country, ".RData")
  
  if (file.exists(rdata_file)) {
    load(rdata_file)
    for (level in levels) {
      matrix <- get_matrix(abbr, level)
      if (!is.null(matrix)) {
        matrix[matrix > 0] <- 1
        colnames(matrix) <- paste0(colnames(matrix), "_", abbr, "_", level)
        matrix_list[[length(matrix_list) + 1]] <- matrix
        column_attr_list[[length(column_attr_list) + 1]] <- data.frame(
          ProgramID = colnames(matrix),
          Country = rep(country, ncol(matrix)),
          Level = rep(level, ncol(matrix)),
          stringsAsFactors = FALSE
        )
      }
    }
    rm(list = ls(pattern = paste0("^Matrix", abbr)))
  }
}

# Combine matrices
common_skills <- Reduce(intersect, lapply(matrix_list, rownames))
matrix_list <- lapply(matrix_list, function(mat) mat[common_skills, , drop = FALSE])
big_mat <- do.call(cbind, matrix_list)
column_attributes <- do.call(rbind, column_attr_list)

# Filter nodes with low connectivity
big_mat <- big_mat[rowSums(big_mat) > 3, colSums(big_mat) > 3]

# Check for duplicates
if (anyDuplicated(colnames(big_mat)) > 0) stop("Duplicated columns detected in big_mat.")
big_mat <- as.matrix(big_mat)
storage.mode(big_mat) <- "numeric"

#################################
# Create Bipartite Network
#################################
create_bipartite_network <- function(big_mat, column_attributes) {
  g_bip <- graph_from_incidence_matrix(big_mat)
  num_skills <- nrow(big_mat)
  num_programs <- ncol(big_mat)
  order <- match(colnames(big_mat), column_attributes$ProgramID)
  column_attributes <- column_attributes[order, , drop = FALSE]
  
  V(g_bip)$type <- c(rep(FALSE, num_skills), rep(TRUE, num_programs))
  V(g_bip)$Country <- c(rep("NoCountry", num_skills), as.character(column_attributes$Country))
  V(g_bip)$Level <- c(rep("NoLevel", num_skills), as.character(column_attributes$Level))
  V(g_bip)$skill <- c(rownames(big_mat), rep(NA, num_programs))
  
  bip_net <- asNetwork(g_bip)
  bip_net %v% "type" <- V(g_bip)$type
  bip_net %v% "Country" <- V(g_bip)$Country
  bip_net %v% "Level" <- V(g_bip)$Level
  bip_net %v% "skill" <- V(g_bip)$skill
  network::set.network.attribute(bip_net, "bipartite", num_skills)
  return(bip_net)
}

# Generate the initial bipartite network
bip_net <- create_bipartite_network(big_mat, column_attributes)

#################################
# Fit Models
#################################
model_no_structural <- ergm(
  bip_net ~ edges + b2factor("Country") + b2factor("Level") + b1factor("skill"),
  control = control.ergm(
    parallel = detectCores() - 1,
    parallel.type = "PSOCK",
    MCMC.samplesize = 10000,
    MCMC.interval = 1000,
    MCMC.burnin = 5000
  )
)

model_with_gwdsp <- ergm(
  bip_net ~ edges + b2factor("Country") + b2factor("Level") + b1factor("skill") + gwdsp(0.25, fixed = TRUE),
  control = control.ergm(
    parallel = detectCores() - 1,
    parallel.type = "PSOCK",
    MCMC.samplesize = 10000,
    MCMC.interval = 1000,
    MCMC.burnin = 5000
  )
)

# Adjusted model with gwdsp
adjusted_model_gwdsp <- ergm(
  bip_net ~ edges + b2factor("Country") + b2factor("Level") + b1factor("skill") + gwdsp(0.5, fixed = TRUE),
  control = control.ergm(
    parallel = detectCores() - 1,
    parallel.type = "PSOCK",
    MCMC.samplesize = 20000,
    MCMC.interval = 1000,
    MCMC.burnin = 10000
  )
)

#################################
# Model Comparison
#################################
model_comparison <- data.frame(
  Model = c("No Structural Terms", "With GWDSP", "Adjusted with GWDSP"),
  AIC = c(AIC(model_no_structural), AIC(model_with_gwdsp), AIC(adjusted_model_gwdsp)),
  BIC = c(BIC(model_no_structural), BIC(model_with_gwdsp), BIC(adjusted_model_gwdsp))
)
print(model_comparison)

#################################
# Goodness-of-Fit (GOF)
#################################
gof_no_structural <- gof(model_no_structural)
gof_with_gwdsp <- gof(model_with_gwdsp)
gof_adjusted_gwdsp <- gof(adjusted_model_gwdsp)

print(gof_no_structural)
print(gof_with_gwdsp)
print(gof_adjusted_gwdsp)

# Save GOF plots
ggplot2::ggsave("GOF_NoStructural.png", plot(gof_no_structural))
ggplot2::ggsave("GOF_WithGWDSP.png", plot(gof_with_gwdsp))
ggplot2::ggsave("GOF_AdjustedGWDSP.png", plot(gof_adjusted_gwdsp))

#################################
# Final Results
#################################
cat("\nFinal Results:\n")
print(summary(adjusted_model_gwdsp))
cat("\nGOF for the Adjusted Model:\n")
print(gof_adjusted_gwdsp$summary.model)

cat("\nAnalysis completed. Results and plots saved in the working directory.")

############################################################
# Network Simulation
############################################################

# Simulate networks from the adjusted model
set.seed(123)
simulated_networks <- simulate(adjusted_model_gwdsp, nsim = 100, output = "network")
summary(simulated_networks)

# Visualize a simulated network
plot(simulated_networks[[1]], main = "Simulated Network from Adjusted Model")

# Calculate statistics for the observed network
library(statnet)
obs_statistics <- summary(bip_net ~ degree(0:10) + esp(0:10) + edges)

print("Statistics of the observed network:")
print(obs_statistics)

# Initialize matrices to store simulated statistics
deg_sim <- matrix(0, nrow = 100, ncol = 11) # Stores degrees 0-10
esp_sim <- matrix(0, nrow = 100, ncol = 11) # Edgewise Shared Partners (ESP)
geo_sim <- list()                           # Geodesic distances
edges_sim <- numeric(100)                   # Total number of edges

# Calculate statistics for simulated networks
for (i in 1:100) {
  # Degree distribution 0-10
  deg_sim[i, ] <- summary(simulated_networks[[i]] ~ degree(0:10))
  
  # Edgewise Shared Partners (up to 10)
  esp_sim[i, ] <- summary(simulated_networks[[i]] ~ esp(0:10))
  
  # Total number of edges
  edges_sim[i] <- summary(simulated_networks[[i]] ~ edges)
  
  # Geodesic distances using sna::geodist
  geo_sim[[i]] <- geodist(simulated_networks[[i]])$gdist
}

# Summarize simulated geodesic distances
geo_mean_sim <- sapply(geo_sim, function(x) mean(as.vector(x), na.rm = TRUE))

# Visualize results
print("Statistics of the simulated networks:")
cat("Average degrees:\n")
print(colMeans(deg_sim))
cat("\nAverage Edgewise Shared Partners (ESP):\n")
print(colMeans(esp_sim))
cat("\nAverage number of edges:\n")
print(mean(edges_sim))
cat("\nAverage geodesic distances:\n")
print(mean(geo_mean_sim))

# Compare observed vs simulated statistics
cat("\nComparison with the observed network:")
print(obs_statistics)
