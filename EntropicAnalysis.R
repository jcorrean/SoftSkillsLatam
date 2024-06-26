load("ARG.RData")
load("BRA.RData")
load("CHL.RData")
load("COL.RData")
load("ECU.RData")
load("MEX.RData")
load("URU.RData")
load("VEN.RData")
rm(list=setdiff(ls(), c("IMARG2", "IMVEN2", "IMBRA2", "IMCL2", "IMCO2", "IMEC2", "IMEX2", "IMURU2")))

library(entropy)

# Function to calculate entropy metrics for matrix data
calculate_entropy_metrics_matrix <- function(dataset_name) {
  # Retrieve the dataset directly using the dataset_name variable
  dataset <- get(dataset_name, envir =.GlobalEnv) # Specify the environment if needed
  
  # Check if the dataset is a matrix and has at least two dimensions
  if (!is.matrix(dataset) || dim(dataset)[2] < 2) {
    warning(paste("Skipping", dataset_name, "due to insufficient dimensions or not being a matrix. It might be empty or incorrectly loaded.", sep = " "))
    return(NA) # Return NA for this iteration
  }
  
  # Calculate total per column
  total_por_columna <- colSums(dataset)
  
  # Normalize frequencies
  probabilidades_normalizadas <- dataset / total_por_columna
  
  epsilon <- 1e-10 # Adjust as necessary
  
  # Apply entropy formula
  entropias_columna <- sapply(probabilidades_normalizadas, function(x) {
    -sum((x + epsilon) * log2(x + epsilon))
  })
  
  EC <- as.matrix(entropias_columna)
  
  # Average entropies
  entropia_promedio <- mean(entropias_columna, na.rm = TRUE)
  
  print(paste("Entropía Promedio:", entropia_promedio))
  
  return(entropia_promedio)
}

# List of datasets
datasets <- c("IMARG2", "IMBRA2", "IMCL2", "IMCO2", "IMEC2", "IMEX2", "IMURU2", "IMVEN2")

# Loop through each dataset and calculate entropy metrics
results <- lapply(datasets, calculate_entropy_metrics_matrix)
summary(results)
