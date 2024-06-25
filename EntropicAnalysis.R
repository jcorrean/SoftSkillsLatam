library(entropy)
load("ARG.RData")
total_por_columna <- colSums(IARG2)

# Normalizamos las frecuencias dividiendo por el total de cada columna
probabilidades_normalizadas <- IARG2 / total_por_columna

epsilon <- 1e-10 # Puedes ajustar este valor según sea necesario
epsilon == 0.0000000001

# Paso 2: Aplicar la Fórmula de Entropía
# Calculamos la entropía para cada columna
entropias_columna <- sapply(probabilidades_normalizadas, function(x) {
  -sum((x + epsilon) * log2(x + epsilon))
})

EC <- as.matrix(entropias_columna)

# Paso 3: Agregar Entropías
# Tomamos el promedio de las entropías de las columnas para obtener una medida compuesta
entropia_promedio <- mean(entropias_columna)

print(paste("Entropía Promedio:", entropia_promedio))
entropia_promedio - epsilon
entropia_promedio == (entropia_promedio - epsilon)
