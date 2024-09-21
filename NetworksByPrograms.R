load("Results/Argentina.RData")
load("Results/Brazil.RData")
load("Results/Chile.RData")
load("Results/Colombia.RData")
load("Results/CostaRica.RData")
load("Results/Ecuador.RData")
load("Results/Mexico.RData")
load("Results/Uruguay.RData")
load("Results/Venezuela.RData")
rm(list=setdiff(ls(), c("ARGSpec",
"ARGMS",
"ARGPhD",
"BRASpec",
"BRAMS",
"BRAPhD",
"CHLSpec",
"CHLMS",
"CHLPhD",
"COLSpec",
"COLMS",
"COLPhD",
"CORISpec",
"CORIMS",
"CORIPhD",
"ECUSpec",
"ECUMS",
"ECUPhD",
"MEXSpec",
"MEXMS",
"MEXPhD",
"URUSpec",
"URUMS",
"URUPhD",
"VENSpec",
"VENMS",
"VENPhD")))
library(quanteda)
Dictionary <- dictionary(list(
  active_listening = c("escucha*", "pregunta*", "cuestiona*", "entend*", "comprend*", "silencio"),
  mathematics = c("matemática", "resolver problemas matemáticos", "cálculos", "calcular"),
  reading_comprehension = c("lectura", "leer", "oraciones", "párrafo*", "textos", "documento*"),
  science = c("ciencia*", "científic*", "método*", "resolver problemas", "investiga*"),
  speaking = c("oratoria", "comunica*"),
  writing = c("escrib*", "redact*", "escrito"),
  active_learning = c("implicaciones", "comprende", "decisión", "futur*", "nueva información"),
  critical_thinking = c("crítico", "pensamiento crítico", "lógic*", "razona*"),
  learning_strategy = c("aprend*", "enseñ*", "instrucci*"),
  monitoring = c("auto-evalu*",  "reflexi*", "desempeñ", "ejecución")
))
VEN_Spec <- tokens(VENSpec, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

VEN_MS <- tokens(VENMS, 
                 remove_numbers = TRUE, 
                 remove_punct = TRUE, 
                 remove_url = TRUE, 
                 remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

VEN_PhD <- tokens(VENPhD, 
       remove_numbers = TRUE, 
       remove_punct = TRUE, 
       remove_url = TRUE, 
       remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MatrizVESPEC <- as.matrix(t(VEN_Spec))
MatrizVEMS <- as.matrix(t(VEN_MS))
MatrizVEPHD <- as.matrix(t(VEN_PhD))


