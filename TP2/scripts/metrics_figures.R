### Librerías necesarias
library(tm)         # Text Mining (Document Term Matrix) 
library(jsonlite)   # Para trabajar con JSON
library(tidylo)     # Log Odds Ratio ponderado
library(scales)     # Para formatear gráficos
library(glue)       # Para strings dinámicos
# ------------------------------------------
# a. Utilizando el archivo generado por el script anterior
#computar la Matriz de Frecuencia de Términos (DTM) para el
#corpus lematizado. A partir de esta matriz, filtrar y
#condensar la información para obtener la frecuencia total de
# 5 términos (palabras) que consideren relevantes dado el
#contexto institucional de la OEA.
# b. A partir de los resultados, generar un gráfico de barras
#utilizando ggplot2 que muestre la cantidad de apariciones
#totales de esos 5 términos seleccionados a lo largo de todos
#los comunicados. Esta figura debe guardarse en la carpeta
# /output con el nombre frecuencia_terminos.png.

# ------------------------------------------

################
# DTM Y GRAFICOS
################

# Calculamos la frecuencia de los tokens
frecuencia_tokens <- tokens |>
  count(id, lemma, name = "n") |>
  arrange(id)
# Convertimos a Document-Term Matrix
matriz_dtm <- frecuencia_tokens |>
  cast_dtm(document = id, term = lemma, value = n)
# Inspeccionamos
matriz_dtm

# términos mas frecuentes
frecuencia_tokens |>
  group_by(lemma) |>
  summarise(total = sum(n)) |>
  arrange(desc(total)) |>
  head(20)

# Filtramos palabras clave 
terminos_de_interes <- c("misión", "derecho", "democrático", "elección", "constitucional")
matriz_dtm_de_interes <- matriz_dtm[, colnames(matriz_dtm) %in% terminos_de_interes]
# Primeras 20 líneas de la matriz
as.matrix(matriz_dtm_de_interes)[1:20, ]

## Condensamos para obtener un conteo total
dtm_df <- as.data.frame(as.matrix(matriz_dtm_de_interes)) |>
  rownames_to_column(var = "id") |>
  pivot_longer(-id, names_to = "lemma", values_to = "n") |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n))











