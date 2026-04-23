### Librerías necesarias
library(tidyverse)  # Manipulación de datos
library(tidytext)   # Análisis de texto
library(here)       # Manejo de rutas de archivos
library(tm)         # Text Mining (Document Term Matrix) 
library(jsonlite)   # Para trabajar con JSON
library(tidylo)     # Log Odds Ratio ponderado
library(scales)     # Para formatear gráficos
library(glue)       # Para strings dinámicos
# ------------------------------------------

################
# DTM Y GRAFICOS
################

# Leemos el dataset procesado
tokens <- read_rds(here("TP2", "output", "processed_text.rds"))

# Calculamos la frecuencia de los tokens
frecuencia_tokens <- tokens |>
  count(id, lemma, name = "n") |>
  arrange(id)
# Convertimos a Document-Term Matrix
matriz_dtm <- frecuencia_tokens |>
  cast_dtm(document = id, term = lemma, value = n)
# Inspeccionamos
matriz_dtm

# Palabras más frecuentes
palabras_frecuentes <- tokens |> count(lemma, sort = TRUE)
head(palabras_frecuentes, 20)

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

## GRAFICO DE BARRAS
# Graficamos la frecuencia de los términos de interés
ggplot(dtm_df, aes(x = lemma, y = frecuencia_total)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Frecuencia de términos de interés en comunicados de la OEA",
    x = "Término",
    y = "Frecuencia",
    caption = "Fuente: Comunicados de prensa de la OEA (enero-abril 2026)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#Guardamos el gráfico
ggsave(
  filename = file.path(output_dir, "frecuencia_terminos.png"),
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300
)








