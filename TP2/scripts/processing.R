### Librerías necesarias
library(tidyverse)  # Manipulación de datos
library(rvest)      # Web scraping
library(httr2)      # Requests HTTP
library(tidytext)   # Análisis de texto
library(here)       # Manejo de rutas de archivos
library(xml2)       # Manejo de HTML (guardar la página completa x ej)

library(udpipe)     # Lematización
# ---------------------------------------------------------------
###################
# LIMPIEZA DE DATOS
###################

# Creación de la crpeta output

# Leemos el dtaset anterior
data_dir <- here("TP2", "data")
comunicados_oea <- read_rds(file.path(data_dir, "comunicados_oea.rds"))

# Creamos la carpeta output
output_dir = here("TP2", "output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
} else {
  cat(
    "'output' ya existe. Se sobrescribirá el archivo.\n"
  )
}

# Combinamos título y cuerpo
comunicados_limpio <- comunicados_oea |>
  mutate(
    texto_completo = str_c(titulo, ". ", cuerpo),
    # Eliminamos retorno de carro, saltos de línea y tabulaciones
    texto_completo = str_replace_all(texto_completo, "[\\r\\n\\t]+", " "),
    # Eliminamos signos de puntuación
    texto_completo = str_replace_all(texto_completo, "[[:punct:]]", " "),
    # Eliminamos números
    texto_completo = str_replace_all(texto_completo, "[[:digit:]]", " "),
    # Eliminamos caracteres especiales
    texto_completo = str_replace_all(texto_completo, "[^[:alpha:][:space:]]", " "),
    # Eliminamos espacios múltiples
    texto_completo = str_squish(texto_completo)
  ) |>
  select(-cuerpo)

# Verificamos
comunicados_limpio |>
  select(id, titulo, texto_completo) |>
  head(2)

# LEMATIZACIÓN
# Cargar modelo de udpipe
m_es <- udpipe_download_model(language = "spanish", overwrite = FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)

# Lematizamos el texto completo
comunicados_lemas <- udpipe_annotate(
  modelo_es,
  x = comunicados_limpio$texto_completo,
  doc_id = comunicados_limpio$id
) |>
  as.data.frame() |>
  mutate(id = as.integer(doc_id)) |>
  select(id, lemma, upos)

comunicados_lemas <- comunicados_lemas |>
  filter(upos %in% c("NOUN", "VERB", "ADJ"))

# Agregamos el título
comunicados_lemas <- comunicados_lemas |>
  left_join(
    comunicados_limpio |> select(id, titulo),
    by = "id"
  )

# ELIMINAMOS STOPWORDS
install.packages("stopwords")
library(stopwords)

# Cargamos las stop words en español del paquete tidytext
data("stopwords", package = "stopwords")
stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")
stop_words <- tibble(lemma = c(stop_es, stop_en))

# Convertir a tibble
tokens <- as_tibble(comunicados_lemas) |>
  # Filtrar sustantivos, verbos y adjetivos
  filter(upos %in% c("NOUN", "ADJ", "VERB")) |>
  # Convertir los lemmas a minúsculas
  mutate(lemma = str_to_lower(lemma)) |>
  # Eliminar stopwords y palabras muy cortas
  anti_join(stop_words, by = "lemma") |>
  filter(str_length(lemma) > 2)

head(tokens)

tokens |> write_rds(file.path(output_dir, "processed_text.rds"))
