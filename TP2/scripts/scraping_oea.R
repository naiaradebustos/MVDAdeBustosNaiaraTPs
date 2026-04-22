### Librerías necesarias
library(tidyverse)  # Manipulación de datos
library(rvest)      # Web scraping
library(httr2)      # Requests HTTP
library(tidytext)   # Análisis de texto
library(robotstxt)  # Verificar permisos de scraping
library(here)       # Manejo de rutas de archivos
library(xml2)       # Manejo de HTML (guardar la página completa x ej)
# ---------------------------------------------------------------

############### 
# WEB SCRAPING
###############

# URL de la página de noticias sobre Paz y Seguridad de la ONU en español

urls_meses <- c(
  "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=1&nAnio=2026",
  "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=2&nAnio=2026",
  "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=3&nAnio=2026",
  "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=4&nAnio=2026"
)
nombres_meses <- c("enero", "febrero", "marzo", "abril")

# Descargamos el HTML de cada mes
pagina_html_enero <- read_html(urls_meses[1])
pagina_html_febrero <- read_html(urls_meses[2])
pagina_html_marzo <- read_html(urls_meses[3])
pagina_html_abril <- read_html(urls_meses[4])

# Guardamos el html y registramos cuándo la descargamos
data_dir = here("TP2", "data")

# Se crea la carpeta data
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
} else {
  cat(
    "'data' ya existe. Se sobrescribirá el archivo de la página HTML.\n"
  )
}

# Guardamos en formato rds y html con registro de fecha de descarga
# -- enero
attr(pagina_html_enero, "fecha_descarga") <- Sys.time()
pagina_html_enero |> write_rds(file.path(data_dir, "comunicados_oea_enero.rds"))
write_html(pagina_html_enero, file = file.path(data_dir, "comunicados_oea_enero.html"))
# -- febrero
attr(pagina_html_febrero, "fecha_descarga") <- Sys.time()
pagina_html_febrero |> write_rds(file.path(data_dir, "comunicados_oea_febrero.rds"))
write_html(pagina_html_febrero, file = file.path(data_dir, "comunicados_oea_febrero.html"))
# -- marzo
attr(pagina_html_marzo, "fecha_descarga") <- Sys.time()
pagina_html_marzo |> write_rds(file.path(data_dir, "comunicados_oea_marzo.rds"))
write_html(pagina_html_marzo, file = file.path(data_dir, "comunicados_oea_marzo.html"))
# -- abril
attr(pagina_html_abril, "fecha_descarga") <- Sys.time()
pagina_html_abril |> write_rds(file.path(data_dir, "comunicados_oea_abril.rds"))
write_html(pagina_html_abril, file = file.path(data_dir, "comunicados_oea_abril.html"))


## EXTRAEMOS LO QUE NOS INTRESA
# Extraemos los nodos de los títulos de los comunicados de prensa para cada mes
# -- enero
nodos_titulos_enero <- pagina_html_enero|>
  html_elements(".itemmenulink")
# -- febrero
nodos_titulos_febrero <- pagina_html_febrero |>
  html_elements(".itemmenulink")
# -- marzo
nodos_titulos_marzo <- pagina_html_marzo |>
  html_elements(".itemmenulink")
# -- abril
nodos_titulos_abril <- pagina_html_abril |>
  html_elements(".itemmenulink")

# Nos quedamos con el texto de cada uno
# -- enero 
titulos_enero <- nodos_titulos_enero |>
  html_text2() |> # Extraemos el texto limpio de cada nodo
  str_trim()  # Limpiamos espacios en blanco
# -- febrero
titulos_febrero <- nodos_titulos_febrero |>
  html_text2() |>
  str_trim()
# -- marzo
titulos_marzo <- nodos_titulos_marzo |>
  html_text2() |>
  str_trim()
# -- abril
titulos_abril <- nodos_titulos_abril |>
  html_text2() |>
  str_trim()

# Sus urls
# -- enero
urls_enero <- nodos_titulos_enero |>
  html_attr("href")
# -- febrero
urls_febrero <- nodos_titulos_febrero |>
  html_attr("href")
# -- marzo
urls_marzo <- nodos_titulos_marzo |>
  html_attr("href")
# -- abril
urls_abril <- nodos_titulos_abril |>
  html_attr("href")


## CREANDO EL DATASET
# -- enero
comunicados_enero <- tibble(
  titulo = titulos_enero,
  url = urls_enero
)
# -- febrero
comunicados_febrero <- tibble(
  titulo = titulos_febrero,
  url = urls_febrero
)
# -- marzo
comunicados_marzo <- tibble(
  titulo = titulos_marzo,
  url = urls_marzo
)
# -- abril
comunicados_abril <- tibble(
  titulo = titulos_abril,
  url = urls_abril
)

# SISTEMATIZAMOS

# Función para scrapear una página de comunicados de la OEA
scrapear_oea <- function(url) {
  
  # Pausa para no sobrecargar el servidor
  Sys.sleep(3)
 
  # Leemos la página
  html <- read_html(url)
  
  # Extraemos los nodos de los títulos
  nodos_titulos <- html |>
    html_elements(".itemmenulink")
  
  titulos <- nodos_titulos |>
    html_text2() |>
    str_trim()
  
  urls <- nodos_titulos |>
    html_attr("href") |>
    
  # Retornamos un tibble
  tibble(
    titulo = titulos,
    url = urls
  )
}

# Aplicamos la función a cada mes 
comunicados_enero <- scrapear_oea(urls_meses[1])
comunicados_febrero <- scrapear_oea(urls_meses[2])
comunicados_marzo <- scrapear_oea(urls_meses[3])
comunicados_abril <- scrapear_oea(urls_meses[4])

## Verificamos que la función funciona
scrapear_oea("https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=1&nAnio=2026")

## funciona :)

# ITERAMOS
scrapear_n_oea <- function() {
  
  # Scrapeamos el primer mes
  comunicados <- scrapear_oea(urls_meses[1])
  # Mensaje de progreso
  mensaje <- paste("\nMes", 1, "de", 4, "scrapeado correctamente. Total comunicados hasta ahora:", nrow(comunicados))
  message(mensaje)
  # Iteramos sobre los meses restantes
  for (i in 2:4) {
    Sys.sleep(1)
    comunicados_i <- scrapear_oea(urls_meses[i])
    # Concatenamos los resultados
    comunicados <- bind_rows(comunicados, comunicados_i)
    # Mensaje de progreso
    mensaje <- paste("\nMes", i, "de", 4, "scrapeado correctamente. Total comunicados hasta ahora:", nrow(comunicados))
    message(mensaje)
  }
  # Agregamos el id
  comunicados <- comunicados |> mutate(id = row_number())
 # Devolvemos el tibble completo
  return(comunicados)
}

comunicados_oea <- scrapear_n_oea()

# Arreglamos las URLs relativas
comunicados_oea <- comunicados_oea |>
  mutate(url = paste0("https://www.oas.org/es/centro_noticias/", url))

# Guardamos el dataset en disco
attr(comunicados_oea, "fecha_descarga") <- Sys.time()
write_rds(comunicados_oea, file.path(data_dir, "comunicados_oea.rds"))


# FUNCIÓN PARA EXTRAER EL CUERPO 
extraer_cuerpo_comunicado <- function(url) {
  Sys.sleep(3)
  html_comunicado <- read_html(url)
 # selector: #rightmaincol 
  cuerpo <- html_comunicado |> 
    html_elements("#rightmaincol") |> # paa el contenido
    html_elements("p") |> # para extraer parrafos
    html_text2() |>
    str_trim()
 # Concatenamos todos los parrafos
  cuerpo <- str_c(cuerpo, collapse = " ")
 # Eliminamos caracteres
  cuerpo <- str_replace_all(cuerpo, "[\\r\\n\\t]+", " ")
 # Eliminamos todo tipo de comillas
  cuerpo <- str_replace_all(cuerpo, "[\\\"'“”‘’«»`´%()]", "")
 # Eliminamos espacios multiples
  cuerpo <- str_squish(cuerpo)
  
  return(cuerpo)
}

# La probamos con un comunicado
extraer_cuerpo_comunicado(comunicados_oea$url[1])

# funciona


# BASE DE DATOS + CUERPOS
# Creamos otra base de datos para almacenar los cuerpos
cuerpos_comunicados_oea <- comunicados_oea |>
  select(id, url) |>
  mutate(cuerpo = map_chr(url, extraer_cuerpo_comunicado)) |>
  select(-url)

# Guardamos esta tabla
attr(cuerpos_comunicados_oea, "fecha_descarga") <- comunicados_oea |>
  attr("fecha_descarga")

cuerpos_comunicados_oea |> write_rds(
  file.path(data_dir, "cuerpos_comunicados_oea.rds")
)

# Metemos todo en la misma tabla 
comunicados_oea <- comunicados_oea |> left_join(cuerpos_comunicados_oea, by = "id")

# Ordenamos 
comunicados_oea <- comunicados_oea |> select(id, titulo, cuerpo)

comunicados_oea |> write_rds(file.path(data_dir, "comunicados_oea.rds"))
