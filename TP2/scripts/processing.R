library(tidyverse)
library(here)
library(udpipe)
library(stopwords)

tabla <- readRDS(here("TP2", "data", "tabla_comunicados.rds"))

# Creo directorio para el output
output_dir <- here("TP2", "output")

# Si no existe la carpeta de output, la creo en el mismo directorio
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  message("Creando carpeta output")
} else {
  message("La carpeta output ya existe")
}

################## Limpieza ##################
tabla_limpia <- tabla %>%
  mutate(
    cuerpo_limpio = cuerpo %>%
      str_replace_all("[[:punct:]]", " ") %>% # Reemplazo la puntuación por espacios
      str_replace_all("[[:digit:]]", " ") %>% # Reemplazo los dígitos por espacios
      str_replace_all("[^[:alnum:]áéíóúüñÁÉÍÓÚÜÑ\\s]", " ") %>% # Reemplazo caracteres especiales por espacios
      str_squish() # Elimina espacios al principio y el final, elimina múltiples espacios
  )

################ Lematización y Tokenización #################
# Descarga y carga el modelo de lematización en español
m_es <- udpipe_download_model(language = "spanish", overwrite = FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)

# Lematización y tokenización
anotaciones <- udpipe_annotate(
  object = modelo_es, # Modelo de lenguaje en español
  x = tabla_limpia$cuerpo_limpio, # El texto que quiero analizar
  doc_id = tabla_limpia$id) # Asignar a cada palabra el id del comunicado
  
comunicados_lemas <- as_tibble(anotaciones) %>% # Convierto a tabla para facilitar interpretación
  select(id = doc_id, lemma, upos) # Me quedo con lo relevante: el id, la forma base de la palabra (lemma) y el tipo de palabra (upos)

################## Stopwords ##########################
stop_es <- stopwords::stopwords("es") # Defino las stopwords en español
stop_en <- stopwords::stopwords("en") # Defino las stopwords en inglés

stop_words <- tibble(lemma = c(stop_es, stop_en)) # Las guardo en una tabla dentro de la variable lemma

# Las elimino con un anti join
comunicados_procesados <- comunicados_lemas %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ")) %>% # Me quedo solo con estos tipos de palabras
  mutate(lemma = str_to_lower(lemma)) %>% # Convierto en minúscula a las palabras base
  anti_join(stop_words, by = "lemma") # Saco las que aparecen en la key "lemma" dentro de la tabla de stop words

saveRDS(comunicados_procesados,
        file = here("TP2", "output", "processed_text.rds"))
