library(tidyverse)
library(here)
library(tm)

message("Iniciando script de métricas y figura")

comunicados_procesados <- readRDS(here("TP2", "output", "processed_text.rds"))

# Creo directorio para el output
output_dir <- here("TP2", "output")

# Si no existe la carpeta de output, la creo en el mismo directorio
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  message("Creando carpeta output")
} else {
  message("La carpeta output ya existe")
}

# Conteo de lemas por documento
conteos_doc_lemma <- comunicados_procesados %>%
  count(id, lemma, name = "frecuencia")

# Paso a formato wide para construir la matriz DTM, donde cada fila es un documento,
# cada columna es una palabra y las celdas indican la frecuencia absoluta
# de un término específico dentro de un documento determinado.
message("Construyendo la matriz DTM")
dtm_base <- conteos_doc_lemma %>%
  pivot_wider(
    names_from = lemma,
    values_from = frecuencia,
    values_fill = 0
  )

# Elijo 5 términos relevantes dentro del contexto institucional de la OEA: 
# libertad, democrático, instituciones, protocolar y prensa. 
terminos_oea <- c("libertad", "democrático", "instituciones", "protocolar", "prensa")

if (all(terminos_oea %in% names(dtm_base))) {
  message("Los cinco términos seleccionados están presentes en la DTM")
} else {
  message("Uno o más términos seleccionados no están presentes en la DTM")
}

message("Calculando frecuencia total de los términos seleccionados")

frecuencia_5_terminos <- dtm_base %>%
  select(all_of(terminos_oea)) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(cols = everything(),
               names_to = "lemma",
               values_to = "frecuencia_total")

grafico_terminos <- ggplot(frecuencia_5_terminos,
                           aes(x = reorder(lemma, frecuencia_total),
                               y = frecuencia_total)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Frecuencia total de términos seleccionados en comunicados de la OEA",
    x = "Término",
    y = "Frecuencia total"
  )

message("Generando gráfico de barras")

if (inherits(grafico_terminos, "ggplot")) {
  message("Gráfico generado correctamente")
} else {
  else("Error al generar el gráfico")
}

message("Guardando gráfico en formato .png")

ggsave(
  filename = here("TP2", "output", "frecuencia_terminos.png"),
  plot = grafico_terminos,
  width = 8,
  height = 6
)

message("Proceso finalizado con éxito")
