# Librerías
library(rvest)
library(tidyverse)
library(here)
library(xml2) # Para write_html

# Creo directorio para los datos
data_dir <- here("TP2", "data")

# Si no existe la carpeta de data, la creo en el mismo directorio
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
  message("Creando carpeta data")
} else {
  message("La carpeta data ya existe")
}

# Creo tres variables: (1) base del URL del sitio web de la OEA, (2) mes y (3) año
base_url <- "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp"
anio <- 2026
meses <- 1:4

# Creo la base de los links de los comunicados de cada mes, que luego se usará
# para concatenarlo con el id específico de cada comunicado
base_url_root <- "https://www.oas.org/es/centro_noticias/"

# Creo una función para extraer el comunicado llamada extraer_comunicado. 
# La función recibe como argumento un link de un comunicado de la OEA y un
# título previamente extraído. Su output es una tabla que incluye el id (link),
# el título del comunicado y el cuerpo del comunicado.
extraer_comunicado <- function(link, titulo) {
  pagina_individual <- read_html(paste0(base_url_root, link)) # Guarda el contenido del HTML del link provisto
  cuerpo <- pagina_individual %>%
    html_elements("p:nth-child(5)") %>% # Guarda el contenido del cuerpo del HTML del link provisto
    html_text2()
  tibble( 
    id = paste0(base_url_root, link), 
    titulo = titulo,
    cuerpo = cuerpo
  )
}

# Inicializo tabla_final como un tibble vacío para después sumarle las observaciones
# de la tabla_mes
tabla_final <- tibble()

# Creo un loop que guarda en el objeto XX el link de diferentes meses, el cuerpo
# de cada comunicado, el título de cada comunicado y el link de cada comunicado. 
# Además, contiene un loop que recorre todos los links y extrae el link y el título 
# correspondiente a cada comunicado y las suma a la tabla_mes, para luego unirla
# dentro de la tabla_final
for (mes in meses) {
  url_mes <- paste0(base_url, "?nMes=", mes, "&nAnio=", anio) # Extrae el URL del mes
  pagina_mes <- read_html(url_mes) # Guarda el HTML del URL del mes
  ruta_html <- file.path(data_dir, paste0("mes_", mes, ".html")) # Crea la ruta para el HTML
  write_html(pagina_mes, ruta_html) # Escribe el HTML
  titulos_mes <- pagina_mes %>% # Guarda los títulos de cada comunicado de la pagina del mes
    html_elements(".itemmenulink") %>%
    html_text2()
  links_mes <- pagina_mes %>% # Guarda los links de cada comunicado de la pagina del mes
    html_elements(".itemmenulink") %>%
    html_attr("href")
  tabla_mes <- tibble() # Inicializa la tabla_mes que va a contener el id (link), el título del comunicado y el cuerpo del comunicado
  for (i in seq_along(links_mes)) { # Abre otro loop para cada uno de los comunicados del mes
    fila <- extraer_comunicado( # Utiliza la función previamente creada, extraer_comunicado
      link = links_mes[i], # Extrae el link de cada uno de los comunicados del mes
      titulo = titulos_mes[i] # Extrae el título de cada uno de los comunicados del mes
    ) 
    tabla_mes <- bind_rows(tabla_mes, fila) # Agrega a tabla_mes la nueva observación
  }
  tabla_final <- bind_rows(tabla_final, tabla_mes) # Agrega a tabla_final las observaciones de la tabla_mes
}


saveRDS(tabla_final, file = file.path(data_dir, "tabla_comunicados.rds"))
