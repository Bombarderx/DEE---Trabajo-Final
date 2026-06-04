
# Cargamos las siguientes librerías (asumimos que ya estan instaladas):
library(sf) # Para el tratamiento de objetos espaciales
library(tidyverse) # para la limpieza, transformacion y represetnacion
library(knitr) # Para el renderizado de tablas via kable()
library(tidygeocoder) # Para la geocodificacion
library(mapSpain) # Para cargar el listado de municipios de España
library(leaflet) # Para la graficación
library(RColorBrewer) # Para paletas de colores

df <- read.csv("./data/DatosEstancos.csv", skip = 2, sep = ";")


df <- df[-nrow(df),]



# Añadimos variable auxiliar con el país = "ESPAÑA"
df$Pais <- "ESPAÑA"

# Cambiamos abreviaturas por nombres completos.

abreviaturas <- c("^C\\. ", "^AV\\. ", "^PL\\. ", "^PS\\. ", "^CL\\. ", 
                  "^PZ\\. ", "^CR\\. ", "^PG\\. ", "^BO\\. ", "^TR\\. ", "^PA\\. ")

nombres <- c("C.", "AV.", "PL.", "PS.", "CL.", "PZ.", "CR.", "PG.", "BO.", "TR.", "PA.")

conteos <- sapply(abreviaturas, function(p) sum(grepl(p, df$Dirección)))
names(conteos) <- nombres

knitr::kable(data.frame(Abreviatura = names(conteos), Frecuencia = conteos), row.names = FALSE)

df$Dirección <- df$Dirección %>%
  gsub("^C\\. ", "CALLE ", .) %>%
  gsub("^AV\\. ", "AVENIDA ", .) %>%
  gsub("^PL\\. ", "PLAZA ", .) %>%
  gsub("^PS\\. ", "PASEO ", .) %>%
  gsub("^CL\\. ", "CALLE ", .) %>%
  gsub("^PZ\\. ", "PLAZA ", .) %>%
  gsub("^CR\\. ", "CARRETERA ", .) %>%
  gsub("^PG\\. ", "POLIGONO ", .) %>%
  gsub("^BO\\. ", "BARRIO ", .) %>%
  gsub("^TR\\. ", "TRAVESIA ", .) %>%
  gsub("^PA\\. ", "PASAJE ", .) %>% 
  gsub("\\. ", " ", .)  %>%  # elimina puntos intermedios
  gsub("\\.$", " ", .)   # elimina punto al final

#####

# Sacamos provincia y ccaa

provincias <- esp_get_prov() %>% 
  st_drop_geometry() %>% 
  select(cpro, ine.prov.name, nuts2.name)

df$cod_provincia <- substr(df$Estanco, 1, 2)

df <- df %>%
  left_join(provincias, by = c("cod_provincia" = "cpro"))



# Y construir la query completa:


df$query <- paste(df$Dirección, df$Municipio, df$ine.prov.name, df$nuts2.name, "España", sep = ", ")

# Comprobamos
head(df$query)

# api <- Sys.getenv("GOOGLE_API_KEY")

##### BUCLE ######

# Parámetros
tam_lote <- 500
n <- nrow(df)
lotes <- split(df, ceiling(seq_len(n) / tam_lote))

# Bucle por lotes
geo_lista <- vector("list", length(lotes))

for (i in seq_along(lotes)) {
  message("Procesando lote ", i, " de ", length(lotes), "...")
  
  tryCatch({
    geo_lista[[i]] <- geo(
      address = lotes[[i]]$query,
      method  = "google",
      full_results = TRUE  # para recuperar el campo types y demás
    )
    save(geo_lista, file = paste0("./data/geo_backup_lote_", i, ".Rdata"))
    Sys.sleep(1)
    
  }, error = function(e) {
    message("Error en lote ", i, ": ", e$message)
    geo_lista[[i]] <<- NULL
  })
}

# Unimos todos los lotes en un único dataframe
df_geocodificado <- bind_rows(geo_lista)


df_geocodificado$Estanco <- df$Estanco

save(df_geocodificado, file = "./data/geocode_final.Rdata")


load("./data/geocode_final.Rdata")



######################################
########### PVR ######################
######################################

pvr <- read.csv("./data/Listado_PVRs.csv", skip = 2, sep = ";")

pvr <- pvr[-nrow(pvr),]

names(pvr) <- c("Nombre","Dirección","Localidad","Provincia")

pvr$Dirección <- pvr$Dirección %>%
  gsub("^C\\. ", "CALLE ", .) %>%
  gsub("^AV\\. ", "AVENIDA ", .) %>%
  gsub("^PL\\. ", "PLAZA ", .) %>%
  gsub("^PS\\. ", "PASEO ", .) %>%
  gsub("^CL\\. ", "CALLE ", .) %>%
  gsub("^PZ\\. ", "PLAZA ", .) %>%
  gsub("^CR\\. ", "CARRETERA ", .) %>%
  gsub("^PG\\. ", "POLIGONO ", .) %>%
  gsub("^BO\\. ", "BARRIO ", .) %>%
  gsub("^TR\\. ", "TRAVESIA ", .) %>%
  gsub("^PA\\. ", "PASAJE ", .) %>% 
  gsub("\\. ", " ", .)  %>%  # elimina puntos intermedios
  gsub("\\.$", " ", .)   # elimina punto al final

pvr$query <- paste(pvr$Dirección, pvr$Localidad, pvr$Provincia, "España", sep = ", ")


##### BUCLE ######

# Parámetros
tam_lote <- 1000
n <- nrow(pvr)
lotes <- split(pvr, ceiling(seq_len(n) / tam_lote))

# Bucle por lotes
geo_lista <- vector("list", length(lotes))

for (i in 74:length(lotes)) {
  message("Procesando lote ", i, " de ", length(lotes), "...")
  
  tryCatch({
    geo_lista[[i]] <- geo(
      address = lotes[[i]]$query,
      method  = "arcgis",
      full_results = TRUE  # para recuperar el campo types y demás
    )
    save(geo_lista, file = paste0("./data/pvr_backup_lote_", i, ".Rdata"))
    Sys.sleep(1)
    
  }, error = function(e) {
    message("Error en lote ", i, ": ", e$message)
    geo_lista[[i]] <<- NULL
  })
}

# Unimos todos los lotes en un único dataframe
pvr_geocodificado <- bind_rows(geo_lista)

# Devolvemos nombres

pvr_geocodificado$Nombre <- pvr$Nombre

save(pvr_geocodificado, file = "./data/pvr_geocode_final.Rdata")

load("./data/pvr_geocode_final.Rdata")
