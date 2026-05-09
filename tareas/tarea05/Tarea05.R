
# Cargamos librerias
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(tidyr)

# Cargamos los datos, pero primero debemos armar un data set diario con los datos del clima por hora

# para ello hacemos los siguientes pasos

# Paquete
install.packages("httr")

library(httr)

# Rango completo
inicio <- as.Date("2019-01-01")
fin    <- as.Date("2025-12-31")

fechas <- seq(inicio, fin, by = "day")

# Carpeta destino
dir.create("smn_data", showWarnings = FALSE)

# Loop de descarga
for (f in fechas) {
  
  fecha_str <- format(as.Date(f), "%Y%m%d")
  
  url <- paste0(
    "https://ssl.smn.gob.ar/dpd/descarga_opendata.php?file=observaciones/datohorario",
    fecha_str,
    ".txt"
  )
  
  archivo <- paste0("smn_data/datohorario_", fecha_str, ".txt")
  
  # Evitar descargar si ya existe
  if (file.exists(archivo)) {
    cat("Ya existe:", fecha_str, "\n")
    next
  }
  
  res <- GET(url)
  
  if (status_code(res) == 200) {
    writeBin(content(res, "raw"), archivo)
    cat("✔ Descargado:", fecha_str, "\n")
  } else {
    cat("✖ Error:", fecha_str, "\n")
  }
}

class(fechas)

# Cargamos los data sets 

list.files()
 
 read.csv2(file="201912-informe-ministerio-actualizado-dic-final.csv")
 read.csv2(file="202012-informe-ministerio-actualizado-dic-final.csv") 
 read.csv2(file="202112-informe-ministerio-actualizado-dic-final.csv")
 read.csv2(file="202212-informe-ministerio-actualizado-dic-final.csv")
 read.csv2(file="202312-informe-ministerio-actualizado-dic.csv")
 read.csv2(file="202412-informe-ministerio-actualizado-dic-final.csv") 
 read.csv2(file="202512-informe-ministerio-actualizado-dic-final.csv") 

 
# Unificamos los smn_data en un solo data set
 
 install.packages("data.table")
 library(data.table)
 
 list.files("smn_data")[1:10]

 files <- list.files("smn_data", pattern = "\\.txt$", full.names = TRUE)
 
 length(files) 
 smn_list <- lapply(files, function(f) {
   tryCatch({
     fread(
       f,
       skip = 2,
       header = FALSE,
       fill = TRUE,
       encoding = "Latin-1"
     )
   }, error = function(e) NULL)
 }) 

 smn_all <- rbindlist(smn_list, fill = TRUE) 

 smn_all[, nombre := do.call(paste, c(.SD, sep = " ")), 
         .SDcols = 8:ncol(smn_all)]
 
 smn_arg <- smn_all[, 1:8]

 setnames(smn_arg, c("fecha","hora","temp","hum","pnm","dd","ff","nombre")) 

 smn_arg[, fecha := sprintf("%08d", as.integer(fecha))]
 smn_arg[, fecha := as.Date(fecha, format = "%d%m%Y")] 

 smn_arg[, hora := as.integer(hora)]
 smn_arg[, temp := as.numeric(temp)]
 smn_arg[, hum  := as.numeric(hum)]
 smn_arg[, pnm  := as.numeric(pnm)]
 smn_arg[, dd   := as.numeric(dd)]
 smn_arg[, ff   := as.numeric(ff)] 

 head(smn_arg)
 str(smn_arg)
  
 glimpse(smn_arg)

 # logramos unificar los datos del clima, ahora debemos unificar los datos de vuelos, para ello hacemos lo siguientes pasos
 
 Vuelos2025 <- read.csv2(file="202512-informe-ministerio-actualizado-dic-final.csv")
 Vuelos2024 <- read.csv2(file="202412-informe-ministerio-actualizado-dic-final.csv") 
 Vuelos2023 <- read.csv2(file="202312-informe-ministerio-actualizado-dic.csv")
 Vuelos2022 <- read.csv2(file="202212-informe-ministerio-actualizado-dic-final.csv")
 Vuelos2021 <- read.csv2(file="202112-informe-ministerio-actualizado-dic-final.csv")
 Vuelos2020 <- read.csv2(file="202012-informe-ministerio-actualizado-dic-final.csv")
 Vuelos2019 <- read.csv2(file="201912-informe-ministerio-actualizado-dic-final.csv")
 
 glimpse(Vuelos2025)
 glimpse(Vuelos2024)
 glimpse(Vuelos2023)
 glimpse(Vuelos2022)
 glimpse(Vuelos2021)
 glimpse(Vuelos2020)
 glimpse(Vuelos2019)
 
 # Unimos todos los data sets de vuelos en uno solo
 
 Vuelos_arg <- (Vuelos2019 %>%
   bind_rows(Vuelos2020) %>%
   bind_rows(Vuelos2021) %>%
   bind_rows(Vuelos2022) %>%
   bind_rows(Vuelos2023) %>%
   bind_rows(Vuelos2024) %>%
   bind_rows(Vuelos2025)
 )
# me tiro error, corregimos 
 Vuelos2019 <- Vuelos2019 |> 
   mutate(
     Aeronave=as.character(Aeronave)
   )

 glimpse(Vuelos_arg)
 
 # Transformamos
 
  Vuelos_arg <- Vuelos_arg |> 
   mutate(
     hora = as.integer(sub(":.*", "", Hora.UTC)),
     Fecha.UTC = as.Date(Fecha.UTC, format = "%d/%m/%Y"),
     Aeropuerto = factor(Aeropuerto),
     Clasificación.Vuelo = factor(`Clasificación.Vuelo`),
     Aeronave = factor(Aeronave),
     Tipo.de.Movimiento = factor(`Tipo.de.Movimiento`),
     Origen.Destino = factor(`Origen...Destino`),
     Aerolinea.Nombre = factor(`Aerolinea.Nombre`),
     )
    
  Vuelos_arg <- Vuelos_arg |> 
   select(-Hora.UTC)
  
  
  Vuelos_arg <- Vuelos_arg |> 
   select(-Calidad.dato)
     
 # chequqeamos 
  glimpse(Vuelos_arg)

  # Nos falto
  
  Vuelos_arg <- Vuelos_arg |> 
   mutate(
     tipo_vuelo = factor(`Clase.de.Vuelo..todos.los.vuelos.`)
   ) 
  Vuelos_arg <- Vuelos_arg |> 
   select(-`Clase.de.Vuelo..todos.los.vuelos.`)
  
  # Probamos Join 
  
 Vuelos_clima <- Vuelos_arg |> 
   left_join(smn_arg, by = c("Fecha.UTC" = "fecha", "hora" = "hora", "Aeropuerto" = "nombre"))
glimpse(Vuelos_clima) 


unique(Vuelos_arg$Aeropuerto)
unique(smn_arg$nombre)

# Tenemos un problema  con los nombres de las bases del data de smn_arg, hay que proponer una correcion 

library(dplyr)
library(stringr)

smn_arg <- smn_arg |>
  mutate(
    Aeropuerto = case_when(
      str_detect(nombre, "EZEIZA") ~ "EZE",
      str_detect(nombre, "AEROPARQUE|AERO") ~ "AER",
      str_detect(nombre, "CORDOBA") ~ "CBA",
      str_detect(nombre, "MENDOZA") ~ "MDZ",
      str_detect(nombre, "TUCUMAN") ~ "TUC",
      str_detect(nombre, "IGUAZU") ~ "IGU",
      str_detect(nombre, "BARILOCHE") ~ "BAR",
      str_detect(nombre, "SALTA") ~ "SAL",
      str_detect(nombre, "NEUQUEN") ~ "NEU",
      TRUE ~ NA_character_
    )
  )

# Segunda prueba del Join, basada en la eleccion arbitraria de subjetivizar que dichas bases climaticas son las mas relevantes para el estudio

Vuelos_clima <- Vuelos_arg |>
  left_join(smn_arg, by = c("Fecha.UTC" = "fecha", "hora", "Aeropuerto"))
glimpse(Vuelos_clima)

# El Join funciono, pero hay relaciones multiples ya sea por que hay mas de un registro de clima por hora o por que hay mas de un vuelo por hora, lo que nos da una cantidad de filas mucho mayor a la cantidad de vuelos, lo que dificulta el analisis, para solucionar esto debemos hacer un resumen de los datos del clima por hora y aeropuerto, para luego hacer el Join con los datos de vuelos, esto nos dara una cantidad de filas igual a la cantidad de vuelos, lo que facilita el analisis.
# Hacemos una verificacion

smn_arg |>
  count(fecha, hora, Aeropuerto) |>
  filter(n > 1)

# Resulta que dichas relaciones multiples es por los N/A de las bases que decidimos ignorar

# Hacemos algo de limpieza

Vuelos_clima <- Vuelos_clima |>
  filter(Aeropuerto %in% c("EZE", "AER", "CBA", "MDZ", "TUC"))

Vuelos_clima <- Vuelos_clima |>
  filter(!is.na(temp))

# Creo obtuvimos un dataset lo bastante limpio para hacer un analisis exploratorio de los datos, probemos

# destinos con mayor recepciones , durante el periodo 2019-2025, con muestras del clima
Vuelos_clima |>
  filter(Tipo.de.Movimiento == "ATERRIZAJE") |>
  group_by(Origen.Destino) |>
  summarise(total_pasajeros = sum(Pasajeros, na.rm = TRUE)) |>
  arrange(desc(total_pasajeros))

# Destinos con mayor recepciones durante el periodo 2019-2025

Vuelos_arg |>
  filter(Tipo.de.Movimiento == "ATERRIZAJE") |>
  group_by(Origen.Destino) |>
  summarise(total_pasajeros = sum(Pasajeros, na.rm = TRUE)) |>
  arrange(desc(total_pasajeros))

# El codigo primero que chequeamos es con la submuestra de vuelos con el clima, lo que nos da menos cantidad de aterrizajes, la segunda parte del directo del dataset unificado de vuelos por ende hay mayor aterrizajes

# Mayores destinos internacionales desde argentina 
Vuelos_arg |>
  filter(
    Tipo.de.Movimiento == "ATERRIZAJE",
    Clasificación.Vuelo == "Internacional"
  ) |>
  group_by(Origen.Destino) |>
  summarise(total_pasajeros = sum(Pasajeros, na.rm = TRUE)) |>
  arrange(desc(total_pasajeros))

# Los ganadores son destinos sudamericanos, Santiago de Chile (Chile), Lima (Peru), San Pablo (Brasil)
# Pero no siempre esto quiere decir que son los destinos son definitivos, porque sucede que sean escala a otros destinos, por ejemplo Santiago de Chile es una escala a otros destinos internacionales, lo que hace que tenga una gran cantidad de pasajeros, pero no necesariamente es un destino final para muchos pasajeros, lo mismo sucede con Lima y San Pablo, por lo que es importante tener en cuenta esto al analizar los datos.

# Vamos a sintetizarlo en un grafico de barras de vuelos internacionales por año

# Hacemos un Top 5 internacional
library(stringr)

top5_anual <- Vuelos_arg |>
  mutate(
    anio = year(Fecha.UTC),
    tipo = str_to_lower(str_trim(Tipo.de.Movimiento)),
    clasif = str_to_lower(str_trim(Clasificación.Vuelo))
  ) |>
  filter(
    tipo == "aterrizaje",
    clasif == "internacional"
  ) |>
  group_by(anio, Origen.Destino) |>
  summarise(pasajeros = sum(Pasajeros, na.rm = TRUE), .groups = "drop") |>
  group_by(anio) |>
  slice_max(pasajeros, n = 5) |>
  ungroup()

top5_anual |> count(anio)

# grafico 
ggplot(top5_anual, aes(x = factor(anio), y = pasajeros , fill = Origen.Destino)) +
  geom_col(position = "dodge") +
  
  scale_fill_manual(values = c(
    "KMIA" = "red",
    "LEMD" = "gold",
    "MPTO" = "green",
    "SBGL" = "darkgreen",
    "SBGR" = "blue",
    "SCEL" = "cyan",
    "SPJC" = "orange"
  )) +
  
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K")) +
  labs(
    title = "Top 5 destinos internacionales por año",
    x = "Año",
    y = "Pasajeros (en miles)",
    fill = "Destino"
  ) +
  theme_minimal()

# Ahora la tarea nos exige 

#  ¿Qué se observa en la pandemia?¿cuando se recuperan los flujos?¿se puede apreciar diferencias en los patrones?

Vuelos_arg <- Vuelos_arg |>
  mutate(
    año = year(Fecha.UTC),
    cantidad_vuelos = 1
  )

ggplot(Vuelos_arg, aes(x = factor(año), y = cantidad_vuelos)) +
  geom_col(fill="orange") +
  labs(
    title = "Cantidad de vuelos por año",
    x = "Año",
    y = "Cantidad de vuelos"
  ) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))+ 
  theme_minimal()

# El grafico nos muestra que en pandemia la cantidad de vuelos se redujo drasticamente alrededor de un 65%, y no recupero el flujo hasta mediados del 2025
# Es dificil obtener un patron fijo de porque el gradualismo en la recuperacion de vuelos, solo observando las cantidades, pero se se puede entender como dificultades en la confianza de los consumidores, posibles miedos sanitarios, contraccion del ingreso, digamos hubo contraccion del pbi en dicha epoca, multiples etc

# otros experimentos 

# Del data_set con clima, evaluamos

# ¿Dias de vuelo con lluvia vs un dia sin lluvia? en promedio
Vuelos_clima |>
  mutate(
    lluvia = ifelse(temp < 15 & hum > 80, "Si", "No")
  ) |>
  group_by(Fecha.UTC) |>
  summarise(
    lluvia_dia = ifelse(any(lluvia == "Si"), "Si", "No"),
    vuelos_dia = n()
  ) |>
  group_by(lluvia_dia) |>
  summarise(
    total_dias = n(),
    promedio_vuelos = mean(vuelos_dia),
    total_vuelos = sum(vuelos_dia)
  )
 
# A tibble: 2 × 4
#lluvia_dia total_dias promedio_vuelos total_vuelos
#chr>           <int>           <dbl>        <int>
  #1 No               1267            386.       489272
   #2 Si               1055            481.       507858
  
  # en promedio hubo mas vuelos los dias clasificados como lluvia, que parte de dias con temperatura menor a 15 y humedad mayor a 80, un total de 95 vuelos mas
  
  # ¿Dias de vuelo con viento fuerte vs sin viento fuerte? en promedio
Vuelos_clima |>
  mutate(
    viento_fuerte = ifelse(!is.na(ff) & ff > 35, "Si", "No")
  ) |>
  group_by(Fecha.UTC) |>
  summarise(
    viento_dia = ifelse(any(viento_fuerte == "Si"), "Si", "No"),
    vuelos_dia = n()
  ) |>
  group_by(viento_dia) |>
  summarise(
    total_dias = n(),
    promedio_vuelos = mean(vuelos_dia),
    total_vuelos = sum(vuelos_dia)
  )
  
#A tibble: 2 × 4
#viento_dia total_dias promedio_vuelos total_vuelos
#chr>           <int>           <dbl>        <int>
# 1 No               2158            428.       924276
# 2 Si                164            444.        72854

#No se observa una relación negativa clara entre viento fuerte y volumen diario de vuelos. Incluso, los días con viento fuerte presentan un promedio ligeramente superior de operaciones, aunque la diferencia es pequeña.

#Este análisis evalúa cantidad total de vuelos, no cancelaciones o demoras específicas. Por lo tanto, un día con viento fuerte puede haber tenido vuelos igualmente, aunque posiblemente con retrasos o cambios operativos.


# Ultima del analisis ¿ Aquellos destinos que son elegidos  a pesar de las clasificaciones de lluvia y viento fuerte?
Vuelos_clima |>
  mutate(
    lluvia = ifelse(temp < 15 & hum > 80, "Si", "No"),
    viento_fuerte = ifelse(!is.na(ff) & ff > 35, "Si", "No")
  ) |>
  group_by(Origen.Destino) |>
  summarise(
    total_vuelos = n(),
    vuelos_lluvia = sum(lluvia == "Si"),
    vuelos_viento = sum(viento_fuerte == "Si"),
    prop_lluvia = vuelos_lluvia / total_vuelos,
    prop_viento = vuelos_viento / total_vuelos
  ) |>
  arrange(desc(total_vuelos)) |>
  head(10)

#Los destinos más demandados mantienen un volumen elevado de vuelos incluso bajo condiciones climáticas adversas. Las proporciones de vuelos realizados con lluvia oscilan aproximadamente entre el 9% y el 13%, mientras que los vuelos con viento fuerte representan menos del 1% en la mayoría de los destinos.

#Separando los 5 destinos mas elegidos por los criterios antes dados
 top5destinosadveros <- Vuelos_clima |>
   mutate(
     lluvia = ifelse(temp < 15 & hum > 80, "Si", "No"),
     viento_fuerte = ifelse(!is.na(ff) & ff > 35, "Si", "No")
   ) |>
   group_by(Origen.Destino) |>
   summarise(
     total_vuelos = n(),
     vuelos_lluvia = sum(lluvia == "Si"),
     vuelos_viento = sum(viento_fuerte == "Si"),
     prop_lluvia = vuelos_lluvia / total_vuelos,
     prop_viento = vuelos_viento / total_vuelos
   ) |>
   arrange(desc(total_vuelos)) |>
   head(5)

#graficamente
 top5destinosadveros |>
   select(Origen.Destino, prop_lluvia, prop_viento) |>
   pivot_longer(
     cols = c(prop_lluvia, prop_viento),
     names_to = "condicion",
     values_to = "proporcion"
   ) |>
   ggplot(aes(x = Origen.Destino,
              y = proporcion * 100,
              fill = condicion)) +
   geom_col(position = "dodge") +
   labs(
     title = "Condiciones climáticas adversas por destino",
     x = "Destino",
     y = "% de vuelos"
   ) +
   theme_minimal()
 
 top5destinosadveros |>
   select(Origen.Destino, prop_lluvia, prop_viento) |>
   pivot_longer(
     cols = c(prop_lluvia, prop_viento),
     names_to = "condicion",
     values_to = "proporcion"
   ) |>
   ggplot(aes(x = Origen.Destino,
              y = proporcion * 100,
              fill = condicion)) +
   geom_col(position = "dodge") +
   labs(
     title = "Condiciones climáticas adversas por destino",
     x = "Destino",
     y = "% de vuelos"
   ) +
   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
   theme_minimal()
