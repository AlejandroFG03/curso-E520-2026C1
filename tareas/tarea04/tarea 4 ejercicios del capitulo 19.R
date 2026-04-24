library(nycflights13)
 library(tidyverse)
# ejercicios 19.2.4

 #ejercicio 1  ¿Cuál es la relación y cómo debería aparecer en el diagrama?weatherairports

origin + time_hour
weather$origin → airports$faa


#ejercicio 2 weather solo contiene información sobre los tres aeropuertos de origen en Nueva York. Si contuviera registros meteorológicos de todos los aeropuertos de EE. UU., ¿qué conexión adicional haría?flights

flights |> 
  left_join(weather, join_by(year, month, day, hour, dest == origin))

#ejercicio 3 Las variables , , , , y casi forman una clave compuesta para , pero hay una hora que tiene observaciones duplicadas. ¿Puedes averiguar qué tiene de especial esa hora?yearmonthdayhouroriginweather
weather |> 
  count(year, month, day, hour, origin) |> 
  filter(n > 1)

#ejercicio 4 Sabemos que algunos días del año son especiales y menos gente de lo habitual vuela en ellos (por ejemplo, Nochebuena y el día de Navidad). ¿Cómo podrías representar esos datos como un data frame? ¿Cuál sería la clave primaria? ¿Cómo se conectaría con los dataframes existentes?
special_days <- tibble(
  year = c(2013, 2013, 2014, 2014),
  month = c(12, 12, 12, 12),
  day = c(24, 25, 24, 25),
  holiday_name = c("Christmas Eve", "Christmas Day", "Christmas Eve", "Christmas Day")
)
flights |> 
  left_join(special_days, join_by(year, month, day))

# Ejercicios 19.3.4

flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier, arr_delay)

 # ejercicio 1 Busca las 48 horas (a lo largo de todo el año) que tienen los mayores retrasos. Haz contraste con los datos. ¿Ves algún patrón?weather

 # El patron que observamos es que aquellos dias donde el clima no es "normal" es decir despejado y soleado, por problemas de visibilidad, vientos,etc, hay una correlacion con los retrasos en los vuelos 
delay_hour <- flights2 |>
  group_by(time_hour) |>
  summarize(
    mean_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(mean_delay))

top48 <- delay_hour |> slice_head(n = 48)

top48 |> mutate(date = as.Date(time_hour))

flights2 |>
  semi_join(top48, by = "time_hour") |>
  count(origin, sort = TRUE)

delay_hour <- flights |>
  group_by(time_hour, origin) |>
  summarize(
    mean_delay = mean(arr_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

delay_weather <- delay_hour |>
  left_join(weather, by = c("time_hour", "origin"))

top48_weather <- delay_weather |>
  arrange(desc(mean_delay)) |>
  slice_head(n = 48)

top48_weather |> arrange(visib) |> select(time_hour, origin, mean_delay, visib)

# 2 Imagina que has encontrado los 10 destinos más populares usando este código:

top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)
# ¿Cómo puedes encontrar todos los vuelos a esos destinos?

flights_top <- flights2 |>
  semi_join(top_dest, by = "dest")

flights_top

# 3 ¿Cada vuelo de salida tiene datos meteorológicos correspondientes para esa hora?

flights_weather_missing <- flights |>
  anti_join(weather, by = c("time_hour", "origin"))
flights_weather_missing
 
# 4 ¿Qué tienen en común los números de cola que no tienen un registro coincidente? (Pista: una variable explica el ~90% de los problemas.)planes

missing_planes <- flights |>
  anti_join(planes, by = "tailnum")

missing_planes

missing_planes |> count(year)

# Hay datos que no asignados en planes, esto puede ser devido a que no se registraron o a que el numero de cola es nuevo y no se ha actualizado la base de datos de planes. La variable que explica el 90% de los problemas es el año, ya que la mayoría de los aviones sin registro en planes son de años recientes, lo que sugiere que son aviones nuevos que aún no han sido incluidos en la base de datos.

# 5 Añade una columna que liste todos los que han volado ese avión. Podrías esperar que haya una relación implícita entre avión y aerolínea, porque cada avión es pilotado por una sola aerolínea. Confirma o rechaza esta hipótesis usando las herramientas que has aprendido en capítulos anteriores.planescarrier

planes_carriers <- flights |>
  distinct(tailnum, carrier)
planes_carriers

planes_carriers |>
  count(tailnum) |>
  arrange(desc(n))
# hay aviones (n) que han sido pilotados por mas de una aerolinea, esto puede ser debido a que el avión ha sido vendido o alquilado a otra aerolinea, o a que el avión ha sido utilizado por varias aerolineas a lo largo del tiempo.ue 
# es decir n>1

library(dplyr)

planes_list <- flights |>
  group_by(tailnum) |>
  summarize(carriers = list(unique(carrier)))

# 6 Añadir la latitud y la longitud del aeropuerto de origen y destino a . ¿Es más fácil renombrar las columnas antes o después de la unión?flights

airports_origin <- airports |>
  select(faa, lat, lon) |>
  rename(origin = faa, lat_origin = lat, lon_origin = lon)

airports_dest <- airports |>
  select(faa, lat, lon) |>
  rename(dest = faa, lat_dest = lat, lon_dest = lon)

flights_geo <- flights |>
  left_join(airports_origin, by = "origin") |>
  left_join(airports_dest, by = "dest")

# 7 Calcula el retraso medio por destino y luego únete en el marco de datos para poder mostrar la distribución espacial de los retardos. Aquí tienes una forma sencilla de dibujar un mapa de Estados Unidos:airports

library(ggplot2)

delay_dest <- flights |>
  group_by(dest) |>
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE))

delay_map <- delay_dest |>
  left_join(airports, by = c("dest" = "faa"))

ggplot(delay_map, aes(lon, lat)) +
  borders("state") +
  geom_point(aes(size = mean_delay, color = mean_delay), alpha = 0.7) +
  coord_quickmap()
# 8 ¿Qué ocurrió el 13 de junio de 2013? Dibuja un mapa de los retrasos y luego usa Google para cruzar con el tiempo.
day_delay <- flights |>
  filter(year == 2013, month == 6, day == 13) |>
  group_by(dest) |>
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE)) |>
  left_join(airports, by = c("dest" = "faa"))

library(ggplot2)

ggplot(day_delay, aes(lon, lat)) +
  borders("state") +
  geom_point(aes(size = mean_delay, color = mean_delay), alpha = 0.7) +
  coord_quickmap()
