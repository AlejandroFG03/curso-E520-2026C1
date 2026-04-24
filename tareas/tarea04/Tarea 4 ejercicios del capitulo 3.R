library(tidyverse)
library(nycflights13)

#capitulo 3.2 Filas
flights

glimpse(flights)


flights |>
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

flights |> 
  filter(dep_delay > 120)

flights |> 
  filter(month == 1 & day == 1)

# Flights that departed in January or February
flights |> 
  filter(month == 1 | month == 2)


# A shorter way to select flights that departed in January or February
flights |> 
  filter(month %in% c(1, 2))

jan1 <- flights |> 
  filter(month == 1 & day == 1)

flights |> 
  arrange(year, month, day, dep_time)


flights |> 
  arrange(desc(dep_delay))


# Remove duplicate rows, if any
flights |> 
  distinct()


#Find all unique origin and destination pairs
flights |> 
  distinct(origin, dest)


flights |> 
  distinct(origin, dest, .keep_all = TRUE)


flights |>
  count(origin, dest, sort = TRUE)

#a partir de aca van los ejercicios del capitulo 3 inciso 3.2.5

# Ejercicio 1
flights |> 
  filter(
    dep_delay > 120,
    dest %in% c("IAH", "HOU"),
    month %in% c(7, 8, 9),
    arr_delay > 120 & dep_delay <= 0,          # 1) Llegó +2hs tarde pero no salió tarde
    dep_delay >= 60 & (dep_delay - arr_delay) > 30  # 2) Demoró 1hs+ pero recuperó 30min
  )
  
flights |> 
  filter(
    dep_delay > 120,
    dest %in% c("IAH", "HOU"),
    month %in% c(7, 8, 9)
  )
  # Condición 1
  flights |> 
  filter(arr_delay > 120, dep_delay <= 0)

# Condición 2
flights |> 
  filter(dep_delay >= 60, (dep_delay - arr_delay) > 30)

# 1a. Vuelos con retrasos de salida más largos
flights |> 
  arrange(desc(dep_delay))

# 1b. Vuelos que salían más temprano por la mañana
flights |> 
  arrange(dep_time)

# 2. Vuelos más rápidos (velocidad = distancia / tiempo en horas)
flights |> 
  arrange(desc(distance / (air_time / 60)))


# 3. ¿Había vuelo todos los días de 2013?
flights |> 
  distinct(year, month, day) |> 
  nrow()  # Si da 365, hubo vuelos todos los días

# 4a. Vuelo con mayor distancia
flights |> 
  arrange(desc(distance)) |> 
  slice(1)

# 4b. Vuelo con menor distancia
flights |> 
  arrange(distance) |> 
  slice(1)


# Capitulo 3.3 Selección de columnas

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  )


flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )


flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )


flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )


# Parte select

flights |> 
  select(year, month, day)

flights |> 
  select(year:day)


flights |> 
  select(!year:day)

flights |> 
  select(where(is.character))

flights |> 
  select(tail_num = tailnum)

# Parte rename

flights |> 
  rename(tail_num = tailnum)

# Parte relocate

flights |> 
  relocate(time_hour, air_time)

flights |> 
  relocate(year:dep_time, .after = time_hour)
flights |> 
  relocate(starts_with("arr"), .before = dep_time)

# Ejercicios 3.3.5

# 1 Compara , , y . ¿Cómo esperarías que estuvieran relacionados esos tres números?dep_timesched_dep_timedep_delay

#como lo que relaciona es la hora en la que salen dep time, la hora programada sched_dep_time y el retraso dep_delay, entonces la relacion es la siguiente: dep_time = sched_dep_time + dep_delay

# 2 Haz una lluvia de ideas en todas las formas posibles de seleccionar , , , y de .dep_timedep_delayarr_timearr_delayflights

# Todas las que empiezan con "dep_" o "arr_"
select(flights, starts_with("dep_"), starts_with("arr_"))

# dep_time hasta arr_delay, excluyendo las del medio
select(flights, dep_time:arr_delay) %>%
  select(-sched_dep_time, -sched_arr_time)

#Basica 
select(flights, dep_time, dep_delay, arr_time, arr_delay)

# 3 ¿Qué ocurre si especificas el nombre de la misma variable varias veces en una llamada?select()

# Si especifico por ejemplo arr_time varias veces, solo se selecciona una vez, no se repite la columna en el resultado final.

# 4 ¿Qué hace la función? ¿Por qué podría ser útil junto con este vector?any_of()

variables <- c("year", "month", "day", "dep_delay", "arr_delay")
 # Con lo primero genera un vector texto o de caracteres con los terminos especificados
 # Que si trabajamos con el comando any_of() dentro de select(), entonces selecciona solo las columnas que existan en el dataframe, por lo que es útil para evitar errores si alguna de las columnas listadas no existe en el dataframe.

select(flights, any_of(variables))

# 5 ¿Te sorprende el resultado de ejecutar el siguiente código? ¿Cómo gestionan los ayudantes selectos las mayúsculas y minúsculas por defecto? ¿Cómo puedes cambiar ese valor por defecto?

flights |> select(contains("TIME"))
 # No me sorprende digamos time es una de las variables para medir eficiencia
 #  Por defecto, los ayudantes de selección no distinguen entre mayúsculas y minúsculas, por lo que "TIME" coincide con "time", "Time", etc. Para cambiar este comportamiento, puedes usar el argumento ignore.case = FALSE dentro del ayudante, por ejemplo: select(contains("TIME", ignore.case = FALSE)) para que solo coincida exactamente con "TIME".

# 6 Renombra para indicar unidades de medida y muévelo al inicio del marco de datos.air_timeair_time_min
flights |> 
  rename(air_time_min = air_time) |> 
  relocate(air_time_min, .before = 1)

# 7 ¿Por qué no funciona lo siguiente y qué significa el error?

flights |> 
  select(tailnum) |> 
  arrange(arr_delay)
# El error ocurre porque después de seleccionar solo la columna tailnum, el dataframe resultante no tiene la columna arr_delay, por lo que no se puede ordenar por esa columna. El error significa que arr_delay no se encuentra en el dataframe actual después de la selección.

# Parte 3.4 la tuberia 
  
  flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))


  # Parte 3.5 grupo 
  
  flights |> 
    group_by(month)
  flights |> 
    group_by(month) |> 
    summarize(
      avg_delay = mean(dep_delay)
    )  
  flights |> 
    group_by(month) |> 
    summarize(
      avg_delay = mean(dep_delay, na.rm = TRUE)
    )  

  flights |> 
    group_by(month) |> 
    summarize(
      avg_delay = mean(dep_delay, na.rm = TRUE), 
      n = n()
    )  

  flights |> 
    group_by(dest) |> 
    slice_max(arr_delay, n = 1) |>
    relocate(dest)
  
      
  daily <- flights |>  
    group_by(year, month, day)

daily  


flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )
# 3.5.3 funciones slice

flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |>
  relocate(dest)

# 3.5.4 Agrupacion multiple

daily <- flights |>  
  group_by(year, month, day)

daily

daily_flights <- daily |> 
  summarize(n = n())

daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop_last"
  )

# 3.5.5 Desagrupacion 

daily |> 
  ungroup()
daily |> 
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    flights = n()
  )  

# 3.5.6 by

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = month
  )
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )

# 3.5.7 Ejercicios

# 1 ¿Qué operadora tiene los peores retrasos medios? Desafío: ¿se pueden distinguir los efectos de aeropuertos deficientes frente a los de transportistas malos? ¿Por qué o por qué no? 

flights |>
  group_by(carrier) |>
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  arrange(desc(avg_delay))
 #1.a La operadora  con los peores retrasos medios es la que tiene el valor más alto de avg_delay. en este caso F9

 #1.b No se pueden distinguir completamente los efectos de aeropuertos deficientes frente a los de transportistas malos solo con esta información, porque un transportista puede operar principalmente en aeropuertos con problemas de retrasos, lo que afectaría su promedio. Para distinguirlos, sería necesario analizar los datos a nivel de aeropuerto y transportista simultáneamente, por ejemplo, agrupando por ambos factores y comparando los retrasos medios en cada combinación.


# 2 Busca los vuelos que más retrasos se retrasan al salir a cada destino.

flights |> 
  group_by(carrier, dest) |> 
  slice_max(arr_delay, n = 1)
 # para filtrar los n/a 
flights |>
  filter(!is.na(arr_delay)) |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1)

# 3 ¿Cómo varían los retrasos a lo largo del día? Ilustra tu respuesta con una trama.

flights |>
  mutate(hour = sched_dep_time %/% 100)

flights |>
  mutate(hour = sched_dep_time %/% 100) |>
  group_by(hour) |>
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE))

flights |>
  mutate(hour = sched_dep_time %/% 100) |>
  group_by(hour) |>
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) |>
  ggplot(aes(x = hour, y = avg_delay)) +
  geom_line() +
  geom_point()
 # el plot nos dice que los retrasos se acumulan, es decir si un avion sale tarde por la mañana va retrasando a los demas, por ende a medida que pasa el dia los aviones se van retrasando aun mas llegando a su maximo alrededor de las 22 horas

# 4 ¿Qué pasa si le das un negativo a tus amigos?
n
slice_min()

flights |>
  group_by(dest) |>
  slice_min(arr_delay, n = 1)
 # el minimo juega una contracara, es decir nos muestra los vuelos mas "eficientes" en cuanto a su llegada un numero negativo refleja que tardo menos de lo previsto en llegar a su destino

# 5 Explica qué significa en términos de los verbos dplyr que acabas de aprender. ¿Qué significa el argumento?count()sortcount()

flights |> count(carrier)

# lo que hace count() es un atajo a un code mas "cargado ejemplo"
flights |>
  group_by(carrier) |>
  summarise(n = n())
# ambos codigos nos dan la misma tabla 

# 6 Supongamos que tenemos el siguiente pequeño marco de datos:

df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

# a Anota cómo crees que será el resultado, luego comprueba si estabas en lo correcto y describe qué es lo que sí.group_by()
 # Va a generar una estructura de grupos que define los grupos en a y b con su respectivo z
df |>
  group_by(y)

# b Anota cómo crees que será el resultado, luego comprueba si estabas en lo correcto y describe qué es lo que sí. Además, comenta en qué se diferencia de la parte (a).arrange()group_by()

# va a cambiar el orden, ordenando por fila y, desde todas las a a todas las b

df |>
  arrange(y)
# el arrange()group_by(), ordena y reagrupa algo que arrange por si solo no hace

# c Apunta cómo crees que será la salida, luego comprueba si estabas en lo correcto y describe qué hace la pipeline.

 # la salida va a agrupar por y e calcular su promedio, es decir nos va a dar una tabla donde nos muestra el promedio de y 
df |>
  group_by(y) |>
  summarize(mean_x = mean(x))

# d Apunta cómo crees que será la salida, luego comprueba si estabas en lo correcto y describe qué hace la pipeline. Luego, comenta lo que dice el mensaje.

 # va a agrupar por y, z , y nos va a dar su promedio respecto a x 
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))
 
# el mensaje nos dice que la funcion summarize va a reagrupar y e z en y, es decir no altera grupos anteriores

# e Apunta cómo crees que será la salida, luego comprueba si estabas en lo correcto y describe qué hace la pipeline. ¿En qué se diferencia la salida de la de la parte (d)?

 # creo que va a desagrupar la estructura previa, pero mantiene la media

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")

# f Apunta cómo crees que serán las salidas, luego comprueba si estabas en lo correcto y describe qué hace cada canalización. ¿En qué se diferencian los resultados de las dos tuberías?

 # en la primera va a dar el previo anterior, es decir va a reagrupar y mostrarnos la media, mientras que la segunda va a armar una columna con la media 

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))

