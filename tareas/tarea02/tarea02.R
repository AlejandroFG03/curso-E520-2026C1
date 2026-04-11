# Asignación de un valor a una variable
encuestado_id <- 1045

# Imprimir el valor de la variable
ingreso <- 350000.50     # numeric
miembros_hogar <- 4L     # integer (la 'L' fuerza que sea entero)
estado <- "Ocupado"      # character
busca_trabajo <- FALSE   # logical

class(ingreso)         # Devuelve: "numeric"
class(busca_trabajo)   # Devuelve: "logical"

horas_trabajadas <- 40.5   # Puede tener decimales
edad_anios <- 28L          # Edad exacta en años completos

sector_actividad <- "Comercio"
categoria_ocupacional <- 'Cuentapropista'


# Unir textos para un reporte
paste("Sector:", sector_actividad, "-", categoria_ocupacional) 
# Buscar un patrón (¿Contiene la palabra "propia"?)
grepl("propia", "Cuenta propia con local") # Devuelve: TRUE

salario_mensual <- 450000
salario_anual <- salario_mensual * 13 # Incluye aguinaldo
# Calcular el ingreso per cápita
ingreso_per_capita <- salario_anual / miembros_hogar
# Imprimir resultados
print(paste("Salario anual:", salario_anual))

Operadores de comparación (==, >, <).
es_mayor_edad <- edad_anios >= 18
es_desocupado <- estado == "Desocupado"

Operadores lógicos (& AND, | OR, ! NOT).
# Verificamos si pertenece a la Población Económicamente Activa (PEA)
es_pea <- (estado == "Ocupado" | estado == "Desocupado") & edad_anios >= 16

if (salario_mensual < 200000) {
  print("Por debajo del salario mínimo")
}

Añadiendo else if y else para clasificar un ingreso.
if (salario_mensual > 800000) {
  decil <- "Alto"
} else if (salario_mensual >= 300000) {
  decil <- "Medio"
} else {
  decil <- "Bajo"
}
Iteración mientras se cumpla una condición (Simulación de búsqueda).
meses_busqueda <- 0
while (meses_busqueda < 3) {
  print(paste("Mes", meses_busqueda, ": Buscando empleo..."))
  meses_busqueda <- meses_busqueda + 1
}
# Simulación: encuentra trabajo al mes 2
meses_busqueda <- 0
while (TRUE) {
  meses_busqueda <- meses_busqueda + 1
  if (meses_busqueda == 2) {
    print("¡Empleo encontrado!")
    break 
  }
}

# Vector con salarios horarios
salarios_hora <- c(1500, 2200, 1800, 3100)
for (salario in salarios_hora) {
  print(salario * 8) # Imprime el salario por jornada de 8 horas
}

for (i in 1:length(salarios_hora)) {
  # Aplicar un aumento del 10% a cada elemento
  salarios_hora[i] <- salarios_hora[i] * 1.10 
}

edades_hogar <- c(45, 42, 16, 12)
promedio_edad <- mean(edades_hogar)

efe_hogar <- list(
  id = 101,
  nombre = "Carlos",
  edades_familia = edades_hogar,
  es_propietario = TRUE
)

# Transición Ocupado/Desocupado (T1 a T2)
datos_transicion <- c(80, 20, 15, 85)
matriz_transicion <- matrix(datos_transicion, nrow = 2, byrow = TRUE)

# 2 filas, 2 columnas, 3 "capas" (trimestres)
panel_laboral <- array(1:12, dim = c(2, 2, 3))

# Creando una "mini base de microdatos"
microdatos <- data.frame(
  id_persona = c(1, 2, 3),
  edad = c(34, 19, 52),
  ingreso = c(450000, 0, 780000),
  trabajo_semana_pasada = c(TRUE, FALSE, TRUE)
)

str(microdatos)      # Estructura y tipos de datos de cada columna
summary(microdatos)  # Resumen estadístico (min, media, max, etc.)

microdatos$ingreso   # Extrae solo el vector de ingresos

vector_estados <- c("Ocupado", "Desocupado", "Inactivo", "Ocupado")
estado_factor <- factor(vector_estados)

levels(estado_factor) # Devuelve: "Desocupado" "Inactivo" "Ocupado"

# Forzar un orden lógico en los niveles (Ordinales)
nivel_edu <- factor(c("Secundario", "Universitario", "Primario"),
                    levels = c("Primario", "Secundario", "Universitario"),
                    ordered = TRUE)



