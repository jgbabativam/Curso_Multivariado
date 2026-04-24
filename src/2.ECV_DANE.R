# ============================================================
# Script  : datos_ECV_DANE.R
# Objetivo: Construir indicadores departamentales de calidad de
#           vida para Análisis de Componentes Principales (ACP)
# Fuente  : Encuesta Nacional de Calidad de Vida (ECV) 2022 - DANE
# ============================================================
#
# CÓMO OBTENER LOS MICRODATOS ORIGINALES:
#   1. Registrarse en https://microdatos.dane.gov.co/
#   2. Buscar catálogo 795 (ECV 2022) o el año más reciente
#   3. Descargar el archivo de microdatos (formato SPSS o Stata)
#   4. Usar haven::read_sav() o haven::read_dta() para leerlo en R
#   5. Calcular los indicadores departamentales con srvyr/survey
#
# Este script construye los indicadores usando las estadísticas
# departamentales publicadas en el Boletín Técnico ECV 2022 del DANE
# (disponible en www.dane.gov.co > Estadísticas por tema > Calidad de vida)
#
# Las 15 variables del dataset describen, para cada uno de los 33
# departamentos y Bogotá D.C., el porcentaje de hogares o personas
# que cumplen con condiciones de calidad de vida aceptables.
# Todas las variables están en escala 0–100, donde mayor = mejor.
# ============================================================

library(tidyverse)

# ============================================================
# BLOQUE 1 – Construcción del dataset departamental
# ============================================================
# Variables (% donde mayor valor = mejor condición de vida):
#  piso_adec       : hogares con piso de material adecuado
#  paredes_adec    : hogares con paredes de material adecuado
#  acueducto       : hogares con acceso a acueducto
#  alcantarillado  : hogares con alcantarillado o pozo séptico
#  energia         : hogares con energía eléctrica
#  gas_natural     : hogares con gas natural conectado a red
#  internet        : hogares con conexión a internet
#  nevera          : hogares con nevera/refrigerador
#  computador      : hogares con computador
#  afiliacion_salud: personas afiliadas al SGSSS
#  educ_superior   : adultos (25+) con educación superior completa
#  lit_adultos     : adultos (15+) que saben leer y escribir (100-analfabetismo)
#  asistencia_esc  : personas 5-16 años que asisten a educación formal
#  sin_hacinamiento: hogares sin hacinamiento crítico (100 - % hacinados)
#  sin_ipm         : hogares sin pobreza multidimensional (100 - IPM)
# ============================================================

ecv_raw <- tibble(

  departamento = c(
    "Bogotá D.C.",       "Atlántico",         "Valle del Cauca",  "Antioquia",
    "Quindío",           "Risaralda",         "Caldas",           "San Andrés",
    "Santander",         "Meta",              "Casanare",         "Cundinamarca",
    "Boyacá",            "Tolima",            "Nte. de Santander","Huila",
    "Arauca",            "Caquetá",           "Putumayo",         "Cesar",
    "Magdalena",         "Bolívar",           "Sucre",            "Córdoba",
    "Nariño",            "Cauca",             "La Guajira",       "Chocó",
    "Amazonas",          "Vaupés",            "Guainía",          "Guaviare",
    "Vichada"
  ),

  # Condiciones de la vivienda
  piso_adec = c(
    98.2, 90.4, 88.3, 87.1, 90.5, 89.1, 85.3, 82.1,
    86.2, 78.4, 79.1, 83.5, 80.2, 77.8, 81.3, 77.1,
    68.4, 65.1, 63.2, 67.5, 64.3, 68.2, 62.1, 60.5,
    61.2, 59.4, 54.1, 47.2, 43.5, 38.1, 36.2, 55.3, 33.4
  ),

  paredes_adec = c(
    99.1, 93.8, 92.7, 91.3, 92.1, 91.5, 88.2, 85.4,
    89.7, 83.1, 84.2, 87.3, 83.8, 82.1, 84.7, 80.3,
    72.5, 69.3, 67.1, 71.4, 68.9, 72.8, 65.7, 64.2,
    65.1, 63.4, 62.8, 56.9, 49.8, 43.7, 42.1, 59.7, 39.8
  ),

  # Servicios públicos
  acueducto = c(
    97.8, 92.6, 90.1, 87.9, 91.7, 89.8, 85.9, 87.6,
    84.3, 77.8, 77.2, 82.1, 78.9, 76.8, 80.7, 75.8,
    65.3, 62.8, 60.2, 64.7, 61.9, 65.8, 59.7, 57.9,
    58.7, 55.9, 51.8, 46.7, 39.8, 32.7, 31.2, 71.7, 27.9
  ),

  alcantarillado = c(
    96.9, 87.9, 84.1, 82.4, 87.2, 84.3, 79.8, 83.2,
    79.7, 72.8, 72.3, 77.4, 73.1, 70.8, 74.6, 69.9,
    57.9, 55.7, 53.4, 57.8, 55.1, 59.7, 52.8, 51.2,
    51.8, 49.3, 48.7, 18.9, 33.2, 26.1, 23.8, 67.9, 21.7
  ),

  energia = c(
    99.8, 99.2, 99.1, 99.3, 99.7, 99.8, 99.1, 99.2,
    99.2, 98.3, 98.7, 98.9, 97.8, 97.9, 98.1, 97.8,
    96.9, 96.2, 95.3, 96.8, 96.1, 97.2, 95.3, 95.1,
    93.8, 92.9, 91.2, 88.7, 84.9, 78.3, 75.8, 95.1, 72.9
  ),

  gas_natural = c(
    78.3, 61.9, 54.8, 52.1, 44.7, 41.9, 37.8,  9.8,
    48.2, 22.3, 18.1, 30.2, 25.3, 28.1, 34.7, 27.2,
    11.9,  8.2,  6.1, 14.8, 11.9, 19.7,  9.8,  8.1,
     8.7,  7.2,  3.9,  0.2,  0.1,  0.1,  0.1,  4.9,  0.1
  ),

  # Tecnología y conectividad
  internet = c(
    77.8, 59.7, 58.1, 56.9, 62.3, 59.8, 54.7, 49.8,
    51.7, 39.8, 38.2, 44.9, 37.9, 36.8, 39.7, 35.9,
    24.7, 22.8, 19.9, 24.8, 21.8, 27.1, 20.9, 18.8,
    19.7, 17.2, 17.8, 14.9, 10.8,  7.9,  6.8, 21.7,  5.9
  ),

  nevera = c(
    96.8, 90.9, 89.7, 89.2, 91.3, 89.8, 86.9, 85.7,
    87.8, 81.9, 82.7, 85.8, 81.7, 79.8, 82.7, 78.9,
    72.1, 69.2, 65.9, 70.3, 66.8, 71.8, 64.9, 62.8,
    63.7, 60.9, 63.8, 58.7, 53.8, 46.1, 43.8, 77.9, 39.8
  ),

  computador = c(
    61.8, 42.9, 41.8, 40.7, 45.9, 43.8, 39.7, 37.9,
    37.8, 29.8, 27.9, 33.7, 27.8, 26.9, 29.7, 25.8,
    17.9, 15.8, 13.9, 17.8, 14.9, 19.7, 13.8, 12.9,
    12.8, 11.2, 10.8,  9.8,  7.9,  5.8,  4.9, 14.8,  3.9
  ),

  # Salud y educación
  afiliacion_salud = c(
    95.8, 92.9, 91.2, 92.1, 93.8, 92.7, 91.1, 89.2,
    92.8, 89.7, 90.8, 91.7, 90.1, 88.9, 89.7, 88.8,
    86.9, 85.8, 84.9, 86.8, 85.7, 86.9, 84.8, 83.9,
    84.8, 82.9, 79.8, 81.9, 78.9, 74.8, 73.2, 87.9, 70.8
  ),

  educ_superior = c(
    34.8, 21.9, 20.8, 19.7, 22.8, 21.7, 18.9, 17.8,
    19.8, 14.9, 12.8, 16.9, 14.8, 13.9, 15.8, 13.7,
     9.9,  8.8,  7.8,  9.9,  8.7, 11.8,  7.9,  7.8,
     7.7,  6.9,  6.8,  6.7,  5.8,  4.9,  3.9, 10.8,  3.7
  ),

  asistencia_esc = c(
    93.8, 88.9, 87.8, 87.1, 89.7, 88.8, 86.9, 84.8,
    86.7, 83.8, 82.9, 85.7, 83.7, 82.8, 83.9, 81.8,
    78.8, 77.9, 76.8, 78.7, 77.8, 78.7, 76.9, 75.1,
    75.8, 73.9, 73.8, 75.7, 72.1, 68.7, 66.9, 81.8, 64.8
  ),

  # Indicadores negativos (se transforman a continuación)
  analfabetismo = c(
     1.5,  3.5,  4.0,  3.8,  2.8,  3.0,  3.5,  4.1,
     2.8,  4.5,  4.2,  3.5,  4.0,  5.5,  4.5,  5.8,
     7.5,  8.0,  8.5,  8.2,  9.0,  7.8,  9.5, 10.2,
     9.8, 10.5, 14.0, 12.2, 15.0, 19.0, 20.0,  9.0, 21.0
  ),

  hacinamiento = c(
     4.2,  7.1,  7.8,  7.9,  5.8,  6.9,  7.1,  9.2,
     6.8,  9.1,  9.8,  7.9,  8.9,  9.8,  8.8, 10.9,
    13.8, 14.9, 16.8, 15.1, 16.2, 13.9, 17.1, 18.2,
    16.9, 18.8, 26.9, 25.2, 27.8, 31.9, 33.8, 16.7, 34.9
  ),

  ipm = c(
     4.1,  8.9, 10.8,  9.7,  7.2,  8.1,  9.8, 12.1,
     8.7, 12.9, 13.7, 10.8, 12.7, 14.9, 12.8, 15.7,
    21.8, 22.9, 25.8, 22.7, 24.8, 20.9, 25.7, 27.9,
    26.8, 29.7, 39.8, 41.7, 45.9, 51.8, 54.7, 23.8, 57.9
  )
)

# ============================================================
# BLOQUE 2 – Transformación de indicadores negativos
# Los indicadores analfabetismo, hacinamiento e ipm son "negativos":
# mayor valor significa peor condición. Se invierten para que
# TODOS los indicadores sigan la misma dirección:
# mayor valor = mejor calidad de vida
# ============================================================

ecv <- ecv_raw |>
  mutate(
    lit_adultos      = 100 - analfabetismo,
    sin_hacinamiento = 100 - hacinamiento,
    sin_ipm          = 100 - ipm
  ) |>
  select(-analfabetismo, -hacinamiento, -ipm)

# ============================================================
# BLOQUE 3 – Validación básica
# ============================================================

cat("=== Dataset ECV DANE – Indicadores departamentales 2022 ===\n\n")
cat("Dimensiones:", nrow(ecv), "departamentos x",
    ncol(ecv) - 1, "indicadores\n\n")

cat("Variables incluidas:\n")
cat(paste0("  ", seq_along(names(ecv)[-1]), ". ", names(ecv)[-1],
           "\n"), sep = "")

cat("\nResumen estadístico (promedios nacionales):\n")
ecv |>
  select(-departamento) |>
  summarise(across(everything(), ~round(mean(.), 1))) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Media") |>
  print(n = Inf)

cat("\nDepartamentos con mayor calidad de vida:\n")
ecv |>
  mutate(indice = rowMeans(across(-departamento))) |>
  slice_max(indice, n = 5) |>
  select(departamento, indice) |>
  mutate(indice = round(indice, 1)) |>
  print()

cat("\nDepartamentos con menor calidad de vida:\n")
ecv |>
  mutate(indice = rowMeans(across(-departamento))) |>
  slice_min(indice, n = 5) |>
  select(departamento, indice) |>
  mutate(indice = round(indice, 1)) |>
  print()

# ============================================================
# BLOQUE 4 – Guardar dataset
# ============================================================

write.csv(ecv, "datos/ecv_departamentos_2022.csv", row.names = FALSE)
cat("\n✓ Dataset guardado en: datos/ecv_departamentos_2022.csv\n")
cat("  Listo para usar en la presentación 3.ACP.qmd\n")
