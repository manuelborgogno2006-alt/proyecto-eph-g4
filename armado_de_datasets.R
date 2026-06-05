install.packages("eph")
library(eph)
help("eph")

library(tidyverse)

getwd()

ruta_ind <- choose.dir()
ruta_ind
list.files(ruta_ind)
#en cada caso agarré todos los trimestres disponibles y armé las bases de datos completas
#los puse en una carpeta, me paré en esa dirección y los armé con el siguiente código
#también los limpié

archivos_ind <- list.files(
  ruta_ind,
  pattern = "\\.txt",   # agarra todos los txt aunque tengan txt.txt
  full.names = TRUE,
  ignore.case = TRUE    # ignora mayúsculas/minúsculas
)
ind <- archivos_ind %>%
  map_dfr(~ read_delim(.x, delim = ";", show_col_types = FALSE))

ind <- archivos_ind %>%
  map_dfr(~ read_delim(
    .x,
    delim = ";",
    locale = locale(encoding = "Latin1"),
    col_types = cols(.default = "c"),  # 👈 CLAVE
    show_col_types = FALSE
  ))

ind <- ind %>%
  mutate(
    CODUSU = as.character(CODUSU),
    NRO_HOGAR = as.character(NRO_HOGAR),
    COMPONENTE = as.character(COMPONENTE),
    PP04D_COD = as.character(PP04D_COD),
    ESTADO = as.character(ESTADO),
    ANO4 = as.numeric(ANO4),
    TRIMESTRE = as.numeric(TRIMESTRE)
  )

ind <- ind %>%
  mutate(periodo = paste0(ANO4, "T", TRIMESTRE))

ruta_hog <- choose.dir()
ruta_hog

list.files(ruta_ind)

library(tidyverse)

archivos_hog <- list.files(
  ruta_hog,
  pattern = "\\.txt",   # agarra todos los txt aunque tengan txt.txt
  full.names = TRUE,
  ignore.case = TRUE    # ignora mayúsculas/minúsculas
)

hog <- archivos_hog %>%
  map_dfr(~ read_delim(.x, delim = ";", show_col_types = FALSE))

hog <- archivos_hog %>%
  map_dfr(~ read_delim(
    .x,
    delim = ";",
    locale = locale(encoding = "Latin1"),
    col_types = cols(.default = "c"),  # 👈 CLAVE
    show_col_types = FALSE
  ))

hog <- hog %>%
  mutate(
    CODUSU = as.character(CODUSU),
    NRO_HOGAR = as.character(NRO_HOGAR),
    ANO4 = as.numeric(ANO4),
    TRIMESTRE = as.numeric(TRIMESTRE)
  )

hog <- hog %>%
  mutate(periodo = paste0(ANO4, "T", TRIMESTRE))

eph <- ind %>%
  left_join(hog, by = c("CODUSU", "NRO_HOGAR", "ANO4", "TRIMESTRE"))

getwd()
write_csv(ind, "eph_ind")
write_csv(hog, "eph_hog")
write_csv(eph, "eph_agg")
