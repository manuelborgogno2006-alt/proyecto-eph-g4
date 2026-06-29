# =====================================================
# PROYECTO FINAL
# Coocurrencia ocupacional en hogares argentinos
# =====================================================

# =====================================================
# 03. VARIABLES DERIVADAS
# =====================================================

# Este script incorpora variables derivadas a la base_final.
#
# En esta primera versión:
# - Se incorpora la Clasificación Nacional de Ocupaciones
#   (CNO).
# - Se construye la categoría ocupacional de 2 dígitos,
#   que será el nodo de la red.
#
# Resultado:
# base_final enriquecida.

# =====================================================
# PAQUETES
# =====================================================

library(dplyr)
library(stringr)

# =====================================================
# PREPARAR CÓDIGO OCUPACIONAL
# =====================================================

base_final <- base_final %>%
  
  mutate(
    
    PP04D_COD = as.character(PP04D_COD),
    
    PP04D_COD = str_pad(
      PP04D_COD,
      width = 5,
      side = "left",
      pad = "0"
    ),
    
    cod_2d = substr(PP04D_COD, 1, 2)
    
  )

# =====================================================
# TABLA DE REFERENCIA CNO
# =====================================================

cno_2d <- CNO %>%
  
  filter(digit == 12) %>%
  
  transmute(
    
    cod_2d = as.character(value),
    
    categoria_ocupacional = label
    
  )

# =====================================================
# INCORPORAR LA NOMENCLATURA CNO
# =====================================================

base_final <- base_final %>%
  
  left_join(
    
    cno_2d,
    
    by = "cod_2d"
    
  )

# =====================================================
# CREAR GRUPOS DE INGRESO
# =====================================================

base_final <- base_final %>%
  
  mutate(
    
    grupo_ingreso = case_when(
      
      DECCFR.x %in% 1:3  ~ "Bajo",
      
      DECCFR.x %in% 4:7  ~ "Medio",
      
      DECCFR.x %in% 8:10 ~ "Alto",
      
      TRUE ~ NA_character_
      
    )
    
  )

# =====================================================
# VERIFICACIÓN
# =====================================================

glimpse(base_final)

summary(base_final)

sum(is.na(base_final$categoria_ocupacional))

