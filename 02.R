# =====================================================
# PROYECTO FINAL
# Coocurrencia ocupacional en hogares argentinos
# =====================================================

# =====================================================
# 02. CONSTRUCCIÓN DE LA BASE FINAL
# =====================================================

# Este script toma la base_completa y construye la
# base definitiva del proyecto.
#
# La base final contiene únicamente los hogares:
# - con 2 o más miembros
# - con al menos 2 ocupados
# - conservando solamente los ocupados
#
# Además incorpora:
# - variables ocupacionales (CNO)
# - variables de ingreso
# - variables para construir informalidad
#
# Resultado:
# base_final

# =====================================================
# PAQUETES
# =====================================================

library(dplyr)
library(stringr)

# =====================================================
# CREAR IDENTIFICADOR ÚNICO DEL HOGAR
# =====================================================

base_completa <- base_completa %>%
  mutate(
    id_hogar = paste(
      CODUSU,
      NRO_HOGAR,
      year,
      trimestre,
      sep = "_"
    )
  )

# =====================================================
# SELECCIONAR VARIABLES DE INTERÉS
# =====================================================

base_final <- base_completa %>%
  select(
    
    # -------------------------
    # Identificación
    # -------------------------
    
    id_hogar,
    CODUSU,
    NRO_HOGAR,
    COMPONENTE,
    year,
    trimestre,
    
    # -------------------------
    # Demográficas
    # -------------------------
    
    CH04,
    CH06,
    
    # -------------------------
    # Mercado laboral
    # -------------------------
    
    ESTADO,
    CAT_OCUP,
    
    # -------------------------
    # Ocupación
    # -------------------------
    
    PP04B_COD,
    PP04D_COD,
    
    # -------------------------
    # Ingresos
    # -------------------------
    
    P47T,
    IPCF.x,
    ITF.x,
    DECCFR.x,
    DECIFR.x,
    
    # -------------------------
    # Variables para informalidad
    # -------------------------
    
    PP07E,
    PP07H,
    PP07I,
    
    # -------------------------
    # Hogar
    # -------------------------
    
    IX_TOT,
    
    # -------------------------
    # Ponderadores
    # -------------------------
    
    PONDIIO,
    PONDERA.x
    
  )

# =====================================================
# FILTRAR HOGARES
# =====================================================

base_final <- base_final %>%
  
  group_by(id_hogar) %>%
  
  # hogares con dos o más miembros
  filter(first(IX_TOT) >= 2) %>%
  
  # hogares con al menos dos ocupados
  filter(sum(ESTADO == 1, na.rm = TRUE) >= 2) %>%
  
  # conservar únicamente los ocupados
  filter(ESTADO == 1) %>%
  
  ungroup()

# =====================================================
# AGREGAR NOMENCLATURA CNO
# =====================================================

base_final <- base_final %>%
  
  mutate(
    
    PP04D_COD = as.character(PP04D_COD),
    
    PP04D_COD = str_pad(
      PP04D_COD,
      width = 5,
      side = "left",
      pad = "0"
    )
    
  )

# =====================================================
# DESCOMPONER EL CÓDIGO CNO
# =====================================================

base_final <- base_final %>%
  
  mutate(
    
    categoria    = substr(PP04D_COD, 1, 2),
    jerarquia    = substr(PP04D_COD, 3, 3),
    tecnologia   = substr(PP04D_COD, 4, 4),
    calificacion = substr(PP04D_COD, 5, 5)
    
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
