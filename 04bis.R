# =====================================================
# PROYECTO FINAL
# Coocurrencia ocupacional en hogares argentinos
# =====================================================

# =====================================================
# 04. FORMALIDAD LABORAL
# =====================================================

# Este script construye la variable de formalidad
# laboral para los trabajadores asalariados y clasifica
# los hogares según la condición laboral de sus ocupados.

# =====================================================
# PAQUETES
# =====================================================

library(dplyr)

# =====================================================
# CONSERVAR SOLO ASALARIADOS
# =====================================================

base_asalariados <- base_final %>%
  
  filter(CAT_OCUP == 3)

# =====================================================
# FORMALIDAD INDIVIDUAL
# =====================================================

base_asalariados <- base_asalariados %>%
  
  mutate(
    
    formalidad_persona = case_when(
      
      PP07H == 1 ~ "Formal",
      
      PP07H == 2 ~ "Informal",
      
      TRUE ~ NA_character_
      
    )
    
  )

# =====================================================
# CLASIFICACIÓN DEL HOGAR
# =====================================================

hogares_formalidad <- base_asalariados %>%
  
  group_by(id_hogar) %>%
  
  summarise(
    
    ocupados = n(),
    
    formales = sum(
      formalidad_persona == "Formal",
      na.rm = TRUE
    ),
    
    informales = sum(
      formalidad_persona == "Informal",
      na.rm = TRUE
    ),
    
    tipo_hogar = case_when(
      
      formales == ocupados ~ "Formal",
      
      informales == ocupados ~ "Informal",
      
      TRUE ~ "Mixto"
      
    ),
    
    .groups = "drop"
    
  )

# =====================================================
# INCORPORAR A LA BASE
# =====================================================

base_asalariados <- base_asalariados %>%
  
  left_join(
    
    hogares_formalidad %>%
      
      select(
        id_hogar,
        tipo_hogar
      ),
    
    by = "id_hogar"
    
  )

# =====================================================
# VERIFICACIÓN
# =====================================================

table(base_asalariados$formalidad_persona)

table(base_asalariados$tipo_hogar)

glimpse(base_asalariados)

