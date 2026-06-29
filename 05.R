# =====================================================
# PROYECTO FINAL
# Coocurrencia ocupacional en hogares argentinos
# =====================================================

# =====================================================
# 05. CONSTRUCCIÓN DE COOCCURRENCIAS
# =====================================================

# Este script construye las tablas de coocurrencias
# ocupacionales para cada universo de análisis y
# las organiza por año.

# =====================================================
# PAQUETES
# =====================================================

library(dplyr)

# =====================================================
# FUNCIÓN PARA CREAR COOCCURRENCIAS
# =====================================================

crear_coocurrencias <- function(base){
  
  #---------------------------------------------
  # Ocupaciones presentes en cada hogar
  #---------------------------------------------
  
  ocupaciones_hogar <- base %>%
    
    select(
      id_hogar,
      year,
      cod_2d
    ) %>%
    
    distinct()
  
  #---------------------------------------------
  # Construcción de pares de ocupaciones
  #---------------------------------------------
  
  pares <- ocupaciones_hogar %>%
    
    inner_join(
      
      ocupaciones_hogar,
      
      by = c("id_hogar", "year"),
      
      suffix = c("_1", "_2"),
      
      relationship = "many-to-many"
      
    ) %>%
    
    # eliminar autocoincidencias
    filter(cod_2d_1 != cod_2d_2) %>%
    
    # ordenar los pares
    mutate(
      
      origen = pmin(cod_2d_1, cod_2d_2),
      
      destino = pmax(cod_2d_1, cod_2d_2)
      
    ) %>%
    
    # una sola vez por hogar
    distinct(
      
      id_hogar,
      year,
      origen,
      destino
      
    )
  
  #---------------------------------------------
  # Tabla de aristas
  #---------------------------------------------
  
  aristas <- pares %>%
    
    group_by(
      
      year,
      origen,
      destino
      
    ) %>%
    
    summarise(
      
      peso = n(),
      
      .groups = "drop"
      
    ) %>%
    
    filter(
      
      peso >= 5
      
    ) %>%
    
    arrange(
      
      year,
      desc(peso)
      
    )
  
  return(aristas)
  
}

# =====================================================
# TABLAS DE COOCCURRENCIAS
# =====================================================

cooc_general <- crear_coocurrencias(base_final)

cooc_bajos <- crear_coocurrencias(
  
  base_final %>%
    
    filter(grupo_ingreso == "Bajo")
  
)

cooc_medios <- crear_coocurrencias(
  
  base_final %>%
    
    filter(grupo_ingreso == "Medio")
  
)

cooc_altos <- crear_coocurrencias(
  
  base_final %>%
    
    filter(grupo_ingreso == "Alto")
  
)

cooc_formales <- crear_coocurrencias(
  
  base_asalariados %>%
    
    filter(tipo_hogar == "Formal")
  
)

cooc_informales <- crear_coocurrencias(
  
  base_asalariados %>%
    
    filter(tipo_hogar == "Informal")
  
)

# =====================================================
# TABLA DE NODOS
# =====================================================

nodos <- base_final %>%
  
  distinct(
    
    cod_2d,
    categoria_ocupacional
    
  ) %>%
  
  rename(
    
    name = cod_2d
    
  )

# =====================================================
# ORGANIZAR COOCCURRENCIAS POR AÑO
# =====================================================

coocurrencias <- list(
  
  General = split(
    cooc_general,
    cooc_general$year
  ),
  
  Bajos = split(
    cooc_bajos,
    cooc_bajos$year
  ),
  
  Medios = split(
    cooc_medios,
    cooc_medios$year
  ),
  
  Altos = split(
    cooc_altos,
    cooc_altos$year
  ),
  
  Formales = split(
    cooc_formales,
    cooc_formales$year
  ),
  
  Informales = split(
    cooc_informales,
    cooc_informales$year
  )
  
)

# =====================================================
# VERIFICACIÓN
# =====================================================

cat("\n=====================================\n")
cat("TABLAS DE COOCCURRENCIAS GENERADAS\n")
cat("=====================================\n\n")

for(tipo in names(coocurrencias)){
  
  cat(tipo, "\n")
  
  for(anio in names(coocurrencias[[tipo]])){
    
    cat(
      
      "  ", anio,
      
      ":",
      
      nrow(coocurrencias[[tipo]][[anio]]),
      
      "aristas\n"
      
    )
    
  }
  
  cat("\n")
  
}

cat("Cantidad de nodos:", nrow(nodos), "\n")

