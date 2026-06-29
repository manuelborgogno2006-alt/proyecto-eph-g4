nodos <- base_final %>%
  distinct(
    cod_2d,
    categoria_ocupacional
  ) %>%
  rename(
    name = cod_2d
  )


# =====================================================
# PROYECTO FINAL
# Coocurrencia ocupacional en hogares argentinos
# =====================================================

# =====================================================
# 06. CONSTRUCCIÓN DE REDES
# =====================================================

library(igraph)
library(dplyr)

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
# FUNCIÓN PARA CREAR REDES
# =====================================================

crear_red <- function(aristas){
  
  aristas <- aristas %>%
    select(
      origen,
      destino,
      peso
    )
  
  red <- graph_from_data_frame(
    d = aristas,
    vertices = nodos,
    directed = FALSE
  )
  
  # Copiar peso a weight
  E(red)$weight <- E(red)$peso
  
  return(red)
  
}

# =====================================================
# CREAR TODAS LAS REDES
# =====================================================

redes <- lapply(
  
  coocurrencias,
  
  function(lista_anios){
    
    lapply(
      
      lista_anios,
      
      crear_red
      
    )
    
  }
  
)

# =====================================================
# VERIFICACIÓN
# =====================================================

cat("\n==============================\n")
cat("REDES CREADAS\n")
cat("==============================\n\n")

for(tipo in names(redes)){
  
  cat("\n", tipo, "\n")
  
  for(anio in names(redes[[tipo]])){
    
    red <- redes[[tipo]][[anio]]
    
    cat(
      
      anio,
      
      "- Nodos:", gorder(red),
      
      "- Aristas:", gsize(red),
      
      "\n"
      
    )
    
  }
  
}
