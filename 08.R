# =====================================================
# PROYECTO FINAL
# 08. COMUNIDADES (LOUVAIN)
# =====================================================

library(igraph)
library(dplyr)
library(tibble)

# =====================================================
# OBJETOS
# =====================================================

comunidades <- list()

comunidades_nodos <- tibble()

resumen_comunidades <- tibble()

modularidad_redes <- tibble()

# =====================================================
# RECORRER REDES
# =====================================================

for(tipo in names(redes)){
  
  comunidades[[tipo]] <- list()
  
  for(anio in names(redes[[tipo]])){
    
    red <- redes[[tipo]][[anio]]
    
    #----------------------------------------
    # LOUVAIN
    #----------------------------------------
    
    com <- cluster_louvain(
      red,
      weights = E(red)$weight
    )
    
    comunidades[[tipo]][[anio]] <- com
    
    #----------------------------------------
    # MODULARIDAD
    #----------------------------------------
    
    modularidad_redes <- bind_rows(
      
      modularidad_redes,
      
      tibble(
        
        Tipo = tipo,
        
        Año = as.numeric(anio),
        
        Modularidad = modularity(com)
        
      )
      
    )
    
    #----------------------------------------
    # COMUNIDAD DE CADA NODO
    #----------------------------------------
    
    memb <- membership(com)
    
    miembros <- tibble(
      
      Tipo = tipo,
      
      Año = as.numeric(anio),
      
      Nodo = names(memb),
      
      Comunidad = as.integer(memb)
      
    )
    
    comunidades_nodos <- bind_rows(
      
      comunidades_nodos,
      
      miembros
      
    )
    
    #----------------------------------------
    # TAMAÑO DE CADA COMUNIDAD
    #----------------------------------------
    
    resumen <- miembros %>%
      
      count(
        
        Comunidad,
        
        name = "Cantidad_nodos"
        
      ) %>%
      
      mutate(
        
        Tipo = tipo,
        
        Año = as.numeric(anio)
        
      ) %>%
      
      relocate(
        
        Tipo,
        
        Año
        
      )
    
    resumen_comunidades <- bind_rows(
      
      resumen_comunidades,
      
      resumen
      
    )
    
  }
  
}


# =====================================================
# INCORPORAR RESULTADOS A LAS MÉTRICAS
# =====================================================

metricas_globales <- metricas_globales %>%
  select(-Modularidad) %>%
  left_join(
    modularidad_redes,
    by = c("Tipo", "Año")
  )

metricas_nodos <- metricas_nodos %>%
  left_join(
    comunidades_nodos,
    by = c("Tipo", "Año", "Nodo")
  )

# =====================================================
# TOP OCUPACIONES POR COMUNIDAD
# =====================================================

top_ocupaciones <- metricas_nodos %>%
  group_by(
    Tipo,
    Año,
    Comunidad
  ) %>%
  arrange(
    desc(Fuerza),
    .by_group = TRUE
  ) %>%
  slice_head(n = 5) %>%
  ungroup()

# =====================================================
# ORDENAR TABLAS
# =====================================================

metricas_globales <- metricas_globales %>%
  arrange(
    Tipo,
    Año
  )

metricas_nodos <- metricas_nodos %>%
  arrange(
    Tipo,
    Año,
    Comunidad,
    desc(Fuerza)
  )

resumen_comunidades <- resumen_comunidades %>%
  arrange(
    Tipo,
    Año,
    Comunidad
  )

top_ocupaciones <- top_ocupaciones %>%
  arrange(
    Tipo,
    Año,
    Comunidad,
    desc(Fuerza)
  )

# =====================================================
# VERIFICACIÓN
# =====================================================

cat("\n=====================================\n")
cat("ANÁLISIS DE COMUNIDADES FINALIZADO\n")
cat("=====================================\n\n")

cat("Redes analizadas: ",
    nrow(metricas_globales),
    "\n")

cat("Registros de nodos: ",
    nrow(metricas_nodos),
    "\n")

cat("Comunidades detectadas: ",
    nrow(resumen_comunidades),
    "\n")

cat("Top ocupaciones: ",
    nrow(top_ocupaciones),
    "\n\n")

cat("Primeras filas de modularidad:\n")
print(head(metricas_globales))

cat("\nPrimeras comunidades:\n")
print(head(resumen_comunidades))

cat("\nPrimeras ocupaciones por comunidad:\n")
print(head(top_ocupaciones))
