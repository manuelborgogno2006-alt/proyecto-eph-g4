# =====================================================
# PROYECTO FINAL
# Coocurrencia ocupacional en hogares argentinos
# =====================================================

# =====================================================
# 07. MÉTRICAS DE RED
# =====================================================

library(igraph)
library(dplyr)
library(tibble)

# =====================================================
# OBJETOS DE SALIDA
# =====================================================

metricas_globales <- tibble()

metricas_nodos <- tibble()

# =====================================================
# RECORRER TODAS LAS REDES
# =====================================================

for (tipo in names(redes)) {
  
  for (anio in names(redes[[tipo]])) {
    
    red <- redes[[tipo]][[anio]]
    
    #-------------------------------------------
    # Distancias (para caminos mínimos)
    #-------------------------------------------
    
    distancias <- 1 / E(red)$weight
    
    #-------------------------------------------
    # MÉTRICAS GLOBALES
    #-------------------------------------------
    
    fila_global <- tibble(
      
      Tipo = tipo,
      
      Año = as.numeric(anio),
      
      Nodos = gorder(red),
      
      Aristas = gsize(red),
      
      Densidad = edge_density(red),
      
      Grado_medio = mean(
        degree(red)
      ),
      
      Fuerza_media = mean(
        strength(
          red,
          weights = E(red)$weight
        )
      ),
      
      Componentes = components(red)$no,
      
      Clustering = transitivity(
        red,
        type = "global"
      ),
      
      Diametro = diameter(
        red,
        weights = distancias
      ),
      
      Distancia_media = mean_distance(
        red,
        weights = distancias,
        directed = FALSE
      ),
      
      Modularidad = NA_real_
      
    )
    
    metricas_globales <- bind_rows(
      
      metricas_globales,
      
      fila_global
      
    )
    
    #-------------------------------------------
    # MÉTRICAS POR NODO
    #-------------------------------------------
    
    tabla_nodos <- tibble(
      
      Tipo = tipo,
      
      Año = as.numeric(anio),
      
      Nodo = V(red)$name,
      
      Categoria = V(red)$categoria_ocupacional,
      
      Grado = degree(red),
      
      Fuerza = strength(
        
        red,
        
        weights = E(red)$weight
        
      ),
      
      Betweenness = betweenness(
        
        red,
        
        weights = distancias
        
      ),
      
      Closeness = closeness(
        
        red,
        
        weights = distancias
        
      ),
      
      Eigenvector = eigen_centrality(
        
        red,
        
        weights = E(red)$weight
        
      )$vector,
      
      PageRank = page_rank(
        
        red,
        
        weights = E(red)$weight
        
      )$vector
      
    )
    
    metricas_nodos <- bind_rows(
      
      metricas_nodos,
      
      tabla_nodos
      
    )
    
  }
  
}

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
    
    desc(Fuerza)
    
  )

# =====================================================
# VERIFICACIÓN
# =====================================================

cat("\n=====================================\n")
cat("MÉTRICAS CALCULADAS\n")
cat("=====================================\n\n")

cat("Cantidad de redes analizadas: ",
    nrow(metricas_globales),
    "\n")

cat("Cantidad de registros de nodos: ",
    nrow(metricas_nodos),
    "\n\n")

cat("Primeras métricas globales:\n\n")

print(metricas_globales)

cat("\nPrimeras métricas por nodo:\n\n")

print(
  
  head(metricas_nodos)
  
)

# =====================================================
# OBJETOS DISPONIBLES
# =====================================================

# metricas_globales
# metricas_nodos

write.csv(metricas_globales,
          "metricas_globales.csv",
          row.names = FALSE)

write.csv(metricas_nodos,
          "metricas_nodos.csv",
          row.names = FALSE)




coocurrencias$General$`2025`
coocurrencias$Bajos$`2025`
coocurrencias$Medios$`2025`
coocurrencias$Altos$`2025`
coocurrencias$Formales$`2025`
coocurrencias$Informales$`2025`
write.csv(
  coocurrencias$General$`2025`,
  "cooc_general_2025.csv",
  row.names = FALSE
)

write.csv(
  coocurrencias$Bajos$`2025`,
  "cooc_bajos_2025.csv",
  row.names = FALSE
)

write.csv(
  coocurrencias$Medios$`2025`,
  "cooc_medios_2025.csv",
  row.names = FALSE
)

write.csv(
  coocurrencias$Altos$`2025`,
  "cooc_altos_2025.csv",
  row.names = FALSE
)

write.csv(
  coocurrencias$Formales$`2025`,
  "cooc_formales_2025.csv",
  row.names = FALSE
)

write.csv(
  coocurrencias$Informales$`2025`,
  "cooc_informales_2025.csv",
  row.names = FALSE
)
base_final %>%
  filter(year == 2025) %>%
  distinct(CODUSU, NRO_HOGAR) %>%
  nrow()

base_final %>%
  filter(
    year == 2025,
    grupo_ingreso == "Bajo"
  ) %>%
  distinct(CODUSU, NRO_HOGAR) %>%
  nrow()
base_final %>%
  filter(
    year == 2025,
    grupo_ingreso == "Medio"
  ) %>%
  distinct(CODUSU, NRO_HOGAR) %>%
  nrow()
base_final %>%
  filter(
    year == 2025,
    grupo_ingreso == "Alto"
  ) %>%
  distinct(CODUSU, NRO_HOGAR) %>%
  nrow()
base_final %>%
  filter(
    year == 2025,
    tipo_hogar == "Formal"
  ) %>%
  distinct(CODUSU, NRO_HOGAR) %>%
  nrow()
