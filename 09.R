# =====================================================
# PROYECTO FINAL
# 09. VISUALIZACIONES
# =====================================================

library(igraph)
library(ggraph)
library(ggplot2)
library(dplyr)

#======================================================
# Carpeta de salida
#======================================================

if(!dir.exists("graficos")){
  dir.create("graficos")
}

#======================================================
# Función de graficado
#======================================================

graficar_red <- function(red,
                         metricas,
                         titulo,
                         archivo){
  
  #-----------------------------------------
  # Componente gigante
  #-----------------------------------------
  
  comp <- components(red)
  
  gigante <- induced_subgraph(
    
    red,
    
    vids = which(
      
      comp$membership == which.max(comp$csize)
      
    )
    
  )
  
  #-----------------------------------------
  # Datos de los nodos
  #-----------------------------------------
  
  datos <- metricas %>%
    
    filter(
      
      Nodo %in% V(gigante)$name
      
    )
  
  V(gigante)$codigo <- V(gigante)$name
  
  V(gigante)$fuerza <-
    
    datos$Fuerza[
      match(
        V(gigante)$name,
        datos$Nodo
      )
    ]
  
  V(gigante)$comunidad <-
    
    factor(
      
      datos$Comunidad[
        match(
          V(gigante)$name,
          datos$Nodo
        )
      ]
      
    )
  
  #-----------------------------------------
  # Layout
  #-----------------------------------------
  
  set.seed(123)
  
  lay <- create_layout(
    
    gigante,
    
    layout="fr"
    
  )
  
  #-----------------------------------------
  # Gráfico
  #-----------------------------------------
  
  p <-
    
    ggraph(lay)+
    
    geom_edge_link(
      
      aes(
        
        alpha = log1p(weight),
        
        width = log1p(weight)
        
      ),
      
      colour="grey70",
      
      show.legend=FALSE
      
    )+
    
    scale_edge_alpha(
      
      range=c(.10,.40)
      
    )+
    
    scale_edge_width(
      
      range=c(.20,.80)
      
    )+
    
    geom_node_point(
      
      aes(
        
        size=fuerza,
        
        colour=comunidad
        
      )
      
    )+
    
    scale_size(
      
      range=c(2,5),
      
      guide="none"
      
    )+
    
    geom_node_text(
      
      aes(
        
        label=codigo
        
      ),
      
      size=3,
      
      vjust=-0.8
      
    )+
    
    labs(
      
      title=titulo,
      
      colour="Comunidad"
      
    )+
    
    theme_void()+
    
    theme(
      
      plot.title=
        
        element_text(
          
          hjust=.5,
          
          face="bold",
          
          size=18
          
        ),
      
      legend.position="right"
      
    )
  
  print(p)
  
  #----------------------------------------------------
  # Guardar figura
  #----------------------------------------------------
  
  ggsave(
    filename = file.path(
      "graficos",
      paste0(archivo, ".png")
    ),
    plot = p,
    width = 10,
    height = 8,
    dpi = 300,
    bg = "white"
  )
  
}

#======================================================
# REDES A GRAFICAR
#======================================================

graficos <- tibble(
  
  Tipo = c(
    "General",
    "General",
    "General",
    "Bajos",
    "Medios",
    "Altos",
    "Formales",
    "Informales"
  ),
  
  Año = c(
    2016,
    2021,
    2025,
    2025,
    2025,
    2025,
    2025,
    2025
  ),
  
  Archivo = c(
    "general_2016",
    "general_2021",
    "general_2025",
    "bajos_2025",
    "medios_2025",
    "altos_2025",
    "formales_2025",
    "informales_2025"
  ),
  
  Titulo = c(
    "General (2016)",
    "General (2021)",
    "General (2025)",
    "Bajos ingresos (2025)",
    "Ingresos medios (2025)",
    "Altos ingresos (2025)",
    "Hogares formales (2025)",
    "Hogares informales (2025)"
  )
  
)

#======================================================
# GENERAR TODAS LAS FIGURAS
#======================================================

for(i in seq_len(nrow(graficos))){
  
  tipo <- graficos$Tipo[i]
  
  anio <- as.character(graficos$Año[i])
  
  # Verificar que exista la red
  if(!tipo %in% names(redes)) next
  
  if(!anio %in% names(redes[[tipo]])) next
  
  red <- redes[[tipo]][[anio]]
  
  datos <- metricas_nodos %>%
    filter(
      Tipo == tipo,
      Año == as.numeric(anio)
    )
  
  # Si no hay datos para esa red, pasar a la siguiente
  if(nrow(datos) == 0) next
  
  graficar_red(
    red = red,
    metricas = datos,
    titulo = graficos$Titulo[i],
    archivo = graficos$Archivo[i]
  )
  
}

#======================================================
# FIN
#======================================================

cat("\n=====================================\n")
cat("VISUALIZACIONES GENERADAS\n")
cat("=====================================\n\n")

print(list.files("graficos"))

