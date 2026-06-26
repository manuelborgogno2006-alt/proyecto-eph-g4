# =====================================================
# LIBRERÍAS
# =====================================================

library(dplyr)
library(ggplot2)

# =====================================================
# 1. TOP 10 COOCCURRENCIAS POR AÑO
# =====================================================

top10 <- tabla_aristas %>%
  
  #identificador único de coocurrencia
  mutate(
    cooc = paste(
      pmin(origen, destino),
      pmax(origen, destino),
      sep = "_"
    )
  ) %>%
  
  # seleccionar top 10 por año
  group_by(year) %>%
  slice_max(peso, n = 10, with_ties = FALSE) %>%
  ungroup()

# =====================================================
# 2. AGREGAR NOMBRES DE CATEGORÍAS
# =====================================================

top10 <- top10 %>%
  
  # nombre origen
  left_join(
    cno_categoria %>%
      select(value, nombre_categoria),
    by = c("origen" = "value")
  ) %>%
  
  rename(origen_nombre = nombre_categoria) %>%
  
  # nombre destino
  left_join(
    cno_categoria %>%
      select(value, nombre_categoria),
    by = c("destino" = "value")
  ) %>%
  
  rename(destino_nombre = nombre_categoria)

# =====================================================
# 3. CREAR ETIQUETA LEGIBLE
# =====================================================

top10 <- top10 %>%
  mutate(
    etiqueta = paste(
      origen_nombre,
      destino_nombre,
      sep = " ↔ "
    )
  )

# =====================================================
# 4. RANKING DENTRO DE CADA AÑO
# =====================================================

top10_rank <- top10 %>%
  
  group_by(year) %>%
  
  arrange(desc(peso), .by_group = TRUE) %>%
  
  mutate(
    rank = row_number()
  ) %>%
  
  ungroup()

# =====================================================
# 5. GRAFICAR SLOPE CHART(coodenadas paralelas)
# =====================================================


ggplot(
  top10_rank,
  aes(
    x = factor(year),
    y = rank,
    group = cooc,
    color = etiqueta
  )
) +
  
  # líneas
  geom_line(
    linewidth = 3,
    alpha = 0.8
  ) +
  
  # nodos
  geom_point(
    size = 8
  ) +
  
  scale_y_reverse(
    breaks = 1:10
  ) +
  
  scale_color_manual(
    name = "Coocurrencias",
    values = c(
      "#E41A1C",
      "#377EB8",
      "#4DAF4A",
      "#984EA3",
      "#FF7F00",
      "#FFFF33",
      "#A65628",
      "#F781BF",
      "#999999",
      "#66C2A5",
      "#FC8D62",
      "#8DA0CB",
      "#E78AC3"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 11
    ),
    
    axis.text = element_text(
      size = 11
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    panel.grid.minor = element_blank(),
    
    # leyenda
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold",
      size = 11
    ),
    
    legend.text = element_text(
      size = 9
    )
  ) +
  
  labs(
    x = "Año",
    y = "Ranking"
  )
