#CODIGOS LIMPIOS


# =====================================================
# LIBRERIAS
# =====================================================

library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(ggraph)

# =====================================================
# 1. SELECCION DE HOGARES
# =====================================================

base_hogares_ocupados <- base_completa %>%
  
  select(
    CODUSU,
    NRO_HOGAR,
    year,
    trimestre,
    CH04,
    CH06,
    ESTADO,
    IX_TOT,
    PP04D_COD
  ) %>%
  
  # identificador unico del hogar
  mutate(
    id_hogar = paste(
      CODUSU,
      NRO_HOGAR,
      year,
      trimestre,
      sep = "_"
    )
  ) %>%
  
  group_by(id_hogar) %>%
  
  # hogares con 2 o más miembros
  filter(first(IX_TOT) >= 2) %>%
  
  # hogares con al menos 2 ocupados
  filter(sum(ESTADO == 1, na.rm = TRUE) >= 2) %>%
  
  # conservar solo ocupados
  filter(ESTADO == 1) %>%
  
  ungroup()


# =====================================================
# CANTIDAD DE HOGARES POR PERIODO
# =====================================================

hogares_periodo <- base_hogares_ocupados %>%
  
  distinct(id_hogar, year) %>% 
  
  count(year, name = "cantidad_hogares")

hogares_periodo






# =====================================================
# 2. DESAGREGAR CODIGO OCUPACIONAL
# =====================================================

base_hogares_ocupados2 <- base_hogares_ocupados %>%
  
  mutate(
    
    PP04D_COD = as.character(PP04D_COD),
    
    PP04D_COD = str_pad(
      PP04D_COD,
      width = 5,
      side = "left",
      pad = "0"
    ),
    
    categoria    = substr(PP04D_COD, 1, 2),
    jerarquia    = substr(PP04D_COD, 3, 3),
    tecnologia   = substr(PP04D_COD, 4, 4),
    calificacion = substr(PP04D_COD, 5, 5)
  )

# =====================================================
# 3. OCUPACIONES POR HOGAR
# =====================================================

ocupaciones_hogar <- base_hogares_ocupados2 %>%
  
  select(
    id_hogar,
    year,
    categoria
  ) %>%
  
  distinct()

# =====================================================
# 4. COOCCURRENCIAS OCUPACIONALES
# =====================================================

pares_ocupaciones <- ocupaciones_hogar %>%
  
  inner_join(
    ocupaciones_hogar,
    by = c("id_hogar", "year"),
    suffix = c("_1", "_2")
  ) %>%
  
  # eliminar autocoincidencias
  filter(categoria_1 != categoria_2) %>%
  
  # ordenar pares
  mutate(
    cat_min = pmin(categoria_1, categoria_2),
    cat_max = pmax(categoria_1, categoria_2)
  ) %>%
  
  # una sola coocurrencia por hogar
  distinct(
    year,
    id_hogar,
    cat_min,
    cat_max
  )

# =====================================================
# 5. TABLA DE ARISTAS POR AÑO
# =====================================================

tabla_aristas <- pares_ocupaciones %>%
  
  count(
    year,
    cat_min,
    cat_max,
    sort = TRUE
  ) %>%
  
  rename(
    origen = cat_min,
    destino = cat_max,
    peso = n
  ) %>%
  
  # eliminar coocurrencias muy raras
  filter(peso >= 5)

# =====================================================
# 6. CREAR REDES POR PERIODO
# =====================================================

# -------------------
# RED 2016
# -------------------

red_2016 <- tabla_aristas %>%
  filter(year == 2016) %>%
  select(origen, destino, peso) %>%
  arrange(peso)

grafo_2016 <- graph_from_data_frame(
  d = red_2016,
  directed = FALSE
)

# -------------------
# RED 2021
# -------------------

red_2021 <- tabla_aristas %>%
  filter(year == 2021) %>%
  select(origen, destino, peso) %>%
  arrange(peso)

grafo_2021 <- graph_from_data_frame(
  d = red_2021,
  directed = FALSE
)

# -------------------
# RED 2025
# -------------------

red_2025 <- tabla_aristas %>%
  filter(year == 2025) %>%
  select(origen, destino, peso) %>%
  arrange(peso)

grafo_2025 <- graph_from_data_frame(
  d = red_2025,
  directed = FALSE
)

# =====================================================
# 7. INFORMACION BASICA
# =====================================================

grafo_2016
grafo_2021
grafo_2025

# cantidad de nodos
vcount(grafo_2016)
vcount(grafo_2021)
vcount(grafo_2025)

# cantidad de aristas
ecount(grafo_2016)
ecount(grafo_2021)
ecount(grafo_2025)

# =====================================================
# 8. CENTRALIDAD
# =====================================================

# -------------------
# GRADO
# -------------------

degree(grafo_2016)
degree(grafo_2021)
degree(grafo_2025)


# =====================================================
# 9. DENSIDAD DE RED
# =====================================================

edge_density(grafo_2016)
edge_density(grafo_2021)
edge_density(grafo_2025)



# =====================================================
# 11. VISUALIZACIONES
# =====================================================

# -------------------
# RED 2016
# -------------------



# fuerza nodos
V(grafo_2016)$fuerza <- strength(
  grafo_2016,
  weights = E(grafo_2016)$peso
)


#GRAFICO
ggraph(grafo_2016, layout = "fr") +
  
  # aristas
  geom_edge_link(
    aes(
      width = peso,
      color = peso
    ),
    show.legend = TRUE
  ) +
  
  # nodos
  geom_node_point(
    aes(
      size = fuerza,
      color = fuerza
    )
  ) +
  
  # etiquetas
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 4
  ) +
  
  # escala nodos
  scale_color_gradient(
    low = "#b7e4f9",
    high = "#03045e",
    name = "Coocurrencia en los hogares"
  ) +
  
  # escala aristas
  scale_edge_color_gradient(
    low = "grey85",
    high = "black",
    name = "Cantidad de hogares"
  ) +
  
  scale_edge_width(
    range = c(0.5, 3),
    guide = "none"
  ) +
  
  scale_size(
    range = c(4, 12),
    guide = "none"
  ) +
  
  theme_void() +
  
  theme(
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  ) +
  
  ggtitle("Red de coocurrencia ocupacional - 2016")



# -------------------
# RED 2021
# -------------------


# fuerza nodos
V(grafo_2021)$fuerza <- strength(
  grafo_2021,
  weights = E(grafo_2021)$peso
)


#GRAFICO
ggraph(grafo_2021, layout = "fr") +
  
  # aristas
  geom_edge_link(
    aes(
      width = peso,
      color = peso
    ),
    show.legend = TRUE
  ) +
  
  # nodos
  geom_node_point(
    aes(
      size = fuerza,
      color = fuerza
    )
  ) +
  
  # etiquetas
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 4
  ) +
  
  # escala nodos
  scale_color_gradient(
    low = "#b7e4f9",
    high = "#03045e",
    name = "Coocurrencia en los hogares"
  ) +
  
  # escala aristas
  scale_edge_color_gradient(
    low = "grey85",
    high = "black",
    name = "Cantidad de hogares"
  ) +
  
  scale_edge_width(
    range = c(0.5, 3),
    guide = "none"
  ) +
  
  scale_size(
    range = c(4, 12),
    guide = "none"
  ) +
  
  theme_void() +
  
  theme(
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  ) +
  
  ggtitle("Red de coocurrencia ocupacional - 2021")



# -------------------
# RED 2025
# -------------------


# fuerza nodos
V(grafo_2025)$fuerza <- strength(
  grafo_2025,
  weights = E(grafo_2025)$peso
)


#GRAFICO
ggraph(grafo_2025, layout = "fr") +
  
  # aristas
  geom_edge_link(
    aes(
      width = peso,
      color = peso
    ),
    show.legend = TRUE
  ) +
  
  # nodos
  geom_node_point(
    aes(
      size = fuerza,
      color = fuerza
    )
  ) +
  
  # etiquetas
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 4
  ) +
  
  # escala nodos
  scale_color_gradient(
    low = "#b7e4f9",
    high = "#03045e",
    name = "Coocurrencia en los hogares"
  ) +
  
  # escala aristas
  scale_edge_color_gradient(
    low = "grey85",
    high = "black",
    name = "Cantidad de hogares"
  ) +
  
  scale_edge_width(
    range = c(0.5, 3),
    guide = "none"
  ) +
  
  scale_size(
    range = c(4, 12),
    guide = "none"
  ) +
  
  theme_void() +
  
  theme(
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  ) +
  
  ggtitle("Red de coocurrencia ocupacional - 2025")


