#PAQUETES CON LOS QUE SE TRABAJA

library(tidyverse)
library(eph)
library(dplyr)
library(purrr)
library(tidyr)


#SELECCION DE PERIODOS 3ER TRIMESTRE: 2016, 2021, 2025
periodos <- tribble(
  ~year, ~trimestre,
  2016, 3,
  2021, 3,
  2025, 3
)




#BASE INDIVIDUAL DATOS
individual <- pmap_dfr(
  periodos,
  function(year, trimestre) {
    
    get_microdata(
      year = year,
      period = trimestre,
      type = "individual"
    ) %>%
      mutate(
        year = year,
        trimestre = trimestre
      )
    
  }
)




#BASE HOGARES DATOS
hogares <- pmap_dfr(
  periodos,
  function(year, trimestre) {
    
    get_microdata(
      year = year,
      period = trimestre,
      type = "hogar"
    ) %>%
      mutate(
        year = year,
        trimestre = trimestre
      )
    
  }
)


#CODOSU, CODIGO DEL HOGAR
names(hogares)
glimpse(hogares)

names(individual)
glimpse(individual)



#BASES UNIDAS :  HOGARES-INDIVIDUOS
base_completa <- individual %>%
  left_join(
    hogares,
    by = c(
      "CODUSU",
      "NRO_HOGAR",
      "year",
      "trimestre"
    )
  )

glimpse(base_completa)
names(base_completa)


#CANTIDAD DE HOGARES EN CADA PERIODO EXAMINADO
hogares %>%
  count(year, trimestre)

#PONDERACION DE LOS HOGARES EN LA POBLACION 
hogares %>%
  group_by(year, trimestre) %>%
  summarise(
    hogares_muestra = n(),  
    hogares_expandidos = sum(PONDERA, na.rm = TRUE) 
  )

#N DE HOGARES * LA SUMA DE TODAS LAS PONDERACIONES
#Entonces PONDERA indica cuántos hogares de la población representa ese hogar de la muestra.
#PUEDE SER QUE SE REALIZARON MENOS ENCUESTAS DE UN PERIODO A OTRO, NO NECESARIAMENTE QUE SE REDUJERON LOS HOGARES

hogares %>%
  group_by(year, REALIZADA) %>%
  summarise(n = n())




#TAMAÑO PROMEDIO Y MEDIANA DEL HOGAR EN CADA PERIODO
hogares %>%
  group_by(year, trimestre) %>%
  summarise(
    promedio = mean(IX_TOT),
    mediana = median(IX_TOT)
  )




#HOGARES UNIPERSONALES
hogares %>%
  filter(IX_TOT == 1) %>%
  count(year, trimestre)

#PROPORCION DE HOGARES UNIPERSONALES EN CADA PERIODO INVESTIGADO
hogares %>%
  group_by(year, trimestre) %>%
  summarise(
    hogares_totales = n(),
    hogares_unipersonales = sum(IX_TOT == 1),
    porcentaje = hogares_unipersonales / hogares_totales * 100
  )




#DISTRIBUCION DEL TAMAÑO DE LOS HOGARES
ggplot(hogares,
       aes(x = factor(IX_TOT), fill = factor(year))) +
  geom_bar(color = "white", linewidth = 0.2) +
  facet_wrap(~year) +
  labs(
    title = "Distribución del tamaño de los hogares",
    subtitle = "EPH - Tercer trimestre de 2016, 2021 y 2025",
    x = "Cantidad de miembros del hogar",
    y = "Cantidad de hogares",
    fill = "Año"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey40"),
    strip.text = element_blank(),      
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


ggsave(
  "tamano_hogares.png",
  width = 12,
  height = 5,
  dpi = 300
)



hogares %>%
  count(year, IX_TOT) %>%
  group_by(year) %>%
  mutate(
    porcentaje = n / sum(n) * 100
  )


#GRAFICO % DEL TAMAÑO DE LOS HOGARES EN 
hogares_graf <- hogares %>%
  mutate(
    tam_hogar = case_when(
      IX_TOT == 1 ~ "1",
      IX_TOT == 2 ~ "2",
      IX_TOT == 3 ~ "3",
      IX_TOT == 4 ~ "4",
      IX_TOT >= 5 ~ "5 o más"
    )
  ) %>%
  count(year, tam_hogar) %>%
  group_by(year) %>%
  mutate(
    porcentaje = n / sum(n) * 100
  )



##

hogares_graf %>%
  ggplot(aes(x = 2, y = porcentaje, fill = tam_hogar)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#80B1D3",
      "2" = "#FDB462",
      "3" = "#FB8072",
      "4" = "#B3DE69",
      "5 o más" = "#BC80BD"
    )
  ) +
  facet_wrap(~ year) +
  theme_void() +
  theme(
    strip.text = element_text(size = 16, face = "bold")
  ) +
  labs(fill = "Tamaño del hogar")



ggsave(
  "hogares_porcentual.png",
  width = 15,
  height = 5,
  dpi = 300
)

getwd()



#CODIGOS DE REGIONES Y AGLOMERADOS SEGUN LA EPH
diccionario_regiones
diccionario_aglomerados
centroides_aglomerados


#REGIONES Y AGLOMERADOS DONDE FUERON HECHAS LAS ENCUESTAS Y SUS CANTIDADES
hogares %>%
  count(REGION)

hogares %>%
  count(AGLOMERADO)







