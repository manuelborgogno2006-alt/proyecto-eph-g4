
#VISUALIZACION DE LA CNO
view(CNO)


#FILTRO DONDE SOLO NOS INTERESAN LAS CATEGORIAS
cno_categoria <- CNO %>%
  filter(variable == "Categoría")




#PEQUEÑA AYUDA PARA SIMPLIFICAR LAS OCUPACIONES
diccionario_categorias <- tibble(
  categoria = c(
    "00","01","02","03","04","05","06","07",
    "10","11","20","30","31","32","33","34","35","36",
    "40","41","42","43","44","45","46","47","48","49",
    "50","51","52","53","54","55","56","57","58",
    "60","61","62","63","64","65",
    "70","71","72",
    "80","81","82",
    "90","91","92"
  ),
  
  nombre_categoria = c(
    
    # DIRECCION Y GESTION
    "Poder ejecutivo",
    "Poder legislativo",
    "Poder judicial",
    "Directivos estatales",
    "Directivos instituciones sociales",
    "Directivos pequeñas empresas",
    "Directivos medianas empresas",
    "Directivos grandes empresas",
    
    "Gestion administrativa",
    "Gestion juridica",
    "Gestion contable y financiera",
    
    # COMERCIALIZACION Y TRANSPORTE
    "Comercializacion directa",
    "Corretaje y ventas",
    "Comercializacion indirecta",
    "Venta ambulante",
    "Transporte",
    "Telecomunicaciones",
    "Almacenaje y logistica",
    
    # SERVICIOS PROFESIONALES Y PUBLICOS
    "Salud",
    "Educacion",
    "Investigacion",
    "Asesoria y consultoria",
    "Prevencion y medio ambiente",
    "Comunicacion y medios",
    "Servicios sociales y comunitarios",
    "Vigilancia y seguridad",
    "Servicios policiales",
    "Fuerzas armadas",
    
    # SERVICIOS PERSONALES
    "Arte",
    "Deporte",
    "Recreacion",
    "Servicios gastronomicos",
    "Turismo y alojamiento",
    "Servicio domestico",
    "Limpieza no domestica",
    "Cuidado de personas",
    "Servicios sociales varios",
    
    # PRODUCCION PRIMARIA
    "Produccion agricola",
    "Produccion ganadera",
    "Produccion forestal",
    "Produccion avicola y apicola",
    "Produccion pesquera",
    "Caza",
    
    # ENERGIA Y CONSTRUCCION
    "Produccion extractiva",
    "Energia, agua y gas",
    "Construccion e infraestructura",
    
    # INDUSTRIA Y TECNOLOGIA
    "Produccion industrial",
    "Produccion de software",
    "Reparacion bienes de consumo",
    
    # MANTENIMIENTO Y TECNOLOGIA
    "Mantenimiento de maquinaria",
    "Desarrollo tecnologico",
    "Instalacion y mantenimiento"
  )
)




# UNIR TABLAS

cno_categoria <- cno_categoria %>%
  
  left_join(
    diccionario_categorias,
    by = c("value" = "categoria")
  )
