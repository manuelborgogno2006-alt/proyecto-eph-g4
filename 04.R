# =====================================================
# PROYECTO FINAL
# Coocurrencia ocupacional en hogares argentinos
# =====================================================

# =====================================================
# 04. BASES PARA EL ANÁLISIS DE REDES
# =====================================================

# Este script genera las distintas bases que serán
# utilizadas para construir las redes de coocurrencia
# ocupacional.

# =====================================================
# BASE GENERAL
# =====================================================

base_general <- base_final

# =====================================================
# HOGARES DE BAJOS INGRESOS
# =====================================================

base_bajos <- base_final %>%
  
  filter(
    
    grupo_ingreso == "Bajo"
    
  )

# =====================================================
# HOGARES DE INGRESOS MEDIOS
# =====================================================

base_medios <- base_final %>%
  
  filter(
    
    grupo_ingreso == "Medio"
    
  )

# =====================================================
# HOGARES DE ALTOS INGRESOS
# =====================================================

base_altos <- base_final %>%
  
  filter(
    
    grupo_ingreso == "Alto"
    
  )

# =====================================================
# VERIFICACIÓN
# =====================================================

cat("\n")

cat("Base general:", nrow(base_general), "ocupados\n")

cat("Ingresos bajos:", nrow(base_bajos), "ocupados\n")

cat("Ingresos medios:", nrow(base_medios), "ocupados\n")

cat("Ingresos altos:", nrow(base_altos), "ocupados\n")

