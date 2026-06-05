#========================================================
# Laboratorio 05
# Douglas Fallas Mora
# 04/06/2026
#========================================================

#Cargar paquetes ----

install.packages("car")
install.packages("MuMIn")
install.packages("writexl")

library(MuMIn)
library(readxl)
library(ggplot2)
library(tidyr)
library(factoextra)
library(vegan)
library(corrplot)
library(ggrepel)
library(dplyr)
library(janitor)
library(stringr)
library(car)
library(writexl)

# Importar archivos ----
datos_original <- read_excel("D:/SEMESTRES/Semestre 9/R/LABORATORIOS/LAB_05/04_respiracion_suelo_bosques.xlsx")

# Exploración inicial ----
head(datos_original)
summary(datos_original)
dim(datos_original)

# Limpiar nombres de columnas ----

datos <-  datos_original %>%
  clean_names()

names(datos)

# Estandarizar caracteres ----

datos <-  datos %>%
  mutate(across(where(is.character), str_trim))

count(datos, plot_id, sort = TRUE)
count(datos, site_name, sort = TRUE)
count(datos, block, sort = TRUE)
count(datos, land_use_class, sort = TRUE)

datos <-  datos %>%
  mutate(
    site_name = case_when(
      site_name %in% c("finca_NORTE","Finca norte") ~ "Finca_norte",
      site_name %in% c("sendero este","Sendero Este") ~ "Sendero_este",
      site_name %in% c("CUENCA_MEDIA","Cuenca media") ~ "Cuenca_media",
      site_name %in% c("Reserva Sur","reserva sur") ~ "Reserva_sur",
      TRUE ~ site_name
  )
)
 datos <-  datos %>%
   mutate(
  land_use_class = case_when(
    land_use_class %in% c("Area degradada","degraded area","DEGRADED_AREA","Degraded","área degradada") ~ "Area_degradada",
    land_use_class %in% c("Primary forest","primary forest","PRIMARY FOREST","Bosque primario","PRIMARY_Forest","primry forest") ~ "Bosque_primario",
    land_use_class %in% c("Bosque secundario","bosque secundario","secondary forest","Secondary forest","SECONDARY_FOREST") ~ "Bosque_secundario",
    TRUE ~ land_use_class
  )
)
 
 datos <-  datos %>%
   mutate(
     high_respiration = case_when(
       high_respiration %in% c("YES","Yes") ~ "Si",
       high_respiration %in% c("No") ~ "No",
       TRUE ~ high_respiration
     )
   )

summary(datos)


# Preparación básica de la base ----

datos <- datos %>%
  dplyr::mutate(
    plot_id = as.factor(plot_id),
    site_name = factor(
      site_name,
      levels = c("Finca_norte", "Sendero_este", "Cuenca_media","Reserva_sur")
    ),
    block = as.factor(block),
    land_use_class = factor(
      land_use_class,
      levels = c("Area_degradada", "Bosque_primario", "Bosque_secundario")
    ),
    high_respiration = factor(
      high_respiration,
      levels = c("Si","No")
    )
  )

# Poner valores de texto en valor NA ----

sapply(datos, class)

columnas <- c("soil_respiration_umol_m_2_s_1", "soil_moisture_percent", "p_h_suelo")

datos <- datos %>%
  mutate(across(
    all_of(columnas),
    ~ suppressWarnings(as.numeric(.))
  ))


# Correlaciones Pearson ----

correlaciones <- datos %>%
  

select(where(is.numeric)) %>%
  
  summarise(
    across(
      everything(),
      ~ cor(.,
            soil_respiration_umol_m_2_s_1,
            use = "complete.obs",
            method = "pearson")
    )
  ) %>%
  
  # Tabla
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "correlacion_pearson"
  ) %>%
  
  arrange(desc(abs(correlacion_pearson)))

# Selección de variables ----

# En este paso se selecionaron als variables predictoras con mayor correlación con la 
# variable respuesta (respiración del suelo), además se selecionaron según clasificación 
# por naturaleza de variable, con el fin que no de redundancia de variables que aporten a una multicolinealidad

variables <-  c(
 "basal_area_m2_ha", # variable dasométrica
  "shannon_index", # variable de diversidad del bosuqe
  "soil_temp_c", # variable fisica del suelo
  "organic_matter_percent", # variable quimica del suelo
  "microbial_biomass_c_mg_kg" # variable bilógica del suelo
)

datos_largos <- datos %>%
  
  select(
    soil_respiration_umol_m_2_s_1,
    all_of(variables)
  ) %>%
  
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "valor"
  )

etiquetas <- c(
  basal_area_m2_ha = "Área~basal~(m^2~ha^{-1})",
  microbial_biomass_c_mg_kg = "Biomasa~microbiana~(mg~kg^{-1})",
  organic_matter_percent = "Materia~orgánica~('%')",
  shannon_index = "Índice~de~Shannon",
  soil_temp_c = "Temperatura~del~suelo~(degree*C)"
)

ggplot(datos_largos,
       aes(x = valor,
           y = soil_respiration_umol_m_2_s_1)) +
  
  geom_point(alpha = 0.7) +
  
  geom_smooth(method = "lm", se = TRUE) +
  
  facet_wrap(
    ~ variable,
    scales = "free_x",
    labeller = as_labeller(etiquetas, label_parsed)
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(size = 11, face = "bold")
  ) +
  
  labs(
    x = "Variable predictora",
    y = expression("Respiración del suelo"~(mu*mol~m^-2~s^-1))
  )

# Cuadro de correlaciones de variables seleccionadas
cuadro_correlaciones_modelo <- correlaciones %>%
  
  filter(variable %in% c(
    "basal_area_m2_ha",
    "shannon_index",
    "soil_temp_c",
    "organic_matter_percent",
    "microbial_biomass_c_mg_kg"
  )) %>%
  mutate(
    variable = recode(
      variable,
      "basal_area_m2_ha" = "Área basal (m² ha⁻¹)",
      "shannon_index" = "Índice de Shannon",
      "soil_temp_c" = "Temperatura del suelo (°C)",
      "organic_matter_percent" = "Materia orgánica (%)",
      "microbial_biomass_c_mg_kg" = "Biomasa microbiana (mg kg⁻¹)"
    ),
    correlacion_pearson =
      round(correlacion_pearson, 4)
  ) %>%
  
  arrange(desc(abs(correlacion_pearson)))

cuadro_correlaciones_modelo


# Revisión de colineidad ----

modelo_vif <- lm(
  
  soil_respiration_umol_m_2_s_1 ~
    basal_area_m2_ha +
    shannon_index +
    soil_temp_c +
    organic_matter_percent +
    microbial_biomass_c_mg_kg,
  
  data = datos
)

vif(modelo_vif)

# Hacer cuadro de análisis de colaneidad

valores_vif <- vif(modelo_vif)
names(valores_vif) <- c(
  "Área basal (m² ha⁻¹)",
  "Índice de Shannon",
  "Temperatura del suelo (°C)",
  "Materia orgánica (%)",
  "Biomasa microbiana (mg kg⁻¹)"
)

# Crear cuadro
cuadro_vif <- data.frame(
  
  Variable = names(valores_vif),
  
  VIF = round(valores_vif, 2)
)

# Interpretación
cuadro_vif <- cuadro_vif %>%
  
  mutate(
    
    Interpretacion = case_when(
      
      VIF < 3 ~ "Baja colinealidad",
      
      VIF >= 3 & VIF < 5 ~
        "Colinealidad moderada",
      
      VIF >= 5 ~
        "Colinealidad relativamente alta"
    )
  )


# Modelos cantidatos a selección ----

# Modelo 1: Dasometrica
m1 <- lm(
  soil_respiration_umol_m_2_s_1 ~ basal_area_m2_ha,
  data = datos
)

# Modelo 2: Ecológica
m2 <- lm(
  soil_respiration_umol_m_2_s_1 ~ shannon_index,
  data = datos
)

# Modelo 3: Física
m3 <- lm(
  soil_respiration_umol_m_2_s_1 ~ soil_temp_c,
  data = datos
)

# Modelo 4: Química
m4 <- lm(
  soil_respiration_umol_m_2_s_1 ~ organic_matter_percent,
  data = datos
)

# Modelo 5: Biológica
m5 <- lm(
  soil_respiration_umol_m_2_s_1 ~ microbial_biomass_c_mg_kg,
  data = datos
)  

# Modelo 6: dasométrica + ecológica + física + química + biológica
m6 <- lm(
  soil_respiration_umol_m_2_s_1 ~ basal_area_m2_ha + shannon_index  + soil_temp_c + organic_matter_percent + microbial_biomass_c_mg_kg,
  data = datos
)

# Modelo 7: física + química + biológica
m7 <- lm(
  soil_respiration_umol_m_2_s_1 ~ soil_temp_c + organic_matter_percent + microbial_biomass_c_mg_kg,
  data = datos
)


AIC(m1, m2, m3, m4, m5, m6, m7)

# Tabla de selección con MuMIn ----
modelos <- list(
  "Modelo dasométrico" = m1,
  "Modelo ecológico" = m2,
  "Modelo físicos" = m3,
  "Modelo químicos" = m4,
  "Modelo biológicos" = m5,
  "Modelo integredo" = m6,
  "Modelo variables del suelo" = m7
)

tabla_modelos <- model.sel(modelos)

tabla_modelos


# Convertir tabla a data frame
tabla_modelos_df <- as.data.frame(tabla_modelos)

tabla_modelos_df

# Tabla comparativa de modelos mediante AIC o BIC, con parámetros y R2 ajustado ----
# Selección del modelo más apto para el caso ----
modelos <- list(
  "Modelo dasométrico" = m1,
  "Modelo ecológico" = m2,
  "Modelo físicos" = m3,
  "Modelo químicos" = m4,
  "Modelo biológicos" = m5,
  "Modelo integredo" = m6,
  "Modelo variables del suelo" = m7
)

# Tabla de selección con AICc
tabla_modelos <- model.sel(modelos)

# Convertir a data.frame
tabla_final <- as.data.frame(tabla_modelos)

# Agregar nombre del modelo
tabla_final$Modelo <- rownames(tabla_final)

# Calcular R2 ajustado para cada modelo
r2_ajustado <- sapply(
  modelos,
  function(x) summary(x)$adj.r.squared
)

# Agregar R2 ajustado respetando el orden de model.sel()
tabla_final$R2_Ajustado <- r2_ajustado[tabla_final$Modelo]

# Seleccionar y ordenar columnas
tabla_final <- tabla_final %>%
  select(
    Modelo,
    Parametros = df,
    AICc,
    Delta_AICc = delta,
    Peso_Akaike = weight,
    R2_Ajustado
  ) %>%
  mutate(
    AICc = round(AICc, 2),
    Delta_AICc = round(Delta_AICc, 2),
    Peso_Akaike = round(Peso_Akaike, 3),
    R2_Ajustado = round(R2_Ajustado, 3)
  )

tabla_final

# Verificación de modelo final ----

modelo_final <- m7

# Resumen del modelo

summary(modelo_final)

coeficientes <- summary(modelo_final)$coefficients

tabla_coeficientes <- data.frame(
  
  Variable = rownames(coeficientes),
  
  Coeficiente_Estimado = round(coeficientes[,1], 4),
  
  Error_Estandar = round(coeficientes[,2], 4),
  
  Estadistico_t = round(coeficientes[,3], 4),
  
  Valor_p = round(coeficientes[,4], 4)
)

# Agregar interpretación de significancia
tabla_coeficientes <- tabla_coeficientes %>%
  
  mutate(
    
    Significancia = case_when(
      
      Valor_p < 0.001 ~ "*** Muy significativo",
      
      Valor_p < 0.01 ~ "** Significativo",
      
      Valor_p < 0.05 ~ "* Significativo",
      
      TRUE ~ "No significativo"
    )
  )

tabla_coeficientes

# Cuadros resultados ----

write_xlsx(
  list(
    "CUADRO_VIF" = cuadro_vif,
    "CUADRO_CORRELACIONES" = cuadro_correlaciones_modelo,
    "CUADRO_MODELOS" = tabla_final
  ),
  
  "D:/SEMESTRES/Semestre 9/R/LABORATORIOS/LAB_05/CUADROS_RESULTADOS.xlsx"
)
# FIN