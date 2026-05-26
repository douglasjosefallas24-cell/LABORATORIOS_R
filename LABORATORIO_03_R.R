#========================================================
# Laboratorio 03
# Douglas Fallas Mora
# 25/05/2026
#========================================================

#Cargar paquetes ----

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(janitor)
library(stringr)
library(performance)
library(agricolae)

# Importar archivos ----
datos_original <- read_excel("D:/SEMESTRES/Semestre 9/R/LABORATORIOS/LAB_03/secado_melina.xlsx")

# Exploración inicial ----
head(datos_original)
summary(datos_original)
dim(datos_original)

# Limpiar nombres de columnas ----

datos_corregidos <-  datos_original %>%
  clean_names()

names(datos_corregidos)

# Homogenizar caracteres----

datos_corregidos <-  datos_corregidos %>%
  mutate(across(where(is.character), str_trim))

count(datos_corregidos, id_pieza, sort = TRUE)
count(datos_corregidos, proceso_produccion, sort = TRUE)
count(datos_corregidos, metodo_secado, sort = TRUE)

# Comprobar supuestos ----

# Normalidad 
modelo <- aov(calidad_pct ~ proceso_produccion * metodo_secado, data = datos_corregidos)

residuos <- residuals(modelo)

shapiro.test(residuos)

#Homocedasticidad

leveneTest(calidad_pct ~ proceso_produccion * metodo_secado, data = datos_corregidos)

# Análisis de cada variable por separado ----

# ANOVA por variable (Proceso de produccion),(Método de secado) y (Proceso de produccion * Método de secado)

# Calidad (%)
modelo_cal_int <- aov(calidad_pct ~ proceso_produccion * metodo_secado, data = datos_corregidos)
summary(modelo_cal_int)
check_normality(modelo_cal_int)
check_heteroscedasticity(modelo_cal_int)

# Curvaturas
modelo_cur_int <- aov(presencia_curvatura ~ proceso_produccion * metodo_secado, data = datos_corregidos)
summary(modelo_cur_int)
check_normality(modelo_cur_int)
check_heteroscedasticity(modelo_cur_int)

# Rajaduras
modelo_raj_int <- aov(presencia_rajadura ~ proceso_produccion * metodo_secado, data = datos_corregidos)
summary(modelo_raj_int)
check_normality(modelo_raj_int)
check_heteroscedasticity(modelo_raj_int)

# ANOVA de dos vías de la variable deseada (Calidad) con Interacción ----
modelo_aov <- aov(calidad_pct ~ proceso_produccion * metodo_secado, data = datos_corregidos)
summary(modelo_aov)

check_normality(modelo_aov)
check_heteroscedasticity(modelo_aov)

# Medias por combinación de factores
datos_corregidos %>%
  group_by(proceso_produccion, metodo_secado) %>%
  summarise(
    n = n(),
    media_calidad_pct = mean(calidad_pct),
    sd_calidad = sd(calidad_pct),
    .groups = "drop"
  )

# Resultados ----

# Gráfico de interacción ----
interaction.plot(
  x.factor = datos_corregidos$proceso_produccion,
  trace.factor = datos_corregidos$metodo_secado,
  response = datos_corregidos$calidad_pct,
  fun = mean,
  type = "b",
  pch = 19,
  xlab = "Proceso de produccion",
  ylab = "Calidad promedio (%)",
  trace.label = "Metodo secado"
)

# Gráfico con Tukey ----

tukey <- HSD.test(
  modelo_aov,
  trt = c("proceso_produccion", "metodo_secado"),
  group = TRUE
)

datos_corregidos <- datos_corregidos %>%
  mutate(
    tratamiento = interaction(
      proceso_produccion,
      metodo_secado,
      sep = ":"
    )
  )

resumen <- datos_corregidos %>%
  group_by(tratamiento) %>%
  summarise(
    media = mean(calidad_pct, na.rm = TRUE),
    se = sd(calidad_pct, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

letras <- tukey$groups %>%
  tibble::rownames_to_column("tratamiento")

resumen <- left_join(
  resumen,
  letras,
  by = "tratamiento"
)

ggplot(resumen,
       aes(x = tratamiento,
           y = media,
           fill = tratamiento)) +
  
  geom_col() +
  
  geom_errorbar(
    aes(
      ymin = media - se,
      ymax = media + se
    ),
    width = 0.2
  ) +
  
  geom_text(
    aes(
      label = groups,
      y = media + se + 4
    ),
    size = 6
  ) +
  
  labs(
    x = "Tratamiento",
    y = "Calidad promedio (%)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Grafico de presencia de defectos ----

datos_long <- datos_corregidos %>%
  pivot_longer(
    cols = c(presencia_curvatura, presencia_rajadura),
    names_to = "defecto",
    values_to = "presencia"
  ) %>%
  filter(presencia == 1)

ggplot(datos_long,
       aes(x = interaction(proceso_produccion, metodo_secado),
           fill = defecto)) +
  
  geom_bar(position = position_dodge(width = 0.9)) +
  
  stat_count(
    aes(label = after_stat(count)),
    geom = "text",
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 5
  ) +
  
  labs(
    x = "Tratamiento",
    y = "Frecuencia",
    fill = "Defecto"
  ) +
  
  scale_x_discrete(
    labels = function(x) gsub("\\.", " - ", x)
  ) +
  
  scale_fill_manual(
    values = c("presencia_curvatura" = "salmon",
               "presencia_rajadura" = "turquoise3"),
    
    labels = c("Curvatura", "Rajadura")
  ) +
  
  theme_minimal(base_size = 14)

# Cuadro resumen de ANOVA para calidad (%)

anova_calidad <- as.data.frame(summary(modelo_aov)[[1]])

anova_calidad <- anova_calidad %>%
  tibble::rownames_to_column("Fuente de variación")

anova_calidad <- anova_calidad %>%
  mutate(
    `F value` = round(`F value`, 3),
    `Pr(>F)` = round(`Pr(>F)`, 4)
  )

anova_calidad

write_xlsx(
  anova_calidad,
  "D:/SEMESTRES/Semestre 9/R/LABORATORIOS/LAB_03/Cuadro_ANOVA_Calidad.xlsx"
)

# FIN

