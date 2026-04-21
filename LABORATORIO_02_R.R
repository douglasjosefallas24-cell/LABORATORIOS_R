#========================================================
# Laboratorio 02
# Douglas Fallas Mora
# version 1 20/04/2026
#========================================================

#Cargar paquetes ----

library(readxl)
library(dplyr)
library(ggplot2)
library(naniar)
library(VIM)
library(mice)
library(writexl)

# Importar archivos ----
datos_original <- read_excel("C:/Users/Douglas Fallas/Downloads/datos_vivero.xlsx", sheet = "Data")

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

count(datos_corregidos, especie, sort = TRUE)
count(datos_corregidos, tratamiento, sort = TRUE)
count(datos_corregidos, planta, sort = TRUE)
count(datos_corregidos, bloque, sort = TRUE)

datos_corregidos <-  datos_corregidos %>%
  mutate(
    tratamiento = case_when(
      tratamiento %in% c("Nitrogeno_Potasio","control") ~ "Nitrogeno_Potasio",
      TRUE ~ tratamiento
    )
  )

summary(datos_corregidos)

# Eliminar datos imposibles----

datos_corregidos <-  datos_corregidos %>%
  mutate(
    altura_cm = if_else(altura_cm < 0, NA_real_, altura_cm),
    diametro_base_mm = if_else(diametro_base_mm <= 0, NA_real_, diametro_base_mm)
  )

# Ver valores faltantes ----

conteo_na <- sapply(datos_corregidos, function(x) sum(is.na(x)))
porc_na   <- round(100 * sapply(datos_corregidos, function(x) mean(is.na(x))), 2)

resumen_na <- data.frame(
  variable = names(conteo_na),
  n_faltantes = conteo_na,
  porcentaje = porc_na
)

# Ver patron de faltantes en gráfico----

vis_miss(datos_corregidos)

# Variables para imputacion ----

vars_imputar <- datos_corregidos %>%
  select(
   altura_cm,
   diametro_base_mm
  )

summary(vars_imputar)

# Revisar variables con muy poca variacion ----

n_unicos <- sapply(vars_imputar, function(x) length(unique(na.omit(x))))
print(n_unicos)

# Eliminar variables con 1 o 0 valores distintos observados
vars_imputar <- vars_imputar[, n_unicos > 1, drop = FALSE]

# Revisar correlacion entre variables numericas 
# Esto ayuda a detectar colinealidad extrema.

cor_mat <- cor(vars_imputar, use = "pairwise.complete.obs")
print(round(cor_mat, 3))

# Comparacion con una imputacion simple (media) ----

datos_media <- vars_imputar

for (i in seq_along(datos_media)) {
  if (is.numeric(datos_media[[i]])) {
    datos_media[[i]][is.na(datos_media[[i]])] <- mean(datos_media[[i]], na.rm = TRUE)
  }
}

summary(datos_media)

# Comparar distribucion original vs imputada por media

# Comparación para la variable altura
  ggplot() +
    geom_density(
      data = vars_imputar,
      aes(x = altura_cm),
      na.rm = TRUE,
      linewidth = 1
    ) +
    geom_density(
      data = datos_media,
      aes(x = altura_cm),
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = "Comparacion de Altura_cm",
      subtitle = "Linea continua: datos observados | linea discontinua: imputacion por media",
      x = "Altura (cm)",
      y = "Diametro a la base (mm)"
    ) +
    theme_minimal()

# Comparación para la variable diametro a la base
  ggplot() +
    geom_density(
      data = vars_imputar,
      aes(x = diametro_base_mm),
      na.rm = TRUE,
      linewidth = 1
    ) +
    geom_density(
      data = datos_media,
      aes(x = diametro_base_mm),
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = "Comparacion de Diámetro a la base",
      subtitle = "Linea continua: datos observados | linea discontinua: imputacion por media",
      x = "Diametro a la base (mm)",
      y = "nose que es)"
    ) +
    theme_minimal()
  
# Imputacion multiple con PMM ----

# Definir metodo para todas las variables
metodos <- rep("pmm", ncol(vars_imputar))
names(metodos) <- names(vars_imputar)

# Construir matriz de predictores
pred <- make.predictorMatrix(vars_imputar)

# Evitar que cada variable se use a si misma
diag(pred) <- 0

# Reducir riesgo de singularidad 

cor_altas <- which(abs(cor_mat) > 0.98 & abs(cor_mat) < 1, arr.ind = TRUE)

if (nrow(cor_altas) > 0) {
  for (i in seq_len(nrow(cor_altas))) {
    fila <- rownames(cor_mat)[cor_altas[i, 1]]
    col  <- colnames(cor_mat)[cor_altas[i, 2]]
    pred[fila, col] <- 0
  }
}

print(pred)

# Ejecutar imputacion ----
set.seed(123)

imp <- mice(
  vars_imputar,
  m = 5,                
  method = metodos,
  predictorMatrix = pred,
  maxit = 10,
  seed = 123,
  printFlag = TRUE
)

# Resumen del objeto de imputacion ----
print(imp)

# Patron final de una de las bases imputadas
md.pattern(complete(imp, 1))

# Diagnosticos de imputacion ----

# Convergencia
plot(imp)

# Comparacion de distribuciones observadas vs imputadas
densityplot(imp)

# Dispersion de valores imputados
stripplot(imp, pch = 20, cex = 0.8)

# Extraer una base imputada ----

datos_pmm <- complete(imp, 1)

# Reintegrar variables originales no imputadas
datos_final <- bind_cols(
  datos_corregidos %>% select(especie, tratamiento, planta, bloque, semana, fecha_medicion),
  datos_pmm
)

# Verificar faltantes despues de imputar
faltantes_finales <- sapply(datos_final, function(x) sum(is.na(x)))
print(faltantes_finales)

# Comparar original vs PMM en una variable ----

# Altura
ggplot() +
  geom_density(
    data = vars_imputar,
    aes(x = altura_cm),
    na.rm = TRUE,
    linewidth = 1
  ) +
  geom_density(
    data = datos_pmm,
    aes(x = altura_cm),
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Altura (cm): observado vs PMM",
    subtitle = "Continua = observado | discontinua = imputado con PMM",
    x = "Altura (cm)",
    y = "y"
  ) +
  theme_minimal()

 # Diametro a la base
ggplot() +
  geom_density(
    data = vars_imputar,
    aes(x = diametro_base_mm),
    na.rm = TRUE,
    linewidth = 1
  ) +
  geom_density(
    data = datos_pmm,
    aes(x = diametro_base_mm),
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Diámetro a la base mm): observado vs PMM",
    subtitle = "Continua = observado | discontinua = imputado con PMM",
    x = "Diámetro a la base (mm)",
    y = "y"
  ) +
  theme_minimal()

# Boxplot comparativo para altura
bind_rows(
  vars_imputar %>% mutate(Metodo = "Observado"),
  datos_media %>% mutate(Metodo = "Media"),
  datos_pmm %>% mutate(Metodo = "PMM")
) %>%
  ggplot(aes(x = Metodo, y = altura_cm)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Comparacion de metodos de imputacion",
    y = "Altura (cm)"
  )
# Boxplot comparativo para diámetro
bind_rows(
  vars_imputar %>% mutate(Metodo = "Observado"),
  datos_media %>% mutate(Metodo = "Media"),
  datos_pmm %>% mutate(Metodo = "PMM")
) %>%
  ggplot(aes(x = Metodo, y = diametro_base_mm)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Comparacion de metodos de imputacion",
    y = "Diámetro a la base (mm)"
  )

# Resultados solicitados ----

     # Tabla resumen

resumen <- datos_corregidos %>%
  group_by(tratamiento, semana) %>%
  summarise(
    # Altura
    altura_prom = mean(altura_cm, na.rm = TRUE),
    altura_sd = sd(altura_cm, na.rm = TRUE),
    altura_se = altura_sd / sqrt(sum(!is.na(altura_cm))),
    
    # Diámetro basal
    diam_prom = mean(diametro_base_mm, na.rm = TRUE),
    diam_sd = sd(diametro_base_mm, na.rm = TRUE),
    diam_se = diam_sd / sqrt(sum(!is.na(diametro_base_mm)))
  ) %>%
  ungroup()
  
      # Gráfico del incremento en altura por tratameinto
ggplot(resumen, aes(x = semana, y = altura_prom, color = tratamiento)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  
  # Barras de error (± error estándar)
  geom_errorbar(aes(ymin = altura_prom - altura_se,
                    ymax = altura_prom + altura_se),
                width = 0.2) +
  
  labs(
    title = "Altura promedio semanal por tratamiento",
    x = "Semana",
    y = "Altura promedio (cm)",
    color = "Tratamiento"
  ) +
  
  theme_minimal()

# Gráfico del incremento en diámetro por tratameinto
ggplot(resumen, aes(x = semana, y = diam_prom, color = tratamiento)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  
  # Barras de error (± error estándar)
  geom_errorbar(aes(ymin = diam_prom - diam_se,
                    ymax = diam_prom + diam_se),
                width = 0.2) +
  
  labs(
    title = "Diámetro basal promedio semanal por tratamiento",
    x = "Semana",
    y = "Diámetro basal (cm)",
    color = "Tratamiento"
  ) +
  
  theme_minimal()

     # Cuadro comparativo Na

cuadro_largo <- tibble(
  variable = rep(c("altura_cm", "diametro_base_mm"), each = 2),
  momento = rep(c("Antes", "Después"), times = 2),
  
  Na = c(
    sum(is.na(datos_corregidos$altura_cm)),
    sum(is.na(datos_final$altura_cm)),
    sum(is.na(datos_corregidos$diametro_base_mm)),
    sum(is.na(datos_final$diametro_base_mm))
  ),
  
  promedio = c(
    mean(datos_corregidos$altura_cm, na.rm = TRUE),
    mean(datos_final$altura_cm, na.rm = TRUE),
    mean(datos_corregidos$diametro_base_mm, na.rm = TRUE),
    mean(datos_final$diametro_base_mm, na.rm = TRUE)
  )
) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

cuadro_largo

""

# FIN
