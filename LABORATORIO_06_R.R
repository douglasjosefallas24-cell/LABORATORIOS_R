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
datos_original <- read_excel("D:/SEMESTRES/Semestre 9/R/LABORATORIOS/LAB_06/04_respiracion_suelo_bosques.xlsx")

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
    all_of(variables),
    land_use_class
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

# Gráfico propuesto en el Laboratorio 5 ----

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

# Mejora del gráfico propuesto en Lab 5  ----
ecuaciones <- datos_largos %>%
  group_by(variable) %>%
  summarise(
    modelo = list(
      lm(soil_respiration_umol_m_2_s_1 ~ valor)
    ),
    intercepto = coef(modelo[[1]])[1],
    pendiente = coef(modelo[[1]])[2],
    r2_ajustado = summary(modelo[[1]])$adj.r.squared,
    .groups = "drop"
  ) %>%
  mutate(
    etiqueta = paste0(
      "y = ",
      round(intercepto, 3),
      ifelse(
        pendiente >= 0,
        " + ",
        " - "
      ),
      round(abs(pendiente), 3),
      "x",
      "\nR² = ",
      round(r2_ajustado, 3)
    )
  )

grafico <- ggplot(
  datos_largos,
  aes(
    x = valor,
    y = soil_respiration_umol_m_2_s_1,
    color = land_use_class
  )
) +
  
  geom_point(
    alpha = 0.8,
    size = 2.5
  ) +
  
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "blue",
    fill = "#7ba9d1",
    linewidth = 1,
    alpha = 0.2
  ) +
  
  geom_text(
    data = ecuaciones,
    aes(
      x = -Inf,
      y = Inf,
      label = etiqueta
    ),
    hjust = -0.1,
    vjust = 1.2,
    size = 3,
    color = "black",
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  
  facet_wrap(
    ~ variable,
    scales = "free_x",
    labeller = as_labeller(
      etiquetas,
      label_parsed
    ),
    ncol = 3
  ) +
  
  scale_color_manual(
    values = c(
      "Area_degradada" = "#659965",
      "Bosque_secundario" = "#50d64f",
      "Bosque_primario" = "#0c700a"
    ),
    
    labels = c(
      "Área degradada",
      "Bosque primario",
      "Bosque secundario"
    ),
    
    name = "Uso del suelo"
  ) +
  
  theme_bw() +
  
  theme(
    legend.position = "bottom",
    strip.background = element_rect(
      fill = "gray90",
      color = "gray60"
    ),
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    axis.title = element_text(
      size = 13,
      face = "bold"
    ),
    axis.text = element_text(
      size = 11,
      color = "gray20"
    ),
    legend.title = element_text(
      size = 12,
      face = "bold"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "gray88",
      linewidth = 0.3
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5
    )
    
    
  ) +
  
  labs(
    subtitle = "Línea azul: regresión lineal ajustada; área sombreada: intervalo de confianza al 95%",
    x = "Variables predictoras",
    y = expression(
      "Respiración del suelo"~
        (mu*mol~m^-2~s^-1)
    )
  )

grafico

ggsave(
  filename = "D:/SEMESTRES/Semestre 9/R/LABORATORIOS/LAB_06/GRAFICO_REGRESIONES_MEJORADO.png",
  plot = grafico,
  width = 11,
  height = 7,
  dpi = 300
)
# FIN