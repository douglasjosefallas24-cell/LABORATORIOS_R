#========================================================
# Laboratorio 04
# Douglas Fallas Mora
# 29/05/2026
#========================================================

#Cargar paquetes ----

library(readxl)
library(ggplot2)
library(tidyr)
library(factoextra)
library(vegan)
library(corrplot)
library(ggrepel)
library(dplyr)

# Importar archivos ----
datos_original <- read_excel("D:/SEMESTRES/Semestre 9/R/LABORATORIOS/LAB_04/04_respiracion_suelo_bosques.xlsx")

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

# Definición de grupos de variables ----

vars_quimicas <- c(
  "p_h_suelo",
  "organic_matter_percent",
  "soil_c_percent",
  "soil_n_percent",
  "c_n_ratio"
)

vars_fisicas <- c(
  "soil_temp_c",
  "soil_moisture_percent",
  "bulk_density_g_cm3"
)

vars_biologicas <- c(
  "soil_respiration_umol_m_2_s_1",
  "microbial_biomass_c_mg_kg",
  "enzyme_activity_index",
  "decomposition_rate_percent",
  "soil_fauna_count"
)

vars_estructurales <- c(
  "successional_age_yr",
  "basal_area_m2_ha",
  "tree_density_ind_ha_1",
  "canopy_cover_percent",
  "species_richness",
  "shannon_index",
  "litter_mass_mg_ha_1",
  "litter_depth_cm",
  "fine_root_biomass_g_m2"
)

vars_respuesta <- c(
  "high_respiration"
)


# Verificación de variables ----

vars_all <- c(
  vars_biologicas,
  vars_estructurales,
  vars_fisicas,
  vars_quimicas,
  vars_respuesta,
  "plot_id",
  "site_name",
  "block",
  "land_use_class"
)

vars_faltantes <- setdiff(vars_all, names(datos))

if(length(vars_faltantes) > 0){
  stop(
    "Estas variables no existen en la base: ",
    paste(vars_faltantes, collapse = ", ")
  )
}

#  Matriz de correlación ----

datos_corr <- datos %>%
  dplyr::select(dplyr::all_of(c(vars_quimicas, vars_biologicas, vars_fisicas, vars_estructurales))) %>%
  stats::na.omit()

matriz_cor <- stats::cor(
  datos_corr,
  use = "complete.obs"
)

corrplot::corrplot(
  matriz_cor,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  tl.col = "black"
)

# Resumen descriptivo de la base ----
 
resumen_biologicas <- datos %>%
  group_by(site_name, land_use_class) %>%
  summarise(
    across(
      all_of(vars_biologicas),
      ~ round(mean(., na.rm = TRUE), 2),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

resumen_quimicas <- datos %>%
  group_by(site_name, land_use_class) %>%
  summarise(
    across(
      all_of(vars_quimicas),
      ~ round(mean(., na.rm = TRUE), 2),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

resumen_fisicas <- datos %>%
  group_by(site_name, land_use_class) %>%
  summarise(
    across(
      all_of(vars_fisicas),
      ~ round(mean(., na.rm = TRUE), 2),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

resumen_estructurales <- datos %>%
  group_by(site_name, land_use_class) %>%
  summarise(
    across(
      all_of(vars_estructurales),
      ~ round(mean(., na.rm = TRUE), 2),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

library(writexl)

write_xlsx(
  list(
    "Resumen biologicas" = resumen_biologicas,
    "Resumen quimicas" = resumen_quimicas,
    "Resumen fisicas" = resumen_fisicas,
    "Resumen estructurales" = resumen_estructurales
  ),
  path = "D:/SEMESTRES/Semestre 9/R/LABORATORIOS/LAB_04/Resumenes.xlsx"
)

# Preparación de datos para PCA ----

pca_vars <- c(
  "p_h_suelo",
  "organic_matter_percent",
  "soil_c_percent",
  "soil_n_percent",
  "c_n_ratio",
  "soil_temp_c",
  "soil_moisture_percent",
  "bulk_density_g_cm3",
  "soil_respiration_umol_m_2_s_1",
  "microbial_biomass_c_mg_kg",
  "enzyme_activity_index",
  "decomposition_rate_percent",
  "soil_fauna_count",
  "successional_age_yr",
  "basal_area_m2_ha",
  "tree_density_ind_ha_1",
  "canopy_cover_percent",
  "species_richness",
  "shannon_index",
  "litter_mass_mg_ha_1",
  "litter_depth_cm",
  "fine_root_biomass_g_m2"
)

datos_pca_full <- datos %>%
  dplyr::select(
    plot_id,
    site_name,
    block,
    land_use_class,
    high_respiration,
    dplyr::all_of(pca_vars)
  ) %>%
  stats::na.omit()

datos_pca <- datos_pca_full %>%
  dplyr::select(dplyr::all_of(pca_vars))


# Análisis de Componentes Principales, PCA ----

pca <- stats::prcomp(
  datos_pca,
  center = TRUE,
  scale. = TRUE
)

summary(pca)

pca_importancia <- summary(pca)$importance
pca_importancia

pca$rotation

pca_scores <- as.data.frame(pca$x)

pca_scores <- pca_scores %>%
  dplyr::mutate(
    plot_id = datos_pca_full$plot_id,
    site_name = datos_pca_full$site_name,
    block = datos_pca_full$block,
    land_use_class = datos_pca_full$land_use_class,
    high_respiration = datos_pca_full$high_respiration
  )


# Gráficos del PCA ----

factoextra::fviz_eig(
  pca,
  addlabels = TRUE,
  ylim = c(0, 60)
) +
  labs(title = "Varianza explicada por los componentes principales")

factoextra::fviz_pca_biplot(
  pca,
  habillage = pca_scores$site_name,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  repel = TRUE,
  col.var = "black"
) +
  labs(title = "PCA de variables")

factoextra::fviz_pca_ind(
  pca,
  habillage = pca_scores$site_name,
  addEllipses = TRUE,
  repel = TRUE
) +
  labs(title = "Ordenación de parcelas según PCA")

factoextra::fviz_pca_var(
  pca,
  col.var = "contrib",
  gradient.cols = c("gray70", "gray30", "black"),
  repel = TRUE
) +
  labs(title = "Contribución de variables al PCA")

factoextra::fviz_contrib(
  pca,
  choice = "var",
  axes = 1,
  top = 10
) +
  labs(title = "Variables con mayor contribución al PC1")

factoextra::fviz_contrib(
  pca,
  choice = "var",
  axes = 2,
  top = 10
) +
  labs(title = "Variables con mayor contribución al PC2")


# Preparación de datos para conglomerados ----

cluster_vars <- c(
  "p_h_suelo",
  "organic_matter_percent",
  "soil_c_percent",
  "soil_n_percent",
  "c_n_ratio",
  "soil_temp_c",
  "soil_moisture_percent",
  "bulk_density_g_cm3",
  "soil_respiration_umol_m_2_s_1",
  "microbial_biomass_c_mg_kg",
  "enzyme_activity_index",
  "decomposition_rate_percent",
  "soil_fauna_count",
  "successional_age_yr",
  "basal_area_m2_ha",
  "tree_density_ind_ha_1",
  "canopy_cover_percent",
  "species_richness",
  "shannon_index",
  "litter_mass_mg_ha_1",
  "litter_depth_cm",
  "fine_root_biomass_g_m2"
)

datos_cluster_full <- datos %>%
  dplyr::select(
    plot_id,
    site_name,
    block,
    land_use_class,
    high_respiration,
    dplyr::all_of(cluster_vars)
  ) %>%
  stats::na.omit()

datos_cluster <- datos_cluster_full %>%
  dplyr::select(dplyr::all_of(cluster_vars))

datos_cluster_scaled <- scale(datos_cluster)


# Análisis de conglomerados jerárquicos ----

dist_cluster <- stats::dist(
  datos_cluster_scaled,
  method = "euclidean"
)

cluster_h <- stats::hclust(
  dist_cluster,
  method = "ward.D2"
)

plot(
  cluster_h,
  main = "Dendrograma de parcelas según perfil multivariado",
  xlab = "",
  sub = ""
)

factoextra::fviz_dend(
  cluster_h,
  k = 3,
  rect = TRUE,
  show_labels = FALSE,
  main = "Agrupamiento jerárquico de parcelas"
)

grupo_cluster <- stats::cutree(
  cluster_h,
  k = 3
)

datos_cluster_resultado <- datos_cluster_full %>%
  dplyr::mutate(
    Cluster = as.factor(grupo_cluster)
  )

# FIN