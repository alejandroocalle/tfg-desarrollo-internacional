# =============================================================================
# TFG: Análisis multivariante del desarrollo económico y social internacional
# -----------------------------------------------------------------------------
# Reduce la dimensión con PCA y agrupa países con k-means.
# Datos:   World Development Indicators (Banco Mundial), año 2017.
# Salida:  Las 13 figuras del trabajo en la carpeta figures/
#
# Nota: el script descarga los datos en directo del Banco Mundial mediante
# el paquete WDI, por lo que no requiere ningún archivo local. Como el WDI
# actualiza retrospectivamente sus datos, los valores absolutos pueden
# diferir ligeramente de los reportados en el TFG original.
# Autor: Alejandro Calle Fernández-Cañamaque
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PAQUETES
# -----------------------------------------------------------------------------
# Si es la primera vez, ejecutar UNA SOLA VEZ:
# install.packages(c("WDI","dplyr","tidyr","ggplot2","reshape2","cluster",
#                    "scales","tibble","e1071"))

library(WDI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(cluster)
library(scales)
library(tibble)
library(e1071)

set.seed(123)
dir.create("figures", showWarnings = FALSE)


# -----------------------------------------------------------------------------
# 1. DESCARGA ROBUSTA DE LOS 5 INDICADORES
# -----------------------------------------------------------------------------
indicador_codigos <- c(
  pib_pc     = "NY.GDP.PCAP.KD",
  internet   = "IT.NET.USER.ZS",
  mortalidad = "SH.DYN.MORT",
  poblacion  = "SP.POP.GROW",
  urbana     = "SP.URB.TOTL.IN.ZS"
)

descargar_seguro <- function(codigo, nombre) {
  for (intento in 1:5) {
    cat("Descargando", nombre, "(intento", intento, ")...\n")
    df <- try(WDI(country = "all", indicator = codigo,
                  start = 2010, end = 2018, extra = TRUE),
              silent = TRUE)
    if (!inherits(df, "try-error") && codigo %in% names(df)) {
      names(df)[names(df) == codigo] <- nombre
      return(df)
    }
    Sys.sleep(3)
  }
  stop("No se pudo descargar ", nombre)
}

lista_dfs <- mapply(descargar_seguro,
                    indicador_codigos, names(indicador_codigos),
                    SIMPLIFY = FALSE)

datos_wdi <- lista_dfs[[1]] %>%
  select(iso3c, country, region, year, all_of(names(indicador_codigos)[1]))

for (i in 2:length(lista_dfs)) {
  datos_wdi <- datos_wdi %>%
    left_join(lista_dfs[[i]] %>%
                select(iso3c, year, all_of(names(indicador_codigos)[i])),
              by = c("iso3c", "year"))
}

# Quitar agregados regionales (Mundo, UE, etc.)
datos_wdi <- datos_wdi %>% filter(region != "Aggregates")


# -----------------------------------------------------------------------------
# 2. FIGURA 0 — Cobertura por año (justifica la elección del 2017)
# -----------------------------------------------------------------------------
cobertura <- datos_wdi %>%
  rowwise() %>%
  mutate(n_completas = sum(!is.na(c_across(c(pib_pc, internet, mortalidad,
                                             poblacion, urbana))))) %>%
  ungroup() %>%
  filter(n_completas == 5) %>%
  count(year, name = "n_paises")

fig0 <- ggplot(cobertura, aes(x = year, y = n_paises)) +
  geom_col(fill = "#1F3864", width = 0.7) +
  geom_col(data = subset(cobertura, year == 2017),
           fill = "#C00000", width = 0.7) +
  geom_text(aes(label = n_paises), vjust = -0.4, size = 4) +
  scale_x_continuous(breaks = 2010:2018) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Cobertura del conjunto de datos por año",
       subtitle = "Número de países con información completa en las 5 variables",
       x = "Año", y = "Número de países") +
  theme_minimal(base_size = 12)

ggsave("figures/figura_0_cobertura_anos.png", fig0,
       width = 8, height = 5, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# 3. DATOS DE 2017 + FIGURA A (valores faltantes por país)
# -----------------------------------------------------------------------------
datos_2017 <- datos_wdi %>%
  filter(year == 2017) %>%
  select(iso3c, country, pib_pc, internet, mortalidad, poblacion, urbana)

missings_por_pais <- datos_2017 %>%
  rowwise() %>%
  mutate(n_missing = sum(is.na(c_across(pib_pc:urbana)))) %>%
  ungroup() %>%
  count(n_missing)

figA <- ggplot(missings_por_pais, aes(x = factor(n_missing), y = n)) +
  geom_col(fill = "#C65911", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.4, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribución del número de variables incompletas por país",
       subtitle = "Año 2017",
       x = "Nº de variables sin información",
       y = "Nº de países") +
  theme_minimal(base_size = 12)

ggsave("figures/figura_A_missings_pais.png", figA,
       width = 8, height = 5, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# 4. PAÍSES COMPLETOS (eliminación por listas)
# -----------------------------------------------------------------------------
datos <- datos_2017 %>%
  filter(complete.cases(select(., pib_pc:urbana)))

cat("\nPaíses con información completa:", nrow(datos), "\n")


# -----------------------------------------------------------------------------
# 5. ESTADÍSTICOS DESCRIPTIVOS (Tabla 1)
# -----------------------------------------------------------------------------
estadisticos <- datos %>%
  select(pib_pc:urbana) %>%
  summarise(across(everything(), list(
    min     = ~min(.),
    q1      = ~quantile(., 0.25),
    mediana = ~median(.),
    media   = ~mean(.),
    q3      = ~quantile(., 0.75),
    max     = ~max(.),
    dt      = ~sd(.),
    asim    = ~skewness(.)
  ))) %>%
  pivot_longer(everything(),
               names_to = c("variable","estadistico"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = estadistico, values_from = value)

print(estadisticos)


# -----------------------------------------------------------------------------
# 6. FIGURAS 1a y 1b — distribuciones
# -----------------------------------------------------------------------------
datos_long <- datos %>%
  select(pib_pc:urbana) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  mutate(variable = factor(variable,
    levels = c("pib_pc","internet","mortalidad","poblacion","urbana"),
    labels = c("PIB per cápita (USD)","Usuarios internet (%)",
               "Mortalidad infantil (‰)","Crec. poblacional (%)",
               "Población urbana (%)")))

fig1a <- ggplot(datos_long, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "#1F3864",
                 color = "white", alpha = 0.85) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Distribución de las variables originales",
       x = NULL, y = "Frecuencia") +
  theme_minimal(base_size = 11)

ggsave("figures/figura_1a_histogramas.png", fig1a,
       width = 10, height = 6, dpi = 300, bg = "white")

X   <- datos[, c("pib_pc","internet","mortalidad","poblacion","urbana")]
X_z <- as.data.frame(scale(X))

datos_z_long <- X_z %>%
  pivot_longer(everything(), names_to = "variable", values_to = "z") %>%
  mutate(variable = factor(variable,
    levels = c("pib_pc","internet","mortalidad","poblacion","urbana"),
    labels = c("PIB per cápita","Usuarios internet","Mortalidad infantil",
               "Crec. poblacional","Población urbana")))

fig1b <- ggplot(datos_z_long, aes(x = variable, y = z)) +
  geom_boxplot(fill = "#9DC3E6", outlier.color = "#C00000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(title = "Distribución de las variables estandarizadas",
       x = NULL, y = "Valor estandarizado (z)") +
  theme_minimal(base_size = 11)

ggsave("figures/figura_1b_boxplots.png", fig1b,
       width = 9, height = 5, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# 7. FIGURA 2 — Matriz de correlaciones (heatmap)
# -----------------------------------------------------------------------------
mat_corr <- cor(X)
print(round(mat_corr, 2))

mat_long <- melt(mat_corr)

fig2 <- ggplot(mat_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 4, color = "black") +
  scale_fill_gradient2(low = "#C00000", mid = "white", high = "#1F3864",
                       midpoint = 0, limits = c(-1, 1), name = "Corr.") +
  labs(title = "Matriz de correlaciones entre las cinco variables",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/figura_2_correlaciones.png", fig2,
       width = 7, height = 6, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# 8. PCA — Figuras 3, 4, 5
# -----------------------------------------------------------------------------
pca <- prcomp(X, center = TRUE, scale. = TRUE)
print(summary(pca))
print(round(pca$rotation, 3))

var_exp <- (pca$sdev^2) / sum(pca$sdev^2)
df_var  <- data.frame(
  PC         = paste0("PC", 1:length(var_exp)),
  individual = var_exp,
  acumulada  = cumsum(var_exp)
)

# Figura 3 — scree plot
fig3 <- ggplot(df_var, aes(x = PC)) +
  geom_col(aes(y = individual), fill = "#1F3864") +
  geom_line(aes(y = acumulada, group = 1), color = "#C00000", size = 1) +
  geom_point(aes(y = acumulada), color = "#C00000", size = 3) +
  geom_text(aes(y = individual,
                label = percent(individual, accuracy = 0.1)),
            vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, 1.05)) +
  labs(title = "Varianza explicada por las componentes principales",
       subtitle = "Barras: varianza individual | Línea: varianza acumulada",
       x = NULL, y = "Proporción de varianza") +
  theme_minimal(base_size = 11)

ggsave("figures/figura_3_scree.png", fig3,
       width = 8, height = 5, dpi = 300, bg = "white")

# Figura 4 — cargas (loadings)
df_load <- as.data.frame(pca$rotation[, 1:2]) %>%
  rownames_to_column("variable")

fig4 <- ggplot(df_load) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "#1F3864", size = 1) +
  geom_text(aes(x = PC1 * 1.15, y = PC2 * 1.15, label = variable),
            color = "#C00000", size = 4, fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  coord_fixed() +
  labs(title = "Cargas de las variables sobre PC1 y PC2",
       x = sprintf("PC1 (%.1f%%)", var_exp[1] * 100),
       y = sprintf("PC2 (%.1f%%)", var_exp[2] * 100)) +
  theme_minimal(base_size = 11)

ggsave("figures/figura_4_cargas.png", fig4,
       width = 7, height = 7, dpi = 300, bg = "white")

# Figura 5 — países proyectados en PC1-PC2
puntuaciones      <- as.data.frame(pca$x[, 1:2])
puntuaciones$pais <- datos$iso3c

fig5 <- ggplot(puntuaciones, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(color = "#1F3864", alpha = 0.6, size = 2) +
  geom_text(aes(label = pais), size = 2.5,
            vjust = -0.7, check_overlap = TRUE) +
  labs(title = "Países en el espacio definido por PC1 y PC2",
       x = sprintf("PC1 (%.1f%%)", var_exp[1] * 100),
       y = sprintf("PC2 (%.1f%%)", var_exp[2] * 100)) +
  theme_minimal(base_size = 11)

ggsave("figures/figura_5_paises_pca.png", fig5,
       width = 10, height = 7, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# 9. K-MEANS — Figuras 6, 7, 8, 9
# -----------------------------------------------------------------------------
# Figura 6 — método del codo
wss <- sapply(1:10, function(k) {
  kmeans(puntuaciones[, 1:2], centers = k, nstart = 25)$tot.withinss
})

df_codo <- data.frame(k = 1:10, wss = wss)

fig6 <- ggplot(df_codo, aes(x = k, y = wss)) +
  geom_line(color = "#1F3864", size = 1) +
  geom_point(color = "#1F3864", size = 3) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "#C00000") +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Método del codo: selección del nº de clusters",
       subtitle = "Línea roja: k = 4 (solución elegida)",
       x = "Número de clusters (k)",
       y = "Suma de cuadrados intra-cluster (WSS)") +
  theme_minimal(base_size = 11)

ggsave("figures/figura_6_codo.png", fig6,
       width = 8, height = 5, dpi = 300, bg = "white")

# Aplicar k-means con k = 4
km                   <- kmeans(puntuaciones[, 1:2], centers = 4, nstart = 25)
puntuaciones$cluster <- factor(km$cluster)
datos$cluster        <- factor(km$cluster)

cat("\nTamaño de cada cluster:\n")
print(table(datos$cluster))

# Figura 7 — clusters en PCA
colores_cluster <- c("#C00000","#548235","#1F3864","#7030A0")

fig7 <- ggplot(puntuaciones, aes(x = PC1, y = PC2, color = cluster)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 2.5, alpha = 0.8) +
  stat_ellipse(level = 0.8, size = 1) +
  scale_color_manual(values = colores_cluster) +
  labs(title = "Clusters de k-means en el espacio PC1-PC2 (k = 4)",
       x = sprintf("PC1 (%.1f%%)", var_exp[1] * 100),
       y = sprintf("PC2 (%.1f%%)", var_exp[2] * 100)) +
  theme_minimal(base_size = 11)

ggsave("figures/figura_7_clusters.png", fig7,
       width = 9, height = 6, dpi = 300, bg = "white")

# Tabla 5 — perfil medio de cada cluster
perfil <- datos %>%
  group_by(cluster) %>%
  summarise(across(pib_pc:urbana, mean), .groups = "drop") %>%
  mutate(across(pib_pc:urbana, ~ round(., 1)))

print(perfil)

# Figura 8 — perfil estandarizado
X_z_clu          <- as.data.frame(scale(X))
X_z_clu$cluster  <- datos$cluster

perfil_z <- X_z_clu %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean), .groups = "drop") %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "z") %>%
  mutate(variable = factor(variable,
    levels = c("pib_pc","internet","mortalidad","poblacion","urbana"),
    labels = c("PIB pc","Internet","Mortalidad","Crec.pob.","Urbano")))

fig8 <- ggplot(perfil_z, aes(x = variable, y = z, fill = cluster)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(yintercept = 0, color = "gray40") +
  scale_fill_manual(values = colores_cluster) +
  labs(title = "Perfil estandarizado de cada cluster",
       x = NULL, y = "Valor estandarizado (z)") +
  theme_minimal(base_size = 11)

ggsave("figures/figura_8_perfiles.png", fig8,
       width = 9, height = 5, dpi = 300, bg = "white")

# Figura 9 — silhouette
sil <- silhouette(km$cluster, dist(puntuaciones[, 1:2]))
cat("\nResumen del análisis de silhouette:\n")
print(summary(sil))

png("figures/figura_9_silhouette.png",
    width = 1800, height = 1200, res = 200, bg = "white")
plot(sil, col = colores_cluster, border = NA,
     main = "Análisis de silhouette por cluster")
dev.off()


# -----------------------------------------------------------------------------
# FIN
# -----------------------------------------------------------------------------
cat("\n=================================================\n")
cat("ANÁLISIS COMPLETADO\n")
cat("=================================================\n")
cat("Países analizados:", nrow(datos), "\n")
cat(sprintf("Varianza PC1 + PC2: %.1f%%\n",
            sum(var_exp[1:2]) * 100))
cat("Figuras guardadas en /figures/\n")
