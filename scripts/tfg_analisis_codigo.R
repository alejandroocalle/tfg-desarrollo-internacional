# =============================================================================
# TFG — Análisis multivariante del desarrollo económico y social internacional
# Autor: Alejandro Calle Fernández-Cañamaque
# Universidad Carlos III de Madrid — Grado en Estadística y Empresa 2025/2026
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PAQUETES
# -----------------------------------------------------------------------------
# Instalar la primera vez descomentando la línea siguiente:
# install.packages(c("WDI","dplyr","tidyr","ggplot2","reshape2","cluster",
#                    "scales","tibble","Rtsne","ggrepel"))

library(WDI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(cluster)
library(scales)
library(tibble)
library(Rtsne)
library(ggrepel)

set.seed(123)

COLORES_CLUSTER <- c("1" = "#E63946", "2" = "#2A9D8F", "3" = "#264653", "4" = "#E9C46A")

# -----------------------------------------------------------------------------
# 1. DESCARGA Y PREPARACIÓN DE DATOS
# -----------------------------------------------------------------------------

indicadores <- c(
  pib_pc       = "NY.GDP.PCAP.KD",
  internet     = "IT.NET.USER.ZS",
  mortalidad   = "SH.DYN.MORT",
  poblacion    = "SP.POP.GROW",
  urbana       = "SP.URB.TOTL.IN.ZS",
  esperanza    = "SP.DYN.LE00.IN",
  educacion    = "SE.XPD.TOTL.GD.ZS",
  desempleo    = "SL.UEM.TOTL.ZS",
  electricidad = "EG.ELC.ACCS.ZS"
)

cat("Descargando datos del Banco Mundial...\n")
datos_raw <- WDI(
  indicator = indicadores,
  start     = 2022,
  end       = 2022,
  extra     = TRUE
)

meta <- datos_raw %>%
  select(iso2c, country, region, income) %>%
  distinct()

agregados <- c("1A","1W","4E","7E","8S","B8","EU","F1","OE","S1","S2","S3",
               "S4","T2","T3","T4","T5","T6","T7","V1","V2","V3","V4","XC",
               "XD","XE","XF","XG","XH","XI","XJ","XK","XL","XM","XN","XO",
               "XP","XQ","XR","XS","XT","XU","XY","Z4","Z7","ZB","ZF","ZG",
               "ZH","ZI","ZJ","ZQ","ZT")

datos_paises <- datos_raw %>%
  filter(!iso2c %in% agregados, region != "Aggregates") %>%
  select(iso2c, country, all_of(names(indicadores)))

datos_completos <- datos_paises %>% drop_na()
n_paises <- nrow(datos_completos)
cat(sprintf("Países con información completa en las 9 variables (2022): %d\n", n_paises))

datos_completos <- datos_completos %>%
  left_join(meta, by = c("iso2c", "country"))

# -----------------------------------------------------------------------------
# 2. ESTANDARIZACIÓN
# -----------------------------------------------------------------------------

vars_numericas <- names(indicadores)

datos_z <- datos_completos %>%
  mutate(across(all_of(vars_numericas), scale)) %>%
  mutate(across(all_of(vars_numericas), as.numeric))

mat_z <- datos_z %>%
  select(all_of(vars_numericas)) %>%
  as.matrix()

rownames(mat_z) <- datos_completos$iso2c

# -----------------------------------------------------------------------------
# 3. ANÁLISIS EXPLORATORIO
# -----------------------------------------------------------------------------

etiquetas_vars <- c(
  pib_pc       = "PIB per cápita (USD)",
  internet     = "Usuarios internet (%)",
  mortalidad   = "Mortalidad infantil (permil)",
  poblacion    = "Crec. poblacional (%)",
  urbana       = "Población urbana (%)",
  esperanza    = "Esperanza de vida",
  educacion    = "Gasto educación (% PIB)",
  desempleo    = "Desempleo (%)",
  electricidad = "Acceso electricidad (%)"
)

# Estadísticos descriptivos
desc <- datos_completos %>%
  select(all_of(vars_numericas)) %>%
  summarise(across(everything(), list(
    min     = ~min(., na.rm = TRUE),
    q1      = ~quantile(., 0.25, na.rm = TRUE),
    mediana = ~median(., na.rm = TRUE),
    media   = ~mean(., na.rm = TRUE),
    q3      = ~quantile(., 0.75, na.rm = TRUE),
    max     = ~max(., na.rm = TRUE),
    dt      = ~sd(., na.rm = TRUE)
  )))
cat("\n--- Estadísticos descriptivos ---\n")
print(as.data.frame(desc))

# Figura 1a: Histogramas
datos_long <- datos_completos %>%
  select(all_of(vars_numericas)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor")

datos_long$variable <- factor(datos_long$variable,
                               levels = names(etiquetas_vars),
                               labels = etiquetas_vars)

fig1a <- ggplot(datos_long, aes(x = valor)) +
  geom_histogram(fill = "#264653", color = "white", bins = 25) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(title = "Distribución de las variables originales", x = NULL, y = "Frecuencia") +
  theme_minimal(base_size = 10)

ggsave("imagenes/figura_1a_histogramas.png", fig1a, width = 12, height = 10, dpi = 300)
cat("Figura 1a guardada.\n")

# Figura 1b: Boxplots estandarizados
datos_z_long <- datos_z %>%
  select(all_of(vars_numericas)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor_z")

datos_z_long$variable <- factor(datos_z_long$variable,
                                 levels = names(etiquetas_vars),
                                 labels = etiquetas_vars)

fig1b <- ggplot(datos_z_long, aes(x = valor_z, y = variable)) +
  geom_boxplot(fill = "#E9C46A", color = "#264653", outlier.size = 1) +
  labs(title = "Distribución de las variables estandarizadas",
       x = "Valor estandarizado (z)", y = NULL) +
  theme_minimal(base_size = 11)

ggsave("imagenes/figura_1b_boxplots.png", fig1b, width = 10, height = 6, dpi = 300)
cat("Figura 1b guardada.\n")

# Figura 2: Heatmap de correlaciones
mat_cor <- cor(datos_completos %>% select(all_of(vars_numericas)), use = "complete.obs")
colnames(mat_cor) <- etiquetas_vars
rownames(mat_cor) <- etiquetas_vars
cor_melt <- melt(mat_cor)

fig2 <- ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.8) +
  scale_fill_gradient2(low = "#E63946", mid = "white", high = "#264653",
                       midpoint = 0, limits = c(-1, 1), name = "Corr.") +
  labs(title = "Matriz de correlaciones entre las nueve variables") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

ggsave("imagenes/figura_2_correlaciones.png", fig2, width = 10, height = 9, dpi = 300)
cat("Figura 2 guardada.\n")

# -----------------------------------------------------------------------------
# 4. PCA
# -----------------------------------------------------------------------------

pca_resultado <- prcomp(mat_z, center = FALSE, scale. = FALSE)
varianza_exp  <- summary(pca_resultado)$importance[2, ]
varianza_acum <- summary(pca_resultado)$importance[3, ]
cargas        <- pca_resultado$rotation
scores        <- as.data.frame(pca_resultado$x)
scores$iso2c  <- datos_completos$iso2c
scores$pais   <- datos_completos$country

cat("\n--- Varianza explicada por componente ---\n")
tabla_var <- data.frame(
  Componente          = paste0("PC", 1:length(varianza_exp)),
  Varianza_individual = paste0(round(varianza_exp * 100, 1), "%"),
  Varianza_acumulada  = paste0(round(varianza_acum * 100, 1), "%")
)
print(tabla_var)

# Figura 3: Varianza explicada
var_df <- data.frame(
  componente = factor(paste0("PC", 1:9), levels = paste0("PC", 1:9)),
  individual = varianza_exp * 100,
  acumulada  = varianza_acum * 100
)

fig3 <- ggplot(var_df, aes(x = componente)) +
  geom_col(aes(y = individual), fill = "#264653", width = 0.6) +
  geom_line(aes(y = acumulada, group = 1), color = "#E63946", linewidth = 1.2) +
  geom_point(aes(y = acumulada), color = "#E63946", size = 3) +
  geom_text(aes(y = individual, label = paste0(round(individual, 1), "%")),
            vjust = -0.5, size = 3.2) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25),
                     labels = paste0(seq(0, 100, 25), "%")) +
  labs(title = "Varianza explicada por las componentes principales",
       subtitle = "Barras: varianza individual | Línea: varianza acumulada",
       x = NULL, y = "Proporción de varianza") +
  theme_minimal(base_size = 12)

ggsave("imagenes/figura_3_varianza_pca.png", fig3, width = 9, height = 5, dpi = 300)
cat("Figura 3 guardada.\n")

# Figura 4: Cargas de las variables sobre PC1 y PC2
cargas_df <- as.data.frame(cargas[, 1:2])
cargas_df$variable <- etiquetas_vars[rownames(cargas_df)]

fig4 <- ggplot(cargas_df, aes(x = PC1, y = PC2, label = variable)) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "#264653", linewidth = 0.8) +
  geom_label_repel(color = "#E63946", fontface = "bold", size = 3.2, max.overlaps = 20) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  labs(title = "Cargas de las variables sobre PC1 y PC2",
       x = paste0("PC1 (", round(varianza_exp[1]*100,1), "%)"),
       y = paste0("PC2 (", round(varianza_exp[2]*100,1), "%)")) +
  theme_minimal(base_size = 12)

ggsave("imagenes/figura_4_cargas_pca.png", fig4, width = 8, height = 7, dpi = 300)
cat("Figura 4 guardada.\n")

# Figura 5: Países en el espacio PCA
fig5 <- ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_point(color = "#264653", alpha = 0.7, size = 1.8) +
  geom_text_repel(aes(label = iso2c), size = 2.2, color = "#264653", max.overlaps = 30) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(title = "Países en el espacio definido por PC1 y PC2",
       x = paste0("PC1 (", round(varianza_exp[1]*100,1), "%)"),
       y = paste0("PC2 (", round(varianza_exp[2]*100,1), "%)")) +
  theme_minimal(base_size = 12)

ggsave("imagenes/figura_5_paises_pca.png", fig5, width = 11, height = 8, dpi = 300)
cat("Figura 5 guardada.\n")

# -----------------------------------------------------------------------------
# 5. CLUSTERING K-MEANS
# -----------------------------------------------------------------------------

# Figura 6: Método del codo
wss <- sapply(1:10, function(k) {
  kmeans(scores[, c("PC1","PC2")], centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

codo_df <- data.frame(k = 1:10, wss = wss)

fig6 <- ggplot(codo_df, aes(x = k, y = wss)) +
  geom_line(color = "#264653", linewidth = 1.2) +
  geom_point(color = "#264653", size = 3) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "#E63946", linewidth = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Método del codo: selección del nº de clusters",
       subtitle = "Línea roja: k = 4 (solución elegida)",
       x = "Número de clusters (k)", y = "Suma de cuadrados intra-cluster (WSS)") +
  theme_minimal(base_size = 12)

ggsave("imagenes/figura_6_codo.png", fig6, width = 8, height = 5, dpi = 300)
cat("Figura 6 guardada.\n")

# Aplicar k-means con k = 4
km <- kmeans(scores[, c("PC1","PC2")], centers = 4, nstart = 25, iter.max = 100)
scores$cluster <- factor(km$cluster)
datos_completos$cluster <- factor(km$cluster)

cat("\n--- Distribución de países por cluster ---\n")
print(table(scores$cluster))

# Figura 7: Clusters en el espacio PCA
fig7 <- ggplot(scores, aes(x = PC1, y = PC2, color = cluster)) +
  stat_ellipse(aes(fill = cluster), alpha = 0.12, geom = "polygon", level = 0.75) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = iso2c), size = 2, max.overlaps = 25) +
  scale_color_manual(values = COLORES_CLUSTER, name = "cluster") +
  scale_fill_manual(values = COLORES_CLUSTER, guide = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(title = "Clusters de k-means en el espacio PC1-PC2 (k = 4)",
       x = paste0("PC1 (", round(varianza_exp[1]*100,1), "%)"),
       y = paste0("PC2 (", round(varianza_exp[2]*100,1), "%)")) +
  theme_minimal(base_size = 12)

ggsave("imagenes/figura_7_clusters_pca.png", fig7, width = 11, height = 8, dpi = 300)
cat("Figura 7 guardada.\n")

# Tabla 5: Perfil medio por cluster
perfil_clusters <- datos_completos %>%
  group_by(cluster) %>%
  summarise(
    n            = n(),
    pib_pc       = round(mean(pib_pc), 0),
    internet     = round(mean(internet), 1),
    mortalidad   = round(mean(mortalidad), 1),
    poblacion    = round(mean(poblacion), 2),
    urbana       = round(mean(urbana), 1),
    esperanza    = round(mean(esperanza), 1),
    educacion    = round(mean(educacion, na.rm = TRUE), 1),
    desempleo    = round(mean(desempleo, na.rm = TRUE), 1),
    electricidad = round(mean(electricidad), 1)
  )

cat("\n--- Perfil medio por cluster ---\n")
print(as.data.frame(perfil_clusters))

# Figura 8: Perfil estandarizado de cada cluster
perfil_z <- datos_z %>%
  mutate(cluster = factor(km$cluster)) %>%
  group_by(cluster) %>%
  summarise(across(all_of(vars_numericas), mean))

perfil_z_long <- perfil_z %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "valor_z")

perfil_z_long$variable <- factor(perfil_z_long$variable,
                                  levels = names(etiquetas_vars),
                                  labels = etiquetas_vars)

fig8 <- ggplot(perfil_z_long, aes(x = variable, y = valor_z, fill = cluster)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = COLORES_CLUSTER, name = "cluster") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(title = "Perfil estandarizado de cada cluster",
       x = NULL, y = "Valor estandarizado (z)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave("imagenes/figura_8_perfil_clusters.png", fig8, width = 11, height = 6, dpi = 300)
cat("Figura 8 guardada.\n")

# Figura 9: Silhouette k-means
sil_km <- silhouette(km$cluster, dist(scores[, c("PC1","PC2")]))

png("imagenes/figura_9_silhouette_kmeans.png", width = 900, height = 600, res = 120)
plot(sil_km,
     col    = COLORES_CLUSTER[levels(scores$cluster)],
     border = NA,
     main   = "Análisis de silhouette por cluster (k-means)")
dev.off()

cat(sprintf("\nSilhouette medio global (k-means): %.2f\n", mean(sil_km[, "sil_width"])))

# -----------------------------------------------------------------------------
# 6. t-SNE
# -----------------------------------------------------------------------------

cat("\nAplicando t-SNE...\n")

tsne_resultado <- Rtsne(mat_z, dims = 2, perplexity = 30,
                         max_iter = 1000, check_duplicates = FALSE, verbose = FALSE)

tsne_df <- data.frame(
  tSNE1   = tsne_resultado$Y[, 1],
  tSNE2   = tsne_resultado$Y[, 2],
  iso2c   = datos_completos$iso2c,
  cluster = factor(km$cluster)
)

# Figura 10: Países en el espacio t-SNE
fig10 <- ggplot(tsne_df, aes(x = tSNE1, y = tSNE2)) +
  geom_point(color = "#264653", alpha = 0.7, size = 1.8) +
  geom_text_repel(aes(label = iso2c), size = 2.2, color = "#264653", max.overlaps = 30) +
  labs(title = "Representación de los países en el espacio t-SNE",
       subtitle = "Reducción no lineal — perplexity = 30, 1000 iteraciones",
       x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal(base_size = 12)

ggsave("imagenes/figura_10_paises_tsne.png", fig10, width = 11, height = 8, dpi = 300)
cat("Figura 10 guardada.\n")

# Figura 11: Clusters en el espacio t-SNE
fig11 <- ggplot(tsne_df, aes(x = tSNE1, y = tSNE2, color = cluster)) +
  stat_ellipse(aes(fill = cluster), alpha = 0.12, geom = "polygon", level = 0.75) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = iso2c), size = 2, max.overlaps = 25) +
  scale_color_manual(values = COLORES_CLUSTER, name = "cluster") +
  scale_fill_manual(values = COLORES_CLUSTER, guide = "none") +
  labs(title = "Clusters k-means representados en el espacio t-SNE",
       subtitle = "Validación de la estructura de grupos mediante reducción no lineal",
       x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal(base_size = 12)

ggsave("imagenes/figura_11_clusters_tsne.png", fig11, width = 11, height = 8, dpi = 300)
cat("Figura 11 guardada.\n")

# -----------------------------------------------------------------------------
# 7. CLUSTERING JERÁRQUICO
# -----------------------------------------------------------------------------

cat("\nAplicando clustering jerárquico...\n")

dist_mat    <- dist(scores[, c("PC1","PC2")], method = "euclidean")
hclust_ward <- hclust(dist_mat, method = "ward.D2")

# Figura 12: Dendrograma
png("imagenes/figura_12_dendrograma.png", width = 1400, height = 700, res = 120)
par(mar = c(8, 4, 4, 2))
plot(hclust_ward,
     labels = datos_completos$iso2c,
     cex    = 0.4,
     main   = "Dendrograma del clustering jerárquico (Ward D2)",
     xlab   = "",
     ylab   = "Altura (distancia)",
     hang   = -1)
rect.hclust(hclust_ward, k = 4,
            border = c("#E63946","#2A9D8F","#264653","#E9C46A"))
dev.off()
cat("Figura 12 guardada.\n")

# Comparación k-means vs jerárquico
grupos_hc    <- cutree(hclust_ward, k = 4)
concordancia <- table(kmeans = km$cluster, hclust = grupos_hc)
cat("\n--- Tabla de concordancia k-means vs jerárquico ---\n")
print(concordancia)

# Figura 13: Silhouette jerárquico
sil_hc <- silhouette(grupos_hc, dist_mat)

png("imagenes/figura_13_silhouette_hclust.png", width = 900, height = 600, res = 120)
plot(sil_hc,
     col    = COLORES_CLUSTER,
     border = NA,
     main   = "Análisis de silhouette — clustering jerárquico (Ward D2)")
dev.off()

cat(sprintf("Silhouette medio global (jerárquico): %.2f\n", mean(sil_hc[, "sil_width"])))

# -----------------------------------------------------------------------------
# 8. RESUMEN FINAL
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("RESUMEN DEL ANÁLISIS\n")
cat("=============================================================\n")
cat(sprintf("Año de análisis          : 2022\n"))
cat(sprintf("Número de países         : %d\n", n_paises))
cat(sprintf("Número de variables      : %d\n", length(vars_numericas)))
cat(sprintf("Varianza explicada PC1   : %.1f%%\n", varianza_exp[1]*100))
cat(sprintf("Varianza explicada PC1+2 : %.1f%%\n", varianza_acum[2]*100))
cat(sprintf("Silhouette k-means       : %.2f\n", mean(sil_km[,"sil_width"])))
cat(sprintf("Silhouette jerárquico    : %.2f\n", mean(sil_hc[,"sil_width"])))
cat("Figuras guardadas en carpeta imagenes/\n")
cat("=============================================================\n")

