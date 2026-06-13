# TFG — Análisis multivariante del desarrollo económico y social internacional

Repositorio del Trabajo Fin de Grado: análisis comparado del desarrollo internacional mediante PCA, t-SNE, k-means y clustering jerárquico, con datos del Banco Mundial (World Development Indicators) para el año 2022.

## Resumen

- Datos: WDI del Banco Mundial, año 2022 (descarga automática mediante el paquete `WDI`).
- Variables (9)**: PIB per cápita, % usuarios de internet, mortalidad infantil, crecimiento poblacional, % población urbana, esperanza de vida, gasto en educación (% PIB), tasa de desempleo y % acceso a la electricidad.
- Técnicas: Análisis de Componentes Principales (PCA), reducción no lineal t-SNE, clustering k-means y clustering jerárquico (Ward D2).
- Resultados: las dos primeras componentes principales explican el 63,2% de la varianza; el k-means con k = 4 identifica cuatro perfiles —países de renta baja, en desarrollo, emergentes y desarrollados—, validados mediante t-SNE y clustering jerárquico.

## Cómo ejecutar el análisis

1. Tener instalado R y, opcionalmente, RStudio.
2. Abrir el archivo `scripts/tfg_analisis_codigo.R`.
3. La primera vez, instalar los paquetes necesarios ejecutando en la consola:

install.packages(c("WDI","dplyr","tidyr","ggplot2","reshape2","cluster","scales","tibble","Rtsne","ggrepel"))

4. Ejecutar el script entero (botón *Source* en RStudio o `Ctrl+Shift+S`).

El script descarga los datos directamente del Banco Mundial mediante el paquete `WDI`, por lo que no requiere ningún archivo local. Solo necesita conexión a internet. Las 13 figuras del trabajo se generan automáticamente en la carpeta `imagenes/`.

## Autor

Alejandro Calle Fernández-Cañamaque — Universidad Carlos III de Madrid — Grado en Estadística y Empresa — 2025/2026.

