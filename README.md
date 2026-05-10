# TFG — Análisis multivariante del desarrollo económico y social internacional

Repositorio del Trabajo Fin de Grado: análisis comparado del desarrollo internacional mediante **PCA** (Análisis de Componentes Principales) y **k-means**, con datos del Banco Mundial (World Development Indicators) para el año 2017.

## Resumen

- **Datos**: WDI del Banco Mundial, año 2017 (descarga automática mediante el paquete `WDI`).
- **Variables (5)**: PIB per cápita, % usuarios de internet, mortalidad infantil, crecimiento poblacional y % población urbana.
- **Resultados**: las dos primeras componentes principales explican el 84% de la varianza; el k-means con k = 4 identifica cuatro perfiles —países de renta baja, en desarrollo, emergentes y desarrollados—.

## Cómo ejecutar el análisis

1. Tener instalado R y, opcionalmente, RStudio.
2. Abrir el archivo `tfg_analisis.R`.
3. La primera vez, instalar los paquetes necesarios ejecutando en la consola:

```r
install.packages(c("WDI","dplyr","tidyr","ggplot2","reshape2","cluster",
                   "scales","tibble","e1071"))
```

4. Ejecutar el script entero (botón *Source* en RStudio o `Ctrl+Shift+S`).

El script descarga los datos directamente del Banco Mundial mediante el paquete `WDI`, por lo que **no requiere ningún archivo local**. Solo necesita conexión a internet.

Las 13 figuras del trabajo se generan automáticamente en la carpeta `figures/`.

## Autor

Alejandro Calle Fernández-Cañamaque — Universidad Carlos III de Madrid — Grado en Estadística y Empresa — 2025/2026.
