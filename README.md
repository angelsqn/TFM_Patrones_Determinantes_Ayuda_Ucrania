# TFM_Ayuda_Ucrania
Repositorio del Trabajo Fin de Máster de Ángel Sánchez Quintero sobre patrones y determinantes de la ayuda internacional a Ucrania (análisis en R).

# 🇺🇦 TFM - Patrones y determinantes de la ayuda internacional a Ucrania

Este repositorio contiene el código y documentación asociados al Trabajo Fin de Máster (TFM) de **Ángel Sánchez Quintero**, desarrollado en el curso 2024/2025, cuyo objetivo es analizar cuantitativamente la respuesta internacional al conflicto entre **Rusia y Ucrania**.

> 🧠 Un estudio basado en análisis factorial, clustering y modelos estadísticos para identificar los factores que explican **por qué unos países han ayudado a Ucrania y otros no**.

---

## 📊 Objetivos

- Clasificar a los países que han apoyado a Ucrania en función de **patrones de ayuda**.
- Construir un **indicador sintético** que mida el nivel real de apoyo.
- Identificar los **determinantes estructurales, económicos e ideológicos** de esa ayuda.

---

## 🧪 Técnicas aplicadas

- Análisis Factorial Mixto (FAMD)
- Clustering K-means
- Análisis de Componentes Principales (PCA)
- Modelos logit
- Random Forest
- Stepwise Backward
- Análisis exploratorio y tratamiento de datos en R

---

## 🛠️ Tecnologías

- Lenguaje: **R**
- IDE: **RStudio**
- Librerías: `FactoMineR`, `factoextra`, `randomForest`, `glmnet`, `MASS`, `caret`, `ggplot2`, etc.
- Dataset: Compuesto por 82 países con variables políticas, geográficas y económicas
- Fuentes de datos: Kiel Institute, FMI, Banco Mundial, ONU, RSF, etc.

---

## 🧬 Estructura del repositorio

```text
TFM-Ayuda-Ucrania/
│
├── TFM_Ayuda_Ucrania.R          # Código principal del análisis
├── figures/                     # Gráficos del análisis
│   ├── gasRuso.jpg
│   ├── indicadorsinteticoayuda.jpg
│   ├── mapapoliticoclusters.jpg
│   └── ...otros
├── README.md                    # Este documento
