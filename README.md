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

📷 Visualizaciones destacadas
🌍 Distribución geográfica de los clústers

📈 Varianza explicada en el análisis factorial

🔥 Indicador sintético de ayuda a Ucrania

🚀 Cómo reproducir el análisis
1. Instalar R y RStudio
Puedes descargar R desde: https://cran.r-project.org/
Y RStudio desde: https://posit.co/download/rstudio-desktop/

2. Instalar las librerías necesarias
r
Copiar
Editar
install.packages(c("FactoMineR", "factoextra", "randomForest", "glmnet", 
                   "MASS", "caret", "dplyr", "ggplot2", "corrplot", "car"))
3. Ejecutar el script
r
Copiar
Editar
source("TFM_Ayuda_Ucrania.R")
⚠️ Nota: Por motivos de confidencialidad, la base de datos real no se incluye en este repositorio. Puedes reconstruirla siguiendo las fuentes indicadas en el documento del TFM.

👨‍💼 Autor
Ángel Sánchez Quintero
📧 sq.angel@hotmail.com
📍 España
🎓 Universidad (2024/2025)

Puedes contactar conmigo para colaborar, comentar el análisis o hablar de datos, conflictos y geopolítica.

📚 Cita recomendada
Si quieres citar este repositorio o el trabajo, puedes usar:

bibtex
Copiar
Editar
@misc{SanchezQuintero2025,
  author = {Ángel Sánchez Quintero},
  title = {TFM - Patrones y determinantes de la ayuda internacional a Ucrania},
  year = {2025},
  howpublished = {\url{https://github.com/angel-sq/TFM-Ayuda-Ucrania}}
}
📎 Licencia
Este proyecto está bajo la licencia MIT, lo que significa que puedes reutilizar el código libremente citando la autoría original.
