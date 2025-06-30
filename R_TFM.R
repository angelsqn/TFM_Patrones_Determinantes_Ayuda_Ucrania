
################# TFM #################
# He tenido que actualizar Rstudio y los paquetes porque algunos me dejaron de funcionar => update.packages(ask = FALSE)

### Librerías necesarias ###
library(readxl)
library(dlookr)
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(ggrepel)
library(grid)
library(patchwork)
library(car)
library(lmtest)
library(knitr)
library(glmnet)
library(MASS)
library(randomForest)
library(dplyr)
library(caret)
library(randomForest)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(countrycode)


################################# CARGA DATOS Y TRATAMIENTO ############################################

### Cargo la Base de Datos y le asigno a cada fila el nombre de su país
datos <- read_excel("BBDD_LIMPIA.xlsx")
datos <- as.data.frame(datos)
rownames(datos) <- datos$Country
datos$Country <- NULL

str(datos) # Población lo detecta como carateres porque utiliza ","
datos$`Población 2025` <- as.numeric(gsub(",", "", datos$`Población 2025`))
datos$`Población 2025` <- datos$`Población 2025`/1000000 #lo expreso en millones

## Para evitar endogeneidad voy a expresar el coste de los refugiados de forma unitaria (coste por refugiado)
datos$'Coste Estimado Acogida Refugiados' <- datos$'Coste Estimado Acogida Refugiados'/datos$'Refugiados Ucranianos Acogidos'
datos$`Coste Estimado Acogida Refugiados`[is.nan(datos$`Coste Estimado Acogida Refugiados`)] <- 0

### Asigno nombres a las variables:

names(datos) <- c(
  "EU",
  "geoEuropa",
  "asignFinanc",
  "asignHum",
  "asignMil",
  "costeRefu",
  "armasPesadas",
  "tanques",
  "asignHow",
  "howitzers",
  "refugiados",
  "poblacion",
  "pibGasto2022",
  "pibGasto2023",
  "varIPC",
  "indDem",
  "indPrensa",
  "votoCondena",
  "gasRusia",
  "sanciones",
  "ideologia",
  "distUcrania",
  "distRusia",
  "pib",
  "impRusia",
  "impChina",
  "emigUcr20"
)

### Transformo las dos dummies a factores (YES, NO)
datos$EU <- factor(datos$EU, levels = c(0,1), labels = c("No", "Sí"))
datos$geoEuropa  <- factor(datos$geoEuropa , levels = c(0,1), labels = c("No", "Sí"))

### Transformo el resto de cualitativas en factores también
datos$votoCondena  <- factor(datos$votoCondena, levels = c("NO_ONU", "SI", "ABS", "AUS"), labels = c("No_ONU","Sí","Abs", "Ausente"))
datos$sanciones  <- factor(datos$sanciones , levels = c("NO","SI","PARCIAL"), labels = c("No","Sí","Parciales"))
datos$ideologia <- factor(datos$ideologia, levels = c("Izquierda","Centro","Derecha"))

datos_completo <- datos
datos <- datos %>% filter(asignFinanc > 0 | asignHum > 0 | asignMil > 0)

################################# ANÁLISIS EXPLORATORIO DE LOS DATOS ############################################

### Hago un breve diagnóstico a las variables numéricas y categóricas
numericas <- datos %>% dplyr::select(-EU, -geoEuropa, -votoCondena, -sanciones, -ideologia)

categoricas <- datos %>% dplyr::select(EU, geoEuropa, votoCondena, sanciones, ideologia)
 
diagnose_numeric(numericas)
sd(numericas$asignFinanc)
sd(numericas$asignHum)
sd(numericas$asignMil)
median(numericas$asignFinanc)
median(numericas$asignHum)
median(numericas$asignMil)

diagnose_category(categoricas) # Arroja algunas ratios interesantes


### Creo algunos gráficos y tablas útiles para presentar las variables más relevantes

# Dependencia gas ruso
ggplot(data = datos, aes(x = reorder(rownames(datos), gasRusia), y = gasRusia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Dependencia del Gas Ruso por País (2021)",
    x = "País",
    y = "Proporción de Importaciones de Gas desde Rusia"
  ) +
  theme_minimal()

# Países Unión Europea y países continente europeo
# Calcular proporciones de países EU vs No EU
prop_eu <- datos %>%
  count(EU) %>%
  mutate(pct = n / sum(n) * 100,
         grupo = "Unión Europea")
# Calcular proporciones de países Europa vs No Europa
prop_geo <- datos %>%
  count(geoEuropa) %>%
  mutate(pct = n / sum(n) * 100,
         grupo = "Europa Geográfica")

prop <- bind_rows(
  prop_eu %>% rename(Pertenece = EU),
  prop_geo %>% rename(Pertenece = geoEuropa)
)

# Gráfico de barras lado a lado con porcentaje
ggplot(prop, aes(x = grupo, y = pct, fill = Pertenece)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Proporción Países Europa Geográfica y Unión Europea",
       x = "",
       y = "Porcentaje",
       fill = "Pertenece") +
  theme_minimal()

# Gráfico ideologías
# Calcular proporciones
prop_ideologia <- datos %>%
  count(ideologia) %>%
  mutate(pct = n / sum(n) * 100)

# Gráfico de barras con porcentaje
ggplot(prop_ideologia, aes(x = ideologia, y = pct, fill = ideologia)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5) +
  labs(title = "Distribución de Ideología de los Gobiernos",
       x = "Ideología",
       y = "Porcentaje (%)") +
  theme_minimal() +
  theme(legend.position = "none")






### Outliers
# Voy a analizar los outliers de las distintas variables pero no los voy a tratar
# ya que considero que deben ser parte del estudio
# y que no contar con ellos desvirtuaría la realidad

# Creo una función que me ayuda a graficar todas las variables numéricas y así poder encontrar outliers

plot_outliers_boxplot <- function(df) {
  df %>%
    dplyr::select(where(is.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
    ggplot(aes(x = "", y = valor)) +
    geom_boxplot(outlier.color = "red", fill = "lightblue") +
    facet_wrap(~ variable, scales = "free", ncol = 3) +
    labs(title = "Boxplots de outliers por variable numérica",
         x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text = element_text(size = 10))
}
plot_outliers_boxplot(datos) ### No incluir en memoria, asighowitzers es discreta usar gráfico de barras



# Vamos a analizar las correlaciones entre las variables
numericas <- datos %>% 
  dplyr::select(where(is.numeric))

matriz_cor <- cor(numericas, use = "complete.obs") ### OJO!!! Correlación impChina con el resto (saber explicar)
ggcorrplot(matriz_cor, 
           method = "circle", 
           type = "upper", 
           lab = TRUE)

## Parece que hay variables muy correlacionadas como puede ser el envío de armas pesadas y el de tanques
## Puede apreciarse que la correlación entre las variables es notable en la mayoría de los casos, 
## por ello voy a eliminar variables que ya están explicadas por otras variables para evitar endogeneidad
## Es el caso de armas pesadas que recoge los howitzers junto a otras armas pesadas
## Todo esto invita a analizar el problema con menos variables e incorrelacionadas (ACP).
## Como tengo variables cualitativas voy a aplicar FAMD

numericas <- numericas %>% dplyr::select(-asignHow, -howitzers, -armasPesadas, -pibGasto2022)
datos <- datos %>% dplyr::select(-asignHow, -howitzers, -armasPesadas, -pibGasto2022)

# Muestro la nueva matriz de correlaciones
matriz_cor <- cor(numericas, use = "complete.obs")
ggcorrplot(matriz_cor, 
           method = "circle", 
           type = "upper", 
           lab = TRUE)

################################################ ANÁLISIS FACTORIAL ########################################

### El objetivo es crear indicadores que permitan analizar el comportamiento de los países
## Y encontrar clusteres de países con comportamientos parecidos
## Es importante no seleccionar un numero elevado de componentes  ya que dificultaria la interpretación
res.famd <- FAMD(datos, ncp = 7, graph = FALSE)
fviz_screeplot(res.famd, addlabels = TRUE, ylim = c(0, 50)) # El punto de inflexión aparece en la cuarta componente
res.famd$eig[,1] # Dimensiones con eigenvalue > 1 indican que la componente aporta más info que una variable individual
res.famd$eig[,3] # Varianza acumulada por componente (hasta 80%)

# Scree plot personalizado más bomito para incluirlo en el TFM
fviz_screeplot(res.famd, 
               addlabels = TRUE, 
               ylim = c(0, 50)) +
  labs(title = "Varianza Explicada Frente a Número de Dimensiones",
       x = "Dimensiones",
       y = "Porcentaje de Varianza Explicada")


# Determino que la idea original es adecuada y que además para tener una interpretación más sencilla
# Es recomendable seleccionar 4 componentes

res.famd <- FAMD(datos, ncp = 4, graph = FALSE)

# Eigenvalues (varianza explicada)
print(res.famd$eig)

# Coordenadas de variables en las componentes
coord_vars <- res.famd$var$coord

contrib_vars <- as.data.frame(res.famd$var$contrib)
tablas_dimensiones <- list()

# Crear una tabla por cada dimensión
for (dim in 1:4) {
  # Ordenar y extraer las 5 variables más influyentes
  top_vars <- contrib_vars[order(-contrib_vars[, dim]), , drop = FALSE][1:5, dim, drop = FALSE]
  
  # Crear tabla con nombres
  top_table <- data.frame(
    Variable = rownames(top_vars),
    `Contribución (%)` = round(top_vars[, 1], 2)
  )
  
  # Guardar la tabla con formato kable en la lista
  tablas_dimensiones[[dim]] <- kable(top_table, align = "lc", caption = paste("Dimensión", dim, "- Variables más influyentes"))
}

# Mostrar las 4 tablas
tablas_dimensiones[[1]] # Peso económico y centralidad regional (visión económica)
tablas_dimensiones[[2]] # Compromiso político, humanitario y democrático (visión social)
tablas_dimensiones[[3]] # Cercanía geográfica e integración democrática europea (visión geográfica y de valores europeos)
tablas_dimensiones[[4]] # Alineamiento estratégico y coste político del apoyo

# Dimensión 1 – Peso económico y centralidad regional
# Explica: La capacidad estructural de los países (PIB, población), su integración económica global (importaciones desde China) 
# y su relevancia geográfica (ubicación en Europa) como factores que permiten o condicionan su capacidad de apoyo a Ucrania.

# Dimensión 2 – Compromiso político, humanitario y democrático
# Explica: El nivel de implicación política e institucional de un país en el conflicto (sanciones, voto en la ONU), 
# su respuesta humanitaria (ayuda y acogida de emigrantes) y la presencia de valores democráticos (libertad de prensa) 
# como motores del apoyo a Ucrania.

# Dimensión 3 – Cercanía geográfica e integración democrática europea
# Explica: Cómo influye la proximidad territorial al conflicto, 
# combinada con la pertenencia a la UE y el nivel de democracia, en la postura de los países. 
# También aparece el factor energético como elemento de exposición o dependencia.

# Dimensión 4 – Alineamiento estratégico y coste político del apoyo
# Explica: El grado en que los países han asumido un rol político-militar activo en el conflicto, 
# con decisiones de alto coste (sanciones, gasto militar, ruptura con Rusia) en función de su ideología y recursos. 
# Se refleja una postura de compromiso estratégico firme frente a Rusia.

################## CLUSTERING ################## 

# Se va a aplicar el método Kmeans para determinar los distintos clusters existentes en base a
# las componentes previamente creadas con FAMD

################## CLUSTERING DE COMPONENTES ################## 

set.seed(9)
coords <- res.famd$ind$coord

fviz_nbclust(coords, kmeans, method = "wss") +
  labs(
    title = "Método del Codo para Número Óptimo de Clústers",
    x = "Número de Clústers",
    y = "Suma Total de Cuadrados Dentro del Grupo"
  )
# A través de la regla del codo se puede apreciar como el número óptimo de clusters a formar es 5


kmeans.res <- kmeans(coords, centers = 5, nstart = 25) # creo los 5 clusters
table(kmeans.res$cluster) # muestro cuantos países pertenecen a cada cluster

# Hago una breve prueba con 4 y 6 clústers para comparar
kmeans.res4 <- kmeans(coords, centers = 4, nstart = 25) # creo los 4 clusters
table(kmeans.res4$cluster) # muestro cuantos países pertenecen a cada cluster
kmeans.res4
# Se obtienen los mismos clústers que con 5 salvo el clúster formado por Alemania, Hungría y Polonia 
# que pasan a formar parte del clúster 3 junto al resto de países europeos

kmeans.res6 <- kmeans(coords, centers = 6, nstart = 25) # creo los 6 clusters
table(kmeans.res6$cluster) # muestro cuantos países pertenecen a cada cluster
kmeans.res6
# Divide a los países europeos en hasta 3 grupos

#################### GRÁFICO CLUSTERS #################### 

# Nuevamente mediante chat gpt genero gráficos por pares de dimensiones
# siendo las coordenadas los scores de los países para cada dimensión
# y representando mediante el color el cluster al que pertenece cada pais

plot_famd_dims_by_cluster <- function(res.famd, cluster_assignments, dims = 1:4) {
  # Extraer coordenadas de las dimensiones seleccionadas
  coords <- as.data.frame(res.famd$ind$coord[, dims])
  colnames(coords) <- paste0("Dim.", dims)  # Asegurar nombres estándar
  
  # Agregar nombres de países y clusters
  coords$pais <- rownames(res.famd$ind$coord)
  coords$cluster <- as.factor(cluster_assignments)
  
  # Crear combinaciones de pares de dimensiones (por nombres)
  dim_pairs <- combn(paste0("Dim.", dims), 2, simplify = FALSE)
  
  # Mostrar los gráficos uno por uno
  for (pair in dim_pairs) {
    dim_x <- pair[1]
    dim_y <- pair[2]
    
    p <- ggplot(coords, aes_string(x = dim_x, y = dim_y, color = "cluster")) +
      geom_point(size = 2.5, alpha = 0.85) +
      geom_text(aes(label = pais), size = 3, vjust = -0.7, alpha = 0.6) +
      labs(
        title = paste(dim_x, "vs", dim_y),
        x = dim_x,
        y = dim_y,
        color = "Clúster"
      ) +
      coord_fixed() +  # Relación 1:1 para forma cuadrada
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom"
      )
    
    print(p)  # Mostrar el gráfico inmediatamente
  }
}

# Ejecutar la función para ver cada gráfico de par de dimensiones uno a uno
plot_famd_dims_by_cluster(res.famd, kmeans.res$cluster)

##################### INTERPRETACIÓN DE LOS CLUSTERS ##################### 

# Cluster 1 (Rojo)
# Países: China, India, Turquía

# Perfil:
# - Alta capacidad económica (Dim.1 alta)
# - Bajo compromiso político, humanitario y democrático (Dim.2 bajo)
# - No próximos al conflicto (Dim.3 medio/bajo)
# - Compromiso estratégico moderado o ambiguo (Dim.4 variable)

# Interpretación:
# Países económicamente relevantes, con regímenes poco democráticos, alejados geográficamente y con posturas ambiguas o neutrales frente al conflicto

# Cluster 2 (Amarillo)
# País: Estados Unidos

# Perfil:
# - Mayor capacidad económica y estructural (Dim.1 muy alta)
# - Compromiso político y democrático máximo (Dim.2 alta)
# - Lejano geográficamente al conflicto (Dim.3 medio)
# - Compromiso estratégico y militar muy alto (Dim.4 alta)

# Interpretación:
# Actor clave con gran capacidad y liderazgo global, fuertemente alineado con Ucrania pese a su distancia geográfica

# Cluster 3 (Verde)
# Países: Canadá, Japón, Australia, Nueva Zelanda, Corea del Sur, Taiwán

# Perfil:
# - Capacidad económica moderada-alta (Dim.1 media)
# - Fuerte compromiso político y democrático (Dim.2 alta)
# - Lejanía geográfica al conflicto (Dim.3 baja)
# - Alto alineamiento estratégico con Occidente (Dim.4 alta)

# Interpretación:
# Democracias consolidadas y aliadas de Occidente, alejadas del conflicto pero firmemente comprometidas con Ucrania

# Cluster 4 (Azul claro)
# Países: Mayoría de países europeos occidentales y centrales

# Perfil:
# - Nivel medio-alto en capacidad económica y estructural (Dim.1 media/alta)
# - Alto compromiso democrático y humanitario (Dim.2 alta)
# - Proximidad geográfica e integración europea (Dim.3 alta)
# - Alineamiento estratégico significativo pero no extremo (Dim.4 medio/alto)

# Interpretación:
# Núcleo europeo pro-ucraniano, con fuerte implicación política y geográfica en el conflicto

# Cluster 5 (Rosa)
# Países: Polonia, Hungría, Alemania

# Perfil:
# - Alta proximidad geográfica y pertenencia a la UE (Dim.3 alta)
# - Niveles dispares de compromiso estratégico y político (Dim.2 y Dim.4 variables)
# - Diferencias en orientación ideológica y respuesta al conflicto

# Interpretación:
# Países europeos cercanos al conflicto pero que se desmarcan del consenso europeo predominante (Cluster 4), mostrando perfiles divergentes en su implicación política y estratégica: desde el fuerte apoyo de Polonia hasta la ambigüedad o reticencia de Hungría


################### MUESTRO LOS PAÍSES POR CLÚSTER EN UN MAPA GEOGRÁFICO


df_clusters <- data.frame(
  country = names(kmeans.res$cluster),
  cluster = as.numeric(kmeans.res$cluster)
)

# Corregir manualmente el clúster de Hungría
df_clusters$cluster[df_clusters$country == "Hungary"] <- 5

# Añadir código ISO3
df_clusters$iso_a3 <- countrycode(df_clusters$country, origin = "country.name", destination = "iso3c")

# Cargar mapa base
world <- ne_countries(scale = "medium", returnclass = "sf")
world$iso_a3[world$name == "France"] <- "FRA"
world$iso_a3[world$name == "Norway"] <- "NOR"

# Unir datos de clúster con mapa
mapa_cluster <- left_join(world, df_clusters, by = "iso_a3")

# Crear gráfico
p <- ggplot() +
  geom_sf(data = mapa_cluster, aes(geometry = geometry), fill = "gray90", color = "white") +
  geom_sf(data = filter(mapa_cluster, !is.na(cluster)),
          aes(fill = as.factor(cluster), geometry = geometry),
          color = "white", size = 0.2) +
  scale_fill_manual(
    name = "Clúster",
    values = c(
      "1" = "#FF6F61",
      "2" = "#8C7B00",
      "3" = "#00B68C",
      "4" = "#00A6F2",
      "5" = "#E073E0"
    ),
    guide = guide_legend(
      override.aes = list(size = 3),  # tamaño de los cuadrados de la leyenda
      title.theme = element_text(size = 10),  # tamaño del título de la leyenda
      label.theme = element_text(size = 9)   # tamaño de las etiquetas
    )
  ) +
  labs(
    title = "Distribución Geográfica de los Clústers de Ayuda a Ucrania"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Exportar el gráfico a PDF de alta calidad (vectorial)
ggsave("mapa_clusters.pdf", plot = p, width = 11, height = 8, dpi = 300)

# Si prefieres SVG:
ggsave("mapa_clusters.svg", plot = p, width = 11, height = 8)

##################### INDICADOR SINTÉTICO DE AYUDA (PCA) ##################### 

# PCA en variables de ayuda 
Iayuda <- datos[, c("asignFinanc", "asignHum", "asignMil")]
# Estandarizo
ayuda_scaled <- scale(Iayuda)
# Aplico PCA
pca_res <- prcomp(ayuda_scaled, center = TRUE, scale. = TRUE)
# Utilizo el primer componente como indicador sintético
indice_ayuda <- pca_res$x[, 1]
# Compruebo que el componente 1 explique la mayor parte de la varianza
summary(pca_res) # Explica el 87,88%


# Creo un data frame con los países e índice
indice_df <- data.frame(Pais = rownames(datos), Indice_Ayuda = indice_ayuda)
# Ordeno de mayor a menor valor del Índice
indice_ordenado <- indice_df[order(-indice_df$Indice_Ayuda), ]
indice_ordenado


ggplot(indice_ordenado, aes(x = reorder(Pais, Indice_Ayuda), y = Indice_Ayuda)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Índicador Sintético de Ayuda a Ucrania",
       x = "País",
       y = "Indicador de Ayuda") +
  theme_minimal()

############## COMPORTAMIENTO DE LOS PAÍSES EUROPEOS ############## 

# Gran asimetría entre el discurso político y la acción material en muchos casos (especialmente países Europeos)

# No hay un líder europeo claro (Alemania más próximo a serlo)
# Alemania confirma su papel como potencia europea clave, 
# aunque algunos podrían haber esperado que estuviera aún más cerca de EE. UU. dado su tamaño económico.


### LOS LÍDERES COHERENTES
# Alemania, Reino Unido, Países Bajos, Canadá y Dinamarca son coherentes en su discurso,
# el apoyo no ha sido solo social y diplomático sino también económico, financiero y militar. Es decir, se ha traducido en ayuda real.

### INTENTO DE LÍDER
# Polonia ha sido uno de los países más activos políticamente por lo que se podría esperar una ayuda a Ucrania mayor,
# sin embargo su ayuda real ha sido bastante discreta probablemente debido a su menor peso económico.

# Francia, Suecia y Noruega muestran algo de implicación en el conflicto pero su ayuda podría ser mucho mayor debido a su tamaño económico.
# Casos similares al de Polonia pero estos países sí cuentan con los medios suficientes para ayudar mucho más.


### LOS INCOHERENTES
# Lituania, Letonia, Estonia ,España ,Italia ,Bélgica ,Finlandia , Eslovaquia ,República Checa ,Rumanía ,Austria, Grecia e Irlanda
# han tenido un discurso muy pro-ucraniano pero su ayuda real ha sido realmente baja

# Países que se presentan como muy comprometidos políticamente (como Polonia o los bálticos), no destacan por su ayuda cuantificable, 
# quizás por su menor potencia económica.

############## COMPORTAMIENTO DE LOS PAÍSES NO-EUROPEOS ############## 

# Japón aparece como una sorpresa positiva, y EE. UU. reafirma su liderazgo absoluto.

# Hungría, India, China, Turquía y Malta están en la parte más baja del índice, 
# lo que refleja su distancia política, estratégica o ambigüedad frente al conflicto.
# Incluyo aquí a Hungría por su rol disonante respecto al resto de países europeos








######### ANALIZAR QUÉ VARIABLES EXPLICAN QUE UN PAÍS AYUDE A UCRANIA ######### 

# Se van a incluir más países a la muestra pero en este caso países que no han ayudado a Ucrania
# Vamos a identificar cuales son los determinantes de que un país ayude a Ucrania 
# Se va a aplicar un modelo logit estableciendo como variable dependiente que el país ayude o no para obtener variables significativas.
# Posteriormente se van a aplicar técnicas adecuadas al contexto para obtener más insights al respecto.
# Se aplicarán técnicas como random forest.

# Para esta parte del TFM trabajaré con datos_completo que incluye 82 países (los 41 que han ayudado + 41 que no)
# He elegido 41 países que no han ayudado con características diversas como distintas capacidades económicas,
# distintas ubicaciones geográficas y distintos tamaños económicos

# Creo una nueva variable dicotómica que indica si un país ha ayudado:
# Crear variable binaria 'ayuda': 1 si ha ayudado, 0 si no
datos_completo$ayuda <- ifelse(
  datos_completo$asignFinanc > 0 |
    datos_completo$asignHum > 0 |
    datos_completo$asignMil > 0,
  1, 0
)

datos_completo$ayuda <- as.factor(datos_completo$ayuda)

# Hago un breve análisis exploratorio de los datos

# Cálculo de medias de los países que ayudan
median(datos$indDem)
median(datos$indPrensa)

# Cálculo de medias de los países que no ayudan
median(subset(datos_completo, ayuda == 0)$indDem)
median(subset(datos_completo, ayuda == 0)$indPrensa)

# Gráfico ideologías
prop_ideologia <- datos_completo %>%
  group_by(ayuda, ideologia) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(ayuda) %>%
  mutate(pct = n / sum(n) * 100)

# Gráfico de barras agrupadas
ggplot(prop_ideologia, aes(x = ideologia, y = pct, fill = as.factor(ayuda))) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(title = "Distribución de la Ideología según Ayuda a Ucrania",
       x = "Ideología del gobierno",
       y = "Porcentaje (%)",
       fill = "Ayuda a Ucrania") +
  scale_fill_manual(values = c("0" = "#d73027", "1" = "#1a9850"),
                    labels = c("No ayudó", "Sí ayudó")) +
  theme_minimal()

# Me quedo solo con las variables de interés:

variables <- c("ayuda", "EU", "geoEuropa", "poblacion", "pibGasto2023", "varIPC", "indDem", "indPrensa",
  "votoCondena", "gasRusia", "sanciones", "ideologia", "distUcrania", "distRusia",
  "pib", "impRusia", "impChina", "emigUcr20")

datos_modelo <- datos_completo[, variables]
datos_modelo_completo <- datos_modelo
modelo_base <- glm(ayuda ~ ., data = datos_modelo, family = binomial)
summary(modelo_base)

datos_completos <- datos_modelo %>% dplyr::select(-EU) # Lo utilizaré posteriormente en Lasso
# Elimino EU porque todos los países europeos ayudaron así que no resulta relevante para el estudio

# Aplico logaritmos para estabilizar las escalas
# Log transformaciones
datos_modelo$log_poblacion <- log(datos_modelo$poblacion)
datos_modelo$log_emigUcr20 <- log(datos_modelo$emigUcr20 + 1)  # para evitar log(0)

datos_modelo <- datos_modelo %>% dplyr::select(-poblacion, -emigUcr20)
datos_completo_est <- datos_modelo

############## Voy a estudiar las correlaciones entre las variables previamente a estimar el modelo ##############

# Selecciona todas las columnas excepto la primera (variable dependiente)
datos_sin_dependiente <- datos_modelo[, -1]

# Selecciona solo las columnas numéricas
variables_numericas <- sapply(datos_sin_dependiente, is.numeric)
datos_numericos <- datos_sin_dependiente[, variables_numericas]

# Calcula la matriz de correlaciones
correlaciones <- cor(datos_numericos)

ggcorrplot(correlaciones, 
           method = "circle", 
           type = "upper", 
           lab = TRUE, 
           lab_size = 3,
           colors = c("red", "white", "blue"),
           title = "Matriz de Correlación",
           ggtheme = ggplot2::theme_minimal())

datos_modelo$geoEuropa <- ifelse(datos_modelo$geoEuropa == "Sí", 1, 0)
cor(datos_modelo$geoEuropa, datos_modelo$distUcrania, use = "complete.obs")

# Se puede apreciar como el Índice de Libertad de Prensa y el Índice de Democracia están altamente correlacionados
# Igual pasa con las importaciones desde china y el PIB y con la distancia con Rusia y con Ucrania
# Elimino la variable de países de la Unión Europea ya que todos los miembros de la UE han ayudado y esto puede dar problemas
# Elimino geoEuropa por alta correlacion y muy probable multiculinealidad con distUcrania

datos_modelo <- datos_modelo %>% dplyr::select(-distRusia, -pib, -indPrensa, -EU, -geoEuropa)

######################### MODELO BASE ####################################################

modelo_base <- glm(ayuda ~ log_poblacion + pibGasto2023 + varIPC + indDem + 
                    votoCondena + gasRusia + sanciones + ideologia + 
                    distUcrania + impRusia + impChina + log_emigUcr20, 
                  data = datos_modelo, family = binomial)

vif(modelo_base)
summary(modelo_base)

# Voy a eliminar votoCondena y sanciones ya que creo que me pueden estar explicando gran parte de que un pais ayude
# y no son tan relevantes en mi análisis

modelo_base <- glm(ayuda ~ log_poblacion + pibGasto2023 + varIPC + indDem + gasRusia + ideologia + 
                     distUcrania + impRusia + impChina + log_emigUcr20, 
                   data = datos_modelo, family = binomial)
vif(modelo_base)
summary(modelo_base)
# Elimino variables con VIF > 10. Las voy a eliminar de una en una para ir comprobando si baja el VIF (< 10) sin eliminarlas todas

modelo_base <- glm(ayuda ~ pibGasto2023 + gasRusia + varIPC + votoCondena + sanciones + ideologia + impRusia + impChina + log_emigUcr20, data = datos_modelo, family = binomial)
vif(modelo_base)
summary(modelo_base)


modelo_base <- glm(ayuda ~ pibGasto2023 + varIPC + gasRusia + ideologia + impRusia + impChina + log_emigUcr20, data = datos_modelo, family = binomial)
summary(modelo_base)

# Aplicar tantas variables independientes al modelo definitivamente esta siendo un problema a la hora de estimar el modelo logit
# He obtenido algunas variables significativas en el último modelo estimado (pibGasto2023, ideologiaCentro, impRusia, log_emigUcr20)
# Sin embargo viendo la situación considero que debo aplicar algún método de selección de variables

################################## MODELO LOGIT CON LAS VARIABLES DE INTERÉS ################################## 

modelo_reducido <- glm(ayuda ~ ideologia + gasRusia + log_emigUcr20 + impChina + distUcrania, 
                       data = datos_modelo, family = binomial)
summary(modelo_reducido)

# Este modelo parece mucho más adecuado para el tamaño de la muestra. 
# Se obtuvieron además muchas variables de interés significativas para la investigación.

################################## LASSO  ################################## 

# Convertimos variables categóricas en dummies, excluyendo la variable dependiente
datos_dummy <- dummyVars("~ .", data = datos_completos[, colnames(datos_completos) != "ayuda"], fullRank = TRUE)
datos_dummies_lasso <- predict(datos_dummy, newdata = datos_completos)
datos_dummies_lasso <- as.data.frame(datos_dummies_lasso)

# Definir variable dependiente y matriz de predictores
y <- datos_completos$ayuda
X <- as.matrix(datos_dummies_lasso)

# Ajuste de modelo Lasso con validación cruzada
set.seed(123)
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1, standardize = TRUE)

# Lambda óptimo
lambda_opt <- cv_lasso$lambda.min

# Modelo final con lambda óptimo
modelo_lasso <- glmnet(X, y, family = "binomial", alpha = 1, lambda = lambda_opt)

# Extraer coeficientes distintos de cero
coeficientes <- coef(modelo_lasso)
nombres_coef <- rownames(coeficientes)[which(coeficientes != 0)]
valores_coef <- as.numeric(coeficientes[which(coeficientes != 0)])

# Mostrar resultado en data frame
resultado <- data.frame(Variable = nombres_coef, Coeficiente = valores_coef)
print(resultado)

# Estandarizo variables
datos_completos$poblacion     <- scale(datos_completos$poblacion)
datos_completos$pibGasto2023  <- scale(datos_completos$pibGasto2023)
datos_completos$indDem        <- scale(datos_completos$indDem)
datos_completos$indPrensa     <- scale(datos_completos$indPrensa)
datos_completos$gasRusia      <- scale(datos_completos$gasRusia)
datos_completos$distRusia     <- scale(datos_completos$distRusia)
datos_completos$impRusia      <- scale(datos_completos$impRusia)

# Aplico las variables seleccionadas mediante Lasso a un modelo Logit
modelo_lasso <- glm(ayuda ~ poblacion + pibGasto2023 + indDem + indPrensa +
                      gasRusia + sanciones + ideologia + distRusia + impRusia,
                    family = binomial,
                    data = datos_completos)

summary(modelo_lasso)

# Obtengo nuevamente problemas ya que Lasso sugiere 11 variables
# Según he leido para el tamaño de la muestra se sugieren 5 o 6

################################## STEPWISE BACKWARD  ################################## 

modelo_nulo <- glm(ayuda ~ 1, data = datos_modelo, family = binomial)
modelo_completo <- glm(ayuda ~ log_poblacion + pibGasto2023 + varIPC + 
                         gasRusia + ideologia + 
                         impRusia + impChina + log_emigUcr20,
                       data = datos_modelo, family = binomial)

# Aplico el stepwise backward
modelo_step_backward <- stepAIC(modelo_completo,
                                scope = list(lower = modelo_nulo, upper = modelo_completo),
                                direction = "backward",
                                trace = TRUE)

summary(modelo_step_backward)

################################ RANDOM FOREST ################################
# Me aseguro de que ayuda es factor
datos_completos$ayuda <- factor(datos_completos$ayuda, levels = c(0,1))

set.seed(42)
Modelo_Random_Forest <- randomForest(ayuda ~ ., data = datos_completos, ntree = 500, importance = TRUE)

# Importancia basada en MeanDecreaseGini o MeanDecreaseAccuracy (sugerencia Chat GPT)
importance(Modelo_Random_Forest)

# Para verlo de forma más ordenada y gráfica
varImpPlot(Modelo_Random_Forest)
