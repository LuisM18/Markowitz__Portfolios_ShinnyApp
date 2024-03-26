# Teoria de portafolios de Markowitz(ShinnyApp)

## Descripcion
Proyecto que realicé en la universidad para hallar y graficar la frontera eficiente a traves de miles de portafolios simulados.

![Screenshot](/img/frontera.png)

## Contenido
 - Archivo principal de la Shinnyapp `app.R`
 - Carpeta con ejemplos de dataset

**Inputs:** Archivo csv o Excel con el historico de precios del universo de inversión a analizar(una accion por columna).
            En [datasets/](/datasets/) existen algunos ejemplos. Puede obtener su propios datos en Investing, Yahoo Finance, etc.

**Outputs:** 
- Gráfico de dominancia y libertad para elegir la cantidad, ademas de una grafica individual de los componentes.
- Gráfico de la frontera eficiente
- Calculo del portafolio optimo

## Librerias
```r
library(shinythemes)
library(ggplot2)
library(scales)
library(pracma)
library(shinyFeedback)
library(readxl)
library(vroom)
library(DT)
library(ggiraph)
library(tidyverse)
```
 
 
## Deploy

  **Link:** ()  




