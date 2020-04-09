---
title: "Manual"
author: "Marco Ortiz Palanques"
date: "4/9/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Leer archivos  
Los archivos se bajan en la página del Ministerio del Interior.
Los archivos que se leerán serán 03, 09 y 10
Una vez descomprimidos podemos guardarlos en un directorio. Los archivos con la data vienen numerados del 01 al 10 y tienen la extensión .DAT. Hay además otros dos archivos con las instrucciones.
Para abrir el archivo en RStudio como un data frame vamos a File> Import Dataset> From Text (readr)... Allí, seleccionamos el archivo a descargar y desmarcamos la opción "First row as names". Para el archivo 03 hay que seleccionar, además, locale> onfigure> encoding> ASCII y luego apretar "Configure" en esa ventana. Como R no acepta que los nombres de los data frames comiencen con números, el nombre sugerido comenzará con "X" más el nombre original del archivo.

## Seleccionar el municipio  



This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
