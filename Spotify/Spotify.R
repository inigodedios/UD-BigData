

# Antes de nada, limpiamos el workspace, por si hubiera alg?n dataset o informaci?n cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Limpiamos la consola
cat("\014")

# Dado que vamos a tener bastantes problemas de codificación de caracteres, lo fijamos a UTF-8
options(encoding = "utf-8")

# Cargamos las librerías que vamos a necesitar
library(rvest)
library(tidyverse)
library(magrittr)
library(scales)
library(knitr)
library(lubridate)
library(fmsb)
library(ggplot2)

# Un tutorial de web scraping en R muy interesante: https://www.datacamp.com/community/tutorials/r-web-scraping-rvest

###################################################################################
# 1. SPOTIFY
###################################################################################
# Parte constante de la URL
url <- "https://spotifycharts.com/regional/es/daily/"

# Vamos a definir un rango de scraping de las canciones más escuchadas
rango <- seq(as.Date("2019/08/01"), as.Date("2019/10/03"), by = "day")
rango[1:3]

# Unificamos para tener todo el rango de URLs
#   Creamos una función para ello
unificarURL<- function(x){
  full_url <- paste0(url, x)
  full_url
}

# Y ya tenemos la URL final
finalurl <- unificarURL(rango)
finalurl

# Nos hacemos una función para escrapear todos los datos
#   Usemos SelectorGadget o el código fuente (depende estructura)
scraperSpotify <- function(x){
  page <- x
  rank <- page %>% 
    read_html() %>% 
    html_nodes('.chart-table-position') %>% 
    html_text() %>% 
    as.data.frame()
  track <- page %>% 
    read_html() %>% 
    html_nodes('strong') %>% 
    html_text() %>% 
    as.data.frame()
  artist <- page %>% 
    read_html() %>% 
    html_nodes('.chart-table-track span') %>% 
    html_text() %>% 
    as.data.frame()
  streams <- page %>% 
    read_html() %>% 
    html_nodes('td.chart-table-streams') %>% 
    html_text() %>% 
    as.data.frame()
  dates <- page %>% 
    read_html() %>% 
    html_nodes('.responsive-select~ .responsive-select+ .responsive-select .responsive-select-value') %>% 
    html_text() %>% 
    as.data.frame()
  
  #combine, name, and make it a tibble
  chart <- cbind(rank, track, artist, streams, dates)
  names(chart) <- c("Rank", "Track", "Artist", "Streams", "Date")
  chart <- as.tibble(chart)
  return(chart)
}

# Con map_df aplicamos una función determinada a todos los elementos de una lista
spotify <- map_df(finalurl, scraperSpotify)
write.csv(spotify,file="spotify.csv")

spotify=read.csv("spotify2.csv")

# load the library
library(forcats)
library(ggplot2)
library(dplyr)

# Hacemos una limpieza de datos de todo lo que hemos traído de Spotify
str(spotify)
spotify$Date = as.Date(spotify$Date, "%m/%d/%Y")

# Podriamos dibujar algo??
summary(spotify)

boxplot(spotify$Streams) #VEMOS LA MEDIA LOS CUARTILES Y QUE HAY MUCHOS OUTLIERS EN ESTE DATASET. HABR'IA QUE ESTUDIAR LOS OUTLIERS AGRUPADOS POR CANCION

agrupadasPorCancion = aggregate(spotify$Streams, FUN = sum, list(spotify$Track))
colnames(agrupadasPorCancion)[colnames(agrupadasPorCancion) == "Group.1"] = "Track"
colnames(agrupadasPorCancion)[colnames(agrupadasPorCancion) == "x"] = "Streams"
agrupadasPorCancion = agrupadasPorCancion %>% arrange(desc(Streams))

masEscuchadas = agrupadasPorCancion[0:10, ]

barplot(masEscuchadas$Streams,  main="Streams",
        xlab="Nombre canciones", horiz=TRUE,
        names.arg=c(masEscuchadas$Track), cex.lab = 2)

labels <- c(masEscuchadas$Track)
slices <- c(masEscuchadas$Streams)
lbls <- c(masEscuchadas$Track)
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Mas escuchadas")

boxplot(masEscuchadas$Streams) #VEMOS QUE UNA DE LAS 10 CANCIONES AGRUPADAS SE HA ESCUCHADO MUCHO MAS

summary(masEscuchadas$Streams)

filtroOutliers = masEscuchadas$Streams > 19077408
outliers = masEscuchadas[filtroOutliers,] #LAS CANCIONES MAS ESCUCHADAS HAN SIDO LAS SIGUIENTES

library(plotrix)

#CREACION DE LABEL INTUITIVO

total = 32547394 + 20406569 + 19775898

outliers$porcentajes = as.integer(outliers$Streams/total*100);

outliers$label = sprintf ("%s %d/100",outliers$Track, outliers$porcentajes)

pie3D(outliers$Streams, labels=outliers$label)

filtroViolin = spotify$Track == "China" |  spotify$Track == "Otro Trago - Remix" |  spotify$Track == "Yo x Ti, Tu x Mi" 
spotifyOutliers = spotify[filtroViolin,]

ggplot(spotifyOutliers, aes(x=Track, y=Streams, fill=Track)) + geom_violin()

