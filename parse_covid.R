library(RCurl)
library(XML)
library("tidyverse")
library(rvest)
library(httr)
library(ggmap)
library(rjson)
library(plyr)
library(stringr)
library(xml2)
library(RJSONIO)
library(jsonlite)
#########################
webpage <- read_html("https://coronavirus.mash.ru/")
tbls <- html_nodes(webpage, "table")
head(tbls)

tbls_ls <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

tbls_ls[[1]]

datat <- data.frame()


for (val in tbls_ls){
  datval <- as.data.frame(val)
  datat <- rbind(datat, datval)
}

head(datat, 4)


###################
datat$adress <- paste(datat$Улица, ", ", datat$Дом)

geoYandex<-function(location)
{
  stopifnot(is.character(location))
  loc <- location
  location <- gsub(",", "", location)
  location <- gsub(" ", "+", location)
  posturl <- paste(location)
  url_string <- paste("https://geocode-maps.yandex.ru/1.x/?lang=ru_RU&apikey=10625dd7-ff33-40c8-acce-227be6bb9aee&geocode=...",
                      posturl, sep = "")
  url_string <- URLencode(url_string)
  xmlText <- paste(readLines(url_string, warn=FALSE), "\n", collapse="")
  data<-xmlParse(xmlText, asText=TRUE)
  xml_data <- xmlToList(data)
  pos<-xml_data$GeoObjectCollection$featureMember$GeoObject$Point$pos
  if (is.null(pos)){
    position = "NA"
  } else{
    lon<-c(word(pos,1))
    lat<-as.numeric(word(pos,2))
    position <- paste(lon, " ", lat)}
  return (position)
}

geocodetest <- geoYandex("г. Москва, Ленинский проспект, 90")
geocodetest4 <- geoYandex("Ленинский проспект, 90")
geocodetest2 <- geoYandex("Одинцовский район, пос. Заречье, Весенняя ,  2")
geocodetest5 <- geoYandex("	д. Рогозино, Лесная ,  6")
datat$lonlat <- NA

#datat$geocodes <- geoYandex(datat$adress)

for (i in 1:length(datat$adress)) {
  datat$lonlat[i] <- geoYandex(datat$adress[i])
  if (i %% 500 == 0){print(i)}
}



write.csv(datat, "covidadress.csv", fileEncoding = "UTF-8")