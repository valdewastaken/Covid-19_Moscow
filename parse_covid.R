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
  url_string <- paste("https://geocode-maps.yandex.ru/1.x/?lang=ru_RU&apikey=<APIKEY>&rspn=1&bbox=54.015631,35.653803~57.214223,39.550980&geocode=...",
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

datat$lonlat <- NA

for (i in 1:length(datat$adress)) {
  datat$lonlat[i] <- geoYandex(datat$adress[i])
  if (i %% 500 == 0){print(i)}
}


#########################################################
saveRDS(datat)
write.csv(datat, "covidadress.csv", fileEncoding = "UTF-8")
