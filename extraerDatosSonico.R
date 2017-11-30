setwd("C:/Users/PatricioDelBoca(PiDa/Repos/letras-cuarteto/")
library(readr)
library(dplyr)
library(rvest)
library(stringr)
library(purrr)
library(tidyr)


baseUrl <- "http://www.sonicomusica.com"

paginaArtistas <- read_html(str_c(baseUrl, "/cuarteto/")) 

nombreArtista <- paginaArtistas %>%
  html_nodes(".nameartist") %>%
  html_text()

urlArtista <- paginaArtistas  %>%
  html_nodes(".nameartist a") %>%
  html_attr("href")

dataArtistas <- data.frame(nombreArtista = nombreArtista, 
                          urlArtista = str_c(baseUrl, urlArtista), 
                          stringsAsFactors = FALSE)

getURLCanciones <- function(urlArtista){
  print(sprintf("Scrapeando URL canciones: %s", urlArtista))
  read_html(urlArtista) %>%
    html_nodes(".listitems article a") %>%
    html_attr("href")
}

dataCanciones <- dataArtistas %>%
  mutate(urlCancion = urlArtista %>% map(getURLCanciones)) %>%
  unnest() %>%
  mutate(urlCancion = str_c(baseUrl, urlCancion))

getLetraCancion <- function(urlCancion){
    read_html(urlCancion, options = "RECOVER") %>%
    html_node("#lyricSong pre") %>%
    html_text()
}

# Error: '/cuarteto/damian-cordoba/por-vos vivo/nina/' does not exist.
dataCanciones$urlCancion <- str_replace_all(dataCanciones$urlCancion, " ", "%20")


dataLetras <- dataCanciones %>%
  mutate(letraCancion = urlCancion %>% map(getLetraCancion)) %>%
  unnest()

write_csv(dataLetras, "letras_sonico.csv")
