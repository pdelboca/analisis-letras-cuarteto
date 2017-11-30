# www.letrasdecanciones.fm
library(stringr)
library(rvest)
library(purrr)
library(readr)

artistas <- c("la-barra sabroso rodrigo banda-xxi trulala la-fiesta la-mona-gimenez la-konga walter-olmos banda-express cachumba ulises-bueno la-banda-de-carlitos")
dataArtistas <- data.frame(nombreArtista = str_replace_all(str_split(artistas, " ")[[1]],"-"," "),
                           urlArtista = str_c("http://www.letrasdecanciones.fm/", str_split(artistas, " ")[[1]]),
                           stringsAsFactors = FALSE)


getUrlCanciones <- function(urlArtista){
  print(sprintf("Scrapeando URL canciones: %s", urlArtista))
  read_html(urlArtista) %>%
    html_nodes(".artist-songs .song-name a") %>%
    html_attr("href")
}

getLetraCancion <- function(urlCancion){
  message(urlCancion)
  read_html(urlCancion) %>% 
    html_node(".lyrics") %>%
    html_text() %>%
    str_replace_all("\n", " ")
}

dataArtistas %>% 
  mutate(urlCancion = urlArtista %>% map(getUrlCanciones)) %>% 
  unnest() %>%
  mutate(letraCancion = urlCancion %>% map(getLetraCancion)) %>%
  unnest() -> letras

write_csv(letras, "letras_letrasdecanciones.csv")

# Stop Words Español
stopWords <- scan("http://www.webmining.cl/wp-content/uploads/2011/03/stopwords.es.txt", character())
customStopWords <- strsplit("ay am do re rem mi mi7 mim fa sol la la7 lam si si7 vas pa vos bis has estribillo qe ie ii mmm mmmm eh oh na mee repite bom cu co é eu", " ")[[1]]
stopWords <- c(stopWords, customStopWords)

topN <- 15

# Palabras General
letras %>%
  unnest_tokens(palabra, letraCancion) %>% 
  filter(!(palabra %in% stopWords)) %>%
  count(palabra, sort = TRUE) %>%
  head(topN) %>%
  mutate(ranking = row_number()) %>% 
  select(ranking, palabra, n) %>% 
  View()

# Palabras por autor
letras %>%
  select(nombreArtista, letraCancion) %>% 
  unnest_tokens(palabra, letraCancion) %>% 
  filter(!(palabra %in% stopWords)) %>%
  group_by(nombreArtista) %>%
  count(palabra) %>%
  arrange(nombreArtista, desc(n)) %>%
  slice(1:topN) %>% 
  mutate(ranking = row_number()) %>% 
  select(nombreArtista, palabra, ranking) %>% 
  spread(nombreArtista, palabra) %>% 
  View()

# Canciones por Autor
letras %>%
  group_by(nombreArtistas) %>%
  summarise(canciones = n()) %>%
  View()