library(readr)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)

letrasSonico <- read_csv("letras_sonico.csv") %>%
  filter(letraCancion != "Letra no disponible") %>%
  mutate(letraCancion = gsub( " *\\(.*?\\) *", "", letraCancion),
         letraCancion = gsub( "\n", " ", letraCancion))

letrasCoveralia <- read_csv("letras_coveralia.csv")
letrasCoveralia$letraCancion <- str_replace(letrasCoveralia$letraCancion,letrasCoveralia$urlCancion, " ")

letrasLetrasDeCanciones <- read_csv("letras_letrasdecanciones.csv")

  
# Stop Words Español
stopWords <- scan("http://www.webmining.cl/wp-content/uploads/2011/03/stopwords.es.txt", character())
customStopWords <- strsplit("vas pa vos bis has estribillo qe ie ii mmm mmmm eh oh na mee repite bom taka ye", " ")[[1]]
stopWords <- c(stopWords, customStopWords)

topN <- 15

# Palabras General
palabrasCuarteto <- function(letras){
  letras %>%
    unnest_tokens(palabra, letraCancion) %>% 
    filter(!(palabra %in% stopWords)) %>%
    count(palabra, sort = TRUE) %>%
    head(topN) %>%
    mutate(ranking = row_number()) %>% 
    select(ranking, palabra, n)
}

# Palabras por autor
palabrasPorAutor <- function(letras){
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
    spread(nombreArtista, palabra)
}

# Canciones por Autor
cancionesPorAutor <- function(letras){
  letras %>%
    group_by(nombreArtista) %>%
    summarise(canciones = n())
}

# Autores Seleccionados
# La Barra: Coveralia
# Sabroso: Coveralia
# La Mona: Sonico
# Tru La La: Coveralia
# La Fiesta Coveralia
# Banda XXI: Sonico
# Damian Córdoba: Sonico
# Ulises Bueno: Coveralia
# Jean Carlos: Coveralia
# Rodrigo: Coveralia
# Banda Registrada: Sonico

