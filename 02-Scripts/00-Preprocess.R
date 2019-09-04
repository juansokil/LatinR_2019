##Core Collection of WOS##
##(TS='gender' AND SU='Social Sciences') or WC="Women's Studies"
#Contenido del registro (registro completo y referencias citadas)
#Formato del archivo texto sin formato
#Guardar todos los archivos en una misma carpeta y ejecutar 'copy *.txt completo.txt'

###Librerias ####
library(readr)
library(dplyr)
library(tidyr)
library(gender)
library(stringr)
library(bibliometrix)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(yarrr)
library(udpipe)
library(igraph)
library(ggraph)
library(ggplot2)



###Carga de Archivo###
#completo <- readFiles("https://raw.githubusercontent.com/juansokil/womens_studies/master/00-Csv/plain.txt")
completo <- readFiles("C:/juntar/completo.txt")
completo <- convert2df(completo, dbsource = "isi", format = "plaintext")
M <- completo

####ANALISIS SCOPUS###
#completo <- readFiles("C:/Users/Juan/Desktop/scopus.bib")
#completo <- convert2df(completo, dbsource = "scopus", format = "bibtex")

###Filter###
base_completa = unique(completo %>%
                         select(UT, AF, TI, SO, AB, C1, PY, WC))


###Genera base autores###
autores <- base_completa %>% 
  select(UT, PY, AF) %>% 
  separate_rows(AF, sep=';')  

View(autores)

autores <- separate(autores, AF, into=c("apellido", "nombre"), sep=',', remove = FALSE)
autores$nombre <- str_trim(autores$nombre, side = c("both"))
autores <- separate(autores, nombre, into=c("primer_nombre", "segundo_nombre"), sep=' ', remove = FALSE)
autores$primer_nombre <- str_trim(autores$primer_nombre, side = c("both"))

gender_df <- gender(autores$primer_nombre)
authors <-   unique(left_join(autores, gender_df, by = c("primer_nombre" = "name")))
authors2 <- authors[-1]
authors_unique <- unique(authors2)


tabla <- authors_unique %>%
  group_by(PY, gender) %>%
  count()

#####################PARTICIPACION##################
###### AUTORES TOTALES######
author_share <- authors %>%
  filter(gender =='male' | gender=='female') %>%
  select(UT) %>%
  group_by(UT) %>%
  mutate(author = n())

         ########PARTICIPACION######
author_male <- authors %>%
  filter(gender =='male') %>%
  select(gender, UT) %>%
  group_by(UT, gender) %>%
  mutate(author_male = n())
  
author_female <- authors %>%
  filter(gender =='female') %>%
  select(gender, UT) %>%
  group_by(UT, gender) %>%
  mutate(author_female = n())

base_completa <- left_join(base_completa, author_share, by = c("UT" = "UT") )
base_completa <- left_join(base_completa, author_male, by = c("UT" = "UT") )
base_completa <- left_join(base_completa, author_female, by = c("UT" = "UT") )

base_completa <- unique(base_completa)

base_completa %>% 
  filter(gender.x =='male') %>% 
  group_by(PY) %>% 
  summarize(Unique_Elements = n_distinct(UT), promedio=mean(author_male))


base_completa %>% 
  filter(gender.y =='female') %>% 
  group_by(PY) %>% 
  summarize(Unique_Elements = n_distinct(UT), promedio=mean(author_female))


table(base_completa$PY)


base_completa %>% 
  filter(gender.x =='male') %>% 
  group_by(PY) %>% 
  summarize(Unique_Elements = n_distinct(UT))
   
table(base_completa$author_female, base_completa$PY)
table(base_completa$author_male, base_completa$PY)



















################################################




base_completa <- read_delim("C:/Users/Juan/Dropbox/LatinR_2019/01-Bases/base_completa.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)




###Genera base autores###
autores <- base_completa %>% 
  select(UT, PY, AF) %>% 
  separate_rows(AF, sep=';')  

View(autores)

autores <- separate(autores, AF, into=c("apellido", "nombre"), sep=',', remove = FALSE)
autores$nombre <- str_trim(autores$nombre, side = c("both"))
autores <- separate(autores, nombre, into=c("primer_nombre", "segundo_nombre"), sep=' ', remove = FALSE)
autores$primer_nombre <- str_trim(autores$primer_nombre, side = c("both"))


gender_df <- gender(autores$primer_nombre)

authors <-   unique(left_join(autores, gender_df, by = c("primer_nombre" = "name")))

View(authors)



recuento = authors %>% 
  select (UT, PY, AF, gender) %>% 
  group_by (PY) %>% 
  summarize (n_distinct(UT))






authors %>% 
  select (UT, PY, AF, gender)

bla = authors %>% 
  group_by (AF) %>% 
  filter (!is.na(gender)) %>% 
  summarize (n_distinct(UT), max(gender))




####ARMAR GRAFOS#####
cooc <- cooccurrence(x = authors, 
                     term = "AF", 
                     group = "UT")



wordnetwork <- head(cooc, 700)

View(wordnetwork)
wordnetwork <- graph_from_data_frame(wordnetwork)

g<- simplify(wordnetwork, remove.multiple = TRUE)
set.seed(1982)
plot.igraph(g, layout=layout_with_fr,vertex.label.color="black", vertex.shape="circle",
            edge.width=E(g)$weight*0.01, vertex.size=degree(g), vertex.label.cex=0.1)



