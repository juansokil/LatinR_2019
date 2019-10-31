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
library(RColorBrewer)
#install.packages("comato")
library(comato)

###Carga de Archivo###
#completo <- readFiles("https://raw.githubusercontent.com/juansokil/womens_studies/master/00-Csv/plain.txt")
completo <- readFiles("C:/juntar/completo.txt")
completo <- convert2df(completo, dbsource = "isi", format = "plaintext")
M <- completo

####ANALISIS SCOPUS###
#completo <- readFiles("C:/Users/Juan/Desktop/scopus.bib")
#completo <- convert2df(completo, dbsource = "scopus", format = "bibtex")

###Filter###
#base_completa = unique(completo %>%
#                         select(UT, AF, TI, SO, AB, C1, PY, WC))


base_completa <- read_delim("C:/Users/jsokil/Documents/LatinR_2019/01-Bases/base_completa.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)


###Genera base autores###
autores <- base_completa %>% 
  select(UT, PY, AF) %>% 
  separate_rows(AF, sep=';')  


autores <- separate(autores, AF, into=c("apellido", "nombre"), sep=',', remove = FALSE)
autores$nombre <- str_trim(autores$nombre, side = c("both"))
autores <- separate(autores, nombre, into=c("primer_nombre", "segundo_nombre"), sep=' ', remove = FALSE)
autores$primer_nombre <- str_trim(autores$primer_nombre, side = c("both"))

gender_df <- gender(autores$primer_nombre)
authors <-   unique(left_join(autores, gender_df, by = c("primer_nombre" = "name")))
authors2 <- authors[-1]
authors_unique <- unique(authors2)


#tabla <- authors_unique %>%
#  group_by(PY, gender) %>%
#  count()

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




####Minimum Spanning Tree###
min_spanning_tree <- mst(g, weights = E(g)$weight)
#subgraph#
min_spanning_tree <- induced.subgraph(min_spanning_tree, which(degree(min_spanning_tree) >3))

#pathfinder(g)


colores_genero <- c("#D79910", "#1B9E70")

set.seed(1977)
plot.igraph(min_spanning_tree, layout=layout_with_fr(min_spanning_tree, niter=100),vertex.color=colores_genero[as.numeric(as.factor(vertex_attr(min_spanning_tree, "gender")))], vertex.shape="circle",
            edge.width=E(min_spanning_tree)$weight*10, vertex.size=degree(min_spanning_tree)*10, vertex.label=NA)

set.seed(1977)
plot.igraph(min_spanning_tree, layout=layout_randomly,vertex.color=colores_genero[as.numeric(as.factor(vertex_attr(min_spanning_tree, "gender")))], vertex.shape="circle",
            edge.width=E(min_spanning_tree)$weight*0.1, vertex.size=degree(min_spanning_tree)*5, vertex.label=NA)

set.seed(1977)
plot.igraph(min_spanning_tree, layout=layout_with_drl,vertex.color=colores_genero[as.numeric(as.factor(vertex_attr(min_spanning_tree, "gender")))], vertex.shape="circle",
            edge.width=E(min_spanning_tree)$weight*0.1, vertex.size=degree(min_spanning_tree)*5, vertex.label=NA)


dev.off()
