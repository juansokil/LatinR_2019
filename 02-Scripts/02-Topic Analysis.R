
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(yarrr)
library(udpipe)
library(igraph)
library(ggraph)
library(Cairo)


base_analisis <- read_csv("LatinR_2019/01-Bases/base_topicos.csv")


View(base_analisis)

paises_colaboracion <- read_delim("C:/Users/jsokil/Desktop/paises_colaboracion.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

paises_colaboracion2 <- unique(paises_colaboracion)

 
listado <- paises_colaboracion2 %>%
  group_by(subcontinente)  %>%
  summarize(cantidad=n_distinct(ut))  %>%
  arrange(desc(cantidad))


View(listado)

write.csv(base_analisis, 'paises.csv')


topic22 <- base_analisis %>%
  filter(topic == 22) %>%
  select(UT, PY, text,topic)



View(paises_colaboracion)

################Defino el modelo UDPIPE##############
#udmodel <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = here("/english-ewt-ud-2.4-190531.udpipe"))

############LEMMATIZACION######
x <- udpipe_annotate(udmodel_english, x = topic22$text, trace = TRUE, doc_id = topic22$UT)
x <- as.data.frame(x)


####ANALISIS DE GRAFOS#####
cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"), skipgram = 10)

wordnetwork <- head(cooc, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)

g<- simplify(wordnetwork, remove.multiple = TRUE)

##########################PODA DEL GRAFO######################
####Minimum Spanning Tree###
min_spanning_tree <- mst(g, weights = E(g)$weight)
#min_spanning_tree <- g
####Clusterizaciones ####
rw <- cluster_walktrap(g, weights = E(g)$weight, steps = 2,
                       merges = TRUE, modularity = TRUE, membership = TRUE)
fg <- fastgreedy.community(as.undirected(g))


layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]
par(mfrow=c(4,4), mar=c(1,1,1,1))
set.seed(1991) 
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(g)) 
  plot(min_spanning_tree, edge.arrow.mode=0, layout=l, main=layout, 
       vertex.size=degree(g)*1.2, vertex.label.cex=0.5,vertex.label.color="black",
       vertex.color=fg$membership, vertex.shape="circle",
       edge.width=E(g)$weight/15) }

par(mfrow=c(1,1))


####DEBO SELECCIONAR ####

dev.off()
CairoSVG(file="plotsfinal2.svg", width=11, height=8.5, family="Helvetica", pointsize=11)
set.seed(1991)
plot(min_spanning_tree, edge.arrow.mode=0, layout=layout_with_fr, main=layout, 
     vertex.size=degree(g)*1.2, vertex.label.cex=0.5,vertex.label.color="black",
     vertex.color=fg$membership, vertex.shape="circle",
     edge.width=E(g)$weight/15) 
dev.off()








base_completa <- read_delim("C:/Users/jsokil/Documents/LatinR_2019/01-Bases/base_completa.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)


View(base_completa)
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


###############Insumos para grafos##################

###Genera Nodos###
nodos <- authors %>% group_by(AF) %>% 
  filter (!is.na(gender)) %>% summarise(gender = max(gender))

###Genera Aristas###
authors2 = authors %>% filter (!is.na(gender)) 
aristas <- cooccurrence(x = authors2, term = "AF", group = c("UT"))

###Arma grafo###
grafo <- graph_from_data_frame(aristas, directed=FALSE, vertices=nodos)
g<- simplify(grafo, remove.multiple = TRUE)
g <- set_vertex_attr(g, "Grado", value = degree(g))
str(vertex.attributes(g))



#####GUARDA EL GRAFO#####
write_graph(g, './grafo_gephi.gml', format = "gml")
g <- read_graph('./grafo_gephi.gml', format = "gml")

