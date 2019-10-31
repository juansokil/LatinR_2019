library(dplyr)
library(readr)
library(rlang)
library(tidyr)
library(udpipe)
library(igraph)
library(scales)
library(Cairo)


total_indices <- read_delim("./base_completa_ldatopicos.txt", "\t", escape_double = FALSE, trim_ws = TRUE)


######################ARMA NODOS############################
###Posicion fija de los nodos####

base <- total_indices %>%
  select(-TI) %>%
  select(UT,gender.x, gender.y, starts_with("T"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
mutate_all(funs(replace(., . <= 0.01, NA))) %>%
  gather(key = "topico", value = "participacion", -c(UT, gender.x, gender.y)) %>%
  filter(!is.na(participacion))
nodos <- base %>% group_by(topico) %>% summarise(n = n())
set.seed(123)
g <- barabasi.game(100)

coords <- layout_with_fr(g, niter = 500)*4

nodos_fijos <- cbind(nodos,coords)
colnames(nodos_fijos) <- c("topico", "weight","x","y")
View(nodos_fijos)




############IDENTIFICO HOMBRES#############################################

base <- total_indices %>%
  filter(gender.x=='male' & is.na(gender.y)) %>%
  select(-TI) %>%
  select(UT,  starts_with("T"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
mutate_all(funs(replace(., . <= 0.15, NA))) %>%
  gather(key = "topico", value = "participacion", -c(UT)) %>%
  filter(!is.na(participacion))

nodos <- base %>% mutate(total=n_distinct(UT))  %>%  group_by(topico, total) %>% summarise(n = n()) %>% mutate(importancia=(n/total)*100)
nodos <- nodos %>% left_join(nodos_fijos, by=c('topico'='topico'))

##################Arma Aristas###############
cooc <- cooccurrence(x = base , term = "topico", group = c("UT"))
wordnetwork <- graph_from_data_frame(cooc, directed=F, vertices=nodos)

vertex_attr(g) %>% glimpse()


###Agrega atributos###
wordnetwork <- set_vertex_attr(wordnetwork, "Importancia", value = degree(wordnetwork))
wordnetwork <- set_edge_attr(wordnetwork, "weight", value= cooc$cooc)

###Agrega atributos###

wordnetwork <- set_vertex_attr(wordnetwork, "Grado", value = degree(wordnetwork))

g<- simplify(wordnetwork, remove.multiple = TRUE)
is_weighted(g)

edge_attr(g) %>% glimpse()
edge_attr(g)$weight

##########################PODA DEL GRAFO######################
####Minimum Spanning Tree###
#min_spanning_tree <- mst(g, weights = E(g)$weight)
#g <- min_spanning_tree

w <- cluster_walktrap(g, weights = E(g)$weight, steps = 2,
                      merges = TRUE, modularity = TRUE, membership = TRUE)
fg <- fastgreedy.community(as.undirected(g))



CairoSVG(file="./grafo_mujeres.svg", width=11, height=8.5, family="Helvetica", pointsize=11)
set.seed(1492)
grafo <- plot(g, edge.arrow.mode=0, edge.color="orange",
              vertex.size=V(g)$importancia*1, 
              #vertex.color=w$membership,
              vertex.color='green',
              vertex.label.cex=0.5,vertex.label.color="black",
              edge.width = E(g)$weight*0.5, edge.curved=.33)


#####GUARDA EL GRAFO#####
write_graph(g, 'grafo_mujeres.gml', format = "gml")

graph.density(g,loop=FALSE)

dev.off()


############IDENTIFICO MUJERES#############################################

base <- total_indices %>%
  filter(is.na(gender.x) & gender.y =='female') %>%
  select(-TI) %>%
  select(UT,  starts_with("T"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
mutate_all(funs(replace(., . <= 0.15, NA))) %>%
  gather(key = "topico", value = "participacion", -c(UT)) %>%
  filter(!is.na(participacion))

nodos <- base %>% mutate(total=n_distinct(UT))  %>%  group_by(topico, total) %>% summarise(n = n()) %>% mutate(importancia=(n/total)*100)
nodos <- nodos %>% left_join(nodos_fijos, by=c('topico'='topico'))


##################Arma Aristas###############
cooc <- cooccurrence(x = base , term = "topico", group = c("UT"))
wordnetwork <- graph_from_data_frame(cooc, directed=F, vertices=nodos)

vertex_attr(g) %>% glimpse()


###Agrega atributos###
wordnetwork <- set_vertex_attr(wordnetwork, "Importancia", value = degree(wordnetwork))
wordnetwork <- set_edge_attr(wordnetwork, "weight", value= cooc$cooc)

###Agrega atributos###

wordnetwork <- set_vertex_attr(wordnetwork, "Grado", value = degree(wordnetwork))

g<- simplify(wordnetwork, remove.multiple = TRUE)
is_weighted(g)

edge_attr(g) %>% glimpse()
edge_attr(g)$weight

##########################PODA DEL GRAFO######################
####Minimum Spanning Tree###
#min_spanning_tree <- mst(g, weights = E(g)$weight)
#g <- min_spanning_tree

w <- cluster_walktrap(g, weights = E(g)$weight, steps = 2,
                      merges = TRUE, modularity = TRUE, membership = TRUE)
fg <- fastgreedy.community(as.undirected(g))




graph.density(g,loop=FALSE)




CairoSVG(file="./grafo_hombres.svg", width=11, height=8.5, family="Helvetica", pointsize=11)
set.seed(1492)
grafo <- plot(g, edge.arrow.mode=0, edge.color="orange",
              vertex.size=V(g)$importancia*1, 
              #vertex.color=w$membership,
              vertex.color='green',
              vertex.label.cex=0.5,vertex.label.color="black",
              edge.width = E(g)$weight*0.5, edge.curved=.33)


#####GUARDA EL GRAFO#####
write_graph(g, 'grafo_hombres.gml', format = "gml")

graph.density(g,loop=FALSE)

dev.off()


