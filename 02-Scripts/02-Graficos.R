library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readr)
library(rlang)
library(igraph)


# how to create a dataframe in r
base <- data.frame ('Año'=2008:2018, 
'Mujeres_Total'=      as.double(c(0.661,  0.672,  0.682,  0.689,  0.681,  0.685,  0.679,  0.660,  0.680,  0.666,  0.676)),
'Hombres_Total' =     as.double(c(0.339,  0.328,  0.318,  0.311,  0.319,  0.315,  0.321,  0.340,  0.320,  0.334,  0.324)),
'Mujeres_Completos' = as.double(c(0.685,  0.693,  0.701,  0.709,  0.707,  0.708,  0.706,  0.681,  0.705,  0.685,  0.696)),
'Hombres_Completos' = as.double(c(0.315,  0.307,  0.299,  0.291,  0.293,  0.292,  0.294,  0.319,  0.295,  0.315,  0.304)),
'Autores' = as.double(c(0.44303797, 0.44731475, 0.43200506, 0.44122966, 0.43726474, 0.43933054, 0.43560389, 0.4342389, 0.42588235, 0.45063575, 0.4614)),
'Autoras' = as.double(c(0.8622487, 0.86199864, 0.8570525, 0.86256781, 0.85884567, 0.86371787, 0.85060103, 0.84821044, 0.85843137,0.84891548,0.8598)),
'Co_Mujeres' =      as.double(c( 0.557, 0.553, 0.568,0.559, 0.563, 0.561, 0.564, 0.566, 0.574,  0.55,  0.54)), 
'Co_Compartido' =      as.double(c(0.305, 0.309, 0.289, 0.304, 0.296, 0.303, 0.286, 0.282, 0.284, 0.299, 0.316)),
'Co_Hombres' =      as.double(c(0.138, 0.138, 0.143, 0.137, 0.141, 0.136, 0.149, 0.152, 0.142, 0.152, 0.143)))




###Autores###
base %>%
   select(Año, Mujeres_Completos, Hombres_Completos) %>%
   gather(key = segmento, value = porcentaje, -c(Año)) %>%
   ggplot(aes(Año, porcentaje, group=segmento, color=segmento, fill=segmento)) +
   geom_bar(stat='identity', alpha=0.8) +
   scale_fill_manual(values = c("darkgreen","orange")) +
   scale_y_continuous(labels = scales::percent, limits = c(0,1))  + 
   scale_x_discrete(limits = c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))



###Participación###

base%>%
   select(Año, Autoras, Autores) %>%
   gather(key = segmento, value = porcentaje, -c(Año)) %>%
   ggplot(aes(Año, porcentaje, group=segmento, color=segmento)) +
   geom_line(alpha=0.6, size=3) + geom_point(size=5, alpha=0.8) +
   scale_colour_manual(values = c("darkgreen","orange")) +
   scale_y_continuous(labels = scales::percent, limits = c(0,1))  + 
   scale_x_discrete(limits = c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))




###Coautoria###
base2 = base %>%
   select(Año, Co_Mujeres, Co_Compartido, Co_Hombres) %>%
   gather(key = segmento, value = porcentaje, -c(Año)) 
  
base2$segmento   <- factor(base2$segmento, 
          levels = c("Co_Hombres",
                     "Co_Compartido",
                     "Co_Mujeres"))


base2 %>%
   ggplot(aes(Año, porcentaje, group=segmento, color=segmento, fill=segmento)) +
   geom_bar(stat= 'identity', position='fill', alpha=0.8) +
   scale_fill_manual(values = c("darkgreen","grey","orange")) +
   scale_y_continuous(labels = scales::percent, limits = c(0,1))  +  
   scale_x_discrete(limits = c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)) +
   coord_flip()







#ggplot(todes2, aes(topico, `as.numeric(topico)`)) + geom_tile(aes(fill = valor), colour = "white") + scale_fill_gradientn(colors=c("#eef6ec","#d6ecd2","#b7ddb0","#99d18f","#3f6f21","darkgreen")) + coord_flip()
















total_indices <- read_delim("C:/Users/Juan/Dropbox/LatinR_2019/01-Bases/base_completa_ldatopicos.txt", "\t", escape_double = FALSE, trim_ws = TRUE)


######################ARMA NODOS############################
###Posicion fija de los nodos####

base <- total_indices %>%
   select(-TI) %>%
   select(UT,gender.x, gender.y, starts_with("Topic"))  %>%
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
   select(UT,  starts_with("Topic"))  %>%
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




set.seed(1492)
grafo <- plot(g, edge.arrow.mode=0, edge.color="darkgrey",
              vertex.size=V(g)$importancia*1, 
              #vertex.color=w$membership,
              vertex.color='darkgreen',
              vertex.label.cex=0.3,vertex.label.color="black",
              edge.width = E(g)$weight*0.2, edge.curved=.33)


#####GUARDA EL GRAFO#####
write_graph(g, 'grafo_mujeres.gml', format = "gml")

graph.density(g,loop=FALSE)

dev.off()


############IDENTIFICO MUJERES#############################################

base <- total_indices %>%
   filter(is.na(gender.x) & gender.y =='female') %>%
   select(-TI) %>%
   select(UT,  starts_with("Topic"))  %>%
   #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
mutate_all(funs(replace(., . <= 0.15, NA))) %>%
   gather(key = "topico", value = "participacion", -c(UT)) %>%
   filter(!is.na(participacion))

nodos <- base %>% mutate(total=n_distinct(UT))  %>%  group_by(topico, total) %>% summarise(n = n()) %>% mutate(importancia=(n/total)*100)
nodos <- nodos %>% left_join(nodos_fijos, by=c('topico'='topico'))
View(nodos)

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





set.seed(1492)
grafo <- plot(g, edge.arrow.mode=0, edge.color="darkgrey",
              vertex.size=V(g)$importancia*1, 
              #vertex.color=w$membership,
              vertex.color='orange',
              vertex.label.cex=0.3,vertex.label.color="black",
              edge.width = E(g)$weight*0.2, edge.curved=.33)


#####GUARDA EL GRAFO#####
write_graph(g, 'grafo_hombres.gml', format = "gml")

graph.density(g,loop=FALSE)

dev.off()












############IDENTIFICO COMPARTIDO#############################################

base <- total_indices %>%
   filter(gender.x =='male' & gender.y =='female') %>%
   select(-TI) %>%
   select(UT,  starts_with("Topic"))  %>%
   #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
mutate_all(funs(replace(., . <= 0.15, NA))) %>%
   gather(key = "topico", value = "participacion", -c(UT)) %>%
   filter(!is.na(participacion))

nodos <- base %>% mutate(total=n_distinct(UT))  %>%  group_by(topico, total) %>% summarise(n = n()) %>% mutate(importancia=(n/total)*100)
nodos <- nodos %>% left_join(nodos_fijos, by=c('topico'='topico'))


View(nodos)

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





set.seed(1492)
grafo <- plot(g, edge.arrow.mode=0, edge.color="darkgrey",
              vertex.size=V(g)$importancia*1, 
              #vertex.color=w$membership,
              vertex.color='darkgrey',
              vertex.label.cex=0.6,vertex.label.color="black",
              edge.width = E(g)$weight*0.2, edge.curved=.33)


#####GUARDA EL GRAFO#####
write_graph(g, 'grafo_compartido.gml', format = "gml")

graph.density(g,loop=FALSE)

dev.off()
