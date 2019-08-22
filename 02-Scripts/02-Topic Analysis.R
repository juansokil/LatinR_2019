
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


topic22 <- base_analisis %>%
  filter(topic == 22) %>%
  select(UT, PY, text,topic)


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
