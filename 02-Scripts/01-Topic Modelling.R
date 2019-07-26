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
library(tm)
#install.packages("ldatuning")
library(ldatuning)
library(scales)
library(Cairo)
library(data.table)


#############Levanta datos#####################3
base_completa = read.csv("https://raw.githubusercontent.com/juansokil/LatinR_2019/master/01-Bases/base_reduce.txt", sep='\t', encoding='latin1', stringsAsFactors=FALSE)
#base_completa = read.csv("C:/source/LatinR_2019/01-Bases/base_completa.txt", sep='\t', encoding='latin1', stringsAsFactors=FALSE)
#base_completa = head(base_completa, 150)
#glimpse(base_completa)

#download.file("https://www.dropbox.com/sh/1z68bazk9ptasqo/AAD8iT3B7n9OkRCNTLORz-zWa?dl=1", "base_completa.txt")
#base_completa = read.csv("base_completa.txt", sep='\t', encoding='latin1', stringsAsFactors=FALSE, quote="")

base_completa = read.csv("C:/Users/jsokil/Documents/LatinR_2019/01-Bases/base_completa.txt", sep='\t', stringsAsFactors=FALSE, header = TRUE,check.names=FALSE)
base_completa = head(base_completa, 3000)


#base_completa = read_csv("base_completa.txt")


################Defino el modelo UDPIPE##############
#udmodel <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = "./english-ewt-ud-2.4-190531.udpipe")
#udmodel_english <- udpipe_load_model("C:/Users/Juan/Documents/english-ewt-ud-2.3-181115.udpipe")



##############TOKENS#######################
abstract <- base_completa %>% 
  select(UT, PY, AB) %>% 
  filter(!is.na(AB))  %>% 
  filter(!is.na(PY) & PY > 2007 & PY < 2019)

data(stop_words)
undesirable_words <- c("purpose", "objective", "study", "conclusion","gender","published",
                       "elsevier","the","research","of","with","gender","elsevier","article",
                       "women","social","women's","methods","results","analysis","conclusions",
                       "findings","background","woman","result","examine","method","report",
                       "explore","suggest", "health")


abstract2 <- abstract %>%
  unnest_tokens(word, AB, strip_punct =FALSE, strip_numeric = TRUE) %>%
  anti_join(stop_words)  %>% 
  filter(!word %in% undesirable_words) %>%
  group_by(UT, PY) %>%
  summarize(text = str_c(word, collapse = " "))

abstract = full_join(abstract2, abstract, by = c("UT", "PY"))


###remuevo objetos###
rm(abstract2, stop_words, base_completa)


############LEMMATIZACION######
x <- udpipe_annotate(udmodel_english, x = abstract$text, trace = TRUE)
x <- as.data.frame(x)



relacion <- data.frame(abstract$UT, unique(x$doc_id))
glimpse(relacion)


####ARMAR GRAFOS#####
cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))


wordnetwork <- head(cooc, 100)
#wordnetwork <- head(cooc, 1000)
wordnetwork <- graph_from_data_frame(wordnetwork)

g<- simplify(wordnetwork, remove.multiple = TRUE)

##########################PODA DEL GRAFO######################
####Minimum Spanning Tree###
min_spanning_tree <- mst(g, weights = E(g)$weight)
####Maximum Spanning Tree###
#max_spanning_tree <- mst(g, weights = 1/E(g)$weight)


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
set.seed(1991)
plot(min_spanning_tree, edge.arrow.mode=0, layout=layout_with_fr, main=layout, 
     vertex.size=degree(g)*1.2, vertex.label.cex=0.5,vertex.label.color="black",
     vertex.color=fg$membership, vertex.shape="circle",
     edge.width=E(g)$weight/15) 


dev.off()
CairoSVG(file="plotsfinal2.svg", width=11, height=8.5, family="Helvetica", pointsize=11)
set.seed(1991)
plot(min_spanning_tree, edge.arrow.mode=0, layout=layout_with_fr, main=layout, 
     vertex.size=degree(g)*1.2, vertex.label.cex=0.5,vertex.label.color="black",
     vertex.color=fg$membership, vertex.shape="circle",
     edge.width=E(g)$weight/15) 
dev.off()














bla <- full_join(x, relacion, by = c("doc_id"="unique.x.doc_id."))
data_lemmatizada <- bla %>% 
  group_by(doc_id,abstract.UT) %>% 
  summarise(text = str_c(lemma, collapse = " "))

View(data_lemmatizada)
#data_lemmatizada <- full_join(abstract, data_lemmatizada, by = c("UT"="abstract.UT"))

#View(data_lemmatizada)
#data_lemmatizada$text.x[1]
#data_lemmatizada$text.y[1]


genera_tokens <- data_lemmatizada %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 4, n_min=1) %>% #, Trigamas, Bigramas y Unigramas
  mutate(ngrama=ngram)  %>%
  select(abstract.UT,ngrama) %>%  
  filter(!nchar(ngrama) < 5) 


################
genera_tokens2 <- genera_tokens %>%
  count(abstract.UT, ngrama) %>%
  distinct() %>%
  ungroup()

dtm <- genera_tokens2 %>%
  cast_dtm(abstract.UT, ngrama, n)

bla <- removeSparseTerms(dtm, 0.99)
rowTotals <- apply(bla , 1, sum) #Find the sum of words in each Document
bla.new   <- bla[rowTotals> 0, ]           #remove all docs without words
View(bla.new)



####LDA TUNING#####
#####http://www.bernhardlearns.com/2017/05/topic-models-lda-and-ctm-in-r-with.html###

control_list_gibbs <- list(burnin = 25,iter = 50,seed = 0:4,nstart = 5,best = TRUE)

system.time(
  topic_number_lemma <- FindTopicsNumber(
    dtm,
    topics = 2:100,
    metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = control_list_gibbs,
    verbose = TRUE
  )
)


system.time(
  topic_number_lemma <- FindTopicsNumber(
    dtm,
    topics = 2:100,
    metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = control_list_gibbs,
    verbose = TRUE
  ))



#score_models(models, dtm, topics = seq(10, 40, by = 10),
#             metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014"), verbose = FALSE)

#http://rpubs.com/nikita-moor/107657


FindTopicsNumber_plot(topic_number_lemma)

para <- tibble(k = c(2,3,9,12,50))

system.time(
  lemma_tm <- para %>%
    mutate(lda = map(k, 
                     function(k) LDA(
                       k=k, 
                       x=dtm, 
                       method="Gibbs", 
                       control=control_list_gibbs
                     )
    )
    )
)


####LDA#################
ap_lda <- LDA(dtm, k = 4, control = list(seed = 1234, alpha = 0.1))
ap_lda

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics


# visualize the most important terms within each topic
#http://rstudio-pubs-static.s3.amazonaws.com/266937_33212ab45f2540d099d07e203ca59812.html
ap_important_terms <- ap_topics %>%
  filter(beta > .004) %>%
  mutate(term = reorder(term, beta))

ggplot(ap_important_terms, aes(term, beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ topic, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, size = 15))


# visualize the top terms within each topic
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


################BETA#############


beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread



# get classification of each document
td_lda_docs <- tidy(ap_lda, matrix = "gamma")

doc_classes <- td_lda_docs %>%
  group_by(document) %>%
  top_n(1) %>%
  ungroup()

# which were we most uncertain about?
doc_classes %>%
  arrange(gamma)


