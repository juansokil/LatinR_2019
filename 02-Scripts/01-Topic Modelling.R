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
library(ldatuning)
library(scales)
library(Cairo)
library(data.table)
library(LDAvis)




#############LATINR#############
base_completa = read.csv("https://raw.githubusercontent.com/juansokil/LatinR_2019/master/01-Bases/base_reduce.txt", sep='\t', encoding='latin1', stringsAsFactors=FALSE)
base_completa = read.csv("C:/Users/jsokil/Documents/LatinR_2019/01-Bases/base_completa.txt", sep='\t', stringsAsFactors=FALSE, header = TRUE,check.names=FALSE)

abstract <- base_completa %>% 
  select(UT, PY, AB) %>% 
  filter(!is.na(AB))  %>% 
  filter(!is.na(PY) & PY > 2007 & PY < 2019)


################Defino el modelo UDPIPE##############
#udmodel <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = "./english-ewt-ud-2.4-190531.udpipe")
#udmodel_english <- udpipe_load_model("C:/Users/Juan/Documents/english-ewt-ud-2.3-181115.udpipe")


##############TOKENS#######################

data(stop_words)
stop_words_domain <- tbl_df(c("purpose", "objective", "study", "conclusion","gender","published",
                              "elsevier","the","research","of","with","gender","elsevier","article",
                              "social","women's","methods","results","analysis","conclusions",
                              "findings","background","result","examine","method","report",
                              "explore","suggest","aim","conclusion","article","univ","na","usa",
                              "dept","studies","university","research","model","sample","paper",
                              'abstract', 'methodology', 'materials', 'discussion',  'examine','emerald', 'group', 'publishing', 
                              'limit','limitations','implications', 'author',  'copyright',  'publication',  'publications', 
                              'sage',  'taylor', 'francis', 'franci', 'informa', 'springer',  'science', 'llc', 'â',
                              'reported', 'journal', 'related', 'de', 'la', 'literature','qualitative', 'discussed', 'authors', 'data', 'examine', 'examined','experience', 'argue', 'aim','focus','focused','context','theory','measure', 'measured', 'interview'
)) %>% rename(word = value)


frequent_words <- abstract %>%
  unnest_tokens(word, AB, strip_numeric = TRUE) %>%
  anti_join(stop_words) %>%
  anti_join(stop_words_domain) %>%
  count(word, sort = TRUE)


abstract2 <- abstract %>%
  unnest_tokens(word, AB, strip_punct =FALSE, strip_numeric = TRUE) %>%
  anti_join(stop_words)  %>% 
  anti_join(stop_words_domain) %>%
  group_by(UT, PY) %>%
  summarize(text = str_c(word, collapse = " "))


abstract = full_join(abstract2, abstract, by = c("UT", "PY"))

###remuevo objetos###
rm(abstract2, stop_words, base_completa)

#https://bnosac.github.io/udpipe/docs/doc6.html
#http://ufal.mff.cuni.cz/udpipe/users-manual



############LEMMATIZACION######
x <- udpipe_annotate(udmodel_english, x = abstract$text, trace = TRUE, doc_id = abstract$UT)
x <- as.data.frame(x)

data_lemmatizada <- x %>% 
  group_by(doc_id) %>% 
  summarise(text = str_c(lemma, collapse = " "))

genera_tokens <- data_lemmatizada %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 4, n_min=1) %>% #, Trigamas, Bigramas y Unigramas
  mutate(ngrama=ngram)  %>%
  select(doc_id,ngrama) %>%  
  filter(!nchar(ngrama) < 5) 


###EN ESTE PUNTO GENERO TODOS LOS TOKENS POR REGISTRO#############
####DEBERIA TENER 3 MIL DOC_ID DISTINTOS
genera_tokens2 <- genera_tokens %>%
  count(doc_id, ngrama) %>%
  distinct() %>%
  ungroup() 

length(unique(genera_tokens2$doc_id))

conteo_palabras <- genera_tokens2 %>%
  group_by(ngrama) %>%
  count(ngrama, sort = TRUE) %>%
  filter(n >5)

##############FILTRO DE PALABRAS MUY FRECUENTES, APARECEN EN MAS DEL 20% de los papers##################
genera_tokens3 <- genera_tokens2 %>%
  filter(ngrama != 'woman') %>%
  filter(ngrama != 'women') %>%
  filter(ngrama != 'health') 


#write.csv(conteo_palabras, 'palabras.csv')

#############Genera el DTM###############

dtm <- genera_tokens3 %>%
  cast_dtm(document=doc_id, term=ngrama, value=n)

dtm

dtm_not_sparce <- removeSparseTerms(dtm, sparse = .99)
dtm_not_sparce

rowTotals <- apply(dtm_not_sparce , 1, sum) #Find the sum of words in each Document
dtm_final   <- dtm_not_sparce[rowTotals> 0, ]           #remove all docs without words
glimpse(dtm_final)
dtm_final$dimnames$Terms







####LDA TUNING#####
#####http://www.bernhardlearns.com/2017/05/topic-models-lda-and-ctm-in-r-with.html###

control_list_gibbs <- list(burnin = 25,iter = 50,seed = 0:4,nstart = 5,best = TRUE)

system.time(
  topic_number_lemma <- FindTopicsNumber(
    dtm_final,
    topics = 2:100,
    metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = control_list_gibbs,
    verbose = TRUE
  )
)


FindTopicsNumber_plot(topic_number_lemma)

para <- tibble(k = c(2,3,9,12,50))

system.time(
  lemma_tm <- para %>%
    mutate(lda = map(k, 
                     function(k) LDA(
                       k=k, 
                       x=dtm_final, 
                       method="Gibbs", 
                       control=control_list_gibbs
                     )
    )
    )
)






####LDA# SE PUEDE GENERAR CON EL NUMERO OPTIMO################
ap_lda <- LDA(dtm_final, k = 6, control = list(seed = 1234, alpha = 0.1))
ap_lda

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics


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


####ANALISIS DE GRAFOS#####
cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"), skipgram = 10)

wordnetwork <- head(cooc, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)

g<- simplify(wordnetwork, remove.multiple = TRUE)

##########################PODA DEL GRAFO######################
####Minimum Spanning Tree###
min_spanning_tree <- mst(g, weights = E(g)$weight)

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


