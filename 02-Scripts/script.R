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

#udmodel <- udpipe_download_model(language = "english")

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


View(base_completa)

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


##############TOKENS#######################
###Genera base abstracts###
abstract <- base_completa %>% 
  select(UT, PY, AB) %>% 
  filter(!is.na(AB))  %>% 
  filter(!is.na(PY) & PY > 2007 & PY < 2019)


abstract %>%
  group_by (PY) %>%
  summarize (cantidad=n())%>%
  ggplot(aes(PY,cantidad)) +
  geom_bar(stat='identity')


#udmodel <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = "C:/Users/Juan/Documents/english-ewt-ud-2.3-181115.udpipe")

x <- udpipe_annotate(udmodel_english, x = abstract$AB, trace = TRUE)
x <- as.data.frame(x)



####ARMAR GRAFOS#####
cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))

wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout  = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "purple", alpha=0.5) +
  geom_node_text(aes(label = name), col = "black", size = 5) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Co-ocurrencias en la misma oración", subtitle = "Sustantivos y Adjetivos")




relacion <- as.data.frame(cbind(unique(x$doc_id),abstract$UT))
bla <- full_join(x, relacion, by = c("doc_id"="V1"))
data_lemmatizada <- bla %>% 
  group_by(doc_id,V2) %>% 
  summarise(text = str_c(lemma, collapse = " "))

data_lemmatizada <- full_join(abstract, data_lemmatizada, by = c("UT"="V2"))



###carga stopwords###
data(stop_words)
###define nuevas stopwords###
undesirable_words <- c("purpose", "objective", "study", "conclusion","gender","published",
                       "elsevier","the","research","of","with","gender","elsevier","article",
                       "women","social","women's","methods","results","analysis","conclusions",
                       "findings","background","woman","result","examine","method","report",
                       "explore","suggest")


genera_tokens <- data_lemmatizada %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 4, n_min=1) %>% #, Trigamas, Bigramas y Unigramas
  mutate(ngrama=ngram) %>%
  separate(ngram, c("word1", "word2", "word3","word4"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !word4 %in% stop_words$word) %>%
  filter(!word1 %in% undesirable_words,
         !word2 %in% undesirable_words,
         !word3 %in% undesirable_words,
         !word4 %in% undesirable_words) %>%
          select(UT,ngrama) %>%
          filter(!str_detect(ngrama, "[0-9]")) %>%  
          filter(!nchar(ngrama) < 5) 


################
genera_tokens2 <- genera_tokens %>%
  count(UT, ngrama) %>%
  distinct() %>%
  ungroup()

dtm <- genera_tokens2 %>%
  cast_dtm(UT, ngrama, n)

bla <- removeSparseTerms(dtm, 0.99)
rowTotals <- apply(bla , 1, sum) #Find the sum of words in each Document
bla.new   <- bla[rowTotals> 0, ]           #remove all docs without words


####LDA####
ap_lda <- LDA(dtm, k = 4, control = list(seed = 1234))
ap_lda

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

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

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

####https://www.tidytextmining.com/topicmodeling.html
#https://cran.r-project.org/web/packages/tidytext/vignettes/tidying_casting.html
#https://github.com/dgrtwo/tidy-text-mining/blob/master/04-word-combinations.Rmd
##https://www.datacamp.com/community/tutorials/sentiment-analysis-R
##https://www.r-bloggers.com/udpipe-version-0-7-for-natural-language-processing-nlp-alongside-tidytext-quanteda-tm/##













####ANALISIS BIBLIOMETRICO####
#https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html
#http://bibliometrix.org/documents/bibliometrix_Report.html

results <- biblioAnalysis(M, sep = ";")  
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)

NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)


NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)


NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", n = 50, Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=3,label.cex=TRUE,label.n=30,edges.min=2)

netstat <- networkStat(NetMatrix)
summary(netstat,k=10)

termExtraction(M, Field = "TI", stemming = FALSE,
               language = "english", remove.numbers = TRUE, remove.terms = NULL,
               keep.terms = NULL, synonyms = NULL, verbose = TRUE)



M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration",  network = "countries", sep = ";")
networkPlot(NetMatrix,  n = dim(NetMatrix)[1], Title = "Country collaboration",type = "sphere", size=10,size.cex=T,edgesize = 1,labelsize=0.6, cluster="none")


NetMatrix <- biblioNetwork(M, analysis = "collaboration",  network = "universities", sep = ";")
net=networkPlot(NetMatrix,  n = 50, Title = "Edu collaboration",type = "auto", size=10,size.cex=T,edgesize = 3,labelsize=0.6)




