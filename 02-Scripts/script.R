##Core Collection of WOS##
##(TS='gender' AND SU='Social Sciences') or WC="Women's Studies"
#baje 2005 y 2006
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
library(yarrr)


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

View(tabla)




##############TOKENS#######################

###Genera base abstracts###
abstract <- base_completa %>% 
  select(UT, PY, AB) %>% 
filter(!is.na(AB))  %>% 
filter(!is.na(PY) & PY < 2019)  


###carga stopwords###
data(stop_words)
###define nuevas stopwords###
undesirable_words <- c("purpose", "objective", "study", "lyrics","repeats", "la", "da", "uh", "ah")


#Create tidy text format: Unnested, Unsummarized, -Undesirables, Stop and Short words
genera_tokens <- abstract %>%
  #unnest_tokens(bigram, AB, token = "ngrams", n = 2)
  unnest_tokens(word, AB) %>% #Break the lyrics into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 4) %>% #Words like "ah" or "oo" used in music
  anti_join(stop_words) #Data provided by the tidytext package

##https://www.datacamp.com/community/tutorials/sentiment-analysis-R






###cuenta la cantidad de palabra por articulo###
word_summary <- genera_tokens %>%
  group_by(PY, UT) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(UT, PY, word_count) %>%
  distinct() %>% #To obtain one record per song
  ungroup()



###Arma diccionario de palabras###
word_dictionary <- genera_tokens %>%
  group_by(word) %>%
  distinct() %>% #To obtain one record per song
  ungroup()




pirateplot(formula =  word_count ~ PY , #Formula
           data = word_summary, #Data frame
           xlab = NULL, ylab = "Recuento de Palabras distintas", #Axis labels
           main = "Diversidad de léxico por año", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size












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








####

ruta <-here('/womens_studies/00-Csv/savedrecs002.txt')
bla <- paste0(ruta,"*.txt")
bla

