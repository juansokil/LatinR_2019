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
  select(UT, AF) %>% 
  separate_rows(AF, sep=';')  

autores <- separate(autores, AF, into=c("apellido", "nombre"), sep=',', remove = FALSE)
autores$nombre <- str_trim(autores$nombre, side = c("both"))
autores <- separate(autores, nombre, into=c("primer_nombre", "segundo_nombre"), sep=' ', remove = FALSE)
autores$primer_nombre <- str_trim(autores$primer_nombre, side = c("both"))
gender_df <- gender(autores$primer_nombre)
authors <-   unique(left_join(autores, gender_df, by = c("primer_nombre" = "name")))

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

base_completa <- left_join(base_completa, author_male, by = c("UT" = "UT") )
base_completa <- left_join(base_completa, author_female, by = c("UT" = "UT") )

table(base_completa$author_female)
table(base_completa$author_male)

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

