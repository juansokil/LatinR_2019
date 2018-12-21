
##El primer paso es juntar todos los txt en un archivo
#copy *.txt completo.txt desde cmd




library(readr)
library(dplyr)
library(tidyr)
library(gender)
library(stringr)
library(bibliometrix)

completo <- read_delim("https://raw.githubusercontent.com/juansokil/womens_studies/master/00-Csv/savedrecs%20(1).txt","\t", escape_double = FALSE, col_types = cols(DA = col_skip()),trim_ws = TRUE)

###Filter###
base_completa = unique(completo %>%
                         filter (DT %in% c('Article; Proceedings Paper', 'Article','Article; Early Access')) %>%
                         select(UT, AF, TI, SO, AB, C1, PY, WC))


autores <- base_completa %>% 
  select(UT, AF) %>% 
  separate_rows(AF, sep=';')  


autores <- separate(autores, AF, into=c("apellido", "nombre"), sep=',', remove = FALSE)
autores$nombre <- str_trim(autores$nombre, side = c("both"))
autores <- separate(autores, nombre, into=c("primer_nombre", "segundo_nombre"), sep=' ', remove = FALSE)
autores$primer_nombre <- str_trim(autores$primer_nombre, side = c("both"))

gender_df <- gender(autores$primer_nombre)
listado_autores <-   unique(left_join(autores, gender_df, by = c("primer_nombre" = "name")))

table(listado_autores$gender)






####VERSION CON BIBLIOMETRIX####
#https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html

completo <- readFiles("C:/Users/Juan/Desktop/prueba.bib")
M <- convert2df(completo, dbsource = "isi", format = "bibtex")
results <- biblioAnalysis(M, sep = ";")  
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)

#A <- cocMatrix(M, Field = "SO", sep = ";")
#sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

#A <- cocMatrix(M, Field = "CR", sep = ".  ")
#A <- cocMatrix(M, Field = "AU", sep = ";")

NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)








####

ruta <-here('/womens_studies/00-Csv/savedrecs002.txt')
bla <- paste0(ruta,"*.txt")
bla

