
##El primer paso es juntar todos los txt en un archivo
#copy *.txt completo.txt desde cmd

library(readr)
library(dplyr)


completo <- read_delim("GitHub/00-Csv/completo.txt","\t", escape_double = FALSE, col_types = cols(DA = col_skip()),trim_ws = TRUE)


###Filter###
base_completa = unique(completo %>%
  filter (DT %in% c('Article; Proceedings Paper', 'Article','Article; Early Access')) %>%
  select(UT, AF, TI, SO, AB, C1, PY, WC))

write.csv(base_completa, 'base_completa.csv')

View(base_completa)

library(here)
library(data.table)
library(tidyr)
library(tidyverse)
library(dplyr)
# Get the files names
ruta = setwd('C:/Users/Juan/Desktop/00-Csv//')


###List Files
files = list.files(pattern="*.txt")


# Load the Files
DT = do.call(rbind, lapply(files, fread, sep="\t"))



## Read data using fread
readdata <- function(fn){
  dt_temp <- fread(fn)
  return(dt_temp)
  
}



bla <- read_delim(here('./womens_studies/00-Csv/savedrecs002.txt'), "\t")


ruta <-here('/womens_studies/00-Csv/savedrecs002.txt')
bla <- paste0(ruta,"*.txt")
bla


savedrecs001 <- read_delim("~/womens_studies/00-Csv/savedrecs001.txt", "\t", escape_double = FALSE)
