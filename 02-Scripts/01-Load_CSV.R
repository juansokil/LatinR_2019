
library(here)
library(data.table)
library(tidyr)
library(tidyverse)
library(dplyr)
# Get the files names
setwd(here('./womens_studies/00-Csv/'))


###List Files
files = list.files(pattern="*.txt")


# Load the Files
DT = do.call(rbind, lapply(files, fread))



## Read data using fread
readdata <- function(fn){
  dt_temp <- fread(fn, sep=",")
  return(dt_temp)
  
}


bla <- read_delim(here('./womens_studies/00-Csv/savedrecs002.txt'), "\t")


ruta <-here('/womens_studies/00-Csv/savedrecs002.txt')
bla <- paste0(ruta,"*.txt")
  bla

savedrecs001 <- read_delim("~/womens_studies/00-Csv/savedrecs001.txt", "\t", escape_double = FALSE)





View(bla)







article <- DT %>% distinct(GA,TI)


View(DT)


