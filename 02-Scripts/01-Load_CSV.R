
library(here)
library(data.table)


# Get the files names
setwd(here('./womens_studies/00-Csv/'))


###List Files
files = list.files(pattern="*.txt")
files

# Load the Files
DT = do.call(rbind, lapply(files, fread))





