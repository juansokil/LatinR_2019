


library(here)
library(data.table)






DT = do.call(rbind, lapply(files, fread))
DT = rbindlist(lapply(files, fread))


tbl <-list.files(pattern = "*.csv") %>% map_df(~read_csv(.))



tbl <- list.files(path = "./subdirectory/", pattern = "*.csv", full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 