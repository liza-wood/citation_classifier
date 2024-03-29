## Combined anystyle jsons
library(data.table)
library(jsonlite)
library(pbapply)
library(magrittr)
setwd("~/Box/citation_classifier/")
#system('ln -s ~/Box/truckee/reference_extracts/ ~/Documents/GitHub/truckee/reference_extracts')
#system('ln -s ~/Box/truckee/data/ ~/Documents/GitHub/truckee/data')

main_dir = 'reference_extracts_trial1_nolayout'

fl = list.files(main_dir,full.names = T,pattern = 'json',recursive = T)
fl_list = pblapply(fl,function(x) fromJSON(x) %>% data.table() %>% .[,File:=x],cl = 4)
retry = which(sapply(fl_list,class)=='try-error')
replace_entries = pblapply(fl[retry],function(x) fromJSON(x) %>% data.table() %>% .[,File:=basename(x)])
fl_list[retry] <- replace_entries
fl_dt = rbindlist(fl_list,fill = T,use.names=T)

dt = data.table()
if(nrow(fl_dt)==0){next}
  fl_dt = fl_dt[nchar(title)<400,]
  dt_nolayout <- rbind(dt,fl_dt,use.names = T,fill = T)
  
  
main_dir = 'reference_extracts_trial1_layout'

fl = list.files(main_dir,full.names = T,pattern = 'json',recursive = T)
fl_list = pblapply(fl,function(x) fromJSON(x) %>% data.table() %>% .[,File:=x],cl = 4)
retry = which(sapply(fl_list,class)=='try-error')
replace_entries = pblapply(fl[retry],function(x) fromJSON(x) %>% data.table() %>% .[,File:=basename(x)])
fl_list[retry] <- replace_entries
fl_dt = rbindlist(fl_list,fill = T,use.names=T)
  
dt = data.table()
if(nrow(fl_dt)==0){next}
fl_dt = fl_dt[nchar(title)<400,]
dt_layout <- rbind(dt,fl_dt,use.names = T,fill = T)

#dt <- full_join(dt_layout, dt_nolayout)
#dt <- data.frame(dt)
#dt <- unique(dt)

dt <- dt_nolayout
  
dir.create('data/') 
saveRDS(object = dt, file = 'data/trial1_references.RDS')
