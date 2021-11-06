## Extract references from Anystyle
nolayout = T
format = '--no-layout'

packs = c('data.table','pdftools','pbapply','stringr')
need = packs[!packs %in% installed.packages()[,'Package']]
sapply(need,install.packages)

lapply(packs,require,character.only = T)

dir.create('reference_extracts/')

already_extracted = list.files('reference_extracts',full.names = T,recursive = T,pattern = 'json')
doc_dir = 'documents'

fls = list.files(doc_dir,recursive = T,pattern = 'PDF$|pdf$',full.names = T)
dirs = dirname(fls)
json_files <- gsub('^documents','reference_extracts',fls)
json_files <- gsub('PDF$|pdf$','json',json_files)
json_dirs <- dirname(json_files)
sapply(unique(json_dirs[!dir.exists(json_dirs)]),dir.create,recursive = T)
still_need = !json_files %in% already_extracted
pblapply(seq_along(json_files[still_need]),function(i){
    system(paste('anystyle --overwrite -f json,xml,csl find --no-layout',fls[still_need][i],' ',json_dirs[still_need][i]))
  },cl = 4)


