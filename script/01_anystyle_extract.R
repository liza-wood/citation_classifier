setwd("~/Box/citation_classifier/")
## Extract references from Anystyle

# Packages
packs = c('data.table','pdftools','pbapply','stringr')
need = packs[!packs %in% installed.packages()[,'Package']]
sapply(need,install.packages)
lapply(packs,require,character.only = T)

# Create directory where Anystyle extractions will be held
dir.create('reference_extracts_trial1_nolayout/')
# Check to see if any files have already been extracted
already_extracted = list.files('reference_extracts_trial1_nolayout',full.names = T,recursive = T,pattern = 'json')
# Identify the document directory where the PDFs are held
doc_dir = 'documents_trial1'
# List the documents that you have to analyze
fls = list.files(doc_dir,recursive = T,pattern = 'PDF$|pdf$',full.names = T)
# Get the directory names for all of the files
dirs = dirname(fls)
#Pull out the names of the documents to analyze and rename name with reference extracts filepaths and with json extensions
json_files <- gsub('^documents_trial1','reference_extracts_trial1_nolayout',fls)
json_files <- gsub('PDF$|pdf$','json',json_files)
# Then get their directory
json_dirs <- dirname(json_files)
# Identify the files that have not yet been extracted
sapply(unique(json_dirs[!dir.exists(json_dirs)]),dir.create,recursive = T)
still_need = !json_files %in% already_extracted

# Then go through the json files we still need to analyze
pblapply(seq_along(json_files[still_need]),function(i){
    system(paste('anystyle --overwrite -f json find --no-layout',fls[still_need][i],' ',json_dirs[still_need][i]))
  },cl = 4)


# Again with layout
dir.create('reference_extracts_trial1_layout/')
already_extracted = list.files('reference_extracts_trial1_layout',full.names = T,recursive = T,pattern = 'json')
doc_dir = 'documents_trial1'
fls = list.files(doc_dir,recursive = T,pattern = 'PDF$|pdf$',full.names = T)
dirs = dirname(fls)
json_files <- gsub('^documents_trial1','reference_extracts_trial1_layout',fls)
json_files <- gsub('PDF$|pdf$','json',json_files)
json_dirs <- dirname(json_files)
sapply(unique(json_dirs[!dir.exists(json_dirs)]),dir.create,recursive = T)
still_need = !json_files %in% already_extracted

pblapply(seq_along(json_files[still_need]),function(i){
  system(paste('anystyle --overwrite -f json find',fls[still_need][i],' ',json_dirs[still_need][i]))
},cl = 4)
