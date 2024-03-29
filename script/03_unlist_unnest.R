## Unlist and unnest anystyle
library(stringr)
library(data.table)
library(tools)
library(tidyr)
library(dplyr)
library(magrittr)
library(purrr)

source("~/Documents/Davis/R-Projects/citation_classifier/script/functions.R")

setwd("~/Box/citation_classifier/")
dt <- readRDS('data/trial1_references.RDS')
df <- dt %>% 
  rename("container" = "container-title")

df <- df %>% 
  select(author, title, date, publisher, container, doi, url, File) 

beginning.df.nrow <- nrow(df)

scimago.j <- fread("~/Box/truckee/data/eia_data/journal_list.csv", fill = T)
scimago.c <- fread("~/Box/truckee/data/eia_data/conference_list.csv", fill = T)

# Matching -- cannot do any string manipulation on it, or else coerced to a character string

container_match_journal <- df$container %in% scimago.j$title
pub_match_journal <- df$publisher %in% scimago.j$title 
container_match_conf <- df$container %in% scimago.c$title
pub_match_conf <- df$publisher %in% scimago.c$title

# Truth is 27 in Hekkert and 14 in doc = 41 citations; 
## 4 govt report (2 European), 1 NGO report, 3 books and 
table(container_match_journal) # 7 of 33 known
table(pub_match_journal) # 0
table(container_match_conf) # 0
table(pub_match_conf) # 0

## Let's also pull in our official agency list
container_match_agency <- df$container %in% agencies$Agency 
pub_match_agency <- df$publisher %in% agencies$Agency 
author_match_agency <- df$author %in% agencies$Agency 
table(container_match_agency) # 0
table(pub_match_agency) # 0
table(author_match_agency) # 0

# Don't do patterns for now because this coerces it into a string

# 1. Identifying the lengths of lists in each cell ----
df$ID <- 1:nrow(df)

df <- df %>% 
  mutate(title = na_if(title, "NULL"),
         container = na_if(container, "NULL"),
         publisher = na_if(publisher, "NULL"),
         date = na_if(date, "NULL"),
         doi = na_if(doi, "NULL"),
         url = na_if(url, "NULL"))

## Identify the length of strings for each column ----

### Specify inputs

#### Title
x = select(df, title)
y = select(df, ID)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "title.lengths"
df <- left_join(df, out, by = "ID")

#### Container
x = select(df, container)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "container.lengths"
df <- left_join(df, out, by = "ID")

#### Publisher
x = select(df, publisher)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "publisher.lengths"
df <- left_join(df, out, by = "ID")

#### DATE
x = select(df, date)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "date.lengths"
df <- left_join(df, out, by = "ID")

#### URL
x = select(df, url)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "url.lengths"
df <- left_join(df, out, by = "ID")

#### DOI
x = select(df, doi)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "doi.lengths"
df <- left_join(df, out, by = "ID")

### Identify the longest list

MAX <- max(c(max(df$container.lengths), max(df$publisher.lengths), max(df$title.lengths), max(df$date.lengths), max(df$url.lengths), max(df$doi.lengths)))
# Sticking with this as the cutoff, in case dates or something get out of control
TITLE_MAX <- max(df$title.lengths)

df <- df %>% filter(date.lengths <= TITLE_MAX,
                    url.lengths <= TITLE_MAX,
                    container.lengths <= TITLE_MAX,
                    publisher.lengths <= TITLE_MAX,
                    doi.lengths <= TITLE_MAX)

### Look for congruent cases for mashed up citations ----

jl <- select(df, container.lengths)
pl <- select(df, publisher.lengths)
tl <- select(df, title.lengths)
yl <- select(df, date.lengths)
ul <- select(df, url.lengths)
dl <- select(df, doi.lengths)
z <- select(df, ID)

match.test <- pmap_dfr(list(jl, pl, tl, yl, ul, dl, z), matching_fx)
table(match.test$nested)

df <- left_join(df, match.test, by = "ID")

# DATE ----
if(max(df$date.lengths) > 1){
df <- separate(df, date, 
               into =  paste0("date",seq(1:max(df$date.lengths))), 
               sep = '\\"\\,\\s\\"')
df$date1 <- str_remove(df$date1, 'c\\(\\"')

date.cols <- grep("date\\d+", colnames(df))
# Need to make a df to be able to hard code date cols into subset
df <- data.frame(df)
last <- length(df[,date.cols])
MAX_DATE_OG <- colnames(df[,date.cols])[last]
df[,date.cols] <- lapply(df[,date.cols], rpl.sep)

## Isolate year only for all dates ----
### Have to pull out dates if they are embedded in some other weird string of numbers
df[,date.cols] <- lapply(df[,date.cols], extract_date_formats) 

### Pull out just year based on different date formats
df[,date.cols] <- lapply(df[,date.cols], assign_year)

## Additional date cleaning before nested consideration ----
### Remove dates that are unreasonable
df[,date.cols] <- lapply(df[,date.cols], rm_yrs)

# Make numeric
df[,date.cols] <- lapply(df[,date.cols], as.numeric)

## Removing duplicate and NA dates ----
# Needs hard coded for max number of columns
date.df <- df %>% 
  filter(!(is.na(date1) & is.na(date2))) %>% 
  select(ID, all_of(date.cols)) %>% 
  pivot_longer(cols = grep("date\\d+", colnames(.)), 
               names_to = "date.number",
               values_to = "date") %>% 
  select(-date.number)

#### Calling this out as output
out <- date.df %>% 
  unique() %>% 
  filter(!is.na(date))

#### Then we create new date number columns (since they are not different from before we made them unique and removed NAs) and then widen again
out <- out %>% 
  group_by(ID) %>% 
  mutate(date.number = paste0("date", row_number())) %>% 
  pivot_wider(names_from = date.number,
              values_from = date) %>% 
  ungroup()

date.cols <- grep("date\\d+", colnames(out))
last <- length(out[,date.cols])
MAX_DATE <- colnames(out[,date.cols])[last]

out$date.lengths = NA
for(i in 1:nrow(out)){
  for(j in date.cols){
    if(is.na(out$date.lengths[i]) & is.na(out[i,j])){
      out$date.lengths[i] <- j-date.cols[1]
    } else {
      next
    }
  }
}

out$date.lengths <- ifelse(is.na(out$date.lengths), length(date.cols), 
                           out$date.lengths)

## Collapsing columns into lists again ----

run.df <- out %>% filter(date.lengths > 1)

string.DT <- collapse_date2(run.df)
colnames(string.DT) <- c("year", "ID")

# Bind back the information from out, and the year information from run.df, and assign year values for those not in run.df
df <- df %>% 
  select(-c(date1:MAX_DATE_OG), -date.lengths) %>% 
  left_join(out, by = "ID") %>% 
  mutate(date.lengths = case_when(
    is.na(date.lengths) ~ 0,
    T ~ as.double(date.lengths))) %>% 
  left_join(string.DT, by = "ID") 

df$year <- ifelse(df$date.lengths == 0, NA,
                  ifelse(df$date.lengths == 1, df$date1, df$year))

df <- df %>% 
  select(-c(date1:MAX_DATE), -date.lengths)

x = select(df, year)
y = select(df, ID)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "date.lengths"
df <- left_join(df, out, by = "ID")
table(df$date.lengths)
}
# 3. Clean up URL formatting to better understand list length ----

## Separating to multiple urls
if(max(df$url.lengths) > 1){
df <- separate(df, url, 
               into = paste0("url",seq(1:max(df$url.lengths))), 
               sep = '\\"\\,\\s\\"')

df$url1 <- str_remove(df$url1, 'c\\(\\"')
url.cols <- grep("url\\d+", colnames(df))
last <- length(df[,url.cols])
MAX_URL_OG <- colnames(df[,url.cols])[last]
df[,url.cols] <- lapply(df[,url.cols], rpl.sep)

## Additional cleaning for non-urls ----

pattern.rm <- c("^[Aa]t:?\\s?", 
                "^[Aa]ccessed:?\\s?",
                "^[Ff]rom:?\\s?",
                "^Website:?\\s?")
pattern.rm <- paste(pattern.rm, collapse = "|")

rm.pattern <- function(x){
  ifelse(str_detect(x, pattern.rm), str_remove(x, pattern.rm), x)
}

df[,url.cols] <- lapply(df[,url.cols], rm.pattern)

pattern.rm <- c("^http:\\/\\/\\d{3}\\.")
pattern.rm <- paste(pattern.rm, collapse = "|")

pattern.keep <- c("^https?:\\/\\/.*",
                  "^ftp:\\/\\/\\.",
                  "doi\\.?:?")
pattern.keep <- paste(pattern.keep, collapse = "|")

rm.urls <- function(x){
  ifelse(str_detect(x, pattern.rm), NA,
         ifelse(str_detect(x, pattern.keep), x, NA))
}

df[,url.cols] <- lapply(df[,url.cols], rm.urls)

## Removing duplicates and NAs 
url.df <- df %>% 
  filter(!(is.na(url1) & is.na(url2) & is.na(url3))) %>% 
  select(ID, all_of(url.cols)) %>% 
  pivot_longer(cols = grep("url\\d+", colnames(.)), 
               names_to = "url.number",
               values_to = "url") %>% 
  select(-url.number)

out <- url.df %>% 
  unique() %>% 
  filter(!is.na(url))

out <- out %>% 
  group_by(ID) %>% 
  mutate(url.number = paste0("url", row_number())) %>% 
  pivot_wider(names_from = url.number,
              values_from = url) %>% 
  ungroup()

url.cols <- grep("url\\d+", colnames(out))
last <- length(out[,url.cols])
MAX_URL <- colnames(out[,url.cols])[last]

out$url.lengths = NA
for(i in 1:nrow(out)){
  for(j in url.cols){
    if(is.na(out$url.lengths[i]) & is.na(out[i,j])){
      out$url.lengths[i] <- j-url.cols[1]
    } else {
      next
    }
  }
}

out$url.lengths <- ifelse(is.na(out$url.lengths), length(url.cols), 
                          out$url.lengths)

## Collapsing columns into list again ----

run.df <- out %>% filter(url.lengths > 1)

string.DT <- collapse_url(run.df)
colnames(string.DT) <- c("url", "ID")

# Bind back the information from out, and the url information from run.df, and assign year values for those not in run.df
df <- df %>% 
  select(-c(url1:MAX_URL_OG), -url.lengths) %>% 
  left_join(out, by = "ID") %>% 
  mutate(url.lengths = case_when(
    is.na(url.lengths) ~ 0,
    T ~ as.double(url.lengths))) %>% 
  left_join(string.DT, by = "ID") 

df$url <- ifelse(df$url.lengths == 0, NA,
                 ifelse(df$url.lengths == 1, df$url1, df$url))

df <- df %>% 
  select(-c(url1:MAX_URL), -url.lengths)

# Re-count lengths, just in case. They should be the same as date.lengths

x = select(df, url)
y = select(df, ID)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "url.lengths"
df <- left_join(df, out, by = "ID")
table(df$url.lengths)
}
# 4. Clean up TITLE formatting to better understand list length ----
## Separating to multiple urls ----
if(max(df$title.lengths) > 1){
df <- separate(df, title, 
               into = paste0("title",seq(1:max(df$title.lengths))), 
               sep = '\\"\\,\\s\\"')
df$title1 <- str_remove(df$title1, 'c\\(\\"')
title.cols <- grep("title\\d+", colnames(df))
last <- length(df[,title.cols])
MAX_TITLE_OG <- colnames(df[,title.cols])[last]
df[,title.cols] <- lapply(df[,title.cols], rpl.sep)

## Additional cleaning for non-urls ----

agency.titles <- data.frame(lapply(df[,title.cols], extract_agency_titles))
if(sum(!is.na(agency.titles$title1.agency.in.title)) > 0){
  df[,title.cols] <- lapply(df[,title.cols], rm_agency_titles)
}

## Removing duplicates and NAs ----

title.df <- df %>% 
  filter(!(is.na(title1) & is.na(title2))) %>% 
  select(ID, all_of(title.cols)) %>% 
  pivot_longer(cols = grep("title\\d+", colnames(.)), 
               names_to = "title.number",
               values_to = "title") %>% 
  select(-title.number)

out <- title.df %>% 
  unique() %>% 
  filter(!is.na(title))

out <- out %>% 
  group_by(ID) %>% 
  mutate(title.number = paste0("title", row_number())) %>% 
  pivot_wider(names_from = title.number,
              values_from = title) %>% 
  ungroup()

title.cols <- grep("title\\d+", colnames(out))
last <- length(out[,title.cols])
MAX_TITLE <- colnames(out[,title.cols])[last]

out$title.lengths = NA
for(i in 1:nrow(out)){
  for(j in title.cols){
    if(is.na(out$title.lengths[i]) & is.na(out[i,j])){
      out$title.lengths[i] <- j-title.cols[1]
    } else {
      next
    }
  }
}

out$title.lengths <- ifelse(is.na(out$title.lengths), length(title.cols), 
                            out$title.lengths)

## Collapsing columns into list again ----

run.df <- out %>% filter(title.lengths > 1)

string.DT <- collapse_title(run.df)
colnames(string.DT) <- c("title", "ID")

# Bind them
df <- df %>% 
  select(-c(title1:MAX_TITLE_OG), -title.lengths) %>% 
  left_join(out, by = "ID") %>% 
  mutate(title.lengths = case_when(
    is.na(title.lengths) ~ 0,
    T ~ as.double(title.lengths))) %>% 
  left_join(string.DT, by = "ID") 

df$title <- ifelse(df$title.lengths == 0, NA,
                   ifelse(df$title.lengths == 1, df$title1, df$title))

df <- df %>% 
  select(-c(title1:MAX_TITLE), -title.lengths)

# Re-count lengths
x = select(df, title)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "title.lengths"
df <- left_join(df, out, by = "ID")
}

# 5. CONTAINER  ----
if(max(df$container.lengths) > 1){
df <- separate(df, container, into = 
                 paste0("container",seq(1:max(df$container.lengths))), sep = '\\"\\,\\s\\"')

df$container1 <- str_remove(df$container1, 'c\\(\\"')

container.cols <- grep("container\\d+", colnames(df))
last <- length(df[,container.cols])
MAX_CONTAINER_OG <- colnames(df[,container.cols])[last]

df[,container.cols] <- lapply(df[,container.cols], rpl.sep)

## Removing duplicates and NAs

container.df <- df %>% 
  filter(!(is.na(container1) & is.na(container2))) %>% 
  select(ID, all_of(container.cols)) %>% 
  pivot_longer(cols = grep("container\\d+", colnames(.)), 
               names_to = "container.number",
               values_to = "container") %>% 
  select(-container.number)

out <- container.df %>% 
  unique() %>% 
  filter(!is.na(container)) %>% 
  filter(container != "NA")

out <- out %>% 
  group_by(ID) %>% 
  mutate(container.number = paste0("container", row_number())) %>% 
  pivot_wider(names_from = container.number,
              values_from = container) %>% 
  ungroup()

container.cols <- grep("container\\d+", colnames(out))
last <- length(out[,container.cols])
MAX_CONTAINER <- colnames(out[,container.cols])[last]
MAX_CONTAINER

out$container.lengths = NA
for(i in 1:nrow(out)){
  for(j in container.cols){
    if(is.na(out$container.lengths[i]) & is.na(out[i,j])){
      out$container.lengths[i] <- j-container.cols[1]
    } else {
      next
    }
  }
}

out$container.lengths <- ifelse(is.na(out$container.lengths), length(container.cols), 
                                out$container.lengths)

## Collapsing columns into list again ----

run.df <- out %>% filter(container.lengths > 1)

string.DT <- collapse_container(run.df)
colnames(string.DT) <- c("container", "ID")

# Bind them
df <- df %>% 
  select(-c(container1:MAX_CONTAINER_OG), -container.lengths) %>% 
  left_join(out, by = "ID") %>% 
  mutate(container.lengths = case_when(
    is.na(container.lengths) ~ 0,
    T ~ as.double(container.lengths))) %>% 
  left_join(string.DT, by = "ID") 

df$container <- ifelse(df$container.lengths == 0, NA,
                       ifelse(df$container.lengths == 1, df$container1, df$container))

df <- df %>% 
  select(-c(container1:MAX_CONTAINER), -container.lengths)

# Re-count lengths

x = select(df, container)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "container.lengths"
df <- left_join(df, out, by = "ID")
}
# 6. PUBLISHER  ----
if(max(df$publisher.lengths) > 1){
df <- separate(df, publisher, 
               into = paste0("publisher",seq(1:max(df$publisher.lengths))), 
               sep = '\\"\\,\\s\\"')

df$publisher1 <- str_remove(df$publisher1, 'c\\(\\"')

# DT is very finnicky with subsetting this way, and I cannot get it to work in an apply function
publisher.cols <- grep("publisher\\d+", colnames(df))
last <- length(df[,publisher.cols])
MAX_PUBLISHER_OLD <- colnames(df[,publisher.cols])[last]

df[,publisher.cols] <- lapply(df[,publisher.cols], rpl.sep)

## Removing duplicates and NAs ----

publisher.df <- df %>% 
  filter(!(is.na(publisher1) & is.na(publisher2))) %>% 
  select(ID, all_of(publisher.cols)) %>% 
  pivot_longer(cols = grep("publisher\\d+", colnames(.)), 
               names_to = "publisher.number",
               values_to = "publisher") %>% 
  select(-publisher.number)

out <- publisher.df %>% 
  unique() %>% 
  filter(!is.na(publisher)) %>% 
  filter(publisher != "NA")

out <- out %>% 
  group_by(ID) %>% 
  mutate(publisher.number = paste0("publisher", row_number())) %>% 
  pivot_wider(names_from = publisher.number,
              values_from = publisher) %>% 
  ungroup()

publisher.cols <- grep("publisher\\d+", colnames(out))
last <- length(out[,publisher.cols])
MAX_PUBLISHER <- colnames(out[,publisher.cols])[last]
MAX_PUBLISHER

out$publisher.lengths = NA
for(i in 1:nrow(out)){
  for(j in publisher.cols){
    if(is.na(out$publisher.lengths[i]) & is.na(out[i,j])){
      out$publisher.lengths[i] <- j-publisher.cols[1]
    } else {
      next
    }
  }
}

out$publisher.lengths <- ifelse(is.na(out$publisher.lengths), length(publisher.cols), 
                                out$publisher.lengths)

## Collapsing columns into list again ----

run.df <- out %>% filter(publisher.lengths > 1)

string.DT <- collapse_publisher(run.df)
colnames(string.DT) <- c("publisher", "ID")

# Bind them
df <- df %>% 
  select(-c(publisher1:MAX_PUBLISHER_OLD), -publisher.lengths) %>% 
  left_join(out, by = "ID") %>% 
  mutate(publisher.lengths = case_when(
    is.na(publisher.lengths) ~ 0,
    T ~ as.double(publisher.lengths))) %>% 
  left_join(string.DT, by = "ID") 

df$publisher <- ifelse(df$publisher.lengths == 0, NA,
                       ifelse(df$publisher.lengths == 1, df$publisher1, df$publisher))

df <- df %>% 
  select(-c(publisher1:MAX_PUBLISHER), -publisher.lengths)

# Re-count lengths

x = select(df, publisher)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "publisher.lengths"
df <- left_join(df, out, by = "ID")
}

# DOI----
if(max(df$doi.lengths) > 1){
## Separating to multiple dois
df <- separate(df, doi, 
               into = paste0("doi",seq(1:max(df$doi.lengths))), 
               sep = '\\"\\,\\s\\"')
df$doi1 <- str_remove(df$doi1, 'c\\(\\"')

doi.cols <- grep("doi\\d+", colnames(df))
last <- length(df[,doi.cols])
MAX_DOI_OG <- colnames(df[,doi.cols])[last]
df[,doi.cols] <- lapply(df[,doi.cols], rpl.sep)

# Identify the doi pattern, maybe starting with 10.

pattern.doi <- c("10\\..*")

extract.doi <- function(x){
  ifelse(str_detect(x, pattern.doi), str_extract(x, pattern.doi), NA)
}

df[,doi.cols] <- lapply(df[,doi.cols], extract.doi)

## Removing duplicates and NAs
doi.df <- df %>% 
  filter(!(is.na(doi1))) %>% 
  select(ID, all_of(doi.cols)) %>% 
  pivot_longer(cols = grep("doi\\d+", colnames(.)), 
               names_to = "doi.number",
               values_to = "doi") %>% 
  select(-doi.number)

out <- doi.df %>% 
  unique() %>% 
  filter(!is.na(doi))

out <- out %>% 
  group_by(ID) %>% 
  mutate(doi.number = paste0("doi", row_number())) %>% 
  pivot_wider(names_from = doi.number,
              values_from = doi) %>% 
  ungroup()

doi.cols <- grep("doi\\d+", colnames(out))
last <- length(out[,doi.cols])
MAX_DOI <- colnames(out[,doi.cols])[last]

out$doi.lengths <- NA
for(i in 1:nrow(out)){
  for(j in doi.cols){
    if(is.na(out$doi.lengths[i]) & is.na(out[i,j])){
      out$doi.lengths[i] <- j-doi.cols[1]
    } else {
      next
    }
  }
}

out$doi.lengths <- ifelse(is.na(out$doi.lengths), 
                          length(doi.cols), 
                          out$doi.lengths)

## Collapsing columns into list again ----

run.df <- out %>% filter(doi.lengths > 1)

string.DT <- collapse_doi(run.df)
colnames(string.DT) <- c("doi", "ID")


# Bind back the information from out, and the doi information from run.df, and assign year values for those not in run.df
df <- df %>% 
  select(-c(doi1:MAX_DOI_OG), -doi.lengths) %>% 
  left_join(out, by = "ID") %>% 
  mutate(doi.lengths = case_when(
    is.na(doi.lengths) ~ 0,
    T ~ as.double(doi.lengths))) %>% 
  left_join(string.DT, by = "ID") 

df$doi <- ifelse(df$doi.lengths == 0, NA,
                 ifelse(df$doi.lengths == 1, df$doi1, df$doi))

df <- df %>% 
  select(-c(doi1:MAX_doi), -doi.lengths)

# Re-count lengths, just in case. They should be the same as date.lengths

x = select(df, doi)
y = select(df, ID)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "doi.lengths"
df <- left_join(df, out, by = "ID")
table(df$doi.lengths)
}


# AUTHOR ----
x = df$author
y = df$ID
out <- pmap_dfr(list(x, y), separate_author)

out$V1 <- base::trimws(out$V1)
out$V1 <- str_remove_all(out$V1, rm.auth.word)
out$V1 <- base::trimws(out$V1)
out$V1 <- ifelse(str_detect(out$V1, rm.auth.cell), NA_character_, out$V1)
out$V1 <- base::trimws(out$V1)
out$V1 <- str_remove_all(out$V1, rm.auth.word)
out$V1 <- base::trimws(out$V1)
out$V1 <- ifelse(str_detect(out$V1, rm.auth.cell), NA_character_, out$V1)
out$V1 <- base::trimws(out$V1)

# Get rid of all non-word characters except: spaces, commas, &s
x <- "a 1~!@#$%^&*(){}_+:\"<>?,./;'[]-="
stringr::str_remove_all(x, "[\\p{P}\\p{S}&&[^., ]]")
out$V1 <- str_remove_all(out$V1, '[\\p{P}\\p{S}&&[^,& ]]')
out$V1 <- base::trimws(out$V1)
out$V1 <- ifelse(out$V1 == "", NA_character_, out$V1)
# Then run these again...
out$V1 <- str_remove_all(out$V1, rm.auth.word)
out$V1 <- base::trimws(out$V1)
out$V1 <- ifelse(str_detect(out$V1, rm.auth.cell), NA_character_, out$V1)
out$V1 <- base::trimws(out$V1)
out$V1 <- ifelse(out$V1 == "", NA_character_, out$V1)
# Want to get get of repeats that happened where it is like NRC & NRC & NRC & NRC

### Longer than 75 or shorter than 3 characters
out$V1 <- ifelse(nchar(out$V1) > 75 | nchar(out$V1) < 3,
                 NA,  out$V1)
out <- out %>% filter(!is.na(V1))

colnames(out) <- c("author", "ID")

# Make df wider by nesting authors into one cell, divided by ;

out <- out %>% 
  group_by(ID) %>% 
  mutate(author.number = paste0("author", row_number())) %>% 
  pivot_wider(names_from = author.number,
              values_from = author) %>% 
  ungroup()

author.cols <- grep("author\\d+", colnames(out))
last <- length(out[,author.cols])
MAX_AUTHOR <- colnames(out[,author.cols])[last]
MAX_AUTHOR

out$author.lengths <- NA
for(i in 1:nrow(out)){
  for(j in author.cols){
    if(is.na(out$author.lengths[i]) & is.na(out[i,j])){
      out$author.lengths[i] <- j-author.cols[1]
    } else {
      next
    }
  }
}

out$author.lengths <- ifelse(is.na(out$author.lengths), 
                             length(author.cols), 
                             out$author.lengths)

# Don't filter here because the normal author column is not an acceptale formate
run.df <- out 

# This is with just run.df and a minimized function

string.DT <- collapse_author(run.df)
colnames(string.DT) <- c("author", "ID")

df <- df %>% 
  select(-author) %>% 
  left_join(out, by = "ID") %>% 
  mutate(author.lengths = case_when(
    is.na(author.lengths) ~ 0,
    T ~ as.double(author.lengths))) %>% 
  left_join(string.DT, by = "ID") 

df$author <- ifelse(df$author.lengths == 0, NA,
                    ifelse(df$author.lengths == 1, df$author1, df$author))

df <- df %>% 
  select(-c(author1:MAX_AUTHOR), -author.lengths)
# Re-count lengths, just in case. They should be the same as author.lengths

x = select(df, author)
y = select(df, ID)
out <- pmap_dfr(list(x, y), index.lengths)
colnames(out)[1] <- "author.lengths"
df <- left_join(df, out, by = "ID")
table(df$author.lengths)

# 4. New nesting variable based on updated date, url, and title lengths ----

## Looking at old breakdown
old.nested <- table(match.test$nested); old.nested
al <- select(df, author.lengths)
jl <- select(df, container.lengths)
pl <- select(df, publisher.lengths)
tl <- select(df, title.lengths)
yl <- select(df, date.lengths)
ul <- select(df, url.lengths)
dl <- select(df, doi.lengths)
z <- select(df, ID)
match.test <- pmap_dfr(list(jl, pl, tl, yl, ul, dl, z), matching_fx)
new.nested <- table(match.test$nested); new.nested
### More evens
match.test.al <- pmap_dfr(list(al, jl, pl, tl, yl, ul, dl, z), matching_fx_auth)
table(match.test.al$nested)

df <- df %>% 
  select(-nested) %>% 
  left_join(match.test, by = "ID")

df <- select(df, ID, author, year, title, container, publisher, doi, url, File, author.lengths, date.lengths, title.lengths, container.lengths, publisher.lengths, doi.lengths, url.lengths, nested) 


run.df <- df %>% filter(nested == "tu_even_unn" | nested == "ty_even_unn")
paste.df <- anti_join(df, run.df)

# Go ahead and collapse authors before unnesting 
if(nrow(run.df) > 0){
run.df[,2] <- apply(run.df[,2], 1, function(x) paste(unlist(x), collapse=', '))

run.un <- data.table()
for(i in 1:nrow(run.df)){
  if (run.df$nested[i] == "tu_even_unn") {
    un <- unnest(run.df[i,], cols = c(title, url)) 
  } else {
    un <- unnest(run.df[i,], cols = c(title, year)) 
  }  
  run.un <- rbind(un, run.un)
}

run.un <- select(run.un, ID, author, year, title, container, publisher, doi, url, File, nested)

# back to DT for thsi subsetting
paste.df <- data.table(paste.df)
id = paste.df[,ID]
auth = paste.df[,author]
yr = paste.df[,year][[1]][1]
ti = paste.df[,title]
c = paste.df[,container]
p = paste.df[,publisher]
doi = paste.df[,doi]
url = paste.df[,url]
File = paste.df[,File]
nested = paste.df[,nested]

paste.un <- pmap_dfr(list(id, auth, yr, ti, c, p, doi, url, File, nested), collpse)

df <- rbind(run.un, paste.un) %>% select(-nested)
}
df$ID <- 1:nrow(df)

df$title <- str_squish(df$title)
df$title <- iconv(df$title, from = "latin1", to = "ASCII", sub = "") 
df$container <- str_squish(df$container)
df$container <- iconv(df$container, from = "latin1", to = "ASCII", sub = "") 
df$publisher <- str_squish(df$publisher)
df$publisher <- iconv(df$publisher, from = "latin1", to = "ASCII", sub = "") 
df$author <- str_squish(df$author)
df$author <- iconv(df$author, from = "latin1", to = "ASCII", sub = "") 

df <- df %>% 
  mutate(container = trimws(toTitleCase(str_remove_all(container, 
                                                       '\\.|,|;|\\*|-|"|\\(|\\)|\\+|\\-|\\/|\\\\|:|\\[|\\]')))) %>% 
  mutate(container = str_remove_all(container, "'")) %>% 
  mutate(container = str_replace_all(container, "\\&", "and")) %>% 
  mutate(container = str_squish(container)) %>% 
  mutate(publisher = trimws(toTitleCase(str_remove_all(publisher, 
                                                       '\\.|,|;|\\*|-|"|\\(|\\)|\\+|\\-|\\/|\\\\|:|\\[|\\]')))) %>% 
  mutate(publisher = str_remove_all(publisher, "'")) %>% 
  mutate(publisher = str_replace_all(publisher, "\\&", "and")) %>% 
  mutate(publisher = str_squish(publisher)) %>% 
  mutate(title = trimws(toTitleCase(str_remove_all(title, 
                                                   '\\.|,|;|\\*|-|"|\\(|\\)|\\+|\\-|\\/|\\\\|:|\\[|\\]')))) %>% 
  mutate(title = str_remove_all(title, "'")) %>% 
  mutate(title = str_replace_all(title, "\\&", "and")) %>% 
  mutate(title = str_squish(title))

df$author <- str_replace_all(df$author, "US(?=[A-Z][a-z]+)", "US ")
df$author <- str_replace_all(df$author, "USACE(?=[A-Z][a-z]+)", "USACE ")
df$author <- str_replace_all(df$author, "USDA(?=[A-Z][a-z]+)", "USDA ")
df$author <- str_replace_all(df$author, "USEPA(?=[A-Z][a-z]+)", "USEPA ")
df$author <- str_replace_all(df$author, "USDI(?=[A-Z][a-z]+)", "USDI ")


container_match_journal <- df$container %in% scimago.j$title
pub_match_journal <- df$publisher %in% scimago.j$title 
container_match_conf <- df$container %in% scimago.c$title
pub_match_conf <- df$publisher %in% scimago.c$title

table(container_match_journal) # still 7
table(pub_match_journal) # still 0
table(container_match_conf) # still 0
table(pub_match_conf) # still 0

container_match_agency <- df$container %in% agencies$Agency 
pub_match_agency <- df$publisher %in% agencies$Agency 
author_match_agency <- df$author %in% agencies$Agency 
table(container_match_agency) # still 0
table(pub_match_agency) # still 0
table(author_match_agency) # still 0

agency.pattern <- paste(agencies$Agency, collapse = "\\b|\\b")
container_match_agency_p <- str_detect(df$container, agency.pattern)
author_match_agency_p <- str_detect(df$author, agency.pattern)
pub_match_agency_p <- str_detect(df$publisher, agency.pattern)

table(container_match_agency_p) # 0
table(author_match_agency_p) # 0
table(pub_match_agency_p) # 1

fwrite(df, "data/trial1_references.csv")
