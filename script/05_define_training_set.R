# To train a model
library(stringr)
library(data.table)
library(tools)
library(tidyverse)
setwd("~/Box/citation_classifier/")
source("functions.R")
scimago.j <- fread("~/Box/truckee/data/eia_data/journal_list.csv", fill = T)
scimago.c <- fread("~/Box/truckee/data/eia_data/conference_list.csv", fill = T)
agencies <- fread("~/Box/truckee/data/eia_data/agency_list.csv", fill = T)
df <- fread("data/gsp_references_clean.csv")
df <- df %>% mutate_all(na_if,"")

# Let's try to make sense of some things -- get rid of cells that won't make any sense
## Removing some of the issues named above
df$container <- trimws(str_remove_all(df$container, rm.word))
df$container <- ifelse(nchar(df$container) < 3, NA_character_, df$container)
df$journal.disam <- trimws(str_remove_all(df$journal.disam, rm.word))
df$journal.disam <- ifelse(nchar(df$journal.disam) < 3, NA_character_, df$journal.disam)
df$title <- trimws(str_remove_all(df$title, rm.word))
df$title <- ifelse(nchar(df$title)  < 3, NA_character_, df$title)
df$publisher <- trimws(str_remove_all(df$publisher, rm.word))
df$publisher <- ifelse(nchar(df$publisher) < 3, NA_character_, df$publisher)
df$author <- trimws(str_remove_all(df$author, rm.word))
df$author <- ifelse(nchar(df$author) < 3, NA_character_, df$author)
df$year <- ifelse(df$year < 1850 | df$year > 2020, NA, df$year)

## If there is a year in the journal, move it to year and get rid of it in journal
df$year <- ifelse(str_detect(df$container, "^[0-9]{4}$") & 
                    is.na(df$year), df$container, df$year)
df$container <- str_remove(df$container, "^[0-9]{4}$")
df$year <- ifelse(str_detect(df$journal.disam, "^[0-9]{4}$") & 
                    is.na(df$year), df$journal.disam, df$year)
df$journal.disam <- str_remove(df$journal.disam, "^[0-9]{4}$")
## Remove the just numbers from publisher
df$publisher <- str_remove(df$publisher, "^[0-9]+$")
#If there is a 4digit thing in author, maybe followed by a-c, remove it and maybe put it in date
df$year <- ifelse(str_detect(df$author, "[0-9]{4}[a-c]?") & 
                    is.na(df$year), str_extract(df$author, "[0-9]{4}[a-c]"), df$year)
df$author <- str_remove_all(df$author, "[0-9]{4}[a-g]?")
df$year <- str_remove_all(df$year, "(?<=[0-9]{4})[a-k]") 

# Make sure that was legit
df$year <- ifelse(df$year < 1850 | df$year > 2020, NA, df$year)

# Run this again
df$container <- str_squish(trimws(str_remove_all(df$container, rm.word)))
df$journal.disam <- str_squish(trimws(str_remove_all(df$journal.disam, rm.word)))
df$title <- str_squish(trimws(str_remove_all(df$title, rm.word)))
df$publisher <- str_squish(trimws(str_remove_all(df$publisher, rm.word)))
df$author <- str_squish(trimws(str_remove_all(df$author, rm.word)))

df$container_match_journal <- df$journal.disam %in% scimago.j$title 
df$pub_match_journal <- df$publisher %in% scimago.j$title 
df$container_match_agency <- df$journal.disam %in% agencies$Agency 
df$author_match_agency <- df$author %in% agencies$Agency 
df$pub_match_agency <- df$publisher %in% agencies$Agency 
df$container_match_conf <- df$journal.disam %in% scimago.c$title 
df$pub_match_conf <- df$publisher %in% scimago.c$title 

# This is way higher so I am going to go with this and then I will match later
agency.pattern <- paste(agencies$Agency, collapse = "\\b|\\b")
df$container_match_agency_p <- str_detect(df$container, agency.pattern)
df$author_match_agency_p <- str_detect(df$author, agency.pattern)
df$pub_match_agency_p <- str_detect(df$publisher, agency.pattern)


#jpat <- scimago.j %>% 
#  mutate(nwords =  str_count(title, " ")+1) %>% 
#  filter(nwords > 2)
#journal.pattern <- paste(jpat$title, collapse = "\\b|\\b")
#df$container_match_journal_p <- str_detect(df$container, journal.pattern)
#df$pub_match_journal_p <- str_detect(df$container, journal.pattern)


# This is for multi-feature, removing the title column
df_title <- df %>% 
  mutate_all(na_if,"") %>% 
  mutate(training.column = case_when(
    container_match_journal == T ~ container,
    pub_match_journal == T ~ publisher,
    author_match_agency == T ~ author, # choosing this because in cases where both match, Federal Reg is more likely to be the journal and the more specific name in the author line
    container_match_agency == T ~ container,
    pub_match_agency == T ~ publisher,
    container_match_conf == T ~ container,
    pub_match_conf == T ~ publisher,
    !is.na(container) ~ container,
    !is.na(publisher) ~ publisher,
    !is.na(author) ~ author,
    T ~ NA_character_
  )) %>% 
  filter(!is.na(training.column)) # over 100K rows without info

# Assign classifiers to the multi-feature
df_title <- df_title %>% 
  mutate(class = case_when(
    str_detect(training.column, rm.row) ~ "remove_row",
    str_detect(training.column, rm.row2) ~ "remove_row", # This is a less refined list
    nchar(training.column) > 250 | nchar(training.column) < 3 ~ "remove_row",
    str_detect(training.column, rm.word) ~ "remove_cell",
    container_match_journal == T | pub_match_journal == T ~ "journal",
    author_match_agency_p == T | container_match_agency_p == T | pub_match_agency_p == T ~ "agency",
    container_match_conf == T | pub_match_conf == T ~ "conference",
    str_detect(training.column, conference) ~ "conference",
    T ~ NA_character_
  ))
table(df_title$class)

df <- df_title %>% mutate_all(na_if,"") %>% 
  mutate(title = case_when(
    is.na(title) ~ "Empty",
    T ~ title
  ))
df <- df %>% mutate_all(na_if,"")
delete <- df %>% select(ID, title, training.column, class) %>% 
  filter(class == "remove_row") 
journal <- df %>% select(ID, title, training.column, class) %>% 
  filter(class == "journal", !is.na(training.column)) 
agency <- df %>% select(ID, title, training.column, class) %>% 
  filter(class == "agency", !is.na(training.column)) 
conference <- df %>% select(ID, title, training.column, class) %>% 
  filter(class == "conference", !is.na(training.column)) 

# Make the training set with 80% of the known observations
train.delete <- sample_n(delete, nrow(delete)*.8)
train.journal <- sample_n(journal, nrow(journal)*.8)
train.agency <- sample_n(agency, nrow(agency)*.8)
train.conference <- sample_n(conference, nrow(conference)*.8)

# Make the testing set with 20% of the known observations
test.delete <- anti_join(delete, train.delete, by = "ID")
test.journal <- anti_join(journal, train.journal, by = "ID")
test.agency <- anti_join(agency, train.agency, by = "ID")
test.conference <- anti_join(conference, train.conference, by = "ID")

# Make them the same size (waiting to do this after splitting, because otherwise the antijoin doesn't work)
lengths <- c(nrow(train.journal), nrow(train.agency), nrow(train.delete), nrow(train.conference))
LENGTH <- max(lengths)

train.agency <- rbind(train.agency, sample_n(train.agency, (LENGTH - nrow(train.agency))))
train.delete <- rbind(train.delete, sample_n(train.delete, (LENGTH - nrow(train.delete)), replace = T))
train.conference <- rbind(train.conference, sample_n(train.conference, (LENGTH - nrow(train.conference)), replace = T))
train.journal <- rbind(train.journal, sample_n(train.journal, (LENGTH - nrow(train.journal)), replace = T))
train <- rbind(train.delete, train.journal, train.agency, train.conference)

# For multi-class, need to make the output into a matrix (same for multi-feature)
train <- train %>% 
  mutate(ID.pivot = 1:nrow(train)) %>% 
  pivot_wider(names_from = class, values_from = class) %>% 
  select(-ID.pivot) 
train[,c(4:7)] <- map(train[,c(4:7)], function(x) ifelse(!is.na(x), 1, 0))


lengths <- c(nrow(test.journal), nrow(test.agency), nrow(test.delete), nrow(test.conference))
LENGTH <- max(lengths)

test.agency <- rbind(test.agency, sample_n(test.agency, (LENGTH - nrow(test.agency))))
test.delete <- rbind(test.delete, sample_n(test.delete, (LENGTH - nrow(test.delete)), replace = T))
test.conference <- rbind(test.conference, sample_n(test.conference, (LENGTH - nrow(test.conference)), replace = T))
test.journal <- rbind(test.journal, sample_n(test.journal, (LENGTH - nrow(test.journal)), replace = T))
test <- rbind(test.delete, test.journal, test.agency, test.conference)

test <- test %>% 
  mutate(ID.pivot = 1:nrow(test)) %>% 
  pivot_wider(names_from = class, values_from = class) %>% 
  select(-ID.pivot) 
test[,c(4:7)] <- map(test[,c(4:7)], function(x) ifelse(!is.na(x), 1, 0))

fwrite(test, "data/gsp_test.csv")
fwrite(train, "data/gsp_train.csv")
fwrite(df, "data/gsp_references_keras.csv")