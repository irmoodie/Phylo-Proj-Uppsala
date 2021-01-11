# Moss Phylogeny Project Script - GBIF search
# IR Moodie 2021
# Adapted using code segments from GBIF helpfiles

# ---- SETUP ENV ----

rm(list=ls()) # clear environment

library(rgbif) # for pulling data from GBIF
library(taxize) # useful for dealing with taxa names
library(magrittr) # for Tee operator
library(tidyverse) 

# ---- GBIF DOWNLOAD ----

user <- "XXXXXXXXXXXXX" # put gbif username here
pwd <- "XXXXXXXXXX" # gbif password
email <- "XXXXXXXXXXXXXXXXX" # gbif email (for email of download)

taxon_keys <- read_csv("species.csv") %>% # read in file containing list of taxa, with names formated as Homo_sapiens in col labeled "species"
  mutate(species = str_replace(species, "_", " ")) %>% # change "Homo_sapiens" to "Homo sapiens"
  pull("species") %>% # pull out species names
  get_gbifid_(method="backbone") %>% # get the taxon IDs
  imap(~ .x %>% mutate(original_sciname = .y)) %>%
  bind_rows() %T>%
  write_tsv(path = "all_matches.tsv") %>% # output the full file for records
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% #ensure only exact and accepted matches are taken to avoid duplicates
  filter(kingdom == "Plantae") %>% # just in case
  pull(usagekey) # makes a list of the usage keys for submitting request

occ_download( # for downloading from GBIF API
  pred_in("taxonKey", taxon_keys), # use taxon keys to id species of interest
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email # user info as entered above
)

# Request is then found in the downloads section of GBIF account