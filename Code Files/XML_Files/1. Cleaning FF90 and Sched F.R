## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file creates a dirty data set merging an indicator
#   of whether or not a nonprofit is anti- or non anti-LGBTQ+
#   with corresponding data from form 990 parts 0, 9, and the
#   return header.

## Last updated: March 9th, 2023 by Sebastian Rojas Cabal
#   Sample of anti nonprofits is based on list of known nonprofits as well as organizations to which those
#   known nonprofits have donated money to.

#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(xml2)
#--------------------------------------------------------
#--------------------------------------------------------
# Importing data and cleaning data
#--------------------------------------------------------
options(scipen=999) # Telling R to avoid the scientific notation altogether
# -------------------------
#   List of anti-LGBTQ+ orgs
# -------------------------
# List of known anti-LGBTQ+ nonprofits. 
orgs1 <- read_excel("/Volumes/SRC_DATA/000_f990_data/anti_lgbtq_eins_20220422.xlsx") %>%
  rename(name = Organization,
         ein_char = ein) %>%
  mutate(ein_char = str_trim(ein_char),
         ein = as.numeric(ein_char)) %>%
  select(name, ein) %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE)
# List of first-order ties to known anti-LGBTQ+ nonprofits.
orgs2 <- read_csv("/Volumes/SRC_DATA/000_f990_data/anti_lgbtq_candidates_20220516.csv") %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE)
# Binding all candidate anti-LGBTQ+ orgs
orgs_all <- bind_rows(orgs1, orgs2) %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(anti_lgbtq = 1)
# -------------------------
# -------------------------
#   2015: F990 and Schedule F
# -------------------------
# 2015 records are in the 2016 file
f990_2015 <- read_csv("/Volumes/SRC_DATA/000_f990_data/xml/xml990_2016.csv") %>%
  select(returnts,
         filer.ein,
         taxyr,
         taxperiodenddt,
         returntypecd,
         schedulef.totalspentamt
         ) %>%
  filter(
        returntypecd == "990",
        taxyr == 2015
        ) %>%
  distinct() %>%
  group_by(filer.ein, taxyr) %>%
  slice_max(returnts) %>%
  slice_min(taxperiodenddt) %>%
  ungroup() %>%
  arrange(filer.ein) # clean! no duplicated EINs :)
# -------------------------
#     Process to find duplicates
# -------------------------
f990_2015_dups <- read_csv("/Volumes/SRC_DATA/000_f990_data/xml/xml990_2016.csv") %>%
  select(returnts,
         filer.ein,
         taxyr,
         taxperiodenddt,
         returntypecd,
         schedulef.totalspentamt
  ) %>%
  filter(
    returntypecd == "990",
    taxyr == 2015
  )

f990_2015_dups2 <- f990_2015_dups %>%
  group_by(filer.ein, taxyr) %>%
  filter(n()>1) %>%
  ungroup() %>%
  arrange(filer.ein)

f990_2015_dups2.1 <- f990_2015_dups %>%
  group_by(filer.ein, taxyr) %>%
  filter(n()>1) %>%
  ungroup() %>%
  arrange(filer.ein) %>%
  group_by(filer.ein) %>%
  slice_max(returnts)

f990_2015_dups3 <- f990_2015_dups2.1 %>%
  group_by(filer.ein, taxyr) %>%
  filter(n()>1) %>%
  ungroup() %>%
  arrange(filer.ein) #%>%
  group_by(filer.ein) %>%
  slice_max(taxperiodenddt)
  
f990_2015_dups3.1 <- f990_2015_dups2.1 %>%
    group_by(filer.ein, taxyr) %>%
    filter(n()>1) %>%
    ungroup() %>%
    arrange(filer.ein) %>%
  group_by(filer.ein) %>%
    slice_min(taxperiodenddt)
# -------------------------
# -------------------------  
# Sched F (from XML file)
# -------------------------
f_2015 <- read_csv("/Volumes/SRC_DATA/000_f990_data/xml_schedF/xml990SchedF_2016.csv") %>%
  select(returnts,
         filer.ein,
         taxyr,
         taxperiodenddt,
         returntypecd,
         schedulef.totalspentamt) %>%
  filter(
    returntypecd == "990",
    taxyr == 2015
  ) %>%
  distinct() %>%
  group_by(filer.ein, taxyr) %>%
  slice_max(returnts) %>%
  slice_min(taxperiodenddt) %>%
  ungroup() %>%
  arrange(filer.ein) # clean! no duplicated EINs :)
# -------------------------
#     Process to find duplicates
# -------------------------
f_2015_dups <- read_csv("/Volumes/SRC_DATA/000_f990_data/xml_schedF/xml990SchedF_2016.csv") %>%
  select(returnts,
         filer.ein,
         taxyr,
         taxperiodenddt,
         returntypecd,
         schedulef.totalspentamt
  ) %>%
  filter(
    returntypecd == "990",
    taxyr == 2015
  )

f_2015_dups2 <- f_2015_dups %>%
  group_by(filer.ein, taxyr) %>%
  filter(n()>1) %>%
  ungroup() %>%
  arrange(filer.ein)

f_2015_dups2.1 <- f_2015_dups2 %>%
  group_by(filer.ein, taxyr) %>%
  filter(n()>1) %>%
  ungroup() %>%
  arrange(filer.ein) %>%
  group_by(filer.ein) %>%
  slice_max(returnts)

f_2015_dups3 <- f_2015_dups2.1 %>%
  group_by(filer.ein, taxyr) %>%
  filter(n()>1) %>%
  ungroup() %>%
  arrange(filer.ein) #%>%
group_by(filer.ein) %>%
  slice_max(taxperiodenddt)

f_2015_dups3.1 <- f_2015_dups3 %>%
  group_by(filer.ein, taxyr) %>%
  filter(n()>1) %>%
  ungroup() %>%
  arrange(filer.ein) %>%
  group_by(filer.ein) %>%
  slice_min(taxperiodenddt)

f_2015_dups4 <- f_2015 %>%
  group_by(filer.ein, taxyr) %>%
  filter(n()>1) %>%
  ungroup() %>%
  arrange(filer.ein) %>%
  group_by(filer.ein) %>%
  slice_min(taxperiodenddt)
# -------------------------
# -------------------------  
# Sched F (from Jacob)
options(scipen=999) # Telling R to avoid the scientific notation altogether
# -------------------------
dirty_f_1 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_i.csv") %>%
  select(-id) %>%
  distinct() %>%
  mutate(
    id_ein = paste0(object_id, ein)
  )
#length(unique(dirty_f_1$id))/nrow(dirty_f_1)
length(unique(dirty_f_1$object_id))/nrow(dirty_f_1)
length(unique(dirty_f_1$id_ein))/nrow(dirty_f_1)

dirty_f_1_dups <- dirty_f_1 %>%
  group_by(object_id, ein) %>%
  filter(n()>1) %>%
  ungroup() %>%
  arrange(ein)

#dirty_f_2 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_ii.csv")

#dirty_f_4 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_iv.csv")

dirty_f_activities <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_activities.csv") %>%
  select(-id) %>%
  select(object_id, ein) %>%
  mutate(
    id_ein = paste0(object_id, ein)
  ) %>%
  distinct(object_id, ein, id_ein)
length(unique(dirty_f_activities$id_ein))/nrow(dirty_f_activities)



#length(unique(dirty_f_activities$id))/nrow(dirty_f_activities)
length(unique(dirty_f_activities$object_id))/nrow(dirty_f_activities)

dirty_f_individualGrants2 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_individ_grants.csv") %>%
  select(-id) %>%
  select(object_id, ein) %>%
  mutate(
    id_ein = paste0(object_id, ein)
  ) %>%
  distinct(object_id, ein, id_ein)
length(unique(dirty_f_activities$id_ein))/nrow(dirty_f_activities)
#length(unique(dirty_f_individualGrants$id))/nrow(dirty_f_individualGrants)
length(unique(dirty_f_individualGrants$id_ein))/nrow(dirty_f_individualGrants)

# Merge across Sched F test

f1_id <- dirty_f_1 %>% select(
  id, object_id, ein
)

f_join_1_activites <- anti_join(dirty_f_1, dirty_f_activities, by = ("id_ein"))
# 7,285 rows in part 1 don't have a match in the activities
f_join_activites_1 <- anti_join(dirty_f_activities, dirty_f_1, by = ("id_ein"))
# 492 rows in activities don't have a match in part 1
# There's a diff of 164,635 rows between them.

random_f1_act <- slice_sample(f_join_activites_1, n = 10)

f_grants_join <- anti_join(dirty_f_1, dirty_f_individualGrants2, by = ("id_ein"))
# 866 rows in part 1 don't have a match in the activities
grants_f_join <- anti_join(dirty_f_individualGrants2, dirty_f_1, by = ("id_ein"))
# 165,484 rows in activities don't have a match in part 1
# There's a diff of 164,635 rows between them.


factivities <- dirty_f_activities %>% select(
  id, object_id, ein
)

fgrants <- dirty_f_individualGrants %>% select(
  id, object_id, ein
)

# f2_id <- dirty_f_2 %>% select(
#   id, object_id, ein
# )
# 
# f4_id <- dirty_f_4 %>% select(
#   id, object_id, ein
# )


f_jacob_dups <- dirty_f_1 %>%
  group_by(ein, TtlSpntAmt) %>% # seeing if the same EIN spent the same in multiple years
  filter(n()>1) %>%
  ungroup() %>%
  arrange(ein)

f_jacob_dups2 <- f_jacob_dups %>%
  group_by(ein, TtlSpntAmt) %>%
  filter(n()>1) %>%
  ungroup() %>%
  arrange(filer.ein) %>%
  group_by(filer.ein) %>%
  slice_max(returnts)









# these are the forms submissions with metadata from Amazon Web Services/IRS.
#   length(unique(rtrn$id))/nrow(rtrn) # all ids are unique
#   length(unique(rtrn990$object_id))/nrow(rtrn990) # object_ids are not unique. Parsing problem?
# There are 3 types of forms here, We will restrict everything to 990.
#   990 is for organizations that bring in more than $250,000 - 1,697,326 orgs in our sample.
#   EZ is for those who bring in between $250,000 and $50,000 - 922,304 orgs in our sample.
#   PF is for private foundations - 440,511 orgs in our sample.