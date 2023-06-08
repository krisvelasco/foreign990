## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file creates a dirty data set merging an indicator
#   of whether or not a nonprofit is anti- or non anti-LGBTQ+
#   with corresponding data from form 990 parts 0, 9, and the
#   return header.

## Last updated: May 1st, 2023 by Sebastian Rojas Cabal
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
#--------------------------
# Strings of region and country names
#--------------------------
# Country and region names from UN
unsd <- read_delim("/Volumes/SRC_DATA/000_f990_data/UNSD.csv",
                   delim = ";")
# Source: https://unstats.un.org/unsd/methodology/m49/overview/
string_regions <- c("America|Europe|Africa|Asia|Pacific|Oceania")
string_lower_regions <- str_to_lower(string_regions, locale = "en")

# Africa
list_africa <- unsd %>%
  filter(`Region Name` == "Africa") %>%
  select(`Country or Area`)

string_africa <- str_c(list_africa$`Country or Area`, collapse = "|")
string_lower_africa <- str_to_lower(string_africa, locale = "en")

# Europe
list_europe <- unsd %>%
  filter(`Region Name` == "Europe") %>%
  select(`Country or Area`)

string_europe <- str_c(list_europe$`Country or Area`, collapse = "|")
string_lower_europe <- str_to_lower(string_europe, locale = "en")

# Americas
list_americas <- unsd %>%
  filter(`Region Name` == "Americas") %>%
  select(`Country or Area`)

string_americas <- str_c(list_americas$`Country or Area`, collapse = "|")
string_lower_americas <- str_to_lower(string_americas, locale = "en")

# Asia-Pacific
list_asia_pacific  <- unsd %>%
  filter(`Region Name` == "Asia" |
           `Region Name` == "Oceania") %>%
  select(`Country or Area`)

string_asia_pacific <- str_c(list_asia_pacific$`Country or Area`, collapse = "|")
string_lower_asia <- str_to_lower(string_asia_pacific, locale = "en")
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
  mutate(anti_lgbtq = 1) %>%
  select(-name)
# -------------------------
# -------------------------  
# Sched F (from Jacob)
options(scipen=999) # Telling R to avoid the scientific notation altogether
#-------------------------
# Part 1
dirty_f_1 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_i.csv")
  
f_1_cln <- dirty_f_1 %>%  
  select(-id) %>%
  distinct() %>%
  mutate(
    id_ein = paste0(object_id, ein)
  ) %>%
  mutate_at(c('TtlSpntAmt'), ~replace_na(.,0)) %>% # replacing NAs with zeros (0)
  select(
    id_ein, object_id, ein, TtlSpntAmt
  ) 
#length(unique(f_1_cln$id))/nrow(f_1_cln)
#length(unique(f_1_cln$object_id))/nrow(f_1_cln)
#length(unique(f_1_cln$id_ein))/nrow(f_1_cln)

# Code to check for duplicates
#   name_df <- og_df %>%
#     group_by(object_id, ein) %>%
#     filter(n()>1) %>%
#     ungroup() %>%
#     arrange(ein)

#dirty_f_2 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_ii.csv")

#dirty_f_4 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_iv.csv")

#-------------
# Activities
#-------------
dirty_f_activities <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_activities.csv")

activities_cln <- dirty_f_activities %>%
  select(-id) %>%
  mutate(
    id_ein = paste0(object_id, ein)
  ) %>%
  mutate_at(c('RgnTtlExpndtrsAmt'), ~replace_na(.,0)) %>% # replacing NAs with zeros (0)
  select(
    id_ein, object_id, ein, RgnTxt, RgnTtlExpndtrsAmt
  ) %>%
  mutate(
    f_location = RgnTxt,
    f_location = str_to_lower(f_location, locale = "en"),
    region = str_extract_all(f_location, string_lower_regions),
    region = as.character(region),
    region2 = str_remove_all(region, "[()\"]"),
    region2 = case_when(region=="character(0)" ~ "NO REGION INFO",
                        TRUE ~ region2),
    region2= str_replace_all(region2, ", ", "_"),
    region2= case_when(str_detect(region2, "_") == TRUE ~ str_remove(region2, "c"),
                       TRUE ~ region2),
    dest_asia_pacific = case_when(str_detect(region2, "asia|pacific") == TRUE |
                                    str_detect(f_location, string_lower_asia) ~ 1,
                                  TRUE ~ 0),
    dest_europe = case_when(str_detect(region2, "europe") == TRUE |
                              str_detect(f_location, string_lower_europe) == TRUE~ 1,
                            TRUE ~ 0),
    dest_africa  = case_when(str_detect(region2, "africa") == TRUE |
                               str_detect(f_location, string_lower_africa) ~ 1,
                             TRUE ~ 0),
    dest_americas  = case_when(str_detect(region2, "america") == TRUE |
                                 str_detect(f_location, string_lower_americas) ~ 1,
                               TRUE ~ 0),
    dest_multi = case_when(dest_asia_pacific + dest_africa + dest_europe + dest_americas > 1 ~ 1,
                       TRUE ~ 0),
    dest_asia_pacific = case_when(dest_multi == 1 ~ 0,
                           TRUE ~ dest_asia_pacific),
    dest_americas = case_when(dest_multi == 1 ~ 0,
                           TRUE ~ dest_americas),
    dest_europe = case_when(dest_multi == 1 ~ 0,
                           TRUE ~ dest_europe),
    dest_africa = case_when(dest_multi == 1 ~ 0,
                           TRUE ~ dest_africa))
    
acts_multi <- activities_cln %>%
  filter(dest_multi == 1)

acts_noInfo <- activities_cln %>%
  filter(region2 == "NO REGION INFO")

acts_noInfo_count <- acts_noInfo %>%
  count(f_location) %>%
  arrange(desc(n))

africa <- acts_multi %>%
  filter(region2 == "africa")

length(unique(acts_multi$f_location))

unique(africa$f_location)


    # This part makes sure that the region data is accurate.
    dest_europe = case_when(
      f_location %in% Europe |
        f_location %in% AfricaEurope |
        f_location %in% AsiaEuropeAfrica |
        f_location %in% AsiaEuropeAfricaAmericas |
        f_location %in% EuropeAmericas |
        f_location %in% EuropeAsia |
        f_location %in% EuropeAsiaAmericas ~ 1,
      TRUE ~ dest_europe
    ),
    dest_africa = case_when(
      f_location %in% AfricaAmericas |
        f_location %in% AfricaEurope |
        f_location %in% AsiaAfrica |
        f_location %in% AsiaAmericasAfrica |
        f_location %in% AsiaEuropeAfrica |
        f_location %in% AsiaEuropeAfricaAmericas ~ 1,
      TRUE ~ dest_africa
    ),
    dest_asia_pacific = case_when(
      f_location %in% Asia |
        f_location %in% AsiaAfrica |
        f_location %in% AsiaAmericas |
        f_location %in% AsiaAmericasAfrica |
        f_location %in% AsiaEuropeAfrica |
        f_location %in% AsiaEuropeAfricaAmericas |
        f_location %in% EuropeAsia |
        f_location %in% EuropeAsiaAmericas  ~ 1,
      TRUE ~ dest_asia_pacific
    ),
    dest_americas = case_when(
      f_location %in% AfricaAmericas |
        f_location %in% AsiaAmericas |
        f_location %in% AsiaAmericasAfrica |
        f_location %in% AsiaEuropeAfricaAmericas |
        f_location %in% EuropeAmericas |
        f_location %in% EuropeAsiaAmericas  ~ 1,
      TRUE ~ dest_americas
    ),
    dest_multi = case_when(dest_asia_pacific + dest_africa + dest_europe + dest_americas > 1 ~ 1,
                           TRUE ~ 0),
    total_regions = dest_asia_pacific + dest_africa + dest_europe + dest_americas,
    # Region data is now accurate
    dst_fctr_americas = as.factor(
      case_when(
      dest_americas == 1 ~ "Americas",
      TRUE ~ "Other"
      )
    ),
    dst_fctr_africa = as.factor(
      case_when(
        dest_africa == 1 ~ "Africa",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_europe = as.factor(
      case_when(
        dest_europe == 1 ~ "Europe",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_asia_pacific = as.factor(
      case_when(
        dest_asia_pacific == 1 ~ "Asia-Pacific",
        TRUE ~ "Other"
      )
    ),
    dst_fctr = as.factor(
      case_when(
        dest_multi == 1 ~ "Multiple regions",
        dest_multi == 0 & dest_americas == 1 ~ "Americas",
        dest_multi == 0 & dest_africa == 1 ~ "Africa",
        dest_multi == 0 & dest_asia_pacific == 1 ~ "Asia-Pacific",
        dest_multi == 0 & dest_europe == 1 ~ "Europe",
        dest_americas == 0 & dest_europe == 0 &
        dest_africa == 0 & dest_asia_pacific == 0 ~ "Pending region info"
      )
    ))

# Important to decide: money (RgnTtlExpndtrsAmt) in -, those obs will be deleted, rights?
activities_americas = case_when(
  dest_americas == 1 & dest_multi == 0 ~ RgnTtlExpndtrsAmt,
  dest_americas == 1 & dest_multi == 1 ~ RgnTtlExpndtrsAmt/total_regions,
  TRUE ~ 0
),
activities_asia_pacific = case_when(
  dest_asia_pacific == 1 & dest_multi == 0 ~ RgnTtlExpndtrsAmt,
  dest_asia_pacific == 1 & dest_multi == 1 ~ RgnTtlExpndtrsAmt/total_regions,
  TRUE ~ 0
),
activities_europe = case_when(
  dest_europe == 1 & dest_multi == 0 ~ RgnTtlExpndtrsAmt,
  dest_europe == 1 & dest_multi == 1 ~ RgnTtlExpndtrsAmt/total_regions,
  TRUE ~ 0
),
activities_africa = case_when(
  dest_africa == 1 ~ & dest_multi == 0 ~ RgnTtlExpndtrsAmt,
  dest_africa == 1 & dest_multi == 1 ~ RgnTtlExpndtrsAmt/total_regions,
  TRUE ~ 0
),
activities_multi = case_when(
  dest_multi == 1 ~ RgnTtlExpndtrsAmt,
  TRUE ~ 0
),
activities_noInfo = case_when(
  dst_fctr == "Pending region info" ~ RgnTtlExpndtrsAmt,
  TRUE ~ 0
)
) %>%
  filter(
    total_regions != 0 # we have to account for total_regions == 0
  )
  group_by(id_ein) %>% 
  summarise(
    TotalActivities_americas=sum(activities_americas),
    TotalActivities_asia_pacific=sum(activities_asia_pacific),
    TotalActivities_europe=sum(activities_europe),
    TotalActivities_africa=sum(activities_africa),
    TotalActivities_multi=sum(activities_multi),
    TotalActivities_noInfo=sum(activities_noInfo),
    .groups = 'drop'
  )
  
# FOR WHEN WE WANT TO DEAL WITH DATA LOSS OF total_regions == 0
  missing_regions <- activities_cln %>%
    filter(
      total_regions == 0
    ) %>%
    group_by(f_location) %>%
    summarise(
      total_obs = n()
    ) %>%
    arrange(desc(total_obs))
#--------
# Checking for duplicates - Activities
#--------
  # select(object_id, ein) %>% # This code is all about making sure there are only unique id/ein pairings
  # mutate(
  #   id_ein = paste0(object_id, ein)
  # ) %>%
  # distinct(object_id, ein, id_ein)

length(unique(activities_cln$id_ein))/nrow(activities_cln)
#length(unique(activities_cln$id))/nrow(activities_cln)
#length(unique(activities_cln$object_id))/nrow(activities_cln)

# Code to check for duplicates
#   dups_act <- activities_cln %>%
#     group_by(object_id, ein) %>%
#     filter(n()>1) %>%
#     ungroup() %>%
#     arrange(ein)
#--------
#--------
# Understanding data loss - Activities
#   Data loss in the grants_rgn file is due to Middle East and North Africa
#   and Russia and Newly Independent States
#   Moreover, multi=1 is hard to know how much is going to what region
#--------
# acts_multi data frame
#----
acts_multi <- dirty_f_activities %>%
  select(-id) %>%
  mutate(
    id_ein = paste0(object_id, ein)
  ) %>%
  mutate_at(c('RgnTtlExpndtrsAmt'), ~replace_na(.,0)) %>% # replacing NAs with zeros (0)
  select(
    id_ein, object_id, ein, RgnTxt, RgnTtlExpndtrsAmt
  ) %>%
  mutate(
    f_location = RgnTxt,
    f_location = str_to_lower(f_location, locale = "en"),
    region = str_extract_all(f_location, string_lower_regions),
    region = as.character(region),
    region2 = str_remove_all(region, "[()\"]"),
    region2 = case_when(region=="character(0)" ~ "NO REGION INFO",
                        TRUE ~ region2),
    region2= str_replace_all(region2, ", ", "_"),
    region2= case_when(str_detect(region2, "_") == TRUE ~ str_remove(region2, "c"),
                       TRUE ~ region2),
    dest_asia_pacific = case_when(str_detect(region2, "asia|pacific") == TRUE |
                                    str_detect(f_location, string_lower_asia) ~ 1,
                                  TRUE ~ 0),
    dest_europe = case_when(str_detect(region2, "europe") == TRUE |
                              str_detect(f_location, string_lower_europe) == TRUE~ 1,
                            TRUE ~ 0),
    dest_africa  = case_when(str_detect(region2, "africa") == TRUE |
                               str_detect(f_location, string_lower_africa) ~ 1,
                             TRUE ~ 0),
    dest_americas  = case_when(str_detect(region2, "america") == TRUE |
                                 str_detect(f_location, string_lower_americas) ~ 1,
                               TRUE ~ 0),
    dest_americas = case_when(
        f_location == "europe (including iceland and greenland)" |
        f_location == "europe/iceland and greenland" |                                             
        f_location == "europe (including iceland & greenland) - albania, andorra, austria, belgium" |
        f_location == "europe (including iceland & greenland)" |                                     
        f_location == "europe including iceland and greenland" |                                     
        f_location == "europe (including iceland & greenland) -" |
        f_location == "europe/iceland/greenland" |
        f_location == "europe (including inceland & greenland)" |                                   
        f_location == "europe (including iceland & greenland) - albania, andorra, austria, belgiu" |
        f_location == "europe (including iceland and greenland" |
        f_location == "europe(including iceland and greenland)" |
        f_location == "europe ( including iceland and greenland)" |
        f_location == "europe/inceland/greenland" ~ 0,
      TRUE ~ dest_americas
    ),
    dest_asia_pacific = case_when(
        f_location == "romania" |
        f_location == "timisoara, romania (europe)" |                                                
        f_location == "europe (including romania) -" |                                               
        f_location == "northeastern romania" |
        f_location == "tulcea, romania" |
        f_location == "timis county, romania" |
        f_location == "romania-eu" |         
        f_location == "e. europe-roman" ~ 0,
      TRUE ~ dest_asia_pacific
    ),
    dest_multi = case_when(dest_asia_pacific + dest_africa + dest_europe + dest_americas > 1 ~ 1,
                           TRUE ~ 0),
    dest_total = dest_asia_pacific + dest_africa + dest_europe + dest_americas,
    dst_fctr_americas = as.factor(
      case_when(
        dest_americas == 1 ~ "Americas",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_africa = as.factor(
      case_when(
        dest_africa == 1 ~ "Africa",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_europe = as.factor(
      case_when(
        dest_europe == 1 ~ "Europe",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_asia_pacific = as.factor(
      case_when(
        dest_asia_pacific == 1 ~ "Asia-Pacific",
        TRUE ~ "Other"
      )
    ),
    dst_fctr = as.factor(
      case_when(
        dest_multi == 1 ~ "Multiple regions",
        dest_multi == 0 & dest_americas == 1 ~ "Americas",
        dest_multi == 0 & dest_africa == 1 ~ "Africa",
        dest_multi == 0 & dest_asia_pacific == 1 ~ "Asia-Pacific",
        dest_multi == 0 & dest_europe == 1 ~ "Europe",
        dest_americas == 0 & dest_europe == 0 &
          dest_africa == 0 & dest_asia_pacific == 0 ~ "Pending region info"
      )
    ),
    activities_americas = case_when(
      dest_americas == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
    activities_asia_pacific = case_when(
      dest_asia_pacific == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
    activities_europe = case_when(
      dest_europe == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
    activities_africa = case_when(
      dest_africa == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
    activities_multi = case_when(
      dest_multi == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
    activities_noInfo = case_when(
      dst_fctr == "Pending region info" ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
  ) %>%
filter(dest_multi == 1) #%>% # Taking out obs with multiple regions. They make things hard. Text is behaving weirdly.
  group_by(id_ein) %>% 
  summarise(
    TotalActivities_americas=sum(activities_americas),
    TotalActivities_asia_pacific=sum(activities_asia_pacific),
    TotalActivities_europe=sum(activities_europe),
    TotalActivities_africa=sum(activities_africa),
    TotalActivities_multi=sum(activities_multi),
    TotalActivities_noInfo=sum(activities_noInfo),
    .groups = 'drop'
  )
#----
acts_rgn_id <- activities_cln %>%
  select(id_ein) %>%
  distinct()

acts_multi_id <- acts_multi %>%
  select(id_ein) %>%
  distinct()

acts_dataloss <- anti_join(acts_multi_id, acts_rgn_id) # since we're keeping the "multi" variable, we don't have data loss
acts_lostID <- acts_dataloss$id_ein
#----
# See how the lost data look like
#----
acts_dataloss_df <- dirty_f_activities %>%
  select(-id) %>%
  mutate(
    id_ein = paste0(object_id, ein)
  ) %>%
  mutate_at(c('RgnTtlExpndtrsAmt'), ~replace_na(.,0)) %>% # replacing NAs with zeros (0)
  select(
    id_ein, object_id, ein, RgnTxt, RgnTtlExpndtrsAmt
  ) %>%
  filter(id_ein %in% acts_lostID) %>%
  mutate(
    f_location = RgnTxt,
    f_location = str_to_lower(f_location, locale = "en"),
    region = str_extract_all(f_location, string_lower_regions),
    region = as.character(region),
    region2 = str_remove_all(region, "[()\"]"),
    region2 = case_when(region=="character(0)" ~ "NO REGION INFO",
                        TRUE ~ region2),
    region2= str_replace_all(region2, ", ", "_"),
    region2= case_when(str_detect(region2, "_") == TRUE ~ str_remove(region2, "c"),
                       TRUE ~ region2),
    dest_asia_pacific = case_when(str_detect(region2, "asia|pacific") == TRUE |
                                    str_detect(f_location, string_lower_asia) ~ 1,
                                  TRUE ~ 0),
    dest_europe = case_when(str_detect(region2, "europe") == TRUE |
                              str_detect(f_location, string_lower_europe) == TRUE~ 1,
                            TRUE ~ 0),
    dest_africa  = case_when(str_detect(region2, "africa") == TRUE |
                               str_detect(f_location, string_lower_africa) ~ 1,
                             TRUE ~ 0),
    dest_americas  = case_when(str_detect(region2, "america") == TRUE |
                                 str_detect(f_location, string_lower_americas) ~ 1,
                               TRUE ~ 0),
    dest_americas = case_when(
        f_location == "europe (including iceland and greenland)" |
        f_location == "europe/iceland and greenland" |                                             
        f_location == "europe (including iceland & greenland) - albania, andorra, austria, belgium" |
        f_location == "europe (including iceland & greenland)" |                                     
        f_location == "europe including iceland and greenland" |                                     
        f_location == "europe (including iceland & greenland) -" |
        f_location == "europe/iceland/greenland" |
        f_location == "europe (including inceland & greenland)" |                                   
        f_location == "europe (including iceland & greenland) - albania, andorra, austria, belgiu" |
        f_location == "europe (including iceland and greenland" |
        f_location == "europe(including iceland and greenland)" |
        f_location == "europe ( including iceland and greenland)" |
        f_location == "europe/inceland/greenland" ~ 0,
      TRUE ~ dest_americas
    ),
    dest_asia_pacific = case_when(
      f_location == "romania" |
        f_location == "timisoara, romania (europe)" |                                                
        f_location == "europe (including romania) -" |                                               
        f_location == "northeastern romania" |
        f_location == "tulcea, romania" |
        f_location == "timis county, romania" |
        f_location == "romania-eu" |         
        f_location == "e. europe-roman" ~ 0,
      TRUE ~ dest_asia_pacific
    ),
    dest_multi = case_when(dest_asia_pacific + dest_africa + dest_europe + dest_americas > 1 ~ 1,
                           TRUE ~ 0),
    dest_total = dest_asia_pacific + dest_africa + dest_europe + dest_americas,
    dst_fctr_americas = as.factor(
      case_when(
        dest_americas == 1 ~ "Americas",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_africa = as.factor(
      case_when(
        dest_africa == 1 ~ "Africa",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_europe = as.factor(
      case_when(
        dest_europe == 1 ~ "Europe",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_asia_pacific = as.factor(
      case_when(
        dest_asia_pacific == 1 ~ "Asia-Pacific",
        TRUE ~ "Other"
      )
    ),
    dst_fctr = as.factor(
      case_when(
        dest_multi == 1 ~ "Multiple regions",
        dest_multi == 0 & dest_americas == 1 ~ "Americas",
        dest_multi == 0 & dest_africa == 1 ~ "Africa",
        dest_multi == 0 & dest_asia_pacific == 1 ~ "Asia-Pacific",
        dest_multi == 0 & dest_europe == 1 ~ "Europe",
        dest_americas == 0 & dest_europe == 0 &
          dest_africa == 0 & dest_asia_pacific == 0 ~ "Pending region info"
      )
    ),
    activities_americas = case_when(
      dest_americas == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
    activities_asia_pacific = case_when(
      dest_asia_pacific == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
    activities_europe = case_when(
      dest_europe == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
    activities_africa = case_when(
      dest_africa == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
    activities_multi = case_when(
      dest_multi == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
    activities_noInfo = case_when(
      dst_fctr == "Pending region info" ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    ),
  )
#----
#-------------
# Individual Grants
#-------------
dirty_f_individualGrants <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_individ_grants.csv")

grants <- dirty_f_individualGrants %>%
  select(-id) %>%
  mutate(
    id_ein = paste0(object_id, ein)
  ) %>%
  select(
    id_ein, object_id, ein, RgnTxt, CshGrntAmt, NnCshAssstncAmt
  ) %>%
  mutate_at(c('CshGrntAmt', "NnCshAssstncAmt"), ~replace_na(.,0)) # Replacing NA values with zeros (0)
#distinct(object_id, ein, id_ein)
#length(unique(grants$id_ein))/nrow(grants)
#length(unique(grants$id))/nrow(grants)

grants_id <- select(grants, id_ein, object_id, ein) %>% distinct()

grants_sum <- grants %>%
  group_by(id_ein, RgnTxt) %>%
  summarise(f.cashGrant=sum(CshGrntAmt),
            f.nonCashGrant=sum(NnCshAssstncAmt),
            .groups = 'drop')

grants_cln <- left_join(grants_sum, grants_id, by = ("id_ein")) %>%
  select(id_ein, object_id, ein, RgnTxt, f.cashGrant, f.nonCashGrant)

# Cleaning regional data
grants_rgn <- grants_cln %>%
  mutate(
    f_location = RgnTxt,
    f_location = str_to_lower(f_location, locale = "en"),
    region = str_extract_all(f_location, string_lower_regions),
    region = as.character(region),
    region2 = str_remove_all(region, "[()\"]"),
    region2 = case_when(region=="character(0)" ~ "NO REGION INFO",
                        TRUE ~ region2),
    region2= str_replace_all(region2, ", ", "_"),
    region2= case_when(str_detect(region2, "_") == TRUE ~ str_remove(region2, "c"),
                       TRUE ~ region2),
    dest_asia_pacific = case_when(str_detect(region2, "asia|pacific") == TRUE |
                                    str_detect(f_location, string_lower_asia) ~ 1,
                                  TRUE ~ 0),
    dest_europe = case_when(str_detect(region2, "europe") == TRUE |
                              str_detect(f_location, string_lower_europe) == TRUE~ 1,
                            TRUE ~ 0),
    dest_africa  = case_when(str_detect(region2, "africa") == TRUE |
                               str_detect(f_location, string_lower_africa) ~ 1,
                             TRUE ~ 0),
    dest_americas  = case_when(str_detect(region2, "america") == TRUE |
                                 str_detect(f_location, string_lower_americas) ~ 1,
                               TRUE ~ 0),
    dest_americas = case_when(
      f_location == "europe (including iceland and greenland)" |
      f_location == "europe/iceland and greenland" |                                             
      f_location == "europe (including iceland & greenland) - albania, andorra, austria, belgium" |
      f_location == "europe (including iceland & greenland)" |                                     
      f_location == "europe including iceland and greenland" |                                     
      f_location == "europe (including iceland & greenland) -" |
      f_location == "europe/iceland/greenland" |
      f_location == "europe (including inceland & greenland)" |                                   
      f_location == "europe (including iceland & greenland) - albania, andorra, austria, belgiu" |
      f_location == "europe (including iceland and greenland" |
      f_location == "europe(including iceland and greenland)" |
      f_location == "europe ( including iceland and greenland)" |
      f_location == "europe/inceland/greenland" ~ 0,
      TRUE ~ dest_americas
      ),
    dest_asia_pacific = case_when(
      f_location == "romania" |
      f_location == "timisoara, romania (europe)" |                                                
      f_location == "europe (including romania) -" |                                               
      f_location == "northeastern romania" |
      f_location == "tulcea, romania" |
      f_location == "timis county, romania" |
      f_location == "romania-eu" |         
      f_location == "e. europe-roman" ~ 0,
      TRUE ~ dest_asia_pacific
    ),
    dest_multi = case_when(dest_asia_pacific + dest_africa + dest_europe + dest_americas > 1 ~ 1,
                           TRUE ~ 0),
    dest_asia_pacific = case_when(dest_multi == 1 ~ 0,
                                  TRUE ~ dest_asia_pacific),
    dest_americas = case_when(dest_multi == 1 ~ 0,
                              TRUE ~ dest_americas),
    dest_europe = case_when(dest_multi == 1 ~ 0,
                            TRUE ~ dest_europe),
    dest_africa = case_when(dest_multi == 1 ~ 0,
                            TRUE ~ dest_africa),
    dst_fctr_americas = as.factor(
      case_when(
        dest_americas == 1 ~ "Americas",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_africa = as.factor(
      case_when(
        dest_africa == 1 ~ "Africa",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_europe = as.factor(
      case_when(
        dest_europe == 1 ~ "Europe",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_asia_pacific = as.factor(
      case_when(
        dest_asia_pacific == 1 ~ "Asia-Pacific",
        TRUE ~ "Other"
      )
    ),
    dst_fctr = as.factor(
      case_when(
        dest_multi == 1 ~ "Multiple regions",
        dest_multi == 0 & dest_americas == 1 ~ "Americas",
        dest_multi == 0 & dest_africa == 1 ~ "Africa",
        dest_multi == 0 & dest_asia_pacific == 1 ~ "Asia-Pacific",
        dest_multi == 0 & dest_europe == 1 ~ "Europe",
        dest_americas == 0 & dest_europe == 0 &
          dest_africa == 0 & dest_asia_pacific == 0 ~ "Pending region info"
      )
    ),
    cashGrants_americas = case_when(
      dest_americas == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_asia_pacific = case_when(
      dest_asia_pacific == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_europe = case_when(
      dest_europe == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_africa = case_when(
      dest_africa == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_multi = case_when(
      dest_multi == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_noInfo = case_when(
      dst_fctr == "Pending region info" ~ f.cashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_americas = case_when(
      dest_americas == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_asia_pacific = case_when(
      dest_asia_pacific == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_europe = case_when(
      dest_europe == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_africa = case_when(
      dest_africa == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_multi = case_when(
      dest_multi == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_noInfo = case_when(
      dst_fctr == "Pending region info" ~ f.nonCashGrant,
      TRUE ~ 0
    )
  ) %>%
  group_by(id_ein) %>% 
  summarise(
    TotalCashGrants_americas=sum(cashGrants_americas),
    TotalCashGrants_asia_pacific=sum(cashGrants_asia_pacific),
    TotalCashGrants_europe=sum(cashGrants_europe),
    TotalCashGrants_africa=sum(cashGrants_africa),
    TotalCashGrants_multi=sum(cashGrants_multi),
    TotalCashGrants_noInfo=sum(cashGrants_noInfo),
    TotalNonCashGrants_americas=sum(NonCashGrants_americas),
    TotalNonCashGrants_asia_pacific=sum(NonCashGrants_asia_pacific),
    TotalNonCashGrants_europe=sum(NonCashGrants_europe),
    TotalNonCashGrants_africa=sum(NonCashGrants_africa),
    TotalNonCashGrants_multi=sum(NonCashGrants_multi),
    TotalNonCashGrants_noInfo=sum(NonCashGrants_noInfo),
    .groups = 'drop'
  )

#--------
# Understanding data loss - Individual grants
#   Data loss in the grants_rgn file is due to Middle East and North Africa
#   and Russia and Newly Independent States
#   Moreover, multi=1 is hard to know how much is going to what region
#--------
grants_rgn_id <- grants_rgn %>%
  select(id_ein) %>%
  distinct()

grants_dataloss <- anti_join(grants_id, grants_rgn_id) # it is 0 because we corrected for the multiple regions problem
grants_lostID <- grants_dataloss$id_ein

grants_lost_df <- grants_cln %>%
  filter(id_ein %in% grants_lostID) %>%
  mutate(
    f_location = RgnTxt,
    f_location = str_to_lower(f_location, locale = "en"),
    region = str_extract_all(f_location, string_lower_regions),
    region = as.character(region),
    region2 = str_remove_all(region, "[()\"]"),
    region2 = case_when(region=="character(0)" ~ "NO REGION INFO",
                        TRUE ~ region2),
    region2= str_replace_all(region2, ", ", "_"),
    region2= case_when(str_detect(region2, "_") == TRUE ~ str_remove(region2, "c"),
                       TRUE ~ region2),
    dest_asia_pacific = case_when(str_detect(region2, "asia|pacific") == TRUE |
                                    str_detect(f_location, string_lower_asia) ~ 1,
                                  TRUE ~ 0),
    dest_europe = case_when(str_detect(region2, "europe") == TRUE |
                              str_detect(f_location, string_lower_europe) == TRUE~ 1,
                            TRUE ~ 0),
    dest_africa  = case_when(str_detect(region2, "africa") == TRUE |
                               str_detect(f_location, string_lower_africa) ~ 1,
                             TRUE ~ 0),
    dest_americas  = case_when(str_detect(region2, "america") == TRUE |
                                 str_detect(f_location, string_lower_americas) ~ 1,
                               TRUE ~ 0),
    dest_multi = case_when(dest_asia_pacific + dest_africa + dest_europe + dest_americas > 1 ~ 1,
                           TRUE ~ 0),
    dest_total = dest_asia_pacific + dest_africa + dest_europe + dest_americas,
    dst_fctr_americas = as.factor(
      case_when(
        dest_americas == 1 ~ "Americas",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_africa = as.factor(
      case_when(
        dest_africa == 1 ~ "Africa",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_europe = as.factor(
      case_when(
        dest_europe == 1 ~ "Europe",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_asia_pacific = as.factor(
      case_when(
        dest_asia_pacific == 1 ~ "Asia-Pacific",
        TRUE ~ "Other"
      )
    ),
    dst_fctr = as.factor(
      case_when(
        dest_multi == 1 ~ "Multiple regions",
        dest_multi == 0 & dest_americas == 1 ~ "Americas",
        dest_multi == 0 & dest_africa == 1 ~ "Africa",
        dest_multi == 0 & dest_asia_pacific == 1 ~ "Asia-Pacific",
        dest_multi == 0 & dest_europe == 1 ~ "Europe",
        dest_americas == 0 & dest_europe == 0 &
          dest_africa == 0 & dest_asia_pacific == 0 ~ "Pending region info"
      )
    )
  ) #%>%
  mutate(
    cashGrants_americas = case_when(
      dest_americas == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_asia_pacific = case_when(
      dest_asia_pacific == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_europe = case_when(
      dest_europe == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_africa = case_when(
      dest_africa == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_multi = case_when(
      dest_multi == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_noInfo = case_when(
      dst_fctr == "Pending region info" ~ f.cashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_americas = case_when(
      dest_americas == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_asia_pacific = case_when(
      dest_asia_pacific == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_europe = case_when(
      dest_europe == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_africa = case_when(
      dest_africa == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_multi = case_when(
      dest_multi == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_noInfo = case_when(
      dst_fctr == "Pending region info" ~ f.nonCashGrant,
      TRUE ~ 0
    )
  )
#------

# Merge across Sched F

f_1_act <- full_join(activities_cln, f_1_cln, by = ("id_ein")) %>% #keeping all obs in part 1 and activities

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
#-------------
# Merge:
# Everything must be matched to pt0 of Form 990, where the yearly data is
#-------------
# -------------------------
#   Importing Return Header of Form 990
# -------------------------
rtrn <- read_csv("/Volumes/SRC_DATA/000_f990_data/return_header.csv")
# these are the forms submissions with metadata from Amazon Web Services/IRS.
#   length(unique(rtrn$id))/nrow(rtrn) # all ids are unique
#   length(unique(rtrn990$object_id))/nrow(rtrn990) # object_ids are not unique. Parsing problem?
# There are 3 types of forms here, We will restrict everything to 990.
#   990 is for organizations that bring in more than $250,000 - 1,697,326 orgs in our sample.
#   EZ is for those who bring in between $250,000 and $50,000 - 922,304 orgs in our sample.
#   PF is for private foundations - 440,511 orgs in our sample.
rtrn990 <- filter(rtrn, RtrnHdr_RtrnCd == 990) %>%
  rename(
    rtrn_timestmp = RtrnHdr_RtrnTs,
    rtrn_txyrbegin = RtrnHdr_TxPrdBgnDt,
    rtrn_txyrend = RtrnHdr_TxPrdEndDt,
    rtrn_form = RtrnHdr_RtrnCd,
    rtrn_EINfiler = Flr_EIN,
    rtrn_name1 = BsnssNm_BsnssNmLn1Txt,
    rtrn_name2 = BsnssNm_BsnssNmLn2Txt,
    rtrn_USaddrs1 = USAddrss_AddrssLn1Txt,
    rtrn_USaddrs2 = USAddrss_AddrssLn2Txt,
    rtrn_state = USAddrss_SttAbbrvtnCd,
    rtrn_county = USAddrss_CtyNm,
    rtrn_zip = USAddrss_ZIPCd) %>%
  select(ein, object_id,
         rtrn_timestmp,
         rtrn_txyrbegin,
         rtrn_txyrend)  %>%
  rename(rtrn_txyrbgndt = rtrn_txyrbegin,
         rtrn_txyrenddt = rtrn_txyrend) %>%
  mutate(rtrn_txyrstart = year(rtrn_txyrbgndt),
         rtrn_txyrend = year(rtrn_txyrenddt),
         id_ein = paste0(object_id, ein)) %>%
  relocate(rtrn_txyrstart, rtrn_txyrend, .before = rtrn_txyrbgndt) %>%
  relocate(id_ein, .before = ein)
# -------------------------
# -------------------------
#   Checking that all the ein x object_id combos
#   appear in the Return Header data
#     From activities and individual grants
# -------------------------
rtrn_id <- select(rtrn990, id_ein) %>% distinct()
id_grants <- select(grants_rgn, id_ein)
id_acts <- select(activities_cln, id_ein)
id_pt1 <- select(f_1_cln, id_ein)

# anti_join() return all rows from x without a match in y.
acts_noGrants <- anti_join(id_acts, id_grants) #40172.
# Most orgs giving out grants don't have "activities".
# This could make sense inasmuch as organizations can donate money without having formal activities (operations) on the ground.
pt1_noGrants <- anti_join(id_pt1, id_grants)

acts_return <- anti_join(id_acts, rtrn_id) # all of them have a match in the rtrn file!
grants_return <- anti_join(id_grants, rtrn_id) # all of them have a match in the rtrn file!
# -------------------------
#   Joining Sched F, part 1 and activities
# -------------------------
f_1_act <- full_join(activities_cln, f_1_cln, by = ("id_ein")) %>% #keeping all obs in part 1 and activities
  rename(
    pt1_f_totalActsSpent = TtlSpntAmt
  ) %>%
  relocate(object_id, ein, .before = TotalActivities_americas) %>%
  mutate_at(c("pt1_f_totalActsSpent",
              "TotalActivities_americas",
              "TotalActivities_asia_pacific",
              "TotalActivities_europe",
              "TotalActivities_africa",
              "TotalActivities_multi",
              "TotalActivities_noInfo"), ~replace_na(.,0)) # Replacing NA values with zeros (0)
# -------------------------
#   Joining Return Header + part 1 and activities
# -------------------------
rtrn_act <- left_join(rtrn990, f_1_act, by = ("id_ein")) %>%
  rename(
    ein = ein.x,
    object_id = object_id.x
         ) %>%
  mutate(
    acts = case_when(
      !is.na(TotalActivities_americas) |
      !is.na(TotalActivities_asia_pacific) |
      !is.na(TotalActivities_europe) |
      !is.na(TotalActivities_africa) |
      !is.na(TotalActivities_multi) |
      !is.na(TotalActivities_noInfo) |
      !is.na(pt1_f_totalActsSpent) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(-ein.y, -object_id.y) %>%
  mutate_at(c("TotalActivities_americas",
              "TotalActivities_asia_pacific",
              "TotalActivities_europe",
              "TotalActivities_africa",
              "TotalActivities_multi",
              "TotalActivities_noInfo",
              "pt1_f_totalActsSpent"
  ),
  ~replace_na(.,0)) %>%
  filter(acts==1)
# -------------------------
#   Joining Return Header + part 1 and activities + individual grants
# -------------------------
rtrn_grants <- left_join(rtrn990, grants_rgn, by = ("id_ein")) %>%
  mutate(
    grants = case_when(
      !is.na(TotalCashGrants_americas) |
      !is.na(TotalCashGrants_asia_pacific) |
      !is.na(TotalCashGrants_europe) |
      !is.na(TotalCashGrants_africa) |         
      !is.na(TotalCashGrants_multi) |
      !is.na(TotalCashGrants_noInfo) |
      !is.na(TotalNonCashGrants_americas) |
      !is.na(TotalNonCashGrants_asia_pacific) |
      !is.na(TotalNonCashGrants_europe) |
      !is.na(TotalNonCashGrants_africa) |
      !is.na(TotalNonCashGrants_multi) |
      !is.na(TotalNonCashGrants_noInfo) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate_at(c("TotalCashGrants_americas",
              "TotalCashGrants_asia_pacific",
              "TotalCashGrants_europe",
              "TotalCashGrants_africa",
              "TotalCashGrants_multi",
              "TotalCashGrants_noInfo",
              "TotalNonCashGrants_americas",
              "TotalNonCashGrants_asia_pacific",
              "TotalNonCashGrants_europe",
              "TotalNonCashGrants_africa",
              "TotalNonCashGrants_multi",
              "TotalNonCashGrants_noInfo"
              ),
            ~replace_na(.,0))  %>%  # replacing NAs with zeros (0)
  filter(grants==1)

# -------------------------
# Exporting the data
# -------------------------
write_csv(rtrn_grants, "/Volumes/SRC_DATA/000_f990_data/rtrn_grants.csv")

write_csv(rtrn_act, "/Volumes/SRC_DATA/000_f990_data/rtrn_activities.csv")
