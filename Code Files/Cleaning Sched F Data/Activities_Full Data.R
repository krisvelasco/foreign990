## Project: Nonprofit Foreign Expenditures

# Merges summarized activities data with year and Anti-LGBTQ indicator data
#   Last updated: June 21, 2023

# Output:
#   activities_full_230621.csv
# Each row = org x year
# Contains year and LGBTQ indicator, as well as expenses x country
#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(xml2)
#-------------
#-------------
# Data Import
#-------------
options(scipen=999) # Telling R to avoid the scientific notation altogether

activities <- read_csv("/Volumes/SRC_DATA/000_f990_data/activities_sum_230621.csv") %>%
  mutate(
    id_ein = as.numeric(id_ein)
  )
# -------------------------
#   Importing Return Header of Form 990
# -------------------------
rtrn <- read_csv("/Volumes/SRC_DATA/000_f990_data/return_header.csv")
  
rtrn990 <- rtrn %>%
  filter(RtrnHdr_RtrnCd == 990) %>%
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

rtrn990 <- rtrn990 %>%
  mutate(
    id_ein = as.numeric(id_ein)
  )
# -------------------------
rtrn_act <- left_join(rtrn990, activities, by = ("id_ein")) %>%
  mutate(
    reports_acts = case_when(
      !is.na(reports_country) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(reports_acts==1)
# -------------------------
#   Checking that all the ein x object_id combos
#   appear in the Return Header data
#     From activities and individual grants
# -------------------------
rtrn_id <- select(rtrn990, id_ein) %>% distinct()

id_acts <- activities_country %>%
  select(id_ein) %>% 
  distinct() %>%
  pull(id_ein)

id_acts_df <- activities_country %>%
  select(id_ein) %>% 
  distinct()

acts_in_rtrn <- rtrn_id %>%
  filter(
    id_ein %in% id_acts
  )

acts_return <- semi_join(id_acts_df, rtrn_id) # all of them have a match in the rtrn file!
#-------------
#-------------
# Data cleaning
#-------------
#-------------

#-------------
#-------------
# Joining to Anti-LGBTQ org data, and to year data
#-------------

#-------------
#-------------
# Data export
#-------------
#write_csv(activities_country,
#          "/Volumes/SRC_DATA/000_f990_data/activities_sum_230621.csv")
#-------------