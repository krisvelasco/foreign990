## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file uses Schedule F to find the location (country, region) of recipients of foreign grants by U.S.-based nonprofits.

## Output:
#   regions_activities.csv
#   regions_cashgrants.csv
#   regions_noncashgrants.csv
#     Contains very rough data on the regions to which money is being sent.
#     There is some region info for at least:
#       93% of the observations in the activities file.
#       91% of the observations in the individual grants file.

## Last updated: Feb. 7 by Sebastian Rojas Cabal
#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(places)
#--------------------------------------------------------
#--------------------------
# Data import
#   Activities, individual grants, UN country classification for country strings
#--------------------------
options(scipen=999) # Telling R to avoid the scientific notation altogether
# Part 1
# NO INFO
#dirty_f_1 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_i.csv")

# Part 2
# NO INFO
#dirty_f_2 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_ii.csv")

# Part 4
#dirty_f_4 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_iv.csv")

# Activities
# Region info available
dirty_f_activities <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_activities.csv")




# Individual Grants
# Region info available
dirty_f_individualGrants <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_individ_grants.csv")

# Supplement
# NO INFO
#dirty_f_supplement <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_supplement.csv")

# Country and region names from UN
unsd <- read_delim("/Volumes/SRC_DATA/000_f990_data/UNSD.csv",
                   delim = ";")
# Source: https://unstats.un.org/unsd/methodology/m49/overview/

#--------------------------
# Strings of region and country names
#--------------------------
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

#--------------------------
# Creating an activities and grants
# data frame to clean
#   Only distinct destination strings included
#--------------------------
activities <- dirty_f_activities %>%
  select(RgnTxt) %>%
  distinct()

grants <- dirty_f_individualGrants %>%
  select(RgnTxt) %>%
  distinct()
#--------------------------
# Cleaning Region Data: Activities
#--------------------------
activities <- dirty_f_activities %>%
  select(RgnTxt, RgnTtlExpndtrsAmt) %>%
  rename(expenses = RgnTtlExpndtrsAmt) %>%
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
  )

# When Total Functional Expenses = NA, drop. There are 917 such NAs.
# When Total Foreign Expenses OR Grants = NA, assume = 0.
# Dropping all cases when Functional Expenses >= 0

activities_cln <- activities %>%
  mutate(
    clean_activities = case_when(
      is.na(expenses) == TRUE ~ -999,
      TRUE ~ expenses)
  ) %>%
  filter(expenses > 0)
# By taking out expenses = NA, and negatives,
# we lose 5% of observations (12499)

#--------------------------
# Exporting regions_activities.csv
#--------------------------
write_csv(activities_cln, "/Volumes/SRC_DATA/000_f990_data/regions_activities.csv")
#--------------------------
#--------------------------
# Cleaning Region Data: Grants
#--------------------------
grants <- dirty_f_individualGrants %>%
  select(RgnTxt, CshGrntAmt, NnCshAssstncAmt) %>%
  rename(
    cash_grant = CshGrntAmt,
    noncash_grant = NnCshAssstncAmt,
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
  )
#--------------------------
#     Cash Grants
#--------------------------
# When Total Functional Expenses = NA, drop. There are 917 such NAs.
# When Total Foreign Expenses OR Grants = NA, assume = 0.
# Dropping all cases when Functional Expenses >= 0

grants_cash_cln <- grants %>%
  select(-noncash_grant) %>%
  mutate(
    clean_cash_grant = case_when(
      is.na(cash_grant) == TRUE ~ -999,
      TRUE ~ cash_grant)
  ) %>%
  filter(clean_cash_grant > 0)
# By taking out cash grants = NA, = 0 and negatives,
# we lose 90% of observations (304707)
# BUT THIS IS ONLY BECAUSE NOT ALL ORGS GIVE CASH GRANTS
#--------------------------
# Exporting regions_cashgrants.csv
#--------------------------
write_csv(grants_cash_cln, "/Volumes/SRC_DATA/000_f990_data/regions_cashgrants.csv")
#--------------------------
#     Non-Cash Grants
#--------------------------
grants_noncash_cln <- grants %>%
  select(-cash_grant) %>%
  mutate(
    clean_noncash_grant = case_when(
      is.na(noncash_grant) == TRUE ~ -999,
      TRUE ~ noncash_grant)
  ) %>%
  filter(clean_noncash_grant > 0)
# By taking out non cash grants = NA, = 0 and negatives,
# we lose 5% of observations (12499) 
# BUT THIS IS ONLY BECAUSE NOT ALL ORGS GIVE OUT NON-CASH GRANTS
#--------------------------
# Exporting regions_noncashgrants.csv
#--------------------------
write_csv(grants_noncash_cln, "/Volumes/SRC_DATA/000_f990_data/regions_noncashgrants.csv")
#--------------------------
