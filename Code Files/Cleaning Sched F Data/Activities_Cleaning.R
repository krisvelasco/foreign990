## Project: Nonprofit Foreign Expenditures

# Cleaning data on foreign activities (Schedule F of Form 990)
#   Last updated: June 8, 2023

# Output:
#   activities_clean_prelim_230608.csv
#     This file includes factor and dummy variables
#     specifying the regions to which nonprofits send data

# BEFORE YOU RUN THIS FILE:
#   Open and run
#     Activities_Vectors_Multiple Regions.R
#     Activities_Vectors_No Region Info.R
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
#-------------
# Activities (from Jacob)
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
                           TRUE ~ dest_africa),
    # This part makes sure that the region data is accurate.
    dest_europe = case_when(
        f_location %in% noregion_EuropeAsia |
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
        f_location %in% noregion_Africa |
        f_location %in% noregion_AsiaAfria |
        f_location %in% AfricaAmericas |
        f_location %in% AfricaEurope |
        f_location %in% AsiaAfrica |
        f_location %in% AsiaAmericasAfrica |
        f_location %in% AsiaEuropeAfrica |
        f_location %in% AsiaEuropeAfricaAmericas ~ 1,
      TRUE ~ dest_africa
    ),
    dest_asia_pacific = case_when(
        f_location %in% noregion_Asia |
        f_location %in% noregion_AsiaAfria |
        f_location %in% noregion_EuropeAsia |
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
      f_location %in% noregion_Americas |
      f_location %in% AfricaAmericas |
      f_location %in% AsiaAmericas |
      f_location %in% AsiaAmericasAfrica |
      f_location %in% AsiaEuropeAfricaAmericas |
      f_location %in% EuropeAmericas |
      f_location %in% EuropeAsiaAmericas  ~ 1,
      TRUE ~ dest_americas
    ),
    dest_antarctica = case_when(
        f_location %in% noregion_Antarctica  ~ 1,
      TRUE ~ 0
    ),
    dest_noInfo = case_when(
      f_location %in% noregion_NoInfo  ~ 1,
      TRUE ~ 0
    ),
    dest_noInfo = case_when(
      f_location %in% UnitedStates  ~ 1,
      TRUE ~ 0
    ),
    dest_multi = case_when(dest_asia_pacific + dest_africa + dest_europe + dest_americas > 1 ~ 1,
                           TRUE ~ 0),
    total_regions = dest_asia_pacific + dest_africa + dest_europe + dest_americas,
    # There is still some info with errors when region2 == NO REGION INFO
    # as well as for cases when total_regions == 0
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
#-------------
# Export data
#-------------
write_csv(activities_cln,
          "/Volumes/SRC_DATA/000_f990_data/activities_clean_prelim_230608.csv")
#-------------
