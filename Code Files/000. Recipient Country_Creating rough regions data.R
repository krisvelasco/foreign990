## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file uses Schedule F to find the location (country, region) of recipients of foreign grants by U.S.-based nonprofits.

## Output:
#   **region**_raw.csv
#   Each file contains very rough data on the regions to which money is being sent.
#   These are only 5 of the 42 unique values in the locations_raw.csv

## Last updated: Feb. 3 by Sebastian Rojas Cabal
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
#--------------------------
# Part 1
# NO INFO
#dirty_f_1 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_i.csv")

# Part 2
# NO INFO
#dirty_f_2 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_ii.csv")

# Part 4
dirty_f_4 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_iv.csv")

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
# Appending region info from activities and individual grants data
#--------------------------
activities <- dirty_f_activities %>%
  select(RgnTxt) %>%
  distinct()

grants <- dirty_f_individualGrants %>%
  select(RgnTxt) %>%
  distinct()

# A string of very rough regions.
string_regions <- c("America|Europe|Africa|Asia|Pacific|Oceania")

regions_raw <- bind_rows(activities, grants) %>%
  mutate(
    RgnTxt = str_to_title(RgnTxt, locale = "en"),
    ) %>%
  rename(f_location = RgnTxt) %>%
  distinct() %>%
  mutate(
    length = str_length(f_location)
  ) %>%
  arrange(desc(length)) %>%
  mutate(
    region = str_extract_all(f_location, string_regions)
  ) %>%
  mutate(region = as.character(region),
         region2 = str_remove_all(region, "[()\"]"),
         region2 = case_when(region=="character(0)" ~ "NO REGION INFO",
                             TRUE ~ region2),
         region2= str_replace_all(region2, ", ", "_"),
         region2= case_when(str_detect(region2, "_") == TRUE ~ str_remove(region2, "c"),
                            TRUE ~ region2),
         dest_asia_pacific = case_when(str_detect(region2, "Asia|Pacific") == TRUE ~ 1,
                                       TRUE ~ 0),
         dest_europe = case_when(str_detect(region2, "Europe") == TRUE ~ 1,
                                 TRUE ~ 0),
         dest_africa  = case_when(str_detect(region2, "Africa") == TRUE ~ 1,
                                  TRUE ~ 0),
         dest_americas  = case_when(str_detect(region2, "America") == TRUE ~ 1,
                                    TRUE ~ 0),
         dest_multi = case_when(dest_asia_pacific + dest_africa + dest_europe + dest_americas > 1 ~ 1,
                                TRUE ~ 0),
         dest_total = dest_asia_pacific + dest_africa + dest_europe + dest_americas
  )
         
         region=="haracter(0)" ~ "NO REGION INFO"
         
         )
         region2 = str_remove_all(region, "[()]"),
         region2 = str_remove_all(region, '[\"]')
         )


'[\"]', ''
  
           
           
           
           region,
         region = str_remove_all(""),
         region2 = case_when(
           region=="character(0)" ~ "NO REGION INFO",
           region=="c(Asia", "Pacific")" ~ "NO REGION INFO"
           region=="c(h)" ~ "NO REGION INFO"
           region=="c(h)" ~ "NO REGION INFO"
           region=="c(h)" ~ "NO REGION INFO"
           region=="c(h)" ~ "NO REGION INFO"
           region=="c(h)" ~ "NO REGION INFO"
           region=="c(h)" ~ "NO REGION INFO"
           region=="c(h)" ~ "NO REGION INFO"
           region=="character(0)" ~ "NO REGION INFO"
           region=="character(0)" ~ "NO REGION INFO"
           region=="character(0)" ~ "NO REGION INFO"
           region=="character(0)" ~ "NO REGION INFO"
           region=="character(0)" ~ "NO REGION INFO"
           region=="character(0)" ~ "NO REGION INFO"
           region=="character(0)" ~ "NO REGION INFO")
  )

# Americas
rgn_americas <- regions_raw %>%
  filter(region == "America") %>%
  mutate(region = "Americas") %>%
  distinct()

# Oceania
rgn_oceania <- regions_raw %>%
  filter(region == "Oceania") %>%
  distinct() %>%
  mutate(regio = "Oceania")

# Asia
rgn_asia <- regions_raw %>%
  filter(region == "Asia") %>%
  distinct() %>%
  mutate(region = "Asia")

# Europe
rgn_europe <- regions_raw %>%
  filter(region == "Europe") %>%
  distinct() %>%
  mutate(region = "Europe")

# Africa
rgn_africa <- regions_raw %>%
  filter(region == "Africa") %>%
  distinct() %>%
  mutate(region = "Africa")

#--------------------------
# Exporting locations_raw.csv
#--------------------------
write_csv(regions_raw, "/Volumes/SRC_DATA/000_f990_data/locations_raw.csv")
# Regions
write_csv(rgn_africa, "/Volumes/SRC_DATA/000_f990_data/africa_raw.csv")
write_csv(rgn_europe, "/Volumes/SRC_DATA/000_f990_data/europe_raw.csv")
write_csv(rgn_asia, "/Volumes/SRC_DATA/000_f990_data/asia_raw.csv")
write_csv(rgn_oceania, "/Volumes/SRC_DATA/000_f990_data/oceania_raw.csv")
write_csv(rgn_americas, "/Volumes/SRC_DATA/000_f990_data/americas_raw.csv")

################
################
################
################
################
#--------------------------
# Cleaning by rough regions
#   There are 44 unique combinations of regions.
#     For example, for those in which region == "Europe",
#     we can be certain all countries are in Europe.
#     If region == "Asia" "Africa", we won't know how much went where.
#--------------------------

# Americas
un_americas <- unsd %>%
  filter(`Region Name` == "Americas")

rgn_americas <- regions_raw %>%
  filter(region == "America") %>%
  distinct()
  
string_americas <- c("South America|Central America|North America")

rgn_americas <- rgn_americas %>%
mutate(
  subregion = str_extract_all(f_location, string_americas)
)

americas_nosub <- filter(rgn_americas, subregion == "character(0)")
  
# A possible way for finding countries
#   I am envisioning each column being named after a country and then doing some sort of summarizing.
  mutate(
    country_candidate = str_extract(f_location, "(?<=-).*"),
    argentina = case_when(
      str_detect(country_candidate, "Argentina") == TRUE ~ 1
    )
  )


#------- NOT THERE YET
# Oceania
rgn_oceania <- regions_raw %>%
  filter(region == "Oceania") %>%
  distinct()

# Asia
rgn_asia <- regions_raw %>%
  filter(region == "Asia") %>%
  distinct()

# Europe
rgn_europe <- regions_raw %>%
  filter(region == "Europe") %>%
  distinct()

# Africa
rgn_africa <- regions_raw %>%
  filter(region == "Africa") %>%
  distinct()


