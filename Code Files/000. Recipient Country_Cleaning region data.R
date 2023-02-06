## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file uses the rough region information (locations_raw.csv) to clean up the region data by world region.

## Output:
#   TK (multiple files, one per world region)

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
# All regions
regions_raw <- read_csv("/Volumes/SRC_DATA/000_f990_data/locations_raw.csv")

# Americas
americas_raw <- read_csv("/Volumes/SRC_DATA/000_f990_data/americas_raw.csv")

# Europe
europe_raw <- read_csv("/Volumes/SRC_DATA/000_f990_data/europe_raw.csv")

# Africa
africa_raw <- read_csv("/Volumes/SRC_DATA/000_f990_data/africa_raw.csv")

# Asia
asia_raw <- read_csv("/Volumes/SRC_DATA/000_f990_data/asia_raw.csv")

# Oceania
oceania_raw <- read_csv("/Volumes/SRC_DATA/000_f990_data/oceania_raw.csv")

# Country and region names from UN
unsd <- read_delim("/Volumes/SRC_DATA/000_f990_data/UNSD.csv",
                   delim = ";")
# Source: https://unstats.un.org/unsd/methodology/m49/overview/

# regions raw has 5318 observations
# in the raw regions we have 2086 observations accounted for (roughly 40%)







#--------------------------------------------------------
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