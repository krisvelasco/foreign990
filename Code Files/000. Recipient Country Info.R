## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file uses Schedule F to find the location (country, region) of recipients of foreign grants by U.S.-based nonprofits.

## Output:
#   locations_raw.csv

## Last updated: Sept. 29th by Sebastian Rojas Cabal
#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
#--------------------------------------------------------
#--------------------------
# Data import
#--------------------------
# Part 1
# NO INFO
dirty_f_1 <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_i.csv")

# Part 2
# NO INFO
dirty_f_2 <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_ii.csv")

# Part 4
dirty_f_4 <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_iv.csv")

# Activities
# Region info available
dirty_f_activities <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_activities.csv")

# Individual Grants
# Region info available
dirty_f_individualGrants <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_individ_grants.csv")

# Supplement
# NO INFO
dirty_f_supplement <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_supplement.csv")

#--------------------------
# Appending region info from activities and individual grants data
#--------------------------
activities <- dirty_f_activities %>%
  select(RgnTxt) %>%
  distinct()

grants <- dirty_f_individualGrants %>%
  select(RgnTxt) %>%
  distinct()

regions_raw <- bind_rows(activities, grants) %>%
  mutate(
    RgnTxt = str_to_title(RgnTxt, locale = "en"),
    ) %>%
  rename(f_location = RgnTxt) %>%
  distinct() %>%
  mutate(
    length = str_length(f_location)
  ) %>%
  arrange(desc(length))

#--------------------------
# Exporting locations_raw.csv
#--------------------------
write_csv(regions_raw, "/Users/srojascabal/Desktop/000_f990_data/locations_raw.csv")

