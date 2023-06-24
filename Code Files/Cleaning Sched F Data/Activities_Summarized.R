## Project: Nonprofit Foreign Expenditures

# Summarizing data on foreign activities (Schedule F of Form 990)
#   Last updated: June 21, 2023

# Output:
#   activities_sum_230621.csv
# Each row = org x year
# Row containas dummy for whether or not money went to country X
# and info on how much money went there.
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
activities <- read_csv("/Volumes/SRC_DATA/000_f990_data/activities_country_230623.csv",
                       col_types = cols(
                          id_ein = col_character()
                          )
                        )
#-------------

kenya <- activities %>%
  filter(
    afr_kenya == 1
  ) %>%
  select(
    id_ein, f_location, lctn_xpnss_total, transfers_total, afr_kenya, exp_afr_kenya
  )

# examples of the problems we might have

#-------------
# Data cleaning
#-------------
#-------------
activities_clean <- activities %>%
  select(-f_location, -lctn_xpnss_total, -transfers_total)
#-------------
#-------------
# Summarized Activities - One Line per Org x Year
#-------------
# Prefixes to consider
prefixes <- c("country_", "asia_", "eur_",
              "amr_", "afr_", "exp_")

# Country. One line x org-year
activities_country <- activities_clean %>%
  group_by(id_ein) %>%
  summarise(across(starts_with((prefixes)), sum)) %>%
  rename(
    country_reported_n = country_reported
    ) %>%
  mutate(
    reports_country = case_when(
      country_reported_n > 0 ~ 1,
      country_reported_n == 0 ~ 0
    )
  )

kenya2 <- activities_country %>%
  filter(
    afr_kenya == 1
  ) %>%
  select(
    id_ein, afr_kenya, exp_afr_kenya
  )
#-------------
#-------------
# Data export
#-------------
#write_csv(activities_country,
#          "/Volumes/SRC_DATA/000_f990_data/activities_sum_230621.csv")
#-------------