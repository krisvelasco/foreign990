## Project: Nonprofit Foreign Expenditures
## Date: January 18, 2022
## Overview: 
  # This file explores the contents of the schedule F portion of the 990 data
  # I also do some data cleaning and produces some preliminary plots
  # Fenton provided base 990 + Schedule I + Schedule F for all nonprofits
  # These data are across 41 files -- so lots of data
  # Note: These data are in CSV and quite messy. It'll take some work to clean
#Last updated: January 18, 2022 by Sebastian Rojas Cabal

# Preliminaries
library(tidyverse)
library(lubridate)
library(readxl)
library(countrycode)

# Import data

#Schedule F
# Note: for now, only using "Activities" (Schedule F, Part 1, Line 3) and
# "Individual grants" (Schedule F, Part 2).
#sched_f_i <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_i.csv")
#sched_f_ii <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_ii.csv")
#sched_f_iv <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_iv.csv")
sched_f_activities <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_activities.csv")
sched_f_individ_grants <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_individ_grants.csv")

# Known anti-LGBTQ orgs
antilgbt <- read_excel("/Users/srojascabal/Google Drive/F990/Data from OneDrive/Select Anti-LGBT+ Organizations - Foreign Expenditures.xlsx") %>%
    distinct(name, ein) %>%
    mutate(anti = 1,
           ein2 = ein,
           ein = as.numeric(str_remove_all(ein2, "-"))) # removed the - in the string in order to make the records compatible

# Header
header <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/return_header.csv")
  header_select <- select(header, ein, RtrnHdr_RtrnTs, RtrnHdr_TxPrdEndDt, RtrnHdr_TxPrdBgnDt) %>% # only the vars we need
  mutate(fiscal_year = year(RtrnHdr_TxPrdEndDt),
         hour_submission = hour(RtrnHdr_RtrnTs)) # some changes on the year/time stamps, for future cleaning work
  
# Header with anti-lgbtq indicator
  header_anti <- full_join(header_select, antilgbt, by = c("ein")) %>%
    mutate(anti = case_when(
      anti == 1 ~ 1,
      TRUE ~ 0),
      anti_factor = as_factor(case_when(anti == 1 ~ "Anti-LGBTQ+",
                                        anti == 0 ~ "Other NGOs"))) %>%
    mutate(FileTs = RtrnHdr_RtrnTs) %>%
    group_by(ein, fiscal_year) %>%
    slice_max(order_by = FileTs, n = 1) # Removes multiple submissions in a single tax year. Keeps the most recent. 20438 obs removed.

# Regions spending
  where_activities$Region1 <- countrycode(sourcevar = where_activities$CleanRgn,
                                          origin = "country.name",
                                          destination = "continent")
  
                                  
# Part 0 - Basic info
part_0 <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/part_0.csv")
  test <- slice(part_0, 1:5)

# Part IV - Where they tell you if they send money abroad or not
part_iv <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/part_iv.csv")

# Sched F - Actvities
sched_f_activities <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_activities.csv")

varnames.sched_f_i <- colnames(sched_f_i)
varnames.sched_f_ii <- colnames(sched_f_ii)
varnames.sched_f_iv <- colnames(sched_f_iv)
varnames.sched_f_activities <- colnames(sched_f_activities)
varnames.sched_f_individ_grants <- colnames(sched_f_individ_grants)

# "Activities" (Schedule F, Part 1, Line 3) and "Individual grants" (Schedule F, Part 2) seem to have a lot of the info.
# Does Individual grants correspond to Part 2 (to entities?) or Part 3 (to individuals?)

# Rough and dirty exploration of unique places places
where_activities <- select(sched_f_activities, RgnTxt) %>%
  mutate(ToLowerRgn = str_to_lower(RgnTxt)) %>%
  mutate(CleanRgn = str_replace_all(ToLowerRgn, "[:punct:]", "")) %>%
  select(CleanRgn) %>%
  distinct() # over 3k unique places. That's a lot!


# THEN, GO ON PART IV TO SEE IF THEY CLAIM TO GIVE MONEY ABROAD, GET THE % OF EXPENDETURES ABROAD
# TOTAL GRANTS, TOTAL MONEY SPENT ABROAD, WHERE?
