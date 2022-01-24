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
#library(countrycode) # I used to try out an efficient way of finding the places to which grants go.

# Import data

#Schedule F
# Note: for now, only using "Activities" (Schedule F, Part 1, Line 3) and
# "Individual grants" (Schedule F, Part 2).
#sched_f_i <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_i.csv")
#sched_f_ii <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_ii.csv")
#sched_f_iv <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_iv.csv")
sched_f_activities <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_f_activities.csv")
getwsched_f_individ_grants <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_f_individ_grants.csv")

# Known anti-LGBTQ orgs
antilgbt <- read_excel("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/Select Anti-LGBT+ Organizations - Foreign Expenditures.xlsx") %>%
    distinct(name, ein) %>%
    mutate(anti = 1,
           ein2 = ein,
           ein = as.numeric(str_remove_all(ein2, "-"))) # removed the - in the string in order to make the records compatible

# Header
header <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/return_header.csv")
  header_select <- select(header, ein, RtrnHdr_RtrnTs, RtrnHdr_TxPrdBgnDt, RtrnHdr_TxPrdEndDt) %>% # only the vars we need
  mutate(fiscal_year = year(RtrnHdr_TxPrdEndDt)) # creating the fiscal_year variable, will be useful later.
  
# Header with anti-lgbtq indicator
  header_anti <- full_join(header_select, antilgbt, by = c("ein")) %>%
    mutate(anti = case_when(
      anti == 1 ~ 1,
      TRUE ~ 0),
      anti_factor = as_factor(case_when(anti == 1 ~ "Anti-LGBTQ+",
                                        anti == 0 ~ "Other NGOs"))) %>%
    mutate(FileTs = RtrnHdr_RtrnTs)
  
  dups <- header_anti %>% count(ein, fiscal_year) %>%
    mutate(dup = case_when(
      n == 1 ~ 0,
      n > 1 ~ 1)) %>%
    group_by(ein, fiscal_year)
  
  dups2 <- filter(dups, dup == 1)
  
  header_anti2 <- full_join(header_anti, dups, by = c("ein", "fiscal_year")) # includes a dummy showing dups per ein x year

  test <- filter(header_anti2, dup == 1 & ein == 237450425)
  
  test2 <- test %>%
    slice_max(order_by = FileTs, n =1)
  

  testA <- filter(header_anti2, ein == 10211486)
  
  testB <- testA %>%
    group_by(fiscal_year) %>%
    slice_max(order_by = FileTs, n =1) # THIS METHOD WORKS, GROUPING BY EIN AND FISCAL YEAR, CHECK FOR DUPS AGAIN AFTER.
  
  # stop here

    group_by(ein) #%>%
    slice_max(order_by = FileTs, n = 1) # Removes multiple submissions in a single tax year. Keeps the most recent. 20436 obs removed.

# TRIAL - Regions spending
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
