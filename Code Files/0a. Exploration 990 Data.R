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
library(readxl)

# Import data

#Schedule F
# Note: for now, only using "Activities" (Schedule F, Part 1, Line 3) and
# "Individual grants" (Schedule F, Part 2).
#sched_f_i <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_i.csv")
#sched_f_ii <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_ii.csv")
#sched_f_iv <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_iv.csv")
sched_f_activities <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_activities.csv")
sched_f_individ_grants <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_individ_grants.csv")

# Part 0 - Basic info
part_0 <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/part_0.csv")
  test <- slice(part_0, 1:5)

# Part IV - Where they tell you if they send money abroad or not
part_iv <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/part_iv.csv")

# Anti-LGBTQ orgs
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


# SO NEXT STEPS: RETURN HEADER TO IDENTIFY ORGS AND FILING YEARS, MERGE WITH LGBTQ REGISTRY TO CREATE "ANTI" DUMMY,
# THEN, GO ON PART IV TO SEE IF THEY CLAIM TO GIVE MONEY ABROAD, GET THE % OF EXPENDETURES ABROAD
# TOTAL GRANTS, TOTAL MONEY SPENT ABROAD, WHERE?
