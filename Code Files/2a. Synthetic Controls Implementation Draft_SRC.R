## Project: Nonprofit Foreign Expenditures

## Overview: 
##  Implementing models with synthetic controls. First draft.

#Last updated: May 20 by Sebastian Rojas Cabal

#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(magrittr)
library(augsynth)
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
#--------------------------------------------------------
#--------------------------------------------------------
# Importing the data
#--------------------------------------------------------
frgnxpns <- read_csv("/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/scm_sample_draft_220520.csv") %>%
  mutate(frgnXpns_2013 =
    case_when(
        tax_year == 2013 ~ frgnXpns/(232.957/232.957),
        tax_year == 2014 ~ frgnXpns/(232.957/236.736),
        tax_year == 2015 ~ frgnXpns/(232.957/237.017),
        tax_year == 2016 ~ frgnXpns/(232.957/240.007),
        tax_year == 2017 ~ frgnXpns/(232.957/245.120),
        tax_year == 2018 ~ frgnXpns/(232.957/251.107),
        tax_year == 2019 ~ frgnXpns/(232.957/255.657),
        tax_year == 2020 ~ frgnXpns/(232.957/258.811)
        )) %>%
  filter(tax_year != "2021") %>%
 select(ein, tax_year, rtrn_state, yearMrgEq_rtrn, frgnXpns_2013) %>%
  pivot_wider(
    values_from = frgnXpns_2013,
    names_from = tax_year,
    names_prefix = "frgnXpns_"
  ) %>%
  select(-frgnXpns_2013, -frgnXpns_2020, -frgnXpns_2019) %>%
  drop_na() %>%
  pivot_longer(
    values_to = "frgnXpns",
    cols = starts_with("frgnXpns_"),
    names_to = "tax_year"
  ) %>%
  mutate(
    tax_year = str_sub(tax_year, start = -4L)
  )

#--------------------------------------------------------
#--------------------------------------------------------
# Running the model
#--------------------------------------------------------
# Following the vignette
# https://github.com/ebenmichael/augsynth/blob/master/vignettes/multisynth-vignette.md
# Form: outcome ~ treatment

# with default nu
ppool_syn <- multisynth(frgnXpns ~ yearMrgEq_rtrn, rtrn_state, tax_year, 
                        frgnxpns)
# this does not run because we do not have enough information before the policy change.




