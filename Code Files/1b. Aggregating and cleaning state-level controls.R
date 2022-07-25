## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file assembles the relevant control variables
#   for the preliminary models, to be appended to the
#   07/03 nonprofit data.
#   File differs from 1a in that it uses different measures
#   of number of tax-excempt orgs (from the Urban Institute's Businness Master Files)
#   and adds other variables from the American Community Survey.

## Last updated: July 25th by Sebastian Rojas Cabal
#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(readxl)
#--------------------------------------------------------
# Preliminaries
#   Loading data
#--------------------------------------------------------
# State-level controls
state_controls <- read_csv("/Users/srojascabal/Desktop/000_f990_data/state_controls.csv",
                           col_types = cols(
                             gov_party = col_character(),
                             gov_republican = col_double(),
                             state_code = col_character(),
                             tax_year = col_character(),
                             gdp_state_2012 = col_double(),
                             churches = col_double(),
                             F990_990EZ = col_double(),    
                             rel_orgs = col_double() 
                           )) %>%
  select(-churches,
         -F990_990EZ,
         -rel_orgs) %>%
  filter(
    tax_year != "2019" &
    tax_year != "2020"
  )

# Controls from the ACS
acs_controls <- read_csv("/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/acs_2008_2018.csv",
                           col_types = cols(
                             GEOID = col_character(),
                             NAME = col_character(),
                             year = col_character(),
                             state = col_character(),
                             frgn_born = col_double(),
                             state_population = col_double(),
                             college_educ_over_25 = col_double()
                           )) %>%
  rename(
    tax_year = year,
    state_code = state
  ) %>%
  select(
    -GEOID, -NAME
  )

# Controls from the Business Master Files
bmf_controls <- read_csv("/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf_excempt_orgs_state_years.csv",
  col_types = cols(
    YEAR = col_character(),
    STATE = col_character(),
    EXCEMPT_ORGS = col_double(),
    CHURCHES = col_double()
  )) %>%
  rename(
    tax_year = YEAR,
    state_code = STATE,
    excempt_orgs = EXCEMPT_ORGS,
    rel_orgs = CHURCHES
  )
#----
# Joining ACS, BMF, and other state-level data
#----
state_controls_new <- left_join(acs_controls, bmf_controls,
                          by = c("state_code", "tax_year")) %>%
                      left_join(state_controls,
                          by = c("state_code", "tax_year")) %>%
                      select(
                        tax_year,
                        state_code,
                        college_educ_over_25,
                        frgn_born,           
                        state_population,
                        excempt_orgs,        
                        rel_orgs,
                        gdp_state_2012,
                        gdp_state_2012_100k, 
                        gov_party,
                        gov_republican    
                      )
#----
# Exporting new state-level controls
#----
write_csv(
  state_controls_new,
  "/Users/srojascabal/Desktop/000_f990_data/state_controls_0725.csv"
)
