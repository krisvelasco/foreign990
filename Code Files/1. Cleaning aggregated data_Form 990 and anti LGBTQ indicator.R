## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file cleans the dirty data set created in
#   1. Data Aggregation_Form 990 and anti LGBTQ indicator

## Last updated: June 30th by Sebastian Rojas Cabal
#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
#--------------------------------------------------------
#--------------------------
# Data import
#--------------------------
dirty_data <- read_csv("/Users/srojascabal/Desktop/000_f990_data/form990_rtrn09_anti_dirty.csv") %>%
  select(ein, object_id, # Selecting relevant variables
         tax_year, anti_lgbtq, anti_factor, name,
         rtrn_timestmp, rtrn_txyrbgndt, rtrn_txyrenddt,           
         pt9_totalFnctnlExpns, pt9_totalFrgnGrnts, FrgnExpnssPrctng, pt9_prgrmSrcvsAmtFrgnGrnts,
         pt0_state, rtrn_state)
#--------------------------
# Identifying the duplicated observations
#--------------------------   
# Many nonprofits submit miltiple forms a year. We only want the most recent.
dups <- dirty_data %>% count(ein, tax_year) %>%
  mutate(dup = case_when(
    n == 1 ~ 0,
    n > 1 ~ 1)) %>%
  group_by(ein, tax_year) %>%
  select(ein, tax_year, dup)

# Adding them to the joint data
joindf_dups <- inner_join(dirty_data, dups, by = c("ein", "tax_year"))
#--------------------------
#--------------------------
# Cleaning some relevant variables and adding new ones
#-------------------------- 
# When Total Functional Expenses = NA, drop. There are 917 such NAs.
# When Total Foreign Expenses OR Grants = NA, assume = 0.
# Assume all negative values are positives with a wrong side. Recode *(-1)
# Dropping all cases when Functional Expenses = 0.
# Keeping only the most recent tax filing for each tax year, according to the return time stamp.
# The analysis will be in constant (real) 2013 dollars.
#     CPIs from BLS: (https://www.bls.gov/cpi/tables/supplemental-files/historical-cpi-u-202203.pdf)
#     Method from making it current dollars (https://www.bls.gov/cpi/factsheets/cpi-math-calculations.pdf)
# Adding the year marriage equality was passed in each state.
#     Source: https://www.lgbtmap.org/equality-maps/marriage_relationship_laws

frgnxpns <- joindf_dups %>%
  mutate(
    clean_totalExpenses = case_when(
      pt9_totalFnctnlExpns < 0 ~ pt9_totalFnctnlExpns*(-1),
      TRUE ~ pt9_totalFnctnlExpns
    ),
    clean_totalFrgnGrnts = case_when(
      pt9_totalFrgnGrnts < 0 ~ pt9_totalFrgnGrnts*(-1),
      is.na(pt9_totalFrgnGrnts) == TRUE ~ 0,
      TRUE ~ pt9_totalFrgnGrnts
    ),
    clean_totalFrgnSrvs = case_when(
      pt9_prgrmSrcvsAmtFrgnGrnts < 0 ~ pt9_prgrmSrcvsAmtFrgnGrnts*(-1),
      is.na(pt9_prgrmSrcvsAmtFrgnGrnts) == TRUE ~ 0,
      TRUE ~ pt9_prgrmSrcvsAmtFrgnGrnts
    )) %>%
  mutate(clean_FrgnExpnsPctg = clean_totalFrgnGrnts/clean_totalExpenses,
         clean_FrgnExpnsPctg = case_when(
           is.na(clean_FrgnExpnsPctg) == TRUE ~ 0,
           TRUE ~ clean_FrgnExpnsPctg
         )) %>%
  arrange(ein, tax_year) %>%
  filter(!is.na(clean_totalExpenses),
         clean_totalExpenses>0) %>%
  select(
    ein, object_id, tax_year, name,
    dup, anti_lgbtq, anti_factor, pt0_state, starts_with("rtrn"), starts_with("clean")
  ) %>%
  rename(
    totalXpns = clean_totalExpenses,
    frgnXpns = clean_totalFrgnGrnts,
    frgnSrvcs = clean_totalFrgnSrvs,
    pctgFrgnXpns = clean_FrgnExpnsPctg
  )
#-----
#-----
# ANTI-LBTQ+ sample: New vars
#-----
anti_frgnxpns <- frgnxpns %>%
  filter(anti_lgbtq == 1) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 =
      case_when(
        tax_year == 2013 ~ totalXpns/(232.957/232.957),
        tax_year == 2014 ~ totalXpns/(232.957/236.736),
        tax_year == 2015 ~ totalXpns/(232.957/237.017),
        tax_year == 2016 ~ totalXpns/(232.957/240.007),
        tax_year == 2017 ~ totalXpns/(232.957/245.120),
        tax_year == 2018 ~ totalXpns/(232.957/251.107),
        tax_year == 2019 ~ totalXpns/(232.957/255.657),
        tax_year == 2020 ~ totalXpns/(232.957/258.811)
      ),
    frgnXpns_2013 =
      case_when(
        tax_year == 2013 ~ frgnXpns/(232.957/232.957),
        tax_year == 2014 ~ frgnXpns/(232.957/236.736),
        tax_year == 2015 ~ frgnXpns/(232.957/237.017),
        tax_year == 2016 ~ frgnXpns/(232.957/240.007),
        tax_year == 2017 ~ frgnXpns/(232.957/245.120),
        tax_year == 2018 ~ frgnXpns/(232.957/251.107),
        tax_year == 2019 ~ frgnXpns/(232.957/255.657),
        tax_year == 2020 ~ frgnXpns/(232.957/258.811)
      ),
    yearMrgEq_rtrn = case_when(
      rtrn_state == "AL" ~ 2015,
      rtrn_state == "AK" ~ 2014,
      rtrn_state == "AZ" ~ 2014,
      rtrn_state == "AR" ~ 2015,
      rtrn_state == "AS" ~ 2014,
      rtrn_state == "CA" ~ 2013,
      rtrn_state == "CO" ~ 2014,
      rtrn_state == "CT" ~ 2008,
      rtrn_state == "DE" ~ 2013,
      rtrn_state == "DC" ~ 2010,
      rtrn_state == "FL" ~ 2015,
      rtrn_state == "GA" ~ 2015,
      rtrn_state == "GU" ~ 2015,
      rtrn_state == "HI" ~ 2013,
      rtrn_state == "ID" ~ 2014,
      rtrn_state == "IL" ~ 2013,
      rtrn_state == "IN" ~ 2014,
      rtrn_state == "IA" ~ 2009,
      rtrn_state == "KS" ~ 2015,
      rtrn_state == "KY" ~ 2015,
      rtrn_state == "LA" ~ 2015,
      rtrn_state == "ME" ~ 2012,
      rtrn_state == "MD" ~ 2012,
      rtrn_state == "MA" ~ 2004,
      rtrn_state == "MI" ~ 2015,
      rtrn_state == "MN" ~ 2013,
      rtrn_state == "MS" ~ 2015,
      rtrn_state == "MO" ~ 2015,
      rtrn_state == "MT" ~ 2014,
      rtrn_state == "NE" ~ 2015,
      rtrn_state == "NV" ~ 2014,
      rtrn_state == "NH" ~ 2010,
      rtrn_state == "NJ" ~ 2013,
      rtrn_state == "NM" ~ 2013,
      rtrn_state == "NY" ~ 2011,
      rtrn_state == "NC" ~ 2014,
      rtrn_state == "ND" ~ 2015,
      rtrn_state == "CM" ~ 2015,
      rtrn_state == "OH" ~ 2015,
      rtrn_state == "OK" ~ 2014,
      rtrn_state == "OR" ~ 2014,
      rtrn_state == "PA" ~ 2014,
      rtrn_state == "PR" ~ 2015,
      rtrn_state == "RI" ~ 2013,
      rtrn_state == "SC" ~ 2014,
      rtrn_state == "SD" ~ 2015,
      rtrn_state == "TN" ~ 2015,
      rtrn_state == "TX" ~ 2015,
      rtrn_state == "UT" ~ 2015,
      rtrn_state == "VT" ~ 2014,
      rtrn_state == "VA" ~ 2009,
      rtrn_state == "VI" ~ 2014,
      rtrn_state == "WA" ~ 2012,
      rtrn_state == "WV" ~ 2014,
      rtrn_state == "WI" ~ 2014,
      rtrn_state == "WY" ~ 2014
    ),
    yearMrgEq_pt0 = case_when(
      pt0_state == "AL" ~ 2015,
      pt0_state == "AK" ~ 2014,
      pt0_state == "AZ" ~ 2014,
      pt0_state == "AR" ~ 2015,
      pt0_state == "AS" ~ 2014,
      pt0_state == "CA" ~ 2013,
      pt0_state == "CO" ~ 2014,
      pt0_state == "CT" ~ 2008,
      pt0_state == "DE" ~ 2013,
      pt0_state == "DC" ~ 2010,
      pt0_state == "FL" ~ 2015,
      pt0_state == "GA" ~ 2015,
      pt0_state == "GU" ~ 2015,
      pt0_state == "HI" ~ 2013,
      pt0_state == "ID" ~ 2014,
      pt0_state == "IL" ~ 2013,
      pt0_state == "IN" ~ 2014,
      pt0_state == "IA" ~ 2009,
      pt0_state == "KS" ~ 2015,
      pt0_state == "KY" ~ 2015,
      pt0_state == "LA" ~ 2015,
      pt0_state == "ME" ~ 2012,
      pt0_state == "MD" ~ 2012,
      pt0_state == "MA" ~ 2004,
      pt0_state == "MI" ~ 2015,
      pt0_state == "MN" ~ 2013,
      pt0_state == "MS" ~ 2015,
      pt0_state == "MO" ~ 2015,
      pt0_state == "MT" ~ 2014,
      pt0_state == "NE" ~ 2015,
      pt0_state == "NV" ~ 2014,
      pt0_state == "NH" ~ 2010,
      pt0_state == "NJ" ~ 2013,
      pt0_state == "NM" ~ 2013,
      pt0_state == "NY" ~ 2011,
      pt0_state == "NC" ~ 2014,
      pt0_state == "ND" ~ 2015,
      pt0_state == "CM" ~ 2015,
      pt0_state == "OH" ~ 2015,
      pt0_state == "OK" ~ 2014,
      pt0_state == "OR" ~ 2014,
      pt0_state == "PA" ~ 2014,
      pt0_state == "PR" ~ 2015,
      pt0_state == "RI" ~ 2013,
      pt0_state == "SC" ~ 2014,
      pt0_state == "SD" ~ 2015,
      pt0_state == "TN" ~ 2015,
      pt0_state == "TX" ~ 2015,
      pt0_state == "UT" ~ 2015,
      pt0_state == "VT" ~ 2014,
      pt0_state == "VA" ~ 2009,
      pt0_state == "VI" ~ 2014,
      pt0_state == "WA" ~ 2012,
      pt0_state == "WV" ~ 2014,
      pt0_state == "WI" ~ 2014,
      pt0_state == "WY" ~ 2014
    )
  ) %>%
  filter(tax_year < 2021)
  
group_by(rtrn_state) %>%
  mutate(
    indicMrgEq_rtrn  = case_when(
      yearMrgEq_rtrn >= tax_year ~ 1,
      FALSE ~ 0
    )
  )
  
#--------
#-----
# NON ANTI-LBTQ+ sample: New vars
#   We'll split the sample into smaller chunks to make computation easier
#-----
nonanti_frgnxpns <- frgnxpns %>%
  filter(anti_lgbtq == 0)
#--------
# WHAT WE SEE BELOW COULD HAVE BEEN A LOOP
#--------
# 2013
#--------
nonanti_frgnxpns2013_1 <- nonanti_frgnxpns %>%
  filter(tax_year == 2013) %>%
  slice(seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/232.957),
    frgnXpns_2013 = frgnXpns/(232.957/232.957))

nonanti_frgnxpns2013_2 <- nonanti_frgnxpns %>%
  filter(tax_year == 2013) %>%
  slice(-seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/232.957),
    frgnXpns_2013 = frgnXpns/(232.957/232.957))
#--------
#--------
# 2014
#--------
nonanti_frgnxpns2014_1 <- nonanti_frgnxpns %>%
  filter(tax_year == 2014) %>%
  slice(seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/236.736),
    frgnXpns_2013 = frgnXpns/(232.957/236.736))

nonanti_frgnxpns2014_2 <- nonanti_frgnxpns %>%
  filter(tax_year == 2014) %>%
  slice(-seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/236.736),
    frgnXpns_2013 = frgnXpns/(232.957/236.736))
#--------
#--------
# 2015
#--------
nonanti_frgnxpns2015_1 <- nonanti_frgnxpns %>%
  filter(tax_year == 2015) %>%
  slice(seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/237.017),
    frgnXpns_2013 = frgnXpns/(232.957/237.017))

nonanti_frgnxpns2015_2 <- nonanti_frgnxpns %>%
  filter(tax_year == 2015) %>%
  slice(-seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/237.017),
    frgnXpns_2013 = frgnXpns/(232.957/237.017))
#--------
#--------
# 2016
#--------
nonanti_frgnxpns2016_1 <- nonanti_frgnxpns %>%
  filter(tax_year == 2016) %>%
  slice(seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/240.007),
    frgnXpns_2013 = frgnXpns/(232.957/240.007))

nonanti_frgnxpns2016_2 <- nonanti_frgnxpns %>%
  filter(tax_year == 2016) %>%
  slice(-seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/240.007),
    frgnXpns_2013 = frgnXpns/(232.957/240.007))
#--------
#--------  
# 2017
#--------
nonanti_frgnxpns2017_1 <- nonanti_frgnxpns %>%
  filter(tax_year == 2017) %>%
  slice(seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/245.120),
    frgnXpns_2013 = frgnXpns/(232.957/245.120))

nonanti_frgnxpns2017_2 <- nonanti_frgnxpns %>%
  filter(tax_year == 2017) %>%
  slice(-seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/245.120),
    frgnXpns_2013 = frgnXpns/(232.957/245.120))
#--------
#--------
# 2018
#--------
nonanti_frgnxpns2018_1 <- nonanti_frgnxpns %>%
  filter(tax_year == 2018) %>%
  slice(seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/251.107),
    frgnXpns_2013 = frgnXpns/(232.957/251.107))

nonanti_frgnxpns2018_2 <- nonanti_frgnxpns %>%
  filter(tax_year == 2018) %>%
  slice(-seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/251.107),
    frgnXpns_2013 = frgnXpns/(232.957/251.107))
#--------
#--------
# 2019
#--------
nonanti_frgnxpns2019_1 <- nonanti_frgnxpns %>%
  filter(tax_year == 2019) %>%
  slice(seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/255.657),
    frgnXpns_2013 = frgnXpns/(232.957/255.657))

nonanti_frgnxpns2019_2 <- nonanti_frgnxpns %>%
  filter(tax_year == 2019) %>%
  slice(-seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/255.657),
    frgnXpns_2013 = frgnXpns/(232.957/255.657))
#--------
#--------
# 2020
#--------
nonanti_frgnxpns2020_1 <- nonanti_frgnxpns %>%
  filter(tax_year == 2020) %>%
  slice(seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/258.811),
    frgnXpns_2013 = frgnXpns/(232.957/258.811))

nonanti_frgnxpns2020_2 <- nonanti_frgnxpns %>%
  filter(tax_year == 2020) %>%
  slice(-seq(0.5 * n())) %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 = totalXpns/(232.957/258.811),
    frgnXpns_2013 = frgnXpns/(232.957/258.811))
#--------

nonanti_frgnxpns2 <- bind_rows(
  nonanti_frgnxpns2013_1,
  nonanti_frgnxpns2013_2,
  nonanti_frgnxpns2014_1,
  nonanti_frgnxpns2014_2,
  nonanti_frgnxpns2015_1,
  nonanti_frgnxpns2015_2,
  nonanti_frgnxpns2016_1,
  nonanti_frgnxpns2016_2,
  nonanti_frgnxpns2017_1,
  nonanti_frgnxpns2017_2,
  nonanti_frgnxpns2018_1,
  nonanti_frgnxpns2018_2,
  nonanti_frgnxpns2019_1,
  nonanti_frgnxpns2019_2,
  nonanti_frgnxpns2020_1,
  nonanti_frgnxpns2020_2
)

#   how to calculate real 2013 dollars:
#   (money in year X)/((CPI in 2013)/(CPI in that year))
#   CPIs from BLS: (https://www.bls.gov/cpi/tables/supplemental-files/historical-cpi-u-202203.pdf)
#   Method from making it current dollars (https://www.bls.gov/cpi/factsheets/cpi-math-calculations.pdf)
#   2013 = 232.957
#   2014 = 236.736
#   2015 = 237.017
#   2016 = 240.007
#   2017 = 245.120
#   2018 = 251.107
#   2019 = 255.657
#   2020 = 258.811
#--------------------------
#--------------------------
# Exporting the data
#-------------------------- 
write_csv(anti_frgnxpns, "/Users/srojascabal/Desktop/000_f990_data/anti_sample_220601.csv")
write_csv(nonanti_frgnxpns2, "/Users/srojascabal/Desktop/000_f990_data/nonanti_sample_220601.csv")
#--------------------------