## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file cleans the dirty data set created in
#   1. Data Aggregation_Form 990 and anti LGBTQ indicator

## Last updated: March 9th, 2023 by Sebastian Rojas Cabal
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
dirty_data <- read_csv("/Volumes/SRC_DATA/000_f990_data/230309_form990_rtrn09_anti_dirty.csv") %>%
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

dup_data <- joindf_dups %>%
  select(
    ein, tax_year, rtrn_timestmp, dup
  ) %>%
  arrange(
    ein, tax_year, rtrn_timestmp, dup
  ) %>%
  filter(
    dup == 1
  )

#sliced_min_dup <- dup_data %>%
#  group_by(ein, tax_year)%>%
#  slice_min(rtrn_timestmp)
#
#max_money <- dirty_data %>%
#  filter(pt9_totalFrgnGrnts == 4160760803)
#--------------------------
#--------------------------
# Cleaning some relevant variables and adding new ones
#-------------------------- 
# When Total Functional Expenses = NA, drop. There are 917 such NAs.
# When Total Foreign Expenses OR Grants = NA, assume = 0.
# Dropping all cases when Functional Expenses >= 0
# Keeping only the most recent tax filing for each tax year, according to the return time stamp (slice_max).
# The analysis will be in constant (real) 2013 dollars.
#     CPIs from BLS: (https://www.bls.gov/cpi/tables/supplemental-files/historical-cpi-u-202203.pdf)
#     Method from making it current dollars (https://www.bls.gov/cpi/factsheets/cpi-math-calculations.pdf)
# Adding the year marriage equality was passed in each state.
#     Source: https://www.lgbtmap.org/equality-maps/marriage_relationship_laws

frgnxpns <- joindf_dups %>%
  mutate(
    clean_totalExpenses = pt9_totalFnctnlExpns,
    clean_totalFrgnGrnts = case_when(
      is.na(pt9_totalFrgnGrnts) == TRUE ~ 0,
      TRUE ~ pt9_totalFrgnGrnts
    ),
    clean_totalFrgnSrvcs = case_when(
      is.na(pt9_prgrmSrcvsAmtFrgnGrnts) == TRUE ~ 0,
      TRUE ~ pt9_prgrmSrcvsAmtFrgnGrnts
    )) %>%
  filter(!is.na(clean_totalExpenses),
         clean_totalExpenses >= 0,
         clean_totalFrgnGrnts >= 0,
         clean_totalFrgnSrvcs >= 0) %>%
  mutate(clean_totalExpenses_100k = clean_totalExpenses/100000,
         clean_totalFrgnGrnts_100k = clean_totalFrgnGrnts/100000,
         clean_totalFrgnSrvcs_100k = clean_totalFrgnSrvcs/100000) %>%
  arrange(ein, tax_year) %>%
  select(
    ein, object_id, tax_year, name,
    dup, anti_lgbtq, anti_factor, pt0_state, starts_with("rtrn"), starts_with("clean")
  ) %>%
  rename(
    totalXpns = clean_totalExpenses,
    frgnXpns = clean_totalFrgnGrnts,
    frgnSrvcs = clean_totalFrgnSrvcs
  )
  
#-----
#-----
# ANTI-LBTQ+ sample: New vars
#-----
anti_frgnxpns_clean <- frgnxpns %>%
  filter(anti_lgbtq == 1) %>%
  group_by(ein, tax_year) %>%
  slice_max(rtrn_timestmp) %>%
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
    totalXpns_2012 =
      case_when(
        tax_year == 2013 ~ totalXpns/(229.594/232.957),
        tax_year == 2014 ~ totalXpns/(229.594/236.736),
        tax_year == 2015 ~ totalXpns/(229.594/237.017),
        tax_year == 2016 ~ totalXpns/(229.594/240.007),
        tax_year == 2017 ~ totalXpns/(229.594/245.120),
        tax_year == 2018 ~ totalXpns/(229.594/251.107),
        tax_year == 2019 ~ totalXpns/(229.594/255.657),
        tax_year == 2020 ~ totalXpns/(229.594/258.811)
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
    frgnXpns_2012 =
      case_when(
        tax_year == 2013 ~ frgnXpns/(229.594/232.957),
        tax_year == 2014 ~ frgnXpns/(229.594/236.736),
        tax_year == 2015 ~ frgnXpns/(229.594/237.017),
        tax_year == 2016 ~ frgnXpns/(229.594/240.007),
        tax_year == 2017 ~ frgnXpns/(229.594/245.120),
        tax_year == 2018 ~ frgnXpns/(229.594/251.107),
        tax_year == 2019 ~ frgnXpns/(229.594/255.657),
        tax_year == 2020 ~ frgnXpns/(229.594/258.811)
      ),
    frgnSrvcs_2013 = 
      case_when(
        tax_year == 2013 ~ frgnSrvcs/(232.957/232.957),
        tax_year == 2014 ~ frgnSrvcs/(232.957/236.736),
        tax_year == 2015 ~ frgnSrvcs/(232.957/237.017),
        tax_year == 2016 ~ frgnSrvcs/(232.957/240.007),
        tax_year == 2017 ~ frgnSrvcs/(232.957/245.120),
        tax_year == 2018 ~ frgnSrvcs/(232.957/251.107),
        tax_year == 2019 ~ frgnSrvcs/(232.957/255.657),
        tax_year == 2020 ~ frgnSrvcs/(232.957/258.811)
      ),
    frgnSrvcs_2012 = 
      case_when(
        tax_year == 2013 ~ frgnSrvcs/(229.594/232.957),
        tax_year == 2014 ~ frgnSrvcs/(229.594/236.736),
        tax_year == 2015 ~ frgnSrvcs/(229.594/237.017),
        tax_year == 2016 ~ frgnSrvcs/(229.594/240.007),
        tax_year == 2017 ~ frgnSrvcs/(229.594/245.120),
        tax_year == 2018 ~ frgnSrvcs/(229.594/251.107),
        tax_year == 2019 ~ frgnSrvcs/(229.594/255.657),
        tax_year == 2020 ~ frgnSrvcs/(229.594/258.811)
      ),
    totalXpns_2013_100k = totalXpns_2013/100000,
    frgnXpns_2013_100k = frgnXpns_2013/100000,
    frgnSrvcs_2013_100k = frgnSrvcs_2013/100000,
    totalXpns_2012_100k = totalXpns_2012/100000,
    frgnXpns_2012_100k = frgnXpns_2012/100000,
    frgnSrvcs_2012_100k = frgnSrvcs_2012/100000,
    propFrgnXpns = frgnXpns/totalXpns,
    propFrgnXpns = case_when(
      is.na(propFrgnXpns) == TRUE ~ 0,
      TRUE ~ propFrgnXpns
    ),    
    propFrgnXpns_2013 = frgnXpns_2013/totalXpns_2013,
    propFrgnXpns_2013 = case_when(
      is.na(propFrgnXpns_2013) == TRUE ~ 0,
      TRUE ~ propFrgnXpns_2013
    ),
    propFrgnXpns_2013_100k = frgnXpns_2013_100k/totalXpns_2013_100k,
    propFrgnXpns_2013_100k = case_when(
      is.na(propFrgnXpns_2013_100k) == TRUE ~ 0,
      TRUE ~ propFrgnXpns_2013_100k
    ),
    propFrgnXpns_2012 = frgnXpns_2012/totalXpns_2012,
    propFrgnXpns_2012 = case_when(
      is.na(propFrgnXpns_2012) == TRUE ~ 0,
      TRUE ~ propFrgnXpns_2012
    ),
    propFrgnXpns_2012_100k = frgnXpns_2012_100k/totalXpns_2012_100k,
    propFrgnXpns_2012_100k = case_when(
      is.na(propFrgnXpns_2012_100k) == TRUE ~ 0,
      TRUE ~ propFrgnXpns_2012_100k
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
    ),
    ind_yearMrgEq_rtrn  = case_when(
      tax_year < yearMrgEq_rtrn ~ 0,
      TRUE ~ 1
    ),
    ind_yearMrgEq_pt0  = case_when(
      tax_year < yearMrgEq_pt0 ~ 0,
      TRUE ~ 1
    ),
  ) %>%
  filter(propFrgnXpns_2013 < 1.02,
         tax_year < 2021) %>%
  ungroup()
#----
#-----
# NON ANTI-LBTQ+ sample: New vars
#   We'll split the sample into smaller chunks to make computation easier
#-----
nonanti_frgnxpns <- frgnxpns %>%
  filter(anti_lgbtq == 0)

tax_year_vector <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
cpi_vector <- c(232.957, 236.736, 237.017, 240.007, 245.120, 251.107, 255.657, 258.811)

for (i in 1:length(tax_year_vector)) {

# Subsetting non-anti data by year, adding and cleaning variables
#----  
  df_subset <- nonanti_frgnxpns %>%
    filter(tax_year == tax_year_vector[i]) %>%
    group_by(ein, tax_year) %>%
    slice_max(rtrn_timestmp) %>%
    mutate(
    totalXpns_2013 = totalXpns/(232.957/cpi_vector[i]),
    frgnXpns_2013 = frgnXpns/(232.957/cpi_vector[i]),
    frgnSrvcs_2013 = frgnSrvcs/(232.957/cpi_vector[i]),
    totalXpns_2012 = totalXpns/(229.594/cpi_vector[i]),
    frgnXpns_2012 = frgnXpns/(229.594/cpi_vector[i]),
    frgnSrvcs_2012 = frgnSrvcs/(229.594/cpi_vector[i]),
    totalXpns_2013_100k = totalXpns_2013/100000,
    frgnXpns_2013_100k = frgnXpns_2013/100000,
    frgnSrvcs_2013_100k = frgnSrvcs_2013/100000,
    totalXpns_2012_100k = totalXpns_2012/100000,
    frgnXpns_2012_100k = frgnXpns_2012/100000,
    frgnSrvcs_2012_100k = frgnSrvcs_2012/100000,
    propFrgnXpns = frgnXpns/totalXpns,
    propFrgnXpns = case_when(
      is.na(propFrgnXpns) == TRUE ~ 0,
      TRUE ~ propFrgnXpns
    ),    
    propFrgnXpns_2013 = frgnXpns_2013/totalXpns_2013,
    propFrgnXpns_2013 = case_when(
      is.na(propFrgnXpns_2013) == TRUE ~ 0,
      TRUE ~ propFrgnXpns_2013
    ),
    propFrgnXpns_2013_100k = frgnXpns_2013_100k/totalXpns_2013_100k,
    propFrgnXpns_2013_100k = case_when(
      is.na(propFrgnXpns_2013_100k) == TRUE ~ 0,
      TRUE ~ propFrgnXpns_2013_100k
    ),
    propFrgnXpns_2012 = frgnXpns_2012/totalXpns_2012,
    propFrgnXpns_2012 = case_when(
      is.na(propFrgnXpns_2012) == TRUE ~ 0,
      TRUE ~ propFrgnXpns_2012
    ),
    propFrgnXpns_2012_100k = frgnXpns_2012_100k/totalXpns_2012_100k,
    propFrgnXpns_2012_100k = case_when(
      is.na(propFrgnXpns_2012_100k) == TRUE ~ 0,
      TRUE ~ propFrgnXpns_2012_100k
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
      ),
    ind_yearMrgEq_rtrn  = case_when(
      tax_year < yearMrgEq_rtrn ~ 0,
      TRUE ~ 1
      ),
    ind_yearMrgEq_pt0  = case_when(
      tax_year < yearMrgEq_pt0 ~ 0,
      TRUE ~ 1
      ),
  ) %>%
   filter(propFrgnXpns_2013 < 1.02)
#----
  
  # Name of the year-specific subset for saving in global environment  
  name_df <- paste0("subset_nonanti_", tax_year_vector[i])
  
  # Saving the year-specific subset to the global env
  assign(name_df, df_subset, envir=globalenv())
}

# Binding all the non-anti data frames together
nonanti_frgnxpns_clean <- mget(ls(pattern="^subset_nonanti_")) %>%
  bind_rows()

# summary(nonanti_frgnxpns_clean)

#--------------------------
# Exporting the data
#-------------------------- 
# July 3, 2022 data
#write_csv(anti_frgnxpns_clean, "/Users/srojascabal/Desktop/000_f990_data/anti_sample_220703.csv")
#write_csv(nonanti_frgnxpns_clean, "/Users/srojascabal/Desktop/000_f990_data/nonanti_sample_220703.csv")
# July 27, 2022 data
write_csv(anti_frgnxpns_clean, "/Volumes/SRC_DATA/000_f990_data/230309_anti_sample_220727.csv")
write_csv(nonanti_frgnxpns_clean, "/Volumes/SRC_DATA/000_f990_data/230309_nonanti_sample_220727.csv")
#--------------------------