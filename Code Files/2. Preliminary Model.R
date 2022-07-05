## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file joins the nonprofit data with the state-level
#   control and implements the preliminary model.

## Last updated: July 5th by Sebastian Rojas Cabal
#--------------------------------------------------------
#--------------------------------------------------------
# Loading packages
#--------------------------------------------------------
library(tidyverse)
#--------------------------------------------------------
#--------------------------------------------------------
# Importing data
#--------------------------------------------------------
nonprofits_anti <- read_csv("/Users/srojascabal/Desktop/000_f990_data/anti_sample_220703.csv",
      col_types = cols(
        ein = col_character(),
        tax_year = col_character(),
        anti_lgbtq = col_double(),
        anti_factor = col_factor(),
        rtrn_state = col_character(),
        totalXpns_2013_100k = col_double(),
        frgnXpns_2013_100k = col_double(),
        propFrgnXpns_2013_100k = col_double(),
        ind_yearMrgEq_rtrn = col_double())
      ) %>%
  select(
    ein, tax_year, anti_lgbtq, anti_factor,
    rtrn_state, totalXpns_2013_100k, frgnXpns_2013_100k,
    propFrgnXpns_2013_100k, ind_yearMrgEq_rtrn
  )

# Excluding DC and PR
anti_states <- nonprofits_anti %>%
  select(rtrn_state) %>%
  distinct() %>%
  filter(
    !rtrn_state %in% c("PR", "DC")
  ) %>%
  pull()
  
nonprofits_nonanti <- read_csv("/Users/srojascabal/Desktop/000_f990_data/nonanti_sample_220703.csv",
    col_types = cols(
      ein = col_character(),
      tax_year = col_character(),
      anti_lgbtq = col_double(),
      anti_factor = col_factor(),
      rtrn_state = col_character(),
      totalXpns_2013_100k = col_double(),
      frgnXpns_2013_100k = col_double(),
      propFrgnXpns_2013_100k = col_double(),
      ind_yearMrgEq_rtrn = col_double())
  ) %>%
    select(
      ein, tax_year, anti_lgbtq, anti_factor,
      rtrn_state, totalXpns_2013_100k, frgnXpns_2013_100k,
      propFrgnXpns_2013_100k, ind_yearMrgEq_rtrn
    ) %>%
  filter(
    !rtrn_state %in% c("PR", "DC")
  )

state_controls <- read_csv("/Users/srojascabal/Desktop/000_f990_data/state_controls.csv",
    col_types = cols(
      gov_party = col_character(),
      state_code = col_character(),
      tax_year = col_character(),
      gdp_state_2012 = col_double(),
      churches = col_double(),
      F990_990EZ = col_double(),    
      rel_orgs = col_double() 
      )) %>%
    mutate(
      state_religiosity = churches + rel_orgs
    ) %>%
    rename(
      rtrn_state = state_code
    ) %>%
    select(-churches, -rel_orgs) %>%
    filter(
      !tax_year %in% c("2013", "2014", "2015",
                       "2016", "2017", "2018",
                       "2019", "2020")
    )
#--------------------------------------------------------
#--------------------------------------------------------
# Aggregating data
#--------------------------------------------------------
nonprofits <- bind_rows(nonprofits_anti, nonprofits_nonanti)

nonprofits_analysis <- left_join(
  nonprofits, state_controls,
  by = c("tax_year", "rtrn_state")
) %>%
  mutate(
    log_frgnXpns_2013_100k = log(frgnXpns_2013_100k)
  )

# Export
write_csv(nonprofits_analysis, "/Users/srojascabal/Desktop/000_f990_data/analytical_sample_220705.csv")
#--------------------------------------------------------
#--------------------------------------------------------
# Models without contorls or FX
#--------------------------------------------------------

# Foreign Expenses
model1 <- lm(frgnXpns_2013_100k ~ anti_lgbtq,
             nonprofits_analysis)

summary(model1)

# Logged Foreign Expenses
model2 <- lm(log_frgnXpns_2013_100k ~ anti_lgbtq,
             nonprofits_analysis)

summary(model2)

# % of Total Expenditures
model3 <- lm(propFrgnXpns_2013_100k ~ anti_lgbtq,
             nonprofits_analysis)

summary(model3)

#--------------------------------------------------------