## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file joins the nonprofit data with the state-level
#   controls to create the analytical sample for the
#   preliminary models.

## Last updated: July 29th by Sebastian Rojas Cabal
#--------------------------------------------------------
#--------------------------------------------------------
# Loading packages
#--------------------------------------------------------
library(tidyverse)
library(DescTools)
#--------------------------------------------------------
#--------------------------------------------------------
# Importing data
#--------------------------------------------------------
nonprofits_anti <- read_csv("/Users/srojascabal/Desktop/000_f990_data/anti_sample_220727.csv",
      col_types = cols(
        ein = col_character(),
        tax_year = col_character(),
        anti_lgbtq = col_double(),
        anti_factor = col_factor(),
        rtrn_state = col_character(),
        totalXpns_2013_100k = col_double(),
        frgnXpns_2013_100k = col_double(),
        propFrgnXpns_2013_100k = col_double(),
        totalXpns_2012_100k = col_double(),
        frgnXpns_2012_100k = col_double(),
        propFrgnXpns_2012_100k = col_double(),
        yearMrgEq_rtrn = col_double(),
        ind_yearMrgEq_rtrn = col_double())
      ) %>%
  select(
    ein, tax_year, anti_lgbtq, anti_factor,
    rtrn_state, totalXpns_2013_100k, frgnXpns_2013_100k,
    propFrgnXpns_2013_100k, totalXpns_2012_100k, frgnXpns_2012_100k,
    propFrgnXpns_2012_100k, yearMrgEq_rtrn, ind_yearMrgEq_rtrn
  ) %>%
  filter(
    !rtrn_state %in% c("PR", "DC"),
    complete.cases(rtrn_state)
  ) 

# July 03 data
#nonprofits_anti <- read_csv("/Users/srojascabal/Desktop/000_f990_data/anti_sample_220703.csv",
#                            col_types = cols(
#                              ein = col_character(),
#                              tax_year = col_character(),
#                              anti_lgbtq = col_double(),
#                              anti_factor = col_factor(),
#                              rtrn_state = col_character(),
#                              totalXpns_2013_100k = col_double(),
#                              frgnXpns_2013_100k = col_double(),
#                              propFrgnXpns_2013_100k = col_double(),
#                              yearMrgEq_rtrn = col_double(),
#                              ind_yearMrgEq_rtrn = col_double())
#) %>%
#  select(
#    ein, tax_year, anti_lgbtq, anti_factor,
#    rtrn_state, totalXpns_2013_100k, frgnXpns_2013_100k,
#    propFrgnXpns_2013_100k, yearMrgEq_rtrn, ind_yearMrgEq_rtrn
#  ) %>%
#  filter(
#    !rtrn_state %in% c("PR", "DC"),
#    complete.cases(rtrn_state)
#  )

na_npanti_total <- nonprofits_anti %>%
     summarise(across(everything(), ~ sum(is.na(.))))

nonprofits_nonanti <- read_csv("/Users/srojascabal/Desktop/000_f990_data/nonanti_sample_220727.csv",
    col_types = cols(
      ein = col_character(),
      tax_year = col_character(),
      anti_lgbtq = col_double(),
      anti_factor = col_factor(),
      rtrn_state = col_character(),
      totalXpns_2013_100k = col_double(),
      frgnXpns_2013_100k = col_double(),
      propFrgnXpns_2013_100k = col_double(),
      totalXpns_2012_100k = col_double(),
      frgnXpns_2012_100k = col_double(),
      propFrgnXpns_2012_100k = col_double(),
      yearMrgEq_rtrn = col_double(),
      ind_yearMrgEq_rtrn = col_double())
) %>%
  select(
    ein, tax_year, anti_lgbtq, anti_factor,
    rtrn_state, totalXpns_2013_100k, frgnXpns_2013_100k,
    propFrgnXpns_2013_100k, totalXpns_2012_100k, frgnXpns_2012_100k,
    propFrgnXpns_2012_100k, yearMrgEq_rtrn, ind_yearMrgEq_rtrn
  ) %>%
  filter(
    !rtrn_state %in% c("PR", "DC", "GU",
                       "MP", "AS", "AP",
                       "AE", "VI"),
    complete.cases(rtrn_state)
  )

# July 03, 2022
# nonprofits_nonanti <- read_csv("/Users/srojascabal/Desktop/000_f990_data/nonanti_sample_220703.csv",
#                                col_types = cols(
#                                  ein = col_character(),
#                                  tax_year = col_character(),
#                                  anti_lgbtq = col_double(),
#                                  anti_factor = col_factor(),
#                                  rtrn_state = col_character(),
#                                  totalXpns_2013_100k = col_double(),
#                                  frgnXpns_2013_100k = col_double(),
#                                  propFrgnXpns_2013_100k = col_double(),
#                                  yearMrgEq_rtrn = col_double(),
#                                  ind_yearMrgEq_rtrn = col_double())
# ) %>%
#   select(
#     ein, tax_year, anti_lgbtq, anti_factor,
#     rtrn_state, totalXpns_2013_100k, frgnXpns_2013_100k,
#     propFrgnXpns_2013_100k, yearMrgEq_rtrn, ind_yearMrgEq_rtrn
#   ) %>%
#   filter(
#     !rtrn_state %in% c("PR", "DC", "GU",
#                        "MP", "AS", "AP",
#                        "AE", "VI"),
#     complete.cases(rtrn_state)
#   ) 
  
  na_npnonanti_total <- nonprofits_nonanti %>%
    summarise(across(everything(), ~ sum(is.na(.))))

#  states_anti <- data.frame(
#    states = pull(nonprofits_anti, rtrn_state),
#    df_name = "anti"
#  ) %>%
#    distinct()
#  
#  states_nonanti <- data.frame(
#    states = pull(nonprofits_nonanti, rtrn_state),
#    df_name = "nonanti"
#  ) %>%
#    distinct()
#  
#  states_to_filter_nonanti <- anti_join(states_nonanti, states_anti, by = c("states"))

state_controls <- read_csv("/Users/srojascabal/Desktop/000_f990_data/state_controls_0726.csv",
    col_types = cols(
      tax_year = col_character(),
      state_code = col_character(),
      college_educ_over_25 = col_double(),
      frgn_born = col_double(),
      state_population = col_double(),
      exempt_orgs = col_double(),
      rel_orgs = col_double(),
      gdp_state_2012 = col_double(),
      gdp_state_2012_100k = col_double(),
      gov_party = col_character(),
      gov_republican = col_double()
    )) %>%
  rename(
    rtrn_state = state_code
  )

 na_controls_total <- state_controls %>%
   summarise(across(everything(), ~ sum(is.na(.))))
#--------------------------------------------------------
#--------------------------------------------------------
# Aggregating data
#--------------------------------------------------------
nonprofits <- bind_rows(nonprofits_anti, nonprofits_nonanti) %>%
   filter(tax_year != "2020")

nonprofits_analysis <- left_join(
  nonprofits, state_controls,
  by = c("tax_year", "rtrn_state")
)

na_analysis_total <- nonprofits_analysis %>%
  summarise(across(everything(), ~ sum(is.na(.))))
#--------------------------------------------------------
# New Variables
#   Indicator of whether or not the org spent money abroad that year
#   Winsorized Total and Foreign Expenses (to the 95th and 99th percentile)
#   Logged State GDP
#   Logged Foreign Expenses
#   Logged Total Expenses
#   Logged # of Religious Organizations by State
#   Logged # of Tax Exempt Organizations by State
#--------------------------------------------------------
nonprofits_analysis <- nonprofits_analysis %>%
  mutate(
    spends_abroad = case_when(
      frgnXpns_2012_100k > 0 ~ 1,
      frgnXpns_2012_100k == 0 ~ 0
    ),
    winsor_totalXpns_2012_99 = Winsorize(totalXpns_2012_100k, probs=c(0.00, 0.99)),
    winsor_totalXpns_2012_95 = Winsorize(totalXpns_2012_100k, probs=c(0.00, 0.95)),
    winsor_frgnXpns_2012_99 = Winsorize(frgnXpns_2012_100k, probs=c(0.00, 0.99)),
    winsor_frgnXpns_2012_95 = Winsorize(frgnXpns_2012_100k, probs=c(0.00, 0.95)),
    log_gdp_2012 = log(gdp_state_2012),
    log_gdp_2012_100k = log(gdp_state_2012_100k),
    log_frgnXpns_2012_100k_95 = log(winsor_frgnXpns_2012_95+1),
    log_frgnXpns_2012_100k_99 = log(winsor_frgnXpns_2012_99+1),
    log_totalXpns_2012_100k_95 = log(winsor_totalXpns_2012_95+1),
    log_totalXpns_2012_100k_99 = log(winsor_totalXpns_2012_99+1),
    log_rel_orgs = log(rel_orgs +1),
    log_exempt_orgs = log(exempt_orgs+1)
  )

na_newvars_total <- nonprofits_analysis %>%
  summarise(across(everything(), ~ sum(is.na(.))))
#--------------------------------------------------------
# Export
write_csv(nonprofits_analysis, "/Users/srojascabal/Desktop/000_f990_data/analytical_sample_220729.csv")
#--------------------------------------------------------

eins_current <- nonprofits_analysis %>%
  select(ein, anti_lgbtq) %>%
  distinct(ein)

write_csv(eins_current, "/Users/srojascabal/Desktop/000_f990_data/eins_analyticalsample_220714.csv")
