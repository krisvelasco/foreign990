## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file joins the nonprofit data with the state-level
#   controls to create the analytical sample for the
#   preliminary models.

## Last updated: July 25th by Sebastian Rojas Cabal
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
        yearMrgEq_rtrn = col_double(),
        ind_yearMrgEq_rtrn = col_double())
      ) %>%
  select(
    ein, tax_year, anti_lgbtq, anti_factor,
    rtrn_state, totalXpns_2013_100k, frgnXpns_2013_100k,
    propFrgnXpns_2013_100k, yearMrgEq_rtrn, ind_yearMrgEq_rtrn
  ) %>%
  filter(
    !rtrn_state %in% c("PR", "DC"),
    complete.cases(rtrn_state)
  ) 

#   na_df_total <- nonprofits_anti %>%
#     summarise(across(everything(), ~ sum(is.na(.))))

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
      yearMrgEq_rtrn = col_double(),
      ind_yearMrgEq_rtrn = col_double())
  ) %>%
    select(
      ein, tax_year, anti_lgbtq, anti_factor,
      rtrn_state, totalXpns_2013_100k, frgnXpns_2013_100k,
      propFrgnXpns_2013_100k, yearMrgEq_rtrn, ind_yearMrgEq_rtrn
    ) %>%
  filter(
    !rtrn_state %in% c("PR", "DC", "GU",
                       "MP", "AS", "AP",
                       "AE", "VI"),
    complete.cases(rtrn_state)
  ) 
  
#  na_df_total <- nonprofits_nonanti %>%
#    summarise(across(everything(), ~ sum(is.na(.))))

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

state_controls <- read_csv("/Users/srojascabal/Desktop/000_f990_data/state_controls_0725.csv",
    col_types = cols(
      tax_year = col_character(),
      state_code = col_character(),
      college_educ_over_25 = col_double(),
      frgn_born = col_double(),
      state_population = col_double(),
      excempt_orgs = col_double(),
      rel_orgs = col_double(),
      gdp_state_2012 = col_double(),
      gdp_state_2012_100k = col_double(),
      gov_party = col_character(),
      gov_republican = col_double()
    )) %>%
  rename(
    rtrn_state = state_code
  )

# na_df_total <- state_controls %>%
#   summarise(across(everything(), ~ sum(is.na(.))))
#--------------------------------------------------------
#--------------------------------------------------------
# Aggregating data
#--------------------------------------------------------
nonprofits <- bind_rows(nonprofits_anti, nonprofits_nonanti)

na_df_total <- data.frame(
  ein = NA,
  tax_year = NA,
  anti_lgbtq = NA,
  anti_factor = NA,
  rtrn_state = NA,
  totalXpns_2013_100k = NA,
  frgnXpns_2013_100k = NA,
  propFrgnXpns_2013_100k = NA,
  yearMrgEq_rtrn = NA,
  ind_yearMrgEq_rtrn = NA,
  college_educ_over_25 = NA,
  frgn_born = NA,
  state_population = NA,
  excempt_orgs = NA,
  rel_orgs = NA,
  gdp_state_2012 = NA,
  gdp_state_2012_100k = NA,
  gov_party = NA,
  gov_republican = NA
)

years <- unique(nonprofits$tax_year)

for (i in 1:length(years)) {

  nonprofits_year <- nonprofits %>%
    filter(tax_year == years[i])
  
  state_controls_year <- state_controls %>%
    filter(tax_year == years[i])
  
  analytical_sample_year <- left_join(nonprofits_year, state_controls_year,
                                      by = c("tax_year", "rtrn_state"))
  
  na_df_year <- analytical_sample_year %>%
         summarise(across(everything(), ~ sum(is.na(.))))
  
  na_df_total <- bind_rows(na_df_total, na_df_year)
  
  df_name <- paste0(years[i], "_sample")
  
  assign(df_name, analytical_sample_year)
}





nonprofits_analysis <- left_join(
  nonprofits, state_controls,
  by = c("tax_year", "rtrn_state")
) %>%
  mutate(
    log_frgnXpns_2013_100k = log(frgnXpns_2013_100k)
  )
#--------------------------------------------------------
# Export
write_csv(nonprofits_analysis, "/Users/srojascabal/Desktop/000_f990_data/analytical_sample_220725.csv")
#--------------------------------------------------------

eins_current <- nonprofits_analysis %>%
  select(ein, anti_lgbtq) %>%
  distinct(ein)

write_csv(eins_current, "/Users/srojascabal/Desktop/000_f990_data/eins_analyticalsample_220714.csv")
