# Computations for ASA Paper

library(tidyverse)
library(readxl)

nonprofits_analysis <- read_csv("/Users/srojascabal/Desktop/000_f990_data/analytical_sample_220729.csv",
                                col_types = cols(
                                  ein = col_character(),
                                  tax_year = col_character(),
                                  anti_lgbtq = col_double(),
                                  anti_factor = col_factor(),
                                  rtrn_state = col_factor(),
                                  totalXpns_2013_100k = col_double(),
                                  frgnXpns_2013_100k = col_double(),
                                  propFrgnXpns_2013_100k = col_double(),
                                  totalXpns_2012_100k = col_double(),
                                  frgnXpns_2012_100k = col_double(),
                                  propFrgnXpns_2012_100k = col_double(),
                                  yearMrgEq_rtrn = col_double(),
                                  ind_yearMrgEq_rtrn = col_double(),
                                  college_educ_over_25 = col_double(),
                                  frgn_born = col_double(),
                                  state_population = col_double(),
                                  exempt_orgs = col_double(),
                                  rel_orgs = col_double(),
                                  gdp_state_2012 = col_double(),
                                  gdp_state_2012_100k = col_double(),
                                  gov_party = col_character(),
                                  gov_republican = col_double(),
                                  gdp_state_2012 = col_double(),
                                  gdp_state_2012_100k = col_double(),
                                  gov_party = col_character(),
                                  gov_republican = col_double(),
                                  spends_abroad = col_double(),
                                  winsor_totalXpns_2012_99 = col_double(),
                                  winsor_totalXpns_2012_95 = col_double(),
                                  winsor_frgnXpns_2012_99 = col_double(),
                                  winsor_frgnXpns_2012_95 = col_double(),
                                  log_gdp_2012 = col_double(),
                                  log_gdp_2012_100k = col_double(),
                                  log_frgnXpns_2012_100k_95 = col_double(),
                                  log_frgnXpns_2012_100k_99 = col_double(),
                                  log_totalXpns_2012_100k_95 = col_double(),
                                  log_totalXpns_2012_100k_99 = col_double(),
                                  log_rel_orgs = col_double(),
                                  log_exempt_orgs = col_double()
                                ))

anti <- nonprofits_analysis %>%
  filter(anti_lgbtq == 1) %>%
  select(ein) %>%
  distinct()

sample1 <- anti_join(orgs2, anti, by = "ein")

# List of known anti-LGBTQ+ nonprofits. 
orgs1 <- read_excel("/Users/srojascabal/Desktop/000_f990_data/anti_lgbtq_eins_20220422.xlsx") %>%
  rename(name = Organization,
         ein_char = ein) %>%
  mutate(ein_char = str_trim(ein_char),
         ein = as.numeric(ein_char)) %>%
  select(name, ein) %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(
    sample = "third-party"
  )
# List of first-order ties to known anti-LGBTQ+ nonprofits.
orgs2 <- read_csv("/Users/srojascabal/Desktop/000_f990_data/anti_lgbtq_candidates_20220516.csv") %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(
    sample = "network"
  )
# Binding all candidate anti-LGBTQ+ orgs
orgs_all <- bind_rows(orgs1, orgs2) %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(anti_lgbtq = 1,
         ein = as.character(ein)) %>%
  select(ein, sample)

orgs_count <- orgs_all %>%
  count(sample)


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

anti <- nonprofits_anti %>%
  select(ein) %>%
  distinct()

nonanti <- nonprofits_analysis %>%
  filter(anti_lgbtq == 0) %>%
  select(ein) %>%
  distinct()

anti2 <- left_join(
  anti, orgs_all,
  by = "ein"
)

anti2_count <- anti2 %>%
  count(sample)

orgsall_count <- nonprofits_analysis %>%
  distinct(ein) %>%
  count(anti_factor)


anti2 <- read_xlsx("/Users/srojascabal/Desktop/000_f990_data/Select Anti-LGBT+ Organizations - Foreign Expenditures.xlsx")
