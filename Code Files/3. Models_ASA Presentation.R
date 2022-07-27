## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file produces diagnostics and other visualizations
#   for the analysis presented in the ASA Annual Meeting.

## Last updated: July 27th by Sebastian Rojas Cabal
#--------------------------------------------------------
#--------------------------------------------------------
# Loading packages
#--------------------------------------------------------
library(tidyverse)
library(DescTools)
library(lmtest)
library(sandwich)
library(stargazer)
#--------------------------------------------------------
#--------------------------------------------------------
# Data Import
#--------------------------------------------------------
nonprofits_analysis <- read_csv("/Users/srojascabal/Desktop/000_f990_data/analytical_sample_220727.csv",
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
                                  gov_republican = col_double()
                                ))
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
    log_frgnXpns_2012_100k_95 = log(frgnXpns_2013_100k+1),
    log_frgnXpns_2012_100k_99 = log(winsor_frgnXpns_2012_95+1),
    log_totalXpns_2012_100k_95 = log(winsor_totalXpns_2012_99+1),
    log_totalXpns_2012_100k_99 = log(winsor_totalXpns_2012_95+1),
    log_rel_orgs = log(rel_orgs +1),
    log_exempt_orgs = log(exempt_orgs+1)
  )
#--------------------------------------------------------
# Models
#--------------------------------------------------------
# Models without Fixed Effects Robust Standard Errors
# Logged Foreign Expenses, no controls
model1 <- lm(log_frgnXpns_2012_100k_95 ~ anti_lgbtq,
             nonprofits_analysis)

# Foreign Expenses, controls + interaction
model2 <- lm(log_frgnXpns_2012_100k_95 ~ anti_lgbtq + 
               anti_lgbtq*ind_yearMrgEq_rtrn +
               gov_republican +
               state_population +
               frgn_born +
               college_educ_over_25 +
               log_gdp_2012 + 
               ind_yearMrgEq_rtrn + 
               log_exempt_orgs + 
               log_rel_orgs + 
               log_totalXpns_2012_100k_95,
             nonprofits_analysis)


a <- coeftest(model2, vcovHC(model2, type = "HC3"))

model2$fitted.values

nonprofits_analysis <- nonprofits_analysis %>%
  mutate(
    model2_fitted = predict$fitted.values
  )



nonprofits_analysis %>%
  ggplot(aes(x = anti_lgbtq, y = log_frgnXpns_2012_100k_95, color = anti_factor) ) +
  geom_point() +
  geom_line(aes(y = model2_fitted), size = 1)

plot_sample <- nonprofits_analysis %>%
  slice_sample(weight_by = anti_lgbtq, n = 1000)

# % of Total Expenditures, no controls
model3 <- lm(propFrgnXpns_2012_100k ~ anti_lgbtq,
             nonprofits_analysis)

# % of Total Expenditures, controls + interaction
model4 <- lm(propFrgnXpns_2012_100k ~ anti_lgbtq +
               anti_lgbtq*ind_yearMrgEq_rtrn +
               gov_republican +
               state_population +
               frgn_born +
               college_educ_over_25 +
               log_gdp_2012 +
               ind_yearMrgEq_rtrn +
               log_exempt_orgs +
               log_rel_orgs,
             nonprofits_analysis)
