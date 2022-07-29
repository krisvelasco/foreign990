## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file produces the figures
#   for the analyses presented in the ASA Annual Meeting.

## Last updated: July 29th by Sebastian Rojas Cabal
#--------------------------------------------------------
#--------------------------------------------------------
# Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lmtest)
library(sandwich)
library(stargazer)
library(estimatr)
#--------------------------------------------------------
#--------------------------------------------------------
# Data Import
#--------------------------------------------------------
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
#--------------------------------------------------------
# Models
#--------------------------------------------------------
#--------------------------------------------------------
# Models: Logged Gross Foreign Expenses
#--------------------------------------------------------
model_frgnXpns <- lm_robust(log_frgnXpns_2012_100k_99 ~ anti_lgbtq + 
                                 anti_lgbtq*ind_yearMrgEq_rtrn +
                                 gov_republican +
                                 state_population +
                                 frgn_born +
                                 college_educ_over_25 +
                                 log_gdp_2012 + 
                                 ind_yearMrgEq_rtrn + 
                                 log_exempt_orgs + 
                                 log_rel_orgs + 
                                 log_totalXpns_2012_100k_99,
                               data = nonprofits_analysis)

tidy(model_frgnXpns)

fitted_frgnXnps <- nonprofits_analysis %>%
  mutate(
    fit_frgnXnps = model_frgnXpns$fitted.values
  ) %>%
  select(
    rtrn_state,
    tax_year,
    ind_yearMrgEq_rtrn,
    anti_factor,
    fit_frgnXnps
  )

# 2013
fitted_frgnXnps %>%
  filter(
    ind_yearMrgEq_rtrn
  )
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    expenses = mean(fit_frgnXnps)
  ) %>%
  ggplot() +
  geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
  labs(y = "Average Foreign Expenses \n(Hundreds of Thousands of USD)",
       caption = "N (Non Anti-LGBTQ+): 1,636,682\nN (Anti-LGBTQ+): 5,608\nAmounts in real 2013 USD\nDashed line indicates the year same-sex marriage was legalized at the federal level") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/gross_national.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

# 2014

# 2015

# Adjust standard errors
model2_cov1         <- vcovHC(model2_frgnXpns_complete, type = "HC3")
model2_rse    <- sqrt(diag(model2_cov1))

# Adjust F statistic 
# wald_model2 <- waldtest(model2, vcov = model2_cov1)

#--------------------------------------------------------
# Models: % of Total Expenditures
#--------------------------------------------------------
# No controls
model3_propXpns <- lm(propFrgnXpns_2012_100k ~ anti_lgbtq,
                      nonprofits_analysis)

# Adjust standard errors
model3_cov1         <- vcovHC(model3_propXpns, type = "HC3")
model3_rse    <- sqrt(diag(model3_cov1))

# Adjust F statistic 
# wald_model3 <- waldtest(model3, vcov = model3_cov1)

# % of Total Expenditures, controls + interaction
model4_propXpns_complete <- lm(propFrgnXpns_2012_100k ~ anti_lgbtq +
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

# Adjust standard errors
model4_cov1         <- vcovHC(model4_propXpns_complete, type = "HC3")
model4_rse    <- sqrt(diag(model4_cov1))

# Adjust F statistic 
# wald_model4 <- waldtest(model4, vcov = model4_cov1)

# Manually adding F-Statistic
#          add.lines = list(c("F Statistic (df = 3; 360)", "12.879***", "7.73***")))

#--------------------------------------------------------
# Models: Likelihood of Start Spending Abroad (Logit)
#--------------------------------------------------------
# No controls
model5_spendsAbrd <- glm(spends_abroad ~ anti_lgbtq,
                         data = nonprofits_analysis, family = "binomial")

# Adjust standard errors
model5_cov1         <- vcovHC(model5_spendsAbrd, type = "HC3")
model5_rse    <- sqrt(diag(model5_cov1))

# Adjust F statistic 
# wald_model5 <- waldtest(model5, vcov = model5_cov1)

# Controls + interaction
model6_spendsAbrd_complete <- glm(spends_abroad ~ anti_lgbtq +
                                    anti_lgbtq*ind_yearMrgEq_rtrn +
                                    gov_republican +
                                    state_population +
                                    frgn_born +
                                    college_educ_over_25 +
                                    log_gdp_2012 + 
                                    ind_yearMrgEq_rtrn + 
                                    log_exempt_orgs + 
                                    log_rel_orgs + 
                                    log_totalXpns_2012_100k_99,
                                  data = nonprofits_analysis,
                                  family = "binomial")

# Adjust standard errors
model6_cov1         <- vcovHC(model6_spendsAbrd_complete, type = "HC3")
model6_rse    <- sqrt(diag(model6_cov1))

# Adjust F statistic 
# wald_model6 <- waldtest(model6, vcov = model6_cov1)

# Manually adding F-Statistic
#          add.lines = list(c("F Statistic (df = 3; 360)", "12.879***", "7.73***")))
