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
library(ggeffects)
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

nonprofits_analysis <- nonprofits_analysis %>%
  mutate(
    model2_fitted = model2$fitted.values
  )

sample_anti <- nonprofits_analysis %>%
  filter(
    anti_lgbtq == 1
  ) %>%
  slice_sample(n = 300)

sample_nonanti <- nonprofits_analysis %>%
  filter(
    anti_lgbtq == 0
  ) %>%
  slice_sample(n = 700)

sample <- bind_rows(sample_anti, sample_nonanti)

robust_se <- coeftest(model2, vcovHC(model2, type = "HC3"))

library(estimatr)
fit <- lm_robust(log_frgnXpns_2012_100k_95 ~ anti_lgbtq + 
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
                    data = nonprofits_analysis)
tidy(fit)

ggplot(nonprofits_analysis, aes(x = anti_lgbtq, y = log_frgnXpns_2012_100k_95)) +
  geom_point() +
  geom_smooth(method = "lm_robust") +
  theme_bw()

nonprofits_analysis %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    expenses = mean(model2_fitted)
  ) %>%
  ggplot() +
  geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
  labs(title = "Avg. Frgn Expenses Based on Fitted Values from Models",
      y = "Average Foreign Expenses \n(Hundreds of Thousands of USD)",
       caption = "N (Non Anti-LGBTQ+): 1,636,682\nN (Anti-LGBTQ+): 5,608\nAmounts in real 2012 USD\nDashed line indicates the year same-sex marriage was legalized at the federal level") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")





me <- ggpredict(model2, "anti_lgbtq")

plot(me)

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
