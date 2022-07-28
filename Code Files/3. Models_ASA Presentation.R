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
library(estimatr)
library(aod)
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
    log_frgnXpns_2012_100k_95 = log(winsor_frgnXpns_2012_95+1),
    log_frgnXpns_2012_100k_99 = log(winsor_frgnXpns_2012_99+1),
    log_totalXpns_2012_100k_95 = log(winsor_totalXpns_2012_95+1),
    log_totalXpns_2012_100k_99 = log(winsor_totalXpns_2012_99+1),
    log_rel_orgs = log(rel_orgs +1),
    log_exempt_orgs = log(exempt_orgs+1)
  )

#--------------------------------------------------------
# Models: Logged Gross Foreign Expenses
#--------------------------------------------------------
# No controls
model1 <- lm(log_frgnXpns_2012_100k_99 ~ anti_lgbtq,
                    data = nonprofits_analysis)

  # Adjust standard errors
  model1_cov1         <- vcovHC(model1, type = "HC3")
  model1_rse    <- sqrt(diag(model1_cov1))
  
  # Adjust F statistic 
  # wald_model1 <- waldtest(model1, vcov = model1_cov1)

# Controls + interaction
model2 <- lm(log_frgnXpns_2012_100k_99 ~ anti_lgbtq + 
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

  # Adjust standard errors
  model2_cov1         <- vcovHC(model2, type = "HC3")
  model2_rse    <- sqrt(diag(model2_cov1))
  
  # Adjust F statistic 
  # wald_model2 <- waldtest(model2, vcov = model2_cov1)

stargazer(model1, model2, type = "html",
          se        = list(model1_rse, model2_rse),
          omit.stat = "f",
          out = "/Users/srojascabal/Desktop/000_f990_data/Results/results_ASA_grossXpns.html")

# Manually adding F-Statistic
#          add.lines = list(c("F Statistic (df = 3; 360)", "12.879***", "7.73***")))

#--------------------------------------------------------
# Models: % of Total Expenditures
#--------------------------------------------------------
# No controls
model3 <- lm(propFrgnXpns_2012_100k ~ anti_lgbtq,
             nonprofits_analysis)

  # Adjust standard errors
  model3_cov1         <- vcovHC(model3, type = "HC3")
  model3_rse    <- sqrt(diag(model3_cov1))
  
  # Adjust F statistic 
  # wald_model3 <- waldtest(model3, vcov = model3_cov1)

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

  # Adjust standard errors
  model4_cov1         <- vcovHC(model4, type = "HC3")
  model4_rse    <- sqrt(diag(model4_cov1))
  
  # Adjust F statistic 
  # wald_model4 <- waldtest(model4, vcov = model4_cov1)

stargazer(model3, model4, type = "html",
          se        = list(model3_rse, model4_rse),
          omit.stat = "f",
          out = "/Users/srojascabal/Desktop/000_f990_data/Results/results_ASA_percentXpns.html")

# Manually adding F-Statistic
#          add.lines = list(c("F Statistic (df = 3; 360)", "12.879***", "7.73***")))

#--------------------------------------------------------
# Models: Likelihood of Start Spending Abroad (Logit)
#--------------------------------------------------------
# No controls
model5 <- glm(spends_abroad ~ anti_lgbtq,
              data = nonprofits_analysis, family = "binomial")

  # Adjust standard errors
  model5_cov1         <- vcovHC(model5, type = "HC3")
  model5_rse    <- sqrt(diag(model5_cov1))
  
  # Adjust F statistic 
  # wald_model5 <- waldtest(model5, vcov = model5_cov1)

# Controls + interaction
model6 <- glm(spends_abroad ~ anti_lgbtq +
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
  model6_cov1         <- vcovHC(model6, type = "HC3")
  model6_rse    <- sqrt(diag(model6_cov1))
  
  # Adjust F statistic 
  # wald_model6 <- waldtest(model6, vcov = model6_cov1)

stargazer(model5, model6, type = "html",
          se        = list(model5_rse, model6_rse),
          omit.stat = "f",
          out = "/Users/srojascabal/Desktop/000_f990_data/Results/results_ASA_probFrgnXpns.html")

# Manually adding F-Statistic
#          add.lines = list(c("F Statistic (df = 3; 360)", "12.879***", "7.73***")))

#--------------------------------------------------------
# Exporting all Models
#--------------------------------------------------------
stargazer(model1, model2, model3, model4, model5, model6, type = "html",
          se        = list(model1_rse, model2_rse, model3_rse, model4_rse, model5_rse, model6_rse),
          omit.stat = "f",
          single.row = TRUE,
          out = "/Users/srojascabal/Desktop/000_f990_data/Results/results_ASA_allModels.html")
#--------------------------------------------------------
# Results for Paper
#--------------------------------------------------------
stargazer(model2, model4, model6, type = "html",
          se        = list(model2_rse, model4_rse, model6_rse),
          omit.stat = c("f", "adj.rsq", "ser"),
          single.row = TRUE,
          out = "/Users/srojascabal/Desktop/000_f990_data/Results/results_ASA_completeModels.html")

# Anti-LGBTQ
exp(0.164)
1.178214-1
0.178214*100
round(0.178214*100, digits = 2)

# Interaction, Anti-LGBTQ: Mrg Eq
exp(0.061)
1.062899-1
0.062899*100
round(0.062899*100, digits = 2)

mean_frgnXpns <- nonprofits_analysis %>%
  select(anti_factor,
         winsor_frgnXpns_2012_99,
         propFrgnXpns_2012_100k) %>%
  group_by(anti_factor) %>%
  summarise(
    avg = mean(winsor_frgnXpns_2012_99),
    prop = mean(propFrgnXpns_2012_100k)
  )

# Likelihood of Spending Abroad
exp(1.892)
1.892*100

exp(0.024)
1.02429*100

#########
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
