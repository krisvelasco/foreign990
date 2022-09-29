## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file runs the models to be presented as results
#   in ASA's annual meeting.

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
library(aod)
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
#--------------------------------------------------------
# Models: Logged Gross Foreign Expenses
#--------------------------------------------------------
# No controls
model1_frgnXpns <- lm(log_frgnXpns_2012_100k_99 ~ anti_lgbtq,
                    data = nonprofits_analysis)

  # Adjust standard errors
  model1_cov1         <- vcovHC(model1_frgnXpns, type = "HC3")
  model1_rse    <- sqrt(diag(model1_cov1))
  
  # Adjust F statistic 
  # wald_model1 <- waldtest(model1, vcov = model1_cov1)

# Controls + interaction
model2_frgnXpns_complete <- lm(log_frgnXpns_2012_100k_99 ~ anti_lgbtq + 
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

#--------------------------------------------------------
# Exporting all Models
#--------------------------------------------------------
# stargazer(model1_frgnXpns, model2_frgnXpns_complete,
#           model3_propXpns, model4_propXpns_complete,
#           model5_spendsAbrd, model6_spendsAbrd_complete,
#           type = "html",
#           se        = list(model1_rse, model2_rse, model3_rse, model4_rse, model5_rse, model6_rse),
#           omit.stat = "f",
#           single.row = TRUE,
#           out = "/Volumes/GoogleDrive/My Drive/F990/foreign990/Results/results_ASA_allModels.html")
#--------------------------------------------------------
# Results for Paper
#--------------------------------------------------------
stargazer(model2_frgnXpns_complete,
          model4_propXpns_complete,
          model6_spendsAbrd_complete,
          type = "html",
          se        = list(model2_rse, model4_rse, model6_rse),
          omit.stat = c("f", "adj.rsq", "ser"),
          single.row = TRUE,
          out = "/Volumes/GoogleDrive/My Drive/F990/foreign990/Results/results_ASA_completeModels.html")

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

mena_winsor <- sample_pro %>%
  select(org_type,
         winsor_frgnXpns_2012_99,
         propFrgnXpns_2012_100k) %>%
  group_by(org_type) %>%
  summarise(
    avg = mean(winsor_frgnXpns_2012_99),
    prop = mean(propFrgnXpns_2012_100k)
  )

(0.58357227*100000)*1027
59,932,872

(0.03125504*100000)*464
1,450,234

# Likelihood of Spending Abroad
exp(1.892)
1.892*100

exp(0.024)
1.02429*100

#########