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
library(ggpubr)
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

fitted_frgnXnps <- nonprofits_analysis %>%
  mutate(
    fit_frgnXnps = model_frgnXpns$fitted.values
  ) %>%
  select(
    rtrn_state,
    tax_year,
    yearMrgEq_rtrn,
    anti_factor,
    fit_frgnXnps
  )

#--------
# 2013
#--------
fitted_frgnXnps_2013 <- fitted_frgnXnps %>%
  filter(
    yearMrgEq_rtrn == 2013
  ) %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  )

table(fitted_frgnXnps_2013$anti_factor)

# Average gross expenses - fitted values
mean_xsps_2013 <- fitted_frgnXnps_2013 %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    mean_expenses = mean(fit_frgnXnps)
  )

plot_2013 <- fitted_frgnXnps_2013 %>%
  ggplot() +
  geom_line(data = mean_xsps_2013, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_frgnXnps, color = anti_factor), alpha = 1/10) +
  labs(title = "2013") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2013", linetype = "dashed", color = "gray29")

#--------
# 2014
#--------
fitted_frgnXnps_2014 <- fitted_frgnXnps %>%
  filter(
    yearMrgEq_rtrn == 2014
  ) %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  )

table(fitted_frgnXnps_2014$anti_factor)

# Average gross expenses - fitted values
mean_xsps_2014 <- fitted_frgnXnps_2014 %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    mean_expenses = mean(fit_frgnXnps)
  )

plot_2014 <- fitted_frgnXnps_2014 %>%
  ggplot() +
  geom_line(data = mean_xsps_2014, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_frgnXnps, color = anti_factor), alpha = 1/10) +
  labs(title = "2014") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2014", linetype = "dashed", color = "gray29")

#--------
# 2015
#--------
fitted_frgnXnps_2015 <- fitted_frgnXnps %>%
  filter(
    yearMrgEq_rtrn == 2015
  ) %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  )

table(fitted_frgnXnps_2015$anti_factor)

mean_xsps_2015 <- fitted_frgnXnps_2015 %>%
  mutate(
    factor_txyr = as.factor(tax_year)
    ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    mean_expenses = mean(fit_frgnXnps)
  )

plot_2015 <- fitted_frgnXnps_2015 %>%
  ggplot() +
  geom_line(data = mean_xsps_2015, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_frgnXnps, color = anti_factor), alpha = 1/10) +
  labs(title = "2015") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

ggarrange(
  plot_2013, plot_2014, plot_2015,
  common.legend = TRUE, legend = "bottom"
)
#--------------------------------------------------------
# Exporting Figure
#--------------------------------------------------------
ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_gross.pdf",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

