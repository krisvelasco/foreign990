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

pro_orgs <- read_csv("/Users/srojascabal/Desktop/000_f990_data/eins_pro.csv",
                     col_types = cols(
                       ein = col_character()
                    )) %>%
  mutate(
    pro_lgbtq = 1,
    pro_factor = c("Pro-LGBTQ+"),
  )

sample_pro <- left_join(nonprofits_analysis,
                                 pro_orgs,
                              by = c("ein")) %>%
  mutate(
    pro_lgbtq = case_when(
      pro_lgbtq == 1 ~ 1,
      is.na(pro_lgbtq) == TRUE ~ 0
    ),
    pro_factor = case_when(
      pro_factor == "Pro-LGBTQ+" ~ "Pro-LGBTQ+",
      is.na(pro_factor) == TRUE ~ "Not Pro-LGBTQ+"
    ),
    org_type = case_when(
      pro_factor == "Pro-LGBTQ+" ~ "Pro-LGBTQ+",
      anti_factor == "Anti-LGBTQ+" ~ "Anti-LGBTQ+",
      anti_lgbtq == 0 & pro_lgbtq == 0 ~ "No LGBTQ-Related Work"
    )
  )

mean_expenses <- sample_pro %>%
  group_by(org_type) %>%
  summarise(
    mean = mean(frgnXpns_2012_100k)
  )

orgs_total <- sample_pro %>%
  group_by(org_type) %>%
  distinct(ein) %>%
  count(org_type)

1027

(56.25710064*100000)*1027
577,760,4236

(50*100000)*1027
500,000,000

462

(0.08770639 * 100000)*462
4,052,035


# Master Businness Files
bmf <- read_csv("/Users/srojascabal/Desktop/000_f990_data/bmf_2008_2019.csv") %>%
  select(EIN, STATE, YEAR, FNDNCD) %>%
  mutate(
    EIN = as.character(EIN)
  ) %>%
  filter(
    YEAR > 2012
  )

# 00  All organizations except 501(c)(3)
# 02	Private operating foundation exempt from paying excise taxes on investment income
# 03	Private operating foundation (other)
# 04	Private non-operating foundation
# 09	Suspense
# 10	Church
# 11	School
# 12	Hospital or medical research organization
# 13	Organization operated for the benefit of a public (government owned or run) college or university
# 14	Governmental unit
# 15	Organization with a substantial portion of support from a governmental unit or the general public
# 16	Organization income is <=1/3 investment or unrelated business and >1/3 donated or related to purpose
# 17	Supporting Organization 509(a)(3) for benefit and in conjunction with organization(s) coded 10-16
# 18	Organization organized and operated to test for public safety
# 21	Supporting Organization 509(a)(3) Type I
# 22	Supporting Organization 509(a)(3) Type II
# 23	Supporting Organization 509(a)(3) Type III functionally integrated
# 24	Supporting Organization 509(a)(3) Type III not functionally integrated
  

bmf_wide <- bmf %>%
  mutate(
    id = rownames(bmf)
  ) %>%
  pivot_wider(
    names_from = c(YEAR, FNDNCD),
    names_prefix = "orgType_",
    values_from = FNDNCD
  )

bmf_wide2 <- bmf_wide %>%
  mutate(across(matches("orgType_"),
                case_when(
                  ~ case_when(. > 0 ~ 1, 
                              is.na(.) == TRUE ~ 0)))
                )

#--------------------------------------------------------
# Descriptive WITH PRO: Avg. Gross Foreign Expenses
#--------------------------------------------------------
orgs_total_sample <- sample_pro %>%
  select(ein, org_type, tax_year) %>%
  group_by(tax_year) %>%
  count(org_type)

plot_pro_gross_national <- sample_pro %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, org_type) %>%
  summarise(
    expenses = mean(frgnXpns_2012_100k)
  ) %>%
  ggplot() +
  geom_line(aes(x = factor_txyr, y = expenses, group = org_type, color = org_type)) +
  geom_point(aes(x = factor_txyr, y = expenses, color = org_type)) +
  labs(y = "Average Foreign Expenses \n(Hundreds of Thousands of USD)",
       caption = "N (Non Anti-LGBTQ+): 324,552\nN (Anti-LGBTQ+): 1,027\nAmounts in real 2012 USD") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_gross_national.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

#--------------------------------------------------------
# Descriptive WITH PRO: Avg. Proportion of Foreign Expenses
#--------------------------------------------------------

plot_pro_prop_national <- sample_pro %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, org_type) %>%
  summarise(
    expenses = mean(propFrgnXpns_2012_100k)
  ) %>%
  ggplot() +
  geom_line(aes(x = factor_txyr, y = expenses, group = org_type, color = org_type)) +
  geom_point(aes(x = factor_txyr, y = expenses, color = org_type)) +
  labs(y = "Avg. Foreign Expenses as \nProportion of All Expenses",
       caption = "N (Non Anti-LGBTQ+): 324,552\nN (Anti-LGBTQ+): 1,027\nAmounts in real 2012 USD") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_prop_national.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

#--------------------------------------------------------
# Descriptive: Avg. Gross Foreign Expenses
#--------------------------------------------------------
anti_lgbtq <- nonprofits_analysis %>%
  select(ein, anti_factor) %>%
  distinct(ein, anti_factor) %>%
  count(anti_factor)

plot_gross_national <- nonprofits_analysis %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    expenses = mean(frgnXpns_2012_100k)
  ) %>%
  ggplot() +
  geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
  labs(y = "Average Foreign Expenses \n(Hundreds of Thousands of USD)",
       caption = "N (Non Anti-LGBTQ+): 324,552\nN (Anti-LGBTQ+): 1,027\nAmounts in real 2012 USD") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_gross_national.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

#--------------------------------------------------------
# Descriptive: Avg. Proportion of Foreign Expenses
#--------------------------------------------------------

plot_prop_national <- nonprofits_analysis %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    expenses = mean(propFrgnXpns_2012_100k)
  ) %>%
  ggplot() +
  geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
  labs(y = "Avg. Foreign Expenses as \nProportion of All Expenses",
       caption = "N (Non Anti-LGBTQ+): 324,552\nN (Anti-LGBTQ+): 1,027\nAmounts in real 2012 USD") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_prop_national.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

#--------------------------------------------------------
# Model: Proportion of Foreign Expenses
#--------------------------------------------------------
model_propXpns <- lm_robust(propFrgnXpns_2012_100k ~ anti_lgbtq + 
                                 anti_lgbtq*ind_yearMrgEq_rtrn +
                                 gov_republican +
                                 state_population +
                                 frgn_born +
                                 college_educ_over_25 +
                                 log_gdp_2012 + 
                                 ind_yearMrgEq_rtrn + 
                                 log_exempt_orgs + 
                                 log_rel_orgs,
                               data = nonprofits_analysis)

fitted_propXpns <- nonprofits_analysis %>%
  mutate(
    fit_propXnps = model_propXpns$fitted.values
  ) %>%
  select(
    rtrn_state,
    tax_year,
    yearMrgEq_rtrn,
    anti_factor,
    fit_propXnps
  )

#--------
#   Viz: 2013
#--------
fitted_propXnps_2013 <- fitted_propXpns %>%
  filter(
    yearMrgEq_rtrn == 2013
  ) %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  )

table(fitted_propXnps_2013$anti_factor)

# Average proportion of expenses - fitted values
prop_xsps_2013 <- fitted_propXnps_2013 %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    mean_expenses = mean(fit_propXnps)
  )

plot_prop_2013 <- fitted_propXnps_2013 %>%
  ggplot() +
  geom_line(data = prop_xsps_2013, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_propXnps, color = anti_factor), alpha = 1/10) +
  labs(title = "2013") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2013", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_prop_13.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

#--------
#   Viz: 2014
#--------
fitted_propXnps_2014 <- fitted_propXpns %>%
  filter(
    yearMrgEq_rtrn == 2014
  ) %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  )

table(fitted_propXnps_2014$anti_factor)

# Average prop expenses - fitted values
prop_xsps_2014 <- fitted_propXnps_2014 %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    mean_expenses = mean(fit_propXnps)
  )

plot_prop_2014 <- fitted_propXnps_2014 %>%
  ggplot() +
  geom_line(data = prop_xsps_2014, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_propXnps, color = anti_factor), alpha = 1/10) +
  labs(title = "2014") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2014", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_prop_14.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

#--------
#   Viz: 2015
#--------

fitted_propXnps_2015 <- fitted_propXpns %>%
  filter(
    yearMrgEq_rtrn == 2015
  ) %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  )

table(fitted_propXnps_2015$anti_factor)

prop_xsps_2015 <- fitted_propXnps_2015 %>%
  mutate(
    factor_txyr = as.factor(tax_year)
    ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    mean_expenses = mean(fit_propXnps)
  )

plot_prop_2015 <- fitted_propXnps_2015 %>%
  ggplot() +
  geom_line(data = prop_xsps_2015, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_propXnps, color = anti_factor), alpha = 1/10) +
  labs(title = "2015") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_prop_15.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

#--------------------------------------------------------
#   Viz: 2013, 2014, 2015 together
#--------------------------------------------------------

ggarrange(
  plot_prop_2013, plot_prop_2014, plot_prop_2015,
  common.legend = TRUE, legend = "bottom"
)

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_prop.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

plot_prop_2014_notitle <- fitted_propXnps_2014 %>%
  ggplot() +
  geom_line(data = prop_xsps_2014, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_propXnps, color = anti_factor), alpha = 1/10) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2014", linetype = "dashed", color = "gray29")

plot_prop_2015_notitle <- fitted_propXnps_2015 %>%
  ggplot() +
  geom_line(data = prop_xsps_2015, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_propXnps, color = anti_factor), alpha = 1/10) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

ggarrange(
  plot_prop_2014_notitle, plot_prop_2015_notitle,
  common.legend = TRUE, legend = "bottom"
)

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_prop_14_15.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

#--------------------------------------------------------
# Models: Gross Foreign Expenses
#--------------------------------------------------------
model_grossXpns <- lm_robust(log_frgnXpns_2012_100k_99 ~ anti_lgbtq + 
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

fitted_grossXpns <- nonprofits_analysis %>%
  mutate(
    fit_grossXnps = model_grossXpns$fitted.values
  ) %>%
  select(
    rtrn_state,
    tax_year,
    yearMrgEq_rtrn,
    anti_factor,
    fit_grossXnps
  )

#--------
#   Viz: 2013
#--------
fitted_grossXnps_2013 <- fitted_grossXpns %>%
  filter(
    yearMrgEq_rtrn == 2013
  ) %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  )

table(fitted_grossXnps_2013$anti_factor)

# Average grossortion of expenses - fitted values
gross_xsps_2013 <- fitted_grossXnps_2013 %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    mean_expenses = mean(fit_grossXnps)
  )

plot_gross_2013 <- fitted_grossXnps_2013 %>%
  ggplot() +
  geom_line(data = gross_xsps_2013, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_grossXnps, color = anti_factor), alpha = 1/10) +
  labs(title = "2013") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2013", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_gross_13.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

#--------
#   Viz: 2014
#--------
fitted_grossXnps_2014 <- fitted_grossXpns %>%
  filter(
    yearMrgEq_rtrn == 2014
  ) %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  )

table(fitted_grossXnps_2014$anti_factor)

# Average gross expenses - fitted values
gross_xsps_2014 <- fitted_grossXnps_2014 %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    mean_expenses = mean(fit_grossXnps)
  )

plot_gross_2014 <- fitted_grossXnps_2014 %>%
  ggplot() +
  geom_line(data = gross_xsps_2014, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_grossXnps, color = anti_factor), alpha = 1/10) +
  labs(title = "2014") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2014", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_gross_14.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)

#--------
#   Viz: 2015
#--------

fitted_grossXnps_2015 <- fitted_grossXpns %>%
  filter(
    yearMrgEq_rtrn == 2015
  ) %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  )

table(fitted_grossXnps_2015$anti_factor)

gross_xsps_2015 <- fitted_grossXnps_2015 %>%
  mutate(
    factor_txyr = as.factor(tax_year)
  ) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    mean_expenses = mean(fit_grossXnps)
  )

plot_gross_2015 <- fitted_grossXnps_2015 %>%
  ggplot() +
  geom_line(data = gross_xsps_2015, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_grossXnps, color = anti_factor), alpha = 1/10) +
  labs(title = "2015") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_gross_15.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)
#--------------------------------------------------------
#   Viz: 2013, 2014, 2015 together
#--------------------------------------------------------

ggarrange(
  plot_gross_2013, plot_gross_2014, plot_gross_2015,
  common.legend = TRUE, legend = "bottom"
)

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_gross.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)


plot_gross_2014_notitle <- fitted_grossXnps_2014 %>%
  ggplot() +
  geom_line(data = gross_xsps_2014, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_grossXnps, color = anti_factor), alpha = 1/10) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2014", linetype = "dashed", color = "gray29")


plot_gross_2015_notitle <- fitted_grossXnps_2015 %>%
  ggplot() +
  geom_line(data = gross_xsps_2015, aes(x = factor_txyr, y = mean_expenses, group = anti_factor, color = anti_factor), size = 1) +
  geom_point(aes(x = factor_txyr, y = fit_grossXnps, color = anti_factor), alpha = 1/10) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

ggarrange(
  plot_gross_2014_notitle, plot_gross_2015_notitle,
  common.legend = TRUE, legend = "bottom"
)

ggsave("/Users/srojascabal/Desktop/000_f990_data/Plots/plots_ASA_gross_14_15.png",
       width = 210,
       height = 145, #A5 (half of A4)
       units = "mm",
       dpi = "print"
)
