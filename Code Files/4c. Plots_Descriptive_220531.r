## Project: Nonprofit Foreign Expenditures

## Overview: 
# This file uses two data sets:
#   anti_sample_220601.csv
#   nonanti_sample_220601.csv
# It plots figures that show how much money do anti-LGBTQ+ non profits
# spend abroad compared to non-anti non profits.

# These are exploratory visualizations.

#Last updated: June 1, 2022 by Sebastian Rojas Cabal

#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
#--------------------------------------------------------
#--------------------------------------------------------
# Importing the data
#--------------------------------------------------------
anti <- read_csv("/Users/srojascabal/Desktop/000_f990_data/anti_sample_220601.csv") %>%
select(ein, tax_year, rtrn_state, frgnXpns_2013, totalXpns_2013)


anti2 <- anti %>%
select(ein, name, tax_year, rtrn_state, frgnXpns_2013, totalXpns_2013) %>%
  pivot_wider(
    values_from = c(frgnXpns_2013, totalXpns_2013),
    names_from = tax_year
  ) %>%
  rename_at(vars(contains("_2013_")), ~str_remove(., "_2013_")) %>%
  select(-contains("2013"), -contains("2020"), -contains("2019")) %>%
  pivot_longer(
    cols = -c("ein", "name", "rtrn_state"),
    names_to = c("xpns_type", "tax_year"),
    names_pattern = "([A-Za-z]+)(\\d+)",
    values_to = "amount") %>%
  pivot_wider(
    values_from = "amount",
    names_from = "xpns_type") %>%
  mutate(
    totalXpns = case_when(
      is.na(totalXpns) == TRUE ~ 0,
      TRUE ~ totalXpns),
    frgnXpns = case_when(
      is.na(frgnXpns) == TRUE ~ 0,
      TRUE ~ frgnXpns)) %>%
  filter(
    totalXpns != 0) %>%
  mutate(
    frgnProp = frgnXpns/totalXpns,
    tax_year = as.factor(tax_year)
  ) %>%
  na.omit() %>%
  mutate(
    anti = 1
  )
  
anti %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))

nonanti %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))

nonanti <- read_csv("/Users/srojascabal/Desktop/000_f990_data/nonanti_sample_220601.csv") %>%
  group_by(ein, tax_year) %>%
  slice_min(rtrn_timestmp) %>%
  select(ein, tax_year, rtrn_state, frgnXpns_2013, totalXpns_2013) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(
    anti = 0
  )
  
nonanti2 <- nonanti %>%
mutate(row = row_number()) %>%
  pivot_wider(
    values_from = c(frgnXpns_2013, totalXpns_2013),
    names_from = tax_year
  ) %>%
  rename_at(vars(contains("_2013_")), ~str_remove(., "_2013_")) %>%
  select(-contains("2013"), -contains("2020"), -contains("2019"), -row) %>%
  pivot_longer(
    cols = -c("ein", "rtrn_state"),
    names_to = c("xpns_type", "tax_year"),
    names_pattern = "([A-Za-z]+)(\\d+)",
    values_to = "amount") %>%
  na.omit() %>%
  pivot_wider(
    id_cols = c("ein", "tax_year"),
    values_from = "amount",
    names_from = "xpns_type") %>%
  mutate(
    frgnProp = frgnXpns/totalXpns,
    tax_year = as.factor(tax_year)
  )

#--------------------------------------------------------

#--------------------------------------------------------
# Visualizations
#--------------------------------------------------------
  
  # ----------
  # All nonprofits in the sample, Avg. Expenses Abroad as % of Total Expenses
  # ----------
  
  frgnxpns %>%
    group_by(tax_year) %>%
    summarise(
      expenses = mean(frgnProp)
    ) %>%
    ggplot() +
    geom_point(aes(x = tax_year, y = expenses)) #+
    geom_point(aes(x = tax_year, y = expenses)) #+
    labs(y = "Avg. Foreign Expenses as \nProportion of All Expenses",
         caption = "N (Non Anti-LGBTQ+): 14,545\nN (Anti-LGBTQ+): 26") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
   geom_vline(xintercept = 2015, linetype = "dashed", color = "gray29")
  
  ggsave("National_Percentage.png")
  
  # ----------
  # All nonprofits in the sample, Avg. Gross Foreign Expenses
  # ----------   
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts_mm)
    ) %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(y = "Average Foreign Expenses (USD Millions)",
         caption = "N (Non Anti-LGBTQ+): 14,545\nN (Anti-LGBTQ+): 26") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2015, linetype = "dashed", color = "gray29")
  
  ggsave("National_Gross.png")  
  
  # ----------    
  # ----------
  # Visualizations by state
  # Only states with more than 1 anti-lgbtq organization
  # ----------
  #frgnxpns_clean %>%
  #   filter(anti == 1) %>%
  #   group_by(state_factor) %>%
  #   summarise(anti_orgs = sum(anti)) %>%
  #   filter(anti_orgs > 1)
  
  # ----------
  # Michigan
  # ---------- 
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "MI") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Michigan",
         y = "Avg. Foreign Expenses as \nProportion of All Expenses") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2015, linetype = "dashed", color = "gray29")  
  
  ggsave("MI_Percentage.png")
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts_mm) 
    ) %>%
    filter(state_factor == "MI") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Michigan",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2015, linetype = "dashed", color = "gray29")
  
  ggsave("MI_Gross.png")  
  
  # ----------
  # Kansas
  # ---------- 
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "KS") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Kansas",
         y = "Avg. Foreign Expenses as \nProportion of All Expenses") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2015, linetype = "dashed", color = "gray29")  
  
  ggsave("KS_Percentage.png")
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts_mm) 
    ) %>%
    filter(state_factor == "KS") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Kansas",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2015, linetype = "dashed", color = "gray29")
  
  ggsave("KS_Gross.png")  
  
  # ----------
  # Arizona
  # ---------- 
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "AZ") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Arizona",
        y = "Avg. Foreign Expenses as \nProportion of All Expenses") +
   theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "gray29")
  
  ggsave("AZ_Percentage.png")
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts_mm) 
    ) %>%
    filter(state_factor == "AZ") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Arizona",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "gray29")
  
  ggsave("AZ_Gross.png")  
  
  # ----------
  # New York - NY
  # ----------
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "NY") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "New York",
         y = "Avg. Foreign Expenses as\nProportion of All Expenses") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("NY_Percentage.png")
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts_mm) 
    ) %>%
    filter(state_factor == "NY") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "New York",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("NY_Gross.png") 
  
  # ----------
  # Colorado - CO
  # ----------
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "CO") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Colorado",
         y = "Avg. Foreign Expenses as\nProportion of All Expenses") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "gray29")
  
  ggsave("CO_Percentage.png")
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts_mm) 
    ) %>%
    filter(state_factor == "CO") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    theme_bw() +
    labs(title = "Colorado",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "gray29")
  
  ggsave("CO_Gross.png")
  
  # ----------
  # DC
  # ----------
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "DC") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Distric of Columbia",
         y = "Avg. Foreign Expenses as\nProportion of All Expenses") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  
  ggsave("DC_Percentage.png")
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts_mm) 
    ) %>%
    filter(state_factor == "DC") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "District of Columbia",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  
  ggsave("DC_Gross.png") 
  
  # ----------
  # Virginia - WA
  # ----------
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "VA") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Virginia",
         y = "Avg. Foreign Expenses as\nProportion of All Expenses") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "gray29")
  
  ggsave("VA_Percentage.png")
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts) 
    ) %>%
    filter(state_factor == "VA") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Virginia",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "gray29")
  
  ggsave("VA_Gross.png")
  # ----------
  # Washington - WA
  # ----------
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "WA") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Washington",
         y = "Avg. Foreign Expenses as\nProportion of All Expenses") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "gray29")
  
  ggsave("WA_Percentage.png")
  
  frgnxpns_clean %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts_mm) 
    ) %>%
    filter(state_factor == "WA") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Washington",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "gray29")
  
  ggsave("WA_Gross.png")
  # ----------
  # Grid - All states
  # ----------
  frgnxpns_clean_states <- frgnxpns_clean %>%
    filter(rtrn_state == "AZ" |
             rtrn_state == "CO" | 
             rtrn_state == "DC" | 
             rtrn_state == "NY" |
             rtrn_state == "VA" |
             rtrn_state == "WA" |
             rtrn_state == "KS" |
             rtrn_state == "MI" )
  
  frgnxpns_clean_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(
         y = "Avg. Foreign Expenses as\nProportion of All Expenses") +
    theme_bw() +
    geom_vline(xintercept = 2015, linetype = "dashed", color = "gray29") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    facet_wrap(~ state_factor, nrow = 4, scales = "free_y") 
  
  ggsave("States_Percentage.png")
  
  frgnxpns_clean_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts_mm)) %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme_bw() +
    geom_vline(xintercept = 2015, linetype = "dashed", color = "gray29") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    facet_wrap(~ state_factor, nrow = 4, scales = "free_y")
  
  ggsave("States_Gross.png")
  
  #-------------------
  
  
