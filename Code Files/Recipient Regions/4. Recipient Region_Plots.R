## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file creates plots that summarize the total
#   amount of USD spent by US-based nonprofits in foreign
#   locations through cash grants, non-cash grants, and
#   other activities

## Output:
#   Regions_Activities_230207.png
#   Regions_Cash Grants_230207.png
#   Regions_Non Cash Grants_230207.png

## Last updated: Feb. 7 by Sebastian Rojas Cabal
#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(places)
#--------------------------------------------------------
#--------------------------
# Data import
#--------------------------
activities <- read_csv("/Volumes/SRC_DATA/000_f990_data/regions_activities.csv")

grants_cash <- read_csv("/Volumes/SRC_DATA/000_f990_data/regions_cashgrants.csv") 

grants_noncash <- read_csv("/Volumes/SRC_DATA/000_f990_data/regions_noncashgrants.csv")
#--------------------------
# Activities
#--------------------------
activities %>%
  select(dst_fctr, clean_activities) %>%
  group_by(dst_fctr) %>% 
  summarise(total_activities = sum(clean_activities)) %>%
  mutate(
    total_activities_b = total_activities/1000000000 # billions
  ) %>%
  ggplot(aes(x=dst_fctr, y=total_activities_b)) +
  geom_bar(stat = "identity") +
  labs(
    title = "USD (Billions) Spent in Foreign Activities\nby US-Based Nonprofits",
    x = "Destination",
    y = "USD Spent",
    caption = "N: 228,961 observations"
  ) +
  theme(axis.text.y = element_text(angle = 45)) +
  coord_flip()

# Export
ggsave("/Volumes/GoogleDrive/My Drive/F990/foreign990/Plots/Regions_Activities_230207.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

#--------------------------
# Cash Grants
#--------------------------
grants_cash %>%
  select(dst_fctr, clean_cash_grant) %>%
  group_by(dst_fctr) %>% 
  summarise(total_cash_grants = sum(clean_cash_grant)) %>%
  mutate(
    total_cash_grants_b = total_cash_grants/1000000000 # billions
  ) %>%
  ggplot(aes(x=dst_fctr, y=total_cash_grants_b)) +
  geom_bar(stat = "identity") +
  labs(
    title = "USD (Billions) Spent in Foreign Cash Grants\nby US-Based Nonprofits",
    x = "Destination",
    y = "USD Spent",
    caption = "N: 304,756 observations"
  ) +
  theme(axis.text.y = element_text(angle = 45)) +
  coord_flip()

# Export
ggsave("/Volumes/GoogleDrive/My Drive/F990/foreign990/Plots/Regions_Cash Grants_230207.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

#--------------------------
# Non-Cash Grants
#--------------------------
grants_noncash %>%
  select(dst_fctr, clean_noncash_grant) %>%
  group_by(dst_fctr) %>% 
  summarise(total_noncash_grants = sum(clean_noncash_grant)) %>%
  mutate(
    total_noncash_grants_b = total_noncash_grants/1000000000 # billions
  ) %>%
  ggplot(aes(x=dst_fctr, y=total_noncash_grants_b)) +
  geom_bar(stat = "identity") +
  labs(
    title = "USD (Billions) Spent in Foreign Non-Cash Grants\nby US-Based Nonprofits",
    x = "Destination",
    y = "USD Spent",
    caption = "N: 33,639 observations"
  ) +
  theme(axis.text.y = element_text(angle = 45)) +
  coord_flip()

# Export
ggsave("/Volumes/GoogleDrive/My Drive/F990/foreign990/Plots/Regions_Non Cash Grants_230207.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)
#--------------------------
