## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file uses Schedule F to find the location (country, region) of recipients of foreign grants by U.S.-based nonprofits.

## Output:

## Last updated: Sept. 29th by Sebastian Rojas Cabal
#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
#--------------------------------------------------------
#--------------------------
# Data import
#--------------------------
# Part 1
# NO INFO
dirty_f_1 <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_i.csv")

# Part 2
# NO INFO
dirty_f_2 <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_ii.csv")

# Part 4
dirty_f_4 <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_iv.csv")

# Activities
# Region info available
dirty_f_activities <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_activities.csv")

# Individual Grants
# Region info available
dirty_f_individualGrants <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_individ_grants.csv")

# Supplement
# NO INFO
dirty_f_supplement <- read_csv("/Users/srojascabal/Desktop/000_f990_data/sched_f_supplement.csv")

