## Project: Nonprofit Foreign Expenditures

## Overview:
#   State-level controls.
#     % immigrant by state, 2008-2018
#     % college educated by state, 2008-2018

## Source:
#   American Community Studies, IPUMS
#     Steven Ruggles, Sarah Flood, Ronald Goeken, Megan Schouweiler and Matthew Sobek. IPUMS USA: Version 12.0 [dataset]. Minneapolis, MN: IPUMS, 2022. 
#     https://doi.org/10.18128/D010.V12.0

## Last updated: July 20th by Sebastian Rojas Cabal
#--------------------------------------------------------
#--------------------------------------------------------
# Loading packages
#--------------------------------------------------------
library(tidyverse)
library(ipumsr)
#--------------------------------------------------------
#--------------------------------------------------------
# Importing data
#--------------------------------------------------------
ddi <- read_ipums_ddi("/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/usa_00001.xml")
data <- read_ipums_micro(ddi)

data_2008 <- data %>%
  filter(YEAR == 2008)
