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
library(parallel)
#--------------------------------------------------------
#--------------------------------------------------------
# Importing data
#--------------------------------------------------------
ddi_college <- read_ipums_ddi("/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/usa_00001.xml")
data_college <- read_ipums_micro(ddi_college)

ddi_frgnborn <- read_ipums_ddi("/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/acs_frgnborn.xml")
data_frgnborn <- read_ipums_micro(ddi_frgnborn)

# States to include in sample (excluding PR and DC)
states_included <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
states_fips <- c("1", "2", "4", "5", "6", "8", "9", "10", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")

#--------------------------------------------------------
# % of college-graduated people x state, 2008-2019
#--------------------------------------------------------
# EDUC Codes
# 00	N/A or no schooling
# 01	Nursery school to grade 4
# 02	Grade 5, 6, 7, or 8
# 03	Grade 9
# 04	Grade 10
# 05	Grade 11
# 06	Grade 12
# 07	1 year of college
# 08	2 years of college
# 09	3 years of college
# 10	4 years of college
# 11	5+ years of college

# This loop calculates the % of college-graduated people in each state
# for the years in the sample and returns a state-level df.
for (i in 1:length(states_fips)) {
  
  # Recoding the education variable
  college_state <- data_college %>%
    select(YEAR, STATEFIP, PERWT, EDUC) %>%
    mutate(
      STATEFIP = as.character(STATEFIP)
    ) %>%
    filter(
      STATEFIP == states_fips[i]
    ) %>%
    mutate(
      EDUC = as.integer(EDUC),
      college = case_when(
        EDUC > 07 ~ 1, # includes codes 08-11. Associate's degree +
        EDUC < 08 ~ 0 # includes all codes up to 2 years of college (associate's degree)
      ),
      college_weighted = college*PERWT
    )
  
  prop_college_state <- college_state %>%
    group_by(YEAR) %>%
    summarise(
      prop_college = weighted.mean(college, PERWT) 
    ) %>%
    mutate(
      state = states_included[i]
    )
  
  df_name <- paste0("educ_", states_included[i])
  
  assign(df_name, prop_college_state)
  
}

college_states <- mget(ls(pattern="^educ_")) %>%
  bind_rows()
#--------------------------------------------------------
# % of foreign-born people x state, 2008-2019
#--------------------------------------------------------
# BPL Codes
# 1-120: US states and posessions
# 150-950: other countries, etc.
# 999: Missing

# This loop calculates the % of college-graduated people in each state
# for the years in the sample and returns a state-level df.
for (i in 1:length(states_fips)) {
  
  # Recoding the education variable
  frgnborn_state <- data_college %>%
    select(YEAR, STATEFIP, PERWT, BPL) %>%
    mutate(
      STATEFIP = as.character(STATEFIP)
    ) %>%
    filter(
      STATEFIP == states_fips[i]
    ) %>%
    mutate(
      BPL = as.integer(BPL),
      college = case_when(
        BPL > 07 ~ 1, # includes codes 08-11. Associate's degree +
        BPL < 08 ~ 0 # includes all codes up to 2 years of college (associate's degree)
      ),
      college_weighted = college*PERWT
    )
  
  prop_college_state <- college_state %>%
    group_by(YEAR) %>%
    summarise(
      prop_college = weighted.mean(college, PERWT) 
    ) %>%
    mutate(
      state = states_included[i]
    )
  
  df_name <- paste0("educ_", states_included[i])
  
  assign(df_name, prop_college_state)
  
}

college_states <- mget(ls(pattern="^educ_")) %>%
  bind_rows()
#--------------------------------------------------------



EDUC = as.integer(EDUC),
college = case_when(
  EDUC > 07 ~ 1, # includes codes 08-11. Associate's degree +
  EDUC < 08 ~ 0 # includes all codes up to 2 years of college (associate's degree)
),

#define number of data frames to split into
n <- 3

#split data frame into n equal-sized data frames
split(data_educ, factor(sort(rank(row.names(data_educ))%%n)))

data_educ <- data_educ %>%
  select(YEAR , STATEFIP, PERWT, EDUC) %>%
  mutate(
    STATEFIP = as.character(STATEFIP),
    state_fip = case_when(
      STATEFIP == "01" ~	"AL",
      STATEFIP == "02" ~	"AK",
      STATEFIP == "04" ~	"AZ",
      STATEFIP == "05" ~	"AR",
      STATEFIP == "06" ~	"CA",
      STATEFIP == "08" ~	"CO",
      STATEFIP == "09" ~	"CT",
      STATEFIP == "10" ~	"DE",
      STATEFIP == "11" ~	"DC",
      STATEFIP == "12" ~	"FL",
      STATEFIP == "13" ~	"GA",
      STATEFIP == "15" ~	"HI",
      STATEFIP == "16" ~	"ID",
      STATEFIP == "17" ~	"IL",
      STATEFIP == "18" ~	"IN",
      STATEFIP == "19" ~	"IA",
      STATEFIP == "20" ~	"KS",
      STATEFIP == "21" ~	"KY",
      STATEFIP == "22" ~	"LA",
      STATEFIP == "23" ~	"ME",
      STATEFIP == "24" ~	"MD",
      STATEFIP == "25" ~	"MA",
      STATEFIP == "26" ~	"MI",
      STATEFIP == "27" ~	"MN",
      STATEFIP == "28" ~	"MS",
      STATEFIP == "29" ~	"MO",
      STATEFIP == "30" ~	"MT",
      STATEFIP == "31" ~	"NE",
      STATEFIP == "32" ~	"NV",
      STATEFIP == "33" ~	"NH",
      STATEFIP == "34" ~	"NJ",
      STATEFIP == "35" ~	"NM",
      STATEFIP == "36" ~	"NY",
      STATEFIP == "37" ~	"NC",
      STATEFIP == "38" ~	"ND",
      STATEFIP == "39" ~	"OH",
      STATEFIP == "40" ~	"OK",
      STATEFIP == "41" ~	"OR",
      STATEFIP == "42" ~	"PA",
      STATEFIP == "44" ~	"RI",
      STATEFIP == "45" ~	"SC",
      STATEFIP == "46" ~	"SD",
      STATEFIP == "47" ~	"TN",
      STATEFIP == "48" ~	"TX",
      STATEFIP == "49" ~	"UT",
      STATEFIP == "50" ~	"VT",
      STATEFIP == "51" ~	"VA",
      STATEFIP == "53" ~	"WA",
      STATEFIP == "54" ~	"WV",
      STATEFIP == "55" ~	"WI",
      STATEFIP == "56" ~	"WY",
      STATEFIP == "61" ~	"Maine-New Hampshire-Vermont",
      STATEFIP == "62" ~	"Massachusetts-Rhode Island",
      STATEFIP == "63" ~	"Minnesota-Iowa-Missouri-Kansas-Nebraska-S. Dakota-N. Dakota",
      STATEFIP == "64" ~	"Maryland-Delaware",
      STATEFIP == "65" ~	"Montana-Idaho-Wyoming",
      STATEFIP == "66" ~	"Utah-Nevada",
      STATEFIP == "67" ~	"Arizona-New Mexico",
      STATEFIP == "68" ~	"Alaska-Hawaii",
      STATEFIP == "72" ~	"PR",
      STATEFIP == "97" ~	"Overseas Military Installations",
      STATEFIP == "99" ~	"State not identified")
) %>%
  filter(
    state_fip_short %in% states_included
  )
