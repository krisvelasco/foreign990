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
library(tidycensus)
library(tidyverse)
##library(ipumsr)
##library(parallel)
#--------------------------------------------------------
#--------------------------------------------------------
# Variables through tidycensus
#--------------------------------------------------------
census_api_key("fb0c648699b7e93713251d0ff83622fa72135efe")

vars0818 <- load_variables(2008, "acs1")

concepts <- data.frame(
 concept = unique(vars0818$concept)
)

# Education vars
educ <- concepts %>%
  mutate(
    educ = str_match(concept, "EDUCATION")
  ) %>%
  filter(
    !is.na(educ)
  )

attainment <- vars0818 %>%
  filter(
    concept == "EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER"
  ) %>%
  rename(
    variable = name
  )

attainment_vars <- attainment$variable

atnmnt_select <- c("B15003_001", "B15003_021", "B15003_022", "B15003_023", "B15003_024",
                   "B15003_025")

# Place of birth
pob <- concepts %>%
  mutate(
    pob = str_match(concept, "PLACE OF BIRTH")
  ) %>%
  filter(
    !is.na(pob)
  )

frgnborn <- vars0818 %>%
  filter(
    concept == "PLACE OF BIRTH BY CITIZENSHIP STATUS"
  ) %>%
  rename(
    variable = name
  )

frgnborn_vars <- frgnborn$variable
frgnborn_select <- c("C05002_001" , "C05002_008")

# Population
pop <- concepts %>%
  mutate(
    pop = str_match(concept, "POPULATION")
  ) %>%
  filter(
    !is.na(pop)
  )

population <- vars0818 %>%
  filter(
    concept == "TOTAL POPULATION"
  ) %>%
  rename(
    variable = name
  )

population_vars <- population$variable

acs_years <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014",
               "2015", "2016", "2017", "2018")

# Prop of population over 25 with AB or more schooling, by state, 2008-2018
for (i in 1:length(acs_years)) {
  educ_year <- get_acs(
                survey = "acs1",            
                geography = "state", 
                variables = attainment_vars,
                year = acs_years[i]) %>%
    left_join(attainment, by = "variable") %>%
    select(-label, -concept, -moe) %>%
    filter(variable %in% atnmnt_select) %>%
    group_by(GEOID) %>%
    pivot_wider(
      names_from = variable,
      values_from = estimate
    ) %>%
    mutate(
      college_educ_over_25 = (B15003_021 + B15003_022 + B15003_023 + B15003_024 + B15003_025)/B15003_001,
      year = acs_years[i]
    ) %>%
    select(GEOID, NAME, year, college_educ_over_25)

  df_name <- paste0("educ_", acs_years[i])

  assign(df_name, educ_year)
}

college_states <- mget(ls(pattern="^educ_")) %>%
  bind_rows() %>%
  filter(
    GEOID != "11" &
    GEOID != "72"
  ) %>%
  mutate(
    number = row_number()
  ) %>%
  filter(
    number != 12
  )

# Prop of population who is foreign-born, by state, 2008-2018

for (i in 1:length(acs_years)) {
  frgnborn_year <- get_acs(
    survey = "acs1",            
    geography = "state", 
    variables = frgnborn_vars,
    year = acs_years[i]) %>%
    left_join(attainment, by = "variable") %>%
    select(-label, -concept, -moe) %>%
    filter(variable %in% frgnborn_select) %>%
    group_by(GEOID) %>%
    pivot_wider(
      names_from = variable,
      values_from = estimate
    ) %>%
    mutate(
      frgn_born = C05002_008/C05002_001,
      year = acs_years[i]
    ) %>%
    select(GEOID, NAME, year, frgn_born)
  
  df_name <- paste0("frgnborn_", acs_years[i])
  
  assign(df_name, frgnborn_year)
}

frgnborn_states <- mget(ls(pattern="^frgnborn_\\d\\d\\d\\d")) %>%
  bind_rows() %>%
  filter(
    GEOID != "11" &
    GEOID != "72"
  ) %>%
  mutate(
    number = row_number()
  )

# Controls from ACS
controls_acs <- left_join(college_states, frgnborn_states) %>%
  select(-number) %>%
  mutate(
    state = case_when(
      GEOID == "01" ~	"AL",
      GEOID == "02" ~	"AK",
      GEOID == "04" ~	"AZ",
      GEOID == "05" ~	"AR",
      GEOID == "06" ~	"CA",
      GEOID == "08" ~	"CO",
      GEOID == "09" ~	"CT",
      GEOID == "10" ~	"DE",
      GEOID == "11" ~	"DC",
      GEOID == "12" ~	"FL",
      GEOID == "13" ~	"GA",
      GEOID == "15" ~	"HI",
      GEOID == "16" ~	"ID",
      GEOID == "17" ~	"IL",
      GEOID == "18" ~	"IN",
      GEOID == "19" ~	"IA",
      GEOID == "20" ~	"KS",
      GEOID == "21" ~	"KY",
      GEOID == "22" ~	"LA",
      GEOID == "23" ~	"ME",
      GEOID == "24" ~	"MD",
      GEOID == "25" ~	"MA",
      GEOID == "26" ~	"MI",
      GEOID == "27" ~	"MN",
      GEOID == "28" ~	"MS",
      GEOID == "29" ~	"MO",
      GEOID == "30" ~	"MT",
      GEOID == "31" ~	"NE",
      GEOID == "32" ~	"NV",
      GEOID == "33" ~	"NH",
      GEOID == "34" ~	"NJ",
      GEOID == "35" ~	"NM",
      GEOID == "36" ~	"NY",
      GEOID == "37" ~	"NC",
      GEOID == "38" ~	"ND",
      GEOID == "39" ~	"OH",
      GEOID == "40" ~	"OK",
      GEOID == "41" ~	"OR",
      GEOID == "42" ~	"PA",
      GEOID == "44" ~	"RI",
      GEOID == "45" ~	"SC",
      GEOID == "46" ~	"SD",
      GEOID == "47" ~	"TN",
      GEOID == "48" ~	"TX",
      GEOID == "49" ~	"UT",
      GEOID == "50" ~	"VT",
      GEOID == "51" ~	"VA",
      GEOID == "53" ~	"WA",
      GEOID == "54" ~	"WV",
      GEOID == "55" ~	"WI",
      GEOID == "56" ~	"WY",  
    )
  )


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
