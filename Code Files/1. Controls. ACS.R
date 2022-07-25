## Project: Nonprofit Foreign Expenditures

## Overview:
#   State-level controls.
#     % immigrant by state, 2008-2018
#     % college educated by state, 2008-2018
#     population by state, 2008-2018

## Source:
#   American Community Studies, through tidycensus
#     https://walker-data.com/tidycensus/index.html

## Last updated: July 25th by Sebastian Rojas Cabal
#--------------------------------------------------------
#--------------------------------------------------------
# Loading packages
#--------------------------------------------------------
library(tidycensus)
library(tidyverse)
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
  population_year <- get_acs(
    survey = "acs1",            
    geography = "state", 
    variables = population_vars,
    year = acs_years[i]) %>%
    left_join(attainment, by = "variable") %>%
    select(-label, -concept, -moe) %>%
    group_by(GEOID) %>%
    pivot_wider(
      names_from = variable,
      values_from = estimate
    ) %>%
    mutate(
      state_population = B01003_001,
      year = acs_years[i]
    ) %>%
    select(GEOID, NAME, year, state_population)
  
  df_name <- paste0("pop_", acs_years[i])
  
  assign(df_name, population_year)
}

population_states <- mget(ls(pattern="^pop_\\d\\d\\d\\d")) %>%
  bind_rows() %>%
  filter(
    GEOID != "11" &
      GEOID != "72"
  ) %>%
  mutate(
    number = row_number()
  )

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
  left_join(population_states) %>%
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


