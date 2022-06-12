# State-level controls
# Prepared by Sebastian Rojas for the F990 project

#Packages
library(tidyverse)
library(readxl)

# Data import
#   State-level GDP, 2008-2015.
#     Source: Bureau of Economic Analysis (https://apps.bea.gov/regional/histdata/releases/0616gsp/index.cfm)
gdp <- read_csv("/Users/srojascabal/Desktop/000_f990_data/gsp_naics_all.csv") %>%
  filter(IndustryId == "1") %>% # Only keeping GDP for all industries ("all industry total")
  select(
    GeoFIPS, GeoName, Region, ComponentId, ComponentName,
    `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`
    ) %>%
  filter( # Keeping relevant measures of GDP
    ComponentId == "200" | # GDP by state
    ComponentId == "900" | # Real GDP by state
    ComponentId == "1000"  # Per capita GDP by state
  )

#   State-level christian religiosity
#     Source: GSS via ARDA. They have some notes on how to cite them.
#       See: 
# gss_2008 <- read_xlsx("/Users/srojascabal/Desktop/000_f990_data/gss_2008.xlsx") %>%
#   select(
#     PARTYID, # Generally speaking, do you usually think of yourself as a Republican, Democrat, Independent, or what? (PARTYID)
#     RELIG, # What is your religious preference? Is it Protestant, Catholic, Jewish, some other religion, or no religion? (RELIG)
#     DENOM, # If 'Protestant' to RELIG: What specific denomination is that, if any? (DENOM)
#     RELITEN # Would you call yourself a strong [RELIGIOUS PREFERENCE] or a not very strong [RELIGIOUS PREFERENCE]? (RELITEN)
#   )

# Governor's party
#   CITE LIKE THIS
#     Kaplan, Jacob. United States Governors 1775-2020: united_states_governors_1775_2020.csv. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2021-01-16. https://doi.org/10.3886/E102000V3-82580
#     From: https://www.openicpsr.org/openicpsr/project/102000/version/V3/view?path=/openicpsr/102000/fcr:versions/V3/united_states_governors_1775_2020.csv&type=file    
#     To view the citation for the overall project, see http://doi.org/10.3886/E102000V3.

gov_party <- read_csv("/Users/srojascabal/Desktop/000_f990_data/united_states_governors_1775_2020.csv") %>%
  filter(
          year == 2008 |
          year == 2009 |
          year == 2010 |
          year == 2011 |
          year == 2012 |
          year == 2013) %>%
  select(state, year, party) %>%
  rename(gov_party = party) %>%
  arrange(state, year)

# Total nonprofits by state
eo_states <- data.frame(
  filename = list.files("/Users/srojascabal/Desktop/000_f990_data/excempt_org_data/")
  ) %>%
  mutate(
  state = str_extract(filename, "_[a-z][a-z]."),
  state = str_to_upper(str_extract(state, "[a-z][a-z]"))
  )

# For loop to create a df with the summarized info for each state
file_names <- paste0("/Users/srojascabal/Desktop/000_f990_data/excempt_org_data/", eo_states$filename)
state_names <- eo_states$state

for (i in 1:length(file_names)){
  
  state_eo <- read_csv(file_names[i])
  state_name <- state_names[i]
  
  state_df <- state_eo %>%
    select(
      STATE, TAX_PERIOD, FILING_REQ_CD, PF_FILING_REQ_CD, STATUS
    ) %>%
    mutate(
      TAX_YR = str_sub(TAX_PERIOD, start = 1L, end = 4L),
      TAX_MTH = str_sub(TAX_PERIOD, -2L)
    ) %>%
    filter(
      TAX_YR == 2008 |
        TAX_YR == 2009 |
        TAX_YR == 2010 |
        TAX_YR == 2011 |
        TAX_YR == 2012 |
        TAX_YR == 2013) %>%
    mutate(
      TAX_YR = as.character(TAX_YR),
      FILING_REQ_CD_txt = case_when(
        FILING_REQ_CD == "01" ~  "(all other) or 990EZ return",
        FILING_REQ_CD == "02" ~  "Required to file Form 990-N - Income less than $25,000 per year",
        FILING_REQ_CD == "03" ~  "Group return",
        FILING_REQ_CD == "04" ~  "Required to file Form 990-BL, Black Lung Trusts",
        FILING_REQ_CD == "06" ~  "Not required to file (church)",
        FILING_REQ_CD == "07" ~  "Government 501(c)(1)",
        FILING_REQ_CD == "13" ~  "Not required to file (religious organization)",
        FILING_REQ_CD == "14" ~  "Not required to file (instrumentalities of states or political subdivisions)",
        FILING_REQ_CD == "00" ~  "Not required to file (all other)"
      ),
      PF_FILING_REQ_CD_txt = case_when(
        PF_FILING_REQ_CD == "1" ~ "990-PF return",
        PF_FILING_REQ_CD == "0" ~ "No 990-PF return"
      ),
      STATUS_txt = case_when(
        STATUS == "01" ~ "Unconditional Exemption",
        STATUS == "02" ~ "Conditional Exemption",
        STATUS == "12" ~ "Trust described in section 4947(a)(2) of the IR Code",
        STATUS == "25" ~ "Organization terminating its private foundation status under section 507(b)(1)(B) of the Code"
      )
    )
  
  state_sum <- state_df %>%
    group_by(TAX_YR) %>%
    summarise(
      excempt_org_total = n()
    ) %>%
    mutate(
      state = state_names[i]
    ) %>%
    rename(
      tax_year = TAX_YR
    ) %>%
    select(
      state, tax_year, excempt_org_total
    )
  
  assign(paste0("sum_", state_names[i]), state_sum)
}

# Appending all the state data frames together

excempt_orgs <- mget(ls(pattern="^sum_*")) %>%
  bind_rows()


