## Project: Nonprofit Foreign Expenditures

## Overview: 
# Implementing models with synthetic controls. First draft.

#Last updated: May 20 by Sebastian Rojas Cabal

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
# Importing data and cleaning data
#--------------------------------------------------------
# -------------------------
#   List of anti-LGBTQ+ orgs
# -------------------------
# List of known anti-LGBTQ+ nonprofits. 
orgs1 <- read_excel("/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/anti_lgbtq_eins_20220422.xlsx") %>%
  rename(name = Organization,
         ein_char = ein) %>%
  mutate(ein_char = str_trim(ein_char),
         ein = as.numeric(ein_char)) %>%
  select(name, ein) %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE)
# List of first-order ties to known anti-LGBTQ+ nonprofits.
orgs2 <- read_csv("/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/anti_lgbtq_candidates_20220516.csv") %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE)
# Binding all candidate anti-LGBTQ+ orgs
orgs_all <- bind_rows(orgs1, orgs2) %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(anti_lgbtq = 1)
# -------------------------
# -------------------------
#   F990 (Return Header, Part 0, Part 9)
# -------------------------
# -------------------------
#       Return Header of Form 990
# -------------------------
rtrn <- read_csv("/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/return_header.csv")
# these are the forms submissions with metadata from Amazon Web Services/IRS.
#   length(unique(rtrn$id))/nrow(rtrn) # all ids are unique
#   length(unique(rtrn$object_id))/nrow(rtrn) # object_ids are not unique. Parsing problem?
# There are 3 types of forms here, We will restrict everything to 990.
#   990 is for organizations that bring in more than $250,000 - 1,697,326 orgs in our sample.
#   EZ is for those who bring in between $250,000 and $50,000 - 922,304 orgs in our sample.
#   PF is for private foundations - 440,511 orgs in our sample.
rtrn990 <- filter(rtrn, RtrnHdr_RtrnCd == 990) %>%
  rename(
    rtrn_id = id,
    rtrn_timestmp = RtrnHdr_RtrnTs,
    rtrn_txyrbegin = RtrnHdr_TxPrdBgnDt,
    rtrn_txyrend = RtrnHdr_TxPrdEndDt,
    rtrn_form = RtrnHdr_RtrnCd,
    rtrn_EINfiler = Flr_EIN,
    rtrn_name1 = BsnssNm_BsnssNmLn1Txt,
    rtrn_name2 = BsnssNm_BsnssNmLn2Txt,
    rtrn_USaddrs1 = USAddrss_AddrssLn1Txt,
    rtrn_USaddrs2 = USAddrss_AddrssLn2Txt,
    rtrn_state = USAddrss_SttAbbrvtnCd,
    rtrn_county = USAddrss_CtyNm,
    rtrn_zip = USAddrss_ZIPCd) %>%
  mutate(ein_rtrn = ein,
         object_id_rtrn = object_id) %>%
  select(ein, object_id, rtrn_id, ein_rtrn, object_id_rtrn,
         rtrn_timestmp,
         rtrn_txyrbegin,
         rtrn_txyrend,
         rtrn_form,
         rtrn_EINfiler,
         rtrn_name1,
         rtrn_name2,
         rtrn_USaddrs1,
         rtrn_USaddrs2,
         rtrn_state,
         rtrn_county,
         rtrn_zip)  %>%
  rename(rtrn_txyrbgndt = rtrn_txyrbegin,
         rtrn_txyrenddt = rtrn_txyrend) %>%
  mutate(rtrn_txyrstart = year(rtrn_txyrbgndt),
         rtrn_txyrend = year(rtrn_txyrenddt)) %>%
  relocate(rtrn_txyrstart, rtrn_txyrend, .before = rtrn_txyrbgndt)
# -------------------------
# -------------------------
#       Part 0 of Form 990
# -------------------------
part0 <- read_csv("/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/part_0.csv")
# the top of 990 form.
# length(unique(part0$id))/nrow(part0) # all ids are unique
# length(unique(part0$object_id))/nrow(part0) # object_ids are not unique. Parsing problem?

part0 <- part0 %>%
  rename(
    pt0_name1 = DngBsnssAsNm_BsnssNmLn1Txt,
    pt0_name2 = DngBsnssAsNm_BsnssNmLn2Txt,
    pt0_USaddrs1 = USAddrss_AddrssLn1Txt,
    pt0_USaddrs2 = USAddrss_AddrssLn2Txt,
    pt0_state = USAddrss_SttAbbrvtnCd,
    pt0_county = USAddrss_CtyNm,
    pt0_zip = USAddrss_ZIPCd,
    pt0_grouprtrn = GrpRtrnFrAffltsInd,
    pt0_grouprtrn_allincluded = AllAffltsInclddInd,
    pt0_grouprtrn_exmptnmbr = GrpExmptnNm)  %>%
  mutate(ein_0 = ein,
         object_id_0 = object_id,
         id_pt0 = id) %>%
  select(ein, object_id, ein_0, object_id_0,
         id_pt0,
         pt0_name1,
         pt0_name2,
         pt0_USaddrs1,
         pt0_USaddrs2,
         pt0_state,
         pt0_county,
         pt0_zip,
         pt0_grouprtrn,
         pt0_grouprtrn_allincluded,
         pt0_grouprtrn_exmptnmbr)
# -------------------------
#--------------------------
#       Part 9 of Form 990
#--------------------------
part9 <- read_csv("/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/part_ix.csv") %>%
  rename(
    pt9_id = id,
    pt9_totalFrgnGrnts = FrgnGrnts_TtlAmt,
    pt9_prgrmSrcvsAmtFrgnGrnts = FrgnGrnts_PrgrmSrvcsAmt,
    pt9_totalFnctnlExpns = TtlFnctnlExpnss_TtlAmt) %>%
  mutate(
    id = pt9_id,
    ein_9 = ein,
    object_id_9 = object_id
  ) %>%
  select(
    pt9_id,
    id,
    ein,
    ein_9,
    object_id_9,
    object_id,
    pt9_totalFrgnGrnts,
    pt9_prgrmSrcvsAmtFrgnGrnts,
    pt9_totalFnctnlExpns
  )
# -------------------------
#--------------------------------------------------------
#   Merging the data Form 990 data
#--------------------------------------------------------
# We use the anti_join function to get a sense of how much data will be
# lost during the merge.
# After confirming that very little or no data will be lost, we use
# the inner_join function. The inner_join function keeps only observations
# that match across both data sets.
#--------------------------
#       Merging Return Header and Part 0
#--------------------------  
# Part 0 and Retrurn header merge according to ein/object id
#   anti_rtrn0 <- anti_join(part0, rtrn990, by = c("ein", "object_id"))
# 0 observations lost
#   anti_0rtn <- anti_join(rtrn990, part0, by = c("ein", "object_id"))
# 0 observations lost

join_rtrn0 <- inner_join(rtrn990, part0, by = c("ein", "object_id")) %>%
  select(
    ein, ein_0, ein_rtrn,
    object_id, object_id_0, object_id_rtrn,
    id_pt0, rtrn_id,
    rtrn_timestmp,
    rtrn_txyrbgndt,           
    rtrn_txyrend,
    rtrn_txyrenddt,
    rtrn_txyrstart, 
    pt0_state,
    rtrn_state,
    pt0_county,
    rtrn_county,
    pt0_name1,
    rtrn_name1,
    pt0_USaddrs1,
    rtrn_USaddrs1,
    pt0_zip,
    rtrn_zip,
    rtrn_EINfiler,
    rtrn_form,
    pt0_grouprtrn,
    pt0_grouprtrn_allincluded,
    pt0_grouprtrn_exmptnmbr)
# more rows mean combinations of ein x obj_id
#--------------------------
#--------------------------
#       Merging Return Header and Part 9
#--------------------------    
# Part 9 and Retrurn header merge according to ein/object id
#    anti_rtrn9 <- anti_join(part9, rtrn990, by = c("ein", "object_id"))
# 0 observations lost
#    anti_9rtn <- anti_join(rtrn990, part9, by = c("ein", "object_id"))
# 0 observations lost

join_rtrn9 <- inner_join(rtrn990, part9, by = c("ein", "object_id")) %>%
  mutate(FrgnExpnssPrctng = pt9_totalFrgnGrnts/pt9_totalFnctnlExpns) %>%
  # more rows mean combinations of ein x obj_id
  select(
    ein, ein_9, ein_rtrn, 
    rtrn_id, pt9_id,
    object_id, object_id_9, object_id_rtrn,
    FrgnExpnssPrctng,
    pt9_totalFnctnlExpns,
    pt9_totalFrgnGrnts,
    pt9_prgrmSrcvsAmtFrgnGrnts)

#--------------------------
#       Merging the Return Header, Part 0 and Part 9
#--------------------------  
# join_rtrn9 and join_rtrn0 merge according to ein/object id
#    anti_9 <- anti_join(join_rtrn9, join_rtrn0, by = c("ein", "object_id"))
# 0 observations lost
#    anti_0 <- anti_join(rtrn990, part9, by = c("ein", "object_id"))
# 0 observations lost

join_rtrn09 <- inner_join(join_rtrn9, join_rtrn0, by = c("ein", "object_id")) %>%
  select(
    ein, ein_0, ein_9, ein_rtrn.x,
    object_id, object_id_0, object_id_9, object_id_rtrn.x,
    id_pt0, pt9_id, rtrn_id.x,
    rtrn_timestmp, rtrn_txyrbgndt, rtrn_txyrend,            
    rtrn_txyrenddt, rtrn_txyrstart, 
    pt9_totalFnctnlExpns, pt9_totalFrgnGrnts, pt9_prgrmSrcvsAmtFrgnGrnts, FrgnExpnssPrctng,
    pt0_state, rtrn_state) %>%
  rename(
    ein_rtrn = ein_rtrn.x,
    object_id_rtrn = object_id_rtrn.x,
    id_rtrn = rtrn_id.x
  ) %>%
  mutate(
    tax_year = rtrn_txyrend # tax year = end of the tax period in form
  )
# more obs because repeated combinations of object id x ein
#--------------------------
#--------------------------------------------------------
# Merging data for analytical sample
#--------------------------------------------------------
#--------------------------
#   Merging the joined data sets with the list of anti-lgbtq orgs
#--------------------------
rtrn09_anti <- full_join(join_rtrn09, orgs_all, by = c("ein")) %>%
  mutate(
    anti_lgbtq = case_when(
      is.na(anti_lgbtq) == TRUE ~ 0,
      TRUE ~ 1
    ),
    anti_factor = case_when(
      anti_lgbtq == 1 ~ "Anti-LGBTQ+",
      anti_lgbtq == 0 ~ "Not Anti-LGBTQ+"
    ),
    anti_factor = as.factor(anti_factor)
  )
#--------------------------
#--------------------------
#   Selecting relevant variables from the merged data sets
#--------------------------  
# We will call the resulting data frame 'frgnxpns'
# This data set will only include anti_lgbtq orgs
joindf <- rtrn09_anti %>%
  select(ein, object_id,
         tax_year, anti_lgbtq, anti_factor, name,
         rtrn_timestmp, rtrn_txyrbgndt, rtrn_txyrenddt,           
         pt9_totalFnctnlExpns, pt9_totalFrgnGrnts, FrgnExpnssPrctng,pt9_prgrmSrcvsAmtFrgnGrnts,
         pt0_state, rtrn_state) %>%
  filter(anti_lgbtq==1)
#--------------------------
# Identifying the duplicated observations
#--------------------------   
# Many nonprofits submit miltiple forms a year. We only want the most recent.
dups <- joindf %>% count(ein, tax_year) %>%
  mutate(dup = case_when(
    n == 1 ~ 0,
    n > 1 ~ 1)) %>%
  group_by(ein, tax_year) %>%
  select(ein, tax_year, dup)

# Adding them to the joint data
joindf_dups <- inner_join(joindf, dups, by = c("ein", "tax_year"))
#--------------------------
#--------------------------
# Cleaning some relevant variables and adding new ones
#-------------------------- 
# When Total Functional Expenses = NA, drop. There are 917 such NAs.
# When Total Foreign Expenses OR Grants = NA, assume = 0.
# Assume all negative values are positives with a wrong side. Recode *(-1)
# Dropping all cases when Functional Expenses = 0.
# Keeping only the most recent tax filing for each tax year, according to the return time stamp.
# The analysis will be in constant (real) 2013 dollars.
#     CPIs from BLS: (https://www.bls.gov/cpi/tables/supplemental-files/historical-cpi-u-202203.pdf)
#     Method from making it current dollars (https://www.bls.gov/cpi/factsheets/cpi-math-calculations.pdf)
# Adding the year marriage equality was passed in each state.
#     Source: https://www.lgbtmap.org/equality-maps/marriage_relationship_laws

frgnxpns <- joindf_dups %>%
  mutate(
    clean_totalExpenses = case_when(
      pt9_totalFnctnlExpns < 0 ~ pt9_totalFnctnlExpns*(-1),
      TRUE ~ pt9_totalFnctnlExpns
    ),
    clean_totalFrgnGrnts = case_when(
      pt9_totalFrgnGrnts < 0 ~ pt9_totalFrgnGrnts*(-1),
      is.na(pt9_totalFrgnGrnts) == TRUE ~ 0,
      TRUE ~ pt9_totalFrgnGrnts
    ),
    clean_totalFrgnSrvs = case_when(
      pt9_prgrmSrcvsAmtFrgnGrnts < 0 ~ pt9_prgrmSrcvsAmtFrgnGrnts*(-1),
      is.na(pt9_prgrmSrcvsAmtFrgnGrnts) == TRUE ~ 0,
      TRUE ~ pt9_prgrmSrcvsAmtFrgnGrnts
    )) %>%
  mutate(clean_FrgnExpnsPctg = clean_totalFrgnGrnts/clean_totalExpenses,
         clean_FrgnExpnsPctg = case_when(
           is.na(clean_FrgnExpnsPctg) == TRUE ~ 0,
           TRUE ~ clean_FrgnExpnsPctg
         )) %>%
  arrange(ein, name, tax_year) %>%
  filter(!is.na(clean_totalExpenses),
         clean_totalExpenses>0) %>%
  select(
    ein, object_id, tax_year, name,
    dup, pt0_state, starts_with("rtrn"), starts_with("clean")
  ) %>%
  rename(
    totalXpns = clean_totalExpenses,
    frgnXpns = clean_totalFrgnGrnts,
    frgnSrvcs = clean_totalFrgnSrvs,
    pctgFrgnXpns = clean_FrgnExpnsPctg
  ) %>%
  group_by(ein, tax_year, name) %>%
  slice_min(rtrn_timestmp) %>%
  mutate(
    totalXpns_2013 =
      case_when(
        tax_year == 2013 ~ totalXpns/(232.957/232.957),
        tax_year == 2014 ~ totalXpns/(232.957/236.736),
        tax_year == 2015 ~ totalXpns/(232.957/237.017),
        tax_year == 2016 ~ totalXpns/(232.957/240.007),
        tax_year == 2017 ~ totalXpns/(232.957/245.120),
        tax_year == 2018 ~ totalXpns/(232.957/251.107),
        tax_year == 2019 ~ totalXpns/(232.957/255.657),
        tax_year == 2020 ~ totalXpns/(232.957/258.811)
      ),
    yearMrgEq_rtrn = case_when(
      rtrn_state == "AL" ~ 2015,
      rtrn_state == "AK" ~ 2014,
      rtrn_state == "AZ" ~ 2014,
      rtrn_state == "AR" ~ 2015,
      rtrn_state == "AS" ~ 2014,
      rtrn_state == "CA" ~ 2013,
      rtrn_state == "CO" ~ 2014,
      rtrn_state == "CT" ~ 2008,
      rtrn_state == "DE" ~ 2013,
      rtrn_state == "DC" ~ 2010,
      rtrn_state == "FL" ~ 2015,
      rtrn_state == "GA" ~ 2015,
      rtrn_state == "GU" ~ 2015,
      rtrn_state == "HI" ~ 2013,
      rtrn_state == "ID" ~ 2014,
      rtrn_state == "IL" ~ 2013,
      rtrn_state == "IN" ~ 2014,
      rtrn_state == "IA" ~ 2009,
      rtrn_state == "KS" ~ 2015,
      rtrn_state == "KY" ~ 2015,
      rtrn_state == "LA" ~ 2015,
      rtrn_state == "ME" ~ 2012,
      rtrn_state == "MD" ~ 2012,
      rtrn_state == "MA" ~ 2004,
      rtrn_state == "MI" ~ 2015,
      rtrn_state == "MN" ~ 2013,
      rtrn_state == "MS" ~ 2015,
      rtrn_state == "MO" ~ 2015,
      rtrn_state == "MT" ~ 2014,
      rtrn_state == "NE" ~ 2015,
      rtrn_state == "NV" ~ 2014,
      rtrn_state == "NH" ~ 2010,
      rtrn_state == "NJ" ~ 2013,
      rtrn_state == "NM" ~ 2013,
      rtrn_state == "NY" ~ 2011,
      rtrn_state == "NC" ~ 2014,
      rtrn_state == "ND" ~ 2015,
      rtrn_state == "CM" ~ 2015,
      rtrn_state == "OH" ~ 2015,
      rtrn_state == "OK" ~ 2014,
      rtrn_state == "OR" ~ 2014,
      rtrn_state == "PA" ~ 2014,
      rtrn_state == "PR" ~ 2015,
      rtrn_state == "RI" ~ 2013,
      rtrn_state == "SC" ~ 2014,
      rtrn_state == "SD" ~ 2015,
      rtrn_state == "TN" ~ 2015,
      rtrn_state == "TX" ~ 2015,
      rtrn_state == "UT" ~ 2015,
      rtrn_state == "VT" ~ 2014,
      rtrn_state == "VA" ~ 2009,
      rtrn_state == "VI" ~ 2014,
      rtrn_state == "WA" ~ 2012,
      rtrn_state == "WV" ~ 2014,
      rtrn_state == "WI" ~ 2014,
      rtrn_state == "WY" ~ 2014
    ),
    yearMrgEq_pt0 = case_when(
      pt0_state == "AL" ~ 2015,
      pt0_state == "AK" ~ 2014,
      pt0_state == "AZ" ~ 2014,
      pt0_state == "AR" ~ 2015,
      pt0_state == "AS" ~ 2014,
      pt0_state == "CA" ~ 2013,
      pt0_state == "CO" ~ 2014,
      pt0_state == "CT" ~ 2008,
      pt0_state == "DE" ~ 2013,
      pt0_state == "DC" ~ 2010,
      pt0_state == "FL" ~ 2015,
      pt0_state == "GA" ~ 2015,
      pt0_state == "GU" ~ 2015,
      pt0_state == "HI" ~ 2013,
      pt0_state == "ID" ~ 2014,
      pt0_state == "IL" ~ 2013,
      pt0_state == "IN" ~ 2014,
      pt0_state == "IA" ~ 2009,
      pt0_state == "KS" ~ 2015,
      pt0_state == "KY" ~ 2015,
      pt0_state == "LA" ~ 2015,
      pt0_state == "ME" ~ 2012,
      pt0_state == "MD" ~ 2012,
      pt0_state == "MA" ~ 2004,
      pt0_state == "MI" ~ 2015,
      pt0_state == "MN" ~ 2013,
      pt0_state == "MS" ~ 2015,
      pt0_state == "MO" ~ 2015,
      pt0_state == "MT" ~ 2014,
      pt0_state == "NE" ~ 2015,
      pt0_state == "NV" ~ 2014,
      pt0_state == "NH" ~ 2010,
      pt0_state == "NJ" ~ 2013,
      pt0_state == "NM" ~ 2013,
      pt0_state == "NY" ~ 2011,
      pt0_state == "NC" ~ 2014,
      pt0_state == "ND" ~ 2015,
      pt0_state == "CM" ~ 2015,
      pt0_state == "OH" ~ 2015,
      pt0_state == "OK" ~ 2014,
      pt0_state == "OR" ~ 2014,
      pt0_state == "PA" ~ 2014,
      pt0_state == "PR" ~ 2015,
      pt0_state == "RI" ~ 2013,
      pt0_state == "SC" ~ 2014,
      pt0_state == "SD" ~ 2015,
      pt0_state == "TN" ~ 2015,
      pt0_state == "TX" ~ 2015,
      pt0_state == "UT" ~ 2015,
      pt0_state == "VT" ~ 2014,
      pt0_state == "VA" ~ 2009,
      pt0_state == "VI" ~ 2014,
      pt0_state == "WA" ~ 2012,
      pt0_state == "WV" ~ 2014,
      pt0_state == "WI" ~ 2014,
      pt0_state == "WY" ~ 2014
    )
  )
  
#   how to calculate real 2013 dollars:
#   (money in year X)/((CPI in 2013)/(CPI in that year))
#   CPIs from BLS: (https://www.bls.gov/cpi/tables/supplemental-files/historical-cpi-u-202203.pdf)
#   Method from making it current dollars (https://www.bls.gov/cpi/factsheets/cpi-math-calculations.pdf)
#   2013 = 232.957
#   2014 = 236.736
#   2015 = 237.017
#   2016 = 240.007
#   2017 = 245.120
#   2018 = 251.107
#   2019 = 255.657
#   2020 = 258.811
#--------------------------
#--------------------------
# Exporting the data
#-------------------------- 
write_csv(frgnxpns, "/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/scm_sample_draft_220520.csv")
#--------------------------

####----------------- FILE ENDS HERE. THE REST IS STUFF.




####----------------- STUFF
  
by_ein_name <- frgnxpns_realUSD %>%
  group_by(ein, name) %>% 
  tally()
  

aa <- frgnxpns_realUSD %>%
  filter(name == "Rofeh Cholim Cancer Society")
  
  
  
  mutate(
    clean_totalExpenses = case_when(
      pt9_totalFnctnlExpns < 0 ~ pt9_totalFnctnlExpns*(-1),
      TRUE ~ pt9_totalFnctnlExpns
    ),
    clean_totalFrgnGrnts = case_when(
      pt9_totalFrgnGrnts < 0 ~ pt9_totalFrgnGrnts*(-1),
      is.na(pt9_totalFrgnGrnts) == TRUE ~ 0,
      TRUE ~ pt9_totalFrgnGrnts
    )) %>%
  mutate(clean_FrgnExpnsPctg = clean_totalFrgnGrnts/clean_totalExpenses,
         clean_FrgnExpnsPctg = case_when(
           is.na(clean_FrgnExpnsPctg) == TRUE ~ 0,
           TRUE ~ clean_FrgnExpnsPctg
         ))
#--------------------------
#--------------------------
# Filtering observations; making a clean data set
#-------------------------- 
# I will drop the obs where expenses are = 0
# I will drop all values that are duplicated. Will only keep unique values.
# I will convert the gross expenses into millions of USD
# I will drop obs where the value for frgn expenses as a % of all expenses is strange
#   there are 13

frgnxpns_clean <- frgnxpns %>%
  filter(dup == 0) %>%
  filter(clean_totalExpenses > 0) %>%
  mutate(spent_abroad = case_when(
    clean_totalFrgnGrnts > 0 ~ 1,
    TRUE ~ 0
  ),
  factor_txyr = as.factor(tax_year),
  clean_totalFrgnGrnts_mm = clean_totalFrgnGrnts/1000000) %>%
  filter(clean_FrgnExpnsPctg < 1) %>%
  filter(tax_year < 2020,
         spent_abroad == 1) %>%
  mutate(state_factor = as.factor(rtrn_state),
         anti = case_when(
           ein == 410692230 ~ 1,
           ein == 530204604 ~ 1,
           ein == 237432162 ~ 1,
           ein == 382822017 ~ 1,
           ein == 134196230 ~ 1,
           ein == 382926822 ~ 1,
           ein == 237325778 ~ 1,
           ein == 132875808 ~ 1,
           ein == 810983298 ~ 1,
           TRUE ~ anti
         ),
         name = case_when(
           ein == 410692230 ~ "Billy Graham Evangelist Association",
           ein == 530204604 ~ "Fellowship Foundation",
           ein == 237432162 ~ "Intervarsity Christian Fellowship",
           ein == 237432162 ~ "Cato Institute",
           ein == 382822017 ~ "Bethany Christian Services",
           ein == 134196230 ~ "World Youth Alliance",
           ein == 382926822 ~ "ACTON Institute for the Study of Religion and Liberty",
           ein == 237325778 ~ "American Society for the Defense of Tradition, Family, Property",
           ein == 132875808 ~ "International Human Rights Group",
           ein == 810983298 ~ "Religious Freedom Institute",
           TRUE ~ name
         ),
         anti_factor = case_when(
           anti == 1 ~ "Anti-LGBTQ+",
           anti == 0 ~ "Non Anti-LGBTQ+"
         ),
         anti_factor = as.factor(anti_factor)) 

# length(unique(frgnxpns_clean$name)) # 26 anti-lgbtq+ orgs
# length(unique(frgnxpns_clean$ein))-length(unique(frgnxpns_clean$name)) # 14,545 non anti-lgtbq
#--------------------------
