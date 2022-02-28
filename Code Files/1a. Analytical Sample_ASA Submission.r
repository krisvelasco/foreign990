## Project: Nonprofit Foreign Expenditures

## Overview: 
# This file uses the return header, part 0 and part 9 of the F990
# forms to produce an data set containing information
# on how much do anti-LGBTQ+ and non anti-LGBTQ+ non-profits
# spend abroad.

# The 'clean' data set should be revised.

# Fenton provided base 990 + Schedule I + Schedule F for all nonprofits
# These data are across 41 files -- so lots of data
# Note: These data are in CSV and quite messy. It'll take some work to clean

#Last updated: Feb. 16 2022 by Sebastian Rojas Cabal
# NOTE: On Feb 16, I changed one of the last filters such that
# there is no "bottom" year filter. Filters can then be done with plots, etc

#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
#--------------------------------------------------------

#--------------------------------------------------------
# Importing F990 data
#--------------------------------------------------------
  # -------------------------
  #   Return Header
  # -------------------------
rtrn <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/return_header.csv")
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
  # Part 0 of Form 990
  # -------------------------
part0 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/part_0.csv")
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
  # Part 9
  #--------------------------
part9 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/part_ix.csv") %>%
  rename(
    pt9_id = id,
    pt9_totalFrgnGrnts = FrgnGrnts_TtlAmt,
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
    pt9_totalFnctnlExpns
  )
  # -------------------------
  #--------------------------
  # Known anti-LGBTQ orgs
  #--------------------------
antilgbt <- read_excel("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/Select Anti-LGBT+ Organizations - Foreign Expenditures.xlsx") %>%
  distinct(name, ein) %>%
  mutate(anti = 1,
         ein2 = ein,
         ein = as.numeric(str_remove_all(ein2, "-"))) # removed the - in the string in order to make the records compatible
  # -------------------------

#--------------------------------------------------------
# Merging the data
#--------------------------------------------------------
# We use the anti_join function to get a sense of how much data will be
# lost during the merge.
# After confirming that very little or no data will be lost, we use
# the inner_join function. The inner_join function keeps only observations
# that match across both data sets.
  #--------------------------
  # Merging Return Header and Part 0
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
  # Merging Return Header and Part 9
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
    pt9_totalFrgnGrnts)

  #--------------------------
  # Merging the joined data sets
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
    pt9_totalFnctnlExpns, pt9_totalFrgnGrnts, FrgnExpnssPrctng,
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
  #--------------------------
  # Merging the joined data sets with the list of anti-lgbtq orgs
  #--------------------------  
# antjoin_lgbtq <- anti_join(antilgbt, join_rtrn09, by = c("ein"))
# Two organizations do not appear in the data, at least by these EINs
#   American Values (21762320)
#   Homeschool Legal Defense Association (521354365)

rtrn09_anti <- full_join(join_rtrn09, antilgbt, by = c("ein")) %>%
  mutate(
    anti = case_when(
      is.na(anti) == TRUE ~ 0,
      TRUE ~ 1
    ),
    anti_factor = case_when(
      anti == 1 ~ "Anti-LGBTQ+",
      anti == 0 ~ "Not Anti-LGBTQ+"
    ),
    anti_factor = as.factor(anti_factor)
  )
  #--------------------------
  
#--------------------------------------------------------
# Cleaning the data
#--------------------------------------------------------
  #--------------------------
  # Selecting relevant variables from the merged data sets
  #--------------------------  
# We will call the resulting data frame 'frgnxpns'
joindf <- rtrn09_anti %>%
  select(ein, object_id,
         tax_year, anti, anti_factor, name,
         rtrn_timestmp, rtrn_txyrbgndt, rtrn_txyrenddt,           
         pt9_totalFnctnlExpns, pt9_totalFrgnGrnts, FrgnExpnssPrctng,
         pt0_state, rtrn_state)

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

# we'll call our data frame frgnxpns
frgnxpns <- inner_join(joindf, dups, by = c("ein", "tax_year"))
  #--------------------------
  #--------------------------
  # Cleaning some relevant variables
  #-------------------------- 

frgnxpns <- frgnxpns %>%
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

#--------------------------------------------------------
# Exporting the data
#--------------------------------------------------------
write_csv(frgnxpns_clean, "/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/foreign_expenses.csv")
#--------------------------------------------------------

#--------------------------------------------------------
# Checking for other anti-LGBTQ organizations in the data
#   Findgins here are reflected in the final cleaning stage
#--------------------------------------------------------
  #--------
  # From OpenDemocracy
  #--------
bg <- filter(frgnxpns_clean, ein == "410692230") # yes
# Billy Graham Evangelist Association
fellowship <-filter(frgnxpns_clean, ein == "530204604") # yes
# Fellowship Foundation
ivarsity <-filter(frgnxpns_clean, ein == "362171714") # yes
# Intervarsity Christian Fellowship
cato <- filter(frgnxpns_clean, ein == "237432162") # yes
# Cato Institute
bethany <- filter(frgnxpns_clean, ein == "382822017") # yes
# Bethany Christian Services
wyouth <- filter(frgnxpns_clean, ein == "134196230") # yes
# World Youth Alliance
acton <- filter(frgnxpns_clean, ein == "382926822") # yes
# ACTON Institute for the Study of Religion and Liberty
dfnstrdtnfam <- filter(frgnxpns_clean, ein == "237325778") # yes
# American Society for the Defense of Tradition, Family, Property
internationalhrg <- filter(frgnxpns_clean, ein == "132875808") # yes
# International Human Rights Group
relfreedom <- filter(frgnxpns_clean, ein == "810983298") # yes
# Religious Freedom Institute

# Not there!

# federalist <- filter(frgnxpns_clean, ein == "363235550") # nope
# heritage <- filter(frgnxpns_clean, ein == "237327730") # no
# summit <- filter(frgnxpns_clean, ein == "730792333") # no
# heartbeat <- filter(frgnxpns_clean, ein == "237335592") # no
# liberty <- filter(frgnxpns_clean, ein == "592986294") # no
# lifesite <- filter(frgnxpns_clean, ein == "510634787") # no
# homeschool <- filter(frgnxpns_clean, ein == "541719605") # no
# capitol <- filter(frgnxpns_clean, ein == "913222222") # no
# exodus <- filter(frgnxpns_clean, ein == "262317116") # no
# houseofprayer <- filter(frgnxpns_clean, ein == "742938029") # no
# famrsrchcouncil <- filter(frgnxpns_clean, ein == "521792772") # no
# famwatch <- filter(frgnxpns_clean, ein == "860981658") # no

  #----------------------
  # From Southern Poverty Law Center / NONE
  #----------------------

hate19 <- read_csv("/Volumes/Google Drive/My Drive/F990/foreign990/Datasets/splc-hate-groups-2019.csv") %>%
  filter(Ideology == "Anti-LGBTQ") %>%
  select(-Year)

hate14 <- read_csv("/Volumes/Google Drive/My Drive/F990/foreign990/Datasets/splc-hate-groups-2014.csv") %>%
  filter(Ideology == "Anti-LGBTQ") %>%
  select(-Year)

hate <- bind_rows(hate19, hate14) %>%
  distinct()

# Traditional Values Coalition
traditional <- filter(frgnxpns_clean, ein == "330055498") # nope
# no record

# Abiding Truth Ministries
abiding <- filter(frgnxpns_clean, ein == "330774765") # nope
# no record

# World Congress of Families
# no record

# Westboro Baptist Church
# no record

# Warriors for Christ
# no record

# Verity Baptist Church
# no record

# United Families International
# no record

# True Light Pentecost Church
# no record

# Tom Brown Ministries
# no record

# The Pray in Jesus Name Project
# no record

# The Campus Ministry USA
# no record

# Sure Foundation Baptist Church
# no record

# Strong Hold Baptist Church
# no record

# Strong Hold Baptist Church
# no record

# Stedfast Baptist Church
# no record

# Scott Lively Ministries
# no record

# Save California
# no record

# Ruth Institute
ruth <- filter(frgnxpns_clean, ein == "463647313") # nope
# no record

# Revival Baptist Church
# no record

# Public Advocate of the United States
publicad <- filter(frgnxpns_clean, ein == "521112449") # nope
# no record

# Probe Ministries
probe <- filter(frgnxpns_clean, ein == "751416858") # nope
# no record

# Pilgrims Covenant Church
# no record

# Pass the Salt Ministries
passalt <- filter(frgnxpns_clean, ein == "300116360") # nope
# no record

# Pacific Justice Institute
pacific <- filter(frgnxpns_clean, ein == "911823641") # nope
# no record

# Mission America
missionam <- filter(frgnxpns_clean, ein == "311597212") # nope
# no record

# Mass Foundation
# no record

# Liberty Counsel
libcounsel <- filter(frgnxpns_clean, ein == "592986294") # nope
# no record

# Liberty Counsel
libcounsel <- filter(frgnxpns_clean, ein == "592986294") # nope
# no record

# Illinois Family Institute
ilfami <- filter(frgnxpns_clean, ein == "371265883") # nope
# no record

# Heterosexuals Organized for a Moral Environment 
# no record

# Generations
# no record

# First Works Baptist Church
# no record

# Family Research Institute
famresin <- filter(frgnxpns_clean, ein == "470649778") # nope
# no record

# Faithful Word Baptist Church
# no record

# Faithful Word Baptist Church
# no record

# Faith2Action
# no record

# Faith Baptist Church
# no record

# D. James Kennedy Ministries
# no record

# Concerned Christian Citizens
# no record

# Church Militant/St. Michael's Media
militant <- filter(frgnxpns_clean, ein == "852545748") # nope
# no record

# Chalcedon Foundation
# no record

# Center for Family and Human Rights (C-FAM)
# no record

# Bible Believers Fellowship
# no record

# ATLAH Media Network
# no record

# Americans for Truth About Homosexuality
# no record

# American Vision
amvision <- filter(frgnxpns_clean, ein == "581374143") # nope

# American Family Association
afamass <- filter(frgnxpns_clean, ein == "640607275") # nope

# All Scripture Baptist Church

# American College of Pediatricians
pediatricians <- filter(frgnxpns_clean, ein == "470886878") # nope

  #----------------------
  # From Kris' other list / NONE
  #----------------------
check <- filter(frgnxpns_clean, ein == "205181132" |
                  ein == "320378923"  |
                  ein == "341781994"  |
                  ein == "383945262"  |
                  ein == "453275881"  |
                  ein == "453719771"  |
                  ein == "460739644"  |
                  ein == "550576038"  |
                  ein == "581713618"  |
                  ein == "710669058"  |
                  ein == "721475326"  |
                  ein == "731323487"  |
                  ein == "742251033"  |
                  ein == "742481573"  |
                  ein == "815312632"  |
                  ein == "820419229"  |
                  ein == "840970592"  |
                  ein == "522120550")