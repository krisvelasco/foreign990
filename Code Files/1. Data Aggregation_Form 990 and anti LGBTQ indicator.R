## Project: Nonprofit Foreign Expenditures

## Overview: 
#   This file creates a dirty data set merging an indicator
#   of whether or not a nonprofit is anti- or non anti-LGBTQ+
#   with corresponding data from form 990 parts 0, 9, and the
#   return header.

## Last updated: June 30th by Sebastian Rojas Cabal
#   Sample of anti nonprofits is based on list of known nonprofits as well as organizations to which those
#   known nonprofits have donated money to.

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
orgs1 <- read_excel("/Users/srojascabal/Desktop/000_f990_data/anti_lgbtq_eins_20220422.xlsx") %>%
  rename(name = Organization,
         ein_char = ein) %>%
  mutate(ein_char = str_trim(ein_char),
         ein = as.numeric(ein_char)) %>%
  select(name, ein) %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE)
# List of first-order ties to known anti-LGBTQ+ nonprofits.
orgs2 <- read_csv("/Users/srojascabal/Desktop/000_f990_data/anti_lgbtq_candidates_20220516.csv") %>%
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
rtrn <- read_csv("/Users/srojascabal/Desktop/000_f990_data/return_header.csv")
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
part0 <- read_csv("/Users/srojascabal/Desktop/000_f990_data/part_0.csv")
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
part9 <- read_csv("/Users/srojascabal/Desktop/000_f990_data/part_ix.csv")

part9 <- part9 %>%
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
#   Exporting merged data set with all variables
#-------------------------- 
write_csv(rtrn09_anti, "Datasets/from990_rtrn09_anti_dirty.csv")
#--------------------------