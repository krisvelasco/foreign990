## Project: Nonprofit Foreign Expenditures

## Overview: 
# This file uses Schedule I from Form 990
# to compile a list of anti-LGBTQ+ non profit organizations.

#Last updated: May 16 by Sebastian Rojas Cabal
# NOTE: On Feb 16, I changed one of the last filters such that
# there is no "bottom" year filter. Filters can then be done with plots, etc

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
# Importing Schedule I data
#--------------------------------------------------------
# List of knwon anti-LGBTQ+ nonprofits and their EINs
antilgbt <- read_excel("/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/anti_lgbtq_eins_20220422.xlsx") %>%
  rename(name_anti_list = Organization,
         ein_char = ein) %>%
  mutate(ein_char = str_trim(ein_char),
         ein = as.numeric(ein_char))

# Part 1
# sched_i_1 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_i_i.csv") %>%
#   mutate(grants_records_kept = case_when(
#     GrntRcrdsMntndInd == "0" ~ 0,
#     GrntRcrdsMntndInd == "false" ~ 0,
#     GrntRcrdsMntndInd == "1" ~ 1,
#     GrntRcrdsMntndInd == "true" ~ 1
#   )) # created a new variable to standardize values in the dummy variable showing whether or not the orgs keeps records of its grants

# Part 2. Grants to Domestic Organizations. Total number and type of orgs receiving grants.
# sched_i_2 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_i_ii.csv")

# Part 2. Grants to Domestic Organizations. Information on grants/recipients.
sched_i_recipients <- read_csv("/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/sched_i_recipient.csv") %>%
  mutate(ein_char = as.character(ein))

# Part 3. Grants to Domestic Individuals.
# sched_i_individ_grants <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_i_individ_grants.csv")

# part 1: whether or not the org keeps a record of who it gives grants to
# part 2: total number of grant-receiving orgs that are section 501(c)(3), government; or other.
# par 3: individual grants.
#------
  # RcpntBsnssNm_BsnssNmLn1Txt = Part 2. (a) Name and address of organization or government. Line 1
  # RcpntBsnssNm_BsnssNmLn2Txt = Part 2. (a) Name and address of organization or government. Line 2
  # RcpntTbl_RcpntEIN = Part 2. (b) Recipient EIN
  # RcpntTbl_IRCSctnDsc = Part 2. (c) IRC section (if applicable)
  # RcpntTbl_CshGrntAmt = Part 2. (d) Amount of cash grant
  # RcpntTbl_NnCshAssstncAmt = Part 2. (e) Amount of noncash assistance
  # RcpntTbl_VltnMthdUsdDsc = Part 2. (f) Method of valuation (book, FMV, appraisal, other)
  # RcpntTbl_NnCshAssstncDsc = Part 2. (g) Description of noncash assistance
  # RcpntTbl_PrpsOfGrntTxt = Part 2. (h) Purpose of grant or assistance
  # USAddrss_AddrssLn1Txt = US address of recipient. Line 1
  # USAddrss_AddrssLn2Txt  = US address of recipient. Line 2
  # USAddrss_CtyNm  = US address of recipient. County
  # USAddrss_SttAbbrvtnCd  = US address of recipient. State
  # USAddrss_ZIPCd  = US address of recipient. Zip
  # FrgnAddrss_AddrssLn1Txt = Foreign address of recipient. Line 1
  # FrgnAddrss_AddrssLn2Txt  = Foreign address of recipient. Line 2
  # FrgnAddrss_CtyNm  = Foreign address of recipient. City or county          
  # FrgnAddrss_PrvncOrSttNm  = Foreign address of recipient. Province or state
  # FrgnAddrss_CntryCd  = Foreign address of recipient. Country Code
  # FrgnAddrss_FrgnPstlCd  = Foreign address of recipient. Postal Code
#------
# indiv grants: 
#----
  # GrnTxt = Part 3. (a) Type of grant or assistance.
  # RcpntCnt = Part 3. (b) Number of recipients.
  # CshGrntAmt = Part 3. (c) Amount of cash grant
  # NnCashAssstncAmt = Part 3. (d) Amount of noncash assistance
  # VltnMthdUsdDsc = Part 3. (e) Method of valuation (book, FMV, appraisal, other)
  # NnCshAssstncDsc = Part 3. (f) Description of noncash assistance.
#----
# -------------------------

# -------------------------
# Join operations to retrieve information on nonprofits that have received grants from known anti-LGBTQ+ nonprofits
# -------------------------

granting_anti_orgs <- semi_join(antilgbt, sched_i_recipients,
                             by = "ein")
granting_vec <- granting_anti_orgs$ein

#----
# Grant recipients from list of known anti-LGBTQ+ orgs
#   We will call this list of recipients "recipients1"
#----
recipients1 <- sched_i_recipients %>%
  filter(ein %in% granting_vec) %>%
  select(
    ein,
    RcpntBsnssNm_BsnssNmLn1Txt,
    RcpntBsnssNm_BsnssNmLn2Txt,
    RcpntTbl_RcpntEIN,
    RcpntTbl_CshGrntAmt,
    RcpntTbl_NnCshAssstncAmt,
    RcpntTbl_PrpsOfGrntTxt,
    USAddrss_CtyNm,
    USAddrss_SttAbbrvtnCd
  ) %>%
  mutate(recipients_from = 0
  ) %>%
  select(
    RcpntTbl_RcpntEIN,
    RcpntBsnssNm_BsnssNmLn1Txt,
    recipients_from
  ) %>%
  rename(
    ein = RcpntTbl_RcpntEIN,
    name = RcpntBsnssNm_BsnssNmLn1Txt
  ) %>%
  distinct() %>%
  drop_na()

# Check for NAs in data frame
recipients1 %>% summarise(across(everything(), ~ sum(is.na(.)))) # check where are we have a lot of NAs
  # Given that for the 4,892 there are no values in the "Foreign Address" columns, I will drop those.
#----
#----
# Grant recipients from orgs that received grants from known anti-LGBTQ+ orgs
#   We will call them "recipients2"
#----

r1_granting_orgs <- semi_join(recipients1, sched_i_recipients,
                                by = "ein")
granting_vec1 <- r1_granting_orgs$ein

recipients2 <- sched_i_recipients %>%
  filter(ein %in% granting_vec1) %>%
  select(
    ein,
    RcpntBsnssNm_BsnssNmLn1Txt,
    RcpntBsnssNm_BsnssNmLn2Txt,
    RcpntTbl_RcpntEIN,
    RcpntTbl_CshGrntAmt,
    RcpntTbl_NnCshAssstncAmt,
    RcpntTbl_PrpsOfGrntTxt,
    USAddrss_CtyNm,
    USAddrss_SttAbbrvtnCd
  ) %>%
  mutate(recipients_from = 1
  ) %>%
  select(
    RcpntTbl_RcpntEIN,
    RcpntBsnssNm_BsnssNmLn1Txt,
    recipients_from
  ) %>%
  rename(
    ein = RcpntTbl_RcpntEIN,
    name = RcpntBsnssNm_BsnssNmLn1Txt
  ) %>%
  distinct() %>%
  drop_na()

#----
# List of candidate organizations on April 22. Included up to 2nd-order ties (orgs receiving money from orgs receiving money)
#----
antilgbt_candidates_2 <- bind_rows(recipients1, recipients2) %>%
  distinct(ein, name) %>%
  anti_join(antilgbt,
            by = "ein") %>%
  distinct(ein, name)
  # this data will have duplicate values for EIN-name combinations.

#----
# List of candidate organizations on May 16. Includes up to 1st-order ties (orgs receiving money from known anti-lgbtq+ orgs)
#----
antilgbt_candidates_1 <- recipients1 %>%
  distinct(ein, name) %>%
  anti_join(antilgbt,
            by = "ein") %>%
  distinct(ein, name)
# this data will have duplicate EIN values as well as org names. It contains all unique EIN-name combinations.
#----

#----
# Exporting the data
#----
# 2nd order ties
write_csv(antilgbt_candidates, "/Volumes/Google Drive/My Drive/F990/Data from OneDrive/anti_lgbtq_candidates_20220422.csv")
# 1st order ties
write_csv(antilgbt_candidates, "/Volumes/GoogleDrive/My Drive/F990/Data from OneDrive/anti_lgbtq_candidates_20220516.csv")
