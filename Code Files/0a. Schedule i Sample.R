## Project: Nonprofit Foreign Expenditures

## Overview: 
# This file uses Schedule I from Form 990
# to compile a list of anti-LGBTQ+ non profit organizations.

#Last updated: April 21 by Sebastian Rojas Cabal
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
sched_i_1 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_i_i.csv") %>%
  mutate(grants_records_kept = case_when(
    GrntRcrdsMntndInd == "0" ~ 0,
    GrntRcrdsMntndInd == "false" ~ 0,
    GrntRcrdsMntndInd == "1" ~ 1,
    GrntRcrdsMntndInd == "true" ~ 1
  )) # created a new variable to standardize values in the dummy variable showing whether or not the orgs keeps records of its grants

sched_i_2 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_i_ii.csv")
sched_i_individ_grants <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_i_individ_grants.csv")
sched_i_recipients <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_i_recipient.csv")

# part 1: whether or not the org keeps a record of who it gives grants to
# part 2: total number of grant-receiving orgs that are section 501(c)(3), government; or other.
# recipients:
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