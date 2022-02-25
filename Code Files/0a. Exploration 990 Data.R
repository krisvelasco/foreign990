## Project: Nonprofit Foreign Expenditures
## Date: January 18, 2022
# Last updated: Feb. 9, 2022 by Sebastian Rojas Cabal

# Please refer to the following files for the clean versions of the work here
# Plots_ASA Submission.R
# Analytical Sample_ASA Submission.R
# Tables_ASA Submission.R

#---------------------------------------

## Overview: 
  # This file explores the contents of the schedule F portion of the 990 data
  # I also do some data cleaning and produces some preliminary plots
  # Fenton provided base 990 + Schedule I + Schedule F for all nonprofits
  # These data are across 41 files -- so lots of data
  # Note: These data are in CSV and quite messy. It'll take some work to clean
#Last updated: Feb. 4, 2022 by Sebastian Rojas Cabal

# Preliminaries
library(tidyverse)
library(lubridate)
library(readxl)
#library(countrycode) # I used to try out an efficient way of finding the places to which grants go.

# -------------------------
# Importing data
# -------------------------

# -------------------------
#   Return Header
# -------------------------
rtrn <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/return_header.csv")
  # these are the forms submissions with metadata from Amazon Web Services/IRS.
  length(unique(rtrn$id))/nrow(rtrn) # all ids are unique
  length(unique(rtrn$object_id))/nrow(rtrn) # object_ids are not unique. Parsing problem?
  # There are 3 types of forms here, We will restrict everything to 990.
  #   990 is for organizations that bring in more than $250,000 - 1,697,326 orgs in our sample.
  #   EZ is for those who bring in between $250,000 and $50,000 - 922,304 orgs in our sample.
  #   PF is for private foundations - 440,511 orgs in our sample.
rtrn990 <- filter(rtrn, RtrnHdr_RtrnCd == "990") %>%
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
  select(ein, object_id, rtrn_id,
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
# Part 0 of Form 990
# -------------------------
part0 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/part_0.csv")
  # the top of 990 form.
  length(unique(part0$id))/nrow(part0) # all ids are unique
  length(unique(part0$object_id))/nrow(part0) # object_ids are not unique. Parsing problem?

part0 <- part0 %>%
  rename(
    pt0_id = id,
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
  select(ein, object_id, pt0_id,
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
# Joining Part 0 and Header
# -------------------------

# Anti joins: what information, if any, would be lost?
#   Looks like no info will be lost. All combinations of ein and object_id show up in both
#     notInRt <- anti_join(part0, rtrn990, by = c("ein", "object_id")) # all pt0 are in rtrn990
#     notInp0 <- anti_join(rtrn990, part0, by = c("ein", "object_id")) # all rtrn990 are in pt0

join_pt0retrn <- left_join(rtrn990, part0, by = c("ein", "object_id"))
# the join has more observations because there might be repeated combinations of object_id and ein.

rtrn_p0 <- join_pt0retrn %>%
  select(ein, object_id,
         rtrn_id, pt0_id,
         rtrn_timestmp,
         rtrn_txyrbgndt, rtrn_txyrenddt, rtrn_txyrstart, rtrn_txyrend,  
         rtrn_USaddrs1, pt0_USaddrs1,
         rtrn_state, pt0_state,
         rtrn_county, pt0_county,
         rtrn_zip, pt0_zip,
         rtrn_name1, pt0_name1,
         rtrn_name2, pt0_name2,
         rtrn_USaddrs2, pt0_USaddrs2,
         rtrn_form, rtrn_EINfiler,
         pt0_grouprtrn, pt0_grouprtrn_allincluded, pt0_grouprtrn_exmptnmbr)

# -------------------------
#--------------------------
# Part 1
#--------------------------

part1 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/part_i.csv") %>%
  rename(
    pt1_id = id,
    pt1_totalRev = CYTtlRvnAmt,
    # Add later: Expenses: grants, benefits to members, salaries, fundraising expenses, other.
    # Add later: org discontinued?
    pt1_totalExp = CYTtlExpnssAmt) %>%
  select(
    pt1_id,
    ein,
    object_id,
    pt1_totalRev,
    pt1_totalExp
  )

# -------------------------
#--------------------------
# Part 8
#--------------------------

part8 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/part_viii.csv") %>%
  rename(
    pt8_id = id,
    pt8_totalRvn = TtlRvn_TtlRvnClmnAmt) %>%
  mutate(
    id = pt8_id
  ) %>%
  select(
    pt8_id,
    id,
    ein,
    object_id,
    pt8_totalRvn)

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
# Joining Header + Pt 0, and Part 9
#--------------------------
  
# Anti joins: what information, if any, would be lost?
#   Following exploration of the data, it looks like the id from part 0 can be matched quite well to
#   the id of part 1.
#       id_p0 <- select(part0, pt0_id, ein) %>%
#         mutate(part0 = "part0",
#                id = pt0_id)
#       id_p1 <- select(part1, pt1_id, ein) %>%
#         mutate(part1 = "part1",
#                id = pt1_id)
#       IDnotInPart0 <- anti_join(id_p1, id_p0, by = c("id"))
#       IDnotInPart1 <- anti_join(id_p0, id_p1, by = c("id")) # 84 obs in part 0 are not in part 1
     
join_pt01retrn <- left_join(rtrn_p0, part1, by = c("pt0_id" = "pt1_id")) %>%
  rename(ein = ein.x,
         ein_pt1 = ein.y,
         object_id = object_id.x,
         object_id_1 = object_id.y) %>%
  relocate(ein_pt1, .before = object_id) %>%
  relocate(object_id_1, .after = object_id) %>%
  mutate(ein_coincide = case_when(
    ein == ein_pt1 ~ 1,
    TRUE ~ 0
  ))
  # summary(join_pt01retrn$pt1_totalRev) # 128 missing values for expenses and revenues.
  # summary(join_pt01retrn$pt1_totalExp)
    summary(join_pt01retrn$ein_coincide)

n_distinct(join_pt01retrn)

ein_inc <- filter(join_pt01retrn, ein_coincide == 0)

dups <- join_pt01retrn %>% count(ein, rtrn_timestmp) %>%
  mutate(dup = case_when(
    n == 1 ~ 0,
    n > 1 ~ 1)) %>%
  group_by(ein, rtrn_txyrend) %>%
  select(ein, rtrn_txyrend, dup)


# Joining parts, then join to HEADER DATA
part1 <- part1 %>%
  mutate(id = pt1_id)

part0 <- part0 %>%
  mutate(id = pt0_id)

join_01_2 <- left_join(part0, part1, by = c("object_id", "ein", "id")) %>%
  mutate(id_coincide = case_when(
    pt0_id == pt1_id ~ 1,
    TRUE ~ 0
  ))

n_distinct(part0$ein)
n_distinct(part1$ein)
n_distinct(part9$ein)
n_distinct(rtrn990$ein)
# same # of unique EINs across

n_distinct(part0$object_id)
n_distinct(part1$object_id) # 109129 unique obj id
n_distinct(part9$object_id)
n_distinct(rtrn990$object_id)
# same # of unique object_ids except for part 1

n_distinct(part0$id)
n_distinct(part1$id)
n_distinct(part9$id)
n_distinct(rtrn990$rtrn_id)
# same # of unique ids



# Yet another mege

sort(colnames(rtrn990))

rtrn990 <- mutate(rtrn990,
                  object_id = object_id_rtrn,
                  ein = ein_rtrn,
                  id_rtrn = id)

part0 <- part0 %>%
  mutate(object_id_0 = object_id,
                  ein_0 = ein,
                  id_pt0 = pt0_id)

#part1 <- mutate(part1,
                object_id = object_id_1,
                ein = ein_1,
                id_pt1 = pt0_id)

# Part 0 and Retrurn header merge according to ein/object id
#   anti_rtrn0 <- anti_join(part0, rtrn990, by = c("ein", "object_id"))
#   anti_0rtn <- anti_join(rtrn990, part0, by = c("ein", "object_id"))

join_rtrn0 <- inner_join(rtrn990, part0, by = c("ein", "object_id")) %>%
  select(
        ein, ein_0, ein_rtrn,
        object_id, object_id_0, object_id_rtrn,
        id_pt0, id_rtrn,
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

# Part 1 and Retrurn header HAVE A MAJOR PROBLEM OF NON MATCHING EIN/OB IDS
#   anti_rtrn1 <- anti_join(part1, rtrn990, by = c("ein", "object_id"))
#   anti_1rtn <- anti_join(rtrn990, part1, by = c("ein", "object_id"))

# Part 9 and Retrurn header merge according to ein/object id
#    anti_rtrn9 <- anti_join(part9, rtrn990, by = c("ein", "object_id"))
#    anti_9rtn <- anti_join(rtrn990, part9, by = c("ein", "object_id"))
    
    join_rtrn9 <- inner_join(rtrn990, part9, by = c("ein", "object_id")) %>%
      mutate(FrgnExpnssPrctng = pt9_totalFrgnGrnts/pt9_totalFnctnlExpns) %>%
    # more rows mean combinations of ein x obj_id
    select(
    ein, ein_9, ein_rtrn, 
    id_rtrn, pt9_id,
    object_id, object_id_9, object_id_rtrn,
    FrgnExpnssPrctng,
    pt9_totalFnctnlExpns,
    pt9_totalFrgnGrnts)
    
# Part 8 and Return header HAVE A MAJOR PROBLEM OF NON MATCHING EIN/OBS ID
#    anti_rtrn8 <- anti_join(part8, rtrn990, by = c("ein", "object_id"))
#    anti_8rtn <- anti_join(rtrn990, part8, by = c("ein", "object_id"))

    
anti_9 <- anti_join(join_rtrn9, join_rtrn0, by = c("ein", "object_id"))
anti_rest <- anti_join(join_rtrn0, join_rtrn9, by = c("ein", "object_id"))

join_rtrn09 <- inner_join(join_rtrn9, join_rtrn0, by = c("ein", "object_id")) %>%
  select(
    ein, ein_0, ein_9, ein_rtrn.x,
    object_id, object_id_0, object_id_9, object_id_rtrn.x,
    id_pt0, pt9_id, id_rtrn.x,
    rtrn_timestmp, rtrn_txyrbgndt, rtrn_txyrend,            
    rtrn_txyrenddt, rtrn_txyrstart, 
    pt9_totalFnctnlExpns, pt9_totalFrgnGrnts, FrgnExpnssPrctng,
    pt0_state, rtrn_state) %>%
  rename(
    ein_rtrn = ein_rtrn.x,
    object_id_rtrn = object_id_rtrn.x,
    id_rtrn = id_rtrn.x
  ) %>%
  mutate(
    tax_year = rtrn_txyrend # tax year = end of the tax period in form
  )
# more obs because repeated combinations of object id x ein
#-----------------

#-----------------
# Let's work with this data
# data_distinct_expenses - only the ones without duplicates that spent money abroad
#-----------------

data <- join_rtrn09 %>%
  select(ein, object_id,
         rtrn_timestmp, tax_year,
         rtrn_timestmp, rtrn_txyrbgndt, rtrn_txyrenddt,           
         pt9_totalFnctnlExpns, pt9_totalFrgnGrnts, FrgnExpnssPrctng,
         pt0_state, rtrn_state)

dups <- data %>% count(ein, tax_year) %>%
  mutate(dup = case_when(
    n == 1 ~ 0,
    n > 1 ~ 1)) %>%
  group_by(ein, tax_year) %>%
  select(ein, tax_year, dup)

data_dup1 <- inner_join(data, dups, by = c("ein", "tax_year"))

data_distinct <- filter(data_dup1, dup == 0) #all data that isn't a repeated ob
 # we'll stick to these for now. We could still filter other dups

        #   data_OnlyDup <- filter(data_dup1, dup == 1) %>%
        #     group_by(ein, tax_year)
        #   
        #   n_distinct(data_OnlyDup)
        #   
        #   data_pureDups <- distinct(data_OnlyDup)

summary(data_distinct$pt9_totalFnctnlExpns)
summary(data_distinct$pt9_totalFrgnGrnts)

data_distinct <- data_distinct %>%
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

# I will drop the obs where expenses are = 0

data_distinct_expenses <- filter(data_distinct,
                                 clean_totalExpenses > 0) %>%
  mutate(spent_abroad = case_when(
    clean_totalFrgnGrnts > 0 ~ 1,
    TRUE ~ 0
  ),
    factor_txyr = as.factor(tax_year)) %>%
  filter(clean_FrgnExpnsPctg < 1) # dropping obs that have a weird pctng value. there are 13

# ----------
# Test visualization - only year avg pctg of expenditure
# ----------
data_distinct_expenses %>%
  filter(spent_abroad == 1) %>%
  group_by(factor_txyr) %>%
  summarise(
    expenses = mean(clean_FrgnExpnsPctg)
  ) %>%
  ggplot() +
  geom_point(aes(x = factor_txyr, y = expenses)) +
  labs(title = "Avg. Foreign Expenses\nas a % of Total Expenses",
       y = "Foreign Expenses as % of All Expenses")
# ----------

# ----------
# Test visualization - only year avg pctg of expenditure
# with indicator for anti-lgbtq
# ----------

# Known anti-LGBTQ orgs
antilgbt <- read_excel("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/Select Anti-LGBT+ Organizations - Foreign Expenditures.xlsx") %>%
  distinct(name, ein) %>%
  mutate(anti = 1,
         ein2 = ein,
         ein = as.numeric(str_remove_all(ein2, "-"))) # removed the - in the string in order to make the records compatible

anti_lgbtq <- anti_join(antilgbt, data_distinct_expenses, by = c("ein"))
  # American Values (21762320) and Homeschool Legal Defense Association (521354365) are not in the data

data_expenses_anti <- full_join(data_distinct_expenses, antilgbt, by = c("ein")) %>%
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

data_expenses_anti %>%
  filter(spent_abroad == 1) %>%
  group_by(factor_txyr, anti_factor) %>%
  summarise(
    expenses = mean(clean_FrgnExpnsPctg)
  ) %>%
  ggplot() +
  geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
  labs(title = "Avg. Foreign Expenses\nas a % of Total Expenses",
       y = "Foreign Expenses as % of All Expenses",
       caption = "N (non-anti): 15,266\nN (anti): 28") +
  theme(legend.position = "bottom")

  ggsave("National.png")

# ----------

# ----------
# VISUALIZATIONS
# ----------

# Data for viz
#   2014-2019
#   Only orgs that spent abroad
  
  data_viz <- data_expenses_anti %>%
    filter(tax_year > 2013 & tax_year < 2020,
           spent_abroad == 1) %>%
    mutate(state_factor = as.factor(rtrn_state))
  
  # length(unique(data_viz$name)) # 17 anti-lgbtq+ orgs
  # length(unique(data_viz$ein))-length(unique(data_viz$name)) # 14,124 non anti-lgtbq
  
# States with anti-lgbtq orgs
  
   #  data_viz %>%
   #    filter(anti == 1) %>%
   #    group_by(state_factor) %>%
   #    summarise(anti_orgs = sum(anti)) %>%
   #    filter(anti_orgs > 1)
  
  data_viz_states <- data_viz %>%
    filter(rtrn_state == "AZ" |
           rtrn_state == "CO" | 
           rtrn_state == "DC" | 
           rtrn_state == "NY" |
           rtrn_state == "VA" |
           rtrn_state == "WA")

# ----------
# NATIONAL - %
# ----------

  data_viz %>%
    group_by(factor_txyr, anti_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "How much money do anti-LBGTQ+ non-proftits spent \nabroad compared to non anti-LGTQ+ non-profits?",
         y = "Avg. Foreign Expenses as % of All Expenses",
         caption = "N (Non Anti-LGBTQ+): 14,124\nN (Anti-LGBTQ+): 17") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("National_Percentage.png")
    
# ----------
# NATIONAL - $$
# ----------   

  data_viz %>%
    group_by(factor_txyr, anti_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts)
    ) %>%
    mutate(expenses = expenses/1000000) %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "How much money do anti-LBGTQ+ non-proftits spent \nabroad compared to non anti-LGTQ+ non-profits?",
         y = "Average Foreign Expenses (USD Millions)",
         caption = "N (Non Anti-LGBTQ+): 14,124\nN (Anti-LGBTQ+): 17") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("National_Gross.png")  
  
# ----------    
  
# ----------
# Viz - by state
# with indicator for anti-lgbtq
# ----------

# ----------
# Arizona
# ---------- 
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "AZ") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Arizona: Comparing % of Expenses Spent Abroad",
         y = "Avg. Foreign Expenses as % of All Expenses") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("AZ_Percentage.png")
 
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts) 
    ) %>%
    mutate(expenses = expenses/1000000) %>%
    filter(state_factor == "AZ") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Arizona: Comparing Gross Expenses Spent Abroad",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("AZ_Gross.png")  
  
# ----------
# New York - NY
# ----------

  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "NY") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "New York: Comparing % of Expenses Spent Abroad",
         y = "Avg. Foreign Expenses as % of All Expenses") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("NY_Percentage.png")
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts) 
    ) %>%
    mutate(expenses = expenses/1000000) %>%
    filter(state_factor == "NY") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "New York: Comparing Gross Expenses Spent Abroad",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("NY_Gross.png") 
  
# ----------
# Colorado - CO
# ----------
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "CO") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Colorado: Comparing % of Expenses Spent Abroad",
         y = "Avg. Foreign Expenses as % of All Expenses") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("CO_Percentage.png")
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts) 
    ) %>%
    mutate(expenses = expenses/1000000) %>%
    filter(state_factor == "CO") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Colorado: Comparing Gross Expenses Spent Abroad",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("CO_Gross.png")
  
# ----------
# DC
# ----------
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "DC") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "DC: Comparing % of Expenses Spent Abroad",
         y = "Avg. Foreign Expenses as % of All Expenses") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("DC_Percentage.png")
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts) 
    ) %>%
    mutate(expenses = expenses/1000000) %>%
    filter(state_factor == "DC") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "DC: Comparing Gross Expenses Spent Abroad",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("DC_Gross.png") 
  
# ----------
# Virginia - WA
# ----------
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "VA") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Virginia: Comparing % of Expenses Spent Abroad",
         y = "Avg. Foreign Expenses as % of All Expenses") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("VA_Percentage.png")
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts) 
    ) %>%
    mutate(expenses = expenses/1000000) %>%
    filter(state_factor == "VA") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Virginia: Comparing Gross Expenses Spent Abroad",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("VA_Gross.png")
# ----------
# Washington - WA
# ----------
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    filter(state_factor == "WA") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Washington: Comparing % of Expenses Spent Abroad",
         y = "Avg. Foreign Expenses as % of All Expenses") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("WA_Percentage.png")
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts) 
    ) %>%
    mutate(expenses = expenses/1000000) %>%
    filter(state_factor == "WA") %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Washington: Comparing Gross Expenses Spent Abroad",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank())
  
  ggsave("WA_Gross.png")
# ----------
# Grid - All states
# ----------

  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_FrgnExpnsPctg)
    ) %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Comparing % of Expenses Abroad",
         y = "Avg. Foreign Expenses as % of All Expenses") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    facet_wrap(~ state_factor, nrow = 3, scales = "free_y") 
  
  ggsave("States_Percentage.png")
  
  data_viz_states %>%
    group_by(factor_txyr, anti_factor, state_factor) %>%
    summarise(
      expenses = mean(clean_totalFrgnGrnts)) %>%
    mutate(expenses = expenses/1000000) %>%
    ggplot() +
    geom_line(aes(x = factor_txyr, y = expenses, group = anti_factor, color = anti_factor)) +
    geom_point(aes(x = factor_txyr, y = expenses, color = anti_factor)) +
    labs(title = "Comparing Gross Expenses Abroad",
         y = "Avg. Foreign Expenses (USD Millions)") +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          legend.title = element_blank()) +
    facet_wrap(~ state_factor, nrow = 3, scales = "free_y")
  
  ggsave("States_Gross.png")
  
#-------------------

anti_orgs_table <- data_viz %>%
    filter(anti == 1) %>%
    select(name, factor_txyr, state_factor,
           clean_totalFrgnGrnts) %>%
    group_by(name, factor_txyr) %>%
    mutate(clean_totalFrgnGrnts = clean_totalFrgnGrnts/1000000) %>%
    spread(factor_txyr, clean_totalFrgnGrnts) %>%
    rename(
      FrgnGrnts_2014 = `2014`,
      FrgnGrnts_2015 = `2015`,
      FrgnGrnts_2016 = `2016`,
      FrgnGrnts_2017 = `2017`,
      FrgnGrnts_2018 = `2018`,
      FrgnGrnts_2019 = `2019`
    ) %>%
    rename(
      state = state_factor,
      expenses_2019 = FrgnGrnts_2019
    ) %>%
    mutate(
      expenses_pctchange_1419 = (expenses_2019/FrgnGrnts_2014)*100
    )
  
  anti_orgs_table_cln <- anti_orgs_table %>%
    select(name,
           state,
           expenses_2019,
           expenses_pctchange_1419)
  
# There's an org which does not show up because name = NA
  






  
  
  
  
  
  
  
  
  
  


pt9_id <- select(part9, id) %>%
  mutate(part = "9")
pt1_id <- select(part1, id) %>%
  mutate(part = "1")
pt0_id <- select(part0, id) %>%
  mutate(part = "0")

join_ids <- full_join(pt9_id, pt0_id, by = c("id"))
join_ids <- full_join(join_ids, pt1_id, by = c("id"))

part0 <- part0 %>%
  rename(
    object_id_0 = object_id,
    ein_0 = ein
  )

part1 <- part1 %>%
  rename(
    object_id_1 = object_id,
    ein_1 = ein
  )

part9 <- part9 %>%
  rename(
    object_id_9 = object_id,
    ein_9 = ein
  )

rtrn990 <- rtrn990 %>%
  rename(
    object_id_rtrn = object_id,
    ein_rtrn = ein
  )
  rtrn990 <- mutate(rtrn990, id = rtrn_id)

join_19 <- full_join(part1, part9, by = c("id"))
join_019 <- full_join(join_19, part0, by = c("id")) %>%
  mutate(
    ein_01 = case_when(
      ein_0 == ein_1 ~ 1,
      TRUE ~ 0),
    ein_19 = case_when(
      ein_1 == ein_9 ~ 1,
      TRUE ~ 0),
    ein_09 = case_when(
      ein_0 == ein_9 ~ 1,
      TRUE ~ 0)
  )

join_09 <- full_join(part0, part9, by = c("id")) %>%
  mutate(ein_09 = case_when(
    ein_0 == ein_9 ~ 1,
    TRUE ~ 0))

join_09rtrn <- anti_join(join_09, rtrn990, by = c("id" = "id"))


summary(join_09$ein_09)

join_019rtrn <- full_join(join_019, rtrn990, by = c("id" = "rtrn_id")) #%>%
  mutate(
    ein_coincide = case_when(
      ein_0 == ein_1 & ein_1 == ein_9 & ein_9 == ein_rtrn ~ 1,
      TRUE ~ 0))

summary(join_rtrn019$ein_coincide)

summary(join_01_2$id_coincide)

join_01_id <- full_join(part0, part1, by = c("id")) %>%
  rename(ein_0 = ein.x,
         ein_1 = ein.y,
         object_id_0 = object_id.x,
         object_id_1 = object_id.y) %>%
  relocate(ein_1, .after = ein_0) %>%
  relocate(object_id_1, .after = object_id_0) %>%
  relocate(id, .before = pt0_id) %>%
  relocate(pt1_id, .after = pt0_id) %>%
  mutate(ein_coincide = case_when(
    ein_0 == ein_1 ~ 1,
    TRUE ~ 0
  )) # only 50% of ein coincide. Shouldn't they coincide for all obs?



summary(join_01_id$ein_coincide)
  

#--------------------------
#Schedule F
#--------------------------
# Note: for now, only using "Activities" (Schedule F, Part 1, Line 3) and
# "Individual grants" (Schedule F, Part 2).
sched_f_i <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_f_i.csv") %>%
  rename(f_i_totalspent = TtlSpntAmt,
         f_i_id = id,
         f_i_records = GrntRcrdsMntndInd) %>%
  select(f_i_id, ein, object_id, f_i_records, f_i_totalspent)

# Join: Part 1 of Sched F and the rest
# We need one to be length = 1,620,944
IDnotMaster <- anti_join(join_pt01retrn, sched_f_i, by = c("pt0_id" = "f_i_id"))

# All in sched_f should have a match in the Join
IDnotF <- anti_join(sched_f_i, join_pt01retrn, by = c("ein", "object_id")) # this should be 0
IDnotMaster <-  anti_join(join_pt01retrn, sched_f_i, by = c("ein", "object_id"))
  # this should be very close to 1,620,944
  # its 1,620,909. Good enough.

all_expenses = left_join(join_pt01retrn, sched_f_i, by = c("ein", "object_id")) # 28 values dropped

#------------
# Cleaning the all_expenses data
#------------

nrow(all_expenses) # This data has 1,697,814 observations.

# sched_f variable: if total_spent is more than 0, then it is 1
all_expenses <- all_expenses %>%
  mutate(sched_f = case_when(
    f_i_totalspent > 0 ~ 1,
    TRUE ~ 0
  ))

sched_f <- all_expenses %>%
  filter(sched_f == 1) %>%
  mutate(ptgExp_abrd = f_i_totalspent/pt1_totalExp) %>%
  # technically, all values for ptgExp_abrd should be between 0 and 1
  # I am going to mark the strange values.
  mutate(revise_ob = case_when(
    ptgExp_abrd > 1 | ptgExp_abrd < 0 ~ 1,
    TRUE ~ 0)) %>% # 30% of the obs are weird like that :o
  mutate(eid_coincide = case_when(
    ein_pt1 == ein ~ 1,
    TRUE ~ 0), # it looks like only 48 % of eins match :o
    obj_coincide = case_when(
      object_id == object_id_1 ~ 1,
      TRUE ~ 0)) # it looks like 25% of the object_ids match

# First step is going to be to keep a single observation
# per ein x tax year
# Tax year will be measured by rtrn_txyrend

dups <- sched_f %>% count(ein, rtrn_timestmp) %>%
  mutate(dup = case_when(
    n == 1 ~ 0,
    n > 1 ~ 1)) %>%
  group_by(ein, rtrn_txyrend) %>%
  select(ein, rtrn_txyrend, dup)

all_expenses_dups <- left_join(all_expenses, dups, by = c("ein", "rtrn_txyrend"))

#------------

nrow(join_pt01retrn) - nrow(IDnotMaster)



sched_f_individ_grants <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_f_individ_grants.csv")

sched_f_grants_summary <- sched_f_individ_grants %>%
  group_by(id, ein) %>%
  summarise(grants_total = count(RgnTxt),
            grants_cash = sum(CshGrntAmt),
            grants_noncash = sum(NnCshAssstncAmt))


sched_f_activities <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_f_activities.csv")


#sched_f_ii <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_ii.csv")
#sched_f_iv <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_iv.csv")
sched_f_activities <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_f_activities.csv")
sched_f_individ_grants <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_f_individ_grants.csv")
#--------------------------








































#-----------------
# Old merging work
#-----------------
#           # Testing Joins    
#           inBoth <- inner_join(part0, rtrn990, by =c("id"))
#             (nrow(semi_test)/nrow(part0))*100 # 55.36137 % of observations in part0 have a match in the rtrn hdr data
#           
#           inBoth_ein <- inner_join(part0, rtrn990, by = c("ein", "object_id"))
#           
#           inBoth_ein_select <- inBoth_ein %>%
#             select(ein, id.x, id.y, object_id.x, object_id.y,
#                    DngBsnssAsNm_BsnssNmLn1Txt, DngBsnssAsNm_BsnssNmLn2Txt,
#                    WbstAddrssTxt, BsnssNm_BsnssNmLn1Txt, BsnssNm_BsnssNmLn2Txt,
#                    USAddrss_AddrssLn1Txt.x, USAddrss_AddrssLn1Txt.y,             
#                    USAddrss_AddrssLn2Txt.x, USAddrss_AddrssLn2Txt.y,             
#                    USAddrss_CtyNm.x, USAddrss_CtyNm.y,                    
#                    USAddrss_SttAbbrvtnCd.x, USAddrss_SttAbbrvtnCd.y,             
#                    USAddrss_ZIPCd.x, USAddrss_ZIPCd.y)
#           
#           inBoth_left <- left_join(rtrn990, part0, by = c("ein", "object_id"))
#             (nrow(inBoth_left)/nrow(part0))*100
#             
#           (nrow(inBoth_ein_select)/nrow(part0))*100
#           
#           inBoth_select <- inBoth %>%
#             select(ein.x, ein.y, id, PrncplOfcrBsnssNm_BsnssNmLn1Txt, BsnssNm_BsnssNmLn1Txt)
#           
#           notInRt <- anti_join(part0, rtrn990, by =c("id")) %>%
#             select(ein, id, object_id, USAddrss_SttAbbrvtnCd, PrncplOfcrBsnssNm_BsnssNmLn1Txt) %>%
#             rename(state = USAddrss_SttAbbrvtnCd,
#                    name = PrncplOfcrBsnssNm_BsnssNmLn1Txt)
#           
#           notInPt0 <- anti_join(rtrn990, part0, by =c("id")) %>%
#             select(ein, id, object_id, USAddrss_SttAbbrvtnCd, BsnssNm_BsnssNmLn1Txt) %>%
#             rename(state = USAddrss_SttAbbrvtnCd,
#                    name = BsnssNm_BsnssNmLn1Txt)
#           
#           sameObId_rtrn <- rtrn990 %>%
#             group_by(object_id) %>%
#             summarise(diff_eins = n_distinct(ein),
#                       diff_states = n_distinct(USAddrss_SttAbbrvtnCd),
#                       diff_id = n_distinct(id)) %>%
#             mutate(ein_rt = diff_eins,
#                    states_rt = diff_states,
#                    id_rt = diff_id) %>%
#             select(object_id, ein_rt, id_rt, states_rt)
#           
#           sameObId_pt0 <- part0 %>%
#             group_by(object_id) %>%
#             summarise(diff_eins = n_distinct(ein),
#                       diff_states = n_distinct(USAddrss_SttAbbrvtnCd),
#                       diff_id = n_distinct(id)) %>%
#             mutate(ein_pt0 = diff_eins,
#                    states_pt0 = diff_states,
#                    id_pt0 = diff_id) %>%
#             select(object_id, ein_pt0, id_pt0, states_pt0)
#           
#           inBothObId <- inner_join(sameObId_pt0, sameObId_rtrn, by = c("object_id")) %>%
#             mutate(diff_sum = case_when(
#               id_pt0 == id_rt & ein_pt0 == ein_rt & states_pt0 == states_rt ~ 0,
#               TRUE ~ 1
#             ))
#           # there is a perfect match in terms of object_ids!
#           
#           NoMatch <- filter(inBothObId, diff_sum == 1) %>%
#             mutate(
#               match_st = case_when(
#                 states_pt0 == states_rt ~ 1,
#                 TRUE ~ 0),
#               match_id = case_when(
#                 id_pt0 == id_rt ~ 1,
#                 TRUE ~ 0),
#               match_ein = case_when(
#                 ein_pt0 == ein_rt ~ 1,
#                 TRUE ~ 0)
#               )
#           # the difference comes from the number of states!
#           
#           # The intuition is that the inviable matches are caused by grouped forms
#           part0_groups <- part0 %>%
#             mutate(
#               grouped = case_when(
#                 GrpRtrnFrAffltsInd == 1 | GrpRtrnFrAffltsInd == "true" ~ 1,
#                 GrpRtrnFrAffltsInd ==0 | GrpRtrnFrAffltsInd == "false" ~ 0
#               )
#             )
#           
#           same_ein <- rtrn990 %>%
#             select(id, ein, Flr_EIN) %>%
#             mutate(same_ein = case_when(
#               ein == Flr_EIN ~ 1,
#               ein != Flr_EIN ~ 0
#             ))
#           
#           rtrn990_notMissing <- rtrn990 %>%
#             select(-Cols_AllMissing(.))
#           
#           rtrn990 %>% select_if(function(x) all(is.na(x))) %>% colnames()
#           
#           
#           notInPart0 
#           
#           DiffStates <- c(NoMatch$object_id)
#           
#           
#           
#           
#           NoMatchBind <- bind_rows(notInPt0, notInRt) %>%
#             group_by(object_id)
#-----------------

#-----------------
# Old duplicates work
#-----------------











# Import data

#Schedule F
# Note: for now, only using "Activities" (Schedule F, Part 1, Line 3) and
# "Individual grants" (Schedule F, Part 2).
sched_f_i <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_f_i.csv")
#sched_f_ii <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_ii.csv")
#sched_f_iv <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_iv.csv")
sched_f_activities <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_f_activities.csv")
getwsched_f_individ_grants <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/sched_f_individ_grants.csv")

# Parts of the form
part0 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/part_0.csv")
  part0_sample <- slice_head(part0, n = 10)
  
  part0_objectid <- select(part0, object_id, ein, USAddrss_SttAbbrvtnCd) %>%
    mutate(from = "part_0")
  
  part0_id <- select(part0, id, ein, USAddrss_SttAbbrvtnCd) %>%
    mutate(from = "part_0")
  
  header_objectid <- select(header, object_id, ein, RtrnHdr_RtrnTs) %>%
    mutate(from = "header")

  header_id <- select(header, id, ein, RtrnHdr_RtrnTs) %>%
    mutate(from = "header")
  
  
  join_obid <- left_join(part0_objectid, header_objectid, by = c("object_id"))
  
    semi_obid <- semi_join(header_objectid, part0_objectid, by = c("object_id")) %>%
      mutate(ein_header = ein) %>%
      select(-ein, -from)
      # (nrow(semi_obid)/nrow(part0))*100 ----- 100% of rows have a match in the return_header data
    semi_obid_0 <- semi_join(part0_objectid, header_objectid, by = c("object_id")) %>%
      mutate(ein_0 = ein) %>%
      select(-ein, -from)
    
    join_semi <- full_join(semi_obid, semi_obid_0, by = c("object_id")) %>%
      mutate(viable = case_when(
        ein_0 == ein_header ~ 1,
        ein_0 != ein_header ~ 0
      ))
    
    summary(join_semi$viable)
  
  join_id <- left_join(part0_id, header_id, by = c("id"))
    semi_id_0 <- semi_join(part0_id, header_id, by = c("id")) # just tells you how many can be matched
      # (nrow(semi_id_0)/nrow(part0))*100 ------- 99.11431 % of observations matched
    semi_id_header <- semi_join(header_id, part0_id, by = c("id"))
      # There are 1,682,293 rows that match based on id.
  
  
# Known anti-LGBTQ orgs
antilgbt <- read_excel("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/Select Anti-LGBT+ Organizations - Foreign Expenditures.xlsx") %>%
    distinct(name, ein) %>%
    mutate(anti = 1,
           ein2 = ein,
           ein = as.numeric(str_remove_all(ein2, "-"))) # removed the - in the string in order to make the records compatible

# Header
header_data <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/return_header.csv")
  header_sample1 <- slice_head(header, n=10)
  header_sample <- select(header, id, object_id, ein, RtrnHdr_RtrnTs)

test_join <- right_join(header, part0_sample, by = c("id", "object_id", "ein"))


#  header_select <- select(header, ein, RtrnHdr_RtrnTs, RtrnHdr_TxPrdBgnDt, RtrnHdr_TxPrdEndDt) %>% # only the vars we need
#  mutate(fiscal_year = year(RtrnHdr_TxPrdEndDt)) # creating the fiscal_year variable, will be useful later.
#  write_csv(header_select, "Datasets/return_header_select.csv") # write the 'select' df as a data frame. Makes import quicker
header_select <- read_csv("/Volumes/Google Drive/My Drive/F990/foreign990/Datasets/return_header_select.csv")
  
# JOIN: Header with anti-lgbtq indicator
  header_anti <- left_join(header_select, antilgbt, by = c("ein")) %>%
    mutate(anti = case_when(
      anti == 1 ~ 1,
      TRUE ~ 0),
      anti_factor = as_factor(case_when(anti == 1 ~ "Anti-LGBTQ+",
                                        anti == 0 ~ "Other NGOs"))) %>%
    mutate(FileTs = RtrnHdr_RtrnTs)
  
  # IMPORTANT NOTE: American Values (EIN: 521762320) and Homeschool Legal Defense Association (521354365)
  # do not have a matching EIN in the general NGO data frame.
    # Checking by their EINs in the general EIN data.
    # aval <- filter(header_select, ein == 521762320)
    # homeschool <- filter(header_select, ein == 521354365)
  
    # antijoin_header <- anti_join(antilgbt, header_select, by = c("ein")) # %>%
    #   mutate(anti = case_when(
    #     anti == 1 ~ 1,
    #     TRUE ~ 0),
    #     anti_factor = as_factor(case_when(anti == 1 ~ "Anti-LGBTQ+",
    #                                       anti == 0 ~ "Other NGOs")))  %>%
    #   mutate(FileTs = RtrnHdr_RtrnTs)
  
# REMOVING DUPLICATE ENTRIES EIN X FISCAL YEAR
  
  # 1. Checking for duplicated entries for ein x year. We keep most recent submission.
  
  # New df with dummy indicating duplicates.
  dups <- header_anti %>% count(ein, fiscal_year) %>%
    mutate(dup = case_when(
      n == 1 ~ 0,
      n > 1 ~ 1)) %>%
    group_by(ein, fiscal_year)
  
  # 2. Joining the df with the dummy variable to the header_anti df. We will keep only the most
  # recent submission to the IRS per year.
  
  # Full join with header data
    header_anti2 <- full_join(header_anti, dups, by = c("ein", "fiscal_year")) 
    
    # eins without multiple subs in a single fiscal year
    keep_unique <- filter(header_anti2, dup == 0) %>%
      mutate(obs_type = "unique entry")
    
          # Checking that indeed there are no duplicates here.
          # dups_nodups_check <- header_anti_nodups %>% count(ein, fiscal_year) %>%
          #   mutate(dup = case_when(
          #     n == 1 ~ 0,
          #     n > 1 ~ 1)) %>%
          #   group_by(ein, fiscal_year)
          # 
          # table(dups_nodups_check$dup) # we're good here! as expected
    
    # Contains ALL duplicated values
    header_anti_dups <- filter(header_anti2, dup == 1) %>%
      mutate(obs_type = case_when(
        dup == 1 ~ "duplicated entry - diff Ts"),
        to_keep = case_when(
          obs_type == "duplicated entry - diff Ts" ~ 2)
      )
    
    # Removes duplicated values for the same EIN in a single tax year.
    # Keeps most recent submission
    header_anti_dups_rm <- header_anti_dups %>%
      group_by(ein, fiscal_year) %>%
      slice_max(order_by = FileTs, n =1) %>% # removes duplicated observations for ein x year. Keeps most recent submission.
      mutate(to_keep = 1) %>% # 20436 have multiple entries for the same fiscal year
      select(ein, ein2, fiscal_year, anti, anti_factor, obs_type, to_keep,
             FileTs, RtrnHdr_RtrnTs, RtrnHdr_TxPrdBgnDt, RtrnHdr_TxPrdEndDt)
    
          # Checking that indeed there are no duplicates here.
          dups_dupsrm_check1 <- header_anti_dups_rm %>% count(ein, fiscal_year) %>%
            mutate(dup = case_when(
              n == 1 ~ 0,
              n > 1 ~ 1)) %>%
            group_by(ein, fiscal_year)
          
          table(dups_dupsrm_check1$dup) # looks like there are 287 duplicates still.
          # these 287 duplicates are: 282 duplicated entries (exactly the same throughout all variables),
          #                           5 entries with the same time stamp but different begin/end dates for tax year
          #                               there is no patter to the difference in dates, I call them "idiosyncratic"
          
          # Removing those weird duplicates
          # Ok this is what's going on here: some entries have exactly the same time stamp.
          # This renders those entries immune to the "sile_max" method
          
          dups_dupsrm_check2 <- dups_dupsrm_check1 %>%
            select(ein, fiscal_year, dup)
          
          header_anti_dups_rm2 <- full_join(header_anti_dups_rm, dups_dupsrm_check2, by = c("ein", "fiscal_year")) %>%
            mutate(obs_type = case_when(
              dup == 0 ~ "duplicated entry - multiple Ts"),
              to_keep = case_when(
                dup == 1 ~ 3
              )) %>% # 575 duplicated entries
            select(ein, ein2, fiscal_year, anti, anti_factor, obs_type, to_keep,
                   FileTs, RtrnHdr_RtrnTs, RtrnHdr_TxPrdBgnDt, RtrnHdr_TxPrdEndDt, dup)
          
          keep_multiEntry <- filter(header_anti_dups_rm2, dup == 0)
            
          dups2 <- filter(header_anti_dups_rm2, dup == 1) %>%
            mutate(to_keep = 1)
          
          dups2_rm <- distinct(dups2)  # why 292 obs?
              # 282 (x2) are exact duplicates - they escape the "slice_max" method
              # 10 that are idyosincratc
          
          # A final check
          # OK, here's what's happening: some entries with the same time stamp, for the same tax year,
          # have different start dates or end dates.
          # Those entries are immune to the 'distinct()' method.
          
          final_check <- dups2_rm %>% count(ein, fiscal_year) %>%
            mutate(dup2 = case_when(
              n == 1 ~ 0,
              n > 1 ~ 1)) %>%
            group_by(ein, fiscal_year)
          
          dups_dupsrm_check3 <- final_check %>%
            select(ein, fiscal_year, dup2)
          
          header_anti_dups_rm3 <- full_join(dups2_rm, dups_dupsrm_check3, by = c("ein", "fiscal_year")) %>%
            mutate(obs_type = case_when(
              dup2 == 1 ~ "idiosyncratic",
              dup2 == 0 ~ "duplicated entry - same Ts"),
              to_keep = case_when(
                dup2 == 1 ~ 1))
          
          keep_sameTs <- filter(header_anti_dups_rm3, obs_type == "duplicated entry - same Ts")
          
          # Solution will be to keep the longest taxable period in the entry.
          # For example:
          # EIN 311071836 return for 2014 has two entries:
          # One is from 2014-07-01 to 2014-11-30 (21 weeks aprox)
          # Another is from 2013-07-01 to 2014-06-30 (52 weeks)
          # We'll keep the longer ones! (Except for EIN 823273907 2019's submissions, which are the same length. I will keep the one that has 2019 dats only)
          # That said: these seem to be very idiosyncratic cases were the tax forms may have mistakes, etc.
          # I am going to flag them with a variable that allows us to drop them from the analytical sample
          # We do not know if they are mistakes, etc.
          
          # All the idiosyncratic duplicates
          dups3 <- filter(header_anti_dups_rm3, dup2 == 1) %>%
            mutate(time.interval = RtrnHdr_TxPrdBgnDt %--% RtrnHdr_TxPrdEndDt) %>%
            mutate(time.duration = as.duration(time.interval))
          
          # Vars 'obs_type' and 'to_keep' help us keep track of a) the type of duplicate,
          # b) whether or not they should be kept
          dups3.2 <- dups3 %>%
            slice_max(order_by = time.duration, n =1) %>% # keep the longest time duration
            mutate(to_keep2 = case_when(
              RtrnHdr_TxPrdBgnDt != "2018-02-28" ~ 1,
              RtrnHdr_TxPrdBgnDt == "2018-02-28" ~ 2)) # keep only the 2019 tax returns for EIN 823273907
          
          keep_idio <- dups3.2 %>%
          filter(to_keep2 == 1) %>%
            select(ein, ein2, fiscal_year, anti, anti_factor, obs_type, to_keep,
                   FileTs, RtrnHdr_RtrnTs, RtrnHdr_TxPrdBgnDt, RtrnHdr_TxPrdEndDt)
          

# MERGE ALL AND KEEP THE APPROPIATE OBSERVATIONS
          keep_idio <- keep_idio %>% 
            select(ein, ein2, fiscal_year, anti, anti_factor, obs_type,
                   FileTs, RtrnHdr_RtrnTs, RtrnHdr_TxPrdBgnDt, RtrnHdr_TxPrdEndDt)
          
          keep_multiEntry <- keep_multiEntry %>% 
            select(ein, ein2, fiscal_year, anti, anti_factor, obs_type,
                   FileTs, RtrnHdr_RtrnTs, RtrnHdr_TxPrdBgnDt, RtrnHdr_TxPrdEndDt)
            
          keep_sameTs <- keep_sameTs %>% 
            select(ein, ein2, fiscal_year, anti, anti_factor, obs_type,
                   FileTs, RtrnHdr_RtrnTs, RtrnHdr_TxPrdBgnDt, RtrnHdr_TxPrdEndDt)
          
          keep_unique <- keep_unique %>% 
            select(ein, ein2, fiscal_year, anti, anti_factor, obs_type,
                   FileTs, RtrnHdr_RtrnTs, RtrnHdr_TxPrdBgnDt, RtrnHdr_TxPrdEndDt)
          
          
          merged_df <- bind_rows(
            keep_idio, keep_multiEntry, keep_sameTs, keep_unique
          )
          
          table(merged_df$obs_type)
          
          check_merged <- distinct(merged_df)
          
          header_sample <- slice(header, 1:10) # trying to figure out unique ids
          length(unique(header$id))
          
# EXPORTING: Clean header data with anti-lgbtq indicator
    # write_csv()
          
# PART 0
# we will use jacob's identifier !!!! go back!
    
test_anti <- filter(header_anti2, anti == 1)
  
    group_by(ein) #%>%
    slice_max(order_by = FileTs, n = 1) # Removes multiple submissions in a single tax year. Keeps the most recent. 20436 obs removed.

# TRIAL - Regions spending
  where_activities$Region1 <- countrycode(sourcevar = where_activities$CleanRgn,
                                          origin = "country.name",
                                          destination = "continent")
  
                                  
# Part 0 - Basic info
part_0 <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/part_0.csv")
  test <- slice(part_0, 1:5)

# Part IV - Where they tell you if they send money abroad or not
part_iv <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/part_iv.csv")

# Sched F - Actvities
sched_f_activities <- read_csv("/Users/srojascabal/Google Drive/F990/Data from OneDrive/sched_f_activities.csv")

varnames.sched_f_i <- colnames(sched_f_i)
varnames.sched_f_ii <- colnames(sched_f_ii)
varnames.sched_f_iv <- colnames(sched_f_iv)
varnames.sched_f_activities <- colnames(sched_f_activities)
varnames.sched_f_individ_grants <- colnames(sched_f_individ_grants)

# "Activities" (Schedule F, Part 1, Line 3) and "Individual grants" (Schedule F, Part 2) seem to have a lot of the info.
# Does Individual grants correspond to Part 2 (to entities?) or Part 3 (to individuals?)

# Rough and dirty exploration of unique places places
where_activities <- select(sched_f_activities, RgnTxt) %>%
  mutate(ToLowerRgn = str_to_lower(RgnTxt)) %>%
  mutate(CleanRgn = str_replace_all(ToLowerRgn, "[:punct:]", "")) %>%
  select(CleanRgn) %>%
  distinct() # over 3k unique places. That's a lot!


# THEN, GO ON PART IV TO SEE IF THEY CLAIM TO GIVE MONEY ABROAD, GET THE % OF EXPENDETURES ABROAD
# TOTAL GRANTS, TOTAL MONEY SPENT ABROAD, WHERE?
