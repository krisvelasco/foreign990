# Merge across Sched F

f_1_act <- full_join(activities_cln, f_1_cln, by = ("id_ein")) %>% #keeping all obs in part 1 and activities
  
  f_join_1_activites <- anti_join(dirty_f_1, dirty_f_activities, by = ("id_ein"))
  # 7,285 rows in part 1 don't have a match in the activities
  f_join_activites_1 <- anti_join(dirty_f_activities, dirty_f_1, by = ("id_ein"))
  # 492 rows in activities don't have a match in part 1
  # There's a diff of 164,635 rows between them.
  
  random_f1_act <- slice_sample(f_join_activites_1, n = 10)
  
  f_grants_join <- anti_join(dirty_f_1, dirty_f_individualGrants2, by = ("id_ein"))
  # 866 rows in part 1 don't have a match in the activities
  grants_f_join <- anti_join(dirty_f_individualGrants2, dirty_f_1, by = ("id_ein"))
  # 165,484 rows in activities don't have a match in part 1
  # There's a diff of 164,635 rows between them.
  
  
  factivities <- dirty_f_activities %>% select(
    id, object_id, ein
  )
  
  fgrants <- dirty_f_individualGrants %>% select(
    id, object_id, ein
  )
  
  # f2_id <- dirty_f_2 %>% select(
  #   id, object_id, ein
  # )
  # 
  # f4_id <- dirty_f_4 %>% select(
  #   id, object_id, ein
  # )
  
  
  f_jacob_dups <- dirty_f_1 %>%
    group_by(ein, TtlSpntAmt) %>% # seeing if the same EIN spent the same in multiple years
    filter(n()>1) %>%
    ungroup() %>%
    arrange(ein)
  
  f_jacob_dups2 <- f_jacob_dups %>%
    group_by(ein, TtlSpntAmt) %>%
    filter(n()>1) %>%
    ungroup() %>%
    arrange(filer.ein) %>%
    group_by(filer.ein) %>%
    slice_max(returnts)
  
  
  
  
  
  
  
  
  
  # these are the forms submissions with metadata from Amazon Web Services/IRS.
  #   length(unique(rtrn$id))/nrow(rtrn) # all ids are unique
  #   length(unique(rtrn990$object_id))/nrow(rtrn990) # object_ids are not unique. Parsing problem?
  # There are 3 types of forms here, We will restrict everything to 990.
  #   990 is for organizations that bring in more than $250,000 - 1,697,326 orgs in our sample.
  #   EZ is for those who bring in between $250,000 and $50,000 - 922,304 orgs in our sample.
  #   PF is for private foundations - 440,511 orgs in our sample.
  #-------------
  # Merge:
  # Everything must be matched to pt0 of Form 990, where the yearly data is
  #-------------
  # -------------------------
  #   Importing Return Header of Form 990
  # -------------------------
  rtrn <- read_csv("/Volumes/SRC_DATA/000_f990_data/return_header.csv")
  # these are the forms submissions with metadata from Amazon Web Services/IRS.
  #   length(unique(rtrn$id))/nrow(rtrn) # all ids are unique
  #   length(unique(rtrn990$object_id))/nrow(rtrn990) # object_ids are not unique. Parsing problem?
  # There are 3 types of forms here, We will restrict everything to 990.
  #   990 is for organizations that bring in more than $250,000 - 1,697,326 orgs in our sample.
  #   EZ is for those who bring in between $250,000 and $50,000 - 922,304 orgs in our sample.
  #   PF is for private foundations - 440,511 orgs in our sample.
  rtrn990 <- filter(rtrn, RtrnHdr_RtrnCd == 990) %>%
    rename(
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
    select(ein, object_id,
           rtrn_timestmp,
           rtrn_txyrbegin,
           rtrn_txyrend)  %>%
    rename(rtrn_txyrbgndt = rtrn_txyrbegin,
           rtrn_txyrenddt = rtrn_txyrend) %>%
    mutate(rtrn_txyrstart = year(rtrn_txyrbgndt),
           rtrn_txyrend = year(rtrn_txyrenddt),
           id_ein = paste0(object_id, ein)) %>%
    relocate(rtrn_txyrstart, rtrn_txyrend, .before = rtrn_txyrbgndt) %>%
    relocate(id_ein, .before = ein)
  # -------------------------
  # -------------------------
  #   Checking that all the ein x object_id combos
  #   appear in the Return Header data
  #     From activities and individual grants
  # -------------------------
  rtrn_id <- select(rtrn990, id_ein) %>% distinct()
  id_grants <- select(grants_rgn, id_ein)
  id_acts <- select(activities_cln, id_ein)
  id_pt1 <- select(f_1_cln, id_ein)
  
  # anti_join() return all rows from x without a match in y.
  acts_noGrants <- anti_join(id_acts, id_grants) #40172.
  # Most orgs giving out grants don't have "activities".
  # This could make sense inasmuch as organizations can donate money without having formal activities (operations) on the ground.
  pt1_noGrants <- anti_join(id_pt1, id_grants)
  
  acts_return <- anti_join(id_acts, rtrn_id) # all of them have a match in the rtrn file!
  grants_return <- anti_join(id_grants, rtrn_id) # all of them have a match in the rtrn file!
  # -------------------------
  #   Joining Sched F, part 1 and activities
  # -------------------------
  f_1_act <- full_join(activities_cln, f_1_cln, by = ("id_ein")) %>% #keeping all obs in part 1 and activities
    rename(
      pt1_f_totalActsSpent = TtlSpntAmt
    ) %>%
    relocate(object_id, ein, .before = TotalActivities_americas) %>%
    mutate_at(c("pt1_f_totalActsSpent",
                "TotalActivities_americas",
                "TotalActivities_asia_pacific",
                "TotalActivities_europe",
                "TotalActivities_africa",
                "TotalActivities_multi",
                "TotalActivities_noInfo"), ~replace_na(.,0)) # Replacing NA values with zeros (0)
  # -------------------------
  #   Joining Return Header + part 1 and activities
  # -------------------------
  rtrn_act <- left_join(rtrn990, f_1_act, by = ("id_ein")) %>%
    rename(
      ein = ein.x,
      object_id = object_id.x
    ) %>%
    mutate(
      acts = case_when(
        !is.na(TotalActivities_americas) |
          !is.na(TotalActivities_asia_pacific) |
          !is.na(TotalActivities_europe) |
          !is.na(TotalActivities_africa) |
          !is.na(TotalActivities_multi) |
          !is.na(TotalActivities_noInfo) |
          !is.na(pt1_f_totalActsSpent) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(-ein.y, -object_id.y) %>%
    mutate_at(c("TotalActivities_americas",
                "TotalActivities_asia_pacific",
                "TotalActivities_europe",
                "TotalActivities_africa",
                "TotalActivities_multi",
                "TotalActivities_noInfo",
                "pt1_f_totalActsSpent"
    ),
    ~replace_na(.,0)) %>%
    filter(acts==1)
  # -------------------------
  #   Joining Return Header + part 1 and activities + individual grants
  # -------------------------
  rtrn_grants <- left_join(rtrn990, grants_rgn, by = ("id_ein")) %>%
    mutate(
      grants = case_when(
        !is.na(TotalCashGrants_americas) |
          !is.na(TotalCashGrants_asia_pacific) |
          !is.na(TotalCashGrants_europe) |
          !is.na(TotalCashGrants_africa) |         
          !is.na(TotalCashGrants_multi) |
          !is.na(TotalCashGrants_noInfo) |
          !is.na(TotalNonCashGrants_americas) |
          !is.na(TotalNonCashGrants_asia_pacific) |
          !is.na(TotalNonCashGrants_europe) |
          !is.na(TotalNonCashGrants_africa) |
          !is.na(TotalNonCashGrants_multi) |
          !is.na(TotalNonCashGrants_noInfo) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate_at(c("TotalCashGrants_americas",
                "TotalCashGrants_asia_pacific",
                "TotalCashGrants_europe",
                "TotalCashGrants_africa",
                "TotalCashGrants_multi",
                "TotalCashGrants_noInfo",
                "TotalNonCashGrants_americas",
                "TotalNonCashGrants_asia_pacific",
                "TotalNonCashGrants_europe",
                "TotalNonCashGrants_africa",
                "TotalNonCashGrants_multi",
                "TotalNonCashGrants_noInfo"
    ),
    ~replace_na(.,0))  %>%  # replacing NAs with zeros (0)
    filter(grants==1)
  
  # -------------------------
  # Exporting the data
  # -------------------------
  write_csv(rtrn_grants, "/Volumes/SRC_DATA/000_f990_data/rtrn_grants.csv")
  
  write_csv(rtrn_act, "/Volumes/SRC_DATA/000_f990_data/rtrn_activities.csv")