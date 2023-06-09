#-------------
# Individual Grants
#-------------
dirty_f_individualGrants <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_individ_grants.csv")

grants <- dirty_f_individualGrants %>%
  select(-id) %>%
  mutate(
    id_ein = paste0(object_id, ein)
  ) %>%
  select(
    id_ein, object_id, ein, RgnTxt, CshGrntAmt, NnCshAssstncAmt
  ) %>%
  mutate_at(c('CshGrntAmt', "NnCshAssstncAmt"), ~replace_na(.,0)) # Replacing NA values with zeros (0)
#distinct(object_id, ein, id_ein)
#length(unique(grants$id_ein))/nrow(grants)
#length(unique(grants$id))/nrow(grants)

grants_id <- select(grants, id_ein, object_id, ein) %>% distinct()

grants_sum <- grants %>%
  group_by(id_ein, RgnTxt) %>%
  summarise(f.cashGrant=sum(CshGrntAmt),
            f.nonCashGrant=sum(NnCshAssstncAmt),
            .groups = 'drop')

grants_cln <- left_join(grants_sum, grants_id, by = ("id_ein")) %>%
  select(id_ein, object_id, ein, RgnTxt, f.cashGrant, f.nonCashGrant)

# Cleaning regional data
grants_rgn <- grants_cln %>%
  mutate(
    f_location = RgnTxt,
    f_location = str_to_lower(f_location, locale = "en"),
    region = str_extract_all(f_location, string_lower_regions),
    region = as.character(region),
    region2 = str_remove_all(region, "[()\"]"),
    region2 = case_when(region=="character(0)" ~ "NO REGION INFO",
                        TRUE ~ region2),
    region2= str_replace_all(region2, ", ", "_"),
    region2= case_when(str_detect(region2, "_") == TRUE ~ str_remove(region2, "c"),
                       TRUE ~ region2),
    dest_asia_pacific = case_when(str_detect(region2, "asia|pacific") == TRUE |
                                    str_detect(f_location, string_lower_asia) ~ 1,
                                  TRUE ~ 0),
    dest_europe = case_when(str_detect(region2, "europe") == TRUE |
                              str_detect(f_location, string_lower_europe) == TRUE~ 1,
                            TRUE ~ 0),
    dest_africa  = case_when(str_detect(region2, "africa") == TRUE |
                               str_detect(f_location, string_lower_africa) ~ 1,
                             TRUE ~ 0),
    dest_americas  = case_when(str_detect(region2, "america") == TRUE |
                                 str_detect(f_location, string_lower_americas) ~ 1,
                               TRUE ~ 0),
    dest_americas = case_when(
      f_location == "europe (including iceland and greenland)" |
        f_location == "europe/iceland and greenland" |                                             
        f_location == "europe (including iceland & greenland) - albania, andorra, austria, belgium" |
        f_location == "europe (including iceland & greenland)" |                                     
        f_location == "europe including iceland and greenland" |                                     
        f_location == "europe (including iceland & greenland) -" |
        f_location == "europe/iceland/greenland" |
        f_location == "europe (including inceland & greenland)" |                                   
        f_location == "europe (including iceland & greenland) - albania, andorra, austria, belgiu" |
        f_location == "europe (including iceland and greenland" |
        f_location == "europe(including iceland and greenland)" |
        f_location == "europe ( including iceland and greenland)" |
        f_location == "europe/inceland/greenland" ~ 0,
      TRUE ~ dest_americas
    ),
    dest_asia_pacific = case_when(
      f_location == "romania" |
        f_location == "timisoara, romania (europe)" |                                                
        f_location == "europe (including romania) -" |                                               
        f_location == "northeastern romania" |
        f_location == "tulcea, romania" |
        f_location == "timis county, romania" |
        f_location == "romania-eu" |         
        f_location == "e. europe-roman" ~ 0,
      TRUE ~ dest_asia_pacific
    ),
    dest_multi = case_when(dest_asia_pacific + dest_africa + dest_europe + dest_americas > 1 ~ 1,
                           TRUE ~ 0),
    dest_asia_pacific = case_when(dest_multi == 1 ~ 0,
                                  TRUE ~ dest_asia_pacific),
    dest_americas = case_when(dest_multi == 1 ~ 0,
                              TRUE ~ dest_americas),
    dest_europe = case_when(dest_multi == 1 ~ 0,
                            TRUE ~ dest_europe),
    dest_africa = case_when(dest_multi == 1 ~ 0,
                            TRUE ~ dest_africa),
    dst_fctr_americas = as.factor(
      case_when(
        dest_americas == 1 ~ "Americas",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_africa = as.factor(
      case_when(
        dest_africa == 1 ~ "Africa",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_europe = as.factor(
      case_when(
        dest_europe == 1 ~ "Europe",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_asia_pacific = as.factor(
      case_when(
        dest_asia_pacific == 1 ~ "Asia-Pacific",
        TRUE ~ "Other"
      )
    ),
    dst_fctr = as.factor(
      case_when(
        dest_multi == 1 ~ "Multiple regions",
        dest_multi == 0 & dest_americas == 1 ~ "Americas",
        dest_multi == 0 & dest_africa == 1 ~ "Africa",
        dest_multi == 0 & dest_asia_pacific == 1 ~ "Asia-Pacific",
        dest_multi == 0 & dest_europe == 1 ~ "Europe",
        dest_americas == 0 & dest_europe == 0 &
          dest_africa == 0 & dest_asia_pacific == 0 ~ "Pending region info"
      )
    ),
    cashGrants_americas = case_when(
      dest_americas == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_asia_pacific = case_when(
      dest_asia_pacific == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_europe = case_when(
      dest_europe == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_africa = case_when(
      dest_africa == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_multi = case_when(
      dest_multi == 1 ~ f.cashGrant,
      TRUE ~ 0
    ),
    cashGrants_noInfo = case_when(
      dst_fctr == "Pending region info" ~ f.cashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_americas = case_when(
      dest_americas == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_asia_pacific = case_when(
      dest_asia_pacific == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_europe = case_when(
      dest_europe == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_africa = case_when(
      dest_africa == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_multi = case_when(
      dest_multi == 1 ~ f.nonCashGrant,
      TRUE ~ 0
    ),
    NonCashGrants_noInfo = case_when(
      dst_fctr == "Pending region info" ~ f.nonCashGrant,
      TRUE ~ 0
    )
  ) %>%
  group_by(id_ein) %>% 
  summarise(
    TotalCashGrants_americas=sum(cashGrants_americas),
    TotalCashGrants_asia_pacific=sum(cashGrants_asia_pacific),
    TotalCashGrants_europe=sum(cashGrants_europe),
    TotalCashGrants_africa=sum(cashGrants_africa),
    TotalCashGrants_multi=sum(cashGrants_multi),
    TotalCashGrants_noInfo=sum(cashGrants_noInfo),
    TotalNonCashGrants_americas=sum(NonCashGrants_americas),
    TotalNonCashGrants_asia_pacific=sum(NonCashGrants_asia_pacific),
    TotalNonCashGrants_europe=sum(NonCashGrants_europe),
    TotalNonCashGrants_africa=sum(NonCashGrants_africa),
    TotalNonCashGrants_multi=sum(NonCashGrants_multi),
    TotalNonCashGrants_noInfo=sum(NonCashGrants_noInfo),
    .groups = 'drop'
  )

#--------
# Understanding data loss - Individual grants
#   Data loss in the grants_rgn file is due to Middle East and North Africa
#   and Russia and Newly Independent States
#   Moreover, multi=1 is hard to know how much is going to what region
#--------
grants_rgn_id <- grants_rgn %>%
  select(id_ein) %>%
  distinct()

grants_dataloss <- anti_join(grants_id, grants_rgn_id) # it is 0 because we corrected for the multiple regions problem
grants_lostID <- grants_dataloss$id_ein

grants_lost_df <- grants_cln %>%
  filter(id_ein %in% grants_lostID) %>%
  mutate(
    f_location = RgnTxt,
    f_location = str_to_lower(f_location, locale = "en"),
    region = str_extract_all(f_location, string_lower_regions),
    region = as.character(region),
    region2 = str_remove_all(region, "[()\"]"),
    region2 = case_when(region=="character(0)" ~ "NO REGION INFO",
                        TRUE ~ region2),
    region2= str_replace_all(region2, ", ", "_"),
    region2= case_when(str_detect(region2, "_") == TRUE ~ str_remove(region2, "c"),
                       TRUE ~ region2),
    dest_asia_pacific = case_when(str_detect(region2, "asia|pacific") == TRUE |
                                    str_detect(f_location, string_lower_asia) ~ 1,
                                  TRUE ~ 0),
    dest_europe = case_when(str_detect(region2, "europe") == TRUE |
                              str_detect(f_location, string_lower_europe) == TRUE~ 1,
                            TRUE ~ 0),
    dest_africa  = case_when(str_detect(region2, "africa") == TRUE |
                               str_detect(f_location, string_lower_africa) ~ 1,
                             TRUE ~ 0),
    dest_americas  = case_when(str_detect(region2, "america") == TRUE |
                                 str_detect(f_location, string_lower_americas) ~ 1,
                               TRUE ~ 0),
    dest_multi = case_when(dest_asia_pacific + dest_africa + dest_europe + dest_americas > 1 ~ 1,
                           TRUE ~ 0),
    dest_total = dest_asia_pacific + dest_africa + dest_europe + dest_americas,
    dst_fctr_americas = as.factor(
      case_when(
        dest_americas == 1 ~ "Americas",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_africa = as.factor(
      case_when(
        dest_africa == 1 ~ "Africa",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_europe = as.factor(
      case_when(
        dest_europe == 1 ~ "Europe",
        TRUE ~ "Other"
      )
    ),
    dst_fctr_asia_pacific = as.factor(
      case_when(
        dest_asia_pacific == 1 ~ "Asia-Pacific",
        TRUE ~ "Other"
      )
    ),
    dst_fctr = as.factor(
      case_when(
        dest_multi == 1 ~ "Multiple regions",
        dest_multi == 0 & dest_americas == 1 ~ "Americas",
        dest_multi == 0 & dest_africa == 1 ~ "Africa",
        dest_multi == 0 & dest_asia_pacific == 1 ~ "Asia-Pacific",
        dest_multi == 0 & dest_europe == 1 ~ "Europe",
        dest_americas == 0 & dest_europe == 0 &
          dest_africa == 0 & dest_asia_pacific == 0 ~ "Pending region info"
      )
    )
  ) #%>%
mutate(
  cashGrants_americas = case_when(
    dest_americas == 1 ~ f.cashGrant,
    TRUE ~ 0
  ),
  cashGrants_asia_pacific = case_when(
    dest_asia_pacific == 1 ~ f.cashGrant,
    TRUE ~ 0
  ),
  cashGrants_europe = case_when(
    dest_europe == 1 ~ f.cashGrant,
    TRUE ~ 0
  ),
  cashGrants_africa = case_when(
    dest_africa == 1 ~ f.cashGrant,
    TRUE ~ 0
  ),
  cashGrants_multi = case_when(
    dest_multi == 1 ~ f.cashGrant,
    TRUE ~ 0
  ),
  cashGrants_noInfo = case_when(
    dst_fctr == "Pending region info" ~ f.cashGrant,
    TRUE ~ 0
  ),
  NonCashGrants_americas = case_when(
    dest_americas == 1 ~ f.nonCashGrant,
    TRUE ~ 0
  ),
  NonCashGrants_asia_pacific = case_when(
    dest_asia_pacific == 1 ~ f.nonCashGrant,
    TRUE ~ 0
  ),
  NonCashGrants_europe = case_when(
    dest_europe == 1 ~ f.nonCashGrant,
    TRUE ~ 0
  ),
  NonCashGrants_africa = case_when(
    dest_africa == 1 ~ f.nonCashGrant,
    TRUE ~ 0
  ),
  NonCashGrants_multi = case_when(
    dest_multi == 1 ~ f.nonCashGrant,
    TRUE ~ 0
  ),
  NonCashGrants_noInfo = case_when(
    dst_fctr == "Pending region info" ~ f.nonCashGrant,
    TRUE ~ 0
  )
)