


#-------------
#-------------
# Summarized Activities - One Line per Org x Year
#-------------
# Important to decide: money (RgnTtlExpndtrsAmt) in -, those obs will be deleted, rights?
activities_americas = case_when(
  dest_americas == 1 & dest_multi == 0 ~ RgnTtlExpndtrsAmt,
  dest_americas == 1 & dest_multi == 1 ~ RgnTtlExpndtrsAmt/total_regions,
  TRUE ~ 0
),
activities_asia_pacific = case_when(
  dest_asia_pacific == 1 & dest_multi == 0 ~ RgnTtlExpndtrsAmt,
  dest_asia_pacific == 1 & dest_multi == 1 ~ RgnTtlExpndtrsAmt/total_regions,
  TRUE ~ 0
),
activities_europe = case_when(
  dest_europe == 1 & dest_multi == 0 ~ RgnTtlExpndtrsAmt,
  dest_europe == 1 & dest_multi == 1 ~ RgnTtlExpndtrsAmt/total_regions,
  TRUE ~ 0
),
activities_africa = case_when(
  dest_africa == 1 ~ & dest_multi == 0 ~ RgnTtlExpndtrsAmt,
  dest_africa == 1 & dest_multi == 1 ~ RgnTtlExpndtrsAmt/total_regions,
  TRUE ~ 0
),
activities_multi = case_when(
  dest_multi == 1 ~ RgnTtlExpndtrsAmt,
  TRUE ~ 0
),
activities_noInfo = case_when(
  dst_fctr == "Pending region info" ~ RgnTtlExpndtrsAmt,
  TRUE ~ 0
)
) %>%
  filter(
    total_regions != 0 # we have to account for total_regions == 0
  )
group_by(id_ein) %>% 
  summarise(
    TotalActivities_americas=sum(activities_americas),
    TotalActivities_asia_pacific=sum(activities_asia_pacific),
    TotalActivities_europe=sum(activities_europe),
    TotalActivities_africa=sum(activities_africa),
    TotalActivities_multi=sum(activities_multi),
    TotalActivities_noInfo=sum(activities_noInfo),
    .groups = 'drop'
  )

# FOR WHEN WE WANT TO DEAL WITH DATA LOSS OF total_regions == 0
missing_regions <- activities_cln %>%
  filter(
    total_regions == 0
  ) %>%
  group_by(f_location) %>%
  summarise(
    total_obs = n()
  ) %>%
  arrange(desc(total_obs))