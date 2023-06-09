

activities <- read_csv("/Volumes/SRC_DATA/000_f990_data/activities_country_230609.csv")

summary(activities$region2)
summary(activities$dest_asia_pacific)
summary(activities$dest_europe)
summary(activities$dest_africa)
summary(activities$dest_americas)
summary(activities$dest_multi)
summary(activities$dest_antarctica)
summary(activities$dest_noInfo)
summary(activities$total_regions)
summary(activities$dst_fctr_americas)
summary(activities$dst_fctr_africa)
summary(activities$dst_fctr_europe)
summary(activities$dst_fctr_asia_pacific)
summary(activities$dst_fctr)

#-------------
activities_clean <- activities %>%
  filter(
    total_regions != 0, # we have to account for total_regions == 0
    RgnTtlExpndtrsAmt > 0 # only positive values for expenses
  )
#-------------
# Summarized Activities - One Line per Org x Year
#-------------
activities_sum <- activities_clean %>%
  mutate(
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
      dest_africa == 1 & dest_multi == 0 ~ RgnTtlExpndtrsAmt,
      dest_africa == 1 & dest_multi == 1 ~ RgnTtlExpndtrsAmt/total_regions,
      TRUE ~ 0
    ),
    activities_multi = case_when(
      dest_multi == 1 ~ RgnTtlExpndtrsAmt,
      TRUE ~ 0
    )
  )

test <- activities_clean %>%
  arrange(id_ein) %>%
  slice(1:10)

test2 <- test %>%
filter(
  id_ein == "20140190934930089194815488"
)

# from here- single line with all countries
# good regional expenses

acts_sum <- activities_sum %>%
group_by(id_ein) %>% 
  summarise(
    TotalActivities_americas=sum(activities_americas),
    TotalActivities_asia_pacific=sum(activities_asia_pacific),
    TotalActivities_europe=sum(activities_europe),
    TotalActivities_africa=sum(activities_africa),
    TotalActivities_multi=sum(activities_multi),
    .groups = 'drop'
  )

# FOR WHEN WE WANT TO DEAL WITH DATA LOSS OF total_regions == 0
missing_regions <- activities %>%
  filter(
    total_regions == 0
  ) %>%
  group_by(f_location) %>%
  summarise(
    total_obs = n()
  ) %>%
  arrange(desc(total_obs))
