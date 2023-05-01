# Foreign Spending by Region

#----
# Listo of Anti-LGBTQ+ Organization
#----
# List of known anti-LGBTQ+ nonprofits. 
orgs1 <- read_excel("/Volumes/SRC_DATA/000_f990_data/anti_lgbtq_eins_20220422.xlsx") %>%
  rename(name = Organization,
         ein_char = ein) %>%
  mutate(ein_char = str_trim(ein_char),
         ein = as.numeric(ein_char)) %>%
  select(name, ein) %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE)
# List of first-order ties to known anti-LGBTQ+ nonprofits.
orgs2 <- read_csv("/Volumes/SRC_DATA/000_f990_data/anti_lgbtq_candidates_20220516.csv") %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE)
# Binding all candidate anti-LGBTQ+ orgs
orgs_all <- bind_rows(orgs1, orgs2) %>%
  distinct(ein, .keep_all = TRUE) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(anti_lgbtq = 1)

#----
# Activities
#----
activities <- read_csv("/Volumes/SRC_DATA/000_f990_data/rtrn_activities.csv") %>%
  mutate(
    tax_year = rtrn_txyrend # tax year = end of the tax period in form
  ) %>%
  filter(
    TotalActivities_americas > -1,
    TotalActivities_asia_pacific > -1,
    TotalActivities_europe > -1,
    TotalActivities_africa > -1,
    TotalActivities_multi > -1,
    TotalActivities_noInfo > -1,
    pt1_f_totalActsSpent > -1,
  ) %>%
  mutate(
    TotalActivities_americas_2013 = case_when(
    TotalActivities_americas)
    TotalActivities_asia_pacific_2013 = TotalActivities_asia_pacific
    TotalActivities_europe_2013 = TotalActivities_europe
    TotalActivities_africa_2013 = TotalActivities_africa
    TotalActivities_multi_2013 = TotalActivities_multi
    TotalActivities_noInfo_2013 = TotalActivities_noInfo
    pt1_f_totalActsSpent_2013 = pt1_f_totalActsSpent
  )

acts_lgbtq <- left_join(activities, orgs_all) %>%
  mutate_at(c("anti_lgbtq"), ~replace_na(.,0))

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
  )

acts_sum <- acts_lgbtq %>%
  select(tax_year, anti_lgbtq, 
      TotalActivities_americas, TotalActivities_asia_pacific,
      TotalActivities_europe, TotalActivities_africa, TotalActivities_multi, TotalActivities_noInfo, pt1_f_totalActsSpent) %>%
  group_by(tax_year, anti_lgbtq) %>% 
  summarise(total_americas = sum(TotalActivities_americas),
            total_asia_pacific = sum(TotalActivities_asia_pacific),
            total_europe = sum(TotalActivities_europe),
            total_africa = sum(TotalActivities_africa),
            .groups = 'drop'
            ) %>%
  pivot_longer(
    cols = starts_with("total_"),
    names_to = "region",
    values_to = "total_acts"
  ) %>%
  mutate(
    anti_factor = as.factor(case_when(
      anti_lgbtq == 1 ~ "Anti-LGBTQ+",
      anti_lgbtq == 0 ~ "Non Anti-LGBTQ+",
    )),
    region_factor = as.factor(case_when(
      region == "total_americas" ~ "Americas",
      region == "total_africa" ~ "Africa",
      region == "total_europe" ~ "Europe",
      region == "total_asia_pacific" ~ "Asia/Pacific"
    )),
    taxyr_factor = as.factor(tax_year)
  ) %>%
  select(
    taxyr_factor,
    anti_factor,
    region_factor,
    total_acts
  ) %>%
  filter(
    !taxyr_factor == "2021"
  ) %>%
  mutate(
    total_acts_b = total_acts/1000000000 # billions
  )

#--------------------------
# Plot
#--------------------------
# Americas
acts_sum %>%
  filter(region_factor == "Americas") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_acts_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_acts_b, color = anti_factor)) +
  labs(y = "Americas (Activities)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Americas_Activities_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Africa
acts_sum %>%
  filter(region_factor == "Africa") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_acts_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_acts_b, color = anti_factor)) +
  labs(y = "Africa (Activities)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Africa_Activities_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Asia/Pacific
acts_sum %>%
  filter(region_factor == "Asia/Pacific") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_acts_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_acts_b, color = anti_factor)) +
  labs(y = "Asia/Pacific (Activities)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Asia_Activities_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Europe
acts_sum %>%
  filter(region_factor == "Europe") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_acts_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_acts_b, color = anti_factor)) +
  labs(y = "Europe (Activities)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Europe_Activities_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Spending by region, only for Anti-LGBTQ+ orgs
acts_sum %>%
  filter(anti_factor == "Anti-LGBTQ+") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_acts_b, group = region_factor, color = region_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_acts_b, color = region_factor)) +
  labs(y = "Spending in Foreign Activities\nby US-based Anti-LGBTQ+ Organizations",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Activities_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

#------------
# Grants
#------------
grants <- read_csv("/Volumes/SRC_DATA/000_f990_data/rtrn_grants.csv") %>%
  mutate(
    tax_year = rtrn_txyrend # tax year = end of the tax period in form
  )

grants_lgbtq <- left_join(grants, orgs_all) %>%
  mutate_at(c("anti_lgbtq"), ~replace_na(.,0))

grants_sum <- grants_lgbtq %>%
  select(tax_year, anti_lgbtq, 
         TotalCashGrants_americas,       
         TotalCashGrants_asia_pacific,   
         TotalCashGrants_europe,         
         TotalCashGrants_africa,         
         TotalNonCashGrants_americas,    
         TotalNonCashGrants_asia_pacific,
         TotalNonCashGrants_europe,      
         TotalNonCashGrants_africa) %>%
  group_by(tax_year, anti_lgbtq) %>% 
  summarise(total_cash_americas = sum(TotalCashGrants_americas),
            total_cash_asia_pacific = sum(TotalCashGrants_asia_pacific),
            total_cash_europe = sum(TotalCashGrants_europe),
            total_cash_africa = sum(TotalCashGrants_africa),
            total_noncash_americas = sum(TotalNonCashGrants_americas),
            total_noncash_asia_pacific = sum(TotalNonCashGrants_asia_pacific),
            total_noncash_europe = sum(TotalNonCashGrants_europe),
            total_noncash_africa = sum(TotalNonCashGrants_africa),
            .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = starts_with("total_"),
    names_to = "region",
    values_to = "total_grants"
  ) %>%
  mutate(
    anti_factor = as.factor(case_when(
      anti_lgbtq == 1 ~ "Anti-LGBTQ+",
      anti_lgbtq == 0 ~ "Non Anti-LGBTQ+",
    )),
    grant_type_factor = as.factor(case_when(
      region == "total_cash_americas" |
      region == "total_cash_africa" |
      region == "total_cash_europe" |
      region == "total_cash_asia_pacific" ~ "Cash",
      region == "total_noncash_americas" |
      region == "total_noncash_africa" |
      region == "total_noncash_europe" |
      region == "total_noncash_asia_pacific" ~ "Non-cash"
    )),
    region_factor = as.factor(case_when(
      region == "total_cash_americas" |
      region == "total_noncash_americas" ~ "Americas",
      region == "total_cash_africa" |
      region == "total_noncash_africa" ~ "Africa",
      region == "total_cash_europe" |
      region == "total_noncash_europe" ~ "Europe",
      region == "total_cash_asia_pacific" |
      region == "total_noncash_asia_pacific" ~ "Asia/Pacific"
    )),
    taxyr_factor = as.factor(tax_year)
  ) %>%
  select(
    taxyr_factor,
    anti_factor,
    region_factor,
    grant_type_factor,
    total_grants
  ) %>%
  filter(
    !taxyr_factor == "2021"
  ) %>%
  mutate(
    total_grants_b = total_grants/1000000000 # billions
  )

#--------------------------
# Plot
#--------------------------
# Americas - Cash
grants_sum %>%
  filter(region_factor == "Americas",
         grant_type_factor == "Cash") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_grants_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_grants_b, color = anti_factor)) +
  labs(y = "Americas (Cash Grants)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Americas_CashGrants_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Americas - Non-Cash
grants_sum %>%
  filter(region_factor == "Americas",
         grant_type_factor == "Non-cash") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_grants_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_grants_b, color = anti_factor)) +
  labs(y = "Americas (Non-Cash Grants)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Americas_NonCashGrants_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Africa - Cash
grants_sum %>%
  filter(region_factor == "Africa",
         grant_type_factor == "Cash") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_grants_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_grants_b, color = anti_factor)) +
  labs(y = "Africa (Cash Grants)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Africa_CashGrants_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Africa - Non-Cash
grants_sum %>%
  filter(region_factor == "Africa",
         grant_type_factor == "Non-cash") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_grants_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_grants_b, color = anti_factor)) +
  labs(y = "Africa (Non-Cash Grants)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Africa_NonCashGrants_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Europe - Cash
grants_sum %>%
  filter(region_factor == "Europe",
         grant_type_factor == "Cash") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_grants_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_grants_b, color = anti_factor)) +
  labs(y = "Europe (Cash Grants)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Europe_CashGrants_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Europe - Non-Cash
grants_sum %>%
  filter(region_factor == "Europe",
         grant_type_factor == "Non-cash") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_grants_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_grants_b, color = anti_factor)) +
  labs(y = "Europe (Non-Cash Grants)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Europe_NonCashGrants_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Asia/Pacific - Cash
grants_sum %>%
  filter(region_factor == "Asia/Pacific",
         grant_type_factor == "Cash") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_grants_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_grants_b, color = anti_factor)) +
  labs(y = "Asia/Pacific (Cash Grants)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Asia/Pacific_CashGrants_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Asia/Pacific - Non-Cash
grants_sum %>%
  filter(region_factor == "Asia/Pacific",
         grant_type_factor == "Non-cash") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_grants_b, group = anti_factor, color = anti_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_grants_b, color = anti_factor)) +
  labs(y = "Asia/Pacific (Non-Cash Grants)",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Asia/Pacific_NonCashGrants_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Cash grants by region, only for Anti-LGBTQ+ orgs
grants_sum %>%
  filter(anti_factor == "Anti-LGBTQ+",
         grant_type_factor == "Cash") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_grants_b, group = region_factor, color = region_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_grants_b, color = region_factor)) +
  labs(y = "Spending in Cash Grants\nby US-based Anti-LGBTQ+ Organizations",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Cash grants_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)

# Non-Cash grants by region, only for Anti-LGBTQ+ orgs
grants_sum %>%
  filter(anti_factor == "Anti-LGBTQ+",
         grant_type_factor == "Non-cash") %>%
  ggplot() +
  geom_line(aes(x = taxyr_factor, y = total_grants_b, group = region_factor, color = region_factor)) +
  geom_point(aes(x = taxyr_factor, y = total_grants_b, color = region_factor)) +
  labs(y = "Spending in Non-cash Grants\nby US-based Anti-LGBTQ+ Organizations",
       caption = "Amounts in US Billions, not adjusted for inflation") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank()) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "gray29")

# Export
ggsave("/Volumes/SRC_DATA/000_f990_data/Plots/230417_Non Cash grants_Spending by region.png",
       width = 210,
       height = 148, #Half of an A4
       units = "mm",
       dpi = "print",
)
