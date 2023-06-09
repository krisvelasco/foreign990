## Project: Nonprofit Foreign Expenditures

# Summarizing data on foreign activities (Schedule F of Form 990)
#   Last updated: June 8, 2023

# Output:
#   XXXXXXX
#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(xml2)
#--------------------------------------------------------
#--------------------------------------------------------
# Importing data and cleaning data
#--------------------------------------------------------
options(scipen=999) # Telling R to avoid the scientific notation altogether
#--------------------------
# Strings of region and country names
#--------------------------
# Country and region names from UN
unsd <- read_delim("/Volumes/SRC_DATA/000_f990_data/UNSD.csv",
                   delim = ";")
# Source: https://unstats.un.org/unsd/methodology/m49/overview/
string_regions <- c("America|Europe|Africa|Asia|Pacific|Oceania")
string_lower_regions <- str_to_lower(string_regions, locale = "en")

# Africa
list_africa <- unsd %>%
  filter(`Region Name` == "Africa") %>%
  select(`Country or Area`)

string_africa <- str_c(list_africa$`Country or Area`, collapse = "|")
string_lower_africa <- str_to_lower(string_africa, locale = "en")

# Europe
list_europe <- unsd %>%
  filter(`Region Name` == "Europe") %>%
  select(`Country or Area`)

string_europe <- str_c(list_europe$`Country or Area`, collapse = "|")
string_lower_europe <- str_to_lower(string_europe, locale = "en")

# Americas
list_americas <- unsd %>%
  filter(`Region Name` == "Americas") %>%
  select(`Country or Area`)

string_americas <- str_c(list_americas$`Country or Area`, collapse = "|")
string_lower_americas <- str_to_lower(string_americas, locale = "en")

# Asia-Pacific
list_asia_pacific  <- unsd %>%
  filter(`Region Name` == "Asia" |
           `Region Name` == "Oceania") %>%
  select(`Country or Area`)

string_asia_pacific <- str_c(list_asia_pacific$`Country or Area`, collapse = "|")
string_lower_asia <- str_to_lower(string_asia_pacific, locale = "en")
# -------------------------
#--------------------------
# Preliminary Activities Data
#--------------------------
activities_prelim <- read_csv("/Volumes/SRC_DATA/000_f990_data/activities_clean_prelim_230608.csv")
#-------------
#-------------
# Making Country Dummy Columns
#-------------

africa_colnames <- pull(list_africa, `Country or Area`)
  africa_colnames <- str_to_lower(africa_colnames)
  africa_colnames2 <- str_replace_all(africa_colnames,
                                     " ",
                                     "_")
  
americas_colnames <- pull(list_americas, `Country or Area`)
  americas_colnames <- str_to_lower(americas_colnames)
  americas_colnames2 <- str_replace_all(americas_colnames,
                                      " ",
                                      "_")
    
activities_country <- activities_prelim %>%
  mutate(
    afr_algeria = case_when(
      str_detect(f_location, "algeria") == TRUE ~ 1,
      str_detect(f_location, "algeria") == FALSE ~ 0
    ),
    afr_egypt = case_when(
      str_detect(f_location, "egypt") == TRUE ~ 1,
      str_detect(f_location, "egypt") == FALSE ~ 0
    ),
    afr_libya = case_when(
      str_detect(f_location, "libya") == TRUE ~ 1,
      str_detect(f_location, "libya") == FALSE ~ 0 
    ),
    afr_morocco = case_when(
      str_detect(f_location, "morocco") == TRUE ~ 1,
      str_detect(f_location, "morocco") == FALSE ~ 0
    ),
    afr_sudan = case_when(
      str_detect(f_location, "sudan") == TRUE ~ 1,
      str_detect(f_location, "sudan") == FALSE ~ 0
    ),
    afr_tunisia = case_when(
      str_detect(f_location, "tunisia") == TRUE ~ 1,
      str_detect(f_location, "tunisia") == FALSE ~ 0
    ),
    afr_western_sahara = case_when(
      str_detect(f_location, "western sahara") == TRUE ~ 1,
      str_detect(f_location, "western sahara") == FALSE ~ 0
    ),
    afr_british_indian_ocean_territory = case_when(
      str_detect(f_location, "british indian ocean territory") == TRUE ~ 1,
      str_detect(f_location, "british indian ocean territory") == FALSE ~ 0
    ),
    afr_burundi = case_when(
      str_detect(f_location, "burundi") == TRUE ~ 1,
      str_detect(f_location, "burundi") == FALSE ~ 0     
    ),
    afr_comoros = case_when(
      str_detect(f_location, "comoros") == TRUE ~ 1,
      str_detect(f_location, "comoros") == FALSE ~ 0      
    ),
    afr_djibouti = case_when(
      str_detect(f_location, "djibouti") == TRUE ~ 1,
      str_detect(f_location, "djibouti") == FALSE ~ 0      
    ),
    afr_eritrea = case_when(
      str_detect(f_location, "eritrea") == TRUE ~ 1,
      str_detect(f_location, "eritrea") == FALSE ~ 0      
    ),
    afr_ethiopia = case_when(
      str_detect(f_location, "ethiopia") == TRUE ~ 1,
      str_detect(f_location, "ethiopia") == FALSE ~ 0
    ),
    afr_french_southern_territories = case_when(
      str_detect(f_location, "french southern territories") == TRUE ~ 1,
      str_detect(f_location, "french southern territories") == FALSE ~ 0      
    ),
    afr_kenya = case_when(
      str_detect(f_location, "kenya") == TRUE ~ 1,
      str_detect(f_location, "kenya") == FALSE ~ 0
    ),
    afr_madagascar = case_when(
      str_detect(f_location, "madagascar") == TRUE ~ 1,
      str_detect(f_location, "madagascar") == FALSE ~ 0       
    ),
    afr_malawi = case_when(
      str_detect(f_location, "malawi") == TRUE ~ 1,
      str_detect(f_location, "malawi") == FALSE ~ 0
    ),
    afr_mauritius = case_when(
      str_detect(f_location, "mauritius") == TRUE ~ 1,
      str_detect(f_location, "mauritius") == FALSE ~ 0
    ),
    afr_mayotte = case_when(
      str_detect(f_location, "mayotte") == TRUE ~ 1,
      str_detect(f_location, "mayotte") == FALSE ~ 0
    ),
    afr_mozambique = case_when(
      str_detect(f_location, "mozambique") == TRUE ~ 1,
      str_detect(f_location, "mozambique") == FALSE ~ 0     
    ),
    afr_reunion = case_when(
      str_detect(f_location, "réunion") == TRUE ~ 1,
      str_detect(f_location, "réunion") == FALSE ~ 0
    ),
    afr_rwanda = case_when(
      str_detect(f_location, "rwanda") == TRUE ~ 1,
      str_detect(f_location, "rwanda") == FALSE ~ 0   
    ),
    afr_seychelles = case_when(
      str_detect(f_location, "seychelles") == TRUE ~ 1,
      str_detect(f_location, "seychelles") == FALSE ~ 0 
    ),
    afr_somalia = case_when(
      str_detect(f_location, "somalia") == TRUE ~ 1,
      str_detect(f_location, "somalia") == FALSE ~ 0 
    ),
    afr_south_sudan = case_when(
      str_detect(f_location, "south sudan") == TRUE ~ 1,
      str_detect(f_location, "south sudan") == FALSE ~ 0  
    ),
    afr_uganda = case_when(
      str_detect(f_location, "uganda") == TRUE ~ 1,
      str_detect(f_location, "uganda") == FALSE ~ 0  
    ),
    afr_united_republic_of_tanzania = case_when(
      str_detect(f_location, "united republic of tanzania") == TRUE ~ 1,
      str_detect(f_location, "united republic of tanzania") == FALSE ~ 0       
    ),
    afr_zambia = case_when(
      str_detect(f_location, "zambia") == TRUE ~ 1,
      str_detect(f_location, "zambia") == FALSE ~ 0
    ),
    afr_zimbabwe = case_when(
      str_detect(f_location, "zimbabwe") == TRUE ~ 1,
      str_detect(f_location, "zimbabwe") == FALSE ~ 0     
    ),
    afr_angola = case_when(
      str_detect(f_location, "angola") == TRUE ~ 1,
      str_detect(f_location, "angola") == FALSE ~ 0 
    ),
    afr_cameroon = case_when(
      str_detect(f_location, "cameroon") == TRUE ~ 1,
      str_detect(f_location, "cameroon") == FALSE ~ 0
    ),
    afr_central_african_republic = case_when(
      str_detect(f_location, "central african republic") == TRUE ~ 1,
      str_detect(f_location, "central african republic") == FALSE ~ 0
    ),
    afr_chad = case_when(
      str_detect(f_location, "chad") == TRUE ~ 1,
      str_detect(f_location, "chad") == FALSE ~ 0
    ),
    afr_congo = case_when(
      str_detect(f_location, "congo") == TRUE ~ 1,
      str_detect(f_location, "congo") == FALSE ~ 0    
    ),
    afr_democratic_republic_of_the_congo = case_when(
      str_detect(f_location, "democratic republic of the congo") == TRUE ~ 1,
      str_detect(f_location, "democratic republic of the congo") == FALSE ~ 0
    ),
    afr_equatorial_guinea = case_when(
      str_detect(f_location, "equatorial guinea") == TRUE ~ 1,
      str_detect(f_location, "equatorial guinea") == FALSE ~ 0
    ),
    afr_gabon = case_when(
      str_detect(f_location, "gabon") == TRUE ~ 1,
      str_detect(f_location, "gabon") == FALSE ~ 0
    ),
    afr_sao_tome_and_principe = case_when(
      str_detect(f_location, "sao tome and principe") == TRUE ~ 1,
      str_detect(f_location, "sao tome and principe") == FALSE ~ 0
    ),
    afr_botswana = case_when(
      str_detect(f_location, "botswana") == TRUE ~ 1,
      str_detect(f_location, "botswana") == FALSE ~ 0
    ),
    afr_eswatini = case_when(
      str_detect(f_location, "eswatini") == TRUE ~ 1,
      str_detect(f_location, "eswatini") == FALSE ~ 0  
    ),
    afr_lesotho = case_when(
      str_detect(f_location, "lesotho") == TRUE ~ 1,
      str_detect(f_location, "lesotho") == FALSE ~ 0
    ),
    afr_namibia = case_when(
      str_detect(f_location, "namibia") == TRUE ~ 1,
      str_detect(f_location, "namibia") == FALSE ~ 0
    ),
    afr_south_africa = case_when(
      str_detect(f_location, "south africa") == TRUE ~ 1,
      str_detect(f_location, "south africa") == FALSE ~ 0
    ),
    afr_benin = case_when(
      str_detect(f_location, "benin") == TRUE ~ 1,
      str_detect(f_location, "benin") == FALSE ~ 0
    ),
    afr_burkina_faso = case_when(
      str_detect(f_location, "burkina faso") == TRUE ~ 1,
      str_detect(f_location, "burkina faso") == FALSE ~ 0
    ),
    afr_cabo_verde = case_when(
      str_detect(f_location, "cabo verde") == TRUE ~ 1,
      str_detect(f_location, "cabo verde") == FALSE ~ 0
    ),
    afr_cote_divoire = case_when(
      str_detect(f_location, "côte d’ivoire") == TRUE ~ 1,
      str_detect(f_location, "côte d’ivoire") == FALSE ~ 0
    ),
    afr_gambia = case_when(
      str_detect(f_location, "gambia") == TRUE ~ 1,
      str_detect(f_location, "gambia") == FALSE ~ 0  
    ),
    afr_ghana = case_when(
      str_detect(f_location, "ghana") == TRUE ~ 1,
      str_detect(f_location, "ghana") == FALSE ~ 0
    ),
    afr_guinea = case_when(
      str_detect(f_location, "guinea") == TRUE ~ 1,
      str_detect(f_location, "guinea") == FALSE ~ 0
    ),
    afr_guinea_bissau = case_when(
      str_detect(f_location, "guinea-bissau") == TRUE ~ 1,
      str_detect(f_location, "guinea-bissau") == FALSE ~ 0 
    ),                 
    afr_liberia = case_when(
      str_detect(f_location, "liberia") == TRUE ~ 1,
      str_detect(f_location, "liberia") == FALSE ~ 0
    ),
    afr_mali = case_when(
      str_detect(f_location, "mali") == TRUE ~ 1,
      str_detect(f_location, "mali") == FALSE ~ 0
    ),
    afr_mauritania = case_when(
      str_detect(f_location, "mauritania") == TRUE ~ 1,
      str_detect(f_location, "mauritania") == FALSE ~ 0
    ),
    afr_niger = case_when(
      str_detect(f_location, "niger") == TRUE ~ 1,
      str_detect(f_location, "niger") == FALSE ~ 0
    ),
    afr_nigeria = case_when(
      str_detect(f_location, "nigeria") == TRUE ~ 1,
      str_detect(f_location, "nigeria") == FALSE ~ 0
    ),
    afr_saint_helena = case_when(
      str_detect(f_location, "saint helena") == TRUE ~ 1,
      str_detect(f_location, "saint helena") == FALSE ~ 0
    ),
    afr_senegal = case_when(
      str_detect(f_location, "senegal") == TRUE ~ 1,
      str_detect(f_location, "senegal") == FALSE ~ 0
    ),
    afr_sierra_leone = case_when(
      str_detect(f_location, "sierra leone") == TRUE ~ 1,
      str_detect(f_location, "sierra leone") == FALSE ~ 0
    ),
    afr_togo = case_when(
      str_detect(f_location, "togo") == TRUE ~ 1,
      str_detect(f_location, "togo") == FALSE ~ 0    
    )
  ) %>%
  mutate(
    country_afr = across(starts_with("afr_")) %>% rowSums,
    country_afr_dummy = case_when(
      country_afr > 0 ~ 1,
      country_afr == 0 ~ 0
    )
    )

amr_anguilla = case_when(
  str_detect(f_location, "anguilla") == TRUE ~ 1,
  str_detect(f_location, "anguilla") == FALSE ~ 0
),
amr_antigua_and_barbuda = case_when(
  str_detect(f_location, "antigua and barbuda") == TRUE ~ 1,
  str_detect(f_location, "antigua and barbuda") == FALSE ~ 0
),
amr_aruba = case_when(
  str_detect(f_location, "aruba") == TRUE ~ 1,
  str_detect(f_location, "aruba") == FALSE ~ 0
),
amr_bahamas = case_when(
  str_detect(f_location, "bahamas") == TRUE ~ 1,
  str_detect(f_location, "bahamas") == FALSE ~ 0
),                                     
amr_barbados = case_when(
  str_detect(f_location, "barbados") == TRUE ~ 1,
  str_detect(f_location, "barbados") == FALSE ~ 0
),                                    
amr_bonaire = case_when(
  str_detect(f_location, "bonaire, sint eustatius, and saba") == TRUE ~ 1,
  str_detect(f_location, "bonaire, sint eustatius, and saba") == FALSE ~ 0
),
amr_british_virgin_islands  = case_when(
  str_detect(f_location, "british virgin islands") == TRUE ~ 1,
  str_detect(f_location, "british virgin islands") == FALSE ~ 0
),
amr_cayman_islands  = case_when(
  str_detect(f_location, "cayman islands") == TRUE ~ 1,
  str_detect(f_location, "cayman islands") == FALSE ~ 0
),
amr_cuba  = case_when(
  str_detect(f_location, "cuba") == TRUE ~ 1,
  str_detect(f_location, "cuba") == FALSE ~ 0
),
amr_curacao  = case_when(
  str_detect(f_location, "curacao") == TRUE ~ 1,
  str_detect(f_location, "curacao") == FALSE ~ 0
),
amr_dominica  = case_when(
  str_detect(f_location, "dominica") == TRUE ~ 1,
  str_detect(f_location, "dominica") == FALSE ~ 0
),
amr_dominican_republic  = case_when(
  str_detect(f_location, "dominican republic") == TRUE ~ 1,
  str_detect(f_location, "dominican republic") == FALSE ~ 0
),
amr_grenada  = case_when(
  str_detect(f_location, "grenada") == TRUE ~ 1,
  str_detect(f_location, "grenada") == FALSE ~ 0
),
amr_guadeloupe  = case_when(
  str_detect(f_location, "guadeloupe") == TRUE ~ 1,
  str_detect(f_location, "guadeloupe") == FALSE ~ 0
),
amr_haiti  = case_when(
  str_detect(f_location, "haiti") == TRUE ~ 1,
  str_detect(f_location, "haiti") == FALSE ~ 0
),
jamaica = case_when(
  str_detect(f_location, "jamaica") == TRUE ~ 1,
  str_detect(f_location, "jamaica") == FALSE ~ 0
),                                     
martinique = case_when(
  str_detect(f_location, "martinique") == TRUE ~ 1,
  str_detect(f_location, "martinique") == FALSE ~ 0
),                                  
montserrat = case_when(
  str_detect(f_location, "montserrat") == TRUE ~ 1,
  str_detect(f_location, "montserrat") == FALSE ~ 0
),                                  
puerto_rico = case_when(
  str_detect(f_location, "puerto rico") == TRUE ~ 1,
  str_detect(f_location, "puerto rico") == FALSE ~ 0
),                                 
saint_barthélemy = case_when(
  str_detect(f_location, "saint barthélemy") == TRUE ~ 1,
  str_detect(f_location, "saint barthélemy") == FALSE ~ 0
),                            
saint_kitts_and_nevis = case_when(
  str_detect(f_location, "saint kitts and nevis") == TRUE ~ 1,
  str_detect(f_location, "saint kitts and nevis") == FALSE ~ 0
),                       
saint_lucia = case_when(
  str_detect(f_location, "saint lucia") == TRUE ~ 1,
  str_detect(f_location, "saint lucia") == FALSE ~ 0
),                                 
saint_martin = case_when(
  str_detect(f_location, "saint martin") == TRUE ~ 1,
  str_detect(f_location, "saint martin") == FALSE ~ 0
),
saint_vincent_and_the_grenadines = case_when(
  str_detect(f_location, "saint vincent and the grenadines") == TRUE ~ 1,
  str_detect(f_location, "saint vincent and the grenadines") == FALSE ~ 0
),            
sint_maarten = case_when(
  str_detect(f_location, "sint maarten") == TRUE ~ 1,
  str_detect(f_location, "sint maarten") == FALSE ~ 0
),
trinidad_and_tobago = case_when(
  str_detect(f_location, "trinidad and tobago") == TRUE ~ 1,
  str_detect(f_location, "trinidad and tobago") == FALSE ~ 0
),                         
turks_and_caicos_islands = case_when(
  str_detect(f_location, "turks and caicos islands") == TRUE ~ 1,
  str_detect(f_location, "turks and caicos islands") == FALSE ~ 0
),                    
united_states_virgin_islands = case_when(
  str_detect(f_location, "united states virgin islands") == TRUE ~ 1,
  str_detect(f_location, "united states virgin islands") == FALSE ~ 0
),

[29] "belize"                                      
[30] "costa_rica"                                  
[31] "el_salvador"                                 
[32] "guatemala"                                   
[33] "honduras"                                    
[34] "mexico"                                      
[35] "nicaragua"                                   
[36] "panama"                                      
[37] "argentina"                                   
[38] "bolivia_(plurinational_state_of)"            
[39] "bouvet_island"                               
[40] "brazil"                                      
[41] "chile"                                       
[42] "colombia"                                    
[43] "ecuador"                                     
[44] "falkland_islands_(malvinas)"                 
[45] "french_guiana"                               
[46] "guyana"                                      
[47] "paraguay"                                    
[48] "peru"                                        
[49] "south_georgia_and_the_south_sandwich_islands"
[50] "suriname"                                    
[51] "uruguay"                                     
[52] "venezuela_(bolivarian_republic_of)"          
[53] "bermuda"                                     
[54] "canada"                                      
[55] "greenland"                                   
[56] "saint_pierre_and_miquelon"                   
[57] "united_states_of_america"

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