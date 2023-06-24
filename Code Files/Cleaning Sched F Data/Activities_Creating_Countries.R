## Project: Nonprofit Foreign Expenditures

# Summarizing data on foreign activities (Schedule F of Form 990)
#   Last updated: June 23, 2023

# Output:
#   activities_country_230623.csv
# Contains dummy variables for whether each country appears
# in a line, as well as other dummies for summarizing info.
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
dirty_f_activities <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_activities.csv")

activities <- dirty_f_activities %>%
  select(-id) %>%
  mutate(
    id_ein = paste0(object_id, ein)
  ) %>%
  mutate_at(c('RgnTtlExpndtrsAmt'), ~replace_na(.,0)) %>% # replacing NAs with zeros (0)
  select(
    id_ein, object_id, ein, RgnTxt, RgnTtlExpndtrsAmt
  ) %>%
  mutate(
    f_location = RgnTxt,
    f_location = str_to_lower(f_location, locale = "en")
  )
#-------------
#--------------------------
# Cleaning activities data
#   1 row = org-year, location
#--------------------------
acts_cln <- activities %>%
  mutate(
    transfers_n = 1
  ) %>%
  filter(
    RgnTtlExpndtrsAmt > 0 # only positive values for expenses
  ) %>%
  group_by(id_ein, f_location) %>%
  summarise(
    lctn_xpnss_total = sum(RgnTtlExpndtrsAmt),
    transfers_total = sum(transfers_n)
  )
#-------------
#-------------
# Making Country Dummy Columns
#-------------
# Column names and country names

# Africa
africa_colnames <- pull(list_africa, `Country or Area`)
  africa_colnames <- str_to_lower(africa_colnames)
  africa_colnames2 <- str_replace_all(africa_colnames,
                                     " ",
                                     "_")

# Americas  
americas_colnames <- pull(list_americas, `Country or Area`)
  americas_colnames <- str_to_lower(americas_colnames)
  americas_colnames2 <- str_replace_all(americas_colnames,
                                      " ",
                                      "_")

# Asia 
asia_colnames <- pull(list_asia_pacific, `Country or Area`)
  asia_colnames <- str_to_lower(asia_colnames)
  asia_colnames2 <- str_replace_all(asia_colnames,
                                        " ",
                                        "_")
 
# Europe 
europe_colnames <- pull(list_europe, `Country or Area`)
  europe_colnames <- str_to_lower(europe_colnames)
  europe_colnames2 <- str_replace_all(europe_colnames,
                                    " ",
                                    "_")

# Adding country dummies
activities_country <- acts_cln %>%
  mutate(
  # AFRICA  
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
    ),
  # AMERICAS
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
    amr_jamaica = case_when(
      str_detect(f_location, "jamaica") == TRUE ~ 1,
      str_detect(f_location, "jamaica") == FALSE ~ 0
    ),                                     
    amr_martinique = case_when(
      str_detect(f_location, "martinique") == TRUE ~ 1,
      str_detect(f_location, "martinique") == FALSE ~ 0
    ),                                  
    amr_montserrat = case_when(
      str_detect(f_location, "montserrat") == TRUE ~ 1,
      str_detect(f_location, "montserrat") == FALSE ~ 0
    ),                                  
    amr_puerto_rico = case_when(
      str_detect(f_location, "puerto rico") == TRUE ~ 1,
      str_detect(f_location, "puerto rico") == FALSE ~ 0
    ),                                 
    amr_saint_barthelemy = case_when(
      str_detect(f_location, "saint barthélemy") == TRUE ~ 1,
      str_detect(f_location, "saint barthélemy") == FALSE ~ 0
    ),                            
    amr_saint_kitts = case_when(
      str_detect(f_location, "saint kitts and nevis") == TRUE ~ 1,
      str_detect(f_location, "saint kitts and nevis") == FALSE ~ 0
    ),                       
    amr_saint_lucia = case_when(
      str_detect(f_location, "saint lucia") == TRUE ~ 1,
      str_detect(f_location, "saint lucia") == FALSE ~ 0
    ),                                 
    amr_saint_martin = case_when(
      str_detect(f_location, "saint martin") == TRUE ~ 1,
      str_detect(f_location, "saint martin") == FALSE ~ 0
    ),
    amr_saint_vincent = case_when(
      str_detect(f_location, "saint vincent and the grenadines") == TRUE ~ 1,
      str_detect(f_location, "saint vincent and the grenadines") == FALSE ~ 0
    ),            
    amr_sint_maarten = case_when(
      str_detect(f_location, "sint maarten") == TRUE ~ 1,
      str_detect(f_location, "sint maarten") == FALSE ~ 0
    ),
    amr_trinidad_and_tobago = case_when(
      str_detect(f_location, "trinidad and tobago") == TRUE ~ 1,
      str_detect(f_location, "trinidad and tobago") == FALSE ~ 0
    ),                         
    amr_turks_and_caicos = case_when(
      str_detect(f_location, "turks and caicos islands") == TRUE ~ 1,
      str_detect(f_location, "turks and caicos islands") == FALSE ~ 0
    ),                    
    amr_us_virgin_islands = case_when(
      str_detect(f_location, "united states virgin islands") == TRUE ~ 1,
      str_detect(f_location, "united states virgin islands") == FALSE ~ 0
    ),
    amr_belize = case_when(
      str_detect(f_location, "belize") == TRUE ~ 1,
      str_detect(f_location, "belize") == FALSE ~ 0
    ),                                    
    amr_costa_rica = case_when(
      str_detect(f_location, "costa rica") == TRUE ~ 1,
      str_detect(f_location, "costa rica") == FALSE ~ 0
    ),                                 
    amr_el_salvador = case_when(
      str_detect(f_location, "el salvador") == TRUE ~ 1,
      str_detect(f_location, "el salvador") == FALSE ~ 0
    ),                               
    amr_guatemala = case_when(
      str_detect(f_location, "guatemala") == TRUE ~ 1,
      str_detect(f_location, "guatemala") == FALSE ~ 0
    ),                                 
    amr_honduras = case_when(
      str_detect(f_location, "honduras") == TRUE ~ 1,
      str_detect(f_location, "honduras") == FALSE ~ 0
    ),
    amr_mexico = case_when(
      str_detect(f_location, "mexico") == TRUE ~ 1,
      str_detect(f_location, "mexico") == FALSE ~ 0
    ),                                     
    amr_nicaragua = case_when(
      str_detect(f_location, "nicaragua") == TRUE ~ 1,
      str_detect(f_location, "nicaragua") == FALSE ~ 0
    ),                                  
    amr_panama = case_when(
      str_detect(f_location, "panama") == TRUE ~ 1,
      str_detect(f_location, "panama") == FALSE ~ 0
    ),                                     
    amr_argentina = case_when(
      str_detect(f_location, "argentina") == TRUE ~ 1,
      str_detect(f_location, "argentina") == FALSE ~ 0
    ),                                  
    amr_bolivia = case_when(
      str_detect(f_location, "bolivia") == TRUE ~ 1,
      str_detect(f_location, "bolivia") == FALSE ~ 0
    ),           
    amr_bouvet = case_when(
      str_detect(f_location, "bouvet island") == TRUE ~ 1,
      str_detect(f_location, "bouvet island") == FALSE ~ 0
    ),                              
    amr_brazil = case_when(
      str_detect(f_location, "brazil") == TRUE ~ 1,
      str_detect(f_location, "brazil") == FALSE ~ 0
    ),                                     
    amr_chile = case_when(
      str_detect(f_location, "chile") == TRUE ~ 1,
      str_detect(f_location, "chile") == FALSE ~ 0
    ),                                      
    amr_colombia = case_when(
      str_detect(f_location, "colombia") == TRUE ~ 1,
      str_detect(f_location, "colombia") == FALSE ~ 0
    ),                                   
    amr_ecuador = case_when(
      str_detect(f_location, "ecuador") == TRUE ~ 1,
      str_detect(f_location, "ecuador") == FALSE ~ 0
    ),                                    
    amr_malvinas_falkland = case_when(
          str_detect(f_location, "malvinas") == TRUE ~ 1,
          str_detect(f_location, "malvinas") == FALSE ~ 0
        ),                
    amr_french_guiana = case_when(
          str_detect(f_location, "french guiana") == TRUE ~ 1,
          str_detect(f_location, "french guiana") == FALSE ~ 0
        ),                              
    amr_guyana = case_when(
      str_detect(f_location, "guyana") == TRUE ~ 1,
      str_detect(f_location, "guyana") == FALSE ~ 0
    ),                                     
    amr_paraguay = case_when(
      str_detect(f_location, "paraguay") == TRUE ~ 1,
      str_detect(f_location, "paraguay") == FALSE ~ 0
    ),                                   
    amr_peru = case_when(
      str_detect(f_location, "peru") == TRUE ~ 1,
      str_detect(f_location, "peru") == FALSE ~ 0
    ),                                       
    amr_south_sandwich_islands = case_when(
      str_detect(f_location, "south georgia and the south sandwich islands") == TRUE ~ 1,
      str_detect(f_location, "south georgia and the south sandwich islands") == FALSE ~ 0
    ),
    amr_suriname = case_when(
      str_detect(f_location, "suriname") == TRUE ~ 1,
      str_detect(f_location, "suriname") == FALSE ~ 0
    ),                                   
    amr_uruguay = case_when(
      str_detect(f_location, "uruguay") == TRUE ~ 1,
      str_detect(f_location, "uruguay") == FALSE ~ 0
    ),                                    
    amr_venezuela = case_when(
      str_detect(f_location, "venezuela") == TRUE ~ 1,
      str_detect(f_location, "venezuela") == FALSE ~ 0
    ),         
    amr_bermuda = case_when(
      str_detect(f_location, "bermuda") == TRUE ~ 1,
      str_detect(f_location, "bermuda") == FALSE ~ 0
    ),                                    
    amr_canada = case_when(
      str_detect(f_location, "canada") == TRUE ~ 1,
      str_detect(f_location, "canada") == FALSE ~ 0
    ),                                     
    amr_saint_pierre = case_when(
      str_detect(f_location, "saint pierre and miquelon") == TRUE ~ 1,
      str_detect(f_location, "saint pierre and miquelon") == FALSE ~ 0
    ),                  
    amr_usa = case_when(
      str_detect(f_location, "united states") == TRUE ~ 1,
      str_detect(f_location, "united states") == FALSE ~ 0
    ),
  # EUROPE
  eur_belarus = case_when(
    str_detect(f_location, "belarus") == TRUE ~ 1,
    str_detect(f_location, "belarus") == FALSE ~ 0
  ),                                             
  eur_bulgaria = case_when(
    str_detect(f_location, "bulgaria") == TRUE ~ 1,
    str_detect(f_location, "bulgaria") == FALSE ~ 0
  ),                                            
  eur_czechia = case_when(
    str_detect(f_location, "czechia") == TRUE ~ 1,
    str_detect(f_location, "czechia") == FALSE ~ 0
  ),                                             
  eur_hungary = case_when(
    str_detect(f_location, "hungary") == TRUE ~ 1,
    str_detect(f_location, "hungary") == FALSE ~ 0
  ),                                             
  eur_poland = case_when(
    str_detect(f_location, "poland") == TRUE ~ 1,
    str_detect(f_location, "poland") == FALSE ~ 0
  ),                                              
  eur_moldova = case_when(
    str_detect(f_location, "moldova") == TRUE ~ 1,
    str_detect(f_location, "moldova") == FALSE ~ 0
  ),                                 
  eur_romania = case_when(
    str_detect(f_location, "romania") == TRUE ~ 1,
    str_detect(f_location, "romania") == FALSE ~ 0
  ),                                             
  eur_russia = case_when(
    str_detect(f_location, "russia") == TRUE ~ 1,
    str_detect(f_location, "russia") == FALSE ~ 0
  ),                                  
  eur_slovakia = case_when(
    str_detect(f_location, "slovakia") == TRUE ~ 1,
    str_detect(f_location, "slovakia") == FALSE ~ 0
  ),                                           
  eur_ukraine = case_when(
    str_detect(f_location, "ukraine") == TRUE ~ 1,
    str_detect(f_location, "ukraine") == FALSE ~ 0
  ),                                             
  eur_aland_islands = case_when(
    str_detect(f_location, "aland islands") == TRUE ~ 1,
    str_detect(f_location, "aland islands") == FALSE ~ 0
  ),                                       
  eur_guernsey = case_when(
    str_detect(f_location, "guernsey") == TRUE ~ 1,
    str_detect(f_location, "guernsey") == FALSE ~ 0
  ),                                            
  eur_jersey = case_when(
    str_detect(f_location, "jersey") == TRUE ~ 1,
    str_detect(f_location, "jersey") == FALSE ~ 0
  ),                                              
  eur_sark = case_when(
    str_detect(f_location, "sark") == TRUE ~ 1,
    str_detect(f_location, "sark") == FALSE ~ 0
  ),                                                
  eur_denmark = case_when(
    str_detect(f_location, "denmark") == TRUE ~ 1,
    str_detect(f_location, "denmark") == FALSE ~ 0
  ),                                             
  eur_estonia = case_when(
    str_detect(f_location, "estonia") == TRUE ~ 1,
    str_detect(f_location, "estonia") == FALSE ~ 0
  ),                                             
  eur_faroe_islands = case_when(
    str_detect(f_location, "faroe islands") == TRUE ~ 1,
    str_detect(f_location, "faroe islands") == FALSE ~ 0
  ),                                       
  eur_finland = case_when(
    str_detect(f_location, "finland") == TRUE ~ 1,
    str_detect(f_location, "finland") == FALSE ~ 0
  ),                                             
  eur_iceland = case_when(
    str_detect(f_location, "iceland") == TRUE ~ 1,
    str_detect(f_location, "iceland") == FALSE ~ 0
  ),                                             
  eur_ireland = case_when(
    str_detect(f_location, "ireland") == TRUE ~ 1,
    str_detect(f_location, "ireland") == FALSE ~ 0
  ),                                             
  eur_isle_of_man = case_when(
    str_detect(f_location, "isle of man") == TRUE ~ 1,
    str_detect(f_location, "isle of man") == FALSE ~ 0
  ),                                         
  eur_latvia = case_when(
    str_detect(f_location, "latvia") == TRUE ~ 1,
    str_detect(f_location, "latvia") == FALSE ~ 0
  ),                                              
  eur_lithuania = case_when(
    str_detect(f_location, "lithuania") == TRUE ~ 1,
    str_detect(f_location, "lithuania") == FALSE ~ 0
  ),                                           
  eur_norway = case_when(
    str_detect(f_location, "norway") == TRUE ~ 1,
    str_detect(f_location, "norway") == FALSE ~ 0
  ),                                              
  eur_svalbard_and_jan_mayen_islands = case_when(
    str_detect(f_location, "svalbard and jan mayen islands") == TRUE ~ 1,
    str_detect(f_location, "svalbard and jan mayen islands") == FALSE ~ 0
  ),                      
  eur_sweden = case_when(
    str_detect(f_location, "sweden") == TRUE ~ 1,
    str_detect(f_location, "sweden") == FALSE ~ 0
  ),                                              
  eur_united_kingdom = case_when(
    str_detect(f_location, "united kingdom") == TRUE ~ 1,
    str_detect(f_location, "united kingdom") == FALSE ~ 0
  ),
  eur_albania = case_when(
    str_detect(f_location, "albania") == TRUE ~ 1,
    str_detect(f_location, "albania") == FALSE ~ 0
  ),                                             
  eur_andorra = case_when(
    str_detect(f_location, "andorra") == TRUE ~ 1,
    str_detect(f_location, "andorra") == FALSE ~ 0
  ),                                             
  eur_bosnia_and_herzegovina = case_when(
    str_detect(f_location, "bosnia and herzegovina") == TRUE ~ 1,
    str_detect(f_location, "bosnia and herzegovina") == FALSE ~ 0
  ),                              
  eur_croatia = case_when(
    str_detect(f_location, "croatia") == TRUE ~ 1,
    str_detect(f_location, "croatia") == FALSE ~ 0
  ),                                             
  eur_gibraltar = case_when(
    str_detect(f_location, "gibraltar") == TRUE ~ 1,
    str_detect(f_location, "gibraltar") == FALSE ~ 0
  ),                                           
  eur_greece = case_when(
    str_detect(f_location, "greece") == TRUE ~ 1,
    str_detect(f_location, "greece") == FALSE ~ 0
  ),                                              
  eur_vatican = case_when(
    str_detect(f_location, "vatican") == TRUE ~ 1,
    str_detect(f_location, "vatican") == FALSE ~ 0
  ),                                            
  eur_italy = case_when(
    str_detect(f_location, "italy") == TRUE ~ 1,
    str_detect(f_location, "italy") == FALSE ~ 0
  ),                                               
  eur_malta = case_when(
    str_detect(f_location, "malta") == TRUE ~ 1,
    str_detect(f_location, "malta") == FALSE ~ 0
  ),                                               
  eur_montenegro = case_when(
    str_detect(f_location, "montenegro") == TRUE ~ 1,
    str_detect(f_location, "montenegro") == FALSE ~ 0
  ),                                          
  eur_north_macedonia = case_when(
    str_detect(f_location, "north macedonia") == TRUE ~ 1,
    str_detect(f_location, "north macedonia") == FALSE ~ 0
  ),                                     
  eur_portugal = case_when(
    str_detect(f_location, "portugal") == TRUE ~ 1,
    str_detect(f_location, "portugal") == FALSE ~ 0
  ),                                            
  eur_san_marino = case_when(
    str_detect(f_location, "san marino") == TRUE ~ 1,
    str_detect(f_location, "san marino") == FALSE ~ 0
  ),                                          
  eur_serbia = case_when(
    str_detect(f_location, "serbia") == TRUE ~ 1,
    str_detect(f_location, "serbia") == FALSE ~ 0
  ),                                              
  eur_slovenia = case_when(
    str_detect(f_location, "slovenia") == TRUE ~ 1,
    str_detect(f_location, "slovenia") == FALSE ~ 0
  ),                                            
  eur_spain = case_when(
    str_detect(f_location, "spain") == TRUE ~ 1,
    str_detect(f_location, "spain") == FALSE ~ 0
  ),                                               
  eur_austria = case_when(
    str_detect(f_location, "austria") == TRUE ~ 1,
    str_detect(f_location, "austria") == FALSE ~ 0
  ),                                             
  eur_belgium = case_when(
    str_detect(f_location, "belgium") == TRUE ~ 1,
    str_detect(f_location, "belgium") == FALSE ~ 0
  ),                                             
  eur_france = case_when(
    str_detect(f_location, "france") == TRUE ~ 1,
    str_detect(f_location, "france") == FALSE ~ 0
  ),                                              
  eur_germany = case_when(
    str_detect(f_location, "germany") == TRUE ~ 1,
    str_detect(f_location, "germany") == FALSE ~ 0
  ),                                             
  eur_liechtenstein = case_when(
    str_detect(f_location, "liechtenstein") == TRUE ~ 1,
    str_detect(f_location, "liechtenstein") == FALSE ~ 0
  ),                                       
  eur_luxembourg = case_when(
    str_detect(f_location, "luxembourg") == TRUE ~ 1,
    str_detect(f_location, "luxembourg") == FALSE ~ 0
  ),                                          
  eur_monaco = case_when(
    str_detect(f_location, "monaco") == TRUE ~ 1,
    str_detect(f_location, "monaco") == FALSE ~ 0
  ),                                              
  eur_netherlands = case_when(
    str_detect(f_location, "netherlands") == TRUE ~ 1,
    str_detect(f_location, "netherlands") == FALSE ~ 0
  ),                                         
  eur_switzerland = case_when(
    str_detect(f_location, "switzerland") == TRUE ~ 1,
    str_detect(f_location, "switzerland") == FALSE ~ 0
  ),
  #ASIA
  asia_kazakhstan = case_when(
    str_detect(f_location, "kazakhstan") == TRUE ~ 1,
    str_detect(f_location, "kazakhstan") == FALSE ~ 0
  ),                                    
  asia_kyrgyzstan = case_when(
    str_detect(f_location, "kyrgyzstan") == TRUE ~ 1,
    str_detect(f_location, "kyrgyzstan") == FALSE ~ 0
  ),                                    
  asia_tajikistan = case_when(
    str_detect(f_location, "tajikistan") == TRUE ~ 1,
    str_detect(f_location, "tajikistan") == FALSE ~ 0
  ),                                    
  asia_turkmenistan = case_when(
    str_detect(f_location, "turkmenistan") == TRUE ~ 1,
    str_detect(f_location, "turkmenistan") == FALSE ~ 0
  ),                                  
  asia_uzbekistan = case_when(
    str_detect(f_location, "uzbekistan") == TRUE ~ 1,
    str_detect(f_location, "uzbekistan") == FALSE ~ 0
  ),                                    
  asia_china = case_when(
    str_detect(f_location, "china") == TRUE ~ 1,
    str_detect(f_location, "china") == FALSE ~ 0
  ),                                         
  asia_hong_kong = case_when(
    str_detect(f_location, "hong kong") == TRUE ~ 1,
    str_detect(f_location, "hong kong") == FALSE ~ 0
  ),
  asia_macao = case_when(
    str_detect(f_location, "macao") == TRUE ~ 1,
    str_detect(f_location, "macao") == FALSE ~ 0
  ),
  asia_north_korea = case_when(
    str_detect(f_location, "north korea") == TRUE ~ 1,
    str_detect(f_location, "north korea") == FALSE ~ 0
  ),
  asia_japan = case_when(
    str_detect(f_location, "japan") == TRUE ~ 1,
    str_detect(f_location, "japan") == FALSE ~ 0
  ),                                         
  asia_mongolia = case_when(
    str_detect(f_location, "mongolia") == TRUE ~ 1,
    str_detect(f_location, "mongolia") == FALSE ~ 0
  ),                                      
  asia_south_korea = case_when(
    str_detect(f_location, "south korea") == TRUE ~ 1,
    str_detect(f_location, "south korea") == FALSE ~ 0
  ),                             
  asia_brunei_darussalam = case_when(
    str_detect(f_location, "brunei darussalam") == TRUE ~ 1,
    str_detect(f_location, "brunei darussalam") == FALSE ~ 0
  ),                             
  asia_cambodia = case_when(
    str_detect(f_location, "cambodia") == TRUE ~ 1,
    str_detect(f_location, "cambodia") == FALSE ~ 0
  ),                                      
  asia_indonesia = case_when(
    str_detect(f_location, "indonesia") == TRUE ~ 1,
    str_detect(f_location, "indonesia") == FALSE ~ 0
  ),                                     
  asia_laos = case_when(
    str_detect(f_location, "laos") == TRUE ~ 1,
    str_detect(f_location, "laos") == FALSE ~ 0
  ),             
  asia_malaysia = case_when(
    str_detect(f_location, "malaysia") == TRUE ~ 1,
    str_detect(f_location, "malaysia") == FALSE ~ 0
  ),                                      
  asia_myanmar = case_when(
    str_detect(f_location, "myanmar") == TRUE ~ 1,
    str_detect(f_location, "myanmar") == FALSE ~ 0
  ),                                       
  asia_philippines = case_when(
    str_detect(f_location, "philippines") == TRUE ~ 1,
    str_detect(f_location, "philippines") == FALSE ~ 0
  ),                                   
  asia_singapore = case_when(
    str_detect(f_location, "singapore") == TRUE ~ 1,
    str_detect(f_location, "singapore") == FALSE ~ 0
  ),                                     
  asia_thailand = case_when(
    str_detect(f_location, "thailand") == TRUE ~ 1,
    str_detect(f_location, "thailand") == FALSE ~ 0
  ),                                      
  asia_timor_leste = case_when(
    str_detect(f_location, "timor leste") == TRUE ~ 1,
    str_detect(f_location, "timor leste") == FALSE ~ 0
  ),                                   
  asia_vietnam = case_when(
    str_detect(f_location, "vietnam") == TRUE ~ 1,
    str_detect(f_location, "vietnam") == FALSE ~ 0
  ),                                      
  asia_afghanistan = case_when(
    str_detect(f_location, "afghanistan") == TRUE ~ 1,
    str_detect(f_location, "afghanistan") == FALSE ~ 0
  ),                                   
  asia_bangladesh = case_when(
    str_detect(f_location, "bangladesh") == TRUE ~ 1,
    str_detect(f_location, "bangladesh") == FALSE ~ 0
  ),                                    
  asia_bhutan = case_when(
    str_detect(f_location, "bhutan") == TRUE ~ 1,
    str_detect(f_location, "bhutan") == FALSE ~ 0
  ),                                        
  asia_india = case_when(
    str_detect(f_location, "india") == TRUE ~ 1,
    str_detect(f_location, "india") == FALSE ~ 0
  ),                                         
  asia_iran = case_when(
    str_detect(f_location, "iran") == TRUE ~ 1,
    str_detect(f_location, "iran") == FALSE ~ 0
  ),                   
  asia_maldives = case_when(
    str_detect(f_location, "maldives") == TRUE ~ 1,
    str_detect(f_location, "maldives") == FALSE ~ 0
  ),                                      
  asia_nepal = case_when(
    str_detect(f_location, "nepal") == TRUE ~ 1,
    str_detect(f_location, "nepal") == FALSE ~ 0
  ),                                         
  asia_pakistan = case_when(
    str_detect(f_location, "pakistan") == TRUE ~ 1,
    str_detect(f_location, "pakistan") == FALSE ~ 0
  ),                                      
  asia_sri_lanka = case_when(
    str_detect(f_location, "sri lanka") == TRUE ~ 1,
    str_detect(f_location, "sri lanka") == FALSE ~ 0
  ),                                     
  asia_armenia = case_when(
    str_detect(f_location, "armenia") == TRUE ~ 1,
    str_detect(f_location, "armenia") == FALSE ~ 0
  ),                                       
  asia_azerbaijan = case_when(
    str_detect(f_location, "azerbaijan") == TRUE ~ 1,
    str_detect(f_location, "azerbaijan") == FALSE ~ 0
  ),                                    
  asia_bahrain = case_when(
    str_detect(f_location, "bahrain") == TRUE ~ 1,
    str_detect(f_location, "bahrain") == FALSE ~ 0
  ),                                       
  asia_cyprus = case_when(
    str_detect(f_location, "cyprus") == TRUE ~ 1,
    str_detect(f_location, "cyprus") == FALSE ~ 0
  ),                                        
  asia_georgia = case_when(
    str_detect(f_location, "georgia") == TRUE ~ 1,
    str_detect(f_location, "georgia") == FALSE ~ 0
  ),                                       
  asia_iraq = case_when(
    str_detect(f_location, "iraq") == TRUE ~ 1,
    str_detect(f_location, "iraq") == FALSE ~ 0
  ),                                          
  asia_israel = case_when(
    str_detect(f_location, "israel") == TRUE ~ 1,
    str_detect(f_location, "israel") == FALSE ~ 0
  ),                                        
  asia_jordan = case_when(
    str_detect(f_location, "jordan") == TRUE ~ 1,
    str_detect(f_location, "jordan") == FALSE ~ 0
  ),                                        
  asia_kuwait = case_when(
    str_detect(f_location, "kuwait") == TRUE ~ 1,
    str_detect(f_location, "kuwait") == FALSE ~ 0
  ),                                        
  asia_lebanon = case_when(
    str_detect(f_location, "lebanon") == TRUE ~ 1,
    str_detect(f_location, "lebanon") == FALSE ~ 0
  ),                                       
  asia_oman = case_when(
    str_detect(f_location, "oman") == TRUE ~ 1,
    str_detect(f_location, "oman") == FALSE ~ 0
  ),                                          
  asia_qatar = case_when(
    str_detect(f_location, "qatar") == TRUE ~ 1,
    str_detect(f_location, "qatar") == FALSE ~ 0
  ),                                         
  asia_saudi_arabia = case_when(
    str_detect(f_location, "saudi arabia") == TRUE ~ 1,
    str_detect(f_location, "saudi arabia") == FALSE ~ 0
  ),                                  
  asia_palestine = case_when(
    str_detect(f_location, "palestine") == TRUE ~ 1,
    str_detect(f_location, "palestine") == FALSE ~ 0
  ),                            
  asia_syria = case_when(
    str_detect(f_location, "syria") == TRUE ~ 1,
    str_detect(f_location, "syria") == FALSE ~ 0
  ),
  asia_turkey = case_when(
    str_detect(f_location, "turkey") == TRUE ~ 1,
    str_detect(f_location, "turkey") == FALSE ~ 0
  ),                                       
  asia_uae = case_when(
    str_detect(f_location, "united arab emirates") == TRUE ~ 1,
    str_detect(f_location, "united arab emirates") == FALSE ~ 0
  ),                          
  asia_yemen = case_when(
    str_detect(f_location, "yemen") == TRUE ~ 1,
    str_detect(f_location, "yemen") == FALSE ~ 0
  ),                                         
  asia_australia = case_when(
    str_detect(f_location, "australia") == TRUE ~ 1,
    str_detect(f_location, "australia") == FALSE ~ 0
  ),                                     
  asia_christmas = case_when(
    str_detect(f_location, "christmas island") == TRUE ~ 1,
    str_detect(f_location, "christmas island") == FALSE ~ 0
  ),                              
  asia_cocos_keeling = case_when(
    str_detect(f_location, "cocos keeling islands") == TRUE ~ 1,
    str_detect(f_location, "cocos keeling islands") == FALSE ~ 0
  ),                       
  asia_heard_and_mcdonald = case_when(
    str_detect(f_location, "heard island and mcdonald islands") == TRUE ~ 1,
    str_detect(f_location, "heard island and mcdonald islands") == FALSE ~ 0
  ),             
  asia_new_zealand = case_when(
    str_detect(f_location, "new zealand") == TRUE ~ 1,
    str_detect(f_location, "new zealand") == FALSE ~ 0
  ),                                   
  asia_norfolk_island = case_when(
    str_detect(f_location, "norfolk island") == TRUE ~ 1,
    str_detect(f_location, "norfolk island") == FALSE ~ 0
  ),                                
  asia_fiji = case_when(
    str_detect(f_location, "fiji") == TRUE ~ 1,
    str_detect(f_location, "fiji") == FALSE ~ 0
  ),                                          
  asia_new_caledonia = case_when(
    str_detect(f_location, "new caledonia") == TRUE ~ 1,
    str_detect(f_location, "new caledonia") == FALSE ~ 0
  ),                                 
  asia_papua_new_guinea = case_when(
    str_detect(f_location, "papua new guinea") == TRUE ~ 1,
    str_detect(f_location, "papua new guinea") == FALSE ~ 0
  ),                              
  asia_solomon_islands = case_when(
    str_detect(f_location, "solomon islands") == TRUE ~ 1,
    str_detect(f_location, "solomon islands") == FALSE ~ 0
  ),                               
  asia_vanuatu = case_when(
    str_detect(f_location, "vanuatu") == TRUE ~ 1,
    str_detect(f_location, "vanuatu") == FALSE ~ 0
  ),                                       
  asia_guam = case_when(
    str_detect(f_location, "guam") == TRUE ~ 1,
    str_detect(f_location, "guam") == FALSE ~ 0
  ),                                          
  asia_kiribati = case_when(
    str_detect(f_location, "kiribati") == TRUE ~ 1,
    str_detect(f_location, "kiribati") == FALSE ~ 0
  ),                                      
  asia_marshall_islands = case_when(
    str_detect(f_location, "marshall islands") == TRUE ~ 1,
    str_detect(f_location, "marshall islands") == FALSE ~ 0
  ),                              
  asia_micronesia = case_when(
    str_detect(f_location, "micronesia") == TRUE ~ 1,
    str_detect(f_location, "micronesia") == FALSE ~ 0
  ),
  asia_nauru = case_when(
    str_detect(f_location, "nauru") == TRUE ~ 1,
    str_detect(f_location, "nauru") == FALSE ~ 0
  ),                                         
  asia_northern_mariana_islands = case_when(
    str_detect(f_location, "northern mariana islands") == TRUE ~ 1,
    str_detect(f_location, "northern mariana islands") == FALSE ~ 0
  ),                      
  asia_palau = case_when(
    str_detect(f_location, "palau") == TRUE ~ 1,
    str_detect(f_location, "palau") == FALSE ~ 0
  ),                                         
  asia_usa_minor = case_when(
    str_detect(f_location, "united states minor outlying islands") == TRUE ~ 1,
    str_detect(f_location, "united states minor outlying islands") == FALSE ~ 0
  ),          
  asia_american_samoa = case_when(
    str_detect(f_location, "american samoa") == TRUE ~ 1,
    str_detect(f_location, "american samoa") == FALSE ~ 0
  ),                                
  asia_cook_islands = case_when(
    str_detect(f_location, "cook islands") == TRUE ~ 1,
    str_detect(f_location, "cook islands") == FALSE ~ 0
  ),                                  
  asia_french_polynesia = case_when(
    str_detect(f_location, "french polynesia") == TRUE ~ 1,
    str_detect(f_location, "french polynesia") == FALSE ~ 0
  ),                              
  asia_niue = case_when(
    str_detect(f_location, "niue") == TRUE ~ 1,
    str_detect(f_location, "niue") == FALSE ~ 0
  ),                                          
  asia_pitcairn = case_when(
    str_detect(f_location, "pitcairn") == TRUE ~ 1,
    str_detect(f_location, "pitcairn") == FALSE ~ 0
  ),                                      
  asia_samoa = case_when(
    str_detect(f_location, "samoa") == TRUE ~ 1,
    str_detect(f_location, "samoa") == FALSE ~ 0
  ),                                         
  asia_tokelau = case_when(
    str_detect(f_location, "tokelau") == TRUE ~ 1,
    str_detect(f_location, "tokelau") == FALSE ~ 0
  ),                                       
  asia_tonga = case_when(
    str_detect(f_location, "tonga") == TRUE ~ 1,
    str_detect(f_location, "tonga") == FALSE ~ 0
  ),                                         
  asia_tuvalu = case_when(
    str_detect(f_location, "tuvalu") == TRUE ~ 1,
    str_detect(f_location, "tuvalu") == FALSE ~ 0
  ),                                        
  asia_wallis_and_futuna = case_when(
    str_detect(f_location, "wallis and futuna islands") == TRUE ~ 1,
    str_detect(f_location, "wallis and futuna islands") == FALSE ~ 0
  ),
  # Summary country dummies
  country_afr = across(starts_with("afr_")) %>% rowSums,
  country_afr_dummy = case_when(
    country_afr > 0 ~ 1,
    country_afr == 0 ~ 0
  ),
  country_amr = across(starts_with("amr_")) %>% rowSums,
  country_amr_dummy = case_when(
    country_amr > 0 ~ 1,
    country_amr == 0 ~ 0
  ),
  country_eur = across(starts_with("eur_")) %>% rowSums,
  country_eur_dummy = case_when(
    country_eur > 0 ~ 1,
    country_eur == 0 ~ 0
  ),
  country_asia = across(starts_with("asia_")) %>% rowSums,
  country_asia_dummy = case_when(
    country_asia > 0 ~ 1,
    country_asia == 0 ~ 0
  ),
  country_reported = case_when(
    country_afr_dummy == 1 |
    country_amr_dummy == 1 |
    country_eur_dummy == 1 |
    country_asia_dummy == 1  ~ 1,
    country_afr_dummy == 0 &
      country_amr_dummy == 0 &
      country_eur_dummy == 0 &
      country_asia_dummy == 0  ~ 0,
  )
)

activities_country <- activities_country %>%
  mutate_at(
    c(3, 260), ~replace_na(.,0)
    )

activities_country <- activities_country %>%
  mutate(
exp_afr_algeria = case_when(
  afr_algeria == 1 ~ lctn_xpnss_total,
  afr_algeria == 0 ~ 0
),                         
exp_afr_egypt = case_when(
  afr_egypt == 1 ~ lctn_xpnss_total,
  afr_egypt == 0 ~ 0
),                           
exp_afr_libya = case_when(
  afr_libya == 1 ~ lctn_xpnss_total,
  afr_libya == 0 ~ 0
),                           
exp_afr_morocco = case_when(
  afr_morocco == 1 ~ lctn_xpnss_total,
  afr_morocco == 0 ~ 0
),                         
exp_afr_sudan = case_when(
  afr_sudan == 1 ~ lctn_xpnss_total,
  afr_sudan == 0 ~ 0
),                           
exp_afr_tunisia = case_when(
  afr_tunisia == 1 ~ lctn_xpnss_total,
  afr_tunisia == 0 ~ 0
),                         
exp_afr_western_sahara = case_when(
  afr_western_sahara == 1 ~ lctn_xpnss_total,
  afr_western_sahara == 0 ~ 0
),                  
exp_afr_british_indian_ocean_territory = case_when(
  afr_british_indian_ocean_territory == 1 ~ lctn_xpnss_total,
  afr_british_indian_ocean_territory == 0 ~ 0
),  
exp_afr_burundi = case_when(
  afr_burundi == 1 ~ lctn_xpnss_total,
  afr_burundi == 0 ~ 0
),                         
exp_afr_comoros = case_when(
  afr_comoros == 1 ~ lctn_xpnss_total,
  afr_comoros == 0 ~ 0
),                         
exp_afr_djibouti = case_when(
  afr_djibouti == 1 ~ lctn_xpnss_total,
  afr_djibouti == 0 ~ 0
),                        
exp_afr_eritrea = case_when(
  afr_eritrea == 1 ~ lctn_xpnss_total,
  afr_eritrea == 0 ~ 0
),                         
exp_afr_ethiopia = case_when(
  afr_ethiopia == 1 ~ lctn_xpnss_total,
  afr_ethiopia == 0 ~ 0
),                        
exp_afr_french_southern_territories = case_when(
  afr_french_southern_territories == 1 ~ lctn_xpnss_total,
  afr_french_southern_territories == 0 ~ 0
),     
exp_afr_kenya = case_when(
  afr_kenya == 1 ~ lctn_xpnss_total,
  afr_kenya == 0 ~ 0
),                           
exp_afr_madagascar = case_when(
  afr_madagascar == 1 ~ lctn_xpnss_total,
  afr_madagascar == 0 ~ 0
),                      
exp_afr_malawi = case_when(
  afr_malawi == 1 ~ lctn_xpnss_total,
  afr_malawi == 0 ~ 0
),                          
exp_afr_mauritius = case_when(
  afr_mauritius == 1 ~ lctn_xpnss_total,
  afr_mauritius == 0 ~ 0
),                       
exp_afr_mayotte = case_when(
  afr_mayotte == 1 ~ lctn_xpnss_total,
  afr_mayotte == 0 ~ 0
),                         
exp_afr_mozambique = case_when(
  afr_mozambique == 1 ~ lctn_xpnss_total,
  afr_mozambique == 0 ~ 0
),                      
exp_afr_reunion = case_when(
  afr_reunion == 1 ~ lctn_xpnss_total,
  afr_reunion == 0 ~ 0
),                         
exp_afr_rwanda = case_when(
  afr_rwanda == 1 ~ lctn_xpnss_total,
  afr_rwanda == 0 ~ 0
),                          
exp_afr_seychelles = case_when(
  afr_seychelles == 1 ~ lctn_xpnss_total,
  afr_seychelles == 0 ~ 0
),                      
exp_afr_somalia = case_when(
  afr_somalia == 1 ~ lctn_xpnss_total,
  afr_somalia == 0 ~ 0
),                         
exp_afr_south_sudan = case_when(
  afr_south_sudan == 1 ~ lctn_xpnss_total,
  afr_south_sudan == 0 ~ 0
),                     
exp_afr_uganda = case_when(
  afr_uganda == 1 ~ lctn_xpnss_total,
  afr_uganda == 0 ~ 0
),                          
exp_afr_united_republic_of_tanzania = case_when(
  afr_united_republic_of_tanzania == 1 ~ lctn_xpnss_total,
  afr_united_republic_of_tanzania == 0 ~ 0
),     
exp_afr_zambia = case_when(
  afr_zambia == 1 ~ lctn_xpnss_total,
  afr_zambia == 0 ~ 0
),                          
exp_afr_zimbabwe = case_when(
  afr_zimbabwe == 1 ~ lctn_xpnss_total,
  afr_zimbabwe == 0 ~ 0
),                        
exp_afr_angola = case_when(
  afr_angola == 1 ~ lctn_xpnss_total,
  afr_angola == 0 ~ 0
),                          
exp_afr_cameroon = case_when(
  afr_cameroon == 1 ~ lctn_xpnss_total,
  afr_cameroon == 0 ~ 0
),                        
exp_afr_central_african_republic = case_when(
  afr_central_african_republic == 1 ~ lctn_xpnss_total,
  afr_central_african_republic == 0 ~ 0
),        
exp_afr_chad = case_when(
  afr_chad == 1 ~ lctn_xpnss_total,
  afr_chad == 0 ~ 0
),                            
exp_afr_congo = case_when(
  afr_congo == 1 ~ lctn_xpnss_total,
  afr_congo == 0 ~ 0
),                           
exp_afr_democratic_republic_of_the_congo = case_when(
  afr_democratic_republic_of_the_congo == 1 ~ lctn_xpnss_total,
  afr_democratic_republic_of_the_congo == 0 ~ 0
),
exp_afr_equatorial_guinea = case_when(
  afr_equatorial_guinea == 1 ~ lctn_xpnss_total,
  afr_equatorial_guinea == 0 ~ 0
),               
exp_afr_gabon = case_when(
  afr_gabon == 1 ~ lctn_xpnss_total,
  afr_gabon == 0 ~ 0
),                           
exp_afr_sao_tome_and_principe = case_when(
  afr_sao_tome_and_principe == 1 ~ lctn_xpnss_total,
  afr_sao_tome_and_principe == 0 ~ 0
),           
exp_afr_botswana = case_when(
  afr_botswana == 1 ~ lctn_xpnss_total,
  afr_botswana == 0 ~ 0
),                        
exp_afr_eswatini = case_when(
  afr_eswatini == 1 ~ lctn_xpnss_total,
  afr_eswatini == 0 ~ 0
),                        
exp_afr_lesotho = case_when(
  afr_lesotho == 1 ~ lctn_xpnss_total,
  afr_lesotho == 0 ~ 0
),                         
exp_afr_namibia = case_when(
  afr_namibia == 1 ~ lctn_xpnss_total,
  afr_namibia == 0 ~ 0
),                         
exp_afr_south_africa = case_when(
  afr_south_africa == 1 ~ lctn_xpnss_total,
  afr_south_africa == 0 ~ 0
),                    
exp_afr_benin = case_when(
  afr_benin == 1 ~ lctn_xpnss_total,
  afr_benin == 0 ~ 0
),                           
exp_afr_burkina_faso = case_when(
  afr_burkina_faso == 1 ~ lctn_xpnss_total,
  afr_burkina_faso == 0 ~ 0
),                    
exp_afr_cabo_verde = case_when(
  afr_cabo_verde == 1 ~ lctn_xpnss_total,
  afr_cabo_verde == 0 ~ 0
),                      
exp_afr_cote_divoire = case_when(
  afr_cote_divoire == 1 ~ lctn_xpnss_total,
  afr_cote_divoire == 0 ~ 0
),                    
exp_afr_gambia = case_when(
  afr_gambia == 1 ~ lctn_xpnss_total,
  afr_gambia == 0 ~ 0
),                          
exp_afr_ghana = case_when(
  afr_ghana == 1 ~ lctn_xpnss_total,
  afr_ghana == 0 ~ 0
),                           
exp_afr_guinea = case_when(
  afr_guinea == 1 ~ lctn_xpnss_total,
  afr_guinea == 0 ~ 0
),                          
exp_afr_guinea_bissau = case_when(
  afr_guinea_bissau == 1 ~ lctn_xpnss_total,
  afr_guinea_bissau == 0 ~ 0
),                   
exp_afr_liberia = case_when(
  afr_liberia == 1 ~ lctn_xpnss_total,
  afr_liberia == 0 ~ 0
),                         
exp_afr_mali = case_when(
  afr_mali == 1 ~ lctn_xpnss_total,
  afr_mali == 0 ~ 0
),                            
exp_afr_mauritania = case_when(
  afr_mauritania == 1 ~ lctn_xpnss_total,
  afr_mauritania == 0 ~ 0
),                      
exp_afr_niger = case_when(
  afr_niger == 1 ~ lctn_xpnss_total,
  afr_niger == 0 ~ 0
),                           
exp_afr_nigeria = case_when(
  afr_nigeria == 1 ~ lctn_xpnss_total,
  afr_nigeria == 0 ~ 0
),                         
exp_afr_saint_helena = case_when(
  afr_saint_helena == 1 ~ lctn_xpnss_total,
  afr_saint_helena == 0 ~ 0
),                    
exp_afr_senegal = case_when(
  afr_senegal == 1 ~ lctn_xpnss_total,
  afr_senegal == 0 ~ 0
),                         
exp_afr_sierra_leone = case_when(
  afr_sierra_leone == 1 ~ lctn_xpnss_total,
  afr_sierra_leone == 0 ~ 0
),                    
exp_afr_togo = case_when(
  afr_togo == 1 ~ lctn_xpnss_total,
  afr_togo == 0 ~ 0
),                            
exp_amr_anguilla = case_when(
  amr_anguilla == 1 ~ lctn_xpnss_total,
  amr_anguilla == 0 ~ 0
),                        
exp_amr_antigua_and_barbuda = case_when(
  amr_antigua_and_barbuda == 1 ~ lctn_xpnss_total,
  amr_antigua_and_barbuda == 0 ~ 0
),             
exp_amr_aruba = case_when(
  amr_aruba == 1 ~ lctn_xpnss_total,
  amr_aruba == 0 ~ 0
),                           
exp_amr_bahamas = case_when(
  amr_bahamas == 1 ~ lctn_xpnss_total,
  amr_bahamas == 0 ~ 0
),                         
exp_amr_barbados = case_when(
  amr_barbados == 1 ~ lctn_xpnss_total,
  amr_barbados == 0 ~ 0
),                        
exp_amr_bonaire = case_when(
  amr_bonaire == 1 ~ lctn_xpnss_total,
  amr_bonaire == 0 ~ 0
),                         
exp_amr_british_virgin_islands = case_when(
  amr_british_virgin_islands == 1 ~ lctn_xpnss_total,
  amr_british_virgin_islands == 0 ~ 0
),          
exp_amr_cayman_islands = case_when(
  amr_cayman_islands == 1 ~ lctn_xpnss_total,
  amr_cayman_islands == 0 ~ 0
),                  
exp_amr_cuba = case_when(
  amr_cuba == 1 ~ lctn_xpnss_total,
  amr_cuba == 0 ~ 0
),                            
exp_amr_curacao = case_when(
  amr_curacao == 1 ~ lctn_xpnss_total,
  amr_curacao == 0 ~ 0
),                         
exp_amr_dominica = case_when(
  amr_dominica == 1 ~ lctn_xpnss_total,
  amr_dominica == 0 ~ 0
),                        
exp_amr_dominican_republic = case_when(
  amr_dominican_republic == 1 ~ lctn_xpnss_total,
  amr_dominican_republic == 0 ~ 0
),              
exp_amr_grenada = case_when(
  amr_grenada == 1 ~ lctn_xpnss_total,
  amr_grenada == 0 ~ 0
),                         
exp_amr_guadeloupe = case_when(
  amr_guadeloupe == 1 ~ lctn_xpnss_total,
  amr_guadeloupe == 0 ~ 0
),                      
exp_amr_haiti = case_when(
  amr_haiti == 1 ~ lctn_xpnss_total,
  amr_haiti == 0 ~ 0
),                           
exp_amr_jamaica = case_when(
  amr_jamaica == 1 ~ lctn_xpnss_total,
  amr_jamaica == 0 ~ 0
),                         
exp_amr_martinique = case_when(
  amr_martinique == 1 ~ lctn_xpnss_total,
  amr_martinique == 0 ~ 0
),                      
exp_amr_montserrat = case_when(
  amr_montserrat == 1 ~ lctn_xpnss_total,
  amr_montserrat == 0 ~ 0
),                      
exp_amr_puerto_rico = case_when(
  amr_puerto_rico == 1 ~ lctn_xpnss_total,
  amr_puerto_rico == 0 ~ 0
),                     
exp_amr_saint_barthelemy = case_when(
  amr_saint_barthelemy == 1 ~ lctn_xpnss_total,
  amr_saint_barthelemy == 0 ~ 0
),                
exp_amr_saint_kitts = case_when(
  amr_saint_kitts == 1 ~ lctn_xpnss_total,
  amr_saint_kitts == 0 ~ 0
),                     
exp_amr_saint_lucia = case_when(
  amr_saint_lucia == 1 ~ lctn_xpnss_total,
  amr_saint_lucia == 0 ~ 0
),                     
exp_amr_saint_martin = case_when(
  amr_saint_martin == 1 ~ lctn_xpnss_total,
  amr_saint_martin == 0 ~ 0
),                    
exp_amr_saint_vincent = case_when(
  amr_saint_vincent == 1 ~ lctn_xpnss_total,
  amr_saint_vincent == 0 ~ 0
),                   
exp_amr_sint_maarten = case_when(
  amr_sint_maarten == 1 ~ lctn_xpnss_total,
  amr_sint_maarten == 0 ~ 0
),                    
exp_amr_trinidad_and_tobago = case_when(
  amr_trinidad_and_tobago == 1 ~ lctn_xpnss_total,
  amr_trinidad_and_tobago == 0 ~ 0
),             
exp_amr_turks_and_caicos = case_when(
  amr_turks_and_caicos == 1 ~ lctn_xpnss_total,
  amr_turks_and_caicos == 0 ~ 0
),                
exp_amr_us_virgin_islands = case_when(
  amr_us_virgin_islands == 1 ~ lctn_xpnss_total,
  amr_us_virgin_islands == 0 ~ 0
),               
exp_amr_belize = case_when(
  amr_belize == 1 ~ lctn_xpnss_total,
  amr_belize == 0 ~ 0
),                          
exp_amr_costa_rica = case_when(
  amr_costa_rica == 1 ~ lctn_xpnss_total,
  amr_costa_rica == 0 ~ 0
),                      
exp_amr_el_salvador = case_when(
  amr_el_salvador == 1 ~ lctn_xpnss_total,
  amr_el_salvador == 0 ~ 0
),                     
exp_amr_guatemala = case_when(
  amr_guatemala == 1 ~ lctn_xpnss_total,
  amr_guatemala == 0 ~ 0
),                       
exp_amr_honduras = case_when(
  amr_honduras == 1 ~ lctn_xpnss_total,
  amr_honduras == 0 ~ 0
),                        
exp_amr_mexico = case_when(
  amr_mexico == 1 ~ lctn_xpnss_total,
  amr_mexico == 0 ~ 0
),                          
exp_amr_nicaragua = case_when(
  amr_nicaragua == 1 ~ lctn_xpnss_total,
  amr_nicaragua == 0 ~ 0
),                       
exp_amr_panama = case_when(
  amr_panama == 1 ~ lctn_xpnss_total,
  amr_panama == 0 ~ 0
),                          
exp_amr_argentina = case_when(
  amr_argentina == 1 ~ lctn_xpnss_total,
  amr_argentina == 0 ~ 0
),                       
exp_amr_bolivia = case_when(
  amr_bolivia == 1 ~ lctn_xpnss_total,
  amr_bolivia == 0 ~ 0
),                         
exp_amr_bouvet = case_when(
  amr_bouvet == 1 ~ lctn_xpnss_total,
  amr_bouvet == 0 ~ 0
),                          
exp_amr_brazil = case_when(
  amr_brazil == 1 ~ lctn_xpnss_total,
  amr_brazil == 0 ~ 0
),                          
exp_amr_chile = case_when(
  amr_chile == 1 ~ lctn_xpnss_total,
  amr_chile == 0 ~ 0
),                           
exp_amr_colombia = case_when(
  amr_colombia == 1 ~ lctn_xpnss_total,
  amr_colombia == 0 ~ 0
),                        
exp_amr_ecuador = case_when(
  amr_ecuador == 1 ~ lctn_xpnss_total,
  amr_ecuador == 0 ~ 0
),                         
exp_amr_malvinas_falkland = case_when(
  amr_malvinas_falkland == 1 ~ lctn_xpnss_total,
  amr_malvinas_falkland == 0 ~ 0
),               
exp_amr_french_guiana = case_when(
  amr_french_guiana == 1 ~ lctn_xpnss_total,
  amr_french_guiana == 0 ~ 0
),                   
exp_amr_guyana = case_when(
  amr_guyana == 1 ~ lctn_xpnss_total,
  amr_guyana == 0 ~ 0
),                          
exp_amr_paraguay = case_when(
  amr_paraguay == 1 ~ lctn_xpnss_total,
  amr_paraguay == 0 ~ 0
),                        
exp_amr_peru = case_when(
  amr_peru == 1 ~ lctn_xpnss_total,
  amr_peru == 0 ~ 0
),                            
exp_amr_south_sandwich_islands = case_when(
  amr_south_sandwich_islands == 1 ~ lctn_xpnss_total,
  amr_south_sandwich_islands == 0 ~ 0
),          
exp_amr_suriname = case_when(
  amr_suriname == 1 ~ lctn_xpnss_total,
  amr_suriname == 0 ~ 0
),                        
exp_amr_uruguay = case_when(
  amr_uruguay == 1 ~ lctn_xpnss_total,
  amr_uruguay == 0 ~ 0
),                         
exp_amr_venezuela = case_when(
  amr_venezuela == 1 ~ lctn_xpnss_total,
  amr_venezuela == 0 ~ 0
),                       
exp_amr_bermuda = case_when(
  amr_bermuda == 1 ~ lctn_xpnss_total,
  amr_bermuda == 0 ~ 0
),                         
exp_amr_canada = case_when(
  amr_canada == 1 ~ lctn_xpnss_total,
  amr_canada == 0 ~ 0
),                          
exp_amr_saint_pierre = case_when(
  amr_saint_pierre == 1 ~ lctn_xpnss_total,
  amr_saint_pierre == 0 ~ 0
),                    
exp_amr_usa = case_when(
  amr_usa == 1 ~ lctn_xpnss_total,
  amr_usa == 0 ~ 0
),                             
exp_eur_belarus = case_when(
  eur_belarus == 1 ~ lctn_xpnss_total,
  eur_belarus == 0 ~ 0
),                         
exp_eur_bulgaria = case_when(
  eur_bulgaria == 1 ~ lctn_xpnss_total,
  eur_bulgaria == 0 ~ 0
),                        
exp_eur_czechia = case_when(
  eur_czechia == 1 ~ lctn_xpnss_total,
  eur_czechia == 0 ~ 0
),                         
exp_eur_hungary = case_when(
  eur_hungary == 1 ~ lctn_xpnss_total,
  eur_hungary == 0 ~ 0
),                         
exp_eur_poland = case_when(
  eur_poland == 1 ~ lctn_xpnss_total,
  eur_poland == 0 ~ 0
),                          
exp_eur_moldova = case_when(
  eur_moldova == 1 ~ lctn_xpnss_total,
  eur_moldova == 0 ~ 0
),                         
exp_eur_romania = case_when(
  eur_romania == 1 ~ lctn_xpnss_total,
  eur_romania == 0 ~ 0
),                         
exp_eur_russia = case_when(
  eur_russia == 1 ~ lctn_xpnss_total,
  eur_russia == 0 ~ 0
),                          
exp_eur_slovakia = case_when(
  eur_slovakia == 1 ~ lctn_xpnss_total,
  eur_slovakia == 0 ~ 0
),                        
exp_eur_ukraine = case_when(
  eur_ukraine == 1 ~ lctn_xpnss_total,
  eur_ukraine == 0 ~ 0
),                         
exp_eur_aland_islands = case_when(
  eur_aland_islands == 1 ~ lctn_xpnss_total,
  eur_aland_islands == 0 ~ 0
),                   
exp_eur_guernsey = case_when(
  eur_guernsey == 1 ~ lctn_xpnss_total,
  eur_guernsey == 0 ~ 0
),                        
exp_eur_jersey = case_when(
  eur_jersey == 1 ~ lctn_xpnss_total,
  eur_jersey == 0 ~ 0
),                          
exp_eur_sark = case_when(
  eur_sark == 1 ~ lctn_xpnss_total,
  eur_sark == 0 ~ 0
),                            
exp_eur_denmark = case_when(
  eur_denmark == 1 ~ lctn_xpnss_total,
  eur_denmark == 0 ~ 0
),                         
exp_eur_estonia = case_when(
  eur_estonia == 1 ~ lctn_xpnss_total,
  eur_estonia == 0 ~ 0
),                         
exp_eur_faroe_islands = case_when(
  eur_faroe_islands == 1 ~ lctn_xpnss_total,
  eur_faroe_islands == 0 ~ 0
),                   
exp_eur_finland = case_when(
  eur_finland == 1 ~ lctn_xpnss_total,
  eur_finland == 0 ~ 0
),                         
exp_eur_iceland = case_when(
  eur_iceland == 1 ~ lctn_xpnss_total,
  eur_iceland == 0 ~ 0
),                         
exp_eur_ireland = case_when(
  eur_ireland == 1 ~ lctn_xpnss_total,
  eur_ireland == 0 ~ 0
),                         
exp_eur_isle_of_man = case_when(
  eur_isle_of_man == 1 ~ lctn_xpnss_total,
  eur_isle_of_man == 0 ~ 0
),                     
exp_eur_latvia = case_when(
  eur_latvia == 1 ~ lctn_xpnss_total,
  eur_latvia == 0 ~ 0
),                          
exp_eur_lithuania = case_when(
  eur_lithuania == 1 ~ lctn_xpnss_total,
  eur_lithuania == 0 ~ 0
),                       
exp_eur_norway = case_when(
  eur_norway == 1 ~ lctn_xpnss_total,
  eur_norway == 0 ~ 0
),                          
exp_eur_svalbard_and_jan_mayen_islands = case_when(
  eur_svalbard_and_jan_mayen_islands == 1 ~ lctn_xpnss_total,
  eur_svalbard_and_jan_mayen_islands == 0 ~ 0
),  
exp_eur_sweden = case_when(
  eur_sweden == 1 ~ lctn_xpnss_total,
  eur_sweden == 0 ~ 0
),                          
exp_eur_united_kingdom = case_when(
  eur_united_kingdom == 1 ~ lctn_xpnss_total,
  eur_united_kingdom == 0 ~ 0
),                  
exp_eur_albania = case_when(
  eur_albania == 1 ~ lctn_xpnss_total,
  eur_albania == 0 ~ 0
),                         
exp_eur_andorra = case_when(
  eur_andorra == 1 ~ lctn_xpnss_total,
  eur_andorra == 0 ~ 0
),                         
exp_eur_bosnia_and_herzegovina = case_when(
  eur_bosnia_and_herzegovina == 1 ~ lctn_xpnss_total,
  eur_bosnia_and_herzegovina == 0 ~ 0
),          
exp_eur_croatia = case_when(
  eur_croatia == 1 ~ lctn_xpnss_total,
  eur_croatia == 0 ~ 0
),                         
exp_eur_gibraltar = case_when(
  eur_gibraltar == 1 ~ lctn_xpnss_total,
  eur_gibraltar == 0 ~ 0
),                       
exp_eur_greece = case_when(
  eur_greece == 1 ~ lctn_xpnss_total,
  eur_greece == 0 ~ 0
),                          
exp_eur_vatican = case_when(
  eur_vatican == 1 ~ lctn_xpnss_total,
  eur_vatican == 0 ~ 0
),                         
exp_eur_italy = case_when(
  eur_italy == 1 ~ lctn_xpnss_total,
  eur_italy == 0 ~ 0
),                           
exp_eur_malta = case_when(
  eur_malta == 1 ~ lctn_xpnss_total,
  eur_malta == 0 ~ 0
),                           
exp_eur_montenegro = case_when(
  eur_montenegro == 1 ~ lctn_xpnss_total,
  eur_montenegro == 0 ~ 0
),                      
exp_eur_north_macedonia = case_when(
  eur_north_macedonia == 1 ~ lctn_xpnss_total,
  eur_north_macedonia == 0 ~ 0
),                 
exp_eur_portugal = case_when(
  eur_portugal == 1 ~ lctn_xpnss_total,
  eur_portugal == 0 ~ 0
),                        
exp_eur_san_marino = case_when(
  eur_san_marino == 1 ~ lctn_xpnss_total,
  eur_san_marino == 0 ~ 0
),                      
exp_eur_serbia = case_when(
  eur_serbia == 1 ~ lctn_xpnss_total,
  eur_serbia == 0 ~ 0
),                          
exp_eur_slovenia = case_when(
  eur_slovenia == 1 ~ lctn_xpnss_total,
  eur_slovenia == 0 ~ 0
),                        
exp_eur_spain = case_when(
  eur_spain == 1 ~ lctn_xpnss_total,
  eur_spain == 0 ~ 0
),                           
exp_eur_austria = case_when(
  eur_austria == 1 ~ lctn_xpnss_total,
  eur_austria == 0 ~ 0
),                         
exp_eur_belgium = case_when(
  eur_belgium == 1 ~ lctn_xpnss_total,
  eur_belgium == 0 ~ 0
),                         
exp_eur_france = case_when(
  eur_france == 1 ~ lctn_xpnss_total,
  eur_france == 0 ~ 0
),                          
exp_eur_germany = case_when(
  eur_germany == 1 ~ lctn_xpnss_total,
  eur_germany == 0 ~ 0
),                         
exp_eur_liechtenstein = case_when(
  eur_liechtenstein == 1 ~ lctn_xpnss_total,
  eur_liechtenstein == 0 ~ 0
),                   
exp_eur_luxembourg = case_when(
  eur_luxembourg == 1 ~ lctn_xpnss_total,
  eur_luxembourg == 0 ~ 0
),                      
exp_eur_monaco = case_when(
  eur_monaco == 1 ~ lctn_xpnss_total,
  eur_monaco == 0 ~ 0
),                          
exp_eur_netherlands = case_when(
  eur_netherlands == 1 ~ lctn_xpnss_total,
  eur_netherlands == 0 ~ 0
),                     
exp_eur_switzerland = case_when(
  eur_switzerland == 1 ~ lctn_xpnss_total,
  eur_switzerland == 0 ~ 0
),                     
exp_asia_kazakhstan = case_when(
  asia_kazakhstan == 1 ~ lctn_xpnss_total,
  asia_kazakhstan == 0 ~ 0
),                     
exp_asia_kyrgyzstan = case_when(
  asia_kyrgyzstan == 1 ~ lctn_xpnss_total,
  asia_kyrgyzstan == 0 ~ 0
),                     
exp_asia_tajikistan = case_when(
  asia_tajikistan == 1 ~ lctn_xpnss_total,
  asia_tajikistan == 0 ~ 0
),                     
exp_asia_turkmenistan = case_when(
  asia_turkmenistan == 1 ~ lctn_xpnss_total,
  asia_turkmenistan == 0 ~ 0
),                   
exp_asia_uzbekistan = case_when(
  asia_uzbekistan == 1 ~ lctn_xpnss_total,
  asia_uzbekistan == 0 ~ 0
),                     
exp_asia_china = case_when(
  asia_china == 1 ~ lctn_xpnss_total,
  asia_china == 0 ~ 0
),                          
exp_asia_hong_kong = case_when(
  asia_hong_kong == 1 ~ lctn_xpnss_total,
  asia_hong_kong == 0 ~ 0
),                      
exp_asia_macao = case_when(
  asia_macao == 1 ~ lctn_xpnss_total,
  asia_macao == 0 ~ 0
),                          
exp_asia_north_korea = case_when(
  asia_north_korea == 1 ~ lctn_xpnss_total,
  asia_north_korea == 0 ~ 0
),                    
exp_asia_japan = case_when(
  asia_japan == 1 ~ lctn_xpnss_total,
  asia_japan == 0 ~ 0
),                          
exp_asia_mongolia = case_when(
  asia_mongolia == 1 ~ lctn_xpnss_total,
  asia_mongolia == 0 ~ 0
),                       
exp_asia_south_korea = case_when(
  asia_south_korea == 1 ~ lctn_xpnss_total,
  asia_south_korea == 0 ~ 0
),                    
exp_asia_brunei_darussalam = case_when(
  asia_brunei_darussalam == 1 ~ lctn_xpnss_total,
  asia_brunei_darussalam == 0 ~ 0
),              
exp_asia_cambodia = case_when(
  asia_cambodia == 1 ~ lctn_xpnss_total,
  asia_cambodia == 0 ~ 0
),                       
exp_asia_indonesia = case_when(
  asia_indonesia == 1 ~ lctn_xpnss_total,
  asia_indonesia == 0 ~ 0
),                      
exp_asia_laos = case_when(
  asia_laos == 1 ~ lctn_xpnss_total,
  asia_laos == 0 ~ 0
),                           
exp_asia_malaysia = case_when(
  asia_malaysia == 1 ~ lctn_xpnss_total,
  asia_malaysia == 0 ~ 0
),                       
exp_asia_myanmar = case_when(
  asia_myanmar == 1 ~ lctn_xpnss_total,
  asia_myanmar == 0 ~ 0
),                        
exp_asia_philippines = case_when(
  asia_philippines == 1 ~ lctn_xpnss_total,
  asia_philippines == 0 ~ 0
),                    
exp_asia_singapore = case_when(
  asia_singapore == 1 ~ lctn_xpnss_total,
  asia_singapore == 0 ~ 0
),                      
exp_asia_thailand = case_when(
  asia_thailand == 1 ~ lctn_xpnss_total,
  asia_thailand == 0 ~ 0
),                       
exp_asia_timor_leste = case_when(
  asia_timor_leste == 1 ~ lctn_xpnss_total,
  asia_timor_leste == 0 ~ 0
),                    
exp_asia_vietnam = case_when(
  asia_vietnam == 1 ~ lctn_xpnss_total,
  asia_vietnam == 0 ~ 0
),                        
exp_asia_afghanistan = case_when(
  asia_afghanistan == 1 ~ lctn_xpnss_total,
  asia_afghanistan == 0 ~ 0
),                    
exp_asia_bangladesh = case_when(
  asia_bangladesh == 1 ~ lctn_xpnss_total,
  asia_bangladesh == 0 ~ 0
),                     
exp_asia_bhutan = case_when(
  asia_bhutan == 1 ~ lctn_xpnss_total,
  asia_bhutan == 0 ~ 0
),                         
exp_asia_india = case_when(
  asia_india == 1 ~ lctn_xpnss_total,
  asia_india == 0 ~ 0
),                          
exp_asia_iran = case_when(
  asia_iran == 1 ~ lctn_xpnss_total,
  asia_iran == 0 ~ 0
),                           
exp_asia_maldives = case_when(
  asia_maldives == 1 ~ lctn_xpnss_total,
  asia_maldives == 0 ~ 0
),                       
exp_asia_nepal = case_when(
  asia_nepal == 1 ~ lctn_xpnss_total,
  asia_nepal == 0 ~ 0
),                          
exp_asia_pakistan = case_when(
  asia_pakistan == 1 ~ lctn_xpnss_total,
  asia_pakistan == 0 ~ 0
),                       
exp_asia_sri_lanka = case_when(
  asia_sri_lanka == 1 ~ lctn_xpnss_total,
  asia_sri_lanka == 0 ~ 0
),                      
exp_asia_armenia = case_when(
  asia_armenia == 1 ~ lctn_xpnss_total,
  asia_armenia == 0 ~ 0
),                        
exp_asia_azerbaijan = case_when(
  asia_azerbaijan == 1 ~ lctn_xpnss_total,
  asia_azerbaijan == 0 ~ 0
),                     
exp_asia_bahrain = case_when(
  asia_bahrain == 1 ~ lctn_xpnss_total,
  asia_bahrain == 0 ~ 0
),                        
exp_asia_cyprus = case_when(
  asia_cyprus == 1 ~ lctn_xpnss_total,
  asia_cyprus == 0 ~ 0
),                         
exp_asia_georgia = case_when(
  asia_georgia == 1 ~ lctn_xpnss_total,
  asia_georgia == 0 ~ 0
),                        
exp_asia_iraq = case_when(
  asia_iraq == 1 ~ lctn_xpnss_total,
  asia_iraq == 0 ~ 0
),                           
exp_asia_israel = case_when(
  asia_israel == 1 ~ lctn_xpnss_total,
  asia_israel == 0 ~ 0
),                         
exp_asia_jordan = case_when(
  asia_jordan == 1 ~ lctn_xpnss_total,
  asia_jordan == 0 ~ 0
),                         
exp_asia_kuwait = case_when(
  asia_kuwait == 1 ~ lctn_xpnss_total,
  asia_kuwait == 0 ~ 0
),                         
exp_asia_lebanon = case_when(
  asia_lebanon == 1 ~ lctn_xpnss_total,
  asia_lebanon == 0 ~ 0
),                        
exp_asia_oman = case_when(
  asia_oman == 1 ~ lctn_xpnss_total,
  asia_oman == 0 ~ 0
),                           
exp_asia_qatar = case_when(
  asia_qatar == 1 ~ lctn_xpnss_total,
  asia_qatar == 0 ~ 0
),                          
exp_asia_saudi_arabia = case_when(
  asia_saudi_arabia == 1 ~ lctn_xpnss_total,
  asia_saudi_arabia == 0 ~ 0
),                   
exp_asia_palestine = case_when(
  asia_palestine == 1 ~ lctn_xpnss_total,
  asia_palestine == 0 ~ 0
),                      
exp_asia_syria = case_when(
  asia_syria == 1 ~ lctn_xpnss_total,
  asia_syria == 0 ~ 0
),                          
exp_asia_turkey = case_when(
  asia_turkey == 1 ~ lctn_xpnss_total,
  asia_turkey == 0 ~ 0
),                         
exp_asia_uae = case_when(
  asia_uae == 1 ~ lctn_xpnss_total,
  asia_uae == 0 ~ 0
),                            
exp_asia_yemen = case_when(
  asia_yemen == 1 ~ lctn_xpnss_total,
  asia_yemen == 0 ~ 0
),                          
exp_asia_australia = case_when(
  asia_australia == 1 ~ lctn_xpnss_total,
  asia_australia == 0 ~ 0
),                      
exp_asia_christmas = case_when(
  asia_christmas == 1 ~ lctn_xpnss_total,
  asia_christmas == 0 ~ 0
),                      
exp_asia_cocos_keeling = case_when(
  asia_cocos_keeling == 1 ~ lctn_xpnss_total,
  asia_cocos_keeling == 0 ~ 0
),                  
exp_asia_heard_and_mcdonald = case_when(
  asia_heard_and_mcdonald == 1 ~ lctn_xpnss_total,
  asia_heard_and_mcdonald == 0 ~ 0
),             
exp_asia_new_zealand = case_when(
  asia_new_zealand == 1 ~ lctn_xpnss_total,
  asia_new_zealand == 0 ~ 0
),                    
exp_asia_norfolk_island = case_when(
  asia_norfolk_island == 1 ~ lctn_xpnss_total,
  asia_norfolk_island == 0 ~ 0
),                 
exp_asia_fiji = case_when(
  asia_fiji == 1 ~ lctn_xpnss_total,
  asia_fiji == 0 ~ 0
),                           
exp_asia_new_caledonia = case_when(
  asia_new_caledonia == 1 ~ lctn_xpnss_total,
  asia_new_caledonia == 0 ~ 0
),                  
exp_asia_papua_new_guinea = case_when(
  asia_papua_new_guinea == 1 ~ lctn_xpnss_total,
  asia_papua_new_guinea == 0 ~ 0
),               
exp_asia_solomon_islands = case_when(
  asia_solomon_islands == 1 ~ lctn_xpnss_total,
  asia_solomon_islands == 0 ~ 0
),                
exp_asia_vanuatu = case_when(
  asia_vanuatu == 1 ~ lctn_xpnss_total,
  asia_vanuatu == 0 ~ 0
),                        
exp_asia_guam = case_when(
  asia_guam == 1 ~ lctn_xpnss_total,
  asia_guam == 0 ~ 0
),                           
exp_asia_kiribati = case_when(
  asia_kiribati == 1 ~ lctn_xpnss_total,
  asia_kiribati == 0 ~ 0
),                       
exp_asia_marshall_islands = case_when(
  asia_marshall_islands == 1 ~ lctn_xpnss_total,
  asia_marshall_islands == 0 ~ 0
),               
exp_asia_micronesia = case_when(
  asia_micronesia == 1 ~ lctn_xpnss_total,
  asia_micronesia == 0 ~ 0
),                     
exp_asia_nauru = case_when(
  asia_nauru == 1 ~ lctn_xpnss_total,
  asia_nauru == 0 ~ 0
),                          
exp_asia_northern_mariana_islands = case_when(
  asia_northern_mariana_islands == 1 ~ lctn_xpnss_total,
  asia_northern_mariana_islands == 0 ~ 0
),       
exp_asia_palau = case_when(
  asia_palau == 1 ~ lctn_xpnss_total,
  asia_palau == 0 ~ 0
),                          
exp_asia_usa_minor = case_when(
  asia_usa_minor == 1 ~ lctn_xpnss_total,
  asia_usa_minor == 0 ~ 0
),                      
exp_asia_american_samoa = case_when(
  asia_american_samoa == 1 ~ lctn_xpnss_total,
  asia_american_samoa == 0 ~ 0
),                 
exp_asia_cook_islands = case_when(
  asia_cook_islands == 1 ~ lctn_xpnss_total,
  asia_cook_islands == 0 ~ 0
),                   
exp_asia_french_polynesia = case_when(
  asia_french_polynesia == 1 ~ lctn_xpnss_total,
  asia_french_polynesia == 0 ~ 0
),               
exp_asia_niue = case_when(
  asia_niue == 1 ~ lctn_xpnss_total,
  asia_niue == 0 ~ 0
),                           
exp_asia_pitcairn = case_when(
  asia_pitcairn == 1 ~ lctn_xpnss_total,
  asia_pitcairn == 0 ~ 0
),                       
exp_asia_samoa = case_when(
  asia_samoa == 1 ~ lctn_xpnss_total,
  asia_samoa == 0 ~ 0
),                          
exp_asia_tokelau = case_when(
  asia_tokelau == 1 ~ lctn_xpnss_total,
  asia_tokelau == 0 ~ 0
),                        
exp_asia_tonga = case_when(
  asia_tonga == 1 ~ lctn_xpnss_total,
  asia_tonga == 0 ~ 0
),                          
exp_asia_tuvalu = case_when(
  asia_tuvalu == 1 ~ lctn_xpnss_total,
  asia_tuvalu == 0 ~ 0
),                         
exp_asia_wallis_and_futuna = case_when(
  asia_wallis_and_futuna == 1 ~ lctn_xpnss_total,
  asia_wallis_and_futuna == 0 ~ 0
)
)

activities_country <- activities_country %>%
  mutate_at(
    c(261, 507), ~replace_na(.,0)
  )

#-----------------------------------------------------------
# Data export
#-----------------------------------------------------------
# write_csv(activities_country,
#           "/Volumes/SRC_DATA/000_f990_data/activities_country_230623.csv")
#-----------------------------------------------------------