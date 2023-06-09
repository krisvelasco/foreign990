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
  
asia_colnames <- pull(list_asia_pacific, `Country or Area`)
  asia_colnames <- str_to_lower(asia_colnames)
  asia_colnames2 <- str_replace_all(asia_colnames,
                                        " ",
                                        "_")
  
europe_colnames <- pull(list_europe, `Country or Area`)
  europe_colnames <- str_to_lower(europe_colnames)
  europe_colnames2 <- str_replace_all(europe_colnames,
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
) %>%
  mutate_at(
    c(17, 272), ~replace_na(.,0)
    )



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