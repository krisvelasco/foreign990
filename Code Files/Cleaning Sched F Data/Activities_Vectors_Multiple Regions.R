# Vectors for cleaning activities data

#-----------------------------------
# MULTIPLE REGIONS
#   f_location strings in multi == 1
#-----------------------------------

#acts_multi <- activities_cln %>%
#  filter(dest_multi == 1)

# Asia
Asia <- c("american samoa", "papua new guinea") 

# Europe.
#------------------------------
Europe <- unique(c("europe - romania",
                   "europe - denmark, croatia, germany, hungary, romania, slovakia, slovenia",
                   "botosani, romania",
                   "birlad, romania",
                   "eastern europte - romania",
                   "europe (romania)", "europe-including iceland and greenland",
                   "europe, iceland and greenland", 
                   "europe (iceland & greenland)",
                   "europe (including iceland & greenland",
                   "europe (incl iceland&greenland",
                   "europe, incl iceland and greenland",
                   "europe(greenland, iceland)",
                   "europe (incl iceland / greenland)",
                   "europe (including iceland & greenland) - albania, andorra, austria,belgium",
                   "europe(including iceland & greenland)",
                   "europe - including iceland and greenland",
                   "europe including iceland greenland",
                   "europe ( including iceland and greenland",
                   "europe(inc. iceland&greenland)",
                   "europe / iceland / greenland",
                   "europe (including iceland & greenland) - denmark, estonia, france, spain",
                   "europe. iceland, greenland, albania, and",
                   "europe, iceland, greenland, albania, an",
                   "europe(including iceland & greenland)-albania, andorra, austria, belgium",
                   "europe (mcludmg iceland and greenland)" , "europe (incl. iceland and greenland)",
                   "europe & iceland & greenland",
                   "europe( including iceland & greenland)",
                   "europe, iceland, and greenland",
                   "europe, incl. iceland and greenland",
                   "europe(iceland & greenland)",
                   "europe (incl iceland and greenland)",
                   "europe (including iceland & greenland - albania, andorra, austria, belgium",
                   "europe (including iceland & greenland)- albania, andorra, austria, belgium",
                   "europe (including iceland & greenland) - bulgaria, hungary, poland, romania",
                   "europe (greenland, iceland)",
                   "europe (including iceland & greenland - albania, andora, austria, belgium",
                   "europe (including iceland & greenland) - spain, portugal, united kingdom",
                   "europe ( including iceland & greenland)",
                   "europe - including iceland & greenland",
                   "europe (including iceland & greenland, albania, andorra, austria, belgium)",
                   "europe(including iceland and greenland",
                   "europe (including iceland & greenland)-albania, andorra, austria, belgium",
                   "europe (incl iceland & greenland)",
                   "europe, iceland, & greenland",
                   "europe including iceland & greenland",
                   "europe (incl iceland/greenland)",
                   "europe vendor(including iceland and greenland)",
                   "europe (including iceland & greenland) - sweden",
                   "europe (including iceland & greenland) - france",
                   "europe (including iseland & greenland)-bulgaria, hungary, poland, romania",
                   "europe (including iceland and greenland) (romania)",
                   "europe, iceland & greenland",
                   "europe - iceland, greenland, albania, andorra, austria, belgium",
                   "europe (including iceland & greenland) - ireland",
                   "europe( including iceland and greenland)",
                   "europe(iceland/greenland)",
                   "europe(including icelane & greenland)",
                   "europe (including iceland & greenland) - austria, belgium, germany, italy,",
                   "europe (iceland, greenland)",
                   "europe (including iceland and greenland)",
                   "europe/iceland and greenland", "europe (including iceland & greenland) - albania, andorra, austria, belgium",
                   "europe (including iceland & greenland)",
                   "europe including iceland and greenland",
                   "europe (including iceland & greenland) -",
                   "europe/iceland/greenland",
                   "europe (including inceland & greenland)",
                   "europe (including iceland & greenland) - albania, andorra, austria, belgiu",
                   "europe (including iceland and greenland", "europe(including iceland and greenland)",
                   "europe ( including iceland and greenland)",
                   "europe/inceland/greenland",
                   "europe ( including iceland and greenland",
                   "europe (including iceland & greenland) - denmark, estonia, france, spain",
                   "europe. iceland, greenland, albania, and",
                   "europe, iceland, greenland, albania, an",
                   "europe(including iceland & greenland)-albania, andorra, austria, belgium",
                   "europe (mcludmg iceland and greenland)", "europe (incl. iceland and greenland)",
                   "europe (including iceland & greenland, albania, andorra, austria, belgium)",
                   "europe (including iceland & greenland)-albania, andorra, austria, belgium",
                   "europe(including icelane & greenland)",
                   "europe vendor(including iceland and greenland)",
                   "europe including iceland & greenland",
                   "europe (including iceland & greenland)- albania, andorra, austria, belgium",
                   "romania",
                   "timisoara, romania (europe)",
                   "europe (including romania) -", 
                   "northeastern romania",
                   "tulcea, romania",
                   "timis county, romania",
                   "romania-eu",
                   "e. europe-roman",
                   "botosani, romania"))
#------------------------------

# Africa and Asia
#------------------------------
AsiaAfrica <- unique(c("middle east and north africa - algeria, bahrain, djibouti, egypt,",
                       "middle east and north africa (israel)",
                       "middle east and north africa - egypt, iraq, israel, kuwait, tunisia",
                       "east africa and the pacific",
                       "china / philippines / kenya",
                       "sub-saharan africa south asia",
                       "middle east and north africa - israel",
                       "middle east and north africa - jordan",
                       "middle east and north africa - cyprus",
                       "middle east & north africa - jordan",
                       "middle east and north africa - algeria, bahrain, djibouti, egypt, israel",
                       "east africa & the pacific",
                       "middle east and north africa - algeria, bahrain, djibouti, egypt, iran, ira",
                       "lebanon,israel,iran,tunisia",
                       "middle east and north africa - pakistan",
                       "middle east and north africa - algeria, bahrain, djibouti, dubai, egypt,",
                       "middle east and north africa - algeria bahrain djibouti egypt",
                       "middle east and north africa - algeria, bahrain, djibouti, egypt, iran",
                       "ethiopia, jordan",
                       "south asia - kenya & nepal",
                       "africa/middle east/india",
                       "middle east and north africa - algeria, bahrain, djibouti, egypt, iran,",
                       "china / kenya",
                       "south asia and sub-saharan africa",
                       "middle east and north africa - algeria, bahrain, djibouti, egypt",
                       "china/india/kenya",
                       "uganda / nepal",
                       "middle east and north africa, lebanon",
                       "middle east and north africa - saudi arabia",
                       "africa/asia",
                       "middle east and north africa -algeria, bahrain, djibouti, egypt,",
                       "india, kenya",
                       "sub-saharan africa -south central asia - afghasnitan, bangladesh, bhutan, i",
                       "asia, india & africa",
                       "middle east and north africa- israel",
                       "asia, africa",
                       "asia pacific & africa",
                       "middle east and north africa - jordan,",
                       "middle east and north africa - algeria, bahrain, dijibouti, egypt",
                       "cyprus & tunisia",
                       "south asia/africa",
                       "middle east (egypt and lebanon)",
                       "middle east and africa and india",
                       "asia, bangladesh and north africa - algeria, bahrain, djibouti, egypt,",
                       "middle east and north africa - algeria, bahrain, djibouti, egypt,,",
                       "middle east and africa-israel",
                       "emiddle east and north africa - algeria, bahrain, djibouti, egypt,",
                       "middle east and north africa - cyprus and morocco",
                       "middle east and north africa - iraq, jordan, morocco",
                       "algeria, bahrain, djibouti,"))
#------------------------------

# Europe and Asia
#------------------------------
EuropeAsia <- unique(c("russia & neighboring states - armenia, azerbijan, belarus,",
                       "russia and the newly independent states",
                       "russia and neighboring states - armenia, azerbijan, belarus,",
                       "russia & the newly independent states - armenia, azerbijan, belarus,",
                       "europe/eurasia",
                       "central asia and eastern europe (including armenia and moldova)",
                       "europe & eastern asia",
                       "europe and east asia",
                       "europe & asia",
                       "russia and neighboring states - armenia, azerbaijan, belarus,",
                       "europe and central asia",
                       "china, netherlands & uk",
                       "cantral asia and eastern europe (including armenia and moldova)",
                       "japan, germany, russia, denmrk",
                       "europe, australia, uniited kingdom, japa",
                       "east asia & the pacific, europe, russia & the newly independent states,",
                       "australia/netherlands",
                       "europe & central asia",
                       "europe and eurasia",
                       "europe & eurasia",
                       "eastern europe central asia",
                       "europe -georgia",
                       "europe - georgia",
                       "europe -georgia reap",
                       "europe -georgia g-hip",
                       "eastern europe (armenia)",
                       "russia and neighboring states - kazakhstan and ukraine",
                       "europe (georgia)",
                       "russia & the newly independent states - armenia, azerbaijan, belarus,",
                       "india, switzerland, bangladesh, nepal",
                       "russia & the newly independent states - armenia, azerbijan, ukraine",
                       "russia - ukraine & uzbekistan",
                       "europe/central asia",
                       "russia and neighboring states - armenia, azerbjan, belarus",
                       "russia and neighboring states - ukraine and tajikistan",
                       "russia and neighboring states - armenia, azerbijan, belarus, israel,",
                       "europe -georgia apg",
                       "russia and newly independent states (hungary, romania, bulgaria)",
                       "israel and greece"))
#------------------------------

# Europe and Americas
EuropeAmericas <- c("america, and europe", "europe (bermuda-united kingdom)"                                            , "europe(bermuda-united kingdom)", "europe (spain) - canada and mexico, but not the united states" , "europe and canada" )

# Asia, Europe, Africa
#------------------------------
AsiaEuropeAfrica <- unique(c("europe/s. africa/middle east",
                      "israel, jordan, turkey, tanzania, zimbabwe, u.k., new zealand, australia",
                      "europe, middle east and africa (emea)",
                      "europe, mid east, north africa",
                      "east asia, pacific, europe, middle east,",
                      "europe, the middle east and africa",
                      "europe, middle east and africa",
                      "europe, middle east & north africa",
                      "middle east, europe, and north africa - algeria, bahrain, djibouti, egypt,",
                      "middle east and north africa - algeria, bahrain, djibouti, egypt, europe",
                      "europe, middle east & africa regional meeting", "middle east/morocco/europe"))
#------------------------------

# Europe, Africa, Asia, Americas
#------------------------------
AsiaEuropeAfricaAmericas <- unique(c("europe, africa, asia, central america, latin america, kenya",
                              "europe, south america, africa, india",
                              "central america and carribeaneast asia and the pacificeuropemiddle ea"))
#------------------------------

# Africa, Americas
#------------------------------
AfricaAmericas <- unique(c("africa & latin america",
                    "sub saharan africa ecuador",
                    "central america, north america, west africa",
                    "north africa and south america"))
#------------------------------

# Asia, Americas, Africa
#------------------------------
AsiaAmericasAfrica <- unique(c("africa, asia & south america",
                        "other-africa,asia pacific,latin america",
                        "middle east,northern africa and latin america",
                        "s america, s asia, sub-saharan africa",
                        "mexico, new zealand, tanzania",
                        "south america, south asia, canada/mexico, central america, africa",
                        "africa, cambodia, haiti",
                        "n africa, canada, mexico, russia,"))
#------------------------------

# Asia, Americas
#------------------------------
AsiaAmericas <- unique(c("americas/israel",
                  "india & mexico",
                  "central america and the caribbeaneast asia and the pacificnorth america",
                  "haiti/philippines",
                  "nepal, haiti, cambodia, central & south america",
                  "india, andes & amazon (south america), nepal, china",
                  "china, latin america, india",
                  "brazil, india & nicaragua"))
#------------------------------

# Africa and Europe
#------------------------------
AfricaEurope <- c("west africa & europe")
#------------------------------

# Europe, Asia, Americas
#------------------------------
EuropeAsiaAmericas <- unique(c("europe, asia and north america (not the united states)",
                        "mexico, new zealand, albania",
                        "asia, western europe, caribbea",
                        "central america, caribbean, asia and europe",
                        "s. asia: afghanistan, bangladesh. east europe. s. america"))
#------------------------------