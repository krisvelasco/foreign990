# Vectors for cleaning activities data

#-----------------------------------
# NO REGION INFO
#   f_location strings in region2 == "NO REGION INFO"
#-----------------------------------

acts_noInfo <- activities_cln %>%
  filter(region2 == "NO REGION INFO")

acts_noInfo_count <- acts_noInfo %>%
  count(f_location) %>%
  arrange(desc(n))

plot(density(acts_noInfo_count$n))

acts_noInfo_10 <- acts_noInfo_count %>%
  filter(
    n > 10
  ) # locations that appear at least 10 times.
    # will get us 85% of the observations

# ACTUALLY, LOOKING AT acts_noInfo, THESE ARE PRETTY
# ACCURATE. Correcting only the ones where locations appear
# at least 10 times which could be for multiple regions or
# that otherwise are a mistake in data input

noregion_EuropeAsia <- c("russia and neighboring states",
"russia and the newly independent states",
"russia and neighboring states - armenia, azerbijan, belarus," ,       
"russia & the newly independent states"                               ,
"russia & neighboring states"                                         ,
"russia and the neighboring states"                                   ,
"russia & the newly independent states -",
"russia & the newly independent states - armenia, azerbijan, belarus,",
"russia and newly independent states",
"russia & newly independent states" ,
"russia/independent states"   ,
"russia & nis"                                                        ,
"russian and neighboring states"   ,
"russia & the newly ind states"   ,
"russia & ind. states"    ,
"russia/neighboring states",
"russia and neighbor states",
"russia and ind. states")

noregion_AsiaAfria <- c("middle east", "mideast", "middle east/ israel", "middle east/israel", "middle east - israel", "middle east (includes israel)", "middle east (israel)")

noregion_Americas <- c("caribbean", "central amer & caribbean", "central amer/caribbean", "carribean", "central am. & caribbean", "caribbean basin", "guatamala", "columbia", "cent amer/caribbean", "the caribbean", "central amer. & carribean", "equador")

noregion_Antarctica <- c("antarctica")

noregion_NoInfo <- c("various", "see part v", "worldwide", "global", "other", "international", "n/a", "other countries")

noregion_Africa <- c("sub-saharan", "sierre leone", "sub sahara")

noregion_Asia <- c("all regions in vietnam", "central vietnam", "the south caucasus")

UnitedStates <- c("united states")
 