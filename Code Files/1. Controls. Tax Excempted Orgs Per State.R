## Project: Nonprofit Foreign Expenditures

## Overview:
# State-level controls.
# Tax-excempt organizations and churches per state, 2008-2018

## Source
#   Business Master Files from Urban Institute
#       https://nccs-data.urban.org/data.php?ds=bmf
#   UI codebook. For selection and cleaning.
#       https://nccs-data.urban.org/dd2.php?close=1&form=BMF+08/2016

## Last updated: July 20th by Sebastian Rojas Cabal
#--------------------------------------------------------
#--------------------------------------------------------
# Loading packages
#--------------------------------------------------------
library(tidyverse)
#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
# Data import and cleaning
#--------------------------------------------------------
file_master <- c("/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/")
file_names <- list.files(file_master)
  files_08 <- c("bmf.bm0801.csv", "bmf.bm0804.csv", "bmf.bm0806.csv", "bmf.bm0810.csv", "bmf.bm0812.csv")
  files_09 <- c("bmf.bm0901.csv", "bmf.bm0904.csv", "bmf.bm0907.csv", "bmf.bm0910.csv")
  files_10 <- c("bmf.bm1001.csv", "bmf.bm1004.csv", "bmf.bm1005.csv", "bmf.bm1007.csv", "bmf.bm1008.csv", "bmf.bm1011.csv")
  files_11 <- c("bmf.bm1106.csv", "bmf.bm1107.csv", "bmf.bm1108.csv", "bmf.bm1109.csv", "bmf.bm1110.csv", "bmf.bm1111.csv", "bmf.bm1112.csv")
  files_12 <- c("bmf.bm1202.csv", "bmf.bm1203.csv", "bmf.bm1204.csv", "bmf.bm1206.csv", "bmf.bm1207.csv", "bmf.bm1208.csv", "bmf.bm1210.csv", "bmf.bm1211.csv", "bmf.bm1212.csv")
  files_13 <- c("bmf.bm1302.csv", "bmf.bm1303.csv", "bmf.bm1304.csv", "bmf.bm1305.csv", "bmf.bm1306.csv", "bmf.bm1307.csv", "bmf.bm1308.csv", "bmf.bm1309.csv", "bmf.bm1310.csv", "bmf.bm1312.csv")
  files_14 <- c("bmf.bm1402.csv", "bmf.bm1404.csv", "bmf.bm1406.csv", "bmf.bm1409.csv", "bmf.bm1411.csv", "bmf.bm1412.csv")
  files_15 <- c("bmf.bm1502.csv", "bmf.bm1504.csv", "bmf.bm1505.csv", "bmf.bm1507.csv", "bmf.bm1509.csv", "bmf.bm1511.csv", "bmf.bm1512.csv")  
  files_16 <- c("bmf.bm1602.csv", "bmf.bm1603.csv", "bmf.bm1604.csv", "bmf.bm1608.csv")
  files_17 <- c("bmf.bm1709.csv", "bmf.bm1712.csv")
#--------------------------------------------------------
# 2008
#--------------------------------------------------------
  colnames(bmf_08_01)
  colnames(bmf_08_04)
  colnames(bmf_08_06)
  colnames(bmf_08_10)
  colnames(bmf_08_12)
  
  bmf08_01 <- bmf_08_01 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OutNCCS, YEAR) %>%
    rename(OUT = OutNCCS) %>%
    mutate(
      OUT = case_when(
        is.na(OUT) == TRUE ~ "IN",
        is.na(OUT) != TRUE ~ "OUT"
      )
    ) %>%
    filter(
      OUT == "IN"
    ) %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUT)
  
  bmf08_04 <- bmf_08_04 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS)
  
  bmf08_06 <- bmf_08_06 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OutNCCS, YEAR) %>%
    rename(OUT = OutNCCS) %>%
    mutate(
      OUT = case_when(
        is.na(OUT) == TRUE ~ "IN",
        is.na(OUT) != TRUE ~ "OUT"
      )
    ) %>%
    filter(
      OUT == "IN"
    ) %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUT)
  
  bmf08_10 <- bmf_08_10 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS)
  
  bmf08_12 <- bmf_08_12 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS)
  
  bmf_08 <- mget(ls(pattern="^bmf08*")) %>%
    bind_rows() %>%
    distinct()
#--------------------------------------------------------
# 2011
#--------------------------------------------------------
  
  for (i in 1:length(files_11)){
    bmf_year <- str_sub(files_11, 7, 8)
    bmf_month <- str_sub(files_11, 9, 10)
    
    # Figure out a way of knowing which variable name it has
    
    path <- paste0(file_master, files_11[i])
    
    bmf <- read_csv(path) %>%
      filter(SUBSECCD == "03") %>%
      mutate(YEAR = str_sub(TAXPER, 1, 4)) %>%
      select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
      filter(OUTNCCS == "IN") %>%
      drop_na(TAXPER, YEAR) %>%
      select(-OUTNCCS)
    
    df_name <- paste0("bmf_", bmf_year[i], "_", bmf_month[i])
    
    assign(df_name, bmf)
  }
  
  colnames(bmf_10_01)
  colnames(bmf_10_04)
  colnames(bmf_10_05)
  colnames(bmf_10_07)
  colnames(bmf_10_08)
  colnames(bmf_10_11)
  
  bmf10_01 <- bmf_10_01 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS)
  
  bmf10_04 <- bmf_10_04 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS)
  
  bmf10_05 <- bmf_10_05 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS)
  
  bmf10_07 <- bmf_10_07 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS)
  
  bmf10_08 <- bmf_10_08 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS)
  
  bmf10_11 <- bmf_10_11 %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS)
  
  bmf_10 <- mget(ls(pattern="^bmf10*")) %>%
    bind_rows() %>%
    distinct()
#--------------------------------------------------------
# 2011
#--------------------------------------------------------  
  
  for (i in 1:length(files_11)){
    bmf_year <- str_sub(files_11, 7, 8)
    bmf_month <- str_sub(files_11, 9, 10)
    
    path <- paste0(file_master, files_11[i])
    
    bmf <- read_csv(path) %>%
      filter(SUBSECCD == "03") %>%
      mutate(YEAR = str_sub(TAXPER, 1, 4)) %>%
      select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
      filter(OUTNCCS == "IN") %>%
      drop_na(TAXPER, YEAR) %>%
      select(-OUTNCCS)
    
    df_name <- paste0("bmf_", bmf_year[i], "_", bmf_month[i])
    
    assign(df_name, bmf)
  }
  
  bmf_11 <- mget(ls(pattern="^bmf_11_*")) %>%
    bind_rows() %>%
    distinct()
  
#--------------------------------------------------------
# 2012
#--------------------------------------------------------  
  
  for (i in 1:length(files_12)){
    bmf_year <- str_sub(files_12, 7, 8)
    bmf_month <- str_sub(files_12, 9, 10)
    
    path <- paste0(file_master, files_12[i])
    
    bmf <- read_csv(path) %>%
      filter(SUBSECCD == "03") %>%
      mutate(YEAR = str_sub(TAXPER, 1, 4)) %>%
      select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
      filter(OUTNCCS == "IN") %>%
      drop_na(TAXPER, YEAR) %>%
      select(-OUTNCCS)
    
    df_name <- paste0("bmf_", bmf_year[i], "_", bmf_month[i])
    
    assign(df_name, bmf)
  }
  
  bmf_12 <- mget(ls(pattern="^bmf_12_*")) %>%
    bind_rows() %>%
    distinct()
  
#--------------------------------------------------------
# 2013
#--------------------------------------------------------  
  
  for (i in 1:length(files_13)){
    bmf_year <- str_sub(files_13, 7, 8)
    bmf_month <- str_sub(files_13, 9, 10)
    
    path <- paste0(file_master, files_13[i])
    
    bmf <- read_csv(path) %>%
      filter(SUBSECCD == "03") %>%
      mutate(YEAR = str_sub(TAXPER, 1, 4)) %>%
      select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
      filter(OUTNCCS == "IN") %>%
      drop_na(TAXPER, YEAR) %>%
      select(-OUTNCCS)
    
    df_name <- paste0("bmf_", bmf_year[i], "_", bmf_month[i])
    
    assign(df_name, bmf)
  }
  
  bmf_13 <- mget(ls(pattern="^bmf_13_*")) %>%
    bind_rows() %>%
    distinct()

#--------------------------------------------------------
# 2014
#--------------------------------------------------------  
  
  for (i in 1:length(files_14)){
    bmf_year <- str_sub(files_14, 7, 8)
    bmf_month <- str_sub(files_14, 9, 10)
    
    path <- paste0(file_master, files_14[i])
    
    bmf <- read_csv(path) %>%
      filter(SUBSECCD == "03") %>%
      mutate(YEAR = str_sub(TAXPER, 1, 4)) %>%
      select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
      filter(OUTNCCS == "IN") %>%
      drop_na(TAXPER, YEAR) %>%
      select(-OUTNCCS)
    
    df_name <- paste0("bmf_", bmf_year[i], "_", bmf_month[i])
    
    assign(df_name, bmf)
  }
  
  bmf_14 <- mget(ls(pattern="^bmf_14_*")) %>%
    bind_rows() %>%
    distinct()

#--------------------------------------------------------
# 2015
#--------------------------------------------------------  
  
  for (i in 1:length(files_15)){
    bmf_year <- str_sub(files_15, 7, 8)
    bmf_month <- str_sub(files_15, 9, 10)
    
    path <- paste0(file_master, files_15[i])
    
    bmf <- read_csv(path) %>%
      filter(SUBSECCD == "03") %>%
      mutate(YEAR = str_sub(TAXPER, 1, 4)) %>%
      select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
      filter(OUTNCCS == "IN") %>%
      drop_na(TAXPER, YEAR) %>%
      select(-OUTNCCS)
    
    df_name <- paste0("bmf_", bmf_year[i], "_", bmf_month[i])
    
    assign(df_name, bmf)
  }
  
  bmf_15 <- mget(ls(pattern="^bmf_15_*")) %>%
    bind_rows() %>%
    distinct()

#--------------------------------------------------------
# 2016
#--------------------------------------------------------  
  
  for (i in 1:length(files_16)){
    bmf_year <- str_sub(files_16, 7, 8)
    bmf_month <- str_sub(files_16, 9, 10)
    
    path <- paste0(file_master, files_16[i])
    
    bmf <- read_csv(path) %>%
      filter(SUBSECCD == "03") %>%
      mutate(YEAR = str_sub(TAXPER, 1, 4)) %>%
      select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
      filter(OUTNCCS == "IN") %>%
      drop_na(TAXPER, YEAR) %>%
      select(-OUTNCCS)
    
    df_name <- paste0("bmf_", bmf_year[i], "_", bmf_month[i])
    
    assign(df_name, bmf)
  }
  
  bmf_16 <- mget(ls(pattern="^bmf_16_*")) %>%
    bind_rows() %>%
    distinct()
  
#--------------------------------------------------------
# 2017
#--------------------------------------------------------  
  
for (i in 1:length(files_17)){
  bmf_year <- str_sub(files_17, 7, 8)
  bmf_month <- str_sub(files_17, 9, 10)
  
  path <- paste0(file_master, files_17[i])
  
  bmf <- read_csv(path) %>%
    filter(SUBSECCD == "03") %>%
    mutate(YEAR = str_sub(TAXPER, 1, 4)) %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS)
  
  df_name <- paste0("bmf_", bmf_year[i], "_", bmf_month[i])
  
  assign(df_name, bmf)
}

  bmf_17 <- mget(ls(pattern="^bmf_17_*")) %>%
    bind_rows() %>%
    distinct()
  
#--------------------------------------------------------
# 2018
#--------------------------------------------------------

  bmf_18 <- read_csv("/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf.bm1812.csv") %>%
    filter(SUBSECCD == "03") %>%
    mutate(YEAR = str_sub(TAXPER, 1, 4)) %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS) %>%
    distinct()

#--------------------------------------------------------
# 2019
#--------------------------------------------------------
  
  bmf_19 <- read_csv("/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf.bm1908.csv") %>%
    filter(SUBSECCD == "03") %>%
    mutate(YEAR = str_sub(TAXPER, 1, 4)) %>%
    select(EIN, STATE, SUBSECCD, FNDNCD, TAXPER, OUTNCCS, YEAR) %>%
    filter(OUTNCCS == "IN") %>%
    drop_na(TAXPER, YEAR) %>%
    select(-OUTNCCS) %>%
    distinct()  

#--------------------------------------------------------
# Yearly BMF export
#--------------------------------------------------------  
  write_csv(bmf_19, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_19.csv")
  write_csv(bmf_18, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_18.csv")
  write_csv(bmf_17, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_17.csv")
  write_csv(bmf_16, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_16.csv")
  write_csv(bmf_15, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_15.csv")
  write_csv(bmf_14, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_14.csv")
  write_csv(bmf_13, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_13.csv")
  write_csv(bmf_12, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_12.csv")
  write_csv(bmf_11, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_11.csv")
  write_csv(bmf_10, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_10.csv")
  write_csv(bmf_09, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_09.csv")
  write_csv(bmf_08, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_08.csv")
#--------------------------------------------------------
# Binding all yearly master files
#--------------------------------------------------------
# You'll have to remove all elements that are not the
# yearly data frames whose name also begins with "bmf_"

# Making sure all the variable types are the same
bmf_08 <- bmf_08 %>%
    mutate(
      EIN = as.character(EIN),
      STATE = as.character(STATE),
      SUBSECCD = as.character(SUBSECCD),
      FNDNCD = as.character(FNDNCD),
      TAXPER  = as.character(TAXPER), 
      YEAR = as.character(YEAR)
    )

bmf_09 <- bmf_09 %>%
  mutate(
    EIN = as.character(EIN),
    STATE = as.character(STATE),
    SUBSECCD = as.character(SUBSECCD),
    FNDNCD = as.character(FNDNCD),
    TAXPER  = as.character(TAXPER), 
    YEAR = as.character(YEAR)
  )

bmf_10 <- bmf_10 %>%
  mutate(
    EIN = as.character(EIN),
    STATE = as.character(STATE),
    SUBSECCD = as.character(SUBSECCD),
    FNDNCD = as.character(FNDNCD),
    TAXPER  = as.character(TAXPER), 
    YEAR = as.character(YEAR)
  )

bmf_11 <- bmf_11 %>%
  mutate(
    EIN = as.character(EIN),
    STATE = as.character(STATE),
    SUBSECCD = as.character(SUBSECCD),
    FNDNCD = as.character(FNDNCD),
    TAXPER  = as.character(TAXPER), 
    YEAR = as.character(YEAR)
  )

bmf_12 <- bmf_12 %>%
  mutate(
    EIN = as.character(EIN),
    STATE = as.character(STATE),
    SUBSECCD = as.character(SUBSECCD),
    FNDNCD = as.character(FNDNCD),
    TAXPER  = as.character(TAXPER), 
    YEAR = as.character(YEAR)
  )

bmf_13 <- bmf_13 %>%
  mutate(
    EIN = as.character(EIN),
    STATE = as.character(STATE),
    SUBSECCD = as.character(SUBSECCD),
    FNDNCD = as.character(FNDNCD),
    TAXPER  = as.character(TAXPER), 
    YEAR = as.character(YEAR)
  )

bmf_14 <- bmf_14 %>%
  mutate(
    EIN = as.character(EIN),
    STATE = as.character(STATE),
    SUBSECCD = as.character(SUBSECCD),
    FNDNCD = as.character(FNDNCD),
    TAXPER  = as.character(TAXPER), 
    YEAR = as.character(YEAR)
  )

bmf_15 <- bmf_15 %>%
  mutate(
    EIN = as.character(EIN),
    STATE = as.character(STATE),
    SUBSECCD = as.character(SUBSECCD),
    FNDNCD = as.character(FNDNCD),
    TAXPER  = as.character(TAXPER), 
    YEAR = as.character(YEAR)
  )

bmf_16 <- bmf_16 %>%
  mutate(
    EIN = as.character(EIN),
    STATE = as.character(STATE),
    SUBSECCD = as.character(SUBSECCD),
    FNDNCD = as.character(FNDNCD),
    TAXPER  = as.character(TAXPER), 
    YEAR = as.character(YEAR)
  )

bmf_17 <- bmf_17 %>%
  mutate(
    EIN = as.character(EIN),
    STATE = as.character(STATE),
    SUBSECCD = as.character(SUBSECCD),
    FNDNCD = as.character(FNDNCD),
    TAXPER  = as.character(TAXPER), 
    YEAR = as.character(YEAR)
  )

bmf_18 <- bmf_18 %>%
  mutate(
    EIN = as.character(EIN),
    STATE = as.character(STATE),
    SUBSECCD = as.character(SUBSECCD),
    FNDNCD = as.character(FNDNCD),
    TAXPER  = as.character(TAXPER), 
    YEAR = as.character(YEAR)
  )

# Binding all yearly data frames
bmf_all <- mget(ls(pattern="^bmf_*")) %>%
  bind_rows()

filter_years <- as.character(c(2008, 2009, 2010, 2011, 2012, 2013,
                  2014, 2015, 2016, 2017, 2018))
state_short = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

bmf_clean <- bmf_all %>%
  group_by(STATE, YEAR) %>%
  distinct() %>%
  mutate(
    church = case_when(
      FNDNCD == "10" ~ 1,
      FNDNCD != "10" ~ 0
    ),
    excempt_org = case_when(
      FNDNCD != "10" ~ 1,
      FNDNCD == "10" ~ 0
    )
  ) %>%
  filter(
    YEAR %in% filter_years,
    STATE %in% state_short
  )

excempt_orgs <- bmf_clean %>%
  group_by(YEAR, STATE) %>%
  summarise(
    EXCEMPT_ORGS = sum(excempt_org),
    CHURCHES = sum(church)
  )
#--------------------------------------------------------
#--------------------------------------------------------
# Data export
#--------------------------------------------------------
# Tax-excempt orgs and churches by state, per year
write_csv(excempt_orgs,
          "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf_excempt_orgs_state_years.csv")

# BMF clean, 2008-2018
write_csv(bmf_clean,
          "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf_2008_2018.csv")
