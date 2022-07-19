## Project: Nonprofit Foreign Expenditures

## Overview: 


## Last updated: July 19th by Sebastian Rojas Cabal
#--------------------------------------------------------
#--------------------------------------------------------
# Loading packages
#--------------------------------------------------------
library(tidyverse)
#--------------------------------------------------------
#--------------------------------------------------------
# Importing Business Master Files from Urban Institute
#     https://nccs-data.urban.org/data.php?ds=bmf
# UI codebook. For selection and cleaning.
#     https://nccs-data.urban.org/dd2.php?close=1&form=BMF+08/2016
#--------------------------------------------------------

file_master <- c("/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/")
file_names <- list.files(file_master)
  files_08 <- c("bmf.bm0801.csv", "bmf.bm0804.csv", "bmf.bm0806.csv", "bmf.bm0810.csv", "bmf.bm0812.csv")
  files_09 <- c("bmf.bm0901.csv", "bmf.bm0904.csv", "bmf.bm0907.csv", "bmf.bm0910.csv")
  files_10 <- c("bmf.bm1001.csv", "bmf.bm1004.csv", "bmf.bm1005.csv", "bmf.bm1007.csv", "bmf.bm1008.csv", "bmf.bm1011.csv")
  files_11 <- c("bmf.bm1106.csv", "bmf.bm1107.csv", "bmf.bm1108.csv", "bmf.bm1109.csv", "bmf.bm1110.csv", "bmf.bm1111.csv", "bmf.bm1112.csv")
    
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

  write_csv(bmf_11, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_11.csv")
  write_csv(bmf_10, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_10.csv")
  write_csv(bmf_09, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_09.csv")
  write_csv(bmf_08, "/Volumes/SRC_FILES/0000_F990 Project/000_f990_data/bmf/bmf_08.csv")
#------
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

#-----
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

#-----

colnames(bmf_11_06)
colnames(bmf_11_07)
colnames(bmf_11_08)
colnames(bmf_11_09)
colnames(bmf_11_10)
colnames(bmf_11_11)
colnames(bmf_11_12)

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

