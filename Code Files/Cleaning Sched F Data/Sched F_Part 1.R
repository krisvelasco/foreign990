# -------------------------  
# Sched F (from Jacob)
options(scipen=999) # Telling R to avoid the scientific notation altogether
#-------------------------
# Part 1
dirty_f_1 <- read_csv("/Volumes/SRC_DATA/000_f990_data/sched_f_i.csv")

f_1_cln <- dirty_f_1 %>%  
  select(-id) %>%
  distinct() %>%
  mutate(
    id_ein = paste0(object_id, ein)
  ) %>%
  mutate_at(c('TtlSpntAmt'), ~replace_na(.,0)) %>% # replacing NAs with zeros (0)
  select(
    id_ein, object_id, ein, TtlSpntAmt
  ) 
#length(unique(f_1_cln$id))/nrow(f_1_cln)
#length(unique(f_1_cln$object_id))/nrow(f_1_cln)
#length(unique(f_1_cln$id_ein))/nrow(f_1_cln)

# Code to check for duplicates
#   name_df <- og_df %>%
#     group_by(object_id, ein) %>%
#     filter(n()>1) %>%
#     ungroup() %>%
#     arrange(ein)