## Project: Nonprofit Foreign Expenditures
## Date: March 29, 2022
## Overview: 
## Implementing word embeddings.
## Source: https://smltar.com/embeddings.html

library(tidyverse)
library(tidytext)
library(SnowballC)

part1 <- read_csv("/Volumes/Google Drive/My Drive/F990/Data from OneDrive/part_i.csv")

# Only the mision statements and the EINs
miss <- select(part1,
               ein,
               ActvtyOrMssnDsc) %>%
  rename(mission = ActvtyOrMssnDsc)

tidy_missions <- miss %>%
  unnest_tokens(word, mission) %>%
  add_count(word) %>%
  filter(n >= 50) %>%
  select(-n)

nested_words <- tidy_missions %>%
  nest(words = c(word))

a <- slice(tidy_missions, 1:10)


slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(
    tbl, 
    ~.x, 
    .after = window_size - 1, 
    .step = 1, 
    .complete = TRUE
  )
  
  safe_mutate <- safely(mutate)
  
  out <- map2(skipgrams,
              1:length(skipgrams),
              ~ safe_mutate(.x, window_id = .y))
  
  out %>%
    transpose() %>%
    pluck("result") %>%
    compact() %>%
    bind_rows()
}


# This is the computationally intensive part.

library(slider)
library(widyr)
library(furrr)

plan(multisession)  ## for parallel processing

tidy_pmi <- nested_words %>%
  mutate(words = future_map(words, slide_windows, 4L)) %>%
  unnest(words) %>%
  unite(window_id, ein, window_id) %>%
  pairwise_pmi(word, window_id)

tidy_pmi