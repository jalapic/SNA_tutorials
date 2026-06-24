# mice following other mice

# import libraries
library(dplyr)
library(readr)
library(igraph)

# source the transition identifier
source("Summer2026/mousetropolis/ai_transition_identifier.R")

# read and combine all days
all_files <- c(
  "Summer2026/mousetropolis/data/rawdata20260418.csv",
  "Summer2026/mousetropolis/data/rawdata20260419.csv",
  "Summer2026/mousetropolis/data/rawdata20260420.csv",
  "Summer2026/mousetropolis/data/rawdata20260421.csv",
  "Summer2026/mousetropolis/data/rawdata20260422.csv",
  "Summer2026/mousetropolis/data/rawdata20260423.csv",
  "Summer2026/mousetropolis/data/rawdata20260424.csv",
  "Summer2026/mousetropolis/data/rawdata20260425.csv",
  "Summer2026/mousetropolis/data/rawdata20260426.csv",
  "Summer2026/mousetropolis/data/rawdata20260427.csv",
  "Summer2026/mousetropolis/data/rawdata20260428.csv",
  "Summer2026/mousetropolis/data/rawdata20260429.csv",
  "Summer2026/mousetropolis/data/rawdata20260430.csv",
  "Summer2026/mousetropolis/data/rawdata20260501.csv",
  "Summer2026/mousetropolis/data/rawdata20260502.csv"
)

all_true_transitions <- lapply(all_files, check_mouse_transitions) %>% bind_rows()


# could be useful???
saveRDS(all_true_transitions, "Summer2026/mousetropolis/data/all_true_trans.RDS")
att <- readRDS("Summer2026/mousetropolis/data/all_true_trans.RDS")

# mice that follow within 250 milliseconds
find_following <- function(df, window_ms = 250) {
  true_trans <- df
  
  results <- list()
  
  for (i in 1:nrow(true_trans)) {
    mid   <- true_trans$mouse_id[i]
    bfrom <- true_trans$from_box[i]
    bto   <- true_trans$to_box[i]
    ts    <- true_trans$start_cantimestamp[i]
    
    matches <- true_trans[
      true_trans$mouse_id != mid &
        true_trans$from_box == bfrom &
        true_trans$to_box == bto &
        abs(true_trans$start_cantimestamp - ts) <= window_ms, ]
    
    if (nrow(matches) > 0) {
      for (j in 1:nrow(matches)) {
        results[[length(results) + 1]] <- data.frame(
          mouse_a = mid,
          mouse_b = matches$mouse_id[j],
          from_box = bfrom,
          to_box = bto,
          time_a = ts,
          time_b = matches$start_cantimestamp[j],
          time_diff_ms = abs(ts - matches$start_cantimestamp[j])
        )
      }
    }
  }
  
  do.call(rbind, results)
}

# find all following pairs
following_pairs <- find_following(att)

following_pairs_unique <- following_pairs %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(mouse_a, mouse_b)), collapse = "_")) %>%
  ungroup() %>%
  distinct(pair, from_box, to_box, .keep_all = TRUE) %>%
  select(-pair)

# save unique pairs as document
saveRDS(following_pairs_unique, "Summer2026/mousetropolis/data/following_pairs_unique.RDS")
