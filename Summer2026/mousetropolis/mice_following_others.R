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

all_transitions <- lapply(all_files, process_mouse_csv) %>% bind_rows()
all_true <- all_transitions %>% filter(true_transition == TRUE)

# could be useful???
saveRDS(all_true, "Summer2026/mousetropolis/data/all_true.RDS")
#all_true <- readRDS("Summer2026/mousetropolis/data/all_true.RDS")

# mice that follow within 250 milliseconds
find_following <- function(df, window_ms = 250) {
  true_trans <- df[df$true_transition == TRUE & !is.na(df$true_transition), ]
  
  results <- list()
  
  for (i in 1:nrow(true_trans)) {
    mid   <- true_trans$mouse_id[i]
    bfrom <- true_trans$box_from[i]
    bto   <- true_trans$box_to[i]
    ts    <- true_trans$cantimestamp[i]
    
    matches <- true_trans[
      true_trans$mouse_id != mid &
        true_trans$box_from == bfrom &
        true_trans$box_to == bto &
        abs(true_trans$cantimestamp - ts) <= window_ms, ]
    
    if (nrow(matches) > 0) {
      for (j in 1:nrow(matches)) {
        results[[length(results) + 1]] <- data.frame(
          mouse_a = mid,
          mouse_b = matches$mouse_id[j],
          box_from = bfrom,
          box_to = bto,
          time_a = ts,
          time_b = matches$cantimestamp[j],
          time_diff_ms = abs(ts - matches$cantimestamp[j])
        )
      }
    }
  }
  
  do.call(rbind, results)
}

# find all following pairs
following_pairs <- find_following(all_true)

following_pairs_unique <- following_pairs %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(mouse_a, mouse_b)), collapse = "_")) %>%
  ungroup() %>%
  distinct(pair, box_from, box_to, .keep_all = TRUE) %>%
  select(-pair)

# save unique pairs as document
saveRDS(following_pairs_unique, "Summer2026/mousetropolis/data/following_pairs_unique.RDS")
