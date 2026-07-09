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

all_true <- lapply(all_files, check_mouse_transitions) %>% bind_rows()
saveRDS(all_true, "Summer2026/mousetropolis/data/all_true_crossings.rds")

# mice that follow within 250 milliseconds
find_following <- function(df, window_ms = 250) {
  true_trans <- df
  true_trans$ts_ms <- as.numeric(as.POSIXct(true_trans$start_datetimestamp, format = "%d.%m.%Y %H:%M:%OS")) * 1000

  results <- list()

  for (i in 1:nrow(true_trans)) {
    mid   <- true_trans$mouse_id[i]
    bfrom <- true_trans$from_box[i]
    bto   <- true_trans$to_box[i]
    ts    <- true_trans$ts_ms[i]

    # only find mice that passed AFTER this mouse (followers)
    matches <- true_trans[
      true_trans$mouse_id != mid &
      true_trans$from_box == bfrom &
      true_trans$to_box == bto &
      true_trans$ts_ms > ts &
      true_trans$ts_ms - ts <= window_ms, ]

    if (nrow(matches) > 0) {
      for (j in 1:nrow(matches)) {
        results[[length(results) + 1]] <- data.frame(
          leader = mid,
          follower = matches$mouse_id[j],
          from_box = bfrom,
          to_box = bto,
          time_leader = ts,
          time_follower = matches$ts_ms[j],
          time_diff_ms = matches$ts_ms[j] - ts
        )
      }
    }
  }

  do.call(rbind, results)
}

# find all following pairs
following_pairs <- find_following(all_true)

# save following pairs
saveRDS(following_pairs, "Summer2026/mousetropolis/data/following_pairs.rds")

d <- load("Summer2026/mousetropolis/data/following_pairs.RDS")
d
