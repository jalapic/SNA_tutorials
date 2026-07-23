library(tidyverse)
library(igraph)

get_class_seq_bot <- function(cohort,
                          graph_file = "rfid_data/nodes6_g.RDS",
                          start_row = 25,
                          time_col = "value1") {
  
  d <- readRDS(graph_file)
  
  cohort <- cohort %>%
    arrange(.data[[time_col]]) %>%
    mutate(
      time   = .data[[time_col]],
      winner = as.character(.[[4]]),
      loser  = as.character(.[[5]]),
      dyad_1 = pmin(winner, loser),
      dyad_2 = pmax(winner, loser),
      dyad   = paste(dyad_1, dyad_2, sep = "__")
    )
  
  all_mice <- sort(unique(c(cohort$winner, cohort$loser)))
  vertex_df <- data.frame(name = all_mice, stringsAsFactors = FALSE)
  
  canonical_key <- function(g) {
    cp <- canonical_permutation(g)
    g_can <- permute(g, cp$labeling)
    mat <- as.matrix(as_adjacency_matrix(g_can, sparse = FALSE, names = FALSE))
    paste(mat, collapse = "")
  }
  
  ref_keys <- vapply(d, canonical_key, character(1))
  
  resolve_pair <- function(df_pair) {
    n <- nrow(df_pair)
    
    if (n <= 2) {
      out <- df_pair[n, c("winner", "loser"), drop = FALSE]
    } else {
      last3 <- df_pair[(n - 2):n, , drop = FALSE]
      tab <- table(last3$winner)
      majority_winner <- names(tab)[which.max(tab)]
      majority_loser <- setdiff(unique(c(last3$winner, last3$loser)), majority_winner)[1]
      out <- tibble(winner = majority_winner, loser = majority_loser)
    }
    
    out
  }
  
  classes <- rep(NA_integer_, nrow(cohort))
  
  for (i in seq.int(start_row, nrow(cohort))) {
    current_dat <- cohort[1:i, , drop = FALSE]
    
    edge_df <- current_dat %>%
      split(.$dyad) %>%
      lapply(resolve_pair) %>%
      bind_rows()
    
    g <- graph_from_data_frame(
      d = edge_df,
      directed = TRUE,
      vertices = vertex_df
    )
    
    key <- canonical_key(g)
    hit <- match(key, ref_keys)
    classes[i] <- ifelse(is.na(hit), NA_integer_, hit)
  }
  
  tibble(
    isomorph_class = classes
  ) %>%
    filter(!is.na(isomorph_class))
}