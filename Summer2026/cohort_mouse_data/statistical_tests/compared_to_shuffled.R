library(igraph)
library(dplyr)
library(reshape2)
library(ggplot2)
library(glue)

set.seed(1)

# ============================================================
# 1. Canonical lookup for unlabeled tournaments (n = 6)
# ============================================================

canon_key <- function(g) {
  p <- igraph::canonical_permutation(g)$labeling
  gc <- igraph::permute(g, p)
  M <- igraph::as_adjacency_matrix(gc, sparse = FALSE)
  paste(as.integer(M), collapse = "")
}

make_tournament <- function(n, bits) {
  A <- matrix(0L, n, n)
  idx <- 0L
  for (i in 1:(n - 1L)) {
    for (j in (i + 1L):n) {
      b <- bitwAnd(bitwShiftR(bits, idx), 1L)
      if (b == 1L) A[i, j] <- 1L else A[j, i] <- 1L
      idx <- idx + 1L
    }
  }
  igraph::graph_from_adjacency_matrix(A, mode = "directed")
}

build_canon_lookup <- function(n) {
  m <- n * (n - 1L) / 2L
  n_states_labeled <- 2^m
  
  key_to_id <- new.env(hash = TRUE, parent = emptyenv())
  class_id <- integer(n_states_labeled)
  reps <- vector("list", 0L)
  
  for (bits in 0:(n_states_labeled - 1L)) {
    g <- make_tournament(n, bits)
    key <- canon_key(g)
    
    if (!exists(key, envir = key_to_id, inherits = FALSE)) {
      new_id <- length(reps) + 1L
      assign(key, new_id, envir = key_to_id)
      reps[[new_id]] <- g
    }
    class_id[bits + 1L] <- get(key, envir = key_to_id, inherits = FALSE)
  }
  
  list(
    reps = reps,
    class_id = class_id,
    m = m,
    n_unlabeled = length(reps),
    n_labeled = n_states_labeled
  )
}

lookup6 <- build_canon_lookup(6)
n_states <- lookup6$n_unlabeled

# ============================================================
# 2. Bit index for dyads
# ============================================================

make_bit_index <- function(vnames) {
  n <- length(vnames)
  idx_mat <- matrix(NA_integer_, n, n, dimnames = list(vnames, vnames))
  idx <- 0L
  for (i in 1:(n - 1L)) {
    for (j in (i + 1L):n) {
      idx_mat[i, j] <- idx
      idx_mat[j, i] <- idx
      idx <- idx + 1L
    }
  }
  idx_mat
}

# ============================================================
# 3. Convert interaction sequence to path of tournament states
# ============================================================

interaction_sequence_to_path <- function(edgelist, vnames, lookup) {
  n <- length(vnames)
  stopifnot(n == 6)
  
  idx_mat <- make_bit_index(vnames)
  state <- 0L
  path <- integer(nrow(edgelist) + 1L)
  path[1] <- lookup$class_id[state + 1L]
  
  for (t in seq_len(nrow(edgelist))) {
    winner <- as.character(edgelist[t, 1])
    loser  <- as.character(edgelist[t, 2])
    
    i <- match(winner, vnames)
    j <- match(loser, vnames)
    
    bit <- idx_mat[i, j]
    a <- min(i, j)
    b <- max(i, j)
    
    # bit = 1 means a -> b, bit = 0 means b -> a
    if (i == a && j == b) {
      state <- bitwOr(state, bitwShiftL(1L, bit))
    } else {
      state <- bitwAnd(state, bitwNot(bitwShiftL(1L, bit)))
    }
    
    path[t + 1L] <- lookup$class_id[state + 1L]
  }
  
  path
}

# ============================================================
# 4. Transition matrix utilities
# ============================================================

create_transition_matrix_fast <- function(sequence, num_states) {
  from <- sequence[-length(sequence)]
  to   <- sequence[-1L]
  idx  <- (from - 1L) * num_states + to
  counts <- tabulate(idx, nbins = num_states * num_states)
  matrix(counts, nrow = num_states, ncol = num_states, byrow = TRUE)
}

row_normalize <- function(TM) {
  rs <- rowSums(TM)
  P <- TM / rs
  P[!is.finite(P)] <- 0
  P
}

# ============================================================
# 5. Null randomization: shuffle order + flip directions
# ============================================================

randomize_edgelist_dyad_preserving <- function(edgelist) {
  pairs <- edgelist[, 1:2]
  
  # unordered dyads
  dyads <- t(apply(pairs, 1, function(x) sort(as.character(x))))
  dyads <- as.data.frame(dyads, stringsAsFactors = FALSE)
  colnames(dyads) <- c("id1", "id2")
  
  # randomize direction within each dyad
  flip <- sample(c(FALSE, TRUE), size = nrow(dyads), replace = TRUE)
  rand_edges <- dyads
  rand_edges[flip, c("id1", "id2")] <- rand_edges[flip, c("id2", "id1")]
  
  # shuffle event order
  rand_edges <- rand_edges[sample.int(nrow(rand_edges)), , drop = FALSE]
  rownames(rand_edges) <- NULL
  
  rand_edges
}

# ============================================================
# 6. Simulate null TM distribution from one cohort's edgelist
# ============================================================

simulate_tm_null_from_edgelist <- function(edgelist, lookup, n_sim = 1000) {
  vnames <- sort(unique(c(as.character(edgelist[[1]]), as.character(edgelist[[2]]))))
  
  tm_prob_list <- vector("list", n_sim)
  
  for (b in seq_len(n_sim)) {
    rand_edgelist <- randomize_edgelist_dyad_preserving(edgelist)
    path <- interaction_sequence_to_path(rand_edgelist, vnames, lookup)
    TM <- create_transition_matrix_fast(path, lookup$n_unlabeled)
    tm_prob_list[[b]] <- row_normalize(TM)
  }
  
  tm_prob_list
}

# ============================================================
# 7. Cellwise p-values
# ============================================================

cellwise_pvals <- function(obs_TM, tm_prob_list, adjust = FALSE, p_adjust_method = "BH") {
  n_states <- nrow(obs_TM)
  n_sim <- length(tm_prob_list)
  
  sim_array <- array(NA_real_, dim = c(n_states, n_states, n_sim))
  for (b in seq_len(n_sim)) sim_array[, , b] <- tm_prob_list[[b]]
  
  p_mat_right <- matrix(NA_real_, n_states, n_states)
  p_mat_left  <- matrix(NA_real_, n_states, n_states)
  p_mat_two   <- matrix(NA_real_, n_states, n_states)
  
  for (i in seq_len(n_states)) {
    for (j in seq_len(n_states)) {
      null_vals <- sim_array[i, j, ]
      obs_val <- obs_TM[i, j]
      
      p_mat_right[i, j] <- (sum(null_vals >= obs_val) + 1) / (n_sim + 1)
      p_mat_left[i, j]  <- (sum(null_vals <= obs_val) + 1) / (n_sim + 1)
      
      center <- median(null_vals)
      p_mat_two[i, j] <- (sum(abs(null_vals - center) >= abs(obs_val - center)) + 1) / (n_sim + 1)
    }
  }
  
  if (adjust) {
    p_mat_right <- matrix(p.adjust(c(p_mat_right), method = p_adjust_method), n_states, n_states)
    p_mat_left  <- matrix(p.adjust(c(p_mat_left),  method = p_adjust_method), n_states, n_states)
    p_mat_two   <- matrix(p.adjust(c(p_mat_two),   method = p_adjust_method), n_states, n_states)
  }
  
  list(right = p_mat_right, left = p_mat_left, two_sided = p_mat_two)
}

# ============================================================
# 8. Example: one cohort
# ============================================================

cohort <- readr::read_csv("Summer2026/cohort_mouse_data/cohort1.csv") %>%
   dplyr::arrange(value1)

obs_edgelist <- cohort[, 4:5]
colnames(obs_edgelist) <- c("from", "to")

vnames <- sort(unique(c(as.character(obs_edgelist$from), as.character(obs_edgelist$to))))

# observed TM
obs_path <- interaction_sequence_to_path(obs_edgelist, vnames, lookup6)
obs_TM_counts <- create_transition_matrix_fast(obs_path, n_states)
obs_TM_prob <- row_normalize(obs_TM_counts)

# null distribution
tm_prob_list <- simulate_tm_null_from_edgelist(obs_edgelist, lookup6, n_sim = 1000)

# p-values
pvals <- cellwise_pvals(obs_TM = obs_TM_prob, tm_prob_list = tm_prob_list)

# heatmap
pval_df <- reshape2::melt(pvals$two_sided)
colnames(pval_df) <- c("Current_State", "Next_State", "P_Value")

ggplot(pval_df, aes(x = Current_State, y = Next_State, fill = P_Value < 0.05)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c(`TRUE` = "black", `FALSE` = "white")) +
  labs(
    title = "Cellwise P-Values (Dyad-Preserving Randomization)",
    x = "Current State",
    y = "Next State",
    fill = "P-Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_reverse(breaks = 1:n_states) +
  scale_x_continuous(breaks = 1:n_states)

# ============================================================
# 9. Loop over all cohorts (if you have full_TMs and cohorts)
# ============================================================

# Assuming:
#   - full_TMs is a list of observed TMs (one per cohort)
#   - cohort_list is a list of edgelists (one per cohort)

# cohort_pvals <- vector("list", length(cohort_list))

# for (i in seq_along(cohort_list)) {
#   edgelist <- cohort_list[[i]][, 4:5]
#   colnames(edgelist) <- c("from", "to")
# 
#   vnames <- sort(unique(c(as.character(edgelist$from), as.character(edgelist$to))))
# 
#   obs_path <- interaction_sequence_to_path(edgelist, vnames, lookup6)
#   obs_TM_counts <- create_transition_matrix_fast(obs_path, n_states)
#   obs_TM_prob <- row_normalize(obs_TM_counts)
# 
#   tm_prob_list <- simulate_tm_null_from_edgelist(edgelist, lookup6, n_sim = 1000)
# 
#   cohort_pvals[[i]] <- cellwise_pvals(obs_TM = obs_TM_prob, tm_prob_list = tm_prob_list)
# }

# Heatmaps for each cohort:
# for (i in seq_along(cohort_pvals)) {
#   pval_df <- reshape2::melt(cohort_pvals[[i]]$two_sided)
#   colnames(pval_df) <- c("Current_State", "Next_State", "P_Value")
# 
#   p <- ggplot(pval_df, aes(x = Current_State, y = Next_State, fill = P_Value < 0.05)) +
#     geom_tile(color = "black") +
#     scale_fill_manual(values = c(`TRUE` = "black", `FALSE` = "white")) +
#     labs(
#       title = glue("Cohort {i} Cellwise P-Values"),
#       x = "Current State",
#       y = "Next State",
#       fill = "P-Value"
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 90, hjust = 1),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank()
#     ) +
#     scale_y_reverse(breaks = 1:n_states) +
#     scale_x_continuous(breaks = 1:n_states)
# 
#   print(p)
# }