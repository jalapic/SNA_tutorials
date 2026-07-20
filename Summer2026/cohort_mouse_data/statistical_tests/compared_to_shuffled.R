library(igraph)
library(dplyr)
library(reshape2)
library(ggplot2)
library(glue)
library(future.apply)

set.seed(1)

# parallel backend for null simulations
future::plan(future::multisession)

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

make_name_index <- function(vnames) {
  x <- seq_along(vnames)
  names(x) <- vnames
  x
}

# ============================================================
# 3. Convert interaction sequence to path of tournament states
#    optimized: no repeated match() / coercion inside loop
# ============================================================

interaction_sequence_to_path <- function(edgelist, vnames, lookup) {
  n <- length(vnames)
  stopifnot(n == 6)
  
  idx_mat <- make_bit_index(vnames)
  name_idx <- make_name_index(vnames)
  
  winners <- as.character(edgelist[[1]])
  losers  <- as.character(edgelist[[2]])
  
  i_vec <- unname(name_idx[winners])
  j_vec <- unname(name_idx[losers])
  
  state <- 0L
  path <- integer(length(i_vec) + 1L)
  class_id <- lookup$class_id
  path[1L] <- class_id[state + 1L]
  
  for (t in seq_along(i_vec)) {
    i <- i_vec[t]
    j <- j_vec[t]
    
    bit <- idx_mat[i, j]
    
    if (i < j) {
      state <- bitwOr(state, bitwShiftL(1L, bit))
    } else {
      state <- bitwAnd(state, bitwNot(bitwShiftL(1L, bit)))
    }
    
    path[t + 1L] <- class_id[state + 1L]
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
#    optimized: no apply()/data.frame conversion
# ============================================================

randomize_edgelist_dyad_preserving <- function(edgelist) {
  from <- as.character(edgelist[[1]])
  to   <- as.character(edgelist[[2]])
  
  id1 <- ifelse(from < to, from, to)
  id2 <- ifelse(from < to, to, from)
  
  flip <- sample(c(FALSE, TRUE), size = length(from), replace = TRUE)
  
  rand_from <- ifelse(flip, id2, id1)
  rand_to   <- ifelse(flip, id1, id2)
  
  ord <- sample.int(length(from))
  
  out <- data.frame(
    from = rand_from[ord],
    to   = rand_to[ord],
    stringsAsFactors = FALSE
  )
  
  out
}

# ============================================================
# 6. Simulate null TM distribution from one cohort's edgelist
#    optimized: parallel simulations
# ============================================================

simulate_tm_null_from_edgelist <- function(edgelist, lookup, n_sim = 1000) {
  vnames <- sort(unique(c(as.character(edgelist[[1]]), as.character(edgelist[[2]]))))
  
  tm_prob_list <- future.apply::future_lapply(
    seq_len(n_sim),
    future.seed = TRUE,
    FUN = function(b) {
      rand_edgelist <- randomize_edgelist_dyad_preserving(edgelist)
      path <- interaction_sequence_to_path(rand_edgelist, vnames, lookup)
      TM <- create_transition_matrix_fast(path, lookup$n_unlabeled)
      row_normalize(TM)
    }
  )
  
  tm_prob_list
}

# ============================================================
# 7. Cellwise p-values
#    optimized: vectorized over matrix cells
# ============================================================

cellwise_pvals <- function(obs_TM, tm_prob_list, adjust = FALSE, p_adjust_method = "BH") {
  n_states <- nrow(obs_TM)
  n_sim <- length(tm_prob_list)
  
  sim_array <- array(NA_real_, dim = c(n_states, n_states, n_sim))
  for (b in seq_len(n_sim)) sim_array[, , b] <- tm_prob_list[[b]]
  
  sim_mat <- matrix(sim_array, nrow = n_states * n_states, ncol = n_sim)
  obs_vec <- c(obs_TM)
  
  p_right <- (rowSums(sim_mat >= obs_vec) + 1) / (n_sim + 1)
  p_left  <- (rowSums(sim_mat <= obs_vec) + 1) / (n_sim + 1)
  
  centers <- apply(sim_mat, 1L, median)
  p_two <- (rowSums(abs(sim_mat - centers) >= abs(obs_vec - centers)) + 1) / (n_sim + 1)
  
  p_mat_right <- matrix(p_right, n_states, n_states)
  p_mat_left  <- matrix(p_left,  n_states, n_states)
  p_mat_two   <- matrix(p_two,   n_states, n_states)
  
  if (adjust) {
    p_mat_right <- matrix(p.adjust(c(p_mat_right), method = p_adjust_method), n_states, n_states)
    p_mat_left  <- matrix(p.adjust(c(p_mat_left),  method = p_adjust_method), n_states, n_states)
    p_mat_two   <- matrix(p.adjust(c(p_mat_two),   method = p_adjust_method), n_states, n_states)
  }
  
  list(right = p_mat_right, left = p_mat_left, two_sided = p_mat_two)
}

# ============================================================
# 8. Loop over all cohorts
# ============================================================

cohort_pvals <- vector("list", length(c_list))

for (i in seq_along(c_list)) {
  c_list[[i]] <- c_list[[i]] %>%
    dplyr::arrange(value1)
  
  edgelist <- c_list[[i]][, 4:5]
  colnames(edgelist) <- c("from", "to")
  
  vnames <- sort(unique(c(as.character(edgelist$from), as.character(edgelist$to))))
  
  obs_path <- interaction_sequence_to_path(edgelist, vnames, lookup6)
  obs_TM_counts <- create_transition_matrix_fast(obs_path, n_states)
  obs_TM_prob <- row_normalize(obs_TM_counts)
  
  tm_prob_list <- simulate_tm_null_from_edgelist(edgelist, lookup6, n_sim = 1000)
  
  cohort_pvals[[i]] <- cellwise_pvals(obs_TM = obs_TM_prob, tm_prob_list = tm_prob_list)
}

# ============================================================
# 9. Heatmaps for each cohort
#    kept in your original structure
# ============================================================

for (i in 1:5) {
  pval_df <- reshape2::melt(cohort_pvals[[i]]$two_sided)
  colnames(pval_df) <- c("Current_State", "Next_State", "P_Value")
  
  p <- ggplot(pval_df, aes(x = Current_State, y = Next_State, fill = P_Value)) +
    geom_tile(color = "black") +
    scale_fill_gradientn(
      colours = c("black", "white"),
      values  = c(0, 1),
      limits  = c(0, 0.05),
      name    = "P-Value",
      oob     = scales::squish
    ) +
    labs(
      title = glue("Shuffled Cohort {i} Cellwise P-Values"),
      x = "Current State",
      y = "Next State",
      fill = "P-Value"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    ) +
    scale_y_reverse(breaks = 1:56) +
    scale_x_continuous(breaks = 1:56)
  
  print(p)
}

for (i in 6:9) {
  pval_df <- reshape2::melt(cohort_pvals[[i]]$two_sided)
  colnames(pval_df) <- c("Current_State", "Next_State", "P_Value")
  
  p <- ggplot(pval_df, aes(x = Current_State, y = Next_State, fill = P_Value)) +
    geom_tile(color = "black") +
    scale_fill_gradientn(
      colours = c("black", "white"),
      values  = c(0, 1),
      limits  = c(0, 0.05),
      name    = "P-Value",
      oob     = scales::squish
    ) +
    labs(
      title = glue("Shuffled Cohort {i+1} Cellwise P-Values"),
      x = "Current State",
      y = "Next State",
      fill = "P-Value"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    ) +
    scale_y_reverse(breaks = 1:56) +
    scale_x_continuous(breaks = 1:56)
  
  print(p)
}