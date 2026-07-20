library(igraph)
library(ggplot2)
library(reshape2)
library(glue)

set.seed(1)

# ------------------------------------------------------------
# One-time setup: unlabeled tournament representatives
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# Fast simulation using bitmasks
# ------------------------------------------------------------

simulate_tournament_transitions_fast <- function(N = 10000, lookup) {
  state <- sample.int(lookup$n_labeled, 1L) - 1L
  path <- integer(N + 1L)
  path[1] <- lookup$class_id[state + 1L]
  
  for (t in 1:N) {
    flip <- sample.int(lookup$m, 1L) - 1L
    state <- bitwXor(state, bitwShiftL(1L, flip))
    path[t + 1L] <- lookup$class_id[state + 1L]
  }
  path
}

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

# ------------------------------------------------------------
# Build for n = 6 & repeat many simulations efficiently
# ------------------------------------------------------------

lookup6 <- build_canon_lookup(6)
n_states <- lookup6$n_unlabeled

Nperms <- 10000
n_sim <- 1000

tm_prob_list <- vector("list", n_sim)

for (b in 1:n_sim) {
  path <- simulate_tournament_transitions_fast(N = Nperms, lookup = lookup6)
  TM <- create_transition_matrix_fast(path, n_states)
  tm_prob_list[[b]] <- row_normalize(TM)
}

# ------------------------------------------------------------
# Cellwise p-values
# ------------------------------------------------------------

cellwise_pvals <- function(obs_TM, tm_prob_list, adjust = FALSE, p_adjust_method = "BH") {
  n_states <- nrow(obs_TM)
  n_sim <- length(tm_prob_list)
  
  sim_array <- array(NA_real_, dim = c(n_states, n_states, n_sim))
  for (b in 1:n_sim) sim_array[, , b] <- tm_prob_list[[b]]
  
  p_mat_right <- matrix(NA_real_, n_states, n_states)
  p_mat_left  <- matrix(NA_real_, n_states, n_states)
  p_mat_two   <- matrix(NA_real_, n_states, n_states)
  
  for (i in 1:n_states) {
    for (j in 1:n_states) {
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

# ------------------------------------------------------------
# Create list of matrices with each cohort's p-values
# ------------------------------------------------------------

cohort_pvals <- vector("list", length(full_TMs))

for (i in seq_along(full_TMs)) {
  cohort_pvals[[i]] <- cellwise_pvals(obs_TM = full_TMs[[i]],
                                      tm_prob_list = tm_prob_list)
}

# ------------------------------------------------------------
# Visualize each cohorts' cellwise p-values
# ------------------------------------------------------------

# Convert matrices into long format for ggplot
cpvals_long <- vector("list", length(cohort_pvals))

for (i in seq_along(cohort_pvals)) {
  cpvals_long[[i]] <- melt(cohort_pvals[[i]]$two_sided)
  colnames(cpvals_long[[i]]) <- c("Current_State", "Next_State", "P_Value")
}

# Create heatmaps illustrating cellwise p-values
pval_maps <- list()

for (i in 1:5) {
  pval_maps[[i]] <- ggplot(cpvals_long[[i]], aes(x = Current_State, y = Next_State, 
                                                 fill = P_Value)) +
    geom_tile(color = "black") +
    scale_fill_gradientn(
      colours = c("black", "white"),
      values  = c(0, 1),
      limits  = c(0, 0.05),
      name    = "P-Value",
      oob     = scales::squish,   # clamp values > 0.05 to 0.05
    ) +
    labs(title = glue("Cohort {i} Cellwise P-Values"),
         x = "Current State",
         y = "Next State") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right"
      ) +
    scale_y_reverse(breaks = 1:56) +
    scale_x_continuous(breaks = 1:56)
}

for (i in 6:9) {
  pval_maps[[i]] <- ggplot(cpvals_long[[i]], aes(x = Current_State, y = Next_State, 
                                                 fill = P_Value)) +
    geom_tile(color = "black") +
    scale_fill_gradientn(
      colours = c("black", "white"),
      values  = c(0, 1),
      limits  = c(0, 0.05),
      name    = "P-Value",
      oob     = scales::squish,   # clamp values > 0.05 to 0.05
    ) +
    labs(title = glue("Cohort {i+1} Cellwise P-Values"),
         x = "Current State",
         y = "Next State",
         fill = "P-Value") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    ) +
    scale_y_reverse(breaks=1:56) +
    scale_x_continuous(breaks=1:56)
}

for (i in seq_along(pval_maps)) {
  print(pval_maps[[i]])
}
