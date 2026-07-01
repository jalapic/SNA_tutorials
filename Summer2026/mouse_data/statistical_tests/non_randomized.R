library(igraph)

set.seed(1)

# ------------------------------------------------------------
# Canonical labeling helpers
# ------------------------------------------------------------

canon_key <- function(g) {
  p  <- igraph::canonical_permutation(g)$labeling
  gc <- igraph::permute(g, p)
  M  <- igraph::as_adjacency_matrix(gc, sparse = FALSE)
  paste(as.integer(M), collapse = "")
}

make_tournament <- function(n, bits) {
  A <- matrix(0L, n, n)
  idx <- 0L
  for (i in 1:(n - 1L)) for (j in (i + 1L):n) {
    b <- bitwAnd(bitwShiftR(bits, idx), 1L)
    if (b == 1L) A[i, j] <- 1L else A[j, i] <- 1L
    idx <- idx + 1L
  }
  igraph::graph_from_adjacency_matrix(A, mode = "directed")
}

enumerate_unlabeled_tournaments <- function(n) {
  m <- n * (n - 1L) / 2L
  seen <- new.env(hash = TRUE, parent = emptyenv())
  reps <- vector("list", 0L)
  for (bits in 0:(2^m - 1L)) {
    g <- make_tournament(n, bits)
    key <- canon_key(g)
    if (!exists(key, envir = seen, inherits = FALSE)) {
      assign(key, TRUE, envir = seen)
      reps[[length(reps) + 1L]] <- g
    }
  }
  reps
}

# ------------------------------------------------------------
# Random-walk transition machinery
# ------------------------------------------------------------

build_canon_index <- function(reps) {
  keys <- vapply(reps, canon_key, "")
  structure(seq_along(reps), names = keys)
}

isomorph_class_id <- function(g, index_map) {
  unname(index_map[[canon_key(g)]])
}

generate_tournament_graph <- function(n = 6) {
  edges <- integer(0)
  for (i in 1:(n - 1L)) for (j in (i + 1L):n) {
    if (runif(1) < 0.5) edges <- c(edges, i, j) else edges <- c(edges, j, i)
  }
  igraph::graph(edges, directed = TRUE)
}

flip_edge <- function(g) {
  E <- igraph::as_edgelist(g, names = FALSE)
  flip_index <- sample(nrow(E), 1L)
  e <- E[flip_index, ]
  Enew <- rbind(E[-flip_index, , drop = FALSE], c(e[2], e[1]))
  igraph::graph_from_edgelist(Enew, directed = TRUE)
}

simulate_tournament_transitions <- function(N = 10000, reps, index_map) {
  n <- igraph::vcount(reps[[1]])
  g <- generate_tournament_graph(n)
  path <- integer(N + 1L)
  path[1] <- isomorph_class_id(g, index_map)
  for (i in 1:N) {
    g <- flip_edge(g)
    path[i + 1L] <- isomorph_class_id(g, index_map)
  }
  path
}

create_transition_matrix <- function(sequence, num_states) {
  TM <- matrix(0L, nrow = num_states, ncol = num_states)
  for (i in 1:(length(sequence) - 1L)) {
    from <- sequence[i]
    to <- sequence[i + 1L]
    if (!is.na(from) && !is.na(to)) TM[from, to] <- TM[from, to] + 1L
  }
  TM
}

# ------------------------------------------------------------
# Build reps and run n = 6
# ------------------------------------------------------------

reps6 <- enumerate_unlabeled_tournaments(6)
idx6  <- build_canon_index(reps6)

Nperms <- 10000
path6 <- simulate_tournament_transitions(N = Nperms, reps = reps6, index_map = idx6)

TM6 <- create_transition_matrix(path6, num_states = length(reps6))
TM6_prob <- TM6 / rowSums(TM6)
TM6_prob[is.na(TM6_prob)] <- 0

print(length(reps6))
print(TM6[1:12, 1:12])
print(TM6_prob[1:12, 1:12])

# ------------------------------------------------------------
# Simulate the random walks a thousand times
# ------------------------------------------------------------

n_sim <- 1000
n_states <- length(reps6)

tm_list <- vector("list", n_sim)

# very slow... how to make more efficient ?
for (b in 1:n_sim) {
  path6 <- simulate_tournament_transitions(N = Nperms, reps = reps6, index_map = idx6)
  tm_list[[b]] <- create_transition_matrix(path6, num_states = n_states)
}

tm_prob_list <- lapply(tm_list, function(TM) {
  P <- TM / rowSums(TM)
  P[is.na(P)] <- 0
  P
})

# ------------------------------------------------------------
# Find p-values for each cell
# ------------------------------------------------------------

cellwise_pvals <- function(obs_TM, tm_prob_list, adjust = FALSE, p_adjust_method = "BH") {
  n_states <- nrow(obs_TM)
  n_sim <- length(tm_prob_list)
  
  sim_array <- array(NA_real_, dim = c(n_states, n_states, n_sim))
  for (b in 1:n_sim) {
    sim_array[, , b] <- tm_prob_list[[b]]
  }
  
  p_mat_right <- matrix(NA_real_, nrow = n_states, ncol = n_states)
  p_mat_left  <- matrix(NA_real_, nrow = n_states, ncol = n_states)
  p_mat_two   <- matrix(NA_real_, nrow = n_states, ncol = n_states)
  
  for (i in 1:n_states) {
    for (j in 1:n_states) {
      null_vals <- sim_array[i, j, ]
      obs_val <- obs_TM[i, j]
      
      p_mat_right[i, j] <- (sum(null_vals >= obs_val) + 1) / (length(null_vals) + 1)
      p_mat_left[i, j]  <- (sum(null_vals <= obs_val) + 1) / (length(null_vals) + 1)
      
      center <- median(null_vals)
      p_mat_two[i, j] <- (sum(abs(null_vals - center) >= abs(obs_val - center)) + 1) /
        (length(null_vals) + 1)
    }
  }
  
  if (adjust) {
    p_mat_right <- matrix(p.adjust(as.vector(p_mat_right), method = p_adjust_method),
                          nrow = n_states, ncol = n_states)
    p_mat_left  <- matrix(p.adjust(as.vector(p_mat_left), method = p_adjust_method),
                          nrow = n_states, ncol = n_states)
    p_mat_two   <- matrix(p.adjust(as.vector(p_mat_two), method = p_adjust_method),
                          nrow = n_states, ncol = n_states)
  }
  
  list(
    right = p_mat_right,
    left = p_mat_left,
    two_sided = p_mat_two
  )
}

cohort_pvals <- vector("list", length(full_TMs))

for (i in seq_along(full_TMs)) {
  cohort_pvals[[i]] <- cellwise_pval(full_TMs[[i]], tm_prob_list)
}

# ------------------------------------------------------------
# Visualize each cohorts' cellwise p-values
# ------------------------------------------------------------

# Convert matrices into long format for ggplot
cpvals_long <- vector("list", length(cohort_pvals))

for (i in seq_along(cohort_pvals)) {
  cpvals_long[[i]] <- melt(cohort_pvals[[i]]$two_sided)
  colnames(cpvals_long[[i]]) <- c("Row", "Column", "Value")
}

# Create heatmaps illustrating cellwise p-values
pval_maps <- list()

for (i in 1:5) {
  pval_maps[[i]] <- ggplot(cpvals_long[[i]], aes(x = Column, y = Row, fill = Value)) +
    geom_tile(color = "black") +  # Add tile borders
    scale_fill_gradient(low = "black", high = "white") +
    labs(title = glue("Cohort {i} Cellwise P-Values"),
         x = "Column",
         y = "Row",
         fill = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right") +
    scale_y_reverse(breaks=1:56) +
    scale_x_continuous(breaks=1:56)
}

for (i in 6:9) {
  pval_maps[[i]] <- ggplot(cpvals_long[[i]], aes(x = Column, y = Row, fill = Value)) +
    geom_tile(color = "black") +  # Add tile borders
    scale_fill_gradient(low = "black", high = "white") +
    labs(title = glue("Cohort {i+1} Cellwise P-Values"),
         x = "Column",
         y = "Row",
         fill = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right") +
    scale_y_reverse(breaks=1:56) +
    scale_x_continuous(breaks=1:56)
}

for (i in seq_along(pval_maps)) {
  print(pval_maps[[i]])
}
