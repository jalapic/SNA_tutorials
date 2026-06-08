# ------------------ isomorph lookup + transitions ------------------

build_canon_index <- function(reps) {
  keys <- vapply(reps, function(G) {
    p  <- igraph::canonical_permutation(G)$labeling
    gc <- igraph::permute(G, p)
    M  <- igraph::as_adjacency_matrix(gc, sparse = FALSE)
    paste(as.integer(M), collapse = "")
  }, "")
  structure(seq_along(reps), names = keys)
}

isomorph_class_id <- function(g, index_map) {
  p  <- igraph::canonical_permutation(g)$labeling
  gc <- igraph::permute(g, p)
  M  <- igraph::as_adjacency_matrix(gc, sparse = FALSE)
  key <- paste(as.integer(M), collapse = "")
  unname(index_map[[key]])
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

simulate_tournament_transitions <- function(N = 1000, reps, index_map) {
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
    from <- sequence[i]; to <- sequence[i + 1L]
    if (!is.na(from) && !is.na(to)) TM[from, to] <- TM[from, to] + 1L
  }
  TM
}