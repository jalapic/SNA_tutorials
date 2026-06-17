# ------------------ isomorph transitions ------------------

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

#--------------------------- RANDOM WALKS ----------------------- #

# this is in brief - look at other R file for more code,
# also other file has better plotting code

# the only benefit to the below is that it slightly might have a better
# way of randomizing than I came up with
#
# but my file has more info 

Nperms <- 10000  #this should probably be much,much,much bigger

reps4  # list of 4 isomorphs
reps5  # list of 12 isomorphs
reps6  # list of 56 isomorphs

# ------------------Random-walk transitions for n=4-----------------
idx4 <- build_canon_index(reps4)
path4 <- simulate_tournament_transitions(N = Nperms, reps = reps4, 
                                         index_map = idx4)

# bounds error: adjusted with AI
states4 <- sort(unique(path4))
path_idx4 <- match(path4, states4)
TM4 <- create_transition_matrix(path_idx4, num_states = length(states4))
# TM4 <- create_transition_matrix(path, num_states = length(reps4))

print(TM4[1:4, 1:4])

# converts row counts to decimals that add up to 1
TM4 <- TM4 / rowSums(TM4)

print(TM4[1:4, 1:4])

# ------------------Random-walk transitions for n=5-----------------
idx5 <- build_canon_index(reps5)
path5 <- simulate_tournament_transitions(N = Nperms, reps = reps5, 
                                         index_map = idx5)

# bounds error: adjusted with AI
states5 <- sort(unique(path5))
path_idx5 <- match(path5, states5)
TM5 <- create_transition_matrix(path_idx5, num_states = length(states5))
#TM5 <- create_transition_matrix(path, num_states = length(reps5))

print(TM5[1:12, 1:12])

# converts row counts to decimals that add up to 1
TM5 <- TM5 / rowSums(TM5)

print(TM5[1:12, 1:12])

# ------------------Random-walk transitions for n=6-----------------
idx6 <- build_canon_index(reps6)
path6 <- simulate_tournament_transitions(N = Nperms, reps = reps6, index_map = idx6)
TM6 <- create_transition_matrix(path6, num_states = length(reps6))

print(TM6[1:12, 1:12])

# converts row counts to decimals that add up to 1
TM6 <- TM6 / rowSums(TM6)

print(TM6[1:12, 1:12])
