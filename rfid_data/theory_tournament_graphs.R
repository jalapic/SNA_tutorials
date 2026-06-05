# ============================================================
# Tournaments (n = 3..6)
# ============================================================

  library(igraph)

# Avoid masked generics if these packages were attached earlier
try(detach("package:sna", unload = TRUE), silent = TRUE)
try(detach("package:network", unload = TRUE), silent = TRUE)

# ------------------ small helpers ------------------

# robust combn -> always returns a list of integer vectors
combn_list <- function(n, k) {
  if (n < k) return(list())
  out <- utils::combn(n, k, simplify = FALSE)
  lapply(out, as.integer)
}

# Build a tournament on n nodes from integer 'bits' encoding of upper triangle
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

# Canonical key for a directed graph (BLISS canonical permutation -> adjacency string)
canon_key <- function(g) {
  p  <- igraph::canonical_permutation(g)$labeling
  gc <- igraph::permute(g, p)
  M  <- igraph::as_adjacency_matrix(gc, sparse = FALSE)
  paste(as.integer(M), collapse = "")
}

# Enumerate one representative per non-isomorphic class (unlabeled tournaments)
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

# ------------------ getting metrics/diagnostics ------------------

# Sorted out-degree vector (score sequence)
get_score_sequence <- function(g) as.integer(sort(igraph::degree(g, mode = "out")))

# Triad counts + per-intransitive-triad out-degree triples
# Returns list(transitive_count, intransitive_count, intransitive_triads=list of triples)
count_triads <- function(g) {
  n <- igraph::vcount(g)
  if (n < 3) return(list(transitive_count = 0L, intransitive_count = 0L, intransitive_triads = list()))
  d <- igraph::degree(g, mode = "out")
  combos <- combn_list(n, 3)
  trans <- 0L; intrans <- 0L
  triad_triples <- list(); pos <- 1L
  for (idx in combos) {
    s <- igraph::induced_subgraph(g, idx)
    cs <- igraph::triad_census(s)
    # igraph order: "030T" (transitive) index 9; "030C" (3-cycle) index 10
    if (cs[9] == 1) {
      trans <- trans + 1L
    } else if (cs[10] == 1) {
      intrans <- intrans + 1L
      triad_triples[[pos]] <- sort(as.integer(d[idx]))
      pos <- pos + 1L
    }
  }
  list(transitive_count = trans, intransitive_count = intrans, intransitive_triads = triad_triples)
}

# Count acyclic (transitive) k-vertex induced subtournaments
count_transitive_k <- function(g, k) {
  n <- igraph::vcount(g); if (n < k) return(0L)
  combos <- combn_list(n, k)
  tot <- 0L
  for (idx in combos) {
    sg <- igraph::induced_subgraph(g, idx)
    if (igraph::is_dag(sg)) tot <- tot + 1L
  }
  tot
}

# --------- 4-vertex census ---------

# Build a fixed canonical-key map for all unlabeled tournaments on 4 vertices
build_four_vtx_key_map_fixed <- function() {
  n <- 4; m <- n * (n - 1L) / 2L  # 6
  keys <- character(0)
  for (bits in 0:(2^m - 1L)) {
    g <- make_tournament(n, bits)
    keys <- c(keys, canon_key(g))
  }
  sort(unique(keys))  # (should be 4 distinct keys)
}

# Census of induced 4-node subtournaments by unlabeled type (aligned with fixed key map)
four_vertex_census_with_fixed_map <- function(g, key_map_fixed) {
  n <- igraph::vcount(g)
  counts <- integer(length(key_map_fixed)); names(counts) <- key_map_fixed
  if (n < 4) return(counts)
  combos <- utils::combn(n, 4, simplify = FALSE)
  for (idx in combos) {
    s <- igraph::induced_subgraph(g, idx)
    k <- canon_key(s)
    pos <- match(k, key_map_fixed)
    if (!is.na(pos)) counts[pos] <- counts[pos] + 1L
  }
  unname(counts)
}

# ------------------ build dataframe  ------------------

# include_4sets: add Transitive_4sets
# include_4census: add FourVertex_Census as "c1,c2,c3,c4" (sums to choose(n,4))
build_tournament_df <- function(reps, include_4sets = TRUE, include_4census = TRUE) {
  # Score_ID mapping
  score_strings <- vapply(reps, function(g) paste(get_score_sequence(g), collapse = ","), "")
  uniq_scores <- sort(unique(score_strings))
  score_to_id <- setNames(seq_along(uniq_scores), uniq_scores)
  
  # Fixed 4-node key map (shared across all graphs)
  key_map_fixed <- if (include_4census) build_four_vtx_key_map_fixed() else character(0)
  
  rows <- lapply(seq_along(reps), function(i) {
    G <- reps[[i]]
    score_seq <- get_score_sequence(G)
    triad_data <- count_triads(G)
    intrans_str <-
      if (length(triad_data$intransitive_triads) == 0) "" else
        paste(vapply(triad_data$intransitive_triads, function(x) paste(x, collapse = ","), ""), collapse = " | ")
    t4 <- if (include_4sets && igraph::vcount(G) >= 4) count_transitive_k(G, 4) else NA_integer_
    c4 <- if (include_4census && length(key_map_fixed) > 0) {
      paste(four_vertex_census_with_fixed_map(G, key_map_fixed), collapse = ",")
    } else ""
    
    data.frame(
      Tournament = i,
      Score_Sequence = paste(score_seq, collapse = ","),
      Transitive_Triads = triad_data$transitive_count,
      Intransitive_Triads = triad_data$intransitive_count,
      Intransitive_Out_Degrees = intrans_str,
      Score_ID = score_to_id[[paste(score_seq, collapse = ",")]],
      Transitive_4sets = t4,
      FourVertex_Census = c4,
      stringsAsFactors = FALSE
    )
  })
  
  df <- do.call(rbind, rows)
  
  # stable sorting of the intransitive out-degree entries
  sort_intrans_entries <- function(s) {
    if (s == "") return("")
    triads <- unlist(strsplit(s, " \\| "))
    paste(sort(triads), collapse = " | ")
  }
  df$Intransitive_Out_Degrees <- vapply(df$Intransitive_Out_Degrees, sort_intrans_entries, "")
  
  # Order, reindex
  df <- df[order(df$Score_Sequence, df$Intransitive_Triads, df$Tournament), ]
  df$Tournament <- seq_len(nrow(df))
  df
}

# ------------------ plotting - this isn't great !!  ------------------

plot_tournament_panel <- function(g, main = "") {
  lay <- igraph::layout_in_circle(g)
  d <- igraph::degree(g, mode = "out")
  plot(g, layout = lay,
       vertex.size = 22, vertex.color = "grey90",
       vertex.frame.color = "black",
       vertex.label = d, vertex.label.color = "black",
       edge.arrow.size = 0.25, edge.color = "black",
       main = main)
}

plot_all_classes <- function(reps) {
  k <- length(reps)
  ncol <- ceiling(sqrt(k)); nrow <- ceiling(k / ncol)
  op <- par(mfrow = c(nrow, ncol), mar = c(0.7, 0.4, 0.7, 0.4)); on.exit(par(op))
  for (i in seq_len(k)) {
    sc <- paste(get_score_sequence(reps[[i]]), collapse = ",")
    plot_tournament_panel(reps[[i]], main = paste0(i, "  (", sc, ")"))
  }
}

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

# ------------------ RUN (n = 3..6) ------------------

reps3 <- enumerate_unlabeled_tournaments(3)  # expect 2
reps4 <- enumerate_unlabeled_tournaments(4)  # expect 4
reps5 <- enumerate_unlabeled_tournaments(5)  # expect 12
reps6 <- enumerate_unlabeled_tournaments(6)  # expect 56

cat("Counts: n=3:", length(reps3), " n=4:", length(reps4), " n=5:", length(reps5), " n=6:", length(reps6), "\n")

# Summary data frames (include 4-sets and 4-vertex census)
df3 <- build_tournament_df(reps3, include_4sets = TRUE, include_4census = TRUE)
df4 <- build_tournament_df(reps4, include_4sets = TRUE, include_4census = TRUE)
df5 <- build_tournament_df(reps5, include_4sets = TRUE, include_4census = TRUE)
df6 <- build_tournament_df(reps6, include_4sets = TRUE, include_4census = TRUE)

df3
df4
df5
df6

# Save
#write.csv(df6, "nodes6_tournaments.csv", row.names = FALSE)
#saveRDS(reps6, "tournaments_6_reps.RDS")
#saveRDS(df6,   "tournaments_6_df.RDS")

# Plots -terrible - let's do better
plot_all_classes(reps3)
plot_all_classes(reps4)
plot_all_classes(reps5)
plot_all_classes(reps6)



#--------------------------- RANDOM WALKS ----------------------- #

# this is in brief - look at other R file for more code,
# also other file has better plotting code

# the only benefit to the below is that it slightly might have a better
# way of randomizing than I came up with
#
# but my file has more info 

Nperms <- 10000  #this should probably be much,much,much bigger

reps6  # this is the list of 56 isomorphs

# Random-walk transitions for n=6
idx6 <- build_canon_index(reps6)
path <- simulate_tournament_transitions(N = Nperms, reps = reps6, index_map = idx6)
TM <- create_transition_matrix(path, num_states = length(reps6))

print(TM[1:10, 1:10])
