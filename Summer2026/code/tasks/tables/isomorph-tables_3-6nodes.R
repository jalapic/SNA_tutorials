library(igraph)

# Avoid masked generics if these packages were attached earlier
try(detach("package:sna", unload = TRUE), silent = TRUE)
try(detach("package:network", unload = TRUE), silent = TRUE)

# ------------------ make functions ------------------

# ------------------ small helper functions ------------------

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

# ------------------ build dataframe  ------------------

build_tournament_df <- function(reps, include_4sets = TRUE, include_4census = TRUE) {
  # Score_ID mapping
  score_strings <- vapply(reps, function(g) paste(get_score_sequence(g), collapse = ","), "")
  uniq_scores <- sort(unique(score_strings))
  score_to_id <- setNames(seq_along(uniq_scores), uniq_scores)

  
  rows <- lapply(seq_along(reps), function(i) {
    G <- reps[[i]]
    score_seq <- get_score_sequence(G)
    triad_data <- count_triads(G)
    intrans_str <-
      if (length(triad_data$intransitive_triads) == 0) "" else
        paste(vapply(triad_data$intransitive_triads, function(x) paste(x, collapse = ","), ""), collapse = " | ")
    
    data.frame(
      Tournament = i,
      Score_Sequence = paste(score_seq, collapse = ","),
      Transitive_Triads = triad_data$transitive_count,
      Intransitive_Triads = triad_data$intransitive_count,
      Intransitive_Out_Degrees = intrans_str,
      Score_ID = score_to_id[[paste(score_seq, collapse = ",")]],
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

# ------------------ make tables ------------------

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