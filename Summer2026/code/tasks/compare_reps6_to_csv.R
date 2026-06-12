# ------------------ compare reps6 plots to nodes6_tournaments.csv ------------------
# this file is comparing the graphs created when n=6 in plot_tournament_graphs.R
# to the graphs created by Paredes, whose paper can be accessed at 
# https://www.researchgate.net/publication/28313750_Some_results_on_the_geometry_of_full_flag_manifolds_and_harmonic_maps
# we are simply using this file to check our work


library(igraph)
try(detach("package:sna", unload = TRUE), silent = TRUE)
try(detach("package:network", unload = TRUE), silent = TRUE)

# -- define only the helper functions needed (avoids sourcing the full script) --

combn_list <- function(n, k) {
  if (n < k) return(list())
  out <- utils::combn(n, k, simplify = FALSE)
  lapply(out, as.integer)
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

canon_key <- function(g) {
  p  <- igraph::canonical_permutation(g)$labeling
  gc <- igraph::permute(g, p)
  M  <- igraph::as_adjacency_matrix(gc, sparse = FALSE)
  paste(as.integer(M), collapse = "")
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

get_score_sequence <- function(g) as.integer(sort(igraph::degree(g, mode = "out")))

count_triads <- function(g) {
  n <- igraph::vcount(g)
  if (n < 3) return(list(transitive_count = 0L, intransitive_count = 0L))
  combos <- combn_list(n, 3)
  trans <- 0L; intrans <- 0L
  for (idx in combos) {
    s  <- igraph::induced_subgraph(g, idx)
    cs <- igraph::triad_census(s)
    if (cs[9] == 1)  trans  <- trans  + 1L
    else if (cs[10] == 1) intrans <- intrans + 1L
  }
  list(transitive_count = trans, intransitive_count = intrans)
}

cat("Enumerating reps6 (this may take a moment)...\n")
reps6 <- enumerate_unlabeled_tournaments(6)

# -- load CSV (correct reference order) --
csv <- read.csv("/Users/delaneyvanderpool/Desktop/Research/Summer26Networks/SNA_tutorials/Summer2026/data/isomorphs/nodes6_tournaments.csv",
                stringsAsFactors = FALSE)

# -- apply the same ordering as plot_all_classes --
score_seqs <- sapply(reps6, function(g) paste(get_score_sequence(g), collapse = ","))
reps6_sorted <- reps6[order(score_seqs)]

# -- compute metrics for each sorted tournament --
computed <- lapply(seq_along(reps6_sorted), function(i) {
  g  <- reps6_sorted[[i]]
  sc <- paste(get_score_sequence(g), collapse = ",")
  tr <- count_triads(g)
  list(
    Tournament        = i,
    Score_Sequence    = sc,
    Transitive_Triads = tr$transitive_count,
    Intransitive_Triads = tr$intransitive_count
  )
})

# -- compare row by row --
cat(sprintf("%-12s %-20s %-10s %-12s %-20s %-10s %-12s  %s\n",
    "Tournament", "CSV_ScoreSeq", "CSV_Trans", "CSV_Intrans",
    "R_ScoreSeq", "R_Trans", "R_Intrans", "Match?"))
cat(strrep("-", 110), "\n")

all_match <- TRUE
for (i in seq_len(nrow(csv))) {
  r <- computed[[i]]
  sc_match <- csv$Score_Sequence[i]    == r$Score_Sequence
  tr_match <- csv$Transitive_Triads[i] == r$Transitive_Triads
  it_match <- csv$Intransitive_Triads[i] == r$Intransitive_Triads
  match    <- sc_match & tr_match & it_match
  if (!match) all_match <- FALSE

  cat(sprintf("%-12d %-20s %-10d %-12d %-20s %-10d %-12d  %s\n",
      i,
      csv$Score_Sequence[i],    csv$Transitive_Triads[i],   csv$Intransitive_Triads[i],
      r$Score_Sequence,         r$Transitive_Triads,         r$Intransitive_Triads,
      ifelse(match, "OK", "MISMATCH")))
}

cat(strrep("-", 110), "\n")
if (all_match) {
  cat("All 56 tournaments match the CSV in score sequence, transitive triads, and intransitive triads.\n")
} else {
  cat("WARNING: One or more mismatches found. Check rows marked MISMATCH above.\n")
}
