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


plot_all_classes(reps3)
plot_all_classes(reps4)
plot_all_classes(reps5)
plot_all_classes(reps6)
