# ------------------ isomorph lookup + transitions ------------------

# get graph_list (isomorph dictionary)
d <- readRDS("rfid_data/nodes6_g.RDS")

# get g <- graph_from_data_frame(lastints)

# Return the index or NA if no match found
check_isomorphism <- function(g, graph_list) {
  match <- which(unlist(lapply(graph_list, function(x) graph.isomorphic(g, x))))
  return(ifelse(length(match) > 0, match, NA))
}