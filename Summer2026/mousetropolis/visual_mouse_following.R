# visual representation of mouse following transitions
# must run mice_following_others.R first

# import libraries
library(dplyr)
library(igraph)

# source files
following_pairs_unique <- readRDS("Summer2026/mousetropolis/data/following_pairs_unique.RDS")

# node positions matching the physical layout
node_coords <- data.frame(
  name = c("FC1", "A", "B", "FC2", "C", "D", "E", "F", "G", "H", "I", "J", "FC3", "K", "L", "FC4"),
  x    = c(1,      2,   3,   4,     1,   2,   3,   4,   1,   2,   3,   4,   1,     2,   3,   4),
  y    = c(4,      4,   4,   4,     3,   3,   3,   3,   2,   2,   2,   2,   1,     1,   1,   1)
)

# count how many following transitions happened on each edge
edge_counts <- following_pairs_unique %>%
  group_by(box_from, box_to) %>%
  summarise(count = n(), .groups = "drop")

# build graph
g <- graph_from_data_frame(edge_counts, directed = TRUE, vertices = node_coords)

# plot
layout_mat <- as.matrix(node_coords[, c("x", "y")])

plot(g,
     layout = layout_mat,
     edge.width = E(g)$count,
     edge.arrow.size = 0.5,
     edge.label = E(g)$count,
     vertex.label = V(g)$name,
     vertex.size = 30,
     main = "Following transitions between boxes")
