library(tidyverse)
library(ggplot2)
library(reshape2)  # For melting matrix into long format
library(glue)  # For printing heatmap titles that correspond to cohort numbers

# compare each cohort TM to TM6 from theory_random_walks.R
TM_diffs <- vector("list", length(full_TMs))

for (i in seq_along(full_TMs)) {
  # subtract TM6 (random walk TM) from each cohort TM
  TM_diffs[[i]] <- full_TMs[[i]] - TM6
}

for (i in seq_along(TM_diffs)) {
  print(TM_diffs[[i]])
}

# Convert matrices into long format for ggplot
TM_diffs_long <- vector("list", length(TM_diffs))

for (i in seq_along(TM_diffs_long)) {
  TM_diffs_long[[i]] <- melt(TM_diffs[[i]])
  colnames(TM_diffs_long[[i]]) <- c("Row", "Column", "Value")
}

# Create heatmaps illustrating differences
heatmaps <- list()

for (i in 1:5) {
  heatmaps[[i]] <- ggplot(TM_diffs_long[[i]], aes(x = Column, y = Row, fill = Value)) +
    geom_tile(color = "black") +  # Add tile borders
    scale_fill_gradient2(low = "darkcyan", mid = "white", high = "orangered") +
    labs(title = glue("Cohort {i} Differences from Random Walk"),
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
  heatmaps[[i]] <- ggplot(TM_diffs_long[[i]], aes(x = Column, y = Row, fill = Value)) +
    geom_tile(color = "black") +  # Add tile borders
    scale_fill_gradient2(low = "darkcyan", mid = "white", high = "orangered") +
    labs(title = glue("Cohort {i+1} Differences from Random Walk"),
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

for (i in seq_along(heatmaps)) {
  print(heatmaps[[i]])
}
