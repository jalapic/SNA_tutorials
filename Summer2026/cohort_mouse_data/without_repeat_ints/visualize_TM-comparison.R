library(tidyverse)
library(ggplot2)
library(reshape2)  # For melting matrix into long format
library(glue)  # For printing heatmap titles that correspond to cohort numbers

# loop to make transition matrices for all cohorts
TM_list <- vector("list", length(clean_dfs))

for (i in seq_along(clean_dfs)) {
  TM_list[[i]] <- make_TM(clean_dfs[[i]])
}

for (i in seq_along(TM_list)) {
  print(TM_list[[i]])
}

# turn TM counts to decimals for each cohort (no 6th cohort)
for (i in seq_along(TM_list)) {
  TM_list[[i]] <- TM_list[[i]] / rowSums(TM_list[[i]])
}

# cohort TMs do not include all possible network states (<56x56) - fill them in
full_TMs <- vector("list", length(TM_list))

for (i in seq_along(full_TMs)) {
  full_TMs[[i]] <- matrix(0, nrow = 56, ncol = 56)
  rownames(full_TMs[[i]]) <- colnames(full_TMs[[i]]) <- 
    as.character(1:56)
  full_TMs[[i]][rownames(TM_list[[i]]), colnames(TM_list[[i]])] <- 
    TM_list[[i]]
}

for (i in seq_along(full_TMs)) {
  print(full_TMs[[i]])
}

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
    labs(title = glue("Collapsed Cohort {i} Diff from Random Walk"),
         x = "Next State",
         y = "Current State",
         fill = "Difference") +
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
    labs(title = glue("Collapsed Cohort {i+1} Diff from Random Walk"),
         x = "Next State",
         y = "Current State",
         fill = "Difference") +
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
