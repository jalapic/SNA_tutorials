# Where are the mice, and who spends time with whom?
#
# Input: mice-location_ten-min-interval.rds -- one row per 10-min sample,
# one column per mouse (A-P), value = room the mouse was in (A-L, or FC1-4).

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(igraph)
library(viridis)

loc <- readRDS("Summer2026/mousetropolis/data/mice-location_ten-min-interval.rds")
mouse_ids <- setdiff(names(loc), "sampled_time")

long <- loc %>%
  pivot_longer(all_of(mouse_ids), names_to = "mouse_id", values_to = "room") %>%
  filter(!is.na(room))

# room positions matching the physical floorplan (same layout as
# identify_transitions_follows/tracking_mice_movements.R)
node_coords <- data.frame(
  name = c("FC1", "A", "B", "FC2", "C", "D", "E", "F", "G", "H", "I", "J", "FC3", "K", "L", "FC4"),
  x    = c(1,      2,   3,   4,     1,   2,   3,   4,   1,   2,   3,   4,   1,     2,   3,   4),
  y    = c(4,      4,   4,   4,     3,   3,   3,   3,   2,   2,   2,   2,   1,     1,   1,   1)
)

long <- long %>% mutate(room = factor(room, levels = node_coords$name))

# ---- 1. visualizing where the mice are over time ----
# each plot below is shown twice: with food chambers (FC1-4) included, and
# with them excluded so the sparser social-room traffic isn't dwarfed by FC1.

long_with_fc <- long
long_no_fc   <- long %>% filter(!grepl("^FC", room)) %>% droplevels()

# 1a. room occupancy heatmap: how many mice are in each room, hour by hour
plot_occupancy <- function(data, subtitle) {
  occupancy <- data %>%
    mutate(hour_bin = lubridate::floor_date(sampled_time, "1 hour")) %>%
    count(hour_bin, room)

  ggplot(occupancy, aes(x = hour_bin, y = room, fill = n)) +
    geom_tile() +
    scale_fill_viridis_c(name = "# mice") +
    labs(x = "Time", y = "Room", title = "Room occupancy over time", subtitle = subtitle) +
    theme_minimal()
}

print(plot_occupancy(long_with_fc, "Food chambers included"))
print(plot_occupancy(long_no_fc, "Food chambers excluded -- social rooms only"))

# 1b. per-mouse room timeline: where is each individual mouse, over the full period
plot_timelines <- function(data, subtitle) {
  ggplot(data, aes(x = sampled_time, y = room, color = mouse_id)) +
    geom_path(aes(group = mouse_id), alpha = 0.15) +
    geom_point(size = 0.5, alpha = 0.6) +
    facet_wrap(~mouse_id) +
    guides(color = "none") +
    labs(x = "Time", y = "Room", title = "Location of each mouse over time", subtitle = subtitle) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
          axis.text.y = element_text(size = 6))
}

print(plot_timelines(long_with_fc, "Food chambers included"))
print(plot_timelines(long_no_fc, "Food chambers excluded -- social rooms only"))

# ---- 2. who spends time with whom (proximity-based social network) ----

# FC1-4 are shared food/water chambers -- 58% of all records are FC1 alone,
# with 9+ mice there at once (up to 15/16). Counting shared-food-chamber time
# as "together" makes nearly every pair look highly associated just from
# feeding traffic, not social preference -- so, like the plots above, this is
# computed both ways for comparison.

# complete grid of all possible pairs, so pairs never seen together get a 0
all_pairs <- as.data.frame(t(combn(sort(mouse_ids), 2))) %>%
  setNames(c("mouse1", "mouse2"))

# simple ratio association index:
# SRI = (time seen together) / (time either was observed, minus the overlap)
compute_assoc <- function(data) {
  n_obs <- data %>% count(mouse_id, name = "n_obs")

  # for every sampled_time + room with >1 mouse present, every pair in that
  # room counts as "together" for that snapshot
  snapshots_with_company <- data %>%
    group_by(sampled_time, room) %>%
    filter(n() > 1) %>%
    group_split()

  pair_together <- map_dfr(snapshots_with_company, function(df) {
    mice <- sort(unique(df$mouse_id))
    as.data.frame(t(combn(mice, 2))) %>% setNames(c("mouse1", "mouse2"))
  }) %>%
    count(mouse1, mouse2, name = "n_together")

  all_pairs %>%
    left_join(pair_together, by = c("mouse1", "mouse2")) %>%
    mutate(n_together = coalesce(n_together, 0L)) %>%
    left_join(n_obs, by = c("mouse1" = "mouse_id")) %>% rename(n_obs1 = n_obs) %>%
    left_join(n_obs, by = c("mouse2" = "mouse_id")) %>% rename(n_obs2 = n_obs) %>%
    mutate(sri = n_together / (n_obs1 + n_obs2 - n_together))
}

assoc_no_fc   <- compute_assoc(long_no_fc)
assoc_with_fc <- compute_assoc(long_with_fc)

saveRDS(assoc_no_fc, "Summer2026/mousetropolis/data/mouse_association_index_no_fc.rds")
saveRDS(assoc_with_fc, "Summer2026/mousetropolis/data/mouse_association_index_with_fc.rds")

cat("Top 10 most-associated mouse pairs (food chambers excluded):\n")
print(assoc_no_fc %>% arrange(desc(sri)) %>% select(mouse1, mouse2, n_together, sri) %>% head(10))

cat("\nTop 10 most-associated mouse pairs (food chambers included):\n")
print(assoc_with_fc %>% arrange(desc(sri)) %>% select(mouse1, mouse2, n_together, sri) %>% head(10))

# association matrix heatmap, ordered by dominance rank (most wins -> fewest wins)
win_rank_order <- c("C", "G", "L", "D", "B", "M", "J", "A", "O", "I", "E", "N", "K", "F", "P", "H")

plot_matrix <- function(assoc_df, subtitle) {
  assoc_sym <- bind_rows(
    assoc_df %>% select(a = mouse1, b = mouse2, sri),
    assoc_df %>% select(a = mouse2, b = mouse1, sri)
  ) %>%
    mutate(a = factor(a, levels = win_rank_order),
           b = factor(b, levels = rev(win_rank_order)))

  ggplot(assoc_sym, aes(x = a, y = b, fill = sri)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(name = "SRI") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "Mouse association index (co-occurrence)",
         subtitle = paste(subtitle, "-- ordered by wins: most -> fewest")) +
    theme_minimal() +
    coord_fixed()
}

print(plot_matrix(assoc_no_fc, "Food chambers excluded"))
print(plot_matrix(assoc_with_fc, "Food chambers included"))

# network graph, edge width/color by association strength
plot_network <- function(assoc_df, subtitle) {
  g <- graph_from_data_frame(
    assoc_df %>% filter(sri > 0) %>% select(mouse1, mouse2, weight = sri),
    directed = FALSE,
    vertices = data.frame(name = sort(mouse_ids))
  )

  set.seed(1)
  plot(g,
       layout = layout_with_fr(g),
       edge.width = E(g)$weight * 25,
       edge.color = rgb(0.2, 0.2, 0.6, 0.4),
       vertex.size = 20,
       vertex.color = "lightblue",
       vertex.label = V(g)$name,
       vertex.label.cex = 0.9,
       main = paste("Mouse co-occurrence network (simple ratio index)\n", subtitle))
}

plot_network(assoc_no_fc, "Food chambers excluded")
plot_network(assoc_with_fc, "Food chambers included")
