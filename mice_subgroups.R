# mice_subgroups.R
# Where do mice spend their time, who do they spend it with, and do
# those "who" groupings (subgroups) persist across multiple days?

library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)

# ---- 1. load transition data -------------------------------------
# produced by mice_following_others.R / ai_transition_identifier.R
# (mouse_id is already character and the timestamps are already POSIXct,
# since saveRDS preserves R types exactly - no re-parsing needed here)
all_true <- readRDS("Summer2026/mousetropolis/data/all_true_crossings.rds")

# mouse 900133000459667 is a different tag batch and never co-occurs with
# the rest of the cohort - drop it so it doesn't show up as a permanent
# singleton "subgroup" of one
all_true <- all_true %>% filter(mouse_id != "900133000459667")

# ---- 2. turn transitions into "time spent in a box" intervals ----
# a mouse is "in" to_box from the moment it finishes crossing
# (end_datetimestamp) until it starts its next crossing
# (start_datetimestamp of the next row). suspicious_jump rows are
# dropped since we can't trust where the mouse actually was.
dwell <- all_true %>%
  arrange(mouse_id, start_datetimestamp) %>%
  group_by(mouse_id) %>%
  mutate(
    dwell_start = end_datetimestamp,
    dwell_end   = lead(start_datetimestamp)
  ) %>%
  ungroup() %>%
  filter(!suspicious_jump, !is.na(dwell_end)) %>%
  transmute(
    mouse_id,
    box = to_box,
    start = dwell_start,
    end = dwell_end,
    duration_sec = as.numeric(difftime(end, start, units = "secs")),
    date = as.Date(start)
  ) %>%
  filter(duration_sec > 0)

# ---- 3. WHERE: total time spent per box, mapped onto the layout --
node_coords <- data.frame(
  box = c("FC1", "A", "B", "FC2", "C", "D", "E", "F", "G", "H", "I", "J", "FC3", "K", "L", "FC4"),
  x   = c(1,      2,   3,   4,     1,   2,   3,   4,   1,   2,   3,   4,   1,     2,   3,   4),
  y   = c(4,      4,   4,   4,     3,   3,   3,   3,   2,   2,   2,   2,   1,     1,   1,   1)
)

box_time <- node_coords %>%
  left_join(
    dwell %>% group_by(box) %>% summarise(total_hours = sum(duration_sec) / 3600, .groups = "drop"),
    by = "box"
  ) %>%
  mutate(total_hours = replace_na(total_hours, 0))

print(
  ggplot(box_time, aes(x = x, y = y, fill = total_hours)) +
    geom_tile(width = 0.9, height = 0.9, color = "white") +
    geom_text(aes(label = paste0(box, "\n", round(total_hours, 1), "h")), size = 3) +
    scale_fill_gradient(low = "lightyellow", high = "darkred") +
    scale_y_reverse() +
    coord_fixed() +
    theme_void() +
    labs(title = "Total time spent per box (all mice, all days)", fill = "Hours")
)

# per-mouse x box breakdown, to see who favors which boxes
mouse_box_time <- dwell %>%
  group_by(mouse_id, box) %>%
  summarise(hours = sum(duration_sec) / 3600, .groups = "drop")

print(
  ggplot(mouse_box_time, aes(x = box, y = mouse_id, fill = hours)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "lightyellow", high = "darkred") +
    labs(title = "Time spent per box, by mouse", x = "Box", y = "Mouse ID", fill = "Hours") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# ---- 4. WHO: co-occupancy between mice in the same box, same time ----
# for each day + box, find pairs of mice whose dwell intervals overlap
# and sum the overlap duration into an edge weight
compute_cooccupancy <- function(dwell_subset) {
  pairs <- list()
  for (bx in unique(dwell_subset$box)) {
    iv <- dwell_subset[dwell_subset$box == bx, ]
    n <- nrow(iv)
    if (n < 2) next
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        if (iv$mouse_id[i] == iv$mouse_id[j]) next
        overlap <- as.numeric(difftime(
          min(iv$end[i], iv$end[j]), max(iv$start[i], iv$start[j]), units = "secs"
        ))
        if (overlap > 0) {
          pairs[[length(pairs) + 1]] <- data.frame(
            mouse_a = min(iv$mouse_id[i], iv$mouse_id[j]),
            mouse_b = max(iv$mouse_id[i], iv$mouse_id[j]),
            overlap_sec = overlap
          )
        }
      }
    }
  }
  if (length(pairs) == 0) return(NULL)
  bind_rows(pairs) %>%
    group_by(mouse_a, mouse_b) %>%
    summarise(overlap_sec = sum(overlap_sec), .groups = "drop")
}

mouse_ids <- sort(unique(all_true$mouse_id))
dates <- sort(unique(dwell$date))

daily_edges <- setNames(
  lapply(dates, function(d) compute_cooccupancy(dwell[dwell$date == d, ])),
  as.character(dates)
)
daily_edges <- daily_edges[!sapply(daily_edges, is.null)]

# ---- 5. detect daily social subgroups (communities) ---------------
build_day_graph <- function(edges, mouse_ids) {
  g <- graph_from_data_frame(edges[, c("mouse_a", "mouse_b")], directed = FALSE,
                              vertices = data.frame(name = mouse_ids))
  E(g)$weight <- edges$overlap_sec
  g
}

daily_communities <- bind_rows(lapply(names(daily_edges), function(d) {
  g <- build_day_graph(daily_edges[[d]], mouse_ids)
  comm <- cluster_louvain(g, weights = E(g)$weight)
  data.frame(date = as.Date(d), mouse_id = mouse_ids, community = as.integer(membership(comm)))
}))

# ---- 6. track subgroup persistence across days --------------------
# community labels are arbitrary each day, so match each day's groups
# to the previous day's by shared membership, giving every recurring
# subgroup a stable id we can track and color consistently over time
relabel_communities <- function(daily_communities) {
  dates <- sort(unique(daily_communities$date))
  daily_communities$stable_id <- NA_integer_
  next_id <- 1L

  first <- dates[1]
  first_ids <- unique(daily_communities$community[daily_communities$date == first])
  id_map <- setNames(seq_along(first_ids), first_ids)
  daily_communities$stable_id[daily_communities$date == first] <-
    id_map[as.character(daily_communities$community[daily_communities$date == first])]
  next_id <- max(id_map) + 1L

  for (i in seq(2, length(dates))) {
    prev <- daily_communities[daily_communities$date == dates[i - 1], ]
    curr_idx <- which(daily_communities$date == dates[i])
    curr_ids <- unique(daily_communities$community[curr_idx])
    used_stable_ids <- c()

    for (cid in curr_ids) {
      members <- daily_communities$mouse_id[curr_idx][daily_communities$community[curr_idx] == cid]
      prev_ids <- unique(prev$stable_id)
      overlap_counts <- setNames(sapply(prev_ids, function(sid) {
        length(intersect(members, prev$mouse_id[prev$stable_id == sid]))
      }), prev_ids)
      best_sid <- if (length(overlap_counts) > 0 && max(overlap_counts) > 0) {
        as.integer(names(overlap_counts)[which.max(overlap_counts)])
      } else NA

      if (!is.na(best_sid) && !(best_sid %in% used_stable_ids)) {
        daily_communities$stable_id[curr_idx][daily_communities$community[curr_idx] == cid] <- best_sid
        used_stable_ids <- c(used_stable_ids, best_sid)
      } else {
        daily_communities$stable_id[curr_idx][daily_communities$community[curr_idx] == cid] <- next_id
        used_stable_ids <- c(used_stable_ids, next_id)
        next_id <- next_id + 1L
      }
    }
  }
  daily_communities
}

tracked <- relabel_communities(daily_communities)

# ---- 7. visualize subgroup persistence -----------------------------
print(
  ggplot(tracked, aes(x = date, y = mouse_id, fill = factor(stable_id))) +
    geom_tile(color = "white") +
    labs(title = "Subgroup membership over time", x = "Date", y = "Mouse ID", fill = "Subgroup") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# how "sticky" is each mouse's subgroup - share of days spent in its
# most common group
persistence_score <- tracked %>%
  group_by(mouse_id) %>%
  summarise(modal_group_share = max(table(stable_id)) / n(), .groups = "drop") %>%
  arrange(desc(modal_group_share))
print(persistence_score)

# ---- 8. daily social network plots, colored by tracked subgroup ----
for (d in names(daily_edges)) {
  edges <- daily_edges[[d]]
  g <- build_day_graph(edges, mouse_ids)

  day_groups <- tracked[tracked$date == as.Date(d), ]
  V(g)$community <- day_groups$stable_id[match(V(g)$name, day_groups$mouse_id)]

  plot(g,
       layout = layout_with_fr(g, weights = E(g)$weight),
       vertex.color = V(g)$community,
       vertex.label = V(g)$name,
       vertex.label.cex = 0.6,
       vertex.size = 15,
       edge.width = pmin(E(g)$weight / 300, 8),
       main = paste("Mouse social network -", d))
}

