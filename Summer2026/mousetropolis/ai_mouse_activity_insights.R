# flags noteworthy patterns in the mouse transition data:
#   1. extended stretches where a mouse has zero verified transitions
#   2. pairs of mice whose box-to-box movement patterns look unusually similar
#   3. mice with a high rate of "suspicious" (topologically impossible) jumps
#   4. mice whose overall activity level is a statistical outlier
#   5. each mouse's peak activity hour, to spot diurnal/nocturnal individuals

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# thresholds - tweak as needed
MIN_GAP_HOURS   <- 6    # flag inactivity runs at or above this length
TOP_N_SIMILAR   <- 10   # how many mouse pairs to report
OUTLIER_Z       <- 2    # |z| above this on total activity counts as an outlier

all_true <- readRDS("Summer2026/mousetropolis/data/all_true_crossings.rds")

# same hourly activity table used in tracking_mice_movements.R
activity <- all_true %>%
  mutate(
    dt   = start_datetimestamp,
    hour = as.integer(format(dt, "%H")),
    date = as.Date(dt, tz = "America/Chicago")
  ) %>%
  filter(!(date == as.Date("2026-05-02") & hour >= 18)) %>%
  group_by(mouse_id, date, hour) %>%
  summarise(transitions = n(), .groups = "drop") %>%
  complete(mouse_id, date, hour = 0:23, fill = list(transitions = 0)) %>%
  arrange(mouse_id, date, hour)


## ---- 1. extended inactivity ----

find_inactivity_gaps <- function(activity, min_gap_hours = MIN_GAP_HOURS) {
  activity %>%
    group_by(mouse_id) %>%
    arrange(date, hour, .by_group = TRUE) %>%
    mutate(
      is_zero = transitions == 0,
      run_id  = cumsum(!is_zero) # increments only on a nonzero hour, so zero-runs share an id
    ) %>%
    filter(is_zero) %>%
    group_by(mouse_id, run_id) %>%
    summarise(
      start_date = first(date), start_hour = first(hour),
      end_date   = last(date),  end_hour   = last(hour),
      length_hours = n(),
      .groups = "drop"
    ) %>%
    select(-run_id) %>%
    filter(length_hours >= min_gap_hours) %>%
    arrange(desc(length_hours))
}

gaps <- find_inactivity_gaps(activity)

cat("=== Extended inactivity (>=", MIN_GAP_HOURS, "consecutive hours with zero transitions) ===\n")
if (nrow(gaps) == 0) {
  cat("none found\n")
} else {
  for (i in seq_len(nrow(gaps))) {
    g <- gaps[i, ]
    cat(sprintf("Mouse %s: %d hrs inactive, from %s %02d:00 to %s %02d:00\n",
                g$mouse_id, g$length_hours,
                g$start_date, g$start_hour, g$end_date, g$end_hour))
  }
}


## ---- 2. mice with similar movement patterns ----
# profile each mouse by how often it uses each box-to-box edge, then compare
# profiles with cosine similarity (scale-invariant, so a quiet mouse and a
# busy mouse can still score as "similar" if they favor the same routes)

edge_profile <- all_true %>%
  count(mouse_id, from_box, to_box, name = "n") %>%
  unite(edge, from_box, to_box, sep = "->") %>%
  pivot_wider(names_from = edge, values_from = n, values_fill = 0)

mouse_ids <- edge_profile$mouse_id
mat <- as.matrix(edge_profile[, -1])
rownames(mat) <- mouse_ids

cosine_sim <- function(m) {
  norm <- sqrt(rowSums(m^2))
  (m %*% t(m)) / outer(norm, norm)
}

sim <- cosine_sim(mat)
diag(sim) <- NA

sim_df <- as.data.frame(as.table(sim)) %>%
  rename(mouse_a = Var1, mouse_b = Var2, similarity = Freq) %>%
  filter(!is.na(similarity), as.character(mouse_a) < as.character(mouse_b)) %>%
  arrange(desc(similarity))

cat("\n=== Most similar box-usage patterns between mouse pairs (cosine similarity) ===\n")
print(head(sim_df, TOP_N_SIMILAR), row.names = FALSE)

p_sim <- ggplot(as.data.frame(as.table(sim)), aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred", na.value = "grey90", name = "cosine\nsimilarity") +
  labs(title = "Mouse-to-mouse box-usage similarity", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_sim)


## ---- 3. suspicious jumps (possible antenna misreads / topology violations) ----

susp <- all_true %>%
  group_by(mouse_id) %>%
  summarise(
    total_transitions = n(),
    suspicious = sum(suspicious_jump, na.rm = TRUE),
    pct_suspicious = round(100 * suspicious / total_transitions, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_suspicious))

cat("\n=== Suspicious jump rate per mouse (higher = more likely antenna misreads) ===\n")
print(susp, row.names = FALSE)


## ---- 4. overall activity level outliers ----

totals <- all_true %>%
  count(mouse_id, name = "total_transitions") %>%
  mutate(z = (total_transitions - median(total_transitions)) / mad(total_transitions)) %>%
  arrange(z)

cat("\n=== Total verified transitions per mouse (whole study) ===\n")
print(totals, row.names = FALSE)

outliers <- totals %>% filter(abs(z) > OUTLIER_Z)
cat("\nActivity-level outliers (|z| >", OUTLIER_Z, "using median/MAD):\n")
if (nrow(outliers) == 0) cat("none found\n") else print(outliers, row.names = FALSE)


## ---- 5. peak activity hour per mouse (diurnal vs. nocturnal individuals) ----

peak_hours <- activity %>%
  group_by(mouse_id, hour) %>%
  summarise(avg_transitions = mean(transitions), .groups = "drop") %>%
  group_by(mouse_id) %>%
  slice_max(avg_transitions, n = 1, with_ties = FALSE) %>%
  arrange(hour)

cat("\n=== Each mouse's peak activity hour (averaged across all days) ===\n")
print(peak_hours, row.names = FALSE)
