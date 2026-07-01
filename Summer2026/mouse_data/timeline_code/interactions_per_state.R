library(dplyr)
library(tidyr)
library(purrr)

# find how many interactions happened in each state - uses c_dfs
state_counts <- map_dfr(seq_along(c_dfs), function(i) {
  c_dfs[[i]] %>%
    count(isomorph_class, name = "n") %>%
    mutate(cohort = if_else(i >= 6, i + 1, i))
})

wide_state_counts <- state_counts %>%
  pivot_wider(
    names_from = cohort,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(isomorph_class)

wide_state_counts

# find median number of within-state interactions across all cohorts
median_ints <- state_counts %>%
  group_by(isomorph_class) %>%
  summarise(median_interactions = median(n), .groups = "drop") %>%
  arrange(isomorph_class)

median_ints

median_dist <- ggplot(data = median_ints, 
                      aes(x = factor(isomorph_class), y = median_interactions)) +
  geom_col() +
  ggtitle("Median Number of Interactions Per Isomorph Class") +
  xlab("Isomorph Class") +
  ylab("Median Number of Interactions") +
  theme_bw() +
  scale_x_discrete(minor_breaks = seq(1, 56, by = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

median_dist
