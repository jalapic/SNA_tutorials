library(dplyr)
library(tidyr)
library(purrr)

# find how many interactions happened in each state - uses bot_dfs
all_isomorphs <- 1:56

state_counts <- map_dfr(seq_along(bot_dfs), function(i) {
  bot_dfs[[i]] %>%
    mutate(isomorph_class = factor(isomorph_class, levels = all_isomorphs)) %>%
    count(isomorph_class, name = "n", .drop = FALSE) %>%
    mutate(cohort = if_else(i >= 6, i + 1, i))
}) %>%
  complete(
    cohort,
    isomorph_class = factor(all_isomorphs, levels = all_isomorphs),
    fill = list(n = 0)
  ) %>%
  arrange(isomorph_class, cohort)

wide_state_counts <- state_counts %>%
  pivot_wider(
    names_from = cohort,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(isomorph_class)

# add columns with transitive and intransitive triad counts
# from Summer2026/code/tasks/tables/isomorph-tables_3-6nodes.R
full_state_counts <- wide_state_counts %>%
  mutate(isomorph_class = as.integer(as.character(isomorph_class))) %>%
  left_join(
    df6 %>% select(Tournament, Intransitive_Triads),
    by = c("isomorph_class" = "Tournament")
  )

View(full_state_counts)

# find median number of within-state interactions across all cohorts
median_ints <- state_counts %>%
  group_by(isomorph_class) %>%
  summarise(median_interactions = median(n), .groups = "drop") %>%
  arrange(isomorph_class)

View(median_ints)

full_median_ints <- median_ints %>%
  mutate(isomorph_class = as.integer(as.character(isomorph_class))) %>%
  left_join(
    df6 %>% select(Tournament, Intransitive_Triads),
    by = c("isomorph_class" = "Tournament")
  )

View(full_median_ints)

median_dist <- ggplot(data = full_median_ints, 
                      aes(x = factor(isomorph_class), y = median_interactions,
                          fill = factor(Intransitive_Triads))) +
  geom_col() +
  ggtitle("Median Number of Interactions Per Isomorph Class") +
  xlab("Isomorph Class") +
  ylab("Median Number of Interactions") +
  labs(fill = "Intransitive Triad Count") +
  theme_bw() +
  scale_x_discrete(minor_breaks = seq(1, 56, by = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

median_dist

# find median number of interactions that occur in each group of 
# isomorph classes with the same number of intransitive triads
median_intrans <- full_state_counts %>%
  pivot_longer(cols = matches("^\\d+$"),
               names_to = NULL,
               values_to = "interactions"
  ) %>%
  select(Intransitive_Triads, interactions) %>%
  group_by(Intransitive_Triads) %>%
  summarise(
    median_interactions = median(interactions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Intransitive_Triads)

View(median_intrans)

med_intrans_plt <- ggplot(data = median_intrans, 
                          aes(x = factor(Intransitive_Triads), 
                              y = median_interactions,
                              fill = factor(Intransitive_Triads))) +
  geom_col() +
  ggtitle("Median Number of Interactions Per Intransitive Triad Group") +
  xlab("Intransitive Triad Count") +
  ylab("Median Number of Interactions") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_discrete(minor_breaks = seq(0, 8, by = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

med_intrans_plt
