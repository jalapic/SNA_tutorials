# run mice_following_others.R first to generate/update following_pairs.rds

suppressMessages({
  library(dplyr)
  library(ggplot2)
})

following_pairs <- readRDS("Summer2026/mousetropolis/data/following_pairs.rds")

times_chased <- following_pairs %>%
  count(mouse_id = leader, name = "total") %>%
  mutate(role = "times chased (submissive)")

times_chasing <- following_pairs %>%
  count(mouse_id = follower, name = "total") %>%
  mutate(role = "times chasing (dominant)")

summary_df <- bind_rows(times_chased, times_chasing)

# order mice by their combined total so the busiest mice show up together
mouse_order <- summary_df %>%
  group_by(mouse_id) %>%
  summarise(grand_total = sum(total), .groups = "drop") %>%
  arrange(desc(grand_total)) %>%
  pull(mouse_id)

summary_df$mouse_id <- factor(summary_df$mouse_id, levels = rev(mouse_order))

ggplot(summary_df, aes(x = mouse_id, y = total, fill = role)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c(
    "times chased (submissive)" = "darkred",
    "times chasing (dominant)" = "steelblue"
  )) +
  labs(
    title = "How often each mouse is chased vs. chases others",
    x = "", y = "count", fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
