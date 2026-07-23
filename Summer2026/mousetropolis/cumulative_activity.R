library(dplyr)
library(ggplot2)

all_true <- readRDS("Summer2026/mousetropolis/data/all_true_crossings.rds")

cumulative_activity <- all_true %>%
  mutate(
    dt   = as.POSIXct(start_datetimestamp, format = "%d.%m.%Y %H:%M:%OS"),
    date = as.Date(dt, tz = "America/Chicago")
  ) %>%
  filter(!(date == as.Date("2026-05-02") & as.integer(format(dt, "%H")) >= 18)) %>%
  arrange(mouse_id, dt) %>%
  group_by(mouse_id) %>%
  mutate(cumulative_transitions = row_number()) %>%
  ungroup()

ggplot(cumulative_activity, aes(x = dt, y = cumulative_transitions, 
                                color = factor(mouse_id))) +
  geom_line() +
  labs(
    title = "Cumulative Transitions Over Time Per Mouse",
    x = "Time",
    y = "Cumulative Transitions",
    color = "Mouse"
  ) +
  theme_bw()
