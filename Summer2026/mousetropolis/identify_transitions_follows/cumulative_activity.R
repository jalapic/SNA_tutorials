library(dplyr)
library(ggplot2)

exclude_ids <- c(900133000459667, 900263002586581, 900263002586595)

all_true <- readRDS("Summer2026/mousetropolis/data/all_true_crossings.rds") %>% 
  filter(!mouse_id %in% exclude_ids)

ids <- read.csv("Summer2026/mousetropolis/data/mousemetRD1_ids.csv", 
                colClasses = c("character", "character"))

all_true <- all_true %>%
  left_join(ids, by = c("mouse_id" = "TagID"))

cumulative_activity <- all_true %>%
  mutate(
    dt   = as.POSIXct(start_datetimestamp, format = "%d.%m.%Y %H:%M:%OS"),
    date = as.Date(dt, tz = "America/Chicago")
  ) %>%
  filter(!(date == as.Date("2026-05-02") & as.integer(format(dt, "%H")) >= 18)) %>%
  arrange(TempID, dt) %>%
  group_by(TempID) %>%
  mutate(cumulative_transitions = row_number()) %>%
  ungroup()

ggplot(cumulative_activity, aes(x = dt, y = cumulative_transitions, 
                                color = factor(TempID))) +
  geom_line() +
  labs(
    title = "Cumulative Transitions Over Time Per Mouse",
    x = "Time",
    y = "Cumulative Transitions",
    color = "Mouse"
  ) +
  theme_bw()
