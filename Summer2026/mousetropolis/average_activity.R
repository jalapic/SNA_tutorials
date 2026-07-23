library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

all_true <- readRDS("Summer2026/mousetropolis/data/all_true_crossings.rds")

# average daily activity per mouse
daily_activity <- all_true %>%
  mutate(
    dt   = as.POSIXct(start_datetimestamp, format = "%d.%m.%Y %H:%M:%OS"),
    hour = as.integer(format(dt, "%H")),
    date = as.Date(dt, tz = "America/Chicago")
  ) %>%
  filter(!(date == as.Date("2026-05-02") & hour >= 18)) %>%
  group_by(mouse_id, date, hour) %>%
  summarise(transitions = n(), .groups = "drop") %>%
  complete(mouse_id, date, hour = 0:23, fill = list(transitions = 0)) %>%
  group_by(mouse_id, hour) %>%
  summarise(avg_transitions = mean(transitions), .groups = "drop")

mouse_ids <- unique(daily_activity$mouse_id)

for (mid in mouse_ids) {
  mouse_data <- daily_activity %>% filter(mouse_id == mid)
  
  p <- ggplot(mouse_data, aes(x = hour, y = avg_transitions)) +
    geom_line(color = "steelblue") +
    geom_point(size = 1.5, color = "steelblue") +
    scale_x_continuous(
      breaks = seq(0, 23, by = 3),
      labels = paste0(seq(0, 23, by = 3), "h")
    ) +
    labs(
      title = paste("Average Activity Per Day — Mouse", mid),
      x = "Hour of Day",
      y = "Average Transitions per Day"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )
  
  print(p)
}

# average weekly activity per mouse
weekly_pattern <- all_true %>%
  mutate(
    dt   = as.POSIXct(start_datetimestamp, format = "%d.%m.%Y %H:%M:%OS"),
    date = as.Date(dt, tz = "America/Chicago"),
    hour = hour(dt),
    weekday = wday(date, label = TRUE, week_start = 1)
  ) %>%
  filter(!(date == as.Date("2026-05-02") & hour >= 18)) %>%
  group_by(mouse_id, date, weekday, hour) %>%
  summarise(transitions = n(), .groups = "drop") %>%
  complete(mouse_id, date, weekday, hour = 0:23, fill = list(transitions = 0)) %>%
  group_by(mouse_id, weekday, hour) %>%
  summarise(avg_transitions = mean(transitions), .groups = "drop") %>%
  mutate(weekday = factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

mouse_ids <- unique(weekly_pattern$mouse_id)

for (mid in mouse_ids) {
  mouse_data <- weekly_pattern %>% filter(mouse_id == mid)
  
  p <- ggplot(mouse_data, aes(x = hour, y = avg_transitions)) +
    geom_line(color = "steelblue") +
    geom_point(size = 1.2, color = "steelblue") +
    facet_wrap(~weekday, nrow = 1) +
    scale_x_continuous(
      breaks = seq(0, 23, by = 3),
      labels = paste0(seq(0, 23, by = 3), "h")
    ) +
    labs(
      title = paste("Average Week of Activity — Mouse", mid),
      x = "Hour of Day",
      y = "Average Transitions per Day"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      strip.text = element_text(size = 8)
    )
  
  print(p)
}