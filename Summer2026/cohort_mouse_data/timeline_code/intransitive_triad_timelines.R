# load libraries
library(tidyverse)

# -------using function_isomorph_transitions.R------------
c1 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort1.csv")
c2 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort2.csv")
c3 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort3.csv")
c4 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort4.csv")
c5 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort5.csv")
c7 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort7.csv")
c8 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort8.csv")
c9 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort9.csv")
c10 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort10.csv")

c_list <- list(c1, c2, c3, c4, c5, c7, c8, c9, c10)

c_dfs <- vector("list", length(c_list))

for (i in seq_along(c_list)) {
  c_dfs[[i]] <- get_class_seq(c_list[[i]])
}

# add column matching isomorph class with intransitive triad count
# uses df6 from Summer2026/code/tasks/tables/isomorph-tables_3-6nodes.R
for (i in seq_along(c_dfs)) {
  c_dfs[[i]] <- c_dfs[[i]] %>% 
    left_join(
      df6 %>% select(Tournament, Intransitive_Triads),
      by = c("isomorph_class" = "Tournament")
    )
}

# c_dfs[[5]]

# -------------------cohort 1----------------------
ggplot(c_dfs[[1]], aes(x = 1:nrow(c_dfs[[1]]), y = Intransitive_Triads)) +
  geom_point(size = .5) +
  ggtitle('Cohort 1 Intransitive Triad Timeline') +
  xlab('Nth Interaction') +
  ylab('Intransitive Triad Number') +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme(panel.grid.minor.y = element_blank())

# -------------------cohort 2----------------------
ggplot(c_dfs[[2]], aes(x = 1:nrow(c_dfs[[2]]), y = Intransitive_Triads)) +
  geom_point(size = .5) +
  ggtitle('Cohort 2 Intransitive Triad Timeline') +
  xlab('Nth Interaction') +
  ylab('Intransitive Triad Number') +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme(panel.grid.minor.y = element_blank())

# -------------------cohort 3----------------------
ggplot(c_dfs[[3]], aes(x = 1:nrow(c_dfs[[3]]), y = Intransitive_Triads)) +
  geom_point(size = .5) +
  ggtitle('Cohort 3 Intransitive Triad Timeline') +
  xlab('Nth Interaction') +
  ylab('Intransitive Triad Number') +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme(panel.grid.minor.y = element_blank())

# -------------------cohort 4----------------------
ggplot(c_dfs[[4]], aes(x = 1:nrow(c_dfs[[4]]), y = Intransitive_Triads)) +
  geom_point(size = .5) +
  ggtitle('Cohort 4 Intransitive Triad Timeline') +
  xlab('Nth Interaction') +
  ylab('Intransitive Triad Number') +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme(panel.grid.minor.y = element_blank())

# -------------------cohort 5----------------------
ggplot(c_dfs[[5]], aes(x = 1:nrow(c_dfs[[5]]), y = Intransitive_Triads)) +
  geom_point(size = .5) +
  ggtitle('Cohort 5 Intransitive Triad Timeline') +
  xlab('Nth Interaction') +
  ylab('Intransitive Triad Number') +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme(panel.grid.minor.y = element_blank())

# -------------------cohort 7----------------------
ggplot(c_dfs[[6]], aes(x = 1:nrow(c_dfs[[6]]), y = Intransitive_Triads)) +
  geom_point(size = .5) +
  ggtitle('Cohort 7 Intransitive Triad Timeline') +
  xlab('Nth Interaction') +
  ylab('Intransitive Triad Number') +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme(panel.grid.minor.y = element_blank())

# -------------------cohort 8----------------------
ggplot(c_dfs[[7]], aes(x = 1:nrow(c_dfs[[7]]), y = Intransitive_Triads)) +
  geom_point(size = .5) +
  ggtitle('Cohort 8 Intransitive Triad Timeline') +
  xlab('Nth Interaction') +
  ylab('Intransitive Triad Number') +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme(panel.grid.minor.y = element_blank())

# -------------------cohort 9----------------------
ggplot(c_dfs[[8]], aes(x = 1:nrow(c_dfs[[8]]), y = Intransitive_Triads)) +
  geom_point(size = .5) +
  ggtitle('Cohort 9 Intransitive Triad Timeline') +
  xlab('Nth Interaction') +
  ylab('Intransitive Triad Number') +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme(panel.grid.minor.y = element_blank())

# -------------------cohort 10----------------------
ggplot(c_dfs[[9]], aes(x = 1:nrow(c_dfs[[9]]), y = Intransitive_Triads)) +
  geom_point(size = .5) +
  ggtitle('Cohort 10 Intransitive Triad Timeline') +
  xlab('Nth Interaction') +
  ylab('Intransitive Triad Number') +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme(panel.grid.minor.y = element_blank())

