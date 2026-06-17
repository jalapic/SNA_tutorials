# load libraries
library(tidyverse)

# -------------------using new function----------------------
c1 <- readr::read_csv("Summer2026/mouse_data/cohort1.csv")
c2 <- readr::read_csv("Summer2026/mouse_data/cohort2.csv")
c3 <- readr::read_csv("Summer2026/mouse_data/cohort3.csv")
c4 <- readr::read_csv("Summer2026/mouse_data/cohort4.csv")
c5 <- readr::read_csv("Summer2026/mouse_data/cohort5.csv")
c7 <- readr::read_csv("Summer2026/mouse_data/cohort7.csv")
c8 <- readr::read_csv("Summer2026/mouse_data/cohort8.csv")
c9 <- readr::read_csv("Summer2026/mouse_data/cohort9.csv")
c10 <- readr::read_csv("Summer2026/mouse_data/cohort10.csv")

c_list <- list(c1, c2, c3, c4, c5, c7, c8, c9, c10)

c_dfs <- vector("list", length(c_list))

for (i in seq_along(c_list)) {
  c_dfs[[i]] <- analyze_lastint_isoclass(c_list[[i]])
}

# -------------------cohort 1----------------------
ggplot(c_dfs[[1]], aes(x = 1:nrow(c_dfs[[1]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 1 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 2----------------------
ggplot(c_dfs[[2]], aes(x = 1:nrow(c_dfs[[2]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 2 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 3----------------------
ggplot(c_dfs[[3]], aes(x = 1:nrow(c_dfs[[3]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 3 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 4----------------------
ggplot(c_dfs[[4]], aes(x = 1:nrow(c_dfs[[4]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 4 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 5----------------------
ggplot(c_dfs[[5]], aes(x = 1:nrow(c_dfs[[5]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 5 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 7----------------------
ggplot(c_dfs[[6]], aes(x = 1:nrow(c_dfs[[6]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 7 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 8----------------------
ggplot(c_dfs[[7]], aes(x = 1:nrow(c_dfs[[7]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 8 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 9----------------------
ggplot(c_dfs[[8]], aes(x = 1:nrow(c_dfs[[8]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 9 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 10----------------------
ggplot(c_dfs[[9]], aes(x = 1:nrow(c_dfs[[9]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 10 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

