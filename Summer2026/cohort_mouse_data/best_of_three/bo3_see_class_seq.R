# -------using bo3_get_class_seq.R------------
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

bot_dfs <- vector("list", length(c_list))

for (i in seq_along(c_list)) {
  bot_dfs[[i]] <- get_class_seq_bot(c_list[[i]])
}

# -------------------cohort 1----------------------
ggplot(bot_dfs[[1]], aes(x = 1:nrow(bot_dfs[[1]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 1 Best of 3 Isomorph Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 2----------------------
ggplot(bot_dfs[[2]], aes(x = 1:nrow(bot_dfs[[2]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 2 Best of 3 Isomorph Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 3----------------------
ggplot(bot_dfs[[3]], aes(x = 1:nrow(bot_dfs[[3]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 3 Best of 3 Isomorph Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 4----------------------
ggplot(bot_dfs[[4]], aes(x = 1:nrow(bot_dfs[[4]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 4 Best of 3 Isomorph Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 5----------------------
ggplot(bot_dfs[[5]], aes(x = 1:nrow(bot_dfs[[5]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 5 Best of 3 Isomorph Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 7----------------------
ggplot(bot_dfs[[6]], aes(x = 1:nrow(bot_dfs[[6]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 7 Best of 3 Isomorph Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 8----------------------
ggplot(bot_dfs[[7]], aes(x = 1:nrow(bot_dfs[[7]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 8 Best of 3 Isomorph Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 9----------------------
ggplot(bot_dfs[[8]], aes(x = 1:nrow(bot_dfs[[8]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 9 Best of 3 Isomorph Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 10----------------------
ggplot(bot_dfs[[9]], aes(x = 1:nrow(bot_dfs[[9]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 10 Best of 3 Isomorph Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

