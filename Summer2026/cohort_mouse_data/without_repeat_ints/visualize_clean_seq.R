# -------using function_get_clean_seq.R------------
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

clean_dfs <- vector("list", length(c_list))

for (i in seq_along(c_list)) {
  clean_dfs[[i]] <- get_clean_seq(c_list[[i]])
}

# strange discrepancy here...
nrow(c_dfs[[7]]) - nrow(clean_dfs[[7]])
attr(clean_dfs[[7]], "num_rows_del")

# -------------------cohort 1----------------------
ggplot(clean_dfs[[1]], aes(x = 1:nrow(clean_dfs[[1]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 1 Cleaned Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 2----------------------
ggplot(clean_dfs[[2]], aes(x = 1:nrow(clean_dfs[[2]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 2 Cleaned Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 3----------------------
ggplot(clean_dfs[[3]], aes(x = 1:nrow(clean_dfs[[3]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 3 Cleaned Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 4----------------------
ggplot(clean_dfs[[4]], aes(x = 1:nrow(clean_dfs[[4]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 4 Cleaned Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 5----------------------
ggplot(clean_dfs[[5]], aes(x = 1:nrow(clean_dfs[[5]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 5 Cleaned Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 7----------------------
ggplot(clean_dfs[[6]], aes(x = 1:nrow(clean_dfs[[6]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 7 Cleaned Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 8----------------------
ggplot(clean_dfs[[7]], aes(x = 1:nrow(clean_dfs[[7]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 8 Cleaned Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 9----------------------
ggplot(clean_dfs[[8]], aes(x = 1:nrow(clean_dfs[[8]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 9 Cleaned Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

# -------------------cohort 10----------------------
ggplot(clean_dfs[[9]], aes(x = 1:nrow(clean_dfs[[9]]), y = isomorph_class)) +
  geom_point(size = .5) +
  ggtitle('Cohort 10 Cleaned Sequence') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))

