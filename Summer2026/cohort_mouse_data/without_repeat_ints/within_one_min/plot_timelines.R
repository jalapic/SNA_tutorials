library(tidyverse)
library(ggplot2)
library(glue)

# ------------------- cohorts 1-5 ----------------------
for (i in 1:5){
  p <- ggplot(clean_dfs2[[i]], aes(x = 1:nrow(clean_dfs2[[i]]), y = isomorph_class)) +
    geom_point(size = .5) +
    labs(title = glue("Cohort {i} Time-Dependent Isomorph Sequence"),
         x = "Nth Interaction",
         y = "Isomorph Number") +
    theme_bw() +
    scale_y_continuous(minor_breaks = seq(1, 56, by = 1))
  
  print(p)
  }

# ------------------- cohorts 7-10 ----------------------
for (i in 6:9){
  p <- ggplot(clean_dfs2[[i]], aes(x = 1:nrow(clean_dfs2[[i]]), y = isomorph_class)) +
    geom_point(size = .5) +
    labs(title = glue("Cohort {i+1} Time-Dependent Isomorph Sequence"),
         x = "Nth Interaction",
         y = "Isomorph Number") +
    theme_bw() +
    scale_y_continuous(minor_breaks = seq(1, 56, by = 1))
  
  print(p)
  }

# add column matching isomorph class with intransitive triad count
# uses df6 from Summer2026/code/tasks/tables/isomorph-tables_3-6nodes.R
for (i in seq_along(clean_dfs2)) {
  clean_dfs2[[i]] <- clean_dfs2[[i]] %>% 
    left_join(
      df6 %>% select(Tournament, Intransitive_Triads),
      by = c("isomorph_class" = "Tournament")
    )
}

# clean_dfs2[[5]]

# ------------------- cohorts 1-5 ----------------------
for (i in 1:5){
  p <- ggplot(clean_dfs2[[i]], aes(x = 1:nrow(clean_dfs2[[i]]), y = Intransitive_Triads)) +
    geom_point(size = .5) +
    labs(title = glue("Cohort {i} Time-Dependent Intransitive Triad Sequence"),
         x = "Nth Interaction",
         y = "Intransitive Triad Number") +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 8, by = 1)) +
    theme(panel.grid.minor.y = element_blank())
  
  print(p)
}

# ------------------- cohorts 7-10 ----------------------
for (i in 6:9){
  p <- ggplot(clean_dfs2[[i]], aes(x = 1:nrow(clean_dfs2[[i]]), y = Intransitive_Triads)) +
    geom_point(size = .5) +
    labs(title = glue("Cohort {i+1} Time-Dependent Intransitive Triad Sequence"),
         x = "Nth Interaction",
         y = "Intransitive Triad Number") +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 8, by = 1)) +
    theme(panel.grid.minor.y = element_blank())
  
  print(p)
}
