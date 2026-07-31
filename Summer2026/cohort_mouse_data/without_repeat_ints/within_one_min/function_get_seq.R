library(tidyverse)
library(igraph)
library(hierformR)
library(compete)

get_clean_seq2 <- function(cohort,
                          graph_file = "rfid_data/nodes6_g.RDS",
                          start_row = 25,
                          repeat_window_ms = 60000) {
  d <- readRDS(graph_file)
  
  check_isomorphism <- function(g, graph_list) {
    match <- which(vapply(
      graph_list,
      function(x) igraph::isomorphic(g, x),
      logical(1)
    ))
    if (length(match) > 0) match[1] else NA_integer_
  }
  
  cohort <- cohort %>%
    dplyr::arrange(value1)
  
  cohort_filtered <- cohort %>%
    dplyr::mutate(
      .keep_row = dplyr::if_else(
        dplyr::row_number() == 1,
        TRUE,
        !(
          .data[[names(cohort)[4]]] == dplyr::lag(.data[[names(cohort)[4]]]) &
            .data[[names(cohort)[5]]] == dplyr::lag(.data[[names(cohort)[5]]]) &
            (.data[["value1"]] - dplyr::lag(.data[["value1"]])) <= repeat_window_ms
        )
      )
    ) %>%
    dplyr::filter(.keep_row) %>%
    dplyr::select(-.keep_row)
  
  num_rows_del <- nrow(cohort) - nrow(cohort_filtered)
  
  edgelists <- hierformR::lastints(cohort_filtered[, 4:5])
  
  classes <- rep(NA_integer_, length(edgelists))
  
  for (i in seq.int(start_row, length(edgelists))) {
    g <- igraph::graph_from_data_frame(edgelists[[i]], directed = TRUE)
    classes[i] <- check_isomorphism(g, d)
  }
  
  class_df <- data.frame(isomorph_class = classes) %>%
    dplyr::filter(!is.na(isomorph_class))
  
  attr(class_df, "num_rows_del") <- num_rows_del
  
  return(class_df)
}

# -------make list of dataframes------------
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

clean_dfs2 <- vector("list", length(c_list))

for (i in seq_along(c_list)) {
  clean_dfs2[[i]] <- get_clean_seq2(c_list[[i]])
}