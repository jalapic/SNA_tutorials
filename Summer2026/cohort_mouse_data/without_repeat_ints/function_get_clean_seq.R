# load libraries
library(tidyverse)
library(igraph)
library(hierformR)
library(compete)

# function - outputs dataframe class_df with attribute "num_rows_del"
get_clean_seq <- function(cohort,
                          graph_file = "rfid_data/nodes6_g.RDS",
                          start_row = 25) {
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
        !(.data[[names(cohort)[4]]] == dplyr::lag(.data[[names(cohort)[4]]]) &
            .data[[names(cohort)[5]]] == dplyr::lag(.data[[names(cohort)[5]]]))
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