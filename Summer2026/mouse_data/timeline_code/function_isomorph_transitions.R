# load libraries
library(tidyverse)
library(igraph)
library(hierformR)
library(compete)

# function - outputs a dataframe
analyze_lastint_isoclass <- function(cohort, graph_file = "rfid_data/nodes6_g.RDS", start_row = 25) {
  d <- readRDS(graph_file)
  
  check_isomorphism <- function(g, graph_list) {
    match <- which(unlist(lapply(graph_list, function(x) igraph::graph.isomorphic(g, x))))
    return(ifelse(length(match) > 0, match, NA))
  }
  
  cohort <- cohort %>%
    dplyr::arrange(value1)
  
  edgelists <- hierformR::lastints(cohort[, 4:5])
  
  classes <- rep(NA, length(edgelists))
  
  for (i in seq.int(start_row, length(edgelists))) {
    g <- igraph::graph_from_data_frame(edgelists[[i]], directed = TRUE)
    classes[i] <- check_isomorphism(g, d)
  }
  
  class_df <- data.frame(isomorph_class = classes)
  class_df <- class_df %>% dplyr::filter(!is.na(isomorph_class))
  
  return(class_df)
}
