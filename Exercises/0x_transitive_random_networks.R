### CALCULATE TRANSITIVITY EXERCISE


#' Create a dataset of interactions (2 columns) where the first column is actor and second recipient. 
#' This should be 200 rows. 
#'
#' Then, create a directed network graph and plot it. The direction of the edge is from the actor to the recipient. 
#'
#'
#' Next, only keep the last edge between any pair of individuals. 
#'
#' Then, calculate a triad census. Then determine what proportion of triads are transitive.
#'
#' 
#'
#' Create a dataset of interactions (200)
#' Only keep the last interaction between any 2 individuals (e.g. if BA was last interaction, only keep that, no other BA or AB)
#'Create a directed network graph from the dataset.
#'Plot the graph.
#'Calculate the triad census.
#'Determine the proportion of transitive triads.
#'
#' EXTENSIONS:
#' 
#' 1. Repeat this 1000 times.  Store the results. What is the average proportion of transitive triads?
#' 
#' 2. For one network, calculate the rolling proportion of transitive triads after every interaction. Plot the results.