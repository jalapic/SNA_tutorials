# Load required package
library(igraph)
library(tidyverse)

# Function to generate all labeled tournament graphs for n=6
generate_tournament_graphs <- function(n) {
  all_tournaments <- list()  # Store labeled tournament graphs
  node_pairs <- combn(1:n, 2)  # Get all unique pairs of nodes
  num_edges <- ncol(node_pairs)  # Total edges to direct
  
  # Generate all possible edge direction assignments (2^15 for n=6)
  for (i in 0:(2^num_edges - 1)) {
    edge_list <- matrix(ncol=2, nrow=num_edges)
    binary <- intToBits(i)[1:num_edges]  # Convert index to binary
    
    for (j in 1:num_edges) {
      if (binary[j] == 1) {
        edge_list[j, ] <- c(node_pairs[, j][1], node_pairs[, j][2])
      } else {
        edge_list[j, ] <- c(node_pairs[, j][2], node_pairs[, j][1])
      }
    }
    
    G <- graph_from_edgelist(edge_list, directed=TRUE)
    all_tournaments <- append(all_tournaments, list(G))
  }
  
  return(all_tournaments)
}

# Function to filter out isomorphic tournaments and get 56 non-isomorphic graphs
get_non_isomorphic_tournaments <- function(tournaments) {
  unique_tournaments <- list()
  
  for (G in tournaments) {
    is_new <- TRUE
    
    # Check if this graph is isomorphic to any already stored graphs
    for (H in unique_tournaments) {
      if (graph.isomorphic(G, H)) {
        is_new <- FALSE
        break
      }
    }
    
    # If it's a new unique structure, add it to the list
    if (is_new) {
      unique_tournaments <- append(unique_tournaments, list(G))
    }
  }
  
  return(unique_tournaments)
}

# Generate all tournaments for n=6
all_tournaments <- generate_tournament_graphs(6)

# Filter to get 56 non-isomorphic tournament graphs
unique_tournaments <- get_non_isomorphic_tournaments(all_tournaments)

# Print the number of non-isomorphic tournaments
print(paste("Number of non-isomorphic tournaments:", length(unique_tournaments)))

getwd()
#saveRDS(unique_tournaments,"tournaments_6nodes.RData")

# make it shorter
l <- unique_tournaments
l


# Load package
library(dplyr)  # For easier data manipulation

# Function to calculate score sequence (sorted out-degree counts)
get_score_sequence <- function(graph) {
  return(sort(degree(graph, mode="out")))
}

# Function to count transitive and intransitive triads
count_triads <- function(graph) {
  nodes <- V(graph)
  triads <- combn(nodes, 3)  # Generate all triads
  transitive_count <- 0
  intransitive_count <- 0
  intransitive_triads <- list()  # Store intransitive triad out-degrees
  
  for (i in 1:ncol(triads)) {
    triad <- triads[, i]
    a <- triad[1]
    b <- triad[2]
    c <- triad[3]
    
    # Check transitivity conditions
    if ((are_adjacent(graph, a, b) && are_adjacent(graph, b, c) && are_adjacent(graph, a, c)) ||
        (are_adjacent(graph, a, c) && are_adjacent(graph, c, b) && are_adjacent(graph, a, b)) ||
        (are_adjacent(graph, b, a) && are_adjacent(graph, a, c) && are_adjacent(graph, b, c)) ||
        (are_adjacent(graph, b, c) && are_adjacent(graph, c, a) && are_adjacent(graph, b, a)) ||
        (are_adjacent(graph, c, a) && are_adjacent(graph, a, b) && are_adjacent(graph, c, b)) ||
        (are_adjacent(graph, c, b) && are_adjacent(graph, b, a) && are_adjacent(graph, c, a))) {
      transitive_count <- transitive_count + 1
    } else if ((are_adjacent(graph, a, b) && are_adjacent(graph, b, c) && !are_adjacent(graph, a, c)) ||
               (are_adjacent(graph, a, c) && are_adjacent(graph, c, b) && !are_adjacent(graph, a, b)) ||
               (are_adjacent(graph, b, a) && are_adjacent(graph, a, c) && !are_adjacent(graph, b, c)) ||
               (are_adjacent(graph, b, c) && are_adjacent(graph, c, a) && !are_adjacent(graph, b, a)) ||
               (are_adjacent(graph, c, a) && are_adjacent(graph, a, b) && !are_adjacent(graph, c, b)) ||
               (are_adjacent(graph, c, b) && are_adjacent(graph, b, a) && !are_adjacent(graph, c, a))) {
      intransitive_count <- intransitive_count + 1
      
      # Get out-degrees and store sorted
      degrees <- sort(degree(graph, mode="out")[c(a, b, c)])
      intransitive_triads <- append(intransitive_triads, list(degrees))
    }
  }
  
  return(list(
    transitive_count = transitive_count,
    intransitive_count = intransitive_count,
    intransitive_triads = intransitive_triads
  ))
}

# Compute properties for all 56 tournaments
tournament_data <- data.frame()

for (i in 1:length(l)) {
  G <- l[[i]]  # Get the ith tournament graph
  
  # Get score sequence
  score_seq <- get_score_sequence(G)
  
  # Get transitive and intransitive triad counts
  triad_data <- count_triads(G)
  
  # Store in data frame
  tournament_data <- rbind(tournament_data, data.frame(
    Tournament = i,
    Score_Sequence = paste(score_seq, collapse=","),
    Transitive_Triads = triad_data$transitive_count,
    Intransitive_Triads = triad_data$intransitive_count,
    Intransitive_Out_Degrees = paste(sapply(triad_data$intransitive_triads, function(x) paste(x, collapse=",")), collapse=" | ")
  ))
}

# Sort rows by Score_Sequence
tournament_data <- tournament_data %>% arrange(Score_Sequence)

# Assign a unique identifier for each score sequence
tournament_data <- tournament_data %>%
  mutate(Score_ID = as.integer(as.factor(Score_Sequence)))  # Convert unique sequences to IDs

# Reorder the tournament numbering 1:56
tournament_data$Tournament <- 1:nrow(tournament_data)

# Reorder `l` based on the sorted tournament order
l <- l[order(match(tournament_data$Score_Sequence, unique(tournament_data$Score_Sequence)))]

# Print the sorted table
tournament_data

#saveRDS(l,"tournaments_6nodes.RData")


