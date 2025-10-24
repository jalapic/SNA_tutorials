getwd()


d <- readRDS("nodes6_g.RDS")
df <- readRDS("nodes6_tournaments_df.RDS")
df

d[[56]]


### Function to make random 6 nodes tournament graph:

library(igraph)

generate_tournament_graph <- function(n = 6) {
  edges <- c()
  
  # Iterate over all pairs of nodes
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # Randomly decide direction of the edge
      if (runif(1) < 0.5) {
        edges <- c(edges, i, j)  # Edge from i -> j
      } else {
        edges <- c(edges, j, i)  # Edge from j -> i
      }
    }
  }
  
  # Create directed graph
  g <- graph(edges, directed = TRUE)
  
  # Plot the graph
  plot(g, 
       layout = layout_nicely(g),
       edge.arrow.size = 0.5, 
       vertex.size = 20, 
       vertex.label.cex = 0.7,
       vertex.color='dodgerblue')
  
  return(g)
}

# Generate and plot a random tournament graph
tg <- generate_tournament_graph()
tg

# check all 56 to find the one that is isomorphic
unlist(lapply(d, function(x) isomorphic(tg,x)))

which(unlist(lapply(d, function(x) isomorphic(tg,x)))==T)



### Flip edges


# Function to flip the direction of a randomly selected edge
flip_edge <- function(g) {
  edges <- as_edgelist(g)
  
  # Randomly choose an edge to flip
  flip_index <- sample(nrow(edges), 1)
  
  # Extract and reverse the chosen edge
  edge_to_flip <- edges[flip_index, ]
  new_edges <- rbind(edges[-flip_index, ], c(edge_to_flip[2], edge_to_flip[1]))
  
  # Create a new graph with the modified edge list
  return(graph_from_edgelist(new_edges, directed = TRUE))
}

# Function to check which isomorphism class a graph belongs to
check_isomorphism <- function(g, graph_list) {
  match <- which(unlist(lapply(graph_list, function(x) graph.isomorphic(g, x))))
  return(ifelse(length(match) > 0, match, NA))  # Return the index or NA if no match found
}

# Simulation function to track isomorphism changes
simulate_tournament_transitions <- function(N = 10, graph_list) {
  tg <- generate_tournament_graph()  # Generate initial tournament graph
  transitions <- numeric(N + 1)  # Store isomorphism classes
  
  # Record initial class
  transitions[1] <- check_isomorphism(tg, graph_list)
  
  # Flip edges N times and track changes
  for (i in 1:N) {
    tg <- flip_edge(tg)  # Flip an edge
    transitions[i + 1] <- check_isomorphism(tg, graph_list)  # Check new isomorphism class
  }
  
  return(transitions)
}

# Example usage
res <- simulate_tournament_transitions(N = 50000, d)
res


# Function to generate a transition matrix from a sequence
create_transition_matrix <- function(sequence, num_states = 56) {
  # Initialize an empty matrix with zeros
  transition_matrix <- matrix(0, nrow = num_states, ncol = num_states)
  
  # Iterate over the sequence and count transitions
  for (i in 1:(length(sequence) - 1)) {
    from <- sequence[i]
    to <- sequence[i + 1]
    transition_matrix[from, to] <- transition_matrix[from, to] + 1
  }
  
  return(transition_matrix)
}


# Create the transition matrix
tm <- create_transition_matrix(res, num_states = 56)
tm
diag(tm) # which isomorphism can transition to itself with an edge flip?


x <- graph_from_adjacency_matrix(tm,weighted=T) #610 edges/transitions (mathematically pleasing?)

plot(x, 
     layout=layout_nicely(x),
     vertex.color = adjustcolor('#123abc',alpha=.5),
     edge.arrow.size = .1,
     vertex.size=6,
     vertex.label.cex=.8,
     edge.curved = .4
     )



#### Check how many edges can flip to convert from self to self


library(igraph)

d


# Function to generate 15 new graphs, each with one edge flipped
generate_flipped_graphs <- function(original_graph) {
  edges <- as_edgelist(original_graph)  # Get the edge list
  flipped_graphs <- list()
  
  for (i in 1:nrow(edges)) {
    flipped_edges <- edges
    flipped_edges[i, ] <- rev(flipped_edges[i, ])  # Flip the direction of one edge
    
    # Create new graph with the flipped edge
    new_graph <- graph_from_edgelist(flipped_edges, directed = TRUE)
    flipped_graphs[[i]] <- new_graph
  }
  
  return(flipped_graphs)
}


fgs <- generate_flipped_graphs(d[[1]])
fgs

fgs
unlist(lapply(fgs, function(x) is_isomorphic_to(x,d[[1]])))
sum(unlist(lapply(fgs, function(x) is_isomorphic_to(x,d[[1]]))))

# graph 1 can be converted to graph 1 through 5 flips.


## do for all 56

totals <- numeric(56)
for(i in 1:56){
fgs <- generate_flipped_graphs(d[[i]])
totals[i] <- sum(unlist(lapply(fgs, function(x) is_isomorphic_to(x,d[[i]]))))
}

totals
diag(tm)
diag(tm)/totals


# which one do they flip to
unlist(lapply(fgs, function(y) which(unlist(lapply(d, function(x) isomorphic(y,x)))==T)))


mat <- matrix(0,nrow=56,ncol=15)
for(i in 1:56){
ff <- generate_flipped_graphs(d[[i]])
mat[i,] <- unlist(lapply(ff, function(y) which(unlist(lapply(d, function(x) isomorphic(y,x)))==T)))
}
mat



# Initialize a 56x56 transition matrix filled with 0s
tmat <- matrix(0, nrow = 56, ncol = 56)
tmat

write.table(tmat, row.names = F, col.names = F, quote = FALSE)


# Loop through each row to update the transition matrix
for (i in 1:nrow(mat)) {
  for (j in 1:ncol(mat)) {
    value <- mat[i, j]  # Get the target column index
    tmat[i, value] <- tmat[i, value] + 1  # Increment count
  }
}

# Convert to a data frame for better readability
tmat.df <- as.data.frame(tmat)
rownames(tmat.df) <- 1:56  # Set row names as 1 to 56
colnames(tmat.df) <- 1:56  # Set column names as 1 to 56

tmat.df


x # look at paths through the network

# could also create weighted one
gx <- graph_from_adjacency_matrix(tmat, weighted=T, mode='directed')
gx
diameter(gx)

# is it possible to visit every single node from every other node?




library(ggplot2)
library(reshape2)  # For melting matrix into long format

# Convert matrix into long format for ggplot
tmat_long <- melt(tmat)
colnames(tmat_long) <- c("Row", "Column", "Value")

# Create the tile plot
ggplot(tmat_long, aes(x = Column, y = Row, fill = factor(Value))) +
  geom_tile(color = "black") +  # Add tile borders
  scale_fill_manual(values = c("#FFFFFF",  # 0 (White)
                                        "#EAD8EC",  # 1 (Very Light Purple)
                                        "#D4B9DA",  # 2 (Light Purple)
                                        "#C994C7",  # 3 (Medium-Light Purple)
                                        "#DF65B0",  # 4 (Medium Purple)
                                        "#DD1C77",  # 5 (Dark Pinkish Purple)
                                        "#980043",  # 6 (Deep Dark Purple)
                                        "#67001F")  # 7 (Almost Black Purple)
  ) +
  labs(title = "56x56 Transition Matrix Heatmap",
       x = "Column",
       y = "Row",
       fill = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right") +
  scale_y_reverse(breaks=1:56) +
  scale_x_continuous(breaks=1:56)
