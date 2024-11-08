library(igraph)
library(hierformR)
library(network) 
library(sna)
library(ggraph)
library(visNetwork)
library(threejs)
library(networkD3)
library(ndtv)
library(tidyverse)
library(compete)

ago_data <- read.csv("ago_data.csv", sep=";")
ago_data$Winner <- as.character(ago_data$Winner)
ago_data$Loser <- as.character(ago_data$Loser)

group_1 <- ago_data %>% filter(Group == 1) %>% arrange(Date)
group_2 <- ago_data %>% filter(Group == 2) %>% arrange(Date)
group_3 <- ago_data %>% filter(Group == 3) %>% arrange(Date)
group_4 <- ago_data %>% filter(Group == 4) %>% arrange(Date)
group_5 <- ago_data %>% filter(Group == 5) %>% arrange(Date)


cohort_lastints <- function(cohort = NULL, winner_column = 3, loser_column = 4) {
  last_cohort <- lastints(cohort[, c(winner_column, loser_column)])
  return(last_cohort)
}

g1 <- cohort_lastints(group_1)
g2 <- cohort_lastints(group_2)
g3 <- cohort_lastints(group_3)
g4 <- cohort_lastints(group_4)
g5 <- cohort_lastints(group_5)



best_order <- function(cohort_og = NULL, winner_column = 3, loser_column = 4) {
  mat <- get_wl_matrix(as.data.frame(cohort_og[,c(winner_column, loser_column)]))
  besto <- isi13(mat)$best_order
  return(besto)
}

besto_g1 <- best_order(group_1)


# creates a igraph object for that cohort
cohort_graph <- function(cohort = NULL) {
  graph_cohort <- list()
  for (i in 1:length(cohort)) {
    graph_cohort[[i]] <- graph_from_edgelist(as.matrix(cohort[[i]]), directed = TRUE)
  }
  return(graph_cohort)
}

triad_state <- function(state = NULL) {
  name = NULL
  trans = NULL
  if (state == 1) {
    name = "003"
    trans = NA
  }
  if (state == 2) {
    name = "012"
    trans = NA
  }
  if (state == 3) {
    name = "102"
    trans = NA
  }
  if (state == 4) {
    name = "021D"
    trans = NA
  }
  if (state == 5) {
    name = "021U"
    trans = NA
  }
  if (state == 6) {
    name = "021C"
    trans = "Intransitive"
  }
  if (state == 7) {
    name = "111D"
    trans = "Intransitive"
  }
  if (state == 8) {
    name = "111U"
    trans = "Intransitive"
  }
  if (state == 9) {
    name = "030T"
    trans = "Transitive"
  }
  if (state == 10) {
    name = "030C"
    trans = "Intransitive"
  }
  if (state == 11) {
    name = "201"
    trans = "Intransitive"
  }
  if (state == 12) {
    name = "120D"
    trans = "Transitive"
  }
  if (state == 13) {
    name = "120U"
    trans = "Transitive"
  }
  if (state == 14) {
    name = "120C"
    trans = "Mixed"
  }
  if (state == 15) {
    name = "210"
    trans = "Mixed"
  }
  if (state == 16) {
    name = "300"
    trans = "Transitive"
  }
  return(list(name, trans))
}


g1_graphs <- cohort_graph(g1)



'''

set_layout <- function(cohort = NULL) {
  g <- cohort_graph(cohort)[[length(cohort)]]
  last <- cohort[[length(cohort)]]
  l = layout_in_circle(g, order = sort(V(g)))
  return(l)
}

l = set_layout(g1)

plot(g1_graphs[[4118]], layout = l, vertex.size = 1, edge.arrow.size = .1, vertex.label = NA, edge.color = "#00aeff50")

"#00aeff50"


plot(g1_graphs[[4000]], layout = l, vertex.color = "black",
     vertex.size = 1, vertex.frame.color = "black", vertex.frame.width = 1.25,
     edge.color = "#00aeff50", edge.arrow.size = .2, vertex.label.color = "black")

'''






rolling_cohort <- function(cohort = NULL) {
  rolling = list()
  rolling_graph <- cohort_graph(cohort)
  for (i in 1:(length(cohort))) {
    rolling[[i]] <- triad_census(rolling_graph[[i]])
  }
  return(rolling)
}


rolling_g1 <- rolling_cohort(g1)



# plots the proportion that a triad state appears across time for that cohort
plot_triad <- function(cohort = NULL, state = NULL) {
  cohort_triad = rolling_cohort(cohort)
  g <- cohort_graph(cohort)
  rolling_census = list()
  for (i in 1:(length(cohort))) {
    rolling_census[[i]] <- cohort_triad[[i]][state]/sum(cohort_triad[[i]])
  }
  time_vs_census <- data.frame(
    timeline = c(1:length(cohort)),
    census = unlist(rolling_census))
  
  new_plot <- ggplot(time_vs_census, aes(timeline, census)) + geom_line() + xlab("Interaction") +
    ylab(paste0("Triad Census: ", triad_state(state)[1]))
  return(new_plot)
}


plot_all <- function(cohort = NULL, cohort_number = NULL) {
  plot_4 <- plot_triad(cohort, 4) + ggtitle(paste0("Rolling Triad - Cohort ", cohort_number))
  plot_5 <- plot_triad(cohort, 5) + ggtitle(paste0("Rolling Triad - Cohort ", cohort_number))
  plot_9 <- plot_triad(cohort, 9) + ggtitle(paste0("Rolling Triad - Cohort ", cohort_number))
  return(list(plot_4, plot_5, plot_9))
}






# plots proportion of 021D, 021U, and 030T across time for all cohorts
plot_all(g1, 1)
plot_all(g2, 2)
plot_all(g3, 3)
plot_all(g4, 4)
plot_all(g5, 5)



new_combn <- function(cohort = NULL, group_size = NULL){
  individuals <- unique(unlist(cohort[length(cohort)], use.names = FALSE))
  all_combos <- combn(individuals, group_size, simplify = FALSE)
  return(all_combos)
}


# example
group_1_all_triads <- new_combn(g1, 3)




# input: cohort name, triad id number, time
# returns the triad, an igraph object of the triad at that point of time, and the triad census
triad_sub_time <- function(cohort = NULL, triad = NULL, time = NULL) {
  g = cohort_graph(cohort)
  ids <- new_combn(cohort, 3)
  sg = NA
  sg_census = NA
  try(sg <- induced_subgraph(g[[time]], vids = ids[triad][[1]]), silent = TRUE)
  try(sg_census <- triad_census(sg), silent = TRUE)
  return(list(ids[[triad]], sg, sg_census))
}


# example
g1_tri_1_time_1000 <- triad_sub_time(cohort = g1, triad = 1, time = 1000)
# if it returns with no subgraph or subgraph census, that means that triad hasn't interacted yet



# input: cohort name, triad id number
# returns as a list: the triad, igraph objects of the triad, and the triad censuses
# different from triad_sub_time because it shows for all interactions
triad_sub_all <- function(cohort = NULL, triad = NULL) {
  g = cohort_graph(cohort)
  ids <- new_combn(cohort, 3)
  sg = rep(list(NA), length(cohort))
  sg_census = rep(list(NA), length(cohort))
  for (i in 1:length(cohort)){
    try(sg[[i]] <- induced_subgraph(g[[i]], vids = ids[triad][[1]]), silent = TRUE)
    try(sg_census[[i]] <- triad_census(sg[[i]]), silent = TRUE)
  }
  return(list(ids[[triad]], sg, sg_census))
}

# example
g1_subgraphs <- triad_sub_all(group = g1, triad = 1)



triad_edge_count <- function(cohort = NULL, triad = NULL) {
  data <- triad_sub_all(cohort, triad)
  e <- rep(list(NA),length(cohort))
  for (i in 1:length(cohort)) {
    try(e[[i]] <- gsize(data[[2]][[i]]), silent = TRUE)
  }
  return(e)
}

# example
g1_edge_count <- triad_edge_count(group = g1, triad = 1)





# returns the id for each state at every time
triad_state_trans <- function(cohort = NULL, triad = NULL) {
  data <- triad_sub_all(cohort, triad)
  state <- rep(list(NA),length(cohort))
  trans <- rep(list(NA),length(cohort))
  for (i in 1:length(cohort)) {
    for (j in 1:10) {
      try(if (data[[3]][[i]][[j]] == 1) {
        state[[i]] = triad_state(j)[[1]]
      }, silent = TRUE)
      try(if (data[[3]][[i]][[j]] == 1) {
        trans[[i]] = triad_state(j)[[2]]
      }, silent = TRUE)
    }
  }
  return(list(state, trans))
}

# triad_state_trans(cohort = c10, triad = 4)
# triad_state_trans(cohort = c7, triad = 12)


# returns the timecodes based on the cohort number
cohort_time <- function(cohort, time_column = 1) {
  time = "Failed"
  try(if (identical(cohort, g1)) {
    time = group_1[, time_column]
  }, silent = TRUE)
  try(if (identical(cohort, g2)) {
    time = group_2[, time_column]
  }, silent = TRUE)
  try(if (identical(cohort, g3)) {
    time = group_3[, time_column]
  }, silent = TRUE)
  try(if (identical(cohort, g4)) {
    time = group_4[, time_column]
  }, silent = TRUE)
  try(if (identical(cohort, g5)) {
    time = group_5[, time_column]
  }, silent = TRUE)
  return(time)
}




# creates a new matrix for subtriad that includes the times, igraph objects,
# triad census, number of edges, the state id, and transitivity
triad_matrix <- function(cohort = NULL, triad = NULL) {
  igraph_and_census <- triad_sub_all(cohort, triad)
  state_trans <- triad_state_trans(cohort, triad)
  tri_mat <- as.matrix(igraph_and_census[[2]]) # igraph
  tri_mat <- cbind(tri_mat, as.matrix(igraph_and_census[[3]])) # triad census
  tri_mat <- cbind(cohort_time(cohort), tri_mat) # timecodes
  tri_mat <- cbind(tri_mat, as.matrix(triad_edge_count(cohort, triad))) # edge count
  tri_mat <- cbind(tri_mat, as.matrix(state_trans[[1]])) # triad state
  tri_mat <- cbind(tri_mat, as.matrix(state_trans[[2]])) # transitive or intransitive
  colnames(tri_mat) <- c(paste("Triad:", paste0(new_combn(cohort, 3)[[triad]], collapse = "-"), "Date"), "IGraph", "Triad_Census", "Edge_Count", "ID", "Transitivity")
  return(tri_mat) # it's going to give an error message if you run it by itself, but it still works if you assign it to an object
}



# example
g1_mat <- triad_matrix(cohort = g1, triad = 4)






## as.numeric(gsub("c","","c10"))
## as.numeric(gsub("\\D","","c10"))

# Example: tmat.c10 <- triad_matrix(cohort_number = 10, cohort = c10, triad = 4)
# Example: tmat.c10[60,3]


# takes forever, not sure if there's something wrong with the code or if it's just because there's so many triads
# returns the average time to complete triads for each vertex, the number of the
# last interaction for each vertex, and the different times each vertex was completed
triad_complete <- function(cohort = NULL) {
  cohort_og = "Failed"
  try(if (identical(cohort, g1)) {
    cohort_og = group_1
  }, silent = TRUE)
  try(if (identical(cohort, g2)) {
    cohort_og = group_2
  }, silent = TRUE)
  try(if (identical(cohort, g3)) {
    cohort_og = group_3
  }, silent = TRUE)
  try(if (identical(cohort, g4)) {
    cohort_og = group_4
  }, silent = TRUE)
  try(if (identical(cohort, g5)) {
    cohort_og = group_5
  }, silent = TRUE)
  triad_ids <- new_combn(cohort, 3)
  outres <- NULL
  besto <- best_order(cohort_og)
  for(i in 1:20){
    outres[[i]] <- which(unlist(triad_edge_count(cohort, i))==3)[1]
  }
  edge3.df <- data.frame(id = unlist(triad_ids),
                         time = rep(unlist(outres), each = 3)
  )
  edge3.df$rank <- match(edge3.df$id, besto)
  # head(edge3.df)
  
  # average time for each triad to complete by rank
  avg_time <- edge3.df %>%
    group_by(rank,id) %>%
    summarize(time = mean(time)) %>%
    as.data.frame()
  
  # last time for all triads to complete by rank
  complete_time <- edge3.df %>%
    group_by(rank,id) %>%
    summarize(time = max(time)) %>%
    as.data.frame()
  
  
  # determine how quickly each finishes their first, second,... etc. triad
  
  quick <- edge3.df %>%
    group_by(id, rank) %>%
    arrange(rank,time) %>%
    mutate(triadno = row_number()) %>%
    as.data.frame()
  
  finish_time <- NULL
  first = 1
  last = 0
  for (i in 1:length(quick$id)) {
    if (quick$id[i] == quick$id[1]) {
      last = last + 1
    }
  }
  len = last
  for (i in 1:max(quick$rank)) {
    finish_time[[i]] <- quick[c(first:last),]
    first = first + len
    last = last + len
  }
  
  return(list(avg_time, complete_time, finish_time))
}

# ex: triad_complete(c1, c1_og)

out <- triad_complete(g1)

out




### Checking if graphs are the same.

out1 <- triad_matrix(g1, 1)

out1[40,3]
out1[43,3]

identical_graphs(out1[[40,3]],out1[[43,3]])

V(out1[[40,3]])
V(out1[[43,3]])

E(out1[[40,3]])
E(out1[[43,3]])

E(out1[[40,3]])==E(out1[[43,3]])
E(out1[[340,3]])==E(out1[[343,3]])


E(out1[[340,3]])
E(out1[[563,3]])

E(out1[[340,3]])==E(out1[[563,3]])


E(out1[[34,3]])
E(out1[[563,3]])

E(out1[[34,3]])==E(out1[[563,3]])

sum((E(out1[[34,3]])==E(out1[[563,3]]))==FALSE) #whenever above 0, graphs are not identical.

#
out1[[343,3]]
out1[[663,3]]

E(out1[[343,3]])==E(out1[[663,3]])

E(out1[[343,3]])






# Everything below is still work-in-progress


# WIP for recording changes between igraph objects
# figure out how to compare igraph objects? != doesnt work
# it's supposed to look at the igraph for each interaction and record it in a
# new list if it's different from the previous interaction
triad_changes <- function(cohort_number = NULL, cohort = NULL, triad = NULL) {
  changes = "NULL"
  data = triad_matrix(cohort_number, cohort, triad)
  most_recent = 0
  count = 1
  attempt = 1
  while (most_recent == 0) {
    try(most_recent <- data[[attempt,3]])
    attempt = attempt + 1
  }
  for (i in 1:10) {
    if (!(identical_graphs(most_recent, data[[i,3]]))) {
      changes[count] <- data[[i,]]
      count <- count + 1
    }
  }
  return(changes)
}



data = triad_matrix(1, c1, 1)
most_recent = 0
attempt = 1
while (is.na(data[[attempt,3]])[1] == TRUE) {
  attempt = attempt + 1
}
most_recent <- data[[attempt,3]]
changes <- attempt
for (i in attempt:50) {
  if (!(identical_graphs(most_recent, data[[i,3]]))) {
    most_recent <- data[[i,3]]
    changes <- append(changes, i)
  }
}




if (!(identical_graphs(most_recent, data[[i,3]]))) {
  changes[count] <- data[[i,]]
  count <- count + 1
}

edge_count <- function(cohort = NULL) {
  data = cohort_graph(cohort)
  e <- rep(list(NA),length(cohort))
  for (i in 1:length(cohort)) {
    try(e[[i]] <- gsize(data[[i]]), silent = TRUE)
  }
  return(e)
}


# WIP - find a way to return how many of each state
state_trans <- function(cohort = NULL) {
  indiv <- length(unique(unlist(cohort[length(cohort)], use.names = FALSE)))
  combination <- factorial(indiv) / ((factorial(3) * (factorial(indiv - 3))))
  all_state = rep(list(NA),length(cohort))
  
  
  return
}



# for (i in 1:combination) {
# s_t <- triad_state_trans(cohort, i)
# for (j in 1:length(s_t[[1]])) {
# }
# }



# WIP - add state and transitivity
# shows as a matrix the entire cohort's data
all_matrix <- function(cohort_number = NULL, cohort = NULL) {
  data = cohort_graph(cohort)
  triad = NULL
  for (i in 1:length(cohort)) {
    triad[[i]] <- triad_census(data[[i]])
  }
  all_mat <- cohort_time(cohort_number) # timecodes
  all_mat <- cbind(all_mat, as.matrix(data)) # igraph
  all_mat <- cbind(all_mat, as.matrix(triad)) # triad census
  all_mat <- cbind(all_mat, as.matrix(edge_count(cohort))) # edge count
  colnames(all_mat) <- c("Enter_Time", "Exit_Time", "IGraph", "Triad_Census", "Edge_Count")
  return(all_mat)
}



data = cohort_graph(c1)
# ids <- unique(unlist(c1[length(c1)], use.names = FALSE))
# g = rep(list(NA), length(c1))
# for (i in 1:length(c1)){
# try(g[[i]] <- induced_subgraph(data[[i]], vids = ids), silent = TRUE)
# }
triad = NULL
for (i in 1:length(c1)) {
  triad[[i]] <- triad_census(data[[i]])
}
all_mat <- as.matrix(cohort_time(1)) # timecodes
all_mat <- cbind(all_mat, as.matrix(data)) # igraph
all_mat <- cbind(all_mat, as.matrix(triad)) # triad census
all_mat <- cbind(all_mat, as.matrix(edge_count(c1))) # edge count
return(all_mat)