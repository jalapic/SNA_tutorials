# triad census

# tubetrans =  the tube they went through
# value1 = start time going through tube
# value2 = end time going through tube
# vector1 = winner/actor - ie mouse that did the chasing
# vector2 = loser/recipient - i.e. mouse that got chased
library(igraph)
# just the actors and recipients: c7[, c("vector1","vector2")] 
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

# transitive: 030T [9]
# on its way to being transitive: 021U [5] and 021D [4]
# intransitive: 030C [10]

# import
c1 <- read_csv("rfid_data/cohort1.csv",
               col_types = cols(vector1 = col_character(), 
                                vector2 = col_character()))
c2 <- read_csv("rfid_data/cohort2.csv",
               col_types = cols(vector1 = col_character(), 
                                vector2 = col_character()))
c3 <- read_csv("rfid_data/cohort3.csv",
               col_types = cols(vector1 = col_character(), 
                                vector2 = col_character()))
c4 <- read_csv("rfid_data/cohort4.csv",
               col_types = cols(vector1 = col_character(), 
                                vector2 = col_character()))
c5 <- read_csv("rfid_data/cohort5.csv",
               col_types = cols(vector1 = col_character(), 
                                vector2 = col_character()))
# c6 <- read_csv("rfid_data/cohort6.csv",
# col_types = cols(vector1 = col_character(), 
#                 vector2 = col_character()))
c7 <- read_csv("rfid_data/cohort7.csv",
               col_types = cols(vector1 = col_character(), 
                                vector2 = col_character()))
c8 <- read_csv("rfid_data/cohort8.csv",
               col_types = cols(vector1 = col_character(), 
                                vector2 = col_character()))
c9 <- read_csv("rfid_data/cohort9.csv",
               col_types = cols(vector1 = col_character(), 
                                vector2 = col_character()))
c10 <- read_csv("rfid_data/cohort10.csv",
                col_types = cols(vector1 = col_character(), 
                                 vector2 = col_character()))
#make sure data is ordered by time (value 1 column)
c1_og <- c1 %>% arrange(value1)
c2_og <- c2 %>% arrange(value1)
c3_og <- c3 %>% arrange(value1)
c4_og <- c4 %>% arrange(value1)
c5_og <- c5 %>% arrange(value1)
# c6_og <- c6 %>% arrange(value1)
c7_og <- c7 %>% arrange(value1)
c8_og <- c8 %>% arrange(value1)
c9_og <- c9 %>% arrange(value1)
c10_og <- c10 %>% arrange(value1)

cohort_lastints <- function(cohort = NULL) {
  last_cohort <- lastints(cohort[, 4:5])
  return(last_cohort)
}



# makes all cohorts just the last interactions
c1 <- cohort_lastints(c1_og)
c2 <- cohort_lastints(c2_og)
c3 <- cohort_lastints(c3_og)
c4 <- cohort_lastints(c4_og)
c5 <- cohort_lastints(c5_og)
# c6 <- cohort_lastints(c6_og)
c7 <- cohort_lastints(c7_og)
c8 <- cohort_lastints(c8_og)
c9 <- cohort_lastints(c9_og)
c10 <- cohort_lastints(c10_og)


best_order <- function(cohort_og = NULL) {
  mat <- get_wl_matrix(as.data.frame(cohort_og[,4:5]))
  besto <- isi13(mat)$best_order
  return(besto)
}


# creates a igraph object for that cohort
cohort_graph <- function(cohort = NULL) {
  graph_cohort <- list()
  for (i in 1:length(cohort)) {
    graph_cohort[[i]] <- graph_from_edgelist(as.matrix(cohort[[i]]), directed = TRUE)
  }
  return(graph_cohort)
}


# gets the triad census state for what number of the column it's in
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




# returns triad census for that cohort for every time
rolling_cohort <- function(cohort = NULL) {
  rolling = list()
  rolling_graph <- cohort_graph(cohort)
  for (i in 1:(length(cohort))) {
    rolling[[i]] <- triad_census(rolling_graph[[i]])
  }
  return(rolling)
}


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


# plots the change in proportion across time for 021D, 021U, and 030T
plot_all <- function(cohort = NULL, cohort_number = NULL) {
  plot_4 <- plot_triad(cohort, 4) + ggtitle(paste0("Rolling Triad - Cohort ", cohort_number))
  plot_5 <- plot_triad(cohort, 5) + ggtitle(paste0("Rolling Triad - Cohort ", cohort_number))
  plot_9 <- plot_triad(cohort, 9) + ggtitle(paste0("Rolling Triad - Cohort ", cohort_number))
  return(list(plot_4, plot_5, plot_9))
}


# plots proportion of 021D, 021U, and 030T across time for all cohorts
plot_all(c1, 1)
plot_all(c2, 2)
plot_all(c3, 3)
plot_all(c4, 4)
plot_all(c5, 5)
# plot_all(c6, 6)
plot_all(c7, 7)
plot_all(c8, 8)
plot_all(c9, 9)
plot_all(c10, 10)


new_combn <- function(cohort = NULL, group_size = NULL){
  individuals <- unique(unlist(cohort[length(cohort)], use.names = FALSE))
  all_combos <- combn(individuals, group_size, simplify = FALSE)
  return(all_combos)
}

# returns the last interactions for a triad for each time point
triad_lastints <- function(cohort = NULL, triad = NULL) {
  data <- NULL
  triad_ids <- new_combn(cohort, 3)
  triad <- triad_ids[triad][[1]]
  for (i in 1:length(cohort)) {
    data[[i]] <- as.data.frame(cohort[[i]])
  }
  new_data <- rep(list(NA),length(cohort))
  for (j in 1:length(cohort)) {
    row_num = 1
    for (k in 1:nrow(data[[j]])){
      if (data[[j]][k,1] %in% triad) {
        if (data[[j]][k,2] %in% triad) {
          if (row_num == 1) {
            new_data[[j]] <- data[[j]][k,]
            row_num = 2
          }
          else {
            new_data[[j]] <- rbind(new_data[[j]],data[[j]][k,])
          }
        }
      }
    }
  }
  return(new_data)
}


# creates a igraph object for that cohort
triad_graph <- function(cohort = NULL, triad = NULL) {
  data = triad_lastints(cohort, triad)
  graph_cohort <- rep(list(NA),length(cohort))
  for (i in 1:length(cohort)) {
    try(graph_cohort[[i]] <- graph_from_edgelist(as.matrix(data[[i]]), directed = TRUE), silent = TRUE)
  }
  return(graph_cohort)
}



# input: cohort name, triad id number, time
# returns the triad, an igraph object of the triad at that point of time, and the triad census
triad_sub_time <- function(cohort = NULL, triad = NULL, time = NULL) {
  g = cohort_graph(cohort)
  ids <- new_combn(cohort, 3)
  sg = triad_graph(cohort, triad)[[time]]
  sg_census = NA
  try(sg_census <- triad_census(sg), silent = TRUE)
  return(list(ids[[triad]], sg, sg_census))
}

# example: triad_sub_time(cohort = c10, triad = 4, time = 888)



# input: cohort name, triad id number
# returns as a list: the triad, igraph objects of the triad, and the triad censuses
# different from triad_sub_time because it shows for all interactions
triad_sub_all <- function(cohort = NULL, triad = NULL) {
  g = cohort_graph(cohort)
  ids <- new_combn(cohort, 3)
  sg = triad_graph(cohort, triad)
  sg_census = rep(list(NA),length(cohort))
  for (i in 1:length(cohort)){
    try(sg_census[[i]] <- triad_census(sg[[i]]), silent = TRUE)
  }
  return(list(ids[[triad]], sg, sg_census))
}




# returns a list of the number of edges for each time
triad_edge_count <- function(cohort = NULL, triad = NULL) {
  data <- triad_sub_all(cohort, triad)
  e <- rep(list(NA),length(cohort))
  for (i in 1:length(cohort)) {
    try(e[[i]] <- gsize(data[[2]][[i]]), silent = TRUE)
  }
  return(e)
}





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




# returns the timecodes based on the cohort number
cohort_time <- function(cohort_number = NULL) {
  time = NULL
  if (cohort_number == 1) {
    time = c1_og[, 2:3]
  }
  if (cohort_number == 2) {
    time = c2_og[, 2:3]
  }
  if (cohort_number == 3) {
    time = c3_og[, 2:3]
  }
  if (cohort_number == 4) {
    time = c4_og[, 2:3]
  }
  if (cohort_number == 5) {
    time = c5_og[, 2:3]
  }
  if (cohort_number == 6) {
    time = c6_og[, 2:3]
  }
  if (cohort_number == 7) {
    time = c7_og[, 2:3]
  }
  if (cohort_number == 8) {
    time = c8_og[, 2:3]
  }
  if (cohort_number == 9) {
    time = c9_og[, 2:3]
  }
  if (cohort_number == 10) {
    time = c10_og[, 2:3]
  }
  return(time)
}



# creates a new matrix for subtriad that includes the times, igraph objects,
# triad census, number of edges, the state id, and transitivity
triad_matrix <- function(cohort_number = NULL, cohort = NULL, triad = NULL) {
  ids <- new_combn(cohort, 3)
  tri_mat <- as.matrix(triad_sub_all(cohort, triad)[[2]]) # igraph
  tri_mat <- cbind(tri_mat, as.matrix(triad_sub_all(cohort, triad)[[3]])) # triad census
  tri_mat <- cbind(cohort_time(cohort_number), tri_mat) # timecodes
  tri_mat <- cbind(tri_mat, as.matrix(triad_edge_count(cohort, triad))) # edge count
  tri_mat <- cbind(tri_mat, as.matrix(triad_state_trans(cohort, triad)[[1]])) # triad state
  tri_mat <- cbind(tri_mat, as.matrix(triad_state_trans(cohort, triad)[[2]])) # transitive or intransitive
  tri_mat <- cbind(as.matrix(rep(list(ids[[triad]]),length(cohort))), tri_mat)
  colnames(tri_mat) <- c("Triad", "Enter_Time", "Exit_Time", "IGraph", "Triad_Census", "Edge_Count", "ID", "Transitivity")
  return(tri_mat) # it's going to give an error message, but it still works if you assign it to a name
}




# returns the average time to complete triads for each vertex, the number of the
# last interaction for each vertex, and the different times each vertex was completed
triad_complete <- function(cohort = NULL, cohort_og = NULL) {
  triad_ids <- new_combn(c1, 3)
  outres <- NULL
  besto <- best_order(c1_og)
  for(i in 1:20){
    outres[[i]] <- which(unlist(triad_edge_count(c1, i))==3)[1]
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


c1_complete <- triad_complete(c1, c1_og)
c2_complete <- triad_complete(c2, c2_og)
c3_complete <- triad_complete(c3, c3_og)
c4_complete <- triad_complete(c4, c4_og)
c5_complete <- triad_complete(c5, c5_og)
# c6_complete <- triad_complete(c6, c6_og)
c7_complete <- triad_complete(c7, c7_og)
c8_complete <- triad_complete(c8, c8_og)
c9_complete <- triad_complete(c9, c9_og)
c10_complete <- triad_complete(c10, c10_og)



# returns a matrix of each time a new interaction happened in the triad
triad_changes <- function(cohort_number = NULL, cohort = NULL, triad = NULL) {
  tri_ints = triad_lastints(c1, 1)
  data = triad_matrix(1, c1, 1)
  most_recent = 0
  attempt = 1
  while (is.na(tri_ints[[attempt]][[1]]) == TRUE) {
    attempt = attempt + 1
  }
  most_recent <- tri_ints[[attempt]]
  changes <- attempt
  for (i in attempt:length(c1)) {
    if (!(identical(sort(most_recent[[1]]), sort(tri_ints[[i]][[1]])))) {
      if (!(identical(sort(most_recent[[2]]), sort(tri_ints[[i]][[2]])))) {
        most_recent <- tri_ints[[i]]
        changes <- append(changes, i)
      }
    }
  }
  new <- as.matrix(data[c(changes),])
  return(new)
}


























# Everything below is still work-in-progress


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














