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
c1 <- c1 %>% arrange(value1)
c2 <- c2 %>% arrange(value1)
c3 <- c3 %>% arrange(value1)
c4 <- c4 %>% arrange(value1)
c5 <- c5 %>% arrange(value1)
# c6 <- c6 %>% arrange(value1)
c7 <- c7 %>% arrange(value1)
c8 <- c8 %>% arrange(value1)
c9 <- c9 %>% arrange(value1)
c10 <- c10 %>% arrange(value1)

cohort_lastints <- function(cohort = NULL) {
  last_cohort <- lastints(cohort[, 4:5])
  # for (i in 1:length(last_cohort)) {
  #   last_cohort[[i]] = last_cohort[[i]][!duplicated(last_cohort[[i]][, 4:5], fromLast = TRUE), ]
  # }
  return(last_cohort)
}


# makes all cohorts just the last interactions
c1 <- cohort_lastints(c1)
c2 <- cohort_lastints(c2)
c3 <- cohort_lastints(c3)
c4 <- cohort_lastints(c4)
c5 <- cohort_lastints(c5)
# c6 <- cohort_lastints(c6)
c7 <- cohort_lastints(c7)
c8 <- cohort_lastints(c8)
c9 <- cohort_lastints(c9)
c10 <- cohort_lastints(c10)


# creates a graph object for that cohort
cohort_graph <- function(cohort = NULL) {
  graph_cohort <- list()
  for (i in 1:length(cohort)) {
    graph_cohort[[i]] <- graph_from_edgelist(as.matrix(cohort[[i]]), directed = TRUE)
  }
  return(graph_cohort)
}



# set_layout <- function(cohort = NULL) {
  # g <- cohort_graph(cohort)[[length(cohort)]]
  # last <- cohort[[length(cohort)]]
  # l = layout_in_circle(g, order = sort(V(g)))
  # return(l)
# }

# plot(graph_from_edgelist(as.matrix(last_cohort[[length(last_cohort)]]), directed = TRUE), layout = l, edge.curved = 0)
# rolling[[length(last_cohort)]]


# gets the triad census state for what number of the column it's in
triad_state <- function(state = NULL) {
  if (state == 1) {
    name = "003"
  }
  if (state == 2) {
    name = "012"
  }
  if (state == 3) {
    name = "102"
  }
  if (state == 4) {
    name = "021D"
  }
  if (state == 5) {
    name = "021U"
  }
  if (state == 6) {
    name = "021C"
  }
  if (state == 7) {
    name = "111D"
  }
  if (state == 8) {
    name = "111U"
  }
  if (state == 9) {
    name = "030T"
  }
  if (state == 10) {
    name = "030C"
  }
  if (state == 11) {
    name = "201"
  }
  if (state == 12) {
    name = "120D"
  }
  if (state == 13) {
    name = "120U"
  }
  if (state == 14) {
    name = "120C"
  }
  if (state == 15) {
    name = "210"
  }
  if (state == 16) {
    name = "300"
  }
  return(name)
}





# layout is still not sorting in order :(
# par(mfrow=c(2,2), mar=c(0,0,0,0)) # plot four figures - 2 rows, 2 columns
# plot_four <- function(cohort = NULL) {
  # g <- cohort_graph(cohort)
  # plot(g[[round(0.25 * length(cohort))]], layout = l, edge.curved = 0)
  # plot(g[[round(0.50 * length(cohort))]], layout = l, edge.curved = 0)
  # plot(g[[round(0.75 * length(cohort))]], layout = l, edge.curved = 0)
  # plot(g[[length(cohort)]], layout = l, edge.curved = 0)
# }

# plot(graph_from_edgelist(as.matrix(cohort[[round(0.25 * length(cohort))]]), directed = TRUE), layout = l, edge.curved = 0)
# plot(graph_from_edgelist(as.matrix(cohort[[round(0.5 * length(cohort))]]), directed = TRUE), layout = l, edge.curved = 0)
# plot(graph_from_edgelist(as.matrix(cohort[[round(0.75 * length(cohort))]]), directed = TRUE), layout = l, edge.curved = 0)
# plot(graph_from_edgelist(as.matrix(cohort[[length(cohort)]]), directed = TRUE), layout = l, edge.curved = 0)


# returns triad census for that cohort for every time
rolling_cohort <- function(cohort = NULL) {
  rolling = list()
  rolling_graph <- cohort_graph(cohort)
  for (i in 1:(length(cohort))) {
    rolling[[i]] <- triad_census(rolling_graph[[i]])
  }
  return(rolling)
}

# adjust the number so it plots it for [[4]], [[5]], and [[9]]
# state <- 9 # the descending number of triad_census code


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
    ylab(paste0("Triad Census: ", triad_state(state)))
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


# rolling_censusX = list()
# for (m in 1:(length(rolling_g))) {
#   rolling_censusX[[m]] = triad_census(rolling_plots[[m]])
# }

new_combn <- function(cohort = NULL, group_size = NULL){
  individuals <- unique(unlist(cohort[length(cohort)], use.names = FALSE))
  all_combos <- combn(individuals, group_size, simplify = FALSE)
  return(all_combos)
}


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


# input: cohort name, triad id number
# returns as a list: the triad, igraph objects of the triad, and the triad censuses
# different from triad_sub_time because it shows for all interactions
triad_sub_all <- function(cohort = NULL, triad = NULL) {
  g = cohort_graph(cohort)
  ids <- new_combn(cohort, 3)
  sg = rep(list(NA),length(cohort))
  sg_census = rep(list(NA),length(cohort))
  for (i in 1:length(cohort)){
    try(sg[[i]] <- induced_subgraph(g[[i]], vids = ids[triad][[1]]), silent = TRUE)
    try(sg_census[[i]] <- triad_census(sg[[i]]), silent = TRUE)
  }
  return(list(ids[[triad]], sg, sg_census))
}




# WIP for recording changes between igraph objects
# figure out how to compare igraph objects? != doesnt work
# it's supposed to look at the igraph for each interaction and record it in a
# new list if it's different from the previous interaction
triad_changes <- function(cohort = NULL, triad = NULL) {
  changes = NULL
  data = triad_sub_all(cohort, triad)
  most_recent = 0
  count = 1
  for (i in 1:10) {
    if (most_recent != data[[2]][[i]]) {
      changes[count] <- data[[2]][[i]]
      count <- count + 1
    }
  }
  return(changes)
}










