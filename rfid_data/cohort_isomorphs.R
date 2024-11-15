# connecting the cohort to the isomorph id
# tubetrans =  the tube they went through
# value1 = start time going through tube
# value2 = end time going through tube
# vector1 = winner/actor - ie mouse that did the chasing
# vector2 = loser/recipient - i.e. mouse that got chased
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

# ids of the isomorphs
cohort_ids <- list("20",
            "19a",
            "19b",
            "19c",
            "19d",
            "18a",
            "18b",
            "18c",
            "18d",
            "17a",
            "17b",
            "16a",
            "16b",
            "16c",
            "15a",
            "15b",
            "15c",
            "15d",
            "14",
            "13a",
            "13b",
            "12")

# number of wins per isomorph
cohort_wins <- as.matrix(list(c(0, 1, 2, 3, 4, 5),
         c(0, 1, 2, 4, 4, 4),
         c(0, 1, 3, 3, 3, 5),
         c(0, 2, 2, 2, 4, 5),
         c(1, 1, 1, 3, 4, 5),
         c(0, 1, 3, 3, 4, 4),
         c(0, 2, 2, 3, 3, 5),
         c(1, 1, 2, 2, 4, 5),
         c(1, 1, 1, 4, 4, 4),
         c(0, 2, 2, 3, 4, 4),
         c(1, 1, 2, 3, 3, 5),
         c(0, 2, 3, 3, 3, 4),
         c(1, 1, 2, 3, 4, 4),
         c(1, 2, 2, 2, 3, 5),
         c(0, 3, 3, 3, 3, 3),
         c(1, 1, 3, 3, 3, 4),
         c(1, 2, 2, 2, 4, 4),
         c(2, 2, 2, 2, 2, 5),
         c(1, 2, 2, 3, 3, 4),
         c(1, 2, 3, 3, 3, 3),
         c(2, 2, 2, 2, 3, 4),
         c(2, 2, 2, 3, 3, 3)))


cohort_iso_mat <- cbind(as.matrix(cohort_ids), cohort_wins)
colnames(cohort_iso_mat) <- c("ID", "Wins")


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


# returns the last interactions between nodes
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


# accepts the cohort and the number interaction
# outputs the isomorph state the interaction is in
# only works after all nodes are connected
find_id <- function(cohort = NULL, cohort_time = NULL) {
  num <- c(0, 0, 0, 0, 0, 0)
  for (j in 1:6) {
    if (!(is.na(as.data.frame(table(as.numeric(unlist(cohort[[cohort_time]][,1]))))[j,2]))) {
      num[[j]] <- as.data.frame(table(as.numeric(unlist(cohort[[cohort_time]][,1]))))[j,2]
    }
  }
  num <- sort(num)
  num_id = NULL
  for (i in 1:nrow(cohort_iso_mat)) {
    if (setequal(cohort_iso_mat[[i,2]], num)) {
      num_id = cohort_iso_mat[[i,1]]
    }
  }
  return(num_id)
}


# outputs a list of isomorph ids by interaction for that cohort
ids_time <- function(cohort = NULL) {
  data <- rep(list(NA), length(cohort))
  for (i in 1:length(cohort)) {
    if (nrow(cohort[[i]]) == 15) {
      data[[i]] <- find_id(cohort, i)
    }
  }
  return(data)
}


iso_order <- list()
iso_order_num <- 1
not_iso_order <- list()
not_iso_order_num <- 1
for (i in 1:nrow(cohort_iso_mat)) {
  for (j in 1:nrow(cohort_iso_mat)) {
    # returns a list of possible id changes if one pair flips
    if (identical(sort(cohort_iso_mat[[i,2]] - cohort_iso_mat[[j, 2]]), c(-1, 0, 0, 0, 0, 1))) {
      iso_order[[iso_order_num]] <- sort(c(cohort_iso_mat[[i,1]], cohort_iso_mat[[j,1]]), decreasing = TRUE)
      iso_order_num = iso_order_num + 1
    }
    else if (identical(sort(cohort_iso_mat[[i,2]] - cohort_iso_mat[[j, 2]]), c(0, 0, 0, 0, 0, 0))) {
      iso_order[[iso_order_num]] <- sort(c(cohort_iso_mat[[i,1]], cohort_iso_mat[[j,1]]), decreasing = TRUE)
      iso_order_num = iso_order_num + 1
    }
    # returns a list of impossible id changes if one pair flips
    else {
      not_iso_order[[not_iso_order_num]] <- sort(c(cohort_iso_mat[[i,1]], cohort_iso_mat[[j,1]]), decreasing = TRUE)
      not_iso_order_num = not_iso_order_num + 1
    }
  }
}
iso_order <- unique(iso_order)
not_iso_order <- unique(not_iso_order)


# returns Markov Transition Matrix
markov <- function(cohort) {
  ids_of_cohort <- ids_time(cohort)
  vec <- unlist(ids_of_cohort[!is.na(ids_of_cohort)])
  
  states <- unique(vec)
  n_states <- length(states)
  
  # Initialize transition matrix
  t_matrix <- matrix(0, nrow = length(cohort_ids), ncol = length(cohort_ids), 
                     dimnames = list(sort(unlist(cohort_ids), decreasing = TRUE), sort(unlist(cohort_ids), decreasing = TRUE)))
  
  # Count transitions
  for (i in 1:(length(vec) - 1)) {
    current_state <- vec[i]
    next_state <- vec[i + 1]
    t_matrix[current_state, next_state] <- t_matrix[current_state, next_state] + 1
  }
  
  # Normalize to get probabilities
  t_matrix <- t_matrix / rowSums(t_matrix)
  t_matrix <- round(t_matrix, 2)
  
  
  # makes NA if the state doesn't occur
  for (i in cohort_ids) {
    if (!(i %in% ids_of_cohort)) {
      t_matrix[i, ] <- NA
      t_matrix[, i] <- NA
    }
  }
  
  for (j in 1:length(not_iso_order)) {
    t_matrix[not_iso_order[[j]][1], not_iso_order[[j]][2]] <- NA
    t_matrix[not_iso_order[[j]][2], not_iso_order[[j]][1]] <- NA
  }
  
  
  return(t_matrix)
}



# markov transition matrix for each cohort
c1_markov <- markov(c1)
c2_markov <- markov(c2)
c3_markov <- markov(c3)
c4_markov <- markov(c4)
c5_markov <- markov(c5)
# c6_markov <- markov(c6)
c7_markov <- markov(c7)
c8_markov <- markov(c8)
c9_markov <- markov(c9)
c10_markov <- markov(c10)



# shows which isomorphs appear in each cohort
c1_isomorphs <- table(unlist(ids_time(c1)))
c2_isomorphs <- table(unlist(ids_time(c2)))
c3_isomorphs <- table(unlist(ids_time(c3)))
c4_isomorphs <- table(unlist(ids_time(c4)))
c5_isomorphs <- table(unlist(ids_time(c5)))
# c6_isomorphs <- table(unlist(ids_time(c6)))
c7_isomorphs <- table(unlist(ids_time(c7)))
c8_isomorphs <- table(unlist(ids_time(c8)))
c9_isomorphs <- table(unlist(ids_time(c9)))
c10_isomorphs <- table(unlist(ids_time(c10)))




