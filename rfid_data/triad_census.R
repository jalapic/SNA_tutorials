# triad census

# tubetrans =  the tube they went through
# value1 = start time going through tube
# value2 = end time going through tube
# vector1 = winner/actor - ie mouse that did the chasing
# vector2 = loser/recipient - i.e. mouse that got chased
library(igraph)
# just the actors and recipients: c7[, c("vector1","vector2")] 
library(hierformR)

# transitive: 030T [9]
# on its way to being transitive: 021U [5] and 021D [4]
# intransitive: 030C [10]

# import
library(tidyverse)
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


cohort <- c7



g <- graph_from_edgelist(as.matrix(cohort[, c("vector1","vector2")]), directed = TRUE)
l = layout_in_circle(g, order = V(g))
plot(g, layout = l, edge.curved = 0)


last_cohort <- lastints(cohort[, 4:5])
# for (i in 1:length(last_cohort)) {
#   last_cohort[[i]] = last_cohort[[i]][!duplicated(last_cohort[[i]][, 4:5], fromLast = TRUE), ]
# }




plot_cohort <- list()
# last_cohort[[i]][, c("vector1","vector2")]
for (i in 1:length(last_cohort)) {
  plot(graph_from_edgelist(as.matrix(last_cohort[[i]]), directed = TRUE), layout = l, edge.curved = 0)
}

rolling = list()
for (m in 1:length(last_cohort)) {
  rolling[[m]] = triad_census(graph_from_edgelist(as.matrix(last_cohort[[m]][, c("vector1","vector2")]), directed = TRUE))
}
rolling[[length(last_cohort)]]






