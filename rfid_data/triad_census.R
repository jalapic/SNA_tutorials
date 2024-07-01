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


cohort <- c1



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
plot(graph_from_edgelist(as.matrix(last_cohort[[length(last_cohort)]]), directed = TRUE), layout = l, edge.curved = 0)
rolling = list()
for (m in 1:length(last_cohort)) {
  rolling[[m]] = triad_census(graph_from_edgelist(as.matrix(last_cohort[[m]][, c("vector1","vector2")]), directed = TRUE))
}
rolling[[length(last_cohort)]]

par(mfrow=c(2,2), mar=c(0,0,0,0)) # plot four figures - 2 rows, 2 columns
plot(graph_from_edgelist(as.matrix(last_cohort[[round(0.25 * length(last_cohort))]]), directed = TRUE), layout = l, edge.curved = 0)
plot(graph_from_edgelist(as.matrix(last_cohort[[round(0.5 * length(last_cohort))]]), directed = TRUE), layout = l, edge.curved = 0)
plot(graph_from_edgelist(as.matrix(last_cohort[[round(0.75 * length(last_cohort))]]), directed = TRUE), layout = l, edge.curved = 0)
plot(graph_from_edgelist(as.matrix(last_cohort[[length(last_cohort)]]), directed = TRUE), layout = l, edge.curved = 0)





# adjust the number so it plots it for [[4]], [[5]], and [[9]]
state <- 9 # the descending number of triad_census code

rolling_g = list() #empty list to store the new dataframes

# iterate through each row and create new df with each new one added
for (j in 1:(length(last_cohort))) {
  rolling_g[[j]] = last_cohort[[j]]
}

rolling_plots = list()
for (k in 1:(length(rolling_g))) {
  rolling_plots[[k]] = graph_from_edgelist(as.matrix(last_cohort[[k]]), directed = TRUE)
}

rolling_census = list()
for (m in 1:(length(rolling_g))) {
  rolling_census[[m]] = triad_census(rolling_plots[[m]])[state]/sum(triad_census(rolling_plots[[m]]))
}

rolling_censusX = list()
for (m in 1:(length(rolling_g))) {
  rolling_censusX[[m]] = triad_census(rolling_plots[[m]])
}


time_vs_census <- data.frame(
  timeline = c(1:length(rolling_census)),
  census = unlist(rolling_census))

ggplot(time_vs_census, aes(timeline, census)) + geom_line() + xlab("Interaction") +
  ylab("Triad Census") + ggtitle("Rolling Triad")





rolling_g

rolling_plots

