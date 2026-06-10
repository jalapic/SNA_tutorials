### Loading in an example RFID dataset

library(tidyverse)
library(igraph)

c7 <- read_csv("Summer2026/mouse_data/cohort7.csv")

head(c7)

#make sure data is ordered by time (value 1 column)
c7 <- c7 %>% arrange(value1)

# Data are now in time ordered sequence.
head(c7)


# the winner loser columns are #vector1 (winner) and #vector2 (loser)

table(c7$vector1)
table(c7$vector2)

# install if needed
# devtools::install_github('jalapic/compete')



# this just shows that there is a hierarchy
mat <- compete::get_wl_matrix(as.matrix(c7[,4:5]))
mat
isi.res <- compete::isi13(mat)
isi.res
compete::get_di_matrix(mat)[isi.res$best_order,isi.res$best_order]


head(c7)



### Get last interactions

# devtools::install_github("jalapic/hierformR") 


library(hierformR)

# returns the last interactions between nodes
cohort_lastints <- function(cohort = NULL) {
  last_cohort <- lastints(cohort[, 4:5])
  return(last_cohort)
}


# makes all cohorts just the last interactions


results <- cohort_lastints(c7)   # this would be for the whole dataset

results <- tibble(results)

nrow(results)

# cohort_lastints(c7[1:25,]) # creates last int. dataset for rows 1-25
results$results[25]

# for example....

# Loop through each nested tibble and convert it to a graph
graphs <- lapply(results$results, function(df) {
  graph_from_data_frame(df, directed = TRUE)
})

# Access your separate graphs by index
graphs[[25]] # Graph from row 25

graphs <- tibble(graphs)

nrow(graphs)

#is.igraph(
graphs$graphs[[148]]
#)

classes <- NULL

for(i in 25:nrow(graphs)) {
  classes[i] <- check_isomorphism(graphs$graphs[[i]], d)
  }

classes
