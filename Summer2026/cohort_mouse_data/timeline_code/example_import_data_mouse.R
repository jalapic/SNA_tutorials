library(tidyverse)
library(igraph)
library(hierformR)
library(compete)

### Loading in an example RFID dataset

cohort <- read_csv("Summer2026/mouse_data/cohort7.csv")

head(cohort)

#make sure data is ordered by time (value 1 column)
cohort <- cohort %>% arrange(value1)

# Data are now in time ordered sequence.
head(cohort)

# the winner loser columns are #vector1 (winner) and #vector2 (loser)
table(cohort$vector1)
table(cohort$vector2)

# install if needed
# devtools::install_github('jalapic/compete')

# this just shows that there is a hierarchy
#mat <- compete::get_wl_matrix(as.matrix(cohort[,4:5]))
#mat
#isi.res <- compete::isi13(mat)
#isi.res
#compete::get_di_matrix(mat)[isi.res$best_order,isi.res$best_order]

#head(cohort)


### Get last interactions

# devtools::install_github("jalapic/hierformR") 
#library(hierformR)

# returns the last interactions between nodes
cohort_lastints <- function(cohort = NULL) {
  last_cohort <- lastints(cohort[, 4:5])
  return(last_cohort)
}


# makes all cohorts just the last interactions
edgelists <- cohort_lastints(cohort)   # this is by row, not whole dataset

edgelists <- tibble(edgelists)

nrow(edgelists)

# cohort_lastints(cohort[1:25,]) # creates last int. dataset for rows 1-25
edgelists$edgelists[25]

# for example....

# loops through each nested tibble and converts it to a graph
graphs <- lapply(edgelists$edgelists, function(df) {
  graph_from_data_frame(df, directed = TRUE)
})

# accesses separate graphs by index
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

# turn list into dataframe and remove NA datapoints
df_classes <- tibble(classes)

View(df_classes)

df_classes <- df_classes %>% filter(!is.na(classes))

# graph timeline of network states
ggplot(df_classes, aes(x = 1:nrow(df_classes), y = classes)) +
  geom_point(size = .5) +
  ggtitle('Cohort 7 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))