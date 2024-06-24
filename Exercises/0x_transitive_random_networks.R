### CALCULATE TRANSITIVITY EXERCISE
library(igraph)
library(dplyr)
library(ggplot2)
# set.seed(15)

#' Create a dataset of interactions (2 columns) where the first column is actor and second recipient. 
#' This should be 200 rows. 
df <- as.data.frame(matrix(round(runif(n = 400, min = 1, max = 4), 0), nrow = 200))
colnames(df) = c("actor", "recipient")

#' Then, create a directed network graph and plot it. The direction of the edge is from the actor to the recipient. 
g <- graph_from_edgelist(as.matrix(df), directed = TRUE)
l = layout_in_circle(g)
plot(g, layout = l)


#' Next, only keep the last edge between any pair of individuals. 

last_df <- data.frame() # makes a new dataframe

for (i in 1:nrow(df)) {
  # runs if the last row - i + 1 is not already in the new dataframe
  # last row - i + 1 because i starts at 1
  x <- df[nrow(df) - i + 1, "actor"]
  y <- df[nrow(df) - i + 1, "recipient"]
  reverse <- as.data.frame(matrix(c(y,x), nrow = 1, ncol = 2, byrow = TRUE))
  if (!(any(apply(last_df, 1, function(row) all(row == df[nrow(df) - i + 1, ]))))) {
    # runs if the actor != recipient in the row
    # if (df[nrow(df) - i + 1, "actor"] != df[nrow(df) - i + 1, "recipient"]){
    if (x != y) {
      # if the two numbers aren't already in the df
      if (!(any(apply(last_df, 1, function(row) all(row == reverse))))) {
        last_df <- rbind(last_df, df[nrow(df) - i + 1, ])
      }
    }
  }
}
last_df <- last_df[nrow(last_df):1, ] # reverses the order of the dataframes

new_g <- graph_from_edgelist(as.matrix(last_df), directed = TRUE)

#' Then, calculate a triad census. Then determine what proportion of triads are transitive.
census <- triad_census(new_g)
transitive <- census[9]
census_total <- sum(census)
triad_proportion <- transitive/census_total





#' Create a dataset of interactions (200)

df2 <- as.data.frame(matrix(round(runif(n = 400, min = 1, max = 4), 0), nrow = 200))
colnames(df2) = c("actor", "recipient")

#' Only keep the last interaction between any 2 individuals (e.g. if BA was last interaction, only keep that, no other BA or AB)
last_df2 <- data.frame() # makes a new dataframe

for (i in 1:nrow(df2)) {
  # runs if the last row - i + 1 is not already in the new dataframe
  # last row - i + 1 because i starts at 1
  x2 <- df2[nrow(df2) - i + 1, "actor"]
  y2 <- df2[nrow(df2) - i + 1, "recipient"]
  reverse2 <- as.data.frame(matrix(c(y2,x2), nrow = 1, ncol = 2, byrow = TRUE))
  if (!(any(apply(last_df2, 1, function(row) all(row == df2[nrow(df2) - i + 1, ]))))) {
    # runs if the actor != recipient in the row
    # if (df[nrow(df) - i + 1, "actor"] != df[nrow(df) - i + 1, "recipient"]){
    if (x2 != y2) {
      # if the two numbers aren't already in the df
      if (!(any(apply(last_df2, 1, function(row) all(row == reverse2))))) {
        last_df2 <- rbind(last_df2, df2[nrow(df2) - i + 1, ])
      }
    }
  }
}
last_df2 <- last_df2[nrow(last_df2):1, ] # reverses the order of the dataframes


#'Create a directed network graph from the dataset.
#'Plot the graph.
new_g2 <- graph_from_edgelist(as.matrix(last_df2), directed = TRUE)
plot(new_g2, layout = l)

#'Calculate the triad census.
census2 <- triad_census(new_g2)
transitive2 <- census2[9]
census_total2 <- sum(census2)


#'Determine the proportion of transitive triads.
triad_proportion2 <- transitive2/census_total2


#' EXTENSIONS:
 
#' 1. Repeat this 1000 times.  Store the results. What is the average proportion of transitive triads?

interactions <- vector()

# this might take awhile to run
for (i in 1:1000) {
  df3 <- as.data.frame(matrix(round(runif(n = 400, min = 1, max = 4), 0), nrow = 200))
  colnames(df3) = c("actor", "recipient")
  #' Only keep the last interaction between any 2 individuals (e.g. if BA was last interaction, only keep that, no other BA or AB)
  last_df3 <- data.frame() # makes a new dataframe
  for (j in 1:nrow(df3)) {
    # runs if the last row - j + 1 is not already in the new dataframe
    # last row - j + 1 because j starts at 1
    x3 <- df3[nrow(df3) - j + 1, "actor"]
    y3 <- df3[nrow(df3) - j + 1, "recipient"]
    reverse3 <- as.data.frame(matrix(c(y3,x3), nrow = 1, ncol = 2, byrow = TRUE))
    if (!(any(apply(last_df3, 1, function(row) all(row == df3[nrow(df3) - j + 1, ]))))) {
      # runs if the actor != recipient in the row
      if (x3 != y3) {
        # if the two numbers aren't already in the df
        if (!(any(apply(last_df3, 1, function(row) all(row == reverse3))))) {
          last_df3 <- rbind(last_df3, df3[nrow(df3) - j + 1, ])
        }
      }
    }
  }
  last_df3 <- last_df3[nrow(last_df3):1, ] # reverses the order of the dataframes
  new_g3 <- graph_from_edgelist(as.matrix(last_df3), directed = TRUE)
  census3 <- triad_census(new_g3)
  transitive3 <- census3[9]
  census_total3 <- sum(census3)
  triad_proportion3 <- transitive3/census_total3
  interactions <- append(interactions, triad_proportion3)
}

avg_proportion <- mean(interactions) # average proportion of transitive triads




#' 2. For one network, calculate the rolling proportion of transitive triads after every interaction. Plot the results.

rolling_g = list() #empty list to store the new dataframes

# iterate through each row and create new df with each new one added
for (j in 1:(nrow(last_df))) {
  rolling_g[[j]] = last_df[1:j, ]
}

rolling_plots = list()
for (k in 1:(length(rolling_g))) {
  rolling_plots[[k]] = graph_from_edgelist(as.matrix(last_df[1:k, ]), directed = TRUE)
}

rolling_census = list()
for (m in 1:(length(rolling_g))) {
  rolling_census[[m]] = triad_census(rolling_plots[[m]])[9]/sum(triad_census(rolling_plots[[m]]))
}

new <- data.frame(
  timeline = c(1:length(rolling_census)),
  census = unlist(rolling_census))

ggplot(new, aes(timeline, census)) + geom_line() + xlab("Interaction") +
  ylab("Triad Census") + ggtitle("Rolling Triad")









