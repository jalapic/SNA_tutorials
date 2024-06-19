library(devtools)
devtools::install_github("jalapic/hierformR")  #version 0.1.1


library(hierformR)

df<-data.frame(winner=c(1,2,3,1,2,3,2,1,2,3,3,1,2,3,4,3,1,3,2,1,1,1,1,2,2),
               loser=c(2,4,4,4,3,2,1,3,3,4,4,2,3,2,3,4,2,4,3,3,3,2,2,4,3)
)

df

# get the last interactions between any pair
lastints(df)

# get network graph for each of the last interaction dfs
lastnet(df)

gs <- lastnet(df) # put into a list

# take very last graph
g <- gs[[25]] 

# get network characteristics
triad.census(g)
netchar(g)
?netchar()

## Add states to every row of a dataframe of interactions.
addstates(df)


## You can also calculate triangle transitivity using the t.tri function from compete

library(compete)
?ttri()

compete::ttri(g)

mat <- igraph::as_adjacency_matrix(g)
mat
compete::ttri_test(mat)

