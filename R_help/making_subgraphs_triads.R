library(igraph)

# Create a sample graph
g <- igraph::erdos.renyi.game(n=6, p=.3, directed=TRUE)

g
plot(g)

# Get the triad census
tc <- triad_census(g)
tc

combn(V(g), 3, simplify = FALSE)

ids <- combn(V(g), 3, simplify = FALSE)
ids

#doing it for one (number 19)
sg <- induced_subgraph(g, vids = ids[19][[1]])
triad_census(sg)

res<-NULL
for(i in 1:length(ids)){
sg <- induced_subgraph(g, vids = ids[i][[1]])
res[[i]] <- triad_census(sg)
}

names(res)<-ids
res

# Extract triads
triads <- extract_triads(g)


