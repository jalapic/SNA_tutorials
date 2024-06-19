# Measuring Networks Part 1: Centrality and Global Measures

# loads sample social network from "intro to animal social networks"
library(asnipe)
library(igraph)
degree=igraph::degree
betweenness=igraph::betweenness
closeness=igraph::closeness
assoc=as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/Sample_association.csv", header=T, row.names=1))
gbi=t(assoc) 
mat=get_network(t(assoc), association_index="SRI") 
g=graph_from_adjacency_matrix(mat, "undirected", weighted=T) #create a graph object

# plot the network
set.seed(10)
l=layout_with_fr(g)
plot(g, layout=l, vertex.label="", vertex.color="gold", edge.color="slateblue", edge.width=E(g)$weight*5)

# calculates degree centrality
degree(g)

# varies node sizes proportional to degree centrality
set.seed(10)
de=igraph::degree(g)
plot(g, vertex.label="", vertex.color="gold", edge.color="slateblue", vertex.size=de*2, edge.width=E(g)$weight*5)

# calculates node strength (sum of weights of edges connected to nodule)
# plots node sizes proportional to degree centrality
set.seed(10)
st=graph.strength(g)
plot(g,  vertex.label="", vertex.color="gold", edge.color="slateblue", edge.width=E(g)$weight*5, vertex.size=st*5)

# calculates and plots betweenness centrality (number of geodesic paths (shortest paths) that go through a given node)
be=betweenness(g, normalized=T)
plot(g,  vertex.label="", vertex.color="gold", edge.color="slateblue", vertex.size=be*50, edge.width=E(g)$weight*5)

# sparrow network
names=V(g)$name
de=degree(g)
st=graph.strength(g)
be=betweenness(g, normalized=T)

#assemble dataset
d=data.frame(node.name=names, degree=de, strength=st, betweenness=be) 
head(d) #display first 6 lines of data

# plots relationship between degree and strength
plot(strength~degree, data=d)

# plots relationship between betweenness and strength
plot(betweenness~strength, data=d)


n=vcount(g) # stores number of nodes
m=ecount(g) # stores number of edges
n

# calculates density of network (density = [# edges that exist] / [# edges that are possible])
dyads=n*(n-1)/2
density=m/dyads
density

# alternate way of calculating density of network
edge_density(g)

# shows components of g
components(g)

# histogram of degree distribution (statistical distribution of node degrees in a network)
hist(degree(g), breaks=10, col="gray")

# plots probability densities of each degree (i.e., what proportion of nodes has degree = 1, degree = 2, etc)
pk=degree_distribution(g)
plot(pk, pch=19)

# finds the shortest oath between each dyad in the network
paths=distances(g, algorithm="unweighted")
paths

# replaces Inf with NA
# calculates mean with just upper triangle or lower triangle of matrix
paths[paths=="Inf"]=NA
mean(paths[upper.tri(paths)], na.rm=T)

# DOES NOT WORK AS EXPECTED, IS SUPPOSED TO PRODUCE THE SAME RESULT AS ABOVE
mean_distance(g)
?mean_distance()

comps=decompose(g)
comps # a list object consisting of each component as graph object

path.list=lapply(comps, function(x) distances(x, algorithm="unweighted")) #make list object with two path length matrices
avg.paths=sapply(path.list, mean) #average path length of each component
diams=sapply(path.list, max) #diameter of each component
avg.paths
diams

g.cluster=transitivity(g, "global") # Global Clustering Coefficient = “ratio of triangles to connected triples”
l.cluster=transitivity(g, "local") # Local Clustering Coefficient = for each node, the proportion of their neighbors that are connected to each other
av.l.cluster=transitivity(g, "localaverage") # Average Local Clustering Coefficient
g.cluster
l.cluster
av.l.cluster
