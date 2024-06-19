# Adjacency Matrix: rows and columns represent different nodes
# If unweighted, edges are represented by 0 or 1
# If weighted, values represent edge qualities (or tie strengths)

# Sets up the same graph as lesson 1
library(igraph)
g=make_graph(~A-B-C-A, D-E-F-D, A-F) 
V(g)$color=c("white", "red", "green", "blue", "orange", "yellow")
E(g)$weight=1:7
E(g)$color=rainbow(7)
plot(g)

# Extracts adjacency matrix, sparse = F means no edge appears as 0
# sparse = T returns no edge as a period
as_adjacency_matrix(g, sparse=F)


# Edge List: two-column list of two connected nodes
# If directed network, edge goes from first column's node to second column's
# If weighted network, third column indicates edge weight
as_edgelist(g)


# Affiliation Matrix (aka, Incidence Matrix, or individual-by-group matrix)
# Creates a matrix for co-occurrence (ex: two individuals appearing together)
A=c(1,1,0,0) 
B=c(1,0,1,0) 
C=c(1,0,1,0) 
D=c(0,1,0,1) 
E=c(0,0,1,1) 
aff=matrix(c(A,B,C,D,E),nrow=5,byrow=TRUE) 
dimnames(aff)=list(c("A","B","C","D","E"),c("Group1","Group2","Group3","Group4"))
aff #The individual-by-group matrix

# one-mode projection: multiplies matrix with the transpose of itself
# The result - diagonal represents how many groups each individual participated in, other represents number of times those two individuals were in the same group
aff %*% t(aff)

# Plots the network
m2=aff %*% t(aff)
g2=graph_from_adjacency_matrix(m2, "undirected", weighted=T, diag=F)
plot(g2, edge.width=E(g2)$weight)


# Adjacency List (aka node list): first column is the "focal" node, going towards the right are all other nodes that are connected/adjacent to it
as_adj_list(g)


# Directed networks 
# cell value = 1 if edge goes from row vertex to column vertex
dir.g=make_graph(~A-+B-+C-+A, D-+E-+F-+D, A+-+F)
plot(dir.g)

# creates adjacency matrix and edgelist
as_adjacency_matrix(dir.g, sparse=F)
as_edgelist(dir.g)

# attr = "weight" makes edge widths represent edge weights or values
as_adjacency_matrix(g, sparse=F, attr="weight")

# display all edge attributes
as_data_frame(g)

# shows matrix with only two nodes (from and to) and edge weights
as_data_frame(g)[,c("from", "to", "weight")]

# imports "sample_edgelist.csv"
edge.dat=read.csv("https://dshizuka.github.io/network2018/NetworkWorkshop_SampleData/sample_edgelist.csv") 
edge.dat

# creates the network eg
set.seed(2)
eg=graph_from_data_frame(edge.dat, directed=FALSE) 
eg

plot(eg, edge.width=E(eg)$weight)

# imports adjacency matrix
am=as.matrix(read.csv("https://dshizuka.github.io/network2018/NetworkWorkshop_SampleData/sample_adjmatrix.csv", header=T, row.names=1))
am

# converts am into an igraph object
# weighted = T or weighted = F tells it if it's weighted or not
g=graph_from_adjacency_matrix(am, mode="undirected", weighted=T)
plot(g, edge.width=E(g)$weight)



