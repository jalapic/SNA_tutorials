# Measuring Networks Part 2: Community Structure and Assortment

# load packages
library(igraph)
library(assortnet)

# create network with two completely connected clusters that are loosely connected to each other
g=make_graph(~A:B:C:D:E-A:B:C:D:E, F:G:H:I:J-F:G:H:I:J, A-F, B-G)

set.seed(7)
l=layout_with_fr(g)
plot(g, layout=l, edge.color="black")

# detects communities
eb=edge.betweenness.community(g) 
eb

length(eb) #number of communities

modularity(eb) #modularity

membership(eb) #assignment of nodes to communities

plot(eb, g, layout=l)

# imports sparrow network data and plots it
library(asnipe)
library(igraph)
assoc=as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/Sample_association.csv", header=T, row.names=1)) #import individual-by-group data
gbi=t(assoc) #transpose the data into group-by-individual
mat=get_network(t(assoc), association_index="SRI") #create adjacency matrix with "simple ratio index"
g.sparrow=graph_from_adjacency_matrix(mat, "undirected", weighted=T) #make into igrpah object

plot(g.sparrow, edge.width=E(g.sparrow)$weight*5, vertex.label="")

# Louvain method of community detection
com=cluster_louvain(g.sparrow)
com

set.seed(2)
plot(com, g.sparrow, vertex.label="", edge.width=E(g.sparrow)$weight*5)

# assigns colors with RColorBrewer
library(RColorBrewer)
colors=brewer.pal(length(com),'Accent') #make a color palette
V(g.sparrow)$color=colors[membership(com)] #assign each vertex a color based on the community assignment

set.seed(2)
plot(g.sparrow, vertex.label="", edge.width=E(g.sparrow)$weight*5)

# Assortment
set.seed(3)
V(g)$size=c(rnorm(5, mean=20, sd=5), rnorm(5, mean=30, sd=5)) #assign sizes to nodes using two normal distributions with different means

plot(g, layout=l, edge.color="black")

# measures assortment using node size
assortativity(g, V(g)$size, directed=F)

# converts size variable into a binary (discrete) trait and then calculates assortment coefficient
V(g)$size.discrete=(V(g)$size>25)+0 #shortcut to make the values = 1 if large individual and 0 if small individual, with cutoff at size = 25
assortativity(g, V(g)$size.discrete, directed=F)

# creates node attribute with random variation
set.seed(3)
V(g)$random=rnorm(10, mean=20, sd=5) #create a node trait that varies randomly for all nodes 
assortativity(g, V(g)$random, directed=F)

plot(g, layout=l, edge.color="black", vertex.size=V(g)$random)

# uses assortnet package to measure assortment coefficient
adj=as_adjacency_matrix(g, sparse=F)
assortment.continuous(adj, V(g)$size)

assortment.discrete(adj, V(g)$size.discrete) # "mixing matrix" shows cumulative edge weights that occur between individuals with the same vs. different vertex labels

# randomly adds edge weights
E(g)$weight=runif(length(E(g)), min=0, max=1)
adj=as_adjacency_matrix(g, sparse=F, attr="weight")
assortment.continuous(adj, V(g)$size)

assortment.discrete(adj, V(g)$size.discrete)