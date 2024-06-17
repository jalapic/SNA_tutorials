### Sparrow Flock Network


library(tidyr) #or you can just load the whole tidyverse with library(tidyverse)
library(asnipe)
library(igraph)


flockdat=read.csv('Shizuka_2023/Data/Flock_Season2_Dryad.csv')
head(flockdat)

#get the columns that contain the bird IDs.
birdcols=grep("Bird",colnames(flockdat))
birdcols

flockdat[,birdcols]

gather(flockdat[,birdcols])
bird.ids=unique(gather(flockdat[,birdcols])$value)
bird.ids
bird.ids=bird.ids[is.na(bird.ids)==F]
bird.ids






m1=apply(flockdat[,birdcols], 1, function(x) match(bird.ids,x))
m1

m1[is.na(m1)]=0
m1[m1>0]=1
m1

rownames(m1)=bird.ids #rows are bird ids
colnames(m1)=paste('flock', 1:ncol(m1), sep="_") #columns are flock IDs (just "flock_#")
m1

adj=get_network(t(m1), data_format="GBI", association_index = "SRI") # the adjacency matrix

g=graph_from_adjacency_matrix(adj, "undirected", weighted=T) #the igraph object

set.seed(2)
plot(g, edge.width=E(g)$weight*10, vertex.label="", vertex.size=5) #plot it

rowSums(m1)

m2=m1[which(rowSums(m1)>2),]
adj=get_network(t(m2), data_format="GBI","SRI")

g=graph_from_adjacency_matrix(adj, "undirected", weighted=T)
set.seed(2)
plot(g, edge.width=E(g)$weight*10, vertex.label="", vertex.size=5)


com=fastgreedy.community(g) #community detection method
node.colors=membership(com) #assign node color based on community membership
set.seed(2)
plot(g, edge.width=E(g)$weight*10, vertex.label="", vertex.size=5, vertex.color=node.colors)

