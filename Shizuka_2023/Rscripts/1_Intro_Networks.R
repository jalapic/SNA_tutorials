
library(help='igraph')

library(igraph) #load the package 
?graph_from_adjacency_matrix

#Make our first network graph
g=make_graph(~A-B-C-A, D-E-F-D, A-F) 
plot(g)


class(g)

g


V(g) #look up vertices 

E(g) #look up edges

V(g)$color=c("white", "red", "green", "blue", "orange", "yellow") #a random set of colors 
plot(g)

E(g)$width=1:7
E(g)$color=rainbow(7) #rainbow() function chooses a specified number of colors 
plot(g)



