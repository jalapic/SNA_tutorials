# Network Plots: Layouts and Attributes
# Same as the end of lesson 2
library(igraph)
am=as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/sample_adjmatrix.csv", header=T, row.names=1))
am
g=graph_from_adjacency_matrix(am, mode="undirected", weighted=T)
plot(g, edge.width=E(g)$weight)

# plot as as a circle
#option 1:
plot(g, layout=layout_in_circle(g))
#option 2:
l=layout_in_circle(g) 
plot(g, layout=l)

# returns class of l
class(l) 
l

# Force-directed layouts: plots g with the default Fruchterman-Reingold algorithm
plot(g, layout=layout_with_fr(g))

# setting the seed creates reproducable plots
set.seed(10) 
l=layout_with_fr(g) 
plot(g, layout=l)

# Plotting using force-directed methods (top) and static methods (bottom)
set.seed(10)
layouts = c("layout_with_fr", "layout_with_kk", "layout_with_dh", "layout_with_gem", "layout_as_star", "layout_as_tree", "layout_in_circle", "layout_on_grid")
par(mfrow=c(2,4), mar=c(1,1,1,1))
for(layout in layouts){
  l=do.call(layout, list(g))
  plot(g, layout=l, edge.color="black", vertex.label="", main=layout)
}

# clears plots
dev.off()

# custom layout: verticies on a line with curved edges
l=matrix(c(1,2,3,4,5,6,7, 1,2,3,4,5,6,7),ncol=2)
plot(g,layout=l,edge.curved=TRUE, vertex.label="")


# Import sample "attributes file"
attrib=read.csv("https://dshizuka.github.io/networkanalysis/SampleData/sample_attrib.csv")
attrib

# Sort attributes alphabetically
V(g)$sex=factor(attrib[match(V(g)$name, attrib$Name), "Sex"]) # factor() preserves data as M/F
V(g)$age=attrib[match(V(g)$name, attrib$Name), "Age"]
g

# assign colors to sex by converting sex to numbers (1 = female, 2 = male)
V(g)$color=c("gold","slateblue")[as.numeric(V(g)$sex)]
set.seed(10)
l=layout_with_fr(g)
plot(g, layout=l,vertex.label="", vertex.size=V(g)$age, edge.width=E(g)$weight, edge.color="black")
legend("topleft", legend=c("Female", "Male"), pch=21, pt.bg=c("gold", "slateblue"))