# Random Graphs
# two types of Erdös-Renyí Random Graph:
# G(n,m) model, in which n nodes are randomly connected by m edges
# G(n,p) model, in which we have a graph of n nodes, and each pair of nodes has probability p of being connected

# G(n,m) random graph with n = 20 and m = 38,
library(igraph)
g1=erdos.renyi.game(20,38,type="gnm") 
g1
edge_density(g1) # graph density
plot(g1,layout=layout.circle) # plot

# G(n,p) random graph with n = 20 and p = 0.2
g2=erdos.renyi.game(20,0.2,type="gnp") 
ecount(g2) # number of edges
edge_density(g2) # graph density
plot(g2,layout=layout.circle)

# using for-loop, make 100 random graphs of n = 20 and p = 0.2
# calculate densities of all graphs and find the mean
densities=vector(length=100) #set up empty vector 
for (i in 1:100){
  r=erdos.renyi.game(20,0.2,type="gnp") #random graph
  densities[i]=graph.density(r) #store the density of random graph as the ith element of the vector
}
densities #print the resulting vector mean(densities) #calculate the mean density

# histogram of densities of random graphs
hist(densities)
abline(v=0.2,lwd=3,lty=2,col="red") #draw a vertical line at x = 0.2. Make this line width = 3, line type = 2 (dashed line), and the line color = red

# Make a plot of 9 random graphs
par(mfrow=c(3,3),mar=c(1,1,1,1)) #the mfrow= argument sets up the number of rows and columns within the plotting region. mar= argument sets the margins of the figures:c(bottom,left,top,right).
for (i in 1:9){
  r=erdos.renyi.game(20,p=0.2)
  plot(r,layout=layout.circle,edge.color="black",edge.width=2,vertex.color="red",vertex.label="") #a bunch of arguments to make the figure look pretty.
}


# Properties of Random Graphs

# create random graphs with n = 20 and p = 0.2, calculate average properties
#First, create a set of vectors in which you'll store the results of the simulations
m=vector(length=100)
mean.k=vector(length=100)
C.loc=vector(length=100)
C.glob=vector(length=100)
#Now, use a For-loop to create 100 random graphs, each time calculating the m, mean degree and clustering coefficient
n=20
p=0.2
for (i in 1:100){
  r=erdos.renyi.game(n,p=p)
  m[i]=ecount(r)
  mean.k[i]=mean(degree(r))
  C.loc[i]=transitivity(r,type="localaverage")
  C.glob[i]=transitivity(r,type="global")
}

# plot all results with par()
# red line represents expected value
par(mfrow=c(1,3)) 
hist(m)
abline(v=(n*(n-1)/2)*p,lty=2,col="red") # expected number of edges, which is simply the number of dyads times p
hist(mean.k)
abline(v=(n-1)*p,lty=2,col="red") # expected mean degree, which is (n-1)*p
hist(C.glob)
abline(v=p,lty=2,col="red") #expected global clustering coefficient, which is simply p


# Simple exercise: path length and network size
seq(from=10,to=100,by=10) #create a vector of numbers starting from 10, ending in 100, in intervals of 10.

# If each person knows 5 people, mean degree = (n-1)p = 5, then p = 5/(n-1)
# create an empty matrix with 10 columns and 500 rows to ‘store’ all the ‘average path length’ values
paths=matrix(ncol=10,nrow=500) 
# create random graphs the size of n
# do it i = 500 times per each size n
# result of each iteration will be stored in the ith row, (n/10)th column (1st column for n = 10, 2nd column for n = 20, so on) of the matrix called paths
for (n in seq(10,100,10)){
  for(i in 1:500){ 
    r=erdos.renyi.game(n,p=(5/(n-1))) 
    paths[i,n/10]=mean_distance(r)
  }
}
head(paths)

# apply('the matrix or array', 'margin'—1 for rows and 2 for columns, 'function')
# this applies mean() to the columns of the matrix "paths"
paths.avg=apply(paths,2,mean) 
paths.avg

# plot
x=seq(10,100,10) #the x-axis is going to be 10, 20, 30,...100
plot(x,paths.avg,type="b",pch=20,xlab="N",ylab="Average Path Length", las=1)

# plot as log-scale
plot(x,paths.avg,type="b",pch=20,xlab="N",ylab="Average Path Length",log="x",las=1)






