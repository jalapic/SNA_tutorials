# Simulating Networks

# load packages
library(asnipe)
library(igraph)


# Flock network with size-related variation in gregariousness

# create population of 50 individuals with a trait distributed normally (mean of 20 and standard deviation of 5)
set.seed(2)
n=50
# sorted largest to smallest
trait=sort(rnorm(n,mean=20, sd=5), decreasing = T) # rnorm() randomly generates values

# assign probability of joining any given flock (gregariousness)
# p, will range from 0.01 to 0.2, and will follow a uniform distribution
# sorted largest to smallest
p=sort(runif(n, min=0.01, max=0.1)) #gregariousness
plot(trait, p, ylab="Gregariousness", xlab="Trait Value") #see that gregariousness and trait are correlated

# flock observations
f=100 # 100 flocks

# create matrix of 0s
ibg=matrix(0,nrow=n, ncol=f)
# use for-loop to fill in 1s
for(i in 1:n){
  for (j in 1:f){
    ibg[i,j]=sample(c(1,0), 1, prob=c(p[i], 1-p[i]))
  }
}

# create adjacency matrix
adj=get_network(t(ibg), data_format="GBI", association_index="SRI")

g=graph_from_adjacency_matrix(adj, "undirected", weighted=T)

# plot with node size proportional to the individual's trait value
plot(g,vertex.size=trait/2, vertex.label="", edge.width=E(g)$weight*5)

# plot relationship between trait value and individual's degree centrality
plot(trait, degree(g), pch=19, col="tomato")

# Pearson's product-moment correlation test
cor.test(trait, degree(g))


