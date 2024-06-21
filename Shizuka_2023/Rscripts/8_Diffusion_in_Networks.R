# Intro to Diffusion on Networks

library(igraph) # load package

# Simulation of asocial changes in state (e.g., asocial learning)
# random graph with 100 nodes
set.seed(4)
n=100
g=erdos.renyi.game(n, p=0.05)
V(g)$status=0 #the 'innovation adoption status' for each individual. All initially 0.

l=layout_with_fr(g) # save layout
plot(g, vertex.label="", vertex.size=8, vertex.color="darkgray", layout=l)

# set ‘asocial learning’ parameter ( probability that any given individual will come up with the innovation–e.g., how to forage for a new prey item) to 0.1
x=0.1

# practice run
naive=which(V(g)$status==0) #which individuals have not adopted yet?

adopt=sample(c(1,0), length(naive), prob=c(x, 1-x), replace=T) #based on the probabilities, flip a coin for each naive individual and determine if the individual adopts the innovation in the current time step

V(g)$status[naive][which(adopt==1)]=1 #change status of individuals whose status is 0 and is in the list of new adopters 

plot(g, vertex.label="", vertex.color=c("darkgray", "red")[V(g)$status+1], vertex.size=8, layout=l)

# repeat 30 times, ignoring the result if that individual has already adopted
t=30
g.time=list()
V(g)$status=0
for(j in 1:t){
  naive=which(V(g)$status==0) 
  adopt=sample(c(1,0), length(naive), prob=c(x, 1-x), replace=T)
  V(g)$status[naive][which(adopt==1)]=1 
  g.time[[j]]=g
}

g.time # 20 igraph objects where individual status changes accross time

# plots how many cumulative individuals adopt the new innovation (i.e., become status=1) across time steps
n.adopt.asocial=sapply(g.time, function(x) length(which(V(x)$status==1))) #for each time step, count the number of individuals that have adopted the innovation
plot(n.adopt.asocial, type="b", las=1, ylab="Cumulative number of nodes adopted", xlab="Time", ylim=c(0,100))

# plots network across first 20 time points
def.par <- par(no.readonly = TRUE)
layout(matrix(1:20, byrow=T, nrow=5))
par(mar=c(1,1,1,1))
for(i in 1:20){
  v.col=c("darkgray", "red")[V(g.time[[i]])$status+1]
  plot(g.time[[i]], vertex.label="", vertex.color=v.col, layout=l, main=paste("Time",i))
}


# Simulation the social transmission of whatever state in a random graph
# same set.seed() as asocial case
set.seed(4)
n=100
g2=erdos.renyi.game(n, p=0.05)
l2=layout_with_fr(g2)

V(g2)$status=0 # Create a vertex attribute for adoption status. 1 if the node has adopted the innovation. 0 if not.
seed=sample(V(g2),2) #select 2 innovators
V(g2)$status[seed]=1 #These 'seed' individuals get a status of 1 at the beginning.
plot(g2, vertex.label="", vertex.size=8, vertex.color=c("darkgray", "red")[V(g2)$status+1], layout=l2)

# s = "social transmission" parameter
# linear increase in the probability that an individual will take on a new “state” (e.g., learn a new foraging strategy or get infected by a disease) when it has a ‘neighbor’ that has that state
# 0 ≤ s ≤ 1
# tau is the parameter that describes the influence of social learning
# s is the number of neighbors of an individual that has already adopted the innovation
tau = 0.1

# simulate 30 time steps, saving the network for each time point
# neighbors() identifies nodes connected to each node
# p = probability that an individual that has not yet adopted the innovation will adopt in that time step = 1−e^(−τ∗s)
# sample() flips a biased coin to see if the focal individual adopts the innovation or not
# repeat for every unadopted individual (status = 0)
# if the individual gets 1, then status = 1
# repeat from beginning
t=30 #time steps to run the simulation
g2.time=list() #empty list to store the output networks
for(j in 1:t){
  nei.adopt=sapply(V(g2), function(x) sum(V(g2)$status[neighbors(g2,x)]))
  p=(1-exp(-tau*nei.adopt))*abs(V(g2)$status-1) #here, we multiply the probabilities by 0 if node is already adopted, and 1 if not yet adopted
  adopters=sapply(p, function(x) sample(c(1,0), 1, prob=c(x, 1-x)))
  V(g2)$status[which(adopters==1)]=1
  g2.time[[j]]=g2
}

# plot accumulation curve for the number of individuals that adopted the innovation through social learning
n.adopt.social=sapply(g2.time, function(x) length(which(V(x)$status==1))) #for each time step, count the number of adopters.

plot(n.adopt.social, type="b", las=1, ylab="Cumulative number of nodes adopted", xlab="Time", ylim=c(0,100))

# plots asocial and social case together
plot(n.adopt.social, type="l", lty=1, col="black",las=1, ylab="Cumulative number of nodes adopted", xlab="Time", ylim=c(0,100))
points(n.adopt.asocial, type="l", las=1, lty=2, col="red")
legend("topleft", lty=c(1,2), col=c("black", "red"), legend=c("asocial", "social"))


# Animating social transmission using GIFs

# plot first 10 time points of social diffusion
layout(matrix(1:10, byrow=T, nrow=2))
par(mar=c(1,1,1,1))
for(i in 1:10){
  v.col=c("darkgray", "red")[V(g2.time[[i]])$status+1]
  plot(g2.time[[i]], vertex.label="", vertex.color=v.col, layout=l2, main=paste("Time",i))
}

par(def.par) # makes sure networks are laid out identically except for node colors

# creates gif showing change in social network across time
# set speed with: interval = 
# show other animation arguments: ?ani.options
library(animation)
saveGIF( 
  {for (i in 1:30) {
    plot(g2.time[[i]], layout=l2, vertex.label="", vertex.size=5, vertex.color=c("darkgray", "red")[V(g2.time[[i]])$status+1], main=paste("time",i,sep=" "))
  }
  }, movie.name="sample_diffusion.gif", interval=0.2, nmax=30, ani.width=600, ani.height=600)


# Effect of network structure on transmission dynamics

# make network with strong community structure and 100 nodes
# # 25 nodes to 4 communities
# probability of edges within communities = 0.5
# probability of edges accross communities = 0.005
set.seed(2)
n=100
mod.assign=c(rep(1,25), rep(2,25), rep(3,25), rep(4,25))
same.mod=outer(mod.assign, mod.assign, FUN="==")
p.mat=apply(same.mod, c(1,2), function(x) c(0.005, 0.5)[x+1])
adj=apply(p.mat, c(1,2), function(x) sample(c(1,0), 1, prob=c(x, 1-x)))
adj[lower.tri(adj)]=0
diag(adj)=0
adj=adj+t(adj)
g3=graph_from_adjacency_matrix(adj, "undirected")
l3=layout_with_fr(g3)
plot(g3, vertex.label="", vertex.size=3, layout=l3)

# randomly select 2 innovators
V(g3)$status=0 # Create a vertex attribute for adoption status. 1 if the node has adopted the innovation. 0 if not.
seed=sample(V(g3),2) #select 2 innovators
V(g3)$status[seed]=1 #These 'seed' individuals get a status of 1 at the beginning.
plot(g3, vertex.label="", vertex.size=8, vertex.color=c("darkgray", "red")[V(g3)$status+1], layout=l3)

# simulation with same tau parameter
tau = 0.1
t=30 #time steps to run the simulation
g3.time=list() #empty list to store the output networks
for(j in 1:t){
  nei.adopt=sapply(V(g3), function(x) sum(V(g3)$status[neighbors(g3,x)]))
  p=(1-exp(-tau*nei.adopt))*abs(V(g3)$status-1) #here, we multiply the probabilities by 0 if node is already adopted, and 1 if not yet adopted
  adopters=sapply(p, function(x) sample(c(1,0), 1, prob=c(x, 1-x)))
  V(g3)$status[which(adopters==1)]=1
  g3.time[[j]]=g3
}

# transmission dynamics
n.adopt.social=sapply(g3.time, function(x) length(which(V(x)$status==1))) #for each time step, count the number of adopters.
plot(n.adopt.social, type="l", las=1, ylab="Cumulative number of nodes adopted", xlab="Time", ylim=c(0,100))

# animate network
library(animation)
saveGIF( 
  {for (i in 1:30) {
    plot(g3.time[[i]], layout=l3, vertex.label="", vertex.size=5, vertex.color=c("darkgray", "red")[V(g3.time[[i]])$status+1], main=paste("time",i,sep=" "))
  }
  }, movie.name="sample_diffusion2.gif", interval=0.2, nmax=30, ani.width=600, ani.height=600)
# spread is highly influenced by community structure


# NBDA: Network-Based Diffusion Analysis
# The package NBDA isn't available for this version of R


# NBDA on the social spread through the modular network
library(NBDA)
adj.mat=as_adj(g3, sparse=F)
adj.array=array(dim=c(nrow(adj.mat), ncol(adj.mat), 1))
adj.array[,,1]=adj.mat
#get list of individuals that solved at each time
solve.mat=sapply(g3.time, function(x){
  V(x)$status
})

#time of acquisition
ta=apply(solve.mat, 1, function(x) min(which(x==1)))
ta[is.infinite(ta)]=30
#order of acquisition
oa=order(ta)

diffdat=nbdaData("try1",  assMatrix=adj.array, orderAcq=oa, timeAcq=ta)

# oa.fit_social=oadaFit(diffdat, type="social")
# oa.fit_social@outputPar
# oa.fit_social@aic
# data.frame(Variable=oa.fit_social@varNames,MLE=oa.fit_social@outputPar,SE=oa.fit_social@se)

ta.fit_social=tadaFit(diffdat, type="social")
#ta.fit_social@outputPar
data.frame(Variable=ta.fit_social@varNames,MLE=round(ta.fit_social@outputPar,3),SE=round(ta.fit_social@se,3))


# NBDA on the asocial spread through the modular network
adj.mat=as_adj(g, sparse=F)
adj.array=array(dim=c(nrow(adj.mat), ncol(adj.mat), 1))
adj.array[,,1]=adj.mat
#get list of individuals that solved at each time
solve.mat=sapply(g.time, function(x){
  V(x)$status
})

#time of acquisition
ta=apply(solve.mat, 1, function(x) which.max(x==1))
ta[is.infinite(ta)]=30
#order of acquisition
oa=order(ta)

diffdat=nbdaData("try1",  assMatrix=adj.array, orderAcq=oa, timeAcq=ta)

# oa.fit_social=oadaFit(diffdat, type="social")
# oa.fit_social@outputPar
# oa.fit_social@aic
# data.frame(Variable=oa.fit_social@varNames,MLE=oa.fit_social@outputPar,SE=oa.fit_social@se)

ta.fit_social2=tadaFit(diffdat, type="social")
#ta.fit_social2@outputPar
data.frame(Variable=ta.fit_social2@varNames,MLE=round(ta.fit_social2@outputPar,3),SE=ta.fit_social2@se)





