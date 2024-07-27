# calculating how many non-isopmorphic graphs there are
library(igraph)
library(combinat)

nodes = 6

permutations <- function(n = NULL) {
  num <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
  combos <- unique(permn(num))
  for (i in 1:length(combos)) {
    combos[[i]] <- combos[[i]][1:n]
  }
  combos <- unique(combos)
  return(combos)
}

total_graphs = (2^5) * (2^4) * (2^3) * (2^2) * 2

# m <- matrix(new[[1]], nrow = nodes)
# g <-graph_from_adjacency_matrix(m, mode="directed")
# plot(g, layout = layout_in_circle(g))


# data_5 <- permutations(5)
# data_4 <- permutations(4)
# data_3 <- permutations(3)
# data_2 <- permutations(2)
# data_1 <- permutations(1)


# this comes from the permutations above
data_5 <- list(c(0,0,0,0,0), c(0,0,0,0,1), c(0,0,0,1,0), c(0,0,1,0,0), c(0,1,0,0,0), c(1,0,0,0,0), c(1,0,0,0,1), c(0,1,0,0,1), c(0,0,1,0,1), c(0,0,0,1,1), c(0,0,1,1,0), c(0,1,0,1,0), c(1,0,0,1,0), c(1,0,1,0,0), c(0,1,1,0,0), c(1,1,0,0,0), c(1,1,0,0,1), c(1,0,1,0,1), c(1,0,0,1,1), c(0,1,0,1,1), c(0,1,1,0,1), c(0,0,1,1,1), c(0,1,1,1,0), c(1,0,1,1,0), c(1,1,0,1,0), c(1,1,1,0,0), c(1,1,0,1,1), c(1,0,1,1,1), c(1,1,1,0,1), c(0,1,1,1,1), c(1,1,1,1,0), c(1,1,1,1,1))
data_4 <- list(c(0,0,0,0), c(0,0,0,1), c(0,0,1,0), c(0,1,0,0), c(1,0,0,0), c(0,0,1,1), c(0,1,0,1), c(1,0,0,1), c(1,0,1,0), c(0,1,1,0), c(1,1,0,0), c(0,1,1,1), c(1,0,1,1), c(1,1,0,1), c(1,1,1,0), c(1,1,1,1))
data_3 <- list(c(0,0,0), c(0,0,1), c(0,1,0), c(1,0,0), c(1,0,1), c(0,1,1), c(1,1,0), c(1,1,1))
data_2 <- list(c(0,0), c(0,1), c(1,0), c(1,1))
data_1 <- list(0, 1)


chunk_1 <- c(data_1[1], data_1[2])
chunk_2 <- c(rep(data_2[1], length(chunk_1)), rep(data_2[2], length(chunk_1)), rep(data_2[3], length(chunk_1)), rep(data_2[4], length(chunk_1)))
chunk_3 <- c(rep(data_3[1], length(chunk_2)), rep(data_3[2], length(chunk_2)), rep(data_3[3], length(chunk_2)), rep(data_3[4], length(chunk_2)), rep(data_3[5], length(chunk_2)), rep(data_3[6], length(chunk_2)), rep(data_3[7], length(chunk_2)), rep(data_3[8], length(chunk_2)))
chunk_4 <- c(rep(data_4[1], length(chunk_3)), rep(data_4[2], length(chunk_3)), rep(data_4[3], length(chunk_3)), rep(data_4[4], length(chunk_3)), rep(data_4[5], length(chunk_3)), rep(data_4[6], length(chunk_3)), rep(data_4[7], length(chunk_3)), rep(data_4[8], length(chunk_3)), rep(data_4[9], length(chunk_3)), rep(data_4[10], length(chunk_3)), rep(data_4[11], length(chunk_3)), rep(data_4[12], length(chunk_3)), rep(data_4[13], length(chunk_3)), rep(data_4[14], length(chunk_3)), rep(data_4[15], length(chunk_3)), rep(data_4[16], length(chunk_3))) 
chunk_5 <- c(rep(data_5[1], length(chunk_4)), rep(data_5[2], length(chunk_4)), rep(data_5[3], length(chunk_4)), rep(data_5[4], length(chunk_4)), rep(data_5[5], length(chunk_4)), rep(data_5[6], length(chunk_4)), rep(data_5[7], length(chunk_4)), rep(data_5[8], length(chunk_4)), rep(data_5[9], length(chunk_4)), rep(data_5[10], length(chunk_4)), rep(data_5[11], length(chunk_4)), rep(data_5[12], length(chunk_4)), rep(data_5[13], length(chunk_4)), rep(data_5[14], length(chunk_4)), rep(data_5[15], length(chunk_4)), rep(data_5[16], length(chunk_4)), rep(data_5[17], length(chunk_4)), rep(data_5[18], length(chunk_4)), rep(data_5[19], length(chunk_4)), rep(data_5[20], length(chunk_4)), rep(data_5[21], length(chunk_4)), rep(data_5[22], length(chunk_4)), rep(data_5[23], length(chunk_4)), rep(data_5[24], length(chunk_4)), rep(data_5[25], length(chunk_4)), rep(data_5[26], length(chunk_4)), rep(data_5[27], length(chunk_4)), rep(data_5[28], length(chunk_4)), rep(data_5[29], length(chunk_4)), rep(data_5[30], length(chunk_4)), rep(data_5[31], length(chunk_4)), rep(data_5[32], length(chunk_4)))


new_data_1 <- rep(chunk_1, total_graphs/length(chunk_1))
new_data_2 <- rep(chunk_2, total_graphs/length(chunk_2))
new_data_3 <- rep(chunk_3, total_graphs/length(chunk_3))
new_data_4 <- rep(chunk_4, total_graphs/length(chunk_4))
new_data_5 <- rep(chunk_5, total_graphs/length(chunk_5))



all_graphs <- list()
for (i in 1:total_graphs) {
  all_graphs[[i]] <- c(0, new_data_5[[i]], abs(new_data_5[[i]][1] - 1), 0, new_data_4[[i]],
                  abs(new_data_5[[i]][2] - 1), abs(new_data_4[[i]][1] - 1), 0, new_data_3[[i]],
                  abs(new_data_5[[i]][3] - 1), abs(new_data_4[[i]][2] - 1), abs(new_data_3[[i]][1] - 1),
                  0, new_data_2[[i]], abs(new_data_5[[i]][4] - 1), abs(new_data_4[[i]][3] - 1),
                  abs(new_data_3[[i]][2] - 1), abs(new_data_2[[i]][1] - 1), 0, new_data_1[[i]],
                  abs(new_data_5[[i]][5] - 1), abs(new_data_4[[i]][4] - 1), abs(new_data_3[[i]][3] - 1),
                  abs(new_data_2[[i]][2] - 1), abs(new_data_1[[i]] - 1), 0
                  )
}


combos <- list()
sorted_combos <- list()
for (i in 1:total_graphs) {
  combos[[i]] <- sum(all_graphs[[i]][1:6])
  combos[[i]] <- c(combos[[i]], sum(all_graphs[[i]][7:12]))
  combos[[i]] <- c(combos[[i]], sum(all_graphs[[i]][13:18]))
  combos[[i]] <- c(combos[[i]], sum(all_graphs[[i]][19:24]))
  combos[[i]] <- c(combos[[i]], sum(all_graphs[[i]][25:30]))
  combos[[i]] <- c(combos[[i]], sum(all_graphs[[i]][31:36]))
  sorted_combos[[i]] <- sort(combos[[i]])
}




iso <- unique(sorted_combos)
iso_count <- rep(list(0), length(iso))
iso_ex_num <- vector()
for (i in 1:length(sorted_combos)) {
  for (j in 1:length(iso)) {
    if (identical(sorted_combos[[i]], iso[[j]])) {
      if (iso_count[[j]] == 0) {
        iso_ex_num <- c(iso_ex_num, i)
      }
      iso_count[[j]] <- iso_count[[j]] + 1
    }
  }
}

# shows the different types of isographs and how many there are of each
new_mat <- matrix(iso)
new_mat <- cbind(new_mat, iso_count)


iso_graph <- list()
for (i in 1:length(iso)) {
  m <- matrix(all_graphs[[iso_ex_num[i]]], nrow = nodes)
  iso_graph[[i]] <-graph_from_adjacency_matrix(m, mode="directed")
  plot(iso_graph[[i]], layout = layout_in_circle(iso_graph[[1]]))
}

new_mat <- cbind(new_mat, iso_graph)
colnames(new_mat) <- c("ID", "Count", "Example IGraph")
# there are 22 total isomorphs







# ## Data from  http://users.cecs.anu.edu.au/~bdm/data/graphs.html ##
# Data <- read.delim("rfid_data/graph6c.g6",header=FALSE)
# 
# DataFrame<-data.frame() #Creating empty data frame to store adjacency matrices
# 
# G<-as.character(Data[1,1])
# n<-as.numeric(charToRaw(substr(G, 1, 1)))-63
# n #number of vertices of the graph
# 
# for(k in 1:nrow(Data)){
#   
#   G<-as.character(Data[k,1])
#   
#   vector<-c()
#   for(i in 2:nchar(G)){
#     vector<-c(vector,rev(as.numeric(intToBits(as.numeric(charToRaw(substr(G, i, i)))-63)[1:6])))}
#   vector
#   
#   A<-matrix(0,nrow=n,ncol=n)
#   vector2<-vector
#   for(j in 2:n){
#     for(i in 1:(j-1)){
#       A[i,j]<-vector2[1];
#       vector2<-vector2[-1]
#     }}#filling the upper triangle of the adjacency matrix
#   for(i in 2:n){
#     for(j in 1:(i-1)){
#       A[i,j]<-A[j,i]
#     }}#filling the lower triangle of the adjacency matrix
#   A
#   
#   Avec<-as.vector(A)
#   DataFrame<-rbind(DataFrame,Avec)
#   if(k%%10000==0){print(k)}
# }
# 
# colnames(DataFrame)<-NULL
# write.csv(DataFrame,paste0("graphs",n,".csv"),row.names=FALSE)


# new_data <- read.csv("graphs6.csv",header=FALSE,sep=",")
# i = 112
# m <- matrix(as.vector(t(new_data[i,])),nrow=sqrt(length(as.vector(t(new_data[i,])))))
