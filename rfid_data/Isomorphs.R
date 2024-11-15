# calculating how many non-isopmorphic graphs there are
library(igraph)
library(combinat)


# total nodes
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



# produces all possible graphs
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

# shows the different types of isomorphs and how many there are of each
new_mat <- matrix(iso)
new_mat <- cbind(new_mat, iso_count)

iso_combos <- list()
iso_graph <- list()
for (i in 1:length(iso)) {
  m <- matrix(all_graphs[[iso_ex_num[i]]], nrow = nodes)
  iso_graph[[i]] <-graph_from_adjacency_matrix(m, mode="directed")
  iso_combos[[i]] <- combos[[iso_ex_num[i]]]
}

new_mat <- cbind(new_mat, iso_graph)
# there are 22 total isomorphs


# gets the triad census state and id
# accepts triad census
triad_state <- function(census = NULL) {
  name = NULL
  trans = NULL
  if (census[1] == 1) {
    name = "003"
    trans = NA
  }
  if (census[2] == 1) {
    name = "012"
    trans = NA
  }
  if (census[3] == 1) {
    name = "102"
    trans = NA
  }
  if (census[4] == 1) {
    name = "021D"
    trans = NA
  }
  if (census[5] == 1) {
    name = "021U"
    trans = NA
  }
  if (census[6] == 1) {
    name = "021C"
    trans = "Intransitive"
  }
  if (census[7] == 1) {
    name = "111D"
    trans = "Intransitive"
  }
  if (census[8] == 1) {
    name = "111U"
    trans = "Intransitive"
  }
  if (census[9] == 1) {
    name = "030T"
    trans = "Transitive"
  }
  if (census[10] == 1) {
    name = "030C"
    trans = "Intransitive"
  }
  if (census[11] == 1) {
    name = "201"
    trans = "Intransitive"
  }
  if (census[12] == 1) {
    name = "120D"
    trans = "Transitive"
  }
  if (census[13] == 1) {
    name = "120U"
    trans = "Transitive"
  }
  if (census[14] == 1) {
    name = "120C"
    trans = "Mixed"
  }
  if (census[15] == 1) {
    name = "210"
    trans = "Mixed"
  }
  if (census[16] == 1) {
    name = "300"
    trans = "Transitive"
  }
  return(list(name, trans))
}



iso_triad <- function(iso_number = NULL) {
  g = iso_graph[[iso_number]]
  individuals <- c(1, 2, 3, 4, 5, 6)
  ids <- combn(individuals, 3, simplify = FALSE)
  sg = rep(list(NA),length(ids))
  sg_census = rep(list(NA),length(ids))
  state <- rep(list(NA),length(ids))
  trans <- rep(list(NA),length(ids))
  for (i in 1:length(ids)){
    sg[[i]] <- induced_subgraph(g, vids = ids[i][[1]])
    sg_census[[i]] <- triad_census(sg[[i]])
    state[[i]] <- triad_state(sg_census[[i]])[[1]]
    trans[[i]] <- triad_state(sg_census[[i]])[[2]]
  }
  sub_mat <- matrix(ids)
  sub_mat <- cbind(sub_mat, as.matrix(sg))
  sub_mat <- cbind(sub_mat, as.matrix(sg_census))
  sub_mat <- cbind(sub_mat, as.matrix(state))
  sub_mat <- cbind(sub_mat, as.matrix(trans))
  colnames(sub_mat) <- c("Triad", "IGraph", "Triad_Census", "State_ID", "Transitivity")
  return(sub_mat)
}


# iso_1_triads <- iso_triad(1)
# iso_2_triads <- iso_triad(2)
# iso_3_triads <- iso_triad(3)
# iso_4_triads <- iso_triad(4)
# iso_5_triads <- iso_triad(5)
# iso_6_triads <- iso_triad(6)
# iso_7_triads <- iso_triad(7)
# iso_8_triads <- iso_triad(8)
# iso_9_triads <- iso_triad(9)
# iso_10_triads <- iso_triad(10)
# iso_11_triads <- iso_triad(11)
# iso_12_triads <- iso_triad(12)
# iso_13_triads <- iso_triad(13)
# iso_14_triads <- iso_triad(14)
# iso_15_triads <- iso_triad(15)
# iso_16_triads <- iso_triad(16)
# iso_17_triads <- iso_triad(17)
# iso_18_triads <- iso_triad(18)
# iso_19_triads <- iso_triad(19)
# iso_20_triads <- iso_triad(20)
# iso_21_triads <- iso_triad(21)
# iso_22_triads <- iso_triad(22)



iso_trans <- function(iso_number = NULL) {
  data <- iso_triad(iso_number)
  trans = 0
  int = 0
  for (i in 1:nrow(data)) {
    if (data[[i,5]] == "Transitive") {
      trans = trans + 1
    }
    if (data[[i,5]] == "Intransitive") {
      int = int + 1
    }
  }
  return(list(Transitive = trans, Intransitive = int))
}

trans_num = list()
for (i in 1:length(iso)) {
  trans_num[[i]] <- unlist(iso_trans(i))
}


all_trans = vector()
for (i in 1:length(iso)) {
  all_trans <- append(all_trans, iso_trans(i)[[1]])
}


iso_names <- vector()
iso_num <- rep(list(1), 20)
for (i in 1:length(iso)) {
  if (sum(all_trans == all_trans[i]) == 1) {
    iso_names <- append(iso_names, toString(all_trans[i]))
  } else {
    temp_name <- as.vector(toString(all_trans[i]))
    temp_name <- append(temp_name, letters[iso_num[[all_trans[i]]]])
    temp_name <- paste(temp_name, collapse = "")
    iso_names <- append(iso_names, temp_name)
    iso_num[[all_trans[i]]] <- iso_num[[all_trans[i]]] + 1
  }
}


new_mat <- cbind(new_mat, as.matrix(trans_num))
new_mat <- cbind(as.matrix(iso_names), new_mat)


# test = iso_graph[[22]]
# test_individuals <- c(1, 2, 3, 4, 5, 6)
# test_ids <- combn(test_individuals, 3, simplify = FALSE)
# test_sg = rep(list(NA),length(test_ids))
# test_sg_census = rep(list(NA),length(test_ids))
# test_state <- rep(list(NA),length(test_ids))
# test_trans <- rep(list(NA),length(test_ids))
# for (i in 1:length(test_ids)){
#   test_sg[[i]] <- induced_subgraph(test, vids = test_ids[i][[1]])
#   test_sg_census[[i]] <- triad_census(test_sg[[i]])
#   test_state[[i]] <- triad_state(test_sg_census[[i]])[[1]]
#   test_trans[[i]] <- triad_state(test_sg_census[[i]])[[2]]
# }
# sub_mat <- matrix(test_ids)
# sub_mat <- cbind(sub_mat, as.matrix(test_sg))
# sub_mat <- cbind(sub_mat, as.matrix(test_sg_census))
# sub_mat <- cbind(sub_mat, as.matrix(test_state))
# sub_mat <- cbind(sub_mat, as.matrix(test_trans))
# colnames(sub_mat) <- c("Triad", "IGraph", "Triad_Census", "State_ID", "Transitivity")


intrans = rep(list(),length(iso))
for (i in 1:length(iso)) {
  data = iso_graph[[i]]
  individuals <- c(1, 2, 3, 4, 5, 6)
  ids <- combn(individuals, 3, simplify = FALSE)
  intrans_temp <- NULL
  int_count = 1
  for (j in 1:length(ids)) {
    sg <- induced_subgraph(data, vids = ids[[j]])
    sg_census <- triad_census(sg)
    if (triad_state(sg_census)[[2]] == "Intransitive") {
      intrans_temp[[int_count]] <- ids[[j]]
      int_count <- int_count + 1
    }
  }
  intrans[[i]] <- intrans_temp
}

new_mat <- cbind(new_mat, as.matrix(intrans))
for (i in 1:nrow(new_mat)) {
  for (j in 1:length(new_mat[[i, 2]])) {
    new_mat[[i, 2]][j] <- abs(5 - new_mat[[i, 2]][j])
  }
}

colnames(new_mat) <- c("ID", "Wins", "Count", "Example IGraph", "Transitivity", "Intransitive_Vert")
iso_combos <- iso_combos[c(1, 2, 4, 7, 14, 3, 6, 12, 15, 5, 11, 8, 10, 17, 9, 13, 18, 22, 16, 19, 21, 20)]
iso_mat <- new_mat[c(1, 2, 4, 7, 14, 3, 6, 12, 15, 5, 11, 8, 10, 17, 9, 13, 18, 22, 16, 19, 21, 20),]



# iso_mat[[2,2]][unlist(iso_mat[[2,6]])]





# x <- c(1, 0.5, -0.5, -1, -0.5, 0.5)
# y <- c(0, 1, 1, 0, -1, -1)
# coords <- data.frame(x, y)

x2 <- c(0, 1, 1, 0, -1, -1)
y2 <- c(1, 0.5, -0.5, -1, -0.5, 0.5)
sideways <- as.matrix(data.frame(x2, y2))





par(mfrow=c(5,5), mar=c(1,0,1,0))
for (i in 1:length(iso)) {
  plot(iso_mat[[i, 4]], layout = sideways, vertex.color = "black",
       vertex.size = 25, vertex.frame.color = "black", vertex.frame.width = 1.25,
       edge.color = "black", edge.arrow.size = .2, vertex.label = NA, main = iso_mat[[i, 1]])
  for (j in 1:length(iso_mat[[i,6]])) {
    try(polygon(sideways[iso_mat[[i,6]][[j]],], col = rgb(0, 0, 0, 0.25), border = NA), silent = TRUE)
  }
}



# par(mfrow=c(5,5), mar=c(0,0,1,0))
# for (i in 1:length(iso)) {
#   plot(iso_mat[[i, 4]], layout = sideways, vertex.color = "#80b2d7",
#        vertex.size = 25, vertex.frame.color = "black", vertex.frame.width = 1.25,
#        edge.color = "black", edge.arrow.size = .2, vertex.label = NA, main = iso_mat[[i, 1]])
#   for (j in 1:length(iso_mat[[i,6]])) {
#     try(polygon(sideways[iso_mat[[i,6]][[j]],], col = rgb(128/255, 178/255, 215/255, 0.4), border = NA), silent = TRUE)
#   }
# }


# par(mfrow=c(5,5), mar=c(0,0,1,0))
# for (i in 1:length(iso)) {
#   plot(iso_mat[[i, 4]], layout = sideways, vertex.color = "#003a67",
#        vertex.size = 25, vertex.frame.color = "#003a67", vertex.frame.width = 1.25,
#        edge.color = "#003a67", edge.arrow.size = .2, vertex.label = NA, main = iso_mat[[i, 1]])
#   for (j in 1:length(iso_mat[[i,6]])) {
#     try(polygon(sideways[iso_mat[[i,6]][[j]],], col = adjustcolor("#003a67", alpha.f = 0.5), border = NA), silent = TRUE)
#   }
# }



# "#003a67"
# "#80b2d7"


# par(mfrow=c(1,1), mar=c(1,1,1,1))
# plot(iso_mat[[22, 4]], layout = sideways, vertex.color = "#80b2d7",
#      vertex.size = 25, vertex.frame.color = "#003a67", vertex.frame.width = 1.25,
#      edge.color = "black", edge.arrow.size = .2, vertex.label = NA, main = iso_mat[[i, 1]])
# for (j in 1:length(iso_mat[[22,6]])) {
#   try(polygon(sideways[iso_mat[[22,6]][[j]],], col = rgb(0, 0, 0, 0.25), border = NA), silent = TRUE)
# }


# plots the isomorphs with number of wins on each vertex
par(mfrow=c(5,5), mar=c(1,0,1,0))
for (i in 1:length(iso)) {
  plot(iso_mat[[i, 4]], layout = sideways, vertex.color = "black",
       vertex.size = 25, vertex.frame.color = "black", vertex.frame.width = 1.25,
       edge.color = "black", vertex.label = iso_mat[[i, 2]], vertex.label.color = "black", edge.arrow.size = .2, main = iso_mat[[i, 1]])
  for (j in 1:length(iso_mat[[i,6]])) {
    try(polygon(sideways[iso_mat[[i,6]][[j]],], col = rgb(0, 0, 0, 0.25), border = NA), silent = TRUE)
  }
  for (k in 1:length(iso_mat[[i,2]])) {
    text(sideways[k, 1], sideways[k, 2], abs(5 - iso_combos[[i]][k]), col = "white")
  }
}



# this is the colorful one
bin <- list(as.list(c(1, 0, 0)), as.list(c(1, 1, 0)), as.list(c(0, 0, 1)), as.list(c(0, 1, 0)), as.list(c(1, 0, 1)), as.list(c(0, 1, 1)))

par(mfrow=c(5,5), mar=c(1,0,1,0))
for (i in 1:length(iso)) {
  plot(iso_mat[[i, 4]], layout = sideways, vertex.color = "black",
       vertex.size = 25, vertex.frame.color = "black", vertex.frame.width = 1.25,
       edge.color = "black", vertex.label = iso_mat[[i, 2]], vertex.label.color = "black", edge.arrow.size = .2, main = iso_mat[[i, 1]])
  for (j in 1:length(iso_mat[[i,6]])) {
    try(polygon(sideways[iso_mat[[i,6]][[j]],], col = rgb(bin[[j]][[1]], bin[[j]][[2]], bin[[j]][[3]], 0.25), border = rgb(bin[[j]][[1]], bin[[j]][[2]], bin[[j]][[3]], 1)), silent = TRUE)
  }
  for (k in 1:length(iso_mat[[i,2]])) {
    text(sideways[k, 1], sideways[k, 2], abs(5 - iso_combos[[i]][k]), col = "white")
  }
}




iso_order <- list()
iso_order_num <- 1
not_iso_order <- list()
not_iso_order_num <- 1
for (i in 1:nrow(iso_mat)) {
  for (j in 1:nrow(iso_mat)) {
    # returns a list of possible id changes if one pair flips
    if (identical(sort(iso_mat[[i,2]] - iso_mat[[j, 2]]), c(-1, 0, 0, 0, 0, 1))) {
      iso_order[[iso_order_num]] <- sort(c(iso_mat[[i,1]], iso_mat[[j,1]]), decreasing = TRUE)
      iso_order_num = iso_order_num + 1
    }
    # returns a list of impossible id changes if one pair flips
    else if (identical(sort(iso_mat[[i,2]] - iso_mat[[j, 2]]), c(0, 0, 0, 0, 0, 0))) {
      iso_order[[iso_order_num]] <- sort(c(iso_mat[[i,1]], iso_mat[[j,1]]), decreasing = TRUE)
      iso_order_num = iso_order_num + 1
    }
    else {
      not_iso_order[[not_iso_order_num]] <- sort(c(iso_mat[[i,1]], iso_mat[[j,1]]), decreasing = TRUE)
      not_iso_order_num = not_iso_order_num + 1
    }
  }
}
iso_order <- unique(iso_order)
not_iso_order <- unique(not_iso_order)



# empty transition matrix
t_matrix <- matrix(0, nrow = length(new_mat[,1]), ncol = length(new_mat[,1]), 
                   dimnames = list(sort(unlist(new_mat[,1]), decreasing = TRUE), sort(unlist(new_mat[,1]), decreasing = TRUE)))

for (i in 1:length(not_iso_order)) {
  t_matrix[not_iso_order[[i]][1], not_iso_order[[i]][2]] <- NA
  t_matrix[not_iso_order[[i]][2], not_iso_order[[i]][1]] <- NA
}




# vertex.label = iso_mat[[i, 2]], vertex.label.color = "black",


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


iso_order


### Make graph layout  - doesn't look good yet.
## Probably have to dictate exact location of nodes to make it look as we need.

# Load the igraph package
library(igraph)


# 1. Convert the list to an igraph object
# Flatten the list and convert to an edge list matrix
edge_list <- do.call(rbind, iso_order)
g <- graph_from_edgelist(edge_list, directed = FALSE)

# 2. Remove self-loops
g <- simplify(g, remove.loops = TRUE, remove.multiple = FALSE)

# 3. Plot the network
dev.off()

# Set a hierarchical layout (tree layout)
#layout <- layout_in_circle(g)
#layout <- layout_as_star(g)
layout <- layout_with_dh(g)

# Plot with customizations
plot(
  g,
  layout = layout,
  vertex.size = 30,
  vertex.label.color = "black",
  vertex.color = "orange",
  vertex.frame.color = "black",     # Black border for nodes
  vertex.frame.width = 2,           # Thicker border for nodes
  edge.color = "black",             # Darker edge color
  edge.width = 1.5,                 # Thicker edges
  edge.arrow.size = 0.5             # Arrow size if directed
)
