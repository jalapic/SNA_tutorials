#-----------------function for making transition matrices-------------------
make_TM <- function(df) {
  x <- df[[1]]
  states <- sort(unique(x))
  m <- table(
    factor(x[-length(x)], levels = states),
    factor(x[-1], levels = states)
  )
  as.matrix(m)
}

#-------------loop to make transition matrices for all cohorts--------------
TM_list <- vector("list", length(c_dfs))

for (i in seq_along(c_dfs)) {
  TM_list[[i]] <- make_TM(c_dfs[[i]])
}

for (i in seq_along(TM_list)) {
  print(TM_list[[i]])
}

# turn TM counts to decimals for each cohort (no 6th cohort)
for (i in seq_along(TM_list)) {
  TM_list[[i]] <- TM_list[[i]] / rowSums(TM_list[[i]])
}

# cohort TMs do not include all possible network states (<56x56) - fill them in
full_TMs <- vector("list", length(TM_list))

for (i in seq_along(full_TMs)) {
  full_TMs[[i]] <- matrix(0, nrow = 56, ncol = 56)
  rownames(full_TMs[[i]]) <- colnames(full_TMs[[i]]) <- 
    as.character(1:56)
  full_TMs[[i]][rownames(TM_list[[i]]), colnames(TM_list[[i]])] <- 
    TM_list[[i]]
}

for (i in seq_along(full_TMs)) {
  print(full_TMs[[i]])
}